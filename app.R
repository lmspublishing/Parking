# Sydney Parking Finder --------------------------------------------------------
# R/Shiny MVP using publicly available parking data.
# Coverage in this first version: City of Sydney metered parking/rate areas.
#
# Run:
# install.packages(c("shiny", "bslib", "bsicons", "leaflet", "DT", "dplyr", "sf",
#                    "httr2", "jsonlite", "lubridate", "htmltools"))
# shiny::runApp()

library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(dplyr)
library(sf)
library(httr2)
library(jsonlite)
library(lubridate)
library(htmltools)

options(shiny.maxRequestSize = 20 * 1024^2)
sf_use_s2(TRUE)

APP_USER_AGENT <- "SydneyParkingFinder/0.1 (personal Shiny prototype)"
PARKING_APP_ITEM_ID <- "71bb12507a3240c4b12e7fbba5be58e1"
ARCGIS_ROOT <- "https://www.arcgis.com/sharing/rest"
COS_PORTAL_ROOT <- "https://cityofsydney.maps.arcgis.com/sharing/rest"

# Current published City of Sydney rates checked 10 Sep 2026.
# The app still tries to identify the official rate area spatially.
CURRENT_RATES <- c(`1` = 9.00, `2` = 7.00)

# ------------------------------ HTTP helpers ---------------------------------
get_json <- function(url, query = list(), timeout_s = 30) {
  req <- request(url) |> req_user_agent(APP_USER_AGENT) |> req_timeout(timeout_s)
  if (length(query)) req <- do.call(req_url_query, c(list(req), query))
  resp <- req_perform(req)
  resp_check_status(resp)
  resp_body_json(resp, simplifyVector = FALSE)
}

get_raw <- function(url, query = list(), timeout_s = 60) {
  req <- request(url) |> req_user_agent(APP_USER_AGENT) |> req_timeout(timeout_s)
  if (length(query)) req <- do.call(req_url_query, c(list(req), query))
  resp <- req_perform(req)
  resp_check_status(resp)
  resp_body_raw(resp)
}

# ------------------------------ ArcGIS helpers -------------------------------
arcgis_item_meta <- function(item_id) {
  get_json(paste0(ARCGIS_ROOT, "/content/items/", item_id), list(f = "json"))
}

arcgis_item_data <- function(item_id) {
  get_json(paste0(ARCGIS_ROOT, "/content/items/", item_id, "/data"), list(f = "json"))
}

flatten_refs <- function(x) {
  vals <- tryCatch(unlist(x, recursive = TRUE, use.names = TRUE), error = function(e) NULL)
  if (is.null(vals) || !length(vals)) return(list(urls = character(), ids = character()))
  vals_chr <- as.character(vals)
  nms <- names(vals)
  if (is.null(nms)) nms <- rep("", length(vals_chr))

  urls <- vals_chr[grepl("https?://", vals_chr, ignore.case = TRUE) &
                     grepl("(FeatureServer|MapServer)", vals_chr, ignore.case = TRUE)]

  id_keys <- grepl("(itemid|item_id|webmap|webmapid|mapid|portalitem)",
                   tolower(nms))
  ids <- vals_chr[id_keys & grepl("^[0-9a-fA-F]{32}$", vals_chr)]

  list(urls = unique(urls), ids = unique(ids))
}

normalize_service_url <- function(x) {
  x <- sub("[?].*$", "", x)
  sub("/+$", "", x)
}

resolve_item_services <- function(item_id, depth = 0, max_depth = 3, seen = character()) {
  if (depth > max_depth || item_id %in% seen) return(character())
  seen <- c(seen, item_id)

  meta <- tryCatch(arcgis_item_meta(item_id), error = function(e) NULL)
  dat  <- tryCatch(arcgis_item_data(item_id), error = function(e) NULL)
  urls <- character()
  child_ids <- character()

  if (!is.null(meta$url) && grepl("(FeatureServer|MapServer)", meta$url, ignore.case = TRUE)) {
    urls <- c(urls, meta$url)
  }

  if (!is.null(dat)) {
    refs <- flatten_refs(dat)
    urls <- c(urls, refs$urls)
    child_ids <- c(child_ids, refs$ids)
  }

  if (length(child_ids) && depth < max_depth) {
    for (cid in unique(child_ids)) {
      urls <- c(urls, resolve_item_services(cid, depth + 1, max_depth, seen))
    }
  }

  unique(vapply(urls, normalize_service_url, character(1)))
}

arcgis_org_search <- function(title, num = 100) {
  out <- get_json(
    paste0(COS_PORTAL_ROOT, "/search"),
    list(q = sprintf('title:"%s"', title), f = "json", num = num)
  )
  results <- out$results
  if (is.null(results) || !length(results)) return(list())

  # Prefer exact title matches; otherwise return all candidates.
  exact <- Filter(function(z) identical(tolower(z$title %||% ""), tolower(title)), results)
  if (length(exact)) exact else results
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

service_layer_info <- function(service_url) {
  service_url <- normalize_service_url(service_url)

  # Already a layer URL, e.g. FeatureServer/0
  if (grepl("/(FeatureServer|MapServer)/[0-9]+$", service_url, ignore.case = TRUE)) {
    meta <- tryCatch(get_json(service_url, list(f = "json")), error = function(e) NULL)
    if (is.null(meta)) return(tibble())
    return(tibble(
      layer_url = service_url,
      layer_name = meta$name %||% basename(service_url),
      geometry_type = meta$geometryType %||% NA_character_
    ))
  }

  meta <- tryCatch(get_json(service_url, list(f = "json")), error = function(e) NULL)
  if (is.null(meta) || is.null(meta$layers) || !length(meta$layers)) return(tibble())

  bind_rows(lapply(meta$layers, function(z) {
    tibble(
      layer_url = paste0(service_url, "/", z$id),
      layer_name = z$name %||% paste0("Layer ", z$id),
      geometry_type = z$geometryType %||% NA_character_
    )
  }))
}

query_layer_sf <- function(layer_url, max_features = 10000) {
  ids <- tryCatch(
    get_json(paste0(layer_url, "/query"), list(
      where = "1=1", returnIdsOnly = "true", f = "json"
    )),
    error = function(e) NULL
  )

  oid <- ids$objectIds %||% integer()
  if (!length(oid)) {
    raw <- get_raw(paste0(layer_url, "/query"), list(
      where = "1=1", outFields = "*", returnGeometry = "true",
      outSR = "4326", f = "geojson", resultRecordCount = 2000
    ))
    tmp <- tempfile(fileext = ".geojson")
    writeBin(raw, tmp)
    on.exit(unlink(tmp), add = TRUE)
    return(st_read(tmp, quiet = TRUE))
  }

  oid <- oid[seq_len(min(length(oid), max_features))]
  chunks <- split(oid, ceiling(seq_along(oid) / 500))
  parts <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    raw <- get_raw(paste0(layer_url, "/query"), list(
      objectIds = paste(chunks[[i]], collapse = ","),
      outFields = "*", returnGeometry = "true", outSR = "4326", f = "geojson"
    ))
    tmp <- tempfile(fileext = ".geojson")
    writeBin(raw, tmp)
    parts[[i]] <- tryCatch(st_read(tmp, quiet = TRUE), error = function(e) NULL)
    unlink(tmp)
  }

  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(NULL)
  do.call(rbind, parts)
}

load_layers_for_items <- function(item_ids) {
  all <- list()
  k <- 1
  for (item_id in unique(item_ids)) {
    services <- resolve_item_services(item_id)
    if (!length(services)) next
    for (svc in services) {
      li <- service_layer_info(svc)
      if (!nrow(li)) next
      li$item_id <- item_id
      all[[k]] <- li
      k <- k + 1
    }
  }
  if (!length(all)) tibble() else bind_rows(all) |> distinct(layer_url, .keep_all = TRUE)
}

find_city_item_ids <- function(title) {
  results <- tryCatch(arcgis_org_search(title), error = function(e) list())
  if (!length(results)) return(character())
  unique(vapply(results, function(z) z$id %||% "", character(1)))
}

# ---------------------------- Parking data logic -----------------------------
row_text <- function(x, preferred_pattern = "area|zone|rate|tariff|name|label|type|status") {
  dat <- st_drop_geometry(x)
  keep <- grep(preferred_pattern, names(dat), ignore.case = TRUE, value = TRUE)
  if (!length(keep)) keep <- names(dat)[seq_len(min(5, ncol(dat)))]
  apply(dat[, keep, drop = FALSE], 1, function(r) paste(r, collapse = " | "))
}

detect_rate_area <- function(x) {
  txt <- row_text(x)
  out <- rep(NA_character_, length(txt))
  out[grepl("(area|rate|zone)[^0-9]{0,8}1\\b", txt, ignore.case = TRUE)] <- "1"
  out[grepl("(area|rate|zone)[^0-9]{0,8}2\\b", txt, ignore.case = TRUE)] <- "2"
  out
}

load_city_parking <- function(progress = NULL) {
  if (!is.null(progress)) progress$set(message = "Discovering City of Sydney parking layers…", value = 0.1)

  # Official replacement item referenced by City of Sydney / Data.NSW.
  meter_item_ids <- PARKING_APP_ITEM_ID

  # Add separately published rate-zone item(s), discovered by title to avoid
  # hard-coding an item ID that may be republished.
  rate_item_ids <- find_city_item_ids("Ticket parking rates")

  if (!is.null(progress)) progress$set(message = "Reading public ArcGIS services…", value = 0.25)
  meter_layers <- load_layers_for_items(meter_item_ids)
  rate_layers  <- load_layers_for_items(rate_item_ids)

  if (!nrow(meter_layers)) stop("Could not discover the City of Sydney parking-meter layers.")

  # Point layers from the official parking item.
  point_layers <- meter_layers |>
    filter(grepl("Point", geometry_type, ignore.case = TRUE) |
             grepl("meter|ticket|parking", layer_name, ignore.case = TRUE))

  point_objs <- list()
  j <- 1
  for (i in seq_len(nrow(point_layers))) {
    if (!is.null(progress)) progress$set(
      message = paste("Loading", point_layers$layer_name[i]),
      value = 0.25 + 0.40 * i / max(1, nrow(point_layers))
    )
    obj <- tryCatch(query_layer_sf(point_layers$layer_url[i]), error = function(e) NULL)
    if (is.null(obj) || !nrow(obj)) next
    gt <- unique(as.character(st_geometry_type(obj)))
    if (!any(grepl("POINT", gt))) next
    obj$source_layer <- point_layers$layer_name[i]
    point_objs[[j]] <- obj
    j <- j + 1
  }

  if (!length(point_objs)) stop("Parking layers were found, but no public point features could be read.")
  meters <- bind_rows(point_objs)
  meters <- st_transform(meters, 4326)

  # Remove obvious non-parking point layers if the app item contains extras.
  meters <- meters[grepl("meter|ticket|parking|1/4|15", meters$source_layer, ignore.case = TRUE), ]
  if (!nrow(meters)) stop("No usable parking-meter point layer was found.")

  # Load rate polygons.
  zone_objs <- list()
  j <- 1
  if (nrow(rate_layers)) {
    poly_layers <- rate_layers |>
      filter(grepl("Polygon", geometry_type, ignore.case = TRUE) |
               grepl("rate|tariff|zone|area", layer_name, ignore.case = TRUE))

    for (i in seq_len(nrow(poly_layers))) {
      obj <- tryCatch(query_layer_sf(poly_layers$layer_url[i]), error = function(e) NULL)
      if (is.null(obj) || !nrow(obj)) next
      gt <- unique(as.character(st_geometry_type(obj)))
      if (!any(grepl("POLYGON", gt))) next
      obj$source_layer <- poly_layers$layer_name[i]
      zone_objs[[j]] <- st_transform(obj, 4326)
      j <- j + 1
    }
  }

  if (!is.null(progress)) progress$set(message = "Assigning rate areas…", value = 0.78)

  # Point-layer attributes may already identify rate area.
  meters$rate_area <- detect_rate_area(meters)

  if (length(zone_objs)) {
    zones <- bind_rows(zone_objs)
    zones$rate_area_join <- detect_rate_area(zones)
    zones <- zones[!is.na(zones$rate_area_join), c("rate_area_join")]

    if (nrow(zones)) {
      idx <- st_intersects(meters, zones)
      zone_for_meter <- vapply(idx, function(ix) {
        if (!length(ix)) NA_character_ else zones$rate_area_join[ix[1]]
      }, character(1))
      meters$rate_area[is.na(meters$rate_area)] <- zone_for_meter[is.na(meters$rate_area)]
    }
  }

  meters$hourly_rate <- unname(CURRENT_RATES[meters$rate_area])

  # Detect the City's 15-minute / 1/4P free-ticket points if present.
  meters$is_free_15 <- grepl("1/4|15|min|free", meters$source_layer, ignore.case = TRUE)

  # Keep one point where duplicated across multiple display layers.
  coords <- st_coordinates(meters)
  meters$.lon_key <- round(coords[, 1], 6)
  meters$.lat_key <- round(coords[, 2], 6)
  meters <- meters |>
    arrange(desc(!is.na(hourly_rate)), desc(is_free_15)) |>
    distinct(.lon_key, .lat_key, .keep_all = TRUE)

  if (!is.null(progress)) progress$set(message = "Parking data ready", value = 1)
  meters
}

# ------------------------------ Geocoding ------------------------------------
geocode_cache <- new.env(parent = emptyenv())
last_geocode_time <- as.POSIXct("1970-01-01", tz = "UTC")

geocode_sydney <- function(address) {
  key <- tolower(trimws(address))
  if (!nzchar(key)) return(NULL)
  if (exists(key, envir = geocode_cache, inherits = FALSE)) return(get(key, envir = geocode_cache))

  # Nominatim public service policy: max 1 request/sec, identify app, cache results.
  elapsed <- as.numeric(difftime(Sys.time(), last_geocode_time, units = "secs"))
  if (is.finite(elapsed) && elapsed < 1) Sys.sleep(1 - elapsed)

  req <- request("https://nominatim.openstreetmap.org/search") |>
    req_user_agent(APP_USER_AGENT) |>
    req_timeout(20)
  req <- do.call(req_url_query, c(list(req), list(
    q = paste(address, "NSW Australia"),
    format = "jsonv2", limit = 5,
    countrycodes = "au",
    viewbox = "150.85,-33.65,151.40,-34.20",
    bounded = 1,
    addressdetails = 1
  )))
  resp <- req_perform(req)
  resp_check_status(resp)
  ans <- resp_body_json(resp, simplifyVector = TRUE)
  last_geocode_time <<- Sys.time()

  if (is.null(ans) || !nrow(ans)) return(NULL)
  out <- list(
    lat = as.numeric(ans$lat[1]),
    lon = as.numeric(ans$lon[1]),
    label = ans$display_name[1]
  )
  assign(key, out, envir = geocode_cache)
  out
}

# ------------------------------- UI ------------------------------------------
ui <- page_sidebar(
  title = "Sydney Parking Finder",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 360,
    h4("Where are you going?"),
    textInput("address", NULL, placeholder = "e.g. 1 Martin Place, Sydney"),
    actionButton("search_address", "Find address", class = "btn-primary w-100"),
    div(class = "text-muted small mt-2", "Or click directly on the map."),
    hr(),
    numericInput("duration", "Parking duration (hours)", value = 2, min = 0.25, max = 24, step = 0.25),
    sliderInput("radius", "Search radius", min = 250, max = 2500, value = 1000, step = 250, post = " m"),
    actionButton("find_parking", "Find cheapest parking", class = "btn-success w-100"),
    hr(),
    uiOutput("destination_text"),
    uiOutput("source_status"),
    div(class = "alert alert-warning small mt-3",
        strong("Important: "),
        "This is a decision-support tool. Always check the parking sign at the space. Signs, temporary restrictions and event controls override the app."
    ),
    div(class = "text-muted small",
        "Address search uses OpenStreetMap Nominatim with caching and its public-use rate limit. For a public/high-traffic deployment, replace it with a production geocoder."
    )
  ),
  layout_columns(
    col_widths = c(8, 4),
    card(
      full_screen = TRUE,
      card_header("Map"),
      leafletOutput("map", height = "72vh")
    ),
    card(
      card_header("Best options"),
      uiOutput("summary_cards"),
      DTOutput("results")
    )
  )
)

# ------------------------------ Server ---------------------------------------
server <- function(input, output, session) {
  destination <- reactiveVal(list(lat = -33.8688, lon = 151.2093, label = "Sydney CBD"))
  parking_data <- reactiveVal(NULL)
  data_error <- reactiveVal(NULL)
  results_data <- reactiveVal(NULL)

  output$map <- renderLeaflet({
    d <- destination()
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(d$lon, d$lat, zoom = 14) |>
      addCircleMarkers(d$lon, d$lat, radius = 8, stroke = TRUE, fillOpacity = 1,
                       label = "Destination", group = "destination")
  })

  output$destination_text <- renderUI({
    d <- destination()
    tagList(
      strong("Destination"),
      div(class = "small", d$label),
      div(class = "text-muted small", sprintf("%.5f, %.5f", d$lat, d$lon))
    )
  })

  output$source_status <- renderUI({
    if (!is.null(data_error())) {
      return(div(class = "alert alert-danger small mt-3", data_error()))
    }
    if (is.null(parking_data())) {
      div(class = "text-muted small mt-3", "Parking data will load the first time you search.")
    } else {
      div(class = "alert alert-success small mt-3",
          paste(format(nrow(parking_data()), big.mark = ","), "parking-meter locations loaded from public City of Sydney data."))
    }
  })

  observeEvent(input$map_click, {
    click <- input$map_click
    req(click$lat, click$lng)
    destination(list(lat = click$lat, lon = click$lng, label = "Map-selected destination"))
    leafletProxy("map") |>
      clearGroup("destination") |>
      addCircleMarkers(click$lng, click$lat, radius = 8, stroke = TRUE,
                       fillOpacity = 1, label = "Destination", group = "destination")
  })

  observeEvent(input$search_address, {
    req(nzchar(trimws(input$address)))
    withProgress(message = "Finding address…", value = 0.3, {
      ans <- tryCatch(geocode_sydney(input$address), error = function(e) NULL)
      incProgress(0.7)
      if (is.null(ans)) {
        showNotification("Address not found. Try a more specific Sydney address or click the map.", type = "error")
        return()
      }
      destination(ans)
      leafletProxy("map") |>
        clearGroup("destination") |>
        addCircleMarkers(ans$lon, ans$lat, radius = 8, stroke = TRUE,
                         fillOpacity = 1, label = ans$label, group = "destination") |>
        setView(ans$lon, ans$lat, zoom = 16)
    })
  })

  ensure_parking_data <- function() {
    if (!is.null(parking_data())) return(TRUE)
    data_error(NULL)
    ok <- FALSE
    withProgress(message = "Loading public parking data…", value = 0, {
      p <- Progress$new(session, min = 0, max = 1)
      on.exit(p$close(), add = TRUE)
      obj <- tryCatch(
        load_city_parking(p),
        error = function(e) {
          data_error(paste("Could not load live parking data:", conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(obj) && nrow(obj)) {
        parking_data(obj)
        ok <- TRUE
      }
    })
    ok
  }

  observeEvent(input$find_parking, {
    if (!ensure_parking_data()) return()
    d <- destination()
    meters <- parking_data()

    dest_sf <- st_sfc(st_point(c(d$lon, d$lat)), crs = 4326)
    dist_m <- as.numeric(st_distance(meters, dest_sf))

    duration <- input$duration
    meters$distance_m <- dist_m
    meters$walk_min <- pmax(1, round(dist_m / 80))  # ~4.8 km/h walking pace

    # 15-minute free-ticket spaces are eligible only for <= 15 minutes.
    meters$effective_rate <- meters$hourly_rate
    meters$estimated_cost <- meters$hourly_rate * duration
    short_free <- meters$is_free_15 & duration <= 0.25
    meters$estimated_cost[short_free] <- 0
    meters$effective_rate[short_free] <- 0

    nearby <- meters |>
      filter(distance_m <= input$radius) |>
      filter(!is.na(estimated_cost)) |>
      arrange(estimated_cost, distance_m)

    if (!nrow(nearby)) {
      results_data(NULL)
      showNotification(
        "No priced City of Sydney meter locations were found in that radius. Try a larger radius or a destination inside the City of Sydney LGA.",
        type = "warning", duration = 8
      )
      leafletProxy("map") |> clearGroup("parking") |> clearGroup("radius") |>
        addCircles(d$lon, d$lat, radius = input$radius, fill = FALSE, weight = 1, group = "radius")
      return()
    }

    coords <- st_coordinates(nearby)
    display <- nearby |>
      st_drop_geometry() |>
      mutate(
        rank = row_number(),
        cost = sprintf("$%.2f", estimated_cost),
        rate = ifelse(effective_rate == 0, "FREE (15 min max)", sprintf("$%.2f/hr", effective_rate)),
        distance = ifelse(distance_m < 1000,
                          paste0(round(distance_m), " m"),
                          sprintf("%.1f km", distance_m / 1000)),
        walk = paste0(walk_min, " min"),
        rate_area = ifelse(is.na(rate_area), "Unknown", paste("Area", rate_area)),
        source = source_layer
      ) |>
      select(rank, cost, rate, distance, walk, rate_area, source)

    results_data(display)

    topn <- nearby[seq_len(min(50, nrow(nearby))), ]
    top_coords <- st_coordinates(topn)
    pop <- sprintf(
      "<b>$%.2f estimated</b><br>$%.2f/hr<br>%s m away · ~%s min walk<br>%s",
      topn$estimated_cost, topn$effective_rate, round(topn$distance_m), topn$walk_min,
      htmlEscape(topn$source_layer)
    )

    proxy <- leafletProxy("map") |>
      clearGroup("parking") |>
      clearGroup("radius") |>
      addCircles(d$lon, d$lat, radius = input$radius, fill = FALSE, weight = 1, group = "radius") |>
      addCircleMarkers(
        lng = top_coords[, 1], lat = top_coords[, 2],
        radius = 7, stroke = TRUE, weight = 1, fillOpacity = 0.75,
        popup = pop, group = "parking",
        clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)
      )

    best_coords <- st_coordinates(nearby[1, ])
    proxy |>
      addCircleMarkers(
        lng = best_coords[1, 1], lat = best_coords[1, 2], radius = 11,
        stroke = TRUE, weight = 3, fillOpacity = 0.9,
        popup = paste0("<b>Cheapest nearby option</b><br>$", sprintf("%.2f", nearby$estimated_cost[1])),
        group = "parking"
      ) |>
      fitBounds(
        lng1 = min(c(d$lon, top_coords[, 1])), lat1 = min(c(d$lat, top_coords[, 2])),
        lng2 = max(c(d$lon, top_coords[, 1])), lat2 = max(c(d$lat, top_coords[, 2]))
      )
  })

  output$summary_cards <- renderUI({
    x <- results_data()
    if (is.null(x) || !nrow(x)) {
      return(div(class = "text-muted p-2", "Choose a destination and click ‘Find cheapest parking’."))
    }
    layout_columns(
      col_widths = c(6, 6),
      value_box(title = "Cheapest", value = x$cost[1], showcase = bsicons::bs_icon("cash-coin")),
      value_box(title = "Walk", value = x$walk[1], showcase = bsicons::bs_icon("person-walking"))
    )
  })

  output$results <- renderDT({
    x <- results_data()
    req(!is.null(x), nrow(x) > 0)
    datatable(
      head(x, 25),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
      class = "compact stripe hover"
    )
  })
}

shinyApp(ui, server)
