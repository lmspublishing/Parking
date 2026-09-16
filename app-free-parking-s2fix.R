# Sydney Parking Finder --------------------------------------------------------
# R/Shiny MVP using publicly available parking data.
# Coverage: City of Sydney meters/rate areas + explicitly mapped free parking from OpenStreetMap.
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

APP_USER_AGENT <- "SydneyParkingFinder/0.2 (personal Shiny prototype)"
PARKING_APP_ITEM_ID <- "71bb12507a3240c4b12e7fbba5be58e1"
ARCGIS_ROOT <- "https://www.arcgis.com/sharing/rest"
COS_PORTAL_ROOT <- "https://cityofsydney.maps.arcgis.com/sharing/rest"
RATE_LAYER_URL <- "https://services1.arcgis.com/cNVyNtjGVZybOQWZ/ArcGIS/rest/services/Ticket_parking_rates/FeatureServer/0"
OVERPASS_ENDPOINTS <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter"
)

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
  # City of Sydney's public rate polygon currently identifies zones by tariff
  # (for example "$8.40 p/h" and "$6.40 p/h") rather than by the literal
  # strings "Area 1" and "Area 2".  The published 2026/27 rates are $9/$7,
  # so recognise both the legacy tariff labels in the GIS layer and current
  # values, as well as explicit area/zone labels if those appear later.
  dat <- st_drop_geometry(x)
  txt <- apply(dat, 1, function(r) paste(as.character(r), collapse = " | "))
  out <- rep(NA_character_, length(txt))

  out[grepl("(area|rate|zone)[^0-9]{0,8}1\\b|\\$?8\\.40\\s*(p/?h|per\\s*hour)|\\$?9(?:\\.00)?\\s*(p/?h|per\\s*hour)",
             txt, ignore.case = TRUE, perl = TRUE)] <- "1"
  out[grepl("(area|rate|zone)[^0-9]{0,8}2\\b|\\$?6\\.40\\s*(p/?h|per\\s*hour)|\\$?7(?:\\.00)?\\s*(p/?h|per\\s*hour)",
             txt, ignore.case = TRUE, perl = TRUE)] <- "2"
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

  # Stable fallback to the official City of Sydney rate-zone FeatureServer.
  # This avoids a total pricing failure if ArcGIS portal search changes.
  if (!nrow(rate_layers)) {
    rate_layers <- tibble(
      layer_url = RATE_LAYER_URL,
      layer_name = "Ticket parking rates",
      geometry_type = "esriGeometryPolygon",
      item_id = NA_character_
    )
  }

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

  zones_all <- NULL
  unmetered_zones <- NULL

  # Point-layer attributes may already identify rate area.
  meters$rate_area <- detect_rate_area(meters)

  if (length(zone_objs)) {
    zones_all <- bind_rows(zone_objs)

    # ArcGIS occasionally returns self-touching / duplicate-vertex polygons.
    # Repair what we can up front. Spatial predicates below also use MGA Zone 56
    # rather than S2, so one malformed City polygon cannot terminate a Shiny session.
    zones_all <- tryCatch(
      suppressWarnings(st_make_valid(zones_all)),
      error = function(e) zones_all
    )
    zones_all <- zones_all[!st_is_empty(zones_all), ]

    # The official City layer contains polygons explicitly labelled
    # "No parking meters in this area". These are useful search areas, but
    # they are NOT assumed to be legal/free parking because signs and permit
    # restrictions can still apply.
    zone_txt <- apply(st_drop_geometry(zones_all), 1, function(r) paste(as.character(r), collapse = " | "))
    no_meter <- grepl("no (ticket )?parking (meters|machines)|no parking meters",
                      zone_txt, ignore.case = TRUE, perl = TRUE)
    if (any(no_meter)) unmetered_zones <- zones_all[no_meter, ]

    rate_zones <- zones_all
    rate_zones$rate_area_join <- detect_rate_area(rate_zones)
    rate_zones <- rate_zones[!is.na(rate_zones$rate_area_join), c("rate_area_join")]

    if (nrow(rate_zones)) {
      # Do the topology test in a projected CRS (metres) with GEOS. This avoids
      # S2 rejecting City polygons that contain duplicate vertices.
      idx <- tryCatch({
        meters_mga <- st_transform(meters, 7856)
        zones_mga <- st_transform(rate_zones, 7856)
        zones_mga <- suppressWarnings(st_make_valid(zones_mga))
        st_intersects(meters_mga, zones_mga)
      }, error = function(e) {
        warning("Rate-zone spatial join skipped because City polygon geometry is invalid: ",
                conditionMessage(e))
        vector("list", nrow(meters))
      })
      zone_for_meter <- vapply(idx, function(ix) {
        if (!length(ix)) NA_character_ else rate_zones$rate_area_join[ix[1]]
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
  list(meters = meters, unmetered_zones = unmetered_zones)
}

# -------------------------- OpenStreetMap free parking -----------------------
osm_cache <- new.env(parent = emptyenv())

tag_value <- function(tags, key) {
  if (is.null(tags) || is.null(tags[[key]]) || !length(tags[[key]])) return(NA_character_)
  as.character(tags[[key]][1])
}

first_tag_value <- function(tags, keys) {
  for (k in keys) {
    v <- tag_value(tags, k)
    if (!is.na(v) && nzchar(v)) return(v)
  }
  NA_character_
}

parse_duration_hours <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(NA_real_)
  z <- tolower(trimws(x))
  z <- gsub(",", ".", z, fixed = TRUE)

  m <- regexec("([0-9]+(?:\\.[0-9]+)?)\\s*(minutes?|mins?|min|m)\\b", z, perl = TRUE)
  hit <- regmatches(z, m)[[1]]
  if (length(hit) >= 2) return(as.numeric(hit[2]) / 60)

  m <- regexec("([0-9]+(?:\\.[0-9]+)?)\\s*(hours?|hrs?|hr|h)\\b", z, perl = TRUE)
  hit <- regmatches(z, m)[[1]]
  if (length(hit) >= 2) return(as.numeric(hit[2]))

  m <- regexec("([0-9]+(?:\\.[0-9]+)?)\\s*(days?|day|d)\\b", z, perl = TRUE)
  hit <- regmatches(z, m)[[1]]
  if (length(hit) >= 2) return(as.numeric(hit[2]) * 24)

  if (grepl("^[0-9]+(?:\\.[0-9]+)?$", z, perl = TRUE)) return(as.numeric(z) / 60)
  NA_real_
}

osm_day_number <- function(code) {
  match(code, c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"))
}

parse_day_spec <- function(x) {
  hits <- regmatches(x, gregexpr("(?:Mo|Tu|We|Th|Fr|Sa|Su)(?:-(?:Mo|Tu|We|Th|Fr|Sa|Su))?", x, perl = TRUE))[[1]]
  if (!length(hits) || identical(hits, character(0))) return(1:7)
  out <- integer()
  for (h in hits) {
    if (grepl("-", h, fixed = TRUE)) {
      p <- strsplit(h, "-", fixed = TRUE)[[1]]
      a <- osm_day_number(p[1]); b <- osm_day_number(p[2])
      if (!is.na(a) && !is.na(b)) {
        out <- c(out, if (a <= b) a:b else c(a:7, 1:b))
      }
    } else {
      d <- osm_day_number(h)
      if (!is.na(d)) out <- c(out, d)
    }
  }
  unique(out)
}

clock_minutes <- function(hhmm) {
  p <- strsplit(hhmm, ":", fixed = TRUE)[[1]]
  h <- as.numeric(p[1]); m <- as.numeric(p[2])
  if (is.na(h) || is.na(m)) return(NA_real_)
  h * 60 + m
}

condition_segment_matches <- function(segment, t) {
  segment <- trimws(segment)
  if (!nzchar(segment)) return(FALSE)
  if (grepl("PH|SH|customers?|permit|residents?|members?|disabled|delivery", segment, ignore.case = TRUE)) return(FALSE)

  days <- parse_day_spec(segment)
  day_now <- lubridate::wday(t, week_start = 1)
  if (!(day_now %in% days)) return(FALSE)

  tr <- regmatches(segment, gregexpr("(?:[01][0-9]|2[0-4]):[0-5][0-9]-(?:[01][0-9]|2[0-4]):[0-5][0-9]", segment, perl = TRUE))[[1]]
  if (!length(tr) || identical(tr, character(0))) return(TRUE)

  now_m <- lubridate::hour(t) * 60 + lubridate::minute(t)
  any(vapply(tr, function(rng) {
    p <- strsplit(rng, "-", fixed = TRUE)[[1]]
    a <- clock_minutes(p[1]); b <- clock_minutes(p[2])
    if (is.na(a) || is.na(b)) return(FALSE)
    if (b == 1440) return(now_m >= a && now_m < b)
    if (a <= b) now_m >= a && now_m < b else now_m >= a || now_m < b
  }, logical(1)))
}

condition_covers_stay <- function(condition, arrival, duration_h) {
  if (is.na(condition) || !nzchar(trimws(condition))) return(FALSE)
  x <- trimws(gsub("^\\(|\\)$", "", condition))

  # Support common OSM stay-duration conditions such as stay < 2 hours.
  stay_pat <- "stay\\s*(?:<|<=|≤)\\s*([0-9]+(?:\\.[0-9]+)?)\\s*(minutes?|mins?|min|hours?|hrs?|hr|days?|day)"
  sm <- regexec(stay_pat, x, ignore.case = TRUE, perl = TRUE)
  sh <- regmatches(x, sm)[[1]]
  if (length(sh) >= 3) {
    lim <- parse_duration_hours(paste(sh[2], sh[3]))
    if (is.na(lim) || duration_h > lim + 1e-9) return(FALSE)
    x <- trimws(sub(stay_pat, "", x, ignore.case = TRUE, perl = TRUE))
    x <- trimws(gsub("(?i)\\bAND\\b", " ", x, perl = TRUE))
  }

  # Reject conditions containing semantics this lightweight parser cannot
  # safely evaluate. This keeps the result conservative.
  if (grepl("customers?|permit|residents?|members?|disabled|delivery|weather|school|PH|SH",
            x, ignore.case = TRUE, perl = TRUE)) return(FALSE)

  # If no day/time material remains, the stay condition itself was enough.
  if (!grepl("Mo|Tu|We|Th|Fr|Sa|Su|[0-2][0-9]:[0-5][0-9]", x, perl = TRUE)) return(TRUE)

  end_time <- arrival + lubridate::hours(duration_h)
  if (end_time <= arrival) return(FALSE)
  samples <- seq(arrival, end_time - 1, by = "15 min")
  if (!length(samples)) samples <- arrival

  segments <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  all(vapply(samples, function(tt) {
    any(vapply(segments, function(seg) condition_segment_matches(seg, tt), logical(1)))
  }, logical(1)))
}

conditional_value_conditions <- function(x, wanted = "no") {
  if (is.na(x) || !nzchar(trimws(x))) return(character())
  # Capture both `no @ (...)` and simple `no @ Su` style clauses.
  pat <- paste0("(?i)", wanted, "\\s*@\\s*(?:\\([^)]*\\)|[^;]+)")
  m <- gregexpr(pat, x, perl = TRUE)
  hits <- regmatches(x, m)[[1]]
  if (!length(hits) || identical(hits, character(0))) return(character())
  out <- sub(paste0("(?i)^\\s*", wanted, "\\s*@\\s*"), "", hits, perl = TRUE)
  out <- sub("^\\(", "", out)
  out <- sub("\\)$", "", out)
  trimws(out)
}


condition_applies_any <- function(condition, arrival, duration_h) {
  if (is.na(condition) || !nzchar(trimws(condition))) return(NA)
  x <- trimws(gsub("^\\(|\\)$", "", condition))

  if (grepl("customers?|permit|residents?|members?|disabled|delivery|weather|school|PH|SH",
            x, ignore.case = TRUE, perl = TRUE)) return(NA)

  # Evaluate simple stay comparisons if present.
  stay_pat <- "stay\\s*(<|<=|≤|>|>=|≥)\\s*([0-9]+(?:\\.[0-9]+)?)\\s*(minutes?|mins?|min|hours?|hrs?|hr|days?|day)"
  sm <- regexec(stay_pat, x, ignore.case = TRUE, perl = TRUE)
  sh <- regmatches(x, sm)[[1]]
  if (length(sh) >= 4) {
    lim <- parse_duration_hours(paste(sh[3], sh[4]))
    if (is.na(lim)) return(NA)
    op <- sh[2]
    stay_ok <- switch(op,
      "<" = duration_h < lim,
      "<=" = duration_h <= lim,
      "≤" = duration_h <= lim,
      ">" = duration_h > lim,
      ">=" = duration_h >= lim,
      "≥" = duration_h >= lim,
      FALSE
    )
    if (!stay_ok) return(FALSE)
    x <- trimws(sub(stay_pat, "", x, ignore.case = TRUE, perl = TRUE))
    x <- trimws(gsub("(?i)\\bAND\\b", " ", x, perl = TRUE))
  }

  if (!grepl("Mo|Tu|We|Th|Fr|Sa|Su|[0-2][0-9]:[0-5][0-9]", x, perl = TRUE)) return(TRUE)

  end_time <- arrival + lubridate::hours(duration_h)
  if (end_time <= arrival) return(FALSE)
  samples <- seq(arrival, end_time - 1, by = "15 min")
  if (!length(samples)) samples <- arrival
  segments <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])

  any(vapply(samples, function(tt) {
    any(vapply(segments, function(seg) condition_segment_matches(seg, tt), logical(1)))
  }, logical(1)))
}

osm_option_status <- function(tags, arrival, duration_h) {
  fee_keys <- c("fee", "parking:both:fee", "parking:left:fee", "parking:right:fee")
  cond_keys <- c("fee:conditional", "parking:both:fee:conditional",
                 "parking:left:fee:conditional", "parking:right:fee:conditional")
  max_keys <- c("maxstay", "parking:both:maxstay", "parking:left:maxstay", "parking:right:maxstay")
  access_keys <- c("access", "vehicle", "motor_vehicle", "parking:both:access",
                   "parking:left:access", "parking:right:access")

  access_vals <- tolower(na.omit(vapply(access_keys, function(k) tag_value(tags, k), character(1))))
  if (any(access_vals %in% c("no", "private", "customers", "customer", "permit", "residents", "resident", "delivery"))) {
    return(list(eligible = FALSE))
  }

  fee_vals <- tolower(na.omit(vapply(fee_keys, function(k) tag_value(tags, k), character(1))))
  cond_vals <- na.omit(vapply(cond_keys, function(k) tag_value(tags, k), character(1)))
  max_text <- first_tag_value(tags, max_keys)
  max_h <- parse_duration_hours(max_text)

  if (!is.na(max_h) && duration_h > max_h + 1e-9) return(list(eligible = FALSE))

  base_free <- any(fee_vals %in% c("no", "0", "free"))
  base_paid <- any(fee_vals %in% c("yes", "1"))

  if (base_free && !length(cond_vals)) {
    limit <- if (!is.na(max_h)) paste0("Max stay ", max_text) else "No fee explicitly mapped; check sign/time limit"
    return(list(eligible = TRUE, group = 1L, status = "Free (mapped)", cost = 0, limit = limit))
  }

  # If normally paid but OSM explicitly maps a free condition that covers the
  # whole requested stay, treat it as free at the selected arrival time.
  if (base_paid && length(cond_vals)) {
    free_conds <- unlist(lapply(cond_vals, conditional_value_conditions, wanted = "no"), use.names = FALSE)
    if (length(free_conds) && any(vapply(free_conds, condition_covers_stay, logical(1),
                                         arrival = arrival, duration_h = duration_h))) {
      limit <- if (!is.na(max_h)) paste0("Max stay ", max_text) else "Conditional free period; check sign"
      return(list(eligible = TRUE, group = 1L, status = "Free at this time", cost = 0, limit = limit))
    }
  }

  # Normally free with a conditional charge: prove that the charge condition
  # does not apply to any part of the requested stay when the syntax is simple
  # enough to evaluate. Otherwise keep it as a lower-ranked caution result.
  if (base_free && length(cond_vals)) {
    paid_conds <- unlist(lapply(cond_vals, conditional_value_conditions, wanted = "yes"), use.names = FALSE)
    if (length(paid_conds)) {
      applied <- vapply(paid_conds, condition_applies_any, logical(1),
                        arrival = arrival, duration_h = duration_h)
      if (any(applied, na.rm = TRUE)) return(list(eligible = FALSE))
      if (all(!is.na(applied)) && all(!applied)) {
        limit <- if (!is.na(max_h)) paste0("Max stay ", max_text) else "Conditional charging does not apply to selected stay; check sign"
        return(list(eligible = TRUE, group = 1L, status = "Free at this time", cost = 0, limit = limit))
      }
    }
    return(list(eligible = TRUE, group = 3L, status = "Potentially free", cost = NA_real_,
                limit = "Conditional fee recorded — check sign"))
  }

  list(eligible = FALSE)
}

load_osm_free_parking <- function(lat, lon, radius_m, arrival, duration_h) {
  key <- sprintf("%.4f|%.4f|%d", lat, lon, as.integer(radius_m))
  raw_elements <- NULL

  if (exists(key, envir = osm_cache, inherits = FALSE)) {
    raw_elements <- get(key, envir = osm_cache)
  } else {
    q <- sprintf('[out:json][timeout:25];(\n      nwr(around:%d,%.6f,%.6f)["amenity"~"^(parking|parking_space)$"]["fee"];\n      nwr(around:%d,%.6f,%.6f)["amenity"~"^(parking|parking_space)$"]["fee:conditional"];\n      way(around:%d,%.6f,%.6f)["highway"]["parking:both:fee"];\n      way(around:%d,%.6f,%.6f)["highway"]["parking:left:fee"];\n      way(around:%d,%.6f,%.6f)["highway"]["parking:right:fee"];\n      way(around:%d,%.6f,%.6f)["highway"]["parking:both:fee:conditional"];\n      way(around:%d,%.6f,%.6f)["highway"]["parking:left:fee:conditional"];\n      way(around:%d,%.6f,%.6f)["highway"]["parking:right:fee:conditional"];\n    );out center tags;',
      radius_m, lat, lon, radius_m, lat, lon, radius_m, lat, lon, radius_m, lat, lon,
      radius_m, lat, lon, radius_m, lat, lon, radius_m, lat, lon, radius_m, lat, lon)

    last_err <- NULL
    for (endpoint in OVERPASS_ENDPOINTS) {
      ans <- tryCatch({
        req <- request(endpoint) |>
          req_user_agent(APP_USER_AGENT) |>
          req_timeout(35) |>
          req_body_form(data = q)
        resp <- req_perform(req)
        resp_check_status(resp)
        resp_body_json(resp, simplifyVector = FALSE)$elements
      }, error = function(e) { last_err <<- e; NULL })
      if (!is.null(ans)) { raw_elements <- ans; break }
    }
    if (is.null(raw_elements)) stop(if (is.null(last_err)) "OpenStreetMap query failed" else conditionMessage(last_err))
    assign(key, raw_elements, envir = osm_cache)
  }

  if (!length(raw_elements)) return(tibble())
  rows <- list(); j <- 1

  for (el in raw_elements) {
    tags <- el$tags %||% list()
    stat <- osm_option_status(tags, arrival, duration_h)
    if (!isTRUE(stat$eligible)) next

    el_lat <- if (!is.null(el$lat)) as.numeric(el$lat) else if (!is.null(el$center$lat)) as.numeric(el$center$lat) else NA_real_
    el_lon <- if (!is.null(el$lon)) as.numeric(el$lon) else if (!is.null(el$center$lon)) as.numeric(el$center$lon) else NA_real_
    if (!is.finite(el_lat) || !is.finite(el_lon)) next

    kind <- if (identical(tag_value(tags, "amenity"), "parking_space")) "Parking space" else if (!is.na(tag_value(tags, "amenity"))) "Car park" else "Street parking"
    nm <- tag_value(tags, "name")
    street <- tag_value(tags, "addr:street")
    if (is.na(nm) || !nzchar(nm)) nm <- if (!is.na(street)) street else tag_value(tags, "ref")
    if (is.na(nm) || !nzchar(nm)) nm <- kind

    rows[[j]] <- tibble(
      lon = el_lon, lat = el_lat, option_name = nm, option_type = kind,
      status = stat$status, sort_group = stat$group, cost_num = stat$cost,
      rate_text = if (stat$group == 1L) "FREE" else "Check",
      limit_text = stat$limit,
      source = paste0("OpenStreetMap · ", el$type %||% "feature", " ", el$id %||% "")
    )
    j <- j + 1
  }

  if (!length(rows)) return(tibble())
  bind_rows(rows) |>
    mutate(.lon_key = round(lon, 6), .lat_key = round(lat, 6)) |>
    distinct(.lon_key, .lat_key, option_name, .keep_all = TRUE) |>
    select(-.lon_key, -.lat_key)
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
now_sydney <- lubridate::with_tz(Sys.time(), "Australia/Sydney")
default_arrival <- lubridate::ceiling_date(now_sydney, "30 minutes")
arrival_choices <- sprintf("%02d:%02d", rep(0:23, each = 2), rep(c(0, 30), 24))

ui <- page_sidebar(
  title = "Sydney Parking Finder",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 380,
    h4("Where are you going?"),
    textInput("address", NULL, placeholder = "e.g. 1 Martin Place, Sydney"),
    actionButton("search_address", "Find address", class = "btn-primary w-100"),
    div(class = "text-muted small mt-2", "Or click directly on the map."),
    hr(),
    dateInput("arrival_date", "Arrival date", value = as.Date(default_arrival)),
    selectInput("arrival_time", "Arrival time", choices = arrival_choices,
                selected = format(default_arrival, "%H:%M")),
    numericInput("duration", "Parking duration (hours)", value = 2,
                 min = 0.25, max = 24, step = 0.25),
    sliderInput("radius", "Search radius", min = 250, max = 2500,
                value = 1000, step = 250, post = " m"),
    actionButton("find_parking", "Find cheapest parking", class = "btn-success w-100"),
    hr(),
    uiOutput("destination_text"),
    uiOutput("source_status"),
    div(class = "alert alert-warning small mt-3",
        strong("Always check the sign: "),
        "The app combines City of Sydney meter data with explicitly mapped free parking from OpenStreetMap. Orange areas mean no City parking meters are installed there — they are not automatically free or unrestricted. Temporary restrictions and street signs override the app."
    ),
    div(class = "text-muted small",
        "Free parking coverage depends on what has been explicitly mapped in public data, so absence of a free result does not prove that no free parking exists nearby. Free-parking data © OpenStreetMap contributors (ODbL)."
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
  unmetered_zones <- reactiveVal(NULL)
  data_error <- reactiveVal(NULL)
  osm_error <- reactiveVal(NULL)
  osm_count <- reactiveVal(NA_integer_)
  results_data <- reactiveVal(NULL)

  selected_arrival <- reactive({
    req(input$arrival_date, input$arrival_time)
    as.POSIXct(paste(as.character(input$arrival_date), input$arrival_time),
               tz = "Australia/Sydney")
  })

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
      return(div(class = "text-muted small mt-3", "Parking data will load the first time you search."))
    }

    d <- parking_data()
    priced_n <- sum(!is.na(d$hourly_rate))
    nz <- unmetered_zones()
    zone_n <- if (is.null(nz)) 0 else nrow(nz)
    osm_txt <- if (is.na(osm_count())) {
      "Free-parking data not queried yet."
    } else if (!is.null(osm_error())) {
      paste0("OpenStreetMap free-parking query failed: ", osm_error())
    } else {
      paste0(format(osm_count(), big.mark = ","), " mapped free/potentially-free candidates found in the last search.")
    }

    div(class = "alert alert-success small mt-3",
        paste0(format(nrow(d), big.mark = ","), " City meter locations loaded; ",
               format(priced_n, big.mark = ","), " priced. ", zone_n,
               " official no-meter zone polygon(s) loaded. ", osm_txt))
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
    withProgress(message = "Loading City of Sydney parking data…", value = 0, {
      p <- Progress$new(session, min = 0, max = 1)
      on.exit(p$close(), add = TRUE)
      obj <- tryCatch(
        load_city_parking(p),
        error = function(e) {
          data_error(paste("Could not load live City parking data:", conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(obj) && !is.null(obj$meters) && nrow(obj$meters)) {
        parking_data(obj$meters)
        unmetered_zones(obj$unmetered_zones)
        ok <- TRUE
      }
    })
    ok
  }

  add_unmetered_overlay <- function(proxy, d, radius_m) {
    z <- unmetered_zones()
    if (is.null(z) || !nrow(z)) return(proxy)

    # City rate polygons occasionally contain duplicate vertices that S2 rejects.
    # The orange overlay is supplementary, so it must never be allowed to crash
    # the search. Work in GDA2020 / MGA Zone 56 (metres), repair polygons, and
    # simply omit the overlay if a geometry still cannot be processed.
    z2 <- tryCatch({
      z_mga <- st_transform(z, 7856)
      z_mga <- suppressWarnings(st_make_valid(z_mga))
      z_mga <- z_mga[!st_is_empty(z_mga), ]

      dest_mga <- st_sfc(
        st_point(c(d$lon, d$lat)),
        crs = 4326
      ) |> st_transform(7856)
      search_area <- st_buffer(dest_mga, dist = radius_m)

      hit <- lengths(st_intersects(z_mga, search_area)) > 0

      # Leaflet expects longitude/latitude.
      if (!any(hit)) NULL else st_transform(z_mga[hit, ], 4326)
    }, error = function(e) {
      warning("Unmetered-zone overlay skipped because City polygon geometry is invalid: ",
              conditionMessage(e))
      NULL
    })

    if (is.null(z2) || !nrow(z2)) return(proxy)

    proxy |>
      addPolygons(
        data = z2,
        color = "#E67E22", weight = 2, dashArray = "6,6",
        fillColor = "#F39C12", fillOpacity = 0.10,
        popup = "<b>No City parking meters in this area</b><br>Do not assume free parking — check street signs and permit/time restrictions.",
        group = "unmetered"
      )
  }

  observeEvent(input$find_parking, {
    if (!ensure_parking_data()) return()

    d <- destination()
    duration <- as.numeric(input$duration)
    arrival <- selected_arrival()
    radius_m <- as.numeric(input$radius)
    meters <- parking_data()

    # ----- City of Sydney meters ---------------------------------------------
    dest_sf <- st_sfc(st_point(c(d$lon, d$lat)), crs = 4326)
    dist_m <- as.numeric(st_distance(meters, dest_sf))
    m_idx <- which(dist_m <= radius_m & !is.na(meters$hourly_rate))

    meter_df <- tibble()
    if (length(m_idx)) {
      m <- meters[m_idx, ]
      mcoords <- st_coordinates(m)
      mdist <- dist_m[m_idx]
      short_free <- m$is_free_15 & duration <= 0.25
      cost <- m$hourly_rate * duration
      cost[short_free] <- 0

      meter_df <- tibble(
        lon = mcoords[, 1], lat = mcoords[, 2],
        option_name = ifelse(short_free, "15-minute free ticket", "City parking meter"),
        option_type = "Street meter",
        status = ifelse(short_free, "Free 15 min", "Paid meter"),
        sort_group = ifelse(short_free, 1L, 2L),
        cost_num = cost,
        rate_text = ifelse(short_free, "FREE", sprintf("$%.2f/hr", m$hourly_rate)),
        limit_text = ifelse(short_free,
                            "Free ticket; 15 minute maximum",
                            "Check the posted sign for operating hours and maximum stay"),
        source = paste0("City of Sydney · ", m$source_layer),
        distance_m = mdist,
        walk_min = pmax(1, round(mdist / 80))
      )
    }

    # ----- Explicitly mapped free parking -----------------------------------
    osm_error(NULL)
    osm <- withProgress(message = "Checking mapped free parking…", value = 0.4, {
      tryCatch(
        load_osm_free_parking(d$lat, d$lon, radius_m, arrival, duration),
        error = function(e) {
          osm_error(conditionMessage(e))
          tibble()
        }
      )
    })

    if (nrow(osm)) {
      osm_sf <- st_as_sf(osm, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
      odist <- as.numeric(st_distance(osm_sf, dest_sf))
      osm$distance_m <- odist
      osm$walk_min <- pmax(1, round(odist / 80))
      osm <- osm |> filter(distance_m <= radius_m)
    }
    osm_count(nrow(osm))

    candidates <- bind_rows(meter_df, osm)
    if (nrow(candidates)) {
      candidates <- candidates |>
        mutate(cost_sort = ifelse(is.na(cost_num), Inf, cost_num)) |>
        arrange(sort_group, cost_sort, distance_m) |>
        mutate(.lon_key = round(lon, 6), .lat_key = round(lat, 6)) |>
        distinct(.lon_key, .lat_key, status, .keep_all = TRUE) |>
        select(-cost_sort, -.lon_key, -.lat_key)
    }

    # ----- Map layers --------------------------------------------------------
    proxy <- leafletProxy("map") |>
      clearGroup("parking_free") |>
      clearGroup("parking_paid") |>
      clearGroup("parking_potential") |>
      clearGroup("unmetered") |>
      clearGroup("radius") |>
      removeControl("parking_legend") |>
      addCircles(d$lon, d$lat, radius = radius_m, fill = FALSE,
                 weight = 1, color = "#6c757d", group = "radius")

    proxy <- add_unmetered_overlay(proxy, d, radius_m)

    if (!nrow(candidates)) {
      results_data(NULL)
      showNotification(
        "No ranked parking options were found in that radius. Orange map areas are unmetered City zones, but are not automatically free.",
        type = "warning", duration = 9
      )
      return()
    }

    candidates <- candidates |>
      mutate(
        rank = row_number(),
        cost = case_when(
          sort_group == 1L ~ "$0.00",
          sort_group == 2L ~ sprintf("$%.2f", cost_num),
          TRUE ~ "Check"
        ),
        distance = ifelse(distance_m < 1000,
                          paste0(round(distance_m), " m"),
                          sprintf("%.1f km", distance_m / 1000)),
        walk = paste0(walk_min, " min"),
        arrival_text = format(arrival, "%a %d %b %H:%M", tz = "Australia/Sydney")
      )

    display <- candidates |>
      transmute(
        Rank = rank,
        Cost = cost,
        Status = status,
        Type = option_type,
        Parking = option_name,
        Distance = distance,
        Walk = walk,
        Rate = rate_text,
        `Limit / caution` = limit_text,
        Source = source
      )
    results_data(display)

    topn <- candidates |> slice_head(n = min(75, nrow(candidates)))
    popup_text <- sprintf(
      "<b>%s</b><br><b>%s</b> · %s<br>%s away · ~%s walk<br>%s<br><small>%s</small>",
      htmlEscape(topn$option_name), htmlEscape(topn$cost), htmlEscape(topn$status),
      htmlEscape(topn$distance), htmlEscape(topn$walk), htmlEscape(topn$limit_text),
      htmlEscape(topn$source)
    )

    free_idx <- which(topn$sort_group == 1L)
    paid_idx <- which(topn$sort_group == 2L)
    potential_idx <- which(topn$sort_group == 3L)

    if (length(free_idx)) {
      proxy <- proxy |> addCircleMarkers(
        lng = topn$lon[free_idx], lat = topn$lat[free_idx], radius = 8,
        color = "#198754", fillColor = "#198754", fillOpacity = 0.85,
        weight = 1, popup = popup_text[free_idx], group = "parking_free",
        clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)
      )
    }
    if (length(paid_idx)) {
      proxy <- proxy |> addCircleMarkers(
        lng = topn$lon[paid_idx], lat = topn$lat[paid_idx], radius = 7,
        color = "#0d6efd", fillColor = "#0d6efd", fillOpacity = 0.75,
        weight = 1, popup = popup_text[paid_idx], group = "parking_paid",
        clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)
      )
    }
    if (length(potential_idx)) {
      proxy <- proxy |> addCircleMarkers(
        lng = topn$lon[potential_idx], lat = topn$lat[potential_idx], radius = 7,
        color = "#fd7e14", fillColor = "#fd7e14", fillOpacity = 0.75,
        weight = 1, popup = popup_text[potential_idx], group = "parking_potential",
        clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE)
      )
    }

    # Emphasise the highest-ranked known option.
    best <- candidates[1, ]
    proxy <- proxy |>
      addCircleMarkers(
        lng = best$lon, lat = best$lat, radius = 12,
        color = "#212529", fillColor = if (best$sort_group == 1L) "#198754" else if (best$sort_group == 2L) "#0d6efd" else "#fd7e14",
        stroke = TRUE, weight = 4, fillOpacity = 0.9,
        popup = paste0("<b>Top ranked option</b><br>", htmlEscape(best$cost), " · ", htmlEscape(best$status)),
        group = if (best$sort_group == 1L) "parking_free" else if (best$sort_group == 2L) "parking_paid" else "parking_potential"
      ) |>
      addLegend(
        position = "bottomleft",
        layerId = "parking_legend",
        colors = c("#198754", "#0d6efd", "#fd7e14", "#E67E22"),
        labels = c("Free / free at selected time", "Paid meter", "Potentially free — check", "No City meters — check signs"),
        opacity = 0.9
      ) |>
      fitBounds(
        lng1 = min(c(d$lon, topn$lon), na.rm = TRUE),
        lat1 = min(c(d$lat, topn$lat), na.rm = TRUE),
        lng2 = max(c(d$lon, topn$lon), na.rm = TRUE),
        lat2 = max(c(d$lat, topn$lat), na.rm = TRUE)
      )
  })

  output$summary_cards <- renderUI({
    x <- results_data()
    if (is.null(x) || !nrow(x)) {
      return(div(class = "text-muted p-2", "Choose a destination, arrival time and duration, then search."))
    }
    layout_columns(
      col_widths = c(6, 6),
      value_box(title = "Best known cost", value = x$Cost[1], showcase = bsicons::bs_icon("cash-coin")),
      value_box(title = "Walk", value = x$Walk[1], showcase = bsicons::bs_icon("person-walking"))
    )
  })

  output$results <- renderDT({
    x <- results_data()
    req(!is.null(x), nrow(x) > 0)
    datatable(
      head(x, 40),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
      class = "compact stripe hover"
    )
  })
}

shinyApp(ui, server)
