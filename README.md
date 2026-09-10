# Sydney Parking Finder — R/Shiny MVP

This is a working first-pass R/Shiny app for finding **low-cost parking near a destination in Sydney**, starting with City of Sydney metered parking because that is where reliable public pricing data is currently available.

## What it does

- Search for a Sydney address or click the map.
- Choose parking duration and search radius.
- Downloads/discovers City of Sydney public ArcGIS parking-meter layers at runtime.
- Discovers the City of Sydney **Ticket parking rates** spatial layer at runtime rather than hard-coding its item ID.
- Assigns each meter to rate Area 1 or Area 2 when the public spatial data permits it.
- Uses the current published City of Sydney rates checked on **10 September 2026**: Area 1 = **$9/hour**, Area 2 = **$7/hour**.
- Detects 15-minute / 1/4P free-ticket locations where they are exposed in the official parking layers and only treats them as free for stays of 15 minutes or less.
- Ranks nearby options by estimated parking cost, then walking distance.
- Shows the best results on a map and in a searchable table.

## Run it

Open R/RStudio in this folder and run:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "leaflet", "DT", "dplyr", "sf",
  "httr2", "jsonlite", "lubridate", "htmltools"
))

shiny::runApp()
```

## Data sources

The MVP is designed around public government data rather than scraped commercial parking sites.

- City of Sydney parking meters / replacement ArcGIS item:
  https://cityofsydney.maps.arcgis.com/home/item.html?id=71bb12507a3240c4b12e7fbba5be58e1
- City of Sydney parking-meter information and current rate summary:
  https://www.cityofsydney.nsw.gov.au/transport-parking/finding-and-using-parking-meter
- City of Sydney Data Hub:
  https://data.cityofsydney.nsw.gov.au/
- Data.NSW parking datasets:
  https://www.data.nsw.gov.au/data/dataset/2-parking-and-council-data

The City of Sydney itself says its parking maps are a guide and drivers must follow posted signs. The app therefore displays the same warning.

## Geocoding

The address box uses the public OpenStreetMap Nominatim service. The code:

- sends an identifying User-Agent;
- limits requests to no more than one per second;
- caches repeated queries during the R session;
- restricts searches to the Sydney region.

That is appropriate for a small personal prototype, **not a high-traffic public deployment**. Before publishing widely, replace Nominatim with a production geocoder or self-host it. Policy: https://operations.osmfoundation.org/policies/nominatim/

## Important limitation

There is currently no single public feed containing every Sydney street space, legal restriction, price and live occupancy. Public data is fragmented by council and Transport for NSW. For that reason this version does **not** claim complete metropolitan-Sydney coverage.

The next useful integrations are:

1. TfNSW Car Park API — live occupancy for selected Park&Ride car parks.
2. Waverley traffic/parking-sign data.
3. Willoughby / Chatswood parking-sign data.
4. Council-owned off-street car parks and published price schedules.
5. Time-limit / sign-rule parsing so a result is filtered out when the requested stay would be illegal.
6. Arrival date/time and event/public-holiday logic.
7. A user preference for “cheapest”, “shortest walk”, or a combined value score.

## Cost calculation

For normal meters, the current MVP estimates:

`estimated cost = published hourly area rate × requested duration`

This is a comparison estimate, not a payment quote. Actual meter billing rules, signs and temporary controls may differ.
