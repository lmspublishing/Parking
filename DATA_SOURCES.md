# Data-source notes

## City of Sydney

### Parking meters
The City of Sydney/Data.NSW catalogue directs users from older meter datasets to ArcGIS item:

`71bb12507a3240c4b12e7fbba5be58e1`

The app resolves the ArcGIS item, any child web maps, and FeatureServer/MapServer layers at runtime. This is intentionally more resilient than hard-coding a FeatureServer URL.

### Ticket parking rates
The app searches the City of Sydney ArcGIS organisation for an item titled **Ticket parking rates**, then resolves its spatial service. This lets the app associate meter points with Area 1/Area 2 where the layer is publicly queryable.

Current rates checked 10 Sep 2026 from City of Sydney:

- Area 1: $9/hour
- Area 2: $7/hour

### Free 15-minute parking
If the official parking item exposes a layer with a name containing `1/4`, `15`, `min` or `free`, those points are treated as $0 only when requested duration is <= 0.25 hours.

## NSW / other councils — planned

Data.NSW's parking collection includes or links to:

- TfNSW Car Park API with real-time occupancy for selected car parks.
- Sydney CBD loading-zone datasets.
- Waverley Council parking/traffic signs.
- Willoughby / Chatswood street parking signs.
- Off-street parking data.

These sources have different schemas and do not form a single metropolitan pricing dataset. They should be normalised into a common table before being ranked.

Recommended common schema:

| field | meaning |
|---|---|
| source_id | source-specific identifier |
| source | council / TfNSW / operator |
| parking_type | street_meter, free_street, council_carpark, park_ride |
| geometry | point or kerb segment |
| hourly_rate | effective hourly cost when meaningful |
| flat_rate | fixed/session rate |
| max_stay_min | maximum legal stay |
| valid_from / valid_to | date validity |
| rule_text | original sign/rate wording |
| spaces_total | capacity when known |
| spaces_available | live availability when known |
| updated_at | freshness |
