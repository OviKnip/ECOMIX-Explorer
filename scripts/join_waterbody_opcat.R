# One-off data-prep script (not run as part of the Shiny app itself).
#
# Works out which operational catchment each EA waterbody polygon sits in
# (by greatest overlap area), the same approach fetch_operational_catchments.R
# uses for HYPE subbasins, so the Map page can colour waterbodies to match
# their operational catchment. Re-run whenever gis-data/EA_Waterbodies_AOI.geojson
# or gis-data/operational_catchments_wgs.shp is updated.
#
# Output (consumed by app.R):
#   data/waterbody_operational_catchment.csv - water_body -> opcat_name

library(sf)
library(dplyr)
library(here)

waterbodies <- read_sf(here("gis-data", "EA_Waterbodies_AOI.geojson")) %>%
  st_transform(4326) %>%
  st_make_valid()

opcat <- read_sf(here("gis-data", "operational_catchments_wgs.shp")) %>%
  mutate(.opcat_row = row_number())

hits <- st_intersects(waterbodies, opcat)

waterbody_opcat <- lapply(seq_len(nrow(waterbodies)), function(i) {
  cand <- hits[[i]]
  if (length(cand) == 0) {
    return(data.frame(water_body = waterbodies$water_body[i], opcat_name = NA_character_))
  }

  # A waterbody usually sits entirely within one operational catchment, but
  # border cases can clip a neighbour too - pick whichever it shares the
  # most area with.
  shared_area <- vapply(cand, function(j) {
    inter <- suppressWarnings(st_intersection(st_geometry(waterbodies)[i], st_geometry(opcat)[j]))
    if (length(inter) == 0) return(0)
    as.numeric(sum(st_area(inter)))
  }, numeric(1))

  best <- cand[which.max(shared_area)]
  data.frame(water_body = waterbodies$water_body[i], opcat_name = opcat$opcat_name[best])
}) %>% bind_rows()

write.csv(waterbody_opcat, here("data", "waterbody_operational_catchment.csv"), row.names = FALSE)
cat(sprintf(
  "Wrote data/waterbody_operational_catchment.csv (%d waterbodies, %d matched to an operational catchment)\n",
  nrow(waterbody_opcat), sum(!is.na(waterbody_opcat$opcat_name))
))
