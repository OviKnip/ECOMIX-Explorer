# One-off data-prep script (not run as part of the Shiny app itself).
#
# Downloads WFD surface water operational catchment polygons from the
# Environment Agency WFS, keeps only the ones overlapping the ECOMIX study
# catchments, and works out which operational catchment each HYPE subbasin
# sits in (by greatest overlap area). Re-run this whenever catchments_wgs /
# subbasins_wgs are updated, or to refresh from a newer WFS cycle.
#
# Outputs (consumed by app.R):
#   gis-data/operational_catchments_wgs.shp - operational catchments overlapping the study area
#   data/subbasin_operational_catchment.csv - subbasin Id -> associated opcat_id / opcat_name

library(sf)
library(dplyr)
library(here)

WFS_URL <- paste0(
  "https://environment.data.gov.uk/spatialdata/wfd-surface-water-operational-catchments-cycle-2/wfs",
  "?service=WFS&request=GetFeature&version=2.0.0",
  "&typeNames=dataset-777577cf-d465-11e4-846a-f0def148f590:WFD_Surface_Water_Operational_Catchments_Cycle_2",
  "&outputFormat=application/json&count=5000"
)

catchments <- read_sf(here("gis-data", "catchments_wgs.shp"))
catchment_union <- st_union(catchments)

# Query the WFS with a bounding-box filter (lat/lon order for EPSG:4326)
# so we don't have to pull the whole-of-England dataset.
bb <- st_bbox(catchments)
bbox_param <- sprintf(
  "&bbox=%f,%f,%f,%f,urn:ogc:def:crs:EPSG::4326",
  bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"]
)

opcat_raw <- read_sf(paste0(WFS_URL, bbox_param)) %>%
  st_transform(4326) %>%
  st_make_valid()

# The bbox is rectangular, so it can include catchments that never actually
# touch the study catchments - drop those.
overlaps <- lengths(st_intersects(opcat_raw, catchment_union)) > 0
opcat <- opcat_raw[overlaps, ] %>%
  select(opcat_id, opcat_name, mncat_name, rbd_name)

st_write(opcat, here("gis-data", "operational_catchments_wgs.shp"), delete_layer = TRUE)
cat(sprintf("Wrote %d operational catchments to gis-data/operational_catchments_wgs.shp\n", nrow(opcat)))

# ---- Associate each subbasin with the operational catchment it overlaps the most ----

subbasins <- read_sf(here("gis-data", "subbasins_wgs.shp")) %>%
  st_make_valid()

opcat_for_join <- opcat %>% mutate(.opcat_row = row_number())

hits <- st_intersects(subbasins, opcat_for_join)

subbasin_opcat <- lapply(seq_len(nrow(subbasins)), function(i) {
  cand <- hits[[i]]
  if (length(cand) == 0) {
    return(data.frame(subbasin = subbasins$Id[i], opcat_id = NA_real_, opcat_name = NA_character_))
  }

  # A subbasin usually sits entirely within one operational catchment, but
  # border cases can clip a neighbour too - pick whichever it shares the
  # most area with.
  shared_area <- vapply(cand, function(j) {
    inter <- suppressWarnings(st_intersection(st_geometry(subbasins)[i], st_geometry(opcat_for_join)[j]))
    if (length(inter) == 0) return(0)
    as.numeric(sum(st_area(inter)))
  }, numeric(1))

  best <- cand[which.max(shared_area)]
  data.frame(
    subbasin = subbasins$Id[i],
    opcat_id = opcat_for_join$opcat_id[best],
    opcat_name = opcat_for_join$opcat_name[best]
  )
}) %>% bind_rows()

write.csv(subbasin_opcat, here("data", "subbasin_operational_catchment.csv"), row.names = FALSE)
cat(sprintf(
  "Wrote data/subbasin_operational_catchment.csv (%d subbasins, %d matched to an operational catchment)\n",
  nrow(subbasin_opcat), sum(!is.na(subbasin_opcat$opcat_id))
))
