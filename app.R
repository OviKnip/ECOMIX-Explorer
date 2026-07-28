# Define path of the project
# path <- "D:/GITHUB/ECOMIX_Explorer/ECOMIX-Explorer/"
path <- here::here()

# Load Packages
library(leaflet)
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(ggsci)
library(DT)
library(ggplot2)
library(here)
library(arrow)
library(tidyr)

# Decimal numbers
options(scipen = 999)

safe_open_dataset <- function(dataset_path) {
  open_dataset(
    dataset_path,
    factory_options = list(
      exclude_invalid_files = TRUE,
      selector_ignore_prefixes = c(".", "_")
    )
  )
}

# Some parquet columns in DB_Proj_Forcing (e.g. p10/p50/p90) were written with
# a stray R "names" attribute (leftover from quantile() output), which Arrow
# can't round-trip and reports as "Invalid metadata$r" on every collect(). The
# values themselves are unaffected, so muffle just this known warning here
# rather than at every call site, while letting any other warning through.
collect_quiet <- function(x) {
  withCallingHandlers(
    collect(x),
    warning = function(w) {
      if (grepl("Invalid metadata\\$r", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


### 0. Design system (Modernist)
#
# Every colour, type and rule below comes from the Modernist token set; the
# matching CSS lives in styles.css and the two must be kept in step.

ec <- list(
  ground      = "#f3f2f2",
  surface     = "#eae9e9",
  ink         = "#201e1d",
  ink_muted   = "#444141",
  ink_dim     = "#605d5d",
  rule        = "#201e1d",
  rule_light  = "#d7d3d3",
  accent      = "#ec3013",
  accent_text = "#ae1800",
  accent_tint = "#ffe0d9",
  accent_ink  = "#7c1405"
)

# Discrete series palette. The system is mono - accent first for the series
# that carries the message, then ink and its neutral steps. Never more than
# one saturated line at a time.
ec_series <- c(ec$accent, ec$ink, ec$ink_dim, ec$accent_text, "#9b9797", "#b9b5b5")

scale_color_ecomix <- function(...) ggplot2::scale_color_manual(values = ec_series, ...)
scale_fill_ecomix  <- function(...) ggplot2::scale_fill_manual(values = ec_series, ...)

# ggplot theme matched to the interface: flat, flush left, one strong baseline
# rule, light grid, no panel border, legend top-left.
theme_ecomix <- function(base_size = 12) {
  ggplot2::theme_minimal(base_family = "Archivo", base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = ec$ink),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size, hjust = 0,
                                               margin = ggplot2::margin(b = 6)),
      plot.title.position = "plot",
      plot.caption     = ggplot2::element_text(colour = ec$ink_dim, size = base_size - 3, hjust = 0),
      plot.caption.position = "plot",
      panel.border     = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = ec$rule_light, linewidth = 0.3),
      axis.line.x      = ggplot2::element_line(colour = ec$ink, linewidth = 0.7),
      axis.ticks       = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(colour = ec$ink_dim, size = base_size - 2),
      axis.title       = ggplot2::element_text(colour = ec$ink_dim, size = base_size - 2),
      legend.position  = "top",
      legend.justification = "left",
      legend.title     = ggplot2::element_blank(),
      legend.key.height = grid::unit(10, "pt"),
      legend.margin    = ggplot2::margin(0, 0, 2, 0),
      strip.text       = ggplot2::element_text(face = "bold", hjust = 0, colour = ec$ink),
      plot.background  = ggplot2::element_rect(fill = ec$ground, colour = NA),
      panel.background = ggplot2::element_rect(fill = ec$ground, colour = NA),
      plot.margin      = ggplot2::margin(4, 8, 4, 2)
    )
}

# Placeholder shown instead of a blank/empty chart whenever the selected
# subbasin has no rows for the current dataset - e.g. a subbasin that's in
# the chemical predictions but outside the ~1446 subbasins the HYPE model
# itself covers (or vice versa). Without this, an empty result set renders
# as an unlabelled blank plot, which reads as a bug ("it flashed and
# vanished") rather than the data-coverage gap it actually is.
no_data_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 4.2, colour = ec$ink_dim) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = ec$ground, colour = NA))
}

# Basemap tile choice shared by every full-size leaflet map (Map,
# Spatial Datasets). Works on both a fresh `leaflet()` widget and a
# `leafletProxy()`, since both dispatch through the same addTiles/
# addProviderTiles S3 methods - callers just pair it with `clearGroup
# ("basetile")` first when switching on an existing map.
add_basemap_tiles <- function(map, choice = "osm") {
  switch(choice,
    esri_satellite = addProviderTiles(map, leaflet::providers$Esri.WorldImagery, group = "basetile"),
    esri_gray      = addProviderTiles(map, leaflet::providers$Esri.WorldGrayCanvas, group = "basetile"),
    addTiles(map, group = "basetile")
  )
}


## Load required datasets

# Study area
catchment_shp <- read_sf(dsn = here("gis-data"), layer = "catchments_wgs")

# HYPE Subbasins (modelling units)
subbasin_shp <- read_sf(dsn = here("gis-data"), layer = "subbasins_wgs")

# WFD surface water operational catchments (Environment Agency WFS),
# pre-filtered to the ones overlapping the study catchments, and each
# subbasin's dominant associated operational catchment (by shared area) -
# both produced by scripts/fetch_operational_catchments.R rather than
# fetched live, so the map doesn't depend on the WFS being reachable at app
# start.
opcat_shp <- read_sf(dsn = here("gis-data"), layer = "operational_catchments_wgs")

df_subbasin_opcat <- read.csv(here("data/subbasin_operational_catchment.csv"))

# Subbasin polygons joined to their associated operational catchment, used to
# colour the Map page.
subbasin_opcat_shp <- subbasin_shp %>%
  left_join(df_subbasin_opcat, by = c("Id" = "subbasin"))

opcat_levels <- sort(unique(na.omit(subbasin_opcat_shp$opcat_name)))

# Operational catchments are coloured by river system, so the map reads as
# "which river am I looking at" rather than 50 unrelated hues. Rivers with
# several operational catchments along their length (source to mouth, listed
# upstream to downstream) share one hue and are told apart by shade; every
# other catchment gets its own hue. Matching is done against the exact
# catchment names rather than a substring, since a naive substring match
# ("Calder" in "Middle Ribble - Settle to Calder") would wrongly pull in
# catchments from a different river.
opcat_river_groups <- list(
  "Ure"     = c("Upper Ure", "Middle and Lower Ure"),
  "Swale"   = c("Upper Swale", "Middle Swale", "Lower Swale"),
  "Derwent" = c("Derwent Upper -  Derbyshire", "Derwent Middle - Derbyshire",
                "Upper Derwent Yorkshire", "Middle Derwent Yorkshire", "Lower Derwent Yorkshire"),
  "Wharfe"  = c("Upper Wharfe", "Middle Wharfe and Washburn", "Lower Wharfe"),
  "Aire"    = c("Upper Aire", "Middle Aire", "Lower Aire"),
  "Nidd"    = c("Upper Nidd", "Middle and Lower Nidd"),
  "Ouse"    = c("Upper Ouse Yorkshire", "Lower Ouse Yorkshire"),
  "Don"     = c("Upper Don", "Middle Don", "Lower Don"),
  "Calder"  = c("Upper Calder", "Middle Calder", "Lower Calder", "Calder")
)

# Any catchment actually present that isn't in a hardcoded group above still
# needs a colour, so it becomes its own single-catchment group. This also
# covers catchments in the data that this list doesn't yet know about.
opcat_grouped_names <- unlist(opcat_river_groups, use.names = FALSE)
opcat_other <- setdiff(opcat_levels, opcat_grouped_names)
opcat_groups <- c(opcat_river_groups, setNames(as.list(opcat_other), opcat_other))
opcat_groups <- opcat_groups[order(names(opcat_groups))]

# One evenly spaced hue per group, then each catchment in a group gets a
# shade of that hue (light tint -> deep shade, upstream -> downstream). The
# sweep skips the red/orange arc around the accent colour (hue ~8) so no
# catchment's base colour can be mistaken for the accent-highlighted
# selection.
n_groups <- length(opcat_groups)
group_hues <- seq(40, 340, length.out = n_groups)

opcat_color_lookup <- c()
for (i in seq_len(n_groups)) {
  members <- opcat_groups[[i]]
  n_members <- length(members)
  lum <- if (n_members == 1) 50 else seq(78, 38, length.out = n_members)
  chroma <- if (n_members == 1) 55 else seq(45, 70, length.out = n_members)
  opcat_color_lookup[members] <- grDevices::hcl(h = group_hues[i], c = chroma, l = lum)
}

# The accent is still reserved for the current selection, so the one bright
# red polygon on screen is always the thing the user picked.
pal_opcat <- function(names) {
  unname(opcat_color_lookup[names])
}

# Operational catchment index for the Map page rail: name, subbasin count and
# the bounding box used to zoom the map when a row is clicked.
opcat_counts <- df_subbasin_opcat %>%
  filter(!is.na(opcat_name)) %>%
  count(opcat_name, name = "n_subbasins") %>%
  arrange(desc(n_subbasins))

opcat_bounds <- lapply(seq_len(nrow(opcat_shp)), function(i) {
  as.numeric(sf::st_bbox(opcat_shp[i, ]))
})
names(opcat_bounds) <- opcat_shp$opcat_name

# EA waterbody polygons (source-to-mouth reach units, finer-grained than
# operational catchments), each tagged with the operational catchment it
# overlaps most - see scripts/join_waterbody_opcat.R. Coloured with the same
# pal_opcat() lookup as subbasins so a waterbody and the catchment it sits in
# always read as the same hue.
waterbody_shp <- read_sf(here("gis-data", "EA_Waterbodies_AOI.geojson")) %>%
  st_transform(4326)
df_waterbody_opcat <- read.csv(here("data/waterbody_operational_catchment.csv"))
waterbody_opcat_shp <- waterbody_shp %>%
  left_join(df_waterbody_opcat, by = "water_body")

# Table with climate information
df_stats_climate <- read.csv(here("data/subbasin_climate.csv"))
df_stats_climate$subbasin <- as.numeric(gsub("X", "", df_stats_climate$subbasin))

# Table with subbasin statistics
df_stats_lc <- read.csv(here("data/subbasin_lc.csv"))
df_stats_lc$subbasin <- as.numeric(gsub("X", "", df_stats_lc$subbasin))

# Table with daily Water temperature (used for dummy testing plot)
df_temp <- read.csv(here("data/Dummy_Data_TT2.csv"))
df_temp$date <- as.Date(df_temp$date)

# ---- Chemical / physicochemical measured dataset ----
#
# Raw measured data is read as all-character columns so that "ND"
# (non-detect) values sit alongside numeric concentrations without read.csv
# silently coercing an entire analyte column to NA.
df_measured_raw <- read.csv(
  here("data/measured/comix_monitoring_data_Dashboard.csv"),
  colClasses = "character",
  check.names = FALSE
)
colnames(df_measured_raw)[1] <- "Site_id" # first header cell carries a stray BOM

# The file's final row is a footnote about the ND/NA convention, not a real
# measured record - drop any row without a parseable latitude.
df_measured_raw <- df_measured_raw[
  !is.na(suppressWarnings(as.numeric(df_measured_raw$Latitude))),
]

measured_meta_cols <- c("Site_id", "Site_full_name", "Latitude", "Longitude", "Week_Month", "Date")
measured_physchem_cols <- tail(setdiff(names(df_measured_raw), measured_meta_cols), 7)
measured_chemical_cols <- setdiff(names(df_measured_raw), c(measured_meta_cols, measured_physchem_cols))

df_measured_raw$Date <- as.Date(df_measured_raw$Date, format = "%d/%m/%Y")
df_measured_raw$Latitude <- as.numeric(df_measured_raw$Latitude)
df_measured_raw$Longitude <- as.numeric(df_measured_raw$Longitude)

# Strip the stray unit suffix baked into the "6PPD-Q" column name for display only.
measured_parameter_label <- function(x) sub("_ng L⁻¹$", "", x)

# One row per site, for the measured sites map.
df_measured_sites <- df_measured_raw %>%
  dplyr::select(Site_id, Site_full_name, Latitude, Longitude) %>%
  dplyr::distinct(Site_id, .keep_all = TRUE)

# Tidy long format: one row per site/date/parameter, with a detection status
# derived from the raw string ("ND" -> non-detect, blank -> no sample taken).
df_measured_long <- df_measured_raw %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(c(measured_chemical_cols, measured_physchem_cols)),
    names_to = "parameter",
    values_to = "value_raw"
  ) %>%
  dplyr::mutate(
    parameter_group = ifelse(parameter %in% measured_physchem_cols,
                              "Physicochemical parameter", "Organic micropollutant"),
    parameter_label = measured_parameter_label(parameter),
    status = dplyr::case_when(
      is.na(value_raw) | value_raw == "" ~ "No sample",
      value_raw == "ND" ~ "Non-detect",
      TRUE ~ "Detected"
    ),
    value_num = dplyr::case_when(
      status == "Detected" ~ suppressWarnings(as.numeric(value_raw)),
      status == "Non-detect" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(Site_id, Site_full_name, Latitude, Longitude, Date,
                parameter, parameter_label, parameter_group, status, value_num)

# Per-site summary powering the Site Details page's site table and its
# context strip: how often anything was detected, and the largest
# concentration seen.
df_measured_site_summary <- df_measured_long %>%
  dplyr::filter(parameter_group == "Organic micropollutant") %>%
  dplyr::group_by(Site_id, Site_full_name) %>%
  dplyr::summarise(
    n_screened = sum(status %in% c("Detected", "Non-detect")),
    n_detected = sum(status == "Detected"),
    detection_rate = ifelse(n_screened > 0, round(100 * n_detected / n_screened), NA_real_),
    max_value = suppressWarnings(max(value_num[status == "Detected"], na.rm = TRUE)),
    n_weeks = dplyr::n_distinct(Date),
    .groups = "drop"
  ) %>%
  dplyr::mutate(max_value = ifelse(is.finite(max_value), round(max_value, 1), NA_real_)) %>%
  dplyr::arrange(dplyr::desc(detection_rate))

# Grouped choices for the Site Details time series chemical/parameter
# selectors below (measured mode).
measured_parameter_choices <- list(
  "Organic micropollutants" = setNames(measured_chemical_cols, measured_parameter_label(measured_chemical_cols)),
  "Physicochemical parameters" = setNames(measured_physchem_cols, measured_physchem_cols)
)

# Short background information for a measured site or modelled water body
# (dummy placeholder data - see the file header of sitesInfo.txt /
# waterbodyInfo.txt). Parsed once at startup into a named list keyed by
# Site_id or water_body id. The name field accepts either "Site" (measured
# blocks) or "Water body" (modelled blocks); "Operational catchment" and
# landcover/population are optional and simply come back NA/empty when a
# block doesn't have them (true for every modelled block).
parse_entity_info <- function(path) {
  lines <- readLines(path, encoding = "UTF-8")
  lines <- lines[!(grepl("^\\s*#", lines) & !grepl("^### ", lines))] # drop comment lines, keep block headers
  block_starts <- grep("^### ", lines)

  entity_info <- list()
  for (i in seq_along(block_starts)) {
    entity_id <- sub("^### ", "", lines[block_starts[i]])
    start <- block_starts[i] + 1
    end <- if (i < length(block_starts)) block_starts[i + 1] - 1 else length(lines)
    block_lines <- lines[start:end]
    block_lines <- block_lines[nzchar(trimws(block_lines))]

    # Split each "Key: Value" line on the first colon only, so summary text
    # containing colons is not truncated.
    parsed <- regmatches(block_lines, regexec("^([^:]+):\\s*(.*)$", block_lines))
    keys <- vapply(parsed, `[`, character(1), 2)
    values <- vapply(parsed, `[`, character(1), 3)
    names(values) <- trimws(keys)

    landcover_idx <- grepl("^Land cover", names(values))
    landcover_names <- gsub("^Land cover - | \\(%\\)$", "", names(values)[landcover_idx])

    name <- unname(values["Site"])
    if (is.na(name)) name <- unname(values["Water body"])

    entity_info[[entity_id]] <- list(
      name = name,
      summary = unname(values["Summary"]),
      population = unname(values["Population (catchment)"]),
      opcat = unname(values["Operational catchment"]),
      landcover = setNames(as.numeric(values[landcover_idx]), landcover_names)
    )
  }
  entity_info
}

measured_site_info <- parse_entity_info(here("data/measured/sitesInfo.txt"))
modelled_site_info <- tryCatch(
  parse_entity_info(here("data/modelled/waterbodyInfo.txt")),
  error = function(e) {
    warning("data/modelled/waterbodyInfo.txt not found - run scripts/build_waterbody_info.R first. ",
            "Modelled water body summaries will be unavailable.")
    list()
  }
)

# Historical simulations at observation sites
db_name <- here("data/DB_Historical_Sim_Obs")
df_historical_observations <- safe_open_dataset(db_name) %>%
  collect()
df_observed_subbasins <- df_historical_observations %>% group_by(subbasin, variable) %>% slice(1) %>%
  dplyr::select(subbasin, variable)
df_historical_observations$date <- as.Date(df_historical_observations$date)

# One point per subbasin that has observed hydrology data, for the Map page's
# "Observed hydrology sites" layer - placed at the subbasin centroid since the
# observations themselves are subbasin-level, not a specific gauge location.
observed_hydro_vars <- df_observed_subbasins %>%
  group_by(subbasin) %>%
  summarise(variables = paste(sort(unique(variable)), collapse = ", "), .groups = "drop")

observed_hydro_centroids <- subbasin_opcat_shp %>%
  filter(Id %in% observed_hydro_vars$subbasin) %>%
  st_centroid()
observed_hydro_coords <- sf::st_coordinates(observed_hydro_centroids)
observed_hydro_points <- observed_hydro_centroids %>%
  sf::st_drop_geometry() %>%
  mutate(lon = observed_hydro_coords[, "X"], lat = observed_hydro_coords[, "Y"]) %>%
  left_join(observed_hydro_vars, by = c("Id" = "subbasin"))

# Keep Arrow dataset handles open once to avoid repeated metadata scans in renderers.
ds_proj_forcing <- safe_open_dataset(here("data/DB_Proj_Forcing"))
ds_proj_year <- safe_open_dataset(here("data/DB_Proj_Year"))
ds_proj_month <- safe_open_dataset(here("data/DB_Proj_Month"))
ds_proj_percentiles <- safe_open_dataset(here("data/DB_Proj_Percentiles"))

# Dynamic widget choices (ssp / period), precomputed once by
# scripts/consolidate_proj_data.R and saved alongside the flattened
# datasets above - cheaper than deriving them from Hive partition folder
# names (as a previous version of this file did), since those folders no
# longer exist once DB_Proj_* is a single flat parquet file per dataset.
dataset_choices <- tryCatch(
  readRDS(here("data/dataset_choices.rds")),
  error = function(e) list()
)

available_scenarios <- dataset_choices$ssp
if (length(available_scenarios) == 0) {
  available_scenarios <- c("Baseline", "SSP126", "SSP585")
}

prediction_period_choices <- dataset_choices$prediction_period
if (length(prediction_period_choices) == 0) {
  prediction_period_choices <- c("2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069", "2070-2080")
}

# Read map input for the Spatial Datasets choropleth. Kept as the full
# extremes table (rather than pre-filtered to one scenario/period/percentile
# as before) so the page's own dropdowns can select among them. "Susp.
# Sediments" is normalised here to match the casing used everywhere else
# (prediction_variable_choices, the monthly/yearly HYPE datasets) - the
# source file spells it "susp. Sediments", which silently produced zero rows
# whenever that variable was selected. The file also round-trips a stray
# dplyr grouping (by hype_variable/ssp/subbasin/unit/period, effectively one
# group per row) baked in when it was written from a grouped tibble -
# ungroup() it immediately, or distinct()/summarise() below would run
# per-group instead of over the whole table.
df_map_input <- read_parquet(here("data/Subbasin_Extremes.gz.parquet")) %>%
  dplyr::ungroup() %>%
  mutate(hype_variable = ifelse(hype_variable == "susp. Sediments", "Susp. Sediments", hype_variable))

# Scenario x period combinations actually present in the extremes data,
# labelled the same way the Data Explorer labels them ("SSP585 (2070-2080)"),
# used to drive the Spatial Datasets "Period" dropdown.
spatial_period_choices <- df_map_input %>%
  dplyr::distinct(ssp, period) %>%
  dplyr::arrange(period, ssp) %>%
  dplyr::mutate(label = paste0(ssp, " (", period, ")"),
                value = paste(ssp, period, sep = "|")) %>%
  { setNames(.$value, .$label) }

spatial_percentile_choices <- c("Low extreme (P0.1)" = "0.1", "High extreme (P99.9)" = "99.9")

# ---- Chemical concentration predictions (daily, by subbasin) ----
#
# All chemical x summary-statistic series live in a single long-format
# parquet file (date, site_id, subbasin, chemical, stat, conc_ng_L) built by
# scripts/build_chem_parquet.py from the original per-chemical wide CSVs
# under data-chem/ (~76.5M rows total, ~600MB). arrow::open_dataset() reads
# it lazily, so each lookup below pushes its chemical/stat/subbasin filters
# down to the parquet file instead of parsing a whole CSV. Unlike the HYPE
# datasets above, there's no SSP scenario or future period here - just a
# single 2018-2022 reconstructed daily record with three summary statistics
# (mean/median/90th) taking the place of p10/p50/p90. Variable ids are
# prefixed "chem_" so they can share the "Model variable" dropdown with the
# HYPE variables without colliding with hype_variable strings used elsewhere
# (e.g. in df_map_input).
chem_dir <- here("data-chem/1_DAILY_BY_SITE_conc_ng_L")
chem_dataset <- arrow::open_dataset(file.path(chem_dir, "1_DAILY_BY_SITE_conc_ng_L.parquet"))

# Display labels, aligned with the names already used on the measured Site
# Details page where the same substance appears there (e.g. "6PPD-Q",
# "Benzo[a]pyrene", "Erythromycin") so a chemical reads the same way across
# the app.
chem_display_lookup <- c(
  "6PPDQ" = "6PPD-Q", "Azoxystrobin" = "Azoxystrobin", "Cu" = "Cu",
  "Metformin" = "Metformin", "Zn" = "Zn", "anthracene" = "Anthracene",
  "azithromycin" = "Azithromycin", "benzoapyrene" = "Benzo[a]pyrene",
  "clarithromycin" = "Clarithromycin", "cypermethrin" = "Cypermethrin",
  "deltamethrin" = "Deltamethrin", "diclofenac" = "Diclofenac",
  "doramectin" = "Doramectin", "erithromycin" = "Erythromycin",
  "fipronil" = "Fipronil", "fluconazole" = "Fluconazole",
  "flufenacet" = "Flufenacet", "fluoranthene" = "Fluoranthene",
  "ibuprofen" = "Ibuprofen", "imidacloprid" = "Imidacloprid",
  "ivermectin" = "Ivermectin", "metazachlor" = "Metazachlor",
  "permethrin" = "Permethrin", "tebuconazole" = "Tebuconazole",
  "triallate" = "Triallate", "venlafaxine" = "Venlafaxine"
)

chem_keys <- sort(names(chem_display_lookup))
chem_variable_choices <- setNames(paste0("chem_", chem_keys), unname(chem_display_lookup[chem_keys]))
chem_variable_choices <- chem_variable_choices[order(names(chem_variable_choices))]

is_chem_variable <- function(v) !is.null(v) && nzchar(v) && grepl("^chem_", v)
chem_key_from_variable <- function(v) sub("^chem_", "", v)

chem_stat_choices <- c("Mean" = "mean", "Median" = "median", "90th percentile" = "90th")

# Real subbasin ids with chemical predictions - the same 537 for every
# chemical/statistic (the parquet already carries the resolved "subbasin" id
# alongside the chemical modelling's internal SITE_ID, so no separate key
# file/lookup is needed to translate between them here).
chem_subbasin_ids <- chem_dataset %>%
  dplyr::distinct(subbasin) %>%
  dplyr::collect() %>%
  dplyr::pull(subbasin) %>%
  sort()

chem_coverage_shp <- subbasin_shp %>% filter(Id %in% chem_subbasin_ids)

# Per-subbasin daily series (date, value) for one chemical/statistic -
# powering the Data Explorer projection plots and the "download this
# selection" / per-subbasin tabular download. Cheap enough (~0.05s) to read
# on demand and cache per subbasin actually visited, rather than eagerly
# loading all 78 x 537 series at startup.
chem_series_cache <- new.env(parent = emptyenv())
get_chem_subbasin_series <- function(chem_key, stat_val, subbasin_id) {
  key <- paste(chem_key, stat_val, subbasin_id, sep = "||")
  if (!exists(key, envir = chem_series_cache, inherits = FALSE)) {
    df <- chem_dataset %>%
      dplyr::filter(chemical == chem_key, stat == stat_val, subbasin == subbasin_id) %>%
      dplyr::select(date, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::arrange(date)
    df$year <- as.integer(format(df$date, "%Y"))
    df$month <- as.integer(format(df$date, "%m"))
    chem_series_cache[[key]] <- df
  }
  chem_series_cache[[key]]
}

# Full (all-subbasin) daily series for one chemical/statistic, long format -
# powers the Spatial Datasets time-averaged map and the Data Downloader's
# "all subbasins" / catchment-level summaries. Cached per chemical/statistic
# actually requested.
chem_full_cache <- new.env(parent = emptyenv())
get_chem_all_subbasins_daily <- function(chem_key, stat_val) {
  key <- paste(chem_key, stat_val, sep = "||")
  if (!exists(key, envir = chem_full_cache, inherits = FALSE)) {
    chem_full_cache[[key]] <- chem_dataset %>%
      dplyr::filter(chemical == chem_key, stat == stat_val) %>%
      dplyr::select(date, subbasin, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::mutate(unit = "ng/L")
  }
  chem_full_cache[[key]]
}

# Time-averaged (whole 2018-2022 record) value per subbasin for one
# chemical/statistic - the basis of the Spatial Datasets choropleth for
# chemicals, which (per product decision) has no period/scenario dimension.
chem_spatial_cache <- new.env(parent = emptyenv())
get_chem_spatial_summary <- function(chem_key, stat_val) {
  key <- paste(chem_key, stat_val, sep = "||")
  if (!exists(key, envir = chem_spatial_cache, inherits = FALSE)) {
    # Grouped by site_id (not subbasin): a handful of SITE_IDs in the
    # chemical modelling's key share the same real subbasin id, and rather
    # than averaging those together this keeps one row per SITE_ID's own
    # time-mean, matching the row-per-site-column shape the old per-file CSV
    # reads produced.
    chem_spatial_cache[[key]] <- chem_dataset %>%
      dplyr::filter(chemical == chem_key, stat == stat_val) %>%
      dplyr::group_by(site_id, subbasin) %>%
      dplyr::summarise(value = mean(conc_ng_L, na.rm = TRUE), .groups = "drop") %>%
      dplyr::select(subbasin, value) %>%
      dplyr::collect() %>%
      as.data.frame()
  }
  chem_spatial_cache[[key]]
}

# ---- Chemical concentration predictions (monthly, by water body) ----
#
# The "modelled" counterpart of the chem_* dataset above: instead of a daily
# record resolved to a subbasin, this is a monthly (2018-2022) record
# resolved to an EA water body (waterbody_id, matching the "water_body"
# property already used by waterbody_shp/waterbody_opcat_shp). Built by
# scripts/build_chem_parquet.py::build_monthly_parquet() from the same style
# of per-chemical wide CSVs as the daily dataset. Kept as clearly-prefixed
# siblings (wb_chem_*/wbchem_*) rather than folding into the chem_* names
# above, since those already unambiguously mean "daily, by subbasin".
wb_chem_dir <- here("data-chem/2_Modelling_results_by_waterbody_monthly_conc_ng_per_L")
wb_chem_dataset <- arrow::open_dataset(
  file.path(wb_chem_dir, "2_Modelling_results_by_waterbody_monthly_conc_ng_per_L.parquet")
)

# The modelled dataset's "chemical" values match chem_keys case-insensitively
# (only "Cypermethrin"/"cypermethrin" and "metformin"/"Metformin" actually
# differ in casing) - reconciled here from the data itself rather than a
# hand-maintained lookup, with a fail-fast check in case the two vocabularies
# ever diverge by more than a casing difference.
wb_chem_raw_labels <- wb_chem_dataset %>%
  # Casting to string before distinct()/collect() avoids an arrow
  # "Unifying differing dictionaries" error - the dictionary-encoded
  # "chemical" column has a different local dictionary per source CSV, and
  # combining a distinct scan across all of them at their native dictionary
  # type isn't supported, even though filtering/collecting matching rows
  # from a single chemical (as every other wb_chem_* lookup below does) is.
  dplyr::mutate(chemical = as.character(chemical)) %>%
  dplyr::distinct(chemical) %>%
  dplyr::collect() %>%
  dplyr::pull(chemical)
wb_chem_key_map <- setNames(chem_keys[match(tolower(wb_chem_raw_labels), tolower(chem_keys))], wb_chem_raw_labels)
stopifnot(!anyNA(wb_chem_key_map))
wb_chem_raw_label <- function(chem_key) names(wb_chem_key_map)[wb_chem_key_map == chem_key][1]

# Water bodies with modelled data (336 of the 370 polygons in
# EA_Waterbodies_AOI.geojson) - the other 34 are drawn on the map but greyed
# out and not selectable anywhere else in the app.
wb_chem_waterbody_ids <- wb_chem_dataset %>%
  dplyr::distinct(waterbody_id) %>%
  dplyr::collect() %>%
  dplyr::pull(waterbody_id) %>%
  sort()

waterbody_opcat_shp <- waterbody_opcat_shp %>%
  dplyr::mutate(has_model_data = water_body %in% wb_chem_waterbody_ids)

# Per-waterbody, per-chemical monthly series - Site Details time series panel
# and Data Explorer's water-body mode.
wb_chem_series_cache <- new.env(parent = emptyenv())
get_wb_chem_series <- function(chem_key, stat_val, wb_id) {
  key <- paste(chem_key, stat_val, wb_id, sep = "||")
  if (!exists(key, envir = wb_chem_series_cache, inherits = FALSE)) {
    raw <- wb_chem_raw_label(chem_key)
    df <- wb_chem_dataset %>%
      dplyr::filter(chemical == raw, stat == stat_val, waterbody_id == wb_id) %>%
      dplyr::select(date, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::arrange(date)
    df$year <- as.integer(format(df$date, "%Y"))
    df$month <- as.integer(format(df$date, "%m"))
    wb_chem_series_cache[[key]] <- df
  }
  wb_chem_series_cache[[key]]
}

# All 26 chemicals for one water body/statistic - Site Details occurrence
# heatmap.
wb_chem_grid_cache <- new.env(parent = emptyenv())
get_wb_chem_grid <- function(wb_id, stat_val) {
  key <- paste(wb_id, stat_val, sep = "||")
  if (!exists(key, envir = wb_chem_grid_cache, inherits = FALSE)) {
    wb_chem_grid_cache[[key]] <- wb_chem_dataset %>%
      dplyr::filter(waterbody_id == wb_id, stat == stat_val) %>%
      dplyr::select(date, chemical, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::mutate(chem_key = unname(wb_chem_key_map[as.character(chemical)]))
  }
  wb_chem_grid_cache[[key]]
}

# All-waterbody long series for one chemical/statistic - Data Downloader and
# the Spatial Datasets choropleth basis.
wb_chem_full_cache <- new.env(parent = emptyenv())
get_wb_chem_all_waterbodies_monthly <- function(chem_key, stat_val) {
  key <- paste(chem_key, stat_val, sep = "||")
  if (!exists(key, envir = wb_chem_full_cache, inherits = FALSE)) {
    raw <- wb_chem_raw_label(chem_key)
    wb_chem_full_cache[[key]] <- wb_chem_dataset %>%
      dplyr::filter(chemical == raw, stat == stat_val) %>%
      dplyr::select(date, waterbody_id, value = conc_ng_L) %>%
      dplyr::collect() %>%
      dplyr::mutate(unit = "ng/L")
  }
  wb_chem_full_cache[[key]]
}

# Time-averaged (2018-2022) value per water body for one chemical/statistic -
# the basis of the Spatial Datasets choropleth for modelled water bodies.
wb_chem_spatial_cache <- new.env(parent = emptyenv())
get_wb_chem_spatial_summary <- function(chem_key, stat_val) {
  key <- paste(chem_key, stat_val, sep = "||")
  if (!exists(key, envir = wb_chem_spatial_cache, inherits = FALSE)) {
    raw <- wb_chem_raw_label(chem_key)
    wb_chem_spatial_cache[[key]] <- wb_chem_dataset %>%
      dplyr::filter(chemical == raw, stat == stat_val) %>%
      dplyr::group_by(waterbody_id) %>%
      dplyr::summarise(value = mean(conc_ng_L, na.rm = TRUE), .groups = "drop") %>%
      dplyr::collect() %>%
      as.data.frame()
  }
  wb_chem_spatial_cache[[key]]
}

wbchem_variable_choices <- setNames(paste0("wbchem_", chem_keys), unname(chem_display_lookup[chem_keys]))
is_wbchem_variable <- function(v) !is.null(v) && nzchar(v) && grepl("^wbchem_", v)
wbchem_key_from_variable <- function(v) sub("^wbchem_", "", v)

# Site Details "Modelled" panel selector - plain chem_key values (unprefixed,
# like the measured chemical column names), no HYPE/physchem groups since
# modelled water bodies only ever have chemical data.
modelled_parameter_choices <- list(
  "Modelled chemicals" = setNames(chem_keys, unname(chem_display_lookup[chem_keys]))
)

# Cache expensive tabular download extracts per variable to avoid repeatedly
# scanning large Arrow datasets when users toggle between options.
tabular_download_cache <- new.env(parent = emptyenv())

build_tabular_download <- function(dl_variable) {
  if (dl_variable %in% c("precip", "temp")) {
    dl_lookup <- c("precip" = "Precipitation", "temp" = "Temperature")
    dl_variable_label <- unname(dl_lookup[dl_variable])

    df_download <- ds_proj_forcing %>%
      filter(variable == dl_variable_label, time_aggregation == "monthly") %>%
      select(subbasin, ssp, period, month, variable, p50, unit) %>%
      collect_quiet() %>%
      rename("scenario" = ssp, "value" = p50)
  } else {
    hype_lookup <- c(
      "runoff" = "discharge",
      "soil_moisture" = "Soil moisture",
      "water_temperature" = "water temperature",
      "susp_sediments" = "Susp. Sediments",
      "inorganic_nitrogen" = "Inorganic Nitrogen"
    )
    hype_variable_label <- unname(hype_lookup[dl_variable])

    df_download <- ds_proj_month %>%
      filter(
        hype_variable == hype_variable_label,
        prediction_percentile == "p50"
      ) %>%
      select(subbasin, ssp, period, month, hype_variable, prediction_percentile, p50_ensemble, unit) %>%
      collect() %>%
      rename("scenario" = ssp, "value" = p50_ensemble)
  }

  df_download %>%
    mutate("value" = round(value, 3)) %>%
    dplyr::select(subbasin, scenario, period, month, value, unit)
}

get_tabular_download <- function(dl_variable) {
  if (!exists(dl_variable, envir = tabular_download_cache, inherits = FALSE)) {
    tabular_download_cache[[dl_variable]] <- build_tabular_download(dl_variable)
  }
  tabular_download_cache[[dl_variable]]
}


## Definition of widgets

# Widget to select one or more scenarios.
#
# Defaults to Baseline + the same future SSP the Spatial Datasets tab
# defaults to (SSP585) - Baseline alone has no data for any future period,
# so a Baseline-only default made picking a period in the sidebar look
# like it did nothing (nothing plots for a future period until a future
# scenario is also selected).
scenario_default <- intersect(c("Baseline", "SSP585"), available_scenarios)
if (length(scenario_default) == 0) scenario_default <- available_scenarios[1]

widget_scenario <-  selectizeInput(
  inputId =  "scenario",
  label = "Scenarios",
  choices = available_scenarios,
  selected = scenario_default,
  multiple = TRUE)

# Climate variable (drop down menu - only one selection)
widget_climate_variable <- selectInput(
  inputId = "climate_variable",
  label = "Climate variable",
  choices = c("Precipitation", "Temperature"),
  selected = "Precipitation")

# Temporal resolution
widget_climate_resolution <- selectInput(
  inputId = "climate_resolution",
  label = "Resolution",
  choices = c("Monthly" = "monthly", "Yearly" = "annual"),
  selected = "Monthly")

# Observational variables - populated once a subbasin is selected
widget_observed_variable <- selectizeInput(
  inputId = "observation_variable",
  label = "Observed variable",
  choices = NULL,
  multiple = FALSE
)

# HYPE output variable.
#
# The Data Explorer now has ONE control rail rather than a sidebar per plot
# card, so its three projection tabs share a single input - the four
# duplicated ids the old nested-sidebar layout needed are gone. Spatial
# Datasets is a separate nav_panel rendered into the DOM at the same time, so
# it still needs its own id.
hype_variable_choices <- c(
  "Discharge" = "discharge",
  "Soil Moisture" = "Soil moisture",
  "Water Temperature" = "water temperature",
  "Susp. Sediments" =  "Susp. Sediments",
  "Inorganic Nitrogen" = "Inorganic Nitrogen"
)

# Grouped so the Model variable dropdown lists the hydrology/climate
# variables first, with the chemical concentration variables in their own
# group underneath. A flat label lookup (keyed by the widget value, working
# across both groups) is kept alongside for renderers that just need "what's
# the display name of the currently selected variable".
prediction_variable_choices <- list(
  "Hydrology & climate" = hype_variable_choices,
  "Chemical concentrations (ng/L)" = chem_variable_choices
)
prediction_variable_labels <- setNames(
  c(names(hype_variable_choices), names(chem_variable_choices), names(wbchem_variable_choices)),
  c(unname(hype_variable_choices), unname(chem_variable_choices), unname(wbchem_variable_choices))
)

# Spatial Datasets and Data Downloader also offer the modelled-by-waterbody
# chemicals as a variable choice; Data Explorer does not (it switches basis
# via its own Subbasin/Water body mode toggle instead, reusing the plain
# chem_variable_choices ids either way - see NAVBAR 2 in the server).
prediction_variable_choices_ext <- list(
  "Hydrology & climate" = hype_variable_choices,
  "Chemical concentrations · subbasin (daily, ng/L)" = chem_variable_choices,
  "Chemical concentrations · water body (monthly, ng/L)" = wbchem_variable_choices
)

widget_prediction_variable <- selectInput(
  inputId = "prediction_variable",
  label = "Model variable",
  choices = prediction_variable_choices,
  selected = "discharge")

widget_prediction_variable_spatial <- selectInput(
  inputId = "prediction_variable_spatial",
  label = "Model variable",
  choices = prediction_variable_choices_ext,
  selected = "discharge")

widget_spatial_period <- selectInput(
  inputId = "prediction_period_spatial",
  label = "Period",
  choices = spatial_period_choices,
  selected = if ("SSP585|2070-2080" %in% spatial_period_choices) "SSP585|2070-2080" else spatial_period_choices[1])

widget_spatial_percentile <- selectInput(
  inputId = "prediction_percentile_spatial",
  label = "Conditions",
  choices = spatial_percentile_choices,
  selected = "99.9")

# Chemicals have no scenario/period dimension - the Spatial Datasets map
# shows a single time-averaged (whole 2018-2022 record) value per subbasin,
# chosen by statistic instead.
widget_spatial_chem_stat <- selectInput(
  inputId = "prediction_stat_spatial_chem",
  label = "Statistic",
  choices = chem_stat_choices,
  selected = "median")

# Output conditions - i.e. prediction percentile
widget_prediction_percentile <- selectInput(
  inputId = "prediction_percentile",
  label = "Conditions",
  choices = c("Low (10th percentile)" = "p10",
              "Average (50th percentile)" = "p50",
              "High (90th percentile)" = "p90"),
  selected = "p50",
  multiple = TRUE)

# Prediction period
widget_prediction_period <- selectInput(
  inputId = "prediction_period",
  label = "Projection periods",
  choices = prediction_period_choices,
  selected = if ("2070-2080" %in% prediction_period_choices) "2070-2080" else prediction_period_choices[1],
  multiple = TRUE)

# Plot type (Absolute or relative change)
widget_plot_type <- radioButtons(
  inputId = "plot_type",
  label = "Values shown as",
  choices = c("Absolute", "Relative"),
  selected = "Absolute",
  inline = TRUE)

widget_download_variable <- selectInput(
  inputId = "dl_variable",
  label = "Variable",
  choices = list(
    "Hydrology & climate" = c("Precipitation" = "precip",
                              "Temperature" = "temp",
                              "Discharge" = "runoff",
                              "Soil Moisture" = "soil_moisture",
                              "Water Temperature" = "water_temperature",
                              "Susp. Sediments" = "susp_sediments",
                              "Inorganic Nitrogen" = "inorganic_nitrogen"),
    "Chemical concentrations · subbasin (daily, ng/L)" = chem_variable_choices,
    "Chemical concentrations · water body (monthly, ng/L)" = wbchem_variable_choices
  ),
  selected = "precip")

# Chemicals have three summary statistics (mean/median/90th) rather than the
# monthly p50 the HYPE downloads use - only shown once a chemical variable
# is picked (see the conditionalPanel around this in the Data Downloader UI).
widget_download_chem_stat <- selectInput(
  inputId = "dl_chem_stat",
  label = "Statistic",
  choices = chem_stat_choices,
  selected = "median")

widget_download_data_type <- selectInput(
  inputId = "dl_data_type",
  label = "Data type",
  choices = c("Tabular" = "tabular", "Spatial" = "spatial"),
  selected = "tabular"
)

widget_download_spatial_layer <- selectInput(
  inputId = "dl_spatial_layer",
  label = "Spatial layer",
  choices = c("Subbasins" = "subbasins", "Catchment" = "catchment", "Water bodies" = "waterbodies"),
  selected = "subbasins"
)

widget_download_format <- selectInput(
  inputId = "dl_format",
  label = "Format",
  choices = c("CSV" = "csv", "XLSX" = "xlsx", "Parquet" = "parquet"),
  selected = "csv"
)


### 0b. Context strip
#
# The selected subbasin used to exist only on the Map tab, and the Data
# Explorer restated four of its numbers in a row of value boxes that ate a
# whole band of the page. Both are replaced by one 96px strip repeated at the
# top of every panel: locator map, identity, inline stats, one action. Its
# geometry never changes; only which stats sit in the middle.
#
# page_navbar() has no slot above the panels, so the strip is the first child
# of each nav_panel(). Every panel therefore needs its own output ids - hence
# the `suffix` argument, mirrored by register_ctx() in the server.

ctx_stat <- function(label, value) {
  div(class = "ctx-stat",
      div(class = "ctx-label", label),
      div(class = "ctx-value", value))
}

ctx_strip <- function(suffix, stats, action = NULL, mini_map = TRUE,
                      id_label = "Subbasin") {
  div(
    class = "ctx",
    if (isTRUE(mini_map)) {
      div(class = "ctx-map", leafletOutput(paste0("ctx_map_", suffix), height = "94px"))
    },
    div(
      class = "ctx-id",
      div(class = "ctx-label", id_label),
      div(class = "ctx-id-value", textOutput(paste0("ctx_id_", suffix), inline = TRUE)),
      div(class = "ctx-id-sub", textOutput(paste0("ctx_sub_", suffix), inline = TRUE))
    ),
    div(class = "ctx-stats", stats),
    if (!is.null(action)) div(class = "ctx-act", action)
  )
}

# The four subbasin stats shared by Map / Data Explorer.
ctx_subbasin_stats <- function(suffix) {
  tagList(
    ctx_stat("Upstream area", textOutput(paste0("ctx_area_", suffix), inline = TRUE)),
    ctx_stat("Annual precip.", textOutput(paste0("ctx_precip_", suffix), inline = TRUE)),
    ctx_stat("Mean annual temp.", textOutput(paste0("ctx_temp_", suffix), inline = TRUE)),
    ctx_stat("Observations", uiOutput(paste0("ctx_obs_", suffix), inline = TRUE))
  )
}

# Segmented-control toggle between a "measured" (real samples) option and a
# "modelled" (model output) option, reused by the Site Details sidebar (which
# entity type is being browsed) and the Data Explorer sidebar (which spatial
# unit is being queried) - the two pages use different value/label pairs, so
# both are parameterized rather than hardcoded.
entity_type_toggle <- function(id, measured_panel, modelled_panel,
                               measured_label = "Measured", modelled_label = "Modelled",
                               values = c("measured", "modelled")) {
  div(class = "rail-toggle",
      navset_pill(id = id,
                  nav_panel(title = measured_label, value = values[1], measured_panel),
                  nav_panel(title = modelled_label, value = values[2], modelled_panel)))
}


### 1. User Interface

ui <- page_navbar(
  id = "main_nav",
  title = "ECOMIX Explorer",
  fillable = TRUE,
  theme = bs_theme(
    version = 5,
    bg = ec$ground,
    fg = ec$ink,
    primary = ec$accent,
    base_font = font_google("Archivo"),
    heading_font = font_google("Archivo"),
    "border-radius" = "0",
    "card-border-width" = "0",
    "card-box-shadow" = "none",
    "font-size-base" = "0.8125rem"
  ),
  header = tags$head(includeCSS(here("styles.css"))),

  # Panel 1: Map - choose a location
  nav_panel(
    title = "Map",
    ctx_strip(
      "map",
      ctx_subbasin_stats("map"),
      action = actionLink("ctx_map_go", "Open in explorer \u2192"),
      mini_map = FALSE
    ),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 320,
        title = "Find a location",
        selectizeInput(
          "subbasin_search",
          label = NULL,
          choices = NULL,
          options = list(placeholder = "Search subbasin id", maxOptions = 50)
        ),
        checkboxGroupInput(
          "map_layers",
          label = "Layers",
          choices = c("Subbasins" = "subbasins",
                      "Operational catchment boundaries" = "opcat",
                      "Waterbodies (modelled)" = "waterbodies",
                      "Measured sites" = "measured_sites",
                      "Observed hydrology sites" = "observed_hydro",
                      "Chemical data coverage" = "chem_coverage"),
          selected = "subbasins"
        ),
        selectInput(
          "map_basemap",
          label = "Basemap",
          choices = c("Streets (OSM)" = "osm",
                      "Satellite (Esri)" = "esri_satellite",
                      "Light gray (Esri)" = "esri_gray"),
          selected = "osm"
        ),
        div(class = "rail-head",
            paste0("Operational catchments \u00b7 ", nrow(opcat_counts))),
        div(class = "rail-list", uiOutput("opcat_rail"))
      ),
      div(
        class = "map-wrap",
        leafletOutput("basemap", width = "100%", height = "100%"),
        tags$div(
          class = "map-note",
          "Data compiled by York, Durham and Sheffield Universities (2026). Operational catchment boundaries and waterbodies \u00a9 Environment Agency copyright and/or database right 2026, licensed under the Open Government Licence v3.0"
        )
      )
    )
  ),

  ## Panel 1c: Site detail view - measured sites and modelled water bodies
  nav_panel(
    title = "Site Details",
    ctx_strip(
      "site",
      tagList(
        ctx_stat("Summary", div(style = "font-size:13px; font-weight:400; line-height:1.45; white-space:normal;",
                                textOutput("ctx_summary_site", inline = TRUE))),
        ctx_stat("Land cover", uiOutput("ctx_landcover_site", inline = TRUE))
      ),
      mini_map = TRUE,
      id_label = textOutput("ctx_idlabel_site", inline = TRUE)
    ),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 220,
        title = "Sites",
        entity_type_toggle(
          "site_entity_type",
          measured_panel = div(class = "rail-list", uiOutput("measured_rail")),
          modelled_panel = div(class = "rail-list", uiOutput("modelled_rail"))
        )
      ),
      layout_columns(
        col_widths = c(7, 5),
        gap = "0px",
        card(
          full_screen = TRUE,
          card_header(
            "Chemical occurrence",
            conditionalPanel(
              condition = "input.site_entity_type == 'modelled'",
              popover(
                actionLink("site_grid_opts", "Statistic \u25be", class = "pill"),
                title = "Occurrence grid",
                selectInput("site_grid_wb_stat", "Statistic", choices = chem_stat_choices, selected = "median")
              )
            ),
            span(class = "panel-note", textOutput("site_chem_grid_note", inline = TRUE))
          ),
          div(
            style = "position: relative; height: 100%;",
            plotOutput(
              "site_chem_grid",
              height = "100%",
              hover = hoverOpts(id = "site_chem_grid_hover", delay = 60, delayType = "debounce")
            ),
            uiOutput("site_chem_grid_tooltip")
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            "Time series",
            popover(
              actionLink("site_ts_opts", "Choose panels \u25be", class = "pill"),
              title = "Time series panels",
              selectInput("site_ts_chemical_1", "Panel 1", choices = measured_parameter_choices),
              selectInput("site_ts_chemical_2", "Panel 2", choices = measured_parameter_choices),
              selectInput("site_ts_chemical_3", "Panel 3", choices = measured_parameter_choices),
              selectInput("site_ts_chemical_4", "Panel 4", choices = measured_parameter_choices)
            ),
            span(class = "panel-note", "widest concentration range at this site")
          ),
          plotOutput("site_time_series", height = "100%")
        )
      )
    )
  ),

  ## Panel 2: Data explorer
  nav_panel(
    title = "Data Explorer",
    ctx_strip(
      "de",
      ctx_subbasin_stats("de"),
      action = actionLink("ctx_de_go", "Change location"),
      mini_map = TRUE
    ),
    layout_sidebar(
      fillable = TRUE,

      # ONE control rail for the whole page. Everything that used to live in
      # a sidebar nested inside each plot card sits here; the two remaining
      # per-panel choices (which climate variable, which observed variable)
      # are popovers in their own card headers.
      sidebar = sidebar(
        width = 248,
        title = "Controls \u00b7 all panels",
        entity_type_toggle(
          "de_entity_type",
          measured_panel = NULL,
          modelled_panel = selectizeInput(
            "de_waterbody_search",
            label = NULL,
            choices = NULL,
            options = list(placeholder = "Search water body", maxOptions = 50)
          ),
          measured_label = "Subbasin",
          modelled_label = "Water body",
          values = c("subbasin", "waterbody")
        ),
        conditionalPanel(
          condition = "input.de_entity_type == 'subbasin'",
          widget_scenario,
          # One shared period control for the whole page - it used to be
          # duplicated (a separate "Periods" dropdown fed only the Climate
          # panel), which looked like two controls doing the same job. Kept
          # outside the conditionalPanel below since the Climate panel needs
          # it regardless of which Model variable is selected; only
          # Absolute/Relative is HYPE-only (chemicals are a single historical
          # daily record with no future period or baseline to compare against).
          widget_prediction_period,
          conditionalPanel(
            condition = "input.prediction_variable && input.prediction_variable.indexOf('chem_') !== 0",
            widget_plot_type
          )
        ),
        widget_prediction_percentile,
        tags$hr(class = "hr"),
        widget_prediction_variable,
        downloadButton("download_selection", "Download this selection")
      ),

      layout_columns(
        col_widths = c(6, 6, 12),
        row_heights = c(1, 1.22),
        gap = "0px",

        # Climate
        card(
          full_screen = TRUE,
          card_header(
            "Climate",
            popover(
              actionLink("climate_opts", textOutput("climate_pill", inline = TRUE), class = "pill"),
              title = "Climate panel",
              widget_climate_variable,
              widget_climate_resolution
            )
          ),
          plotOutput("climate_plot", height = "100%")
        ),

        # Simulations vs observations
        card(
          full_screen = TRUE,
          card_header(
            "Simulated vs observed",
            popover(
              actionLink("observation_opts", textOutput("observation_pill", inline = TRUE), class = "pill"),
              title = "Observation panel",
              widget_observed_variable
            )
          ),
          plotOutput("observation_plot", height = "100%")
        ),

        # Projections - views of the same selection. "Daily" only makes
        # sense for chemicals (HYPE output here is monthly/yearly/percentile
        # only, no daily series) - it's shown/hidden server-side based on
        # the selected Model variable rather than always being present.
        navset_card_pill(
          id = "projection_tabs",
          full_screen = TRUE,
          title = textOutput("projection_title", inline = TRUE),
          nav_panel("Yearly", plotOutput("projections_yearly_plot", height = "100%")),
          nav_panel("Monthly", plotOutput("projections_monthly_plot", height = "100%")),
          nav_panel("Distributions", plotOutput("projections_cfc_plot", height = "100%")),
          nav_panel("Daily", plotOutput("projections_daily_plot", height = "100%"))
        )
      )
    )
  ),

  # Panel 3: Spatial mapping
  nav_panel(
    title = "Spatial Datasets",
    ctx_strip(
      "sp",
      tagList(
        ctx_stat("Value here", textOutput("ctx_value_sp", inline = TRUE)),
        ctx_stat("Rank", textOutput("ctx_rank_sp", inline = TRUE)),
        ctx_stat("Study-area range", textOutput("ctx_range_sp", inline = TRUE))
      ),
      action = actionLink("ctx_sp_go", "Change location"),
      mini_map = TRUE
    ),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 248,
        title = "Map layer",
        widget_prediction_variable_spatial,
        conditionalPanel(
          condition = paste0(
            "input.prediction_variable_spatial && ",
            "input.prediction_variable_spatial.indexOf('chem_') !== 0 && ",
            "input.prediction_variable_spatial.indexOf('wbchem_') !== 0"
          ),
          widget_spatial_period,
          widget_spatial_percentile
        ),
        conditionalPanel(
          condition = paste0(
            "input.prediction_variable_spatial && ",
            "(input.prediction_variable_spatial.indexOf('chem_') === 0 || ",
            "input.prediction_variable_spatial.indexOf('wbchem_') === 0)"
          ),
          widget_spatial_chem_stat
        ),
        selectInput(
          "sp_basemap",
          label = "Basemap",
          choices = c("Streets (OSM)" = "osm",
                      "Satellite (Esri)" = "esri_satellite",
                      "Light gray (Esri)" = "esri_gray"),
          selected = "osm"
        )
      ),
      div(
        class = "map-wrap",
        leafletOutput("prediction_map", width = "100%", height = "100%")
      )
    )
  ),

  # Panel 4: Download of data (or tables)
  nav_panel(
    title = "Data Downloader",
    ctx_strip(
      "dl",
      tagList(
        ctx_stat("Rows in selection", textOutput("ctx_rows_dl", inline = TRUE)),
        ctx_stat("Scenarios \u00d7 periods", textOutput("ctx_combos_dl", inline = TRUE)),
        ctx_stat("Estimated size", textOutput("ctx_size_dl", inline = TRUE))
      ),
      action = actionLink("ctx_dl_go", "Change location"),
      mini_map = TRUE
    ),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 280,
        title = "Build a download",
        widget_download_variable,
        conditionalPanel(
          condition = paste0(
            "input.dl_variable && ",
            "(input.dl_variable.indexOf('chem_') === 0 || input.dl_variable.indexOf('wbchem_') === 0)"
          ),
          widget_download_chem_stat
        ),
        widget_download_data_type,
        widget_download_spatial_layer,
        widget_download_format,
        helpText("Monthly p50 values for the current selection (full daily values for chemical concentrations; water body variables are always exported for all 336 modelled water bodies, since this page has no water body picker). Units travel with the file."),
        downloadButton("download_data", "Download data")
      ),
      card(
        card_header("Preview", span(class = "panel-note", textOutput("dl_preview_note", inline = TRUE))),
        DT::dataTableOutput("data_table")
      )
    )
  ),

  ## PANEL 5: Food Web Dynamic Model (Embedded Julia Dash)
  nav_panel(
    title = "Food Web Dynamics",
    tags$iframe(
      src = "http://127.0.0.1:8050",
      height = "100%",
      width = "100%",
      frameborder = "0",
      style = "margin: 0; border: 0;"
    )
  ),

  ## PANEL 6: Help
  nav_panel(
    title = "Help",
    div(style = "padding: 24px; max-width: 760px;", includeHTML(here("help.htm")))
  ),

  nav_spacer(),

  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("York", href = "https://www.york.ac.uk")),
    nav_item(tags$a("Durham", href = "https://durham.ac.uk")),
    nav_item(tags$a("Sheffield", href = "https://sheffield.ac.uk")),
  )
)


### 2. Server

server <- function(input, output, session) {

  ### NAVBAR 1 - MAP ###

  # use reactive values to store the id from observing the shape click
  rv <- reactiveVal()

  # Selected measured site (Site_id) and modelled water body (water_body
  # code), set by clicking a marker/polygon on the Map; drive the Site
  # Details tab. Kept as two separate reactiveVals (rather than one shared id
  # + a type flag) so toggling between Measured/Modelled on Site Details
  # preserves each side's own last selection instead of clobbering it.
  rv_measured_site <- reactiveVal()
  rv_waterbody <- reactiveVal()

  projection_sources <- reactive({
    list(
      forcing = ds_proj_forcing,
      year = ds_proj_year,
      month = ds_proj_month,
      percentiles = ds_proj_percentiles
    )
  })

  selected_climate <- reactive({
    req(rv())
    df_stats_climate %>% filter(subbasin == rv())
  })

  selected_lc <- reactive({
    req(rv())
    df_stats_lc %>% filter(subbasin == rv())
  })

  selected_historical <- reactive({
    req(rv())
    df_historical_observations %>% filter(subbasin == rv())
  })

  chem_stat_labels <- setNames(names(chem_stat_choices), unname(chem_stat_choices))

  # Daily series (subbasin mode) or monthly series (water body mode) for the
  # selected chemical and statistic (or statistics - the "Conditions"
  # selector allows more than one), feeding the chemical branch of the three
  # Projections plots and the per-subbasin downloads. Because every
  # Projections renderer already routes through this one reactive whenever
  # is_chem_variable() is true, the water-body branch below is the only
  # change those renderers need.
  chem_selected_series <- reactive({
    var <- input$prediction_variable
    req(is_chem_variable(var))
    chem_key <- chem_key_from_variable(var)
    # "Conditions" briefly still holds the HYPE p10/p50/p90 selection for one
    # reactive tick after prediction_variable switches to a chemical (the
    # observer that relabels it to mean/median/90th hasn't landed yet) - drop
    # anything that isn't a real chemical statistic so that tick renders
    # nothing instead of trying to open e.g. "..._p50_conc_by_SITE_ID.csv".
    stats <- intersect(unique(input$prediction_percentile), unname(chem_stat_choices))
    req(length(stats) > 0)

    if (identical(input$de_entity_type, "waterbody")) {
      req(rv_de_waterbody())
      dplyr::bind_rows(lapply(stats, function(s) {
        get_wb_chem_series(chem_key, s, rv_de_waterbody()) %>% dplyr::mutate(stat = s)
      }))
    } else {
      req(rv())
      if (!(rv() %in% chem_subbasin_ids)) return(data.frame())
      dplyr::bind_rows(lapply(stats, function(s) {
        get_chem_subbasin_series(chem_key, s, rv()) %>% dplyr::mutate(stat = s)
      }))
    }
  })

  selected_opcat <- reactive({
    if (is.null(rv())) return(NA_character_)
    df_tmp <- df_subbasin_opcat %>% filter(subbasin == rv())
    if (nrow(df_tmp) == 0 || is.na(df_tmp$opcat_name[1])) NA_character_ else df_tmp$opcat_name[1]
  })

  # Track clicks
  observeEvent(input$basemap_shape_click, {
    rv(input$basemap_shape_click$id)
  })

  # Search box on the Map rail
  updateSelectizeInput(session, "subbasin_search",
                       choices = sort(unique(subbasin_opcat_shp$Id)),
                       selected = character(0), server = TRUE)

  observeEvent(input$subbasin_search, {
    req(nzchar(input$subbasin_search))
    rv(as.numeric(input$subbasin_search))
  }, ignoreInit = TRUE)

  # Open Data Explorer from map popup link and keep selected subbasin in sync.
  observeEvent(input$open_data_explorer, {
    rv(as.numeric(input$open_data_explorer))
    bslib::nav_select("main_nav", selected = "Data Explorer", session = session)
  })

  # Context strip actions
  observeEvent(input$ctx_map_go, {
    bslib::nav_select("main_nav", selected = "Data Explorer", session = session)
  })
  observeEvent(input$ctx_de_go, {
    bslib::nav_select("main_nav", selected = "Map", session = session)
  })
  observeEvent(input$ctx_sp_go, {
    bslib::nav_select("main_nav", selected = "Map", session = session)
  })
  observeEvent(input$ctx_dl_go, {
    bslib::nav_select("main_nav", selected = "Map", session = session)
  })

  ## Reactive selection of observed variables
  observation_choices <- reactive({
    if (is.null(rv())) {
      c("Please select a subbasin")
    } else {
      df_tmp <- df_observed_subbasins %>% filter(subbasin == rv())
      if (nrow(df_tmp) == 0) {
        c("No Observations available")
      } else {
        df_tmp[["variable"]]
      }
    }
  })

  observeEvent(observation_choices(), {
    updateSelectizeInput(
      session,
      inputId = "observation_variable",
      choices = observation_choices(),
      selected = NULL
    )
  })

  # Base map. Subbasins are drawn in the neutral ramp so the accent can mark
  # the current selection unambiguously.
  output$basemap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      add_basemap_tiles("osm") %>%
      setView(lng = -1.16, lat = 53.75, zoom = 8.5) %>%
      addPolygons(data = subbasin_opcat_shp,
                  fill = TRUE,
                  fillColor = ~ifelse(is.na(opcat_name), "#e0dede", pal_opcat(opcat_name)),
                  fillOpacity = 0.55,
                  color = ec$ink,
                  opacity = 0.45,
                  weight = 1,
                  popup = ~paste0(
                    "<strong>Subbasin id: </strong>", Id,
                    "<br><strong>Operational catchment: </strong>", ifelse(is.na(opcat_name), "None", opcat_name),
                    "<br><a href='#' onclick=\"Shiny.setInputValue('open_data_explorer', '",
                    Id,
                    "', {priority: 'event'}); return false;\">Open in Data Explorer</a>"
                  ),
                  layerId = ~Id,
                  group = "Subbasins")
  })

  # Layer toggles from the rail (replacing leaflet's own layers control, so
  # the map surface carries no floating chrome of its own).
  observeEvent(input$map_layers, {
    proxy <- leafletProxy("basemap")
    proxy %>% clearGroup("opcat") %>% clearGroup("waterbodies") %>%
      clearGroup("measured_sites") %>% clearGroup("observed_hydro") %>% clearGroup("chem_coverage")

    if ("subbasins" %in% input$map_layers) {
      proxy %>% showGroup("Subbasins")
    } else {
      proxy %>% hideGroup("Subbasins")
    }

    if ("opcat" %in% input$map_layers) {
      proxy %>% addPolygons(
        data = opcat_shp, fill = FALSE, color = ec$ink,
        weight = 2.5, opacity = 0.9, label = ~opcat_name, group = "opcat"
      )
    }
    if ("waterbodies" %in% input$map_layers) {
      proxy %>% addPolygons(
        data = waterbody_opcat_shp,
        fill = TRUE,
        fillColor = ~ifelse(!has_model_data, ec$rule_light,
                            ifelse(is.na(opcat_name), "#e0dede", pal_opcat(opcat_name))),
        fillOpacity = ~ifelse(has_model_data, 0.45, 0.2),
        color = ec$ink,
        weight = 0.75,
        opacity = 0.5,
        layerId = ~water_body,
        label = ~paste0(water_bod0, ifelse(is.na(opcat_name), "", paste0(" · ", opcat_name)),
                        ifelse(has_model_data, "", " (no modelled data)")),
        popup = ~ifelse(
          has_model_data,
          paste0(
            "<strong>", water_bod0, "</strong>",
            ifelse(is.na(opcat_name), "", paste0("<br>", opcat_name)),
            "<br><a href='#' onclick=\"Shiny.setInputValue('open_waterbody_details', '",
            water_body,
            "', {priority: 'event'}); return false;\">Open in Site Details \u2192</a>"
          ),
          paste0("<strong>", water_bod0, "</strong><br><em>No modelled chemical data available</em>")
        ),
        group = "waterbodies"
      )
    }
    if ("measured_sites" %in% input$map_layers) {
      proxy %>% addCircleMarkers(
        data = df_measured_sites, lng = ~Longitude, lat = ~Latitude,
        layerId = ~Site_id,
        radius = 5, color = "#ffffff", weight = 1.5, fillColor = ec$ink,
        fillOpacity = 0.95, label = ~Site_full_name,
        popup = ~paste0(
          "<strong>", Site_full_name, "</strong>",
          "<br><a href='#' onclick=\"Shiny.setInputValue('open_site_details', '",
          Site_id,
          "', {priority: 'event'}); return false;\">Open in Site Details \u2192</a>"
        ),
        group = "measured_sites"
      )
    }
    if ("observed_hydro" %in% input$map_layers) {
      proxy %>% addCircleMarkers(
        data = observed_hydro_points, lng = ~lon, lat = ~lat,
        layerId = ~Id,
        radius = 5, color = ec$ink, weight = 1.5, fillColor = "#0f6659",
        fillOpacity = 0.9,
        popup = ~paste0(
          "<strong>Subbasin id: </strong>", Id,
          "<br><strong>Observed variables: </strong>", variables,
          "<br><a href='#' onclick=\"Shiny.setInputValue('open_data_explorer', '",
          Id,
          "', {priority: 'event'}); return false;\">Open in Data Explorer \u2192</a>"
        ),
        group = "observed_hydro"
      )
    }
    if ("chem_coverage" %in% input$map_layers) {
      proxy %>% addPolygons(
        data = chem_coverage_shp, fill = TRUE, fillColor = "#2f6690", fillOpacity = 0.22,
        color = "#2f6690", weight = 1.5, opacity = 0.85,
        label = ~paste0("Subbasin ", Id, " · chemical data available"),
        group = "chem_coverage"
      )
    }
  }, ignoreNULL = FALSE)

  # Basemap tile switcher.
  observeEvent(input$map_basemap, {
    leafletProxy("basemap") %>%
      clearGroup("basetile") %>%
      add_basemap_tiles(input$map_basemap)
  }, ignoreInit = TRUE)

  # Open Site Details from a measured site marker's popup link, same
  # "click marker -> pick from its popup" pattern as the subbasin/observed-
  # hydrology "Open in Data Explorer" links.
  observeEvent(input$open_site_details, {
    rv_measured_site(input$open_site_details)
    bslib::nav_select("site_entity_type", selected = "measured", session = session)
    bslib::nav_select("main_nav", selected = "Site Details", session = session)
  })

  # Open Site Details from a water body polygon's popup link, landing on the
  # "Modelled" side of the Site Details toggle with that water body selected.
  observeEvent(input$open_waterbody_details, {
    rv_waterbody(input$open_waterbody_details)
    bslib::nav_select("site_entity_type", selected = "modelled", session = session)
    bslib::nav_select("main_nav", selected = "Site Details", session = session)
  })

  # Highlight the selected subbasin in the accent.
  observeEvent(rv(), {
    shp <- subbasin_opcat_shp %>% filter(Id == rv())
    if (nrow(shp) == 0) return()
    leafletProxy("basemap") %>%
      clearGroup("selection") %>%
      addPolygons(data = shp, fill = TRUE, fillColor = ec$accent, fillOpacity = 0.35,
                  color = ec$accent, weight = 3, opacity = 1, group = "selection")
  })

  # Operational catchment rail: click a row to frame that catchment.
  output$opcat_rail <- renderUI({
    active <- selected_opcat()
    tagList(lapply(seq_len(nrow(opcat_counts)), function(i) {
      nm <- opcat_counts$opcat_name[i]
      div(
        class = paste("rail-row", if (!is.na(active) && identical(active, nm)) "is-active" else ""),
        onclick = paste0("Shiny.setInputValue('opcat_pick', ", jsonlite::toJSON(nm, auto_unbox = TRUE),
                         ", {priority:'event'})"),
        div(class = "rail-row-label",
            span(class = "rail-swatch", style = paste0("background:", pal_opcat(nm), ";")),
            span(nm)),
        span(class = "rail-num", opcat_counts$n_subbasins[i])
      )
    }))
  })

  observeEvent(input$opcat_pick, {
    bb <- opcat_bounds[[input$opcat_pick]]
    req(!is.null(bb))
    leafletProxy("basemap") %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
  })


  ### CONTEXT STRIP ###
  #
  # One registration call per panel; every panel gets its own output ids
  # because nav_panel() renders all panels into the DOM at once.

  ctx_area <- reactive({
    if (is.null(rv())) return("\u2014")
    df <- selected_lc()
    v <- df$value[df$variable == "Upstream area"]
    if (length(v) == 0) return("\u2014")
    paste0(round(v / 1e6, 2), " km\u00b2")
  })

  ctx_precip <- reactive({
    if (is.null(rv())) return("\u2014")
    paste0(round(selected_climate()$precip[1], 0), " mm")
  })

  ctx_temp <- reactive({
    if (is.null(rv())) return("\u2014")
    paste0(round(selected_climate()$maat[1], 1), " \u00b0C")
  })

  register_ctx_subbasin <- function(suffix, mini_map = TRUE) {
    output[[paste0("ctx_id_", suffix)]] <- renderText({
      if (is.null(rv())) "No location" else as.character(rv())
    })
    output[[paste0("ctx_sub_", suffix)]] <- renderText({
      if (is.null(rv())) return("Pick a subbasin on the map")
      nm <- selected_opcat()
      if (is.na(nm)) "No operational catchment" else nm
    })
    output[[paste0("ctx_area_", suffix)]] <- renderText(ctx_area())
    output[[paste0("ctx_precip_", suffix)]] <- renderText(ctx_precip())
    output[[paste0("ctx_temp_", suffix)]] <- renderText(ctx_temp())
    output[[paste0("ctx_obs_", suffix)]] <- renderUI({
      if (is.null(rv())) return(span(style = "font-size:13px; font-weight:400; color:#605d5d;", "\u2014"))
      vars <- df_observed_subbasins %>% filter(subbasin == rv()) %>% pull(variable)
      if (length(vars) == 0) {
        return(span(style = "font-size:13px; font-weight:400; color:#605d5d;", "none at this subbasin"))
      }
      tagList(lapply(vars, function(v) span(class = "ctx-tag", v)))
    })

    if (isTRUE(mini_map)) {
      output[[paste0("ctx_map_", suffix)]] <- renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE,
                                         dragging = FALSE, scrollWheelZoom = FALSE,
                                         doubleClickZoom = FALSE)) %>%
          addTiles() %>%
          setView(lng = -1.16, lat = 53.75, zoom = 7)
      })

      observeEvent(rv(), {
        shp <- subbasin_opcat_shp %>% filter(Id == rv())
        if (nrow(shp) == 0) return()
        bb <- as.numeric(sf::st_bbox(shp))
        leafletProxy(paste0("ctx_map_", suffix)) %>%
          clearShapes() %>%
          addPolygons(data = shp, fillColor = ec$accent, fillOpacity = 0.9,
                      color = ec$accent, weight = 2) %>%
          fitBounds(bb[1], bb[2], bb[3], bb[4])
      })
    }
  }

  register_ctx_subbasin("map", mini_map = FALSE)
  register_ctx_subbasin("de")
  register_ctx_subbasin("sp")
  register_ctx_subbasin("dl")


  ### SITE DETAILS ###

  # Resets the four time series selectors to the active entity's top-range
  # chemicals, so switching sites/water bodies or toggling Measured/Modelled
  # gives a fresh, sensible default rather than carrying over the previous
  # selection's picks.
  observeEvent(list(input$site_entity_type, rv_measured_site(), rv_waterbody()), {
    is_modelled <- identical(input$site_entity_type, "modelled")
    if (is_modelled) {
      req(rv_waterbody())
      choices <- modelled_parameter_choices
      top4 <- selected_waterbody_top_chemicals()$chem_key
      fallback <- chem_keys
    } else {
      req(rv_measured_site())
      choices <- measured_parameter_choices
      top4 <- selected_measured_site_top_chemicals()$parameter
      fallback <- measured_chemical_cols
    }
    if (length(top4) < 4) {
      top4 <- c(top4, setdiff(fallback, top4))[seq_len(4)]
    }
    for (i in seq_len(4)) {
      updateSelectInput(session, paste0("site_ts_chemical_", i), choices = choices, selected = top4[i])
    }
  }, ignoreNULL = FALSE)

  # Full measured record (all parameters, all weeks) for the selected site
  selected_measured_site_long <- reactive({
    req(rv_measured_site())
    df_measured_long %>% dplyr::filter(Site_id == rv_measured_site())
  })

  selected_measured_site_summary <- reactive({
    req(rv_measured_site())
    df_measured_site_summary %>% dplyr::filter(Site_id == rv_measured_site())
  })

  # Site Details context strips (site/water-body-scoped, so registered
  # separately from the subbasin strips above). Both entity types share one
  # set of ctx_* outputs, branching internally on input$site_entity_type.
  measured_site_display_name <- reactive({
    if (is.null(rv_measured_site())) return(NULL)
    row <- df_measured_sites %>% filter(Site_id == rv_measured_site())
    if (nrow(row) == 0) NULL else row$Site_full_name[1]
  })

  waterbody_display_name <- reactive({
    if (is.null(rv_waterbody())) return(NULL)
    row <- waterbody_opcat_shp %>% sf::st_drop_geometry() %>% filter(water_body == rv_waterbody())
    if (nrow(row) == 0) NULL else row$water_bod0[1]
  })

  output$ctx_idlabel_site <- renderText({
    if (identical(input$site_entity_type, "modelled")) "Modelled water body" else "Measured site"
  })

  output$ctx_id_site <- renderText({
    if (identical(input$site_entity_type, "modelled")) {
      waterbody_display_name() %||% "No water body"
    } else {
      measured_site_display_name() %||% "No site"
    }
  })
  output$ctx_sub_site <- renderText({
    if (identical(input$site_entity_type, "modelled")) {
      if (is.null(rv_waterbody())) return("Pick a water body in the list")
      row <- waterbody_opcat_shp %>% sf::st_drop_geometry() %>% filter(water_body == rv_waterbody())
      if (nrow(row) == 0) return(rv_waterbody())
      paste0(rv_waterbody(), ifelse(is.na(row$opcat_name[1]), "", paste0(" · ", row$opcat_name[1])))
    } else {
      if (is.null(rv_measured_site())) return("Pick a site in the list")
      row <- df_measured_sites %>% filter(Site_id == rv_measured_site())
      paste0(rv_measured_site(), " · ", round(row$Latitude[1], 4), ", ", round(row$Longitude[1], 4))
    }
  })
  output$ctx_summary_site <- renderText({
    if (identical(input$site_entity_type, "modelled")) {
      if (is.null(rv_waterbody())) return("Select a modelled water body to see its background.")
      info <- modelled_site_info[[rv_waterbody()]]
      if (is.null(info)) return(paste("No water body information available yet for", rv_waterbody()))
      info$summary
    } else {
      if (is.null(rv_measured_site())) return("Select a measured site to see its background.")
      info <- measured_site_info[[rv_measured_site()]]
      if (is.null(info)) return(paste("No site information available yet for", rv_measured_site()))
      info$summary
    }
  })
  output$ctx_landcover_site <- renderUI({
    is_modelled <- identical(input$site_entity_type, "modelled")
    active_id <- if (is_modelled) rv_waterbody() else rv_measured_site()
    if (is.null(active_id)) return(NULL)
    info <- if (is_modelled) modelled_site_info[[rv_waterbody()]] else measured_site_info[[rv_measured_site()]]
    if (is.null(info) || length(info$landcover) == 0) return(NULL)

    lc <- info$landcover
    shades <- grDevices::colorRampPalette(c(ec$accent_text, ec$accent_tint))(length(lc))
    tagList(
      div(style = "display:flex; height:14px; margin-top:2px;",
          lapply(seq_along(lc), function(i) {
            div(style = paste0("width:", lc[i], "%; background:", shades[i], ";"))
          })),
      div(style = "display:flex; flex-wrap:wrap; gap:10px; margin-top:6px; font-size:10px; font-weight:400; color:#605d5d;",
          lapply(seq_along(lc), function(i) span(paste0(names(lc)[i], " ", lc[i]))))
    )
  })

  # Small locator map in the Site Details context strip
  output$ctx_map_site <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE,
                                     dragging = FALSE, scrollWheelZoom = FALSE)) %>%
      addTiles() %>%
      setView(lng = -1.16, lat = 53.75, zoom = 7)
  })

  observeEvent(list(input$site_entity_type, rv_measured_site(), rv_waterbody()), {
    if (identical(input$site_entity_type, "modelled")) {
      req(rv_waterbody())
      shp <- waterbody_opcat_shp %>% filter(water_body == rv_waterbody())
      req(nrow(shp) == 1)
      bb <- as.numeric(sf::st_bbox(shp))
      leafletProxy("ctx_map_site") %>%
        clearMarkers() %>% clearShapes() %>%
        addPolygons(data = shp, fillColor = ec$accent, fillOpacity = 0.5,
                    color = ec$accent, weight = 2) %>%
        fitBounds(bb[1], bb[2], bb[3], bb[4])
    } else {
      req(rv_measured_site())
      row <- df_measured_sites %>% dplyr::filter(Site_id == rv_measured_site())
      req(nrow(row) == 1)
      leafletProxy("ctx_map_site") %>%
        clearMarkers() %>% clearShapes() %>%
        setView(lng = row$Longitude, lat = row$Latitude, zoom = 10) %>%
        addCircleMarkers(lng = row$Longitude, lat = row$Latitude,
                         radius = 6, color = ec$accent, weight = 2,
                         fillColor = ec$accent, fillOpacity = 1)
    }
  })

  # Measured sites rail (replaces the full-width selectInput)
  output$measured_rail <- renderUI({
    active <- rv_measured_site()
    sites <- df_measured_sites %>% arrange(Site_full_name)
    tagList(lapply(seq_len(nrow(sites)), function(i) {
      id <- sites$Site_id[i]
      div(
        class = paste("rail-row", if (!is.null(active) && identical(active, id)) "is-active" else ""),
        onclick = paste0("Shiny.setInputValue('site_pick', ", jsonlite::toJSON(id, auto_unbox = TRUE),
                         ", {priority:'event'})"),
        span(sites$Site_full_name[i])
      )
    }))
  })

  observeEvent(input$site_pick, { rv_measured_site(input$site_pick) })

  # Modelled water bodies rail - only the 336 with modelled data are listed
  # (the other 34 geojson polygons are shown greyed-out on the Map but are
  # never selectable here or anywhere else in the app).
  output$modelled_rail <- renderUI({
    active <- rv_waterbody()
    wbs <- waterbody_opcat_shp %>% sf::st_drop_geometry() %>%
      dplyr::filter(has_model_data) %>% dplyr::arrange(water_bod0)
    tagList(lapply(seq_len(nrow(wbs)), function(i) {
      id <- wbs$water_body[i]
      div(
        class = paste("rail-row", if (!is.null(active) && identical(active, id)) "is-active" else ""),
        onclick = paste0("Shiny.setInputValue('waterbody_pick', ", jsonlite::toJSON(id, auto_unbox = TRUE),
                         ", {priority:'event'})"),
        span(wbs$water_bod0[i])
      )
    }))
  })

  observeEvent(input$waterbody_pick, { rv_waterbody(input$waterbody_pick) })

  # The four organic micropollutants with the greatest concentration range
  # at the selected measured site, used to auto-pick the time series panels.
  selected_measured_site_top_chemicals <- reactive({
    selected_measured_site_long() %>%
      dplyr::filter(parameter_group == "Organic micropollutant") %>%
      dplyr::group_by(parameter, parameter_label) %>%
      dplyr::summarise(
        range_val = {
          rng <- suppressWarnings(range(value_num, na.rm = TRUE))
          if (all(is.finite(rng))) diff(rng) else NA_real_
        },
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(range_val)) %>%
      dplyr::arrange(dplyr::desc(range_val)) %>%
      dplyr::slice_head(n = 4)
  })

  # The four modelled chemicals with the greatest concentration range at the
  # selected water body (at the median statistic), mirroring the measured
  # equivalent above.
  selected_waterbody_top_chemicals <- reactive({
    req(rv_waterbody())
    get_wb_chem_grid(rv_waterbody(), "median") %>%
      dplyr::group_by(chem_key) %>%
      dplyr::summarise(
        range_val = {
          rng <- suppressWarnings(range(value, na.rm = TRUE))
          if (all(is.finite(rng))) diff(rng) else NA_real_
        },
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(range_val)) %>%
      dplyr::arrange(dplyr::desc(range_val)) %>%
      dplyr::slice_head(n = 4)
  })

  # Chemical x week occurrence grid data for the selected measured site.
  # Colour is a log-scaled concentration relative to that parameter's own
  # maximum at this site (0-1). Non-detects sit at the bottom of the scale;
  # missing samples are a distinct flat grey via NA.
  measured_chem_grid_data <- reactive({
    df_grid <- selected_measured_site_long() %>%
      dplyr::group_by(parameter) %>%
      dplyr::mutate(
        max_detected = suppressWarnings(max(value_num[status == "Detected"], na.rm = TRUE)),
        max_detected = ifelse(is.finite(max_detected) & max_detected > 0, max_detected, 1),
        color_value = dplyr::if_else(
          status == "No sample",
          NA_real_,
          log1p(pmax(value_num, 0)) / log1p(max_detected)
        )
      ) %>%
      dplyr::ungroup()

    parameter_order <- df_grid %>%
      dplyr::distinct(parameter_label, parameter_group) %>%
      dplyr::arrange(parameter_group, parameter_label) %>%
      dplyr::pull(parameter_label)
    df_grid$parameter_label <- factor(df_grid$parameter_label, levels = rev(parameter_order))
    df_grid
  })

  # Chemical x month concentration grid data for the selected modelled water
  # body, at the chosen statistic. Colour is a log-scaled concentration
  # relative to that chemical's own maximum at this water body (0-1) - there
  # is no detect/non-detect concept here, every cell has a modelled value.
  wb_chem_grid_data <- reactive({
    req(rv_waterbody())
    stat_val <- input$site_grid_wb_stat %||% "median"
    df_grid <- get_wb_chem_grid(rv_waterbody(), stat_val) %>%
      dplyr::group_by(chem_key) %>%
      dplyr::mutate(
        max_val = suppressWarnings(max(value, na.rm = TRUE)),
        max_val = ifelse(is.finite(max_val) & max_val > 0, max_val, 1),
        color_value = log1p(pmax(value, 0)) / log1p(max_val)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(chem_label = unname(chem_display_lookup[chem_key]))

    chem_order <- df_grid %>% dplyr::distinct(chem_label) %>% dplyr::arrange(chem_label) %>% dplyr::pull(chem_label)
    df_grid$chem_label <- factor(df_grid$chem_label, levels = rev(chem_order))
    df_grid
  })

  output$site_chem_grid_note <- renderText({
    if (identical(input$site_entity_type, "modelled")) {
      "colour = concentration relative to this water body's own maximum"
    } else {
      "colour = concentration relative to this site's own maximum; grey = no sample"
    }
  })

  output$site_chem_grid <- renderPlot({
    if (identical(input$site_entity_type, "modelled")) {
      req(rv_waterbody())
      df_grid <- wb_chem_grid_data()

      ggplot(df_grid, aes(x = date, y = chem_label, fill = color_value)) +
        geom_tile(colour = ec$ground, linewidth = 0.4) +
        scale_fill_gradientn(
          colours = c(ec$rule_light, "#ffc4b8", "#ff563c", ec$accent_text),
          na.value = ec$surface,
          limits = c(0, 1),
          guide = guide_colourbar(barheight = grid::unit(8, "pt"), barwidth = grid::unit(90, "pt"))
        ) +
        scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_ecomix(11) +
        theme(
          panel.grid = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_text(size = 9, colour = ec$ink),
          axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
        )
    } else {
      df_grid <- measured_chem_grid_data()

      ggplot(df_grid, aes(x = Date, y = parameter_label, fill = color_value)) +
        geom_tile(colour = ec$ground, linewidth = 0.4) +
        scale_fill_gradientn(
          colours = c(ec$rule_light, "#ffc4b8", "#ff563c", ec$accent_text),
          na.value = ec$surface,
          limits = c(0, 1),
          guide = guide_colourbar(barheight = grid::unit(8, "pt"), barwidth = grid::unit(90, "pt"))
        ) +
        scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_ecomix(11) +
        theme(
          panel.grid = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_text(size = 9, colour = ec$ink),
          axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
        )
    }
  })

  # Hover tooltip for the occurrence/concentration grid.
  output$site_chem_grid_tooltip <- renderUI({
    hover <- input$site_chem_grid_hover
    req(hover)

    if (identical(input$site_entity_type, "modelled")) {
      point <- nearPoints(wb_chem_grid_data(), hover,
                           xvar = "date", yvar = "chem_label",
                           threshold = 15, maxpoints = 1)
      if (nrow(point) == 0) return(NULL)
      detail_text <- paste0("Value: ", round(point$value[1], 3), " ng/L")
      label_text <- as.character(point$chem_label[1])
      date_text <- format(point$date[1], "%b %Y")
    } else {
      point <- nearPoints(measured_chem_grid_data(), hover,
                           xvar = "Date", yvar = "parameter_label",
                           threshold = 15, maxpoints = 1)
      if (nrow(point) == 0) return(NULL)
      detail_text <- switch(point$status[1],
        "Detected" = paste0("Value: ", round(point$value_num[1], 3)),
        "Non-detect" = "Not detected (below LOD)",
        "No sample" = "No sample taken"
      )
      label_text <- as.character(point$parameter_label[1])
      date_text <- format(point$Date[1], "%d %b %Y")
    }

    style <- paste0(
      "position:absolute; z-index:1000; pointer-events:none; ",
      "left:", hover$coords_css$x + 12, "px; top:", hover$coords_css$y + 12, "px; ",
      "background:", ec$ground, "; border:2px solid ", ec$ink, "; ",
      "padding:6px 10px; font-size:12px; white-space:nowrap;"
    )

    div(
      style = style,
      tags$strong(label_text), tags$br(),
      date_text, tags$br(),
      detail_text
    )
  })

  # Stacked time series for the four chemicals chosen in the popover.
  output$site_time_series <- renderPlot({
    if (identical(input$site_entity_type, "modelled")) {
      req(rv_waterbody())
      chem_key_vals <- vapply(seq_len(4), function(i) input[[paste0("site_ts_chemical_", i)]] %||% "", character(1))
      req(all(nzchar(chem_key_vals)))
      stat_val <- input$site_grid_wb_stat %||% "median"

      df_ts <- dplyr::bind_rows(lapply(chem_key_vals, function(k) {
        get_wb_chem_series(k, stat_val, rv_waterbody()) %>%
          dplyr::mutate(panel_label = unname(chem_display_lookup[k]))
      }))
      panel_levels <- unname(chem_display_lookup[chem_key_vals])
      df_ts$panel_label <- factor(df_ts$panel_label, levels = panel_levels)

      ggplot(df_ts, aes(x = date, y = value)) +
        geom_line(colour = ec$accent, na.rm = TRUE) +
        geom_point(colour = ec$accent, size = 1.6, na.rm = TRUE) +
        scale_x_date(expand = expansion(mult = c(0.02, 0.06))) +
        facet_wrap(vars(panel_label), ncol = 1, scales = "free_y") +
        labs(x = NULL, y = NULL, caption = "Monthly modelled concentration, 2018-2022.") +
        theme_ecomix(11)
    } else {
      params <- vapply(seq_len(4), function(i) input[[paste0("site_ts_chemical_", i)]] %||% "", character(1))
      req(all(nzchar(params)))

      df_ts <- dplyr::bind_rows(lapply(seq_along(params), function(i) {
        selected_measured_site_long() %>%
          dplyr::filter(parameter == params[i]) %>%
          dplyr::mutate(panel_label = measured_parameter_label(params[i]))
      }))
      panel_levels <- unique(measured_parameter_label(params))
      df_ts$panel_label <- factor(df_ts$panel_label, levels = panel_levels)

      ggplot(df_ts, aes(x = Date, y = value_num)) +
        geom_line(colour = ec$accent, na.rm = TRUE) +
        geom_point(aes(colour = status), size = 1.6, na.rm = TRUE) +
        scale_colour_manual(
          breaks = c("Detected", "Non-detect"),
          values = c("Detected" = ec$accent, "Non-detect" = ec$ink_dim)
        ) +
        scale_x_date(expand = expansion(mult = c(0.02, 0.06))) +
        facet_wrap(vars(panel_label), ncol = 1, scales = "free_y") +
        labs(x = NULL, y = NULL, caption = "Gaps indicate no sample taken.") +
        theme_ecomix(11)
    }
  })


  ### NAVBAR 2 - DATA EXPLORER ###

  # Selected water body for Data Explorer's "Water body" mode - section-local
  # (unlike rv_waterbody on Site Details) since nothing outside this section
  # needs it.
  rv_de_waterbody <- reactiveVal()

  updateSelectizeInput(session, "de_waterbody_search",
                       choices = waterbody_opcat_shp %>% sf::st_drop_geometry() %>%
                         dplyr::filter(has_model_data) %>% dplyr::arrange(water_bod0) %>%
                         { setNames(.$water_body, .$water_bod0) },
                       selected = character(0), server = TRUE)

  observeEvent(input$de_waterbody_search, {
    req(nzchar(input$de_waterbody_search))
    rv_de_waterbody(input$de_waterbody_search)
  }, ignoreInit = TRUE)

  # Model variable choices are HYPE + subbasin chemicals in Subbasin mode,
  # chemicals only in Water body mode (there's no per-water-body hydrology).
  observeEvent(input$de_entity_type, {
    if (identical(input$de_entity_type, "waterbody")) {
      updateSelectInput(session, "prediction_variable", choices = chem_variable_choices)
    } else {
      updateSelectInput(session, "prediction_variable", choices = prediction_variable_choices)
    }
  }, ignoreInit = TRUE)

  # Card-header pill labels, so the header states the current choice rather
  # than a sidebar restating it.
  output$climate_pill <- renderText({
    res <- if (identical(input$climate_resolution, "annual")) "Yearly" else "Monthly"
    paste0(input$climate_variable, " \u00b7 ", res, " \u25be")
  })

  output$observation_pill <- renderText({
    v <- input$observation_variable
    if (is.null(v) || !nzchar(v)) "Choose a variable \u25be" else paste0(v, " \u25be")
  })

  output$projection_title <- renderText({
    var <- input$prediction_variable
    lbl <- prediction_variable_labels[[var]] %||% "output"
    if (is_chem_variable(var)) paste0(lbl, " concentration") else paste0("Projected ", tolower(lbl))
  })

  # "Daily" is only meaningful for chemicals - the HYPE datasets in this app
  # are monthly/yearly/percentile only, with no daily series behind them.
  # Default Model variable is a HYPE one, so start with the tab hidden.
  bslib::nav_hide("projection_tabs", target = "Daily", session = session)

  # The "Conditions" selector doubles up as the percentile picker for HYPE
  # variables (p10/p50/p90) and the statistic picker for chemicals (mean/
  # median/90th, matching the data-chem file suffixes) - swap its choices
  # when the Model variable crosses that boundary.
  observeEvent(input$prediction_variable, {
    if (is_chem_variable(input$prediction_variable)) {
      updateSelectInput(session, "prediction_percentile",
                        label = "Statistic",
                        choices = chem_stat_choices,
                        selected = "median")
      bslib::nav_show("projection_tabs", target = "Daily", session = session)
    } else {
      updateSelectInput(session, "prediction_percentile",
                        label = "Conditions",
                        choices = c("Low (10th percentile)" = "p10",
                                    "Average (50th percentile)" = "p50",
                                    "High (90th percentile)" = "p90"),
                        selected = "p50")
      bslib::nav_hide("projection_tabs", target = "Daily", session = session)
    }
  }, ignoreInit = TRUE)

  ## Plot 1: Climate
  output$climate_plot <- renderPlot({
    if (identical(input$de_entity_type, "waterbody")) {
      return(no_data_plot("Not available in Water body mode"))
    }
    if (is.null(rv())) return(NULL)

    projection_data <- projection_sources()

    sub_subbasin <- rv()
    sub_climate_variable <- if (is.null(input$climate_variable) || !nzchar(input$climate_variable)) "Precipitation" else input$climate_variable
    sub_climate_resolution <- if (is.null(input$climate_resolution) || !nzchar(input$climate_resolution)) "monthly" else input$climate_resolution
    sub_scenarios <- unique(input$scenario)
    sub_periods <- unique(input$prediction_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c("2000-2022", sub_periods)
    }

    df_plot <- projection_data$forcing %>%
      filter(
        variable == sub_climate_variable,
        subbasin %in% sub_subbasin,
        ssp %in% sub_scenarios,
        time_aggregation == sub_climate_resolution) %>%
      collect_quiet()

    # Climate/forcing data only covers the subset of subbasins the HYPE
    # model was run for (~1446 of the ~16,500 subbasins on the map) - most
    # others, including nearly all of the chemical-prediction subbasins,
    # have none. Say so explicitly rather than leaving the panel blank.
    if (nrow(df_plot) == 0) {
      return(no_data_plot("No climate model data for this subbasin"))
    }

    if (sub_climate_resolution == "monthly") {
      df_plot <- df_plot %>% filter(period %in% sub_periods) %>%
        mutate("scenario" = paste0(ssp, " (", period, ")"))
      df_plot$xaxis <- df_plot$month
      xlab <- "Month"
    }
    if (sub_climate_resolution == "annual") {
      df_plot$scenario <- df_plot$ssp
      df_plot$xaxis <- df_plot$year
      xlab <- "Year"
    }
    if (nrow(df_plot) == 0) {
      return(no_data_plot("No climate model data for the selected scenario/period"))
    }
    ylab <- paste0(sub_climate_variable, " [", unique(df_plot$unit), "]")

    ggplot(df_plot, aes(x = xaxis, y = p50, color = scenario, fill = scenario)) +
      geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.18, colour = NA) +
      geom_line(linewidth = 0.8) +
      scale_color_ecomix() +
      scale_fill_ecomix() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = xlab, y = ylab) +
      theme_ecomix()
  })

  ## Plot 2 - Observations
  output$observation_plot <- renderPlot({
    if (identical(input$de_entity_type, "waterbody")) {
      return(no_data_plot("Not available in Water body mode"))
    }
    if (is.null(rv())) return(NULL)

    sub_observation_variable <- input$observation_variable
    if (is.null(sub_observation_variable) || !nzchar(sub_observation_variable)) {
      sub_observation_variable <- observation_choices()[1]
    }

    df_data <- selected_historical()
    df_plot <- df_data[df_data$variable == sub_observation_variable, ]

    if (identical(sub_observation_variable, "discharge")) {

      df_tmp <- df_plot %>% dplyr::select(-prediction_percentile, -variable, -sim_P10, -sim_P50, -sim_P90) %>%
        rename("low" = obs_min, "med" = obs, "high" = obs_max) %>% mutate("type" = "Observation")
      df_plot <- df_plot %>% dplyr::select(-prediction_percentile, -variable, -obs_min, -obs_max, -obs) %>%
        rename("low" = sim_P10, "med" = sim_P50, "high" = sim_P90) %>% mutate("type" = "Simulation")
      df_plot <- rbind(df_plot, df_tmp)

      ggplot(df_plot, aes(x = date, y = med, color = type, fill = type)) +
        geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.25, colour = NA) +
        geom_line(linewidth = 0.7) +
        scale_color_manual(values = c("Simulation" = ec$accent, "Observation" = ec$ink)) +
        scale_fill_manual(values = c("Simulation" = ec$accent, "Observation" = ec$ink)) +
        scale_x_date(expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, max(df_plot$high, na.rm = TRUE) * 1.1), expand = c(0, 0)) +
        labs(x = NULL, y = "Discharge [m\u00b3/s]",
             caption = paste0(unique(df_plot$station_label), " (Station ", unique(df_plot$id_station), ")")) +
        theme_ecomix()

    } else {

      if (nrow(df_plot) == 0 || all(is.na(c(df_plot$sim_P90, df_plot$obs)))) {
        return(NULL)
      }
      upper <- max(c(df_plot$sim_P90, df_plot$obs), na.rm = TRUE)
      ylab <- paste0(unique(df_plot$variable), " [", unique(df_plot$unit), "]")

      ggplot(df_plot, aes(x = date, y = sim_P50)) +
        geom_ribbon(aes(ymin = sim_P10, ymax = sim_P90), fill = ec$accent, alpha = 0.2) +
        geom_line(colour = ec$accent, linewidth = 0.7) +
        geom_point(aes(x = date, y = obs), shape = 15, colour = ec$ink, size = 1.6) +
        scale_x_date(expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, upper * 1.1), expand = c(0, 0)) +
        labs(x = NULL, y = ylab,
             caption = paste0(unique(df_plot$station_label), " (Station ", unique(df_plot$id_station), ")")) +
        theme_ecomix()
    }
  })

  ## Plot 3: Yearly Projections
  output$projections_yearly_plot <- renderPlot({
    if (is.null(rv())) return(NULL)

    projection_data <- projection_sources()

    sub_subbasin <- rv()
    sub_variable <- input$prediction_variable[1]

    # Chemicals: a single 2018-2022 daily record with no scenario/period, so
    # "Yearly" here is the annual mean of the daily values per statistic
    # (Mean/Median/90th) rather than an SSP projection.
    if (is_chem_variable(sub_variable)) {
      df_chem <- chem_selected_series()
      if (nrow(df_chem) == 0) return(no_data_plot("No chemical data for this subbasin"))

      df_yearly <- df_chem %>%
        dplyr::group_by(year, stat) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(stat_label = factor(chem_stat_labels[stat], levels = unname(chem_stat_labels)))

      lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable
      return(
        ggplot(df_yearly, aes(x = year, y = value, color = stat_label)) +
          geom_line(linewidth = 0.8) +
          geom_point(size = 1.6) +
          scale_x_continuous(expand = c(0, 0), breaks = unique(df_yearly$year)) +
          scale_color_ecomix() +
          labs(x = "Year", y = paste0(lbl, " [ng/L]"),
               caption = "Annual mean of the daily 2018-2022 record.") +
          theme_ecomix()
      )
    }

    sub_scenarios <- unique(input$scenario)
    sub_percentiles <- unique(input$prediction_percentile)
    sub_plot_type <- unique(input$plot_type)

    df_projections_year <- projection_data$year %>%
      filter(subbasin %in% sub_subbasin,
             hype_variable %in% sub_variable,
             ssp %in% c("Baseline", sub_scenarios),
             prediction_percentile %in% sub_percentiles) %>%
      collect()

    # This subbasin may simply be outside the ~1446 subbasins the HYPE model
    # covers (most subbasins on the map, and nearly all of the chemical
    # ones, are) - show that plainly instead of a blank/vanishing chart.
    if (nrow(df_projections_year) == 0) {
      return(no_data_plot("No hydrology model data for this subbasin"))
    }

    if (sub_plot_type == "Absolute") {

      df_plot <- df_projections_year %>% filter(ssp %in% sub_scenarios)
      df_plot <- df_plot[!(df_plot$ssp == "Baseline" & df_plot$year > 2020), ]
      df_plot$percentile_label <- factor(df_plot$prediction_percentile, levels = c("p10", "p50", "p90"),
                                         labels = c("Low (10th percentile)", "Average (50th percentile)", "High (90th percentile)"))

      ylab <- paste0(sub_variable, " [", unique(df_plot$unit), "]")
      ggplot(df_plot, aes(x = year, y = p50_ensemble, color = ssp, fill = ssp, linetype = percentile_label)) +
        geom_ribbon(aes(ymin = p10_ensemble, ymax = p90_ensemble), alpha = 0.18, colour = NA) +
        geom_line(linewidth = 0.8) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_color_ecomix() +
        scale_fill_ecomix() +
        labs(x = "Year", y = ylab) +
        theme_ecomix()
    } else {

      df_proj <- df_projections_year %>% filter(ssp != "Baseline")
      df_base <- df_projections_year %>% filter(ssp == "Baseline") %>%
        group_by(subbasin, prediction_percentile) %>%
        summarise("p10_base" = mean(p10_ensemble), "p50_base" = mean(p50_ensemble), "p90_base" = mean(p90_ensemble))

      df_proj <- left_join(df_proj, df_base, by = c("subbasin", "prediction_percentile"))
      df_proj$p10_anomaly <- df_proj$p10_ensemble - df_proj$p10_base
      df_proj$p50_anomaly <- df_proj$p50_ensemble - df_proj$p50_base
      df_proj$p90_anomaly <- df_proj$p90_ensemble - df_proj$p90_base

      df_proj$low_uci <- apply(df_proj[, c("p10_anomaly", "p50_anomaly", "p90_anomaly")], 1, min, na.rm = TRUE)
      df_proj$high_uci <- apply(df_proj[, c("p10_anomaly", "p50_anomaly", "p90_anomaly")], 1, max, na.rm = TRUE)

      df_plot <- df_proj
      ylab <- paste0("Change to baseline: ", sub_variable, " [", unique(df_plot$unit), "]")

      ggplot(df_plot, aes(x = year, color = ssp, fill = ssp, linetype = prediction_percentile)) +
        geom_ribbon(aes(ymin = low_uci, ymax = high_uci), alpha = 0.18, colour = NA) +
        geom_line(aes(y = low_uci)) +
        geom_line(aes(y = high_uci)) +
        geom_hline(yintercept = 0, colour = ec$ink, linewidth = 0.5) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_color_ecomix() +
        scale_fill_ecomix() +
        labs(x = "Year", y = ylab, caption = "Change against the 2000-2020 baseline.") +
        theme_ecomix()
    }
  })

  ## Plot 4: Monthly Projections
  output$projections_monthly_plot <- renderPlot({
    if (is.null(rv())) return(NULL)

    projection_data <- projection_sources()

    sub_subbasin <- rv()
    sub_variable <- input$prediction_variable[1]

    # Chemicals: monthly climatology across the whole 2018-2022 record
    # (mean per calendar month, ribbon = range across the covered years) per
    # statistic, in place of an SSP/period monthly projection.
    if (is_chem_variable(sub_variable)) {
      df_chem <- chem_selected_series()
      if (nrow(df_chem) == 0) return(no_data_plot("No chemical data for this subbasin"))

      df_monthly <- df_chem %>%
        dplyr::group_by(month, stat) %>%
        dplyr::summarise(value_mean = mean(value, na.rm = TRUE),
                         value_min = min(value, na.rm = TRUE),
                         value_max = max(value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(stat_label = factor(chem_stat_labels[stat], levels = unname(chem_stat_labels)))

      lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable
      return(
        ggplot(df_monthly, aes(x = month, y = value_mean, color = stat_label, fill = stat_label)) +
          geom_ribbon(aes(ymin = value_min, ymax = value_max), alpha = 0.18, colour = NA) +
          geom_line(linewidth = 0.8) +
          scale_x_continuous(expand = c(0, 0), breaks = 1:12) +
          scale_color_ecomix() +
          scale_fill_ecomix() +
          labs(x = "Month", y = paste0(lbl, " [ng/L]"),
               caption = "Monthly climatology across 2018-2022; ribbon = range across years.") +
          theme_ecomix()
      )
    }

    sub_scenarios <- unique(input$scenario)
    sub_percentiles <- unique(input$prediction_percentile)
    sub_periods <- unique(input$prediction_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c("2000-2022", sub_periods)
    }
    sub_plot_type <- unique(input$plot_type)

    df_projections_month <- projection_data$month %>%
      filter(subbasin %in% sub_subbasin,
             hype_variable %in% sub_variable,
             ssp %in% c("Baseline", sub_scenarios),
             prediction_percentile %in% sub_percentiles,
             period %in% sub_periods) %>%
      collect()

    if (nrow(df_projections_month) == 0) {
      return(no_data_plot("No hydrology model data for this subbasin"))
    }

    if (sub_plot_type == "Absolute") {

      df_plot <- df_projections_month %>% filter(ssp %in% sub_scenarios)
      df_plot <- df_plot %>% mutate("scenario" = paste0(ssp, " (", period, ")"))
      df_plot$percentile_label <- factor(df_plot$prediction_percentile, levels = c("p10", "p50", "p90"),
                                         labels = c("Low (10th percentile)", "Average (50th percentile)", "High (90th percentile)"))

      ylab <- paste0(sub_variable, " [", unique(df_plot$unit), "]")
      ggplot(df_plot, aes(x = month, y = p50_ensemble, color = scenario, fill = scenario, linetype = percentile_label)) +
        geom_ribbon(aes(ymin = p10_ensemble, ymax = p90_ensemble), alpha = 0.18, colour = NA) +
        geom_line(linewidth = 0.8) +
        scale_x_continuous(expand = c(0, 0), breaks = 1:12) +
        scale_color_ecomix() +
        scale_fill_ecomix() +
        labs(x = "Month", y = ylab) +
        theme_ecomix()

    } else {

      df_proj <- df_projections_month %>% filter(ssp != "Baseline")
      df_base <- df_projections_month %>% filter(ssp == "Baseline") %>%
        group_by(subbasin, month, prediction_percentile) %>%
        summarise("p10_base" = mean(p10_ensemble), "p50_base" = mean(p50_ensemble), "p90_base" = mean(p90_ensemble))

      df_proj <- left_join(df_proj, df_base, by = c("subbasin", "month", "prediction_percentile"))
      df_proj$p10_anomaly <- df_proj$p10_ensemble - df_proj$p10_base
      df_proj$p50_anomaly <- df_proj$p50_ensemble - df_proj$p50_base
      df_proj$p90_anomaly <- df_proj$p90_ensemble - df_proj$p90_base

      df_proj$low_uci <- apply(df_proj[, c("p10_anomaly", "p50_anomaly", "p90_anomaly")], 1, min, na.rm = TRUE)
      df_proj$high_uci <- apply(df_proj[, c("p10_anomaly", "p50_anomaly", "p90_anomaly")], 1, max, na.rm = TRUE)

      df_plot <- df_proj
      ylab <- paste0("Change to baseline: ", sub_variable, " [", unique(df_plot$unit), "]")

      ggplot(df_plot, aes(x = month, color = ssp, fill = ssp, linetype = prediction_percentile)) +
        geom_ribbon(aes(ymin = low_uci, ymax = high_uci), alpha = 0.18, colour = NA) +
        geom_line(aes(y = low_uci)) +
        geom_line(aes(y = high_uci)) +
        geom_hline(yintercept = 0, colour = ec$ink, linewidth = 0.5) +
        scale_x_continuous(expand = c(0, 0), breaks = 1:12) +
        scale_color_ecomix() +
        scale_fill_ecomix() +
        labs(x = "Month", y = ylab, caption = "Change against the 2000-2020 baseline.") +
        theme_ecomix()
    }
  })

  ## Plot 5: Cumulative Frequency Curves
  output$projections_cfc_plot <- renderPlot({
    if (is.null(rv())) return(NULL)

    projection_data <- projection_sources()

    sub_subbasin <- rv()
    sub_variable <- input$prediction_variable[1]

    # Chemicals: empirical cumulative frequency curve of the daily
    # concentration values themselves (per statistic), rather than the
    # ensemble percentile spread the HYPE variables use.
    if (is_chem_variable(sub_variable)) {
      df_chem <- chem_selected_series()
      if (nrow(df_chem) == 0) return(no_data_plot("No chemical data for this subbasin"))

      probs <- seq(1, 99, by = 1)
      df_cfc <- df_chem %>%
        dplyr::group_by(stat) %>%
        dplyr::reframe(p = probs, value = as.numeric(stats::quantile(value, probs = probs / 100, na.rm = TRUE))) %>%
        dplyr::mutate(stat_label = factor(chem_stat_labels[stat], levels = unname(chem_stat_labels)))

      lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable
      return(
        ggplot(df_cfc, aes(x = p, y = value, color = stat_label)) +
          geom_line(linewidth = 0.8) +
          scale_x_continuous(expand = c(0, 0), breaks = c(0, 25, 50, 75, 100),
                             labels = c("0", "25", "50 (Median)", "75", "100")) +
          scale_y_log10() +
          scale_color_ecomix() +
          labs(x = "Cumulative frequency [%]", y = paste0(lbl, " [ng/L]")) +
          theme_ecomix()
      )
    }

    sub_scenarios <- unique(input$scenario)
    sub_periods <- unique(input$prediction_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c("2000-2022", sub_periods)
    }

    df_plot <- projection_data$percentiles %>%
      filter(subbasin %in% sub_subbasin,
             hype_variable %in% sub_variable,
             ssp %in% sub_scenarios,
             period %in% sub_periods) %>%
      collect()

    if (nrow(df_plot) == 0) {
      return(no_data_plot("No hydrology model data for this subbasin"))
    }

    df_plot <- df_plot %>% mutate("scenario" = paste0(ssp, " (", period, ")"))
    ylab <- paste0(sub_variable, " [", unique(df_plot$unit), "]")

    ggplot(df_plot, aes(x = prediction_percentile, y = p50_ensemble, color = scenario, fill = scenario)) +
      geom_ribbon(aes(ymin = p10_ensemble, ymax = p90_ensemble), alpha = 0.18, colour = NA) +
      geom_line(linewidth = 0.8) +
      scale_x_continuous(expand = c(0, 0), breaks = c(0, 25, 50, 75, 100),
                         labels = c("0", "25", "50 (Median)", "75", "100")) +
      scale_y_log10() +
      scale_color_ecomix() +
      scale_fill_ecomix() +
      labs(x = "Cumulative frequency [%]", y = ylab) +
      theme_ecomix()
  })

  ## Plot 6: Daily chemical concentration series (chemicals only - the HYPE
  ## datasets in this app have no daily granularity, only monthly/yearly/
  ## percentile summaries, so this tab is hidden via nav_hide/nav_show
  ## whenever a HYPE variable is selected).
  output$projections_daily_plot <- renderPlot({
    if (is.null(rv())) return(NULL)

    sub_variable <- input$prediction_variable[1]
    if (!is_chem_variable(sub_variable)) {
      return(no_data_plot("Daily data is only available for chemical concentration variables"))
    }

    df_chem <- chem_selected_series()
    if (nrow(df_chem) == 0) return(no_data_plot("No chemical data for this subbasin"))

    df_chem$stat_label <- factor(chem_stat_labels[df_chem$stat], levels = unname(chem_stat_labels))
    lbl <- prediction_variable_labels[[sub_variable]] %||% sub_variable

    ggplot(df_chem, aes(x = date, y = value, color = stat_label)) +
      geom_line(linewidth = 0.5) +
      scale_x_date(expand = c(0, 0)) +
      scale_color_ecomix() +
      labs(x = NULL, y = paste0(lbl, " [ng/L]"),
           caption = "Daily reconstructed concentration, 2018-2022.") +
      theme_ecomix()
  })

  # "Download this selection" in the Data Explorer rail - the current
  # subbasin/variable extract, same shape as the Data Downloader output.
  output$download_selection <- downloadHandler(
    filename = function() {
      var <- input$prediction_variable
      label <- if (is_chem_variable(var)) chem_key_from_variable(var) else var
      paste0("ecomix_subbasin_", rv(), "_", label, ".csv")
    },
    content = function(file) {
      req(rv())
      var <- input$prediction_variable
      if (is_chem_variable(var)) {
        df <- chem_selected_series() %>%
          dplyr::mutate(chemical = prediction_variable_labels[[var]] %||% chem_key_from_variable(var),
                        subbasin = rv(), unit = "ng/L") %>%
          dplyr::select(subbasin, chemical, statistic = stat, date, value, unit)
        write.csv(df, file, row.names = FALSE)
      } else {
        hype_to_dl <- c("discharge" = "runoff", "Soil moisture" = "soil_moisture",
                        "water temperature" = "water_temperature",
                        "Susp. Sediments" = "susp_sediments",
                        "Inorganic Nitrogen" = "inorganic_nitrogen")
        df <- get_tabular_download(unname(hype_to_dl[var])) %>%
          filter(subbasin == rv())
        write.csv(df, file, row.names = FALSE)
      }
    }
  )


  ### NAVBAR 3 - SPATIAL DATASETS ###

  spatial_layer_data <- reactive({
    sub_variable <- input$prediction_variable_spatial[1]

    # Chemicals have no scenario/period - the map shows one time-averaged
    # (whole 2018-2022 record) value per subbasin (or, for modelled water
    # body chemicals, per water body) for the chosen statistic. The result is
    # shaped to match df_map_input's columns (hype_variable, subbasin,
    # p50_ensemble, unit, prediction_percentile) so the rendering and
    # context-strip code below needs no branching of its own. The water body
    # branch has no "subbasin" concept, so it carries a placeholder NA
    # subbasin column purely so the context-strip's existing
    # filter(subbasin == rv()) calls don't error - they simply find no match
    # and read "-", since this page has no water-body-selection context strip.
    if (is_wbchem_variable(sub_variable)) {
      chem_key <- wbchem_key_from_variable(sub_variable)
      stat <- input$prediction_stat_spatial_chem %||% "median"
      get_wb_chem_spatial_summary(chem_key, stat) %>%
        dplyr::mutate(hype_variable = sub_variable, p50_ensemble = value,
                      unit = "ng/L", prediction_percentile = stat, subbasin = NA_integer_)
    } else if (is_chem_variable(sub_variable)) {
      chem_key <- chem_key_from_variable(sub_variable)
      stat <- input$prediction_stat_spatial_chem %||% "median"
      get_chem_spatial_summary(chem_key, stat) %>%
        dplyr::mutate(hype_variable = sub_variable, p50_ensemble = value,
                      unit = "ng/L", prediction_percentile = stat)
    } else {
      sub_period <- strsplit(input$prediction_period_spatial %||% "SSP585|2070-2080", "\\|")[[1]]
      sub_percentile <- as.numeric(input$prediction_percentile_spatial %||% "99.9")

      df_map_input %>% filter(
        hype_variable == sub_variable,
        ssp == sub_period[1],
        period == sub_period[2],
        prediction_percentile == sub_percentile
      )
    }
  })

  output$prediction_map <- renderLeaflet({
    sub_variable <- input$prediction_variable_spatial[1]
    df_sub <- spatial_layer_data()
    shp_map <- if (is_wbchem_variable(sub_variable)) {
      left_join(waterbody_opcat_shp, df_sub, by = c("water_body" = "waterbody_id")) %>% filter(!is.na(hype_variable))
    } else {
      left_join(subbasin_shp, df_sub, by = c("Id" = "subbasin")) %>% filter(!is.na(hype_variable))
    }

    pal <- colorNumeric(palette = "viridis", domain = shp_map$p50_ensemble)
    legend_title <- if (nrow(df_sub) == 0) {
      "No data"
    } else if (is_chem_variable(sub_variable) || is_wbchem_variable(sub_variable)) {
      paste0(chem_stat_labels[df_sub$prediction_percentile[1]], " conc. (ng/L)")
    } else {
      paste0("P", df_sub$prediction_percentile[1])
    }

    leaflet() %>%
      add_basemap_tiles("osm") %>%
      setView(lng = -1.16, lat = 53.75, zoom = 8.5) %>%
      addPolygons(
        data = shp_map,
        fillColor = ~pal(p50_ensemble),
        fillOpacity = 0.75,
        color = ec$ink,
        weight = 0.6
      ) %>%
      addLegend("bottomleft", pal = pal, values = shp_map$p50_ensemble,
                title = legend_title, opacity = 1)
  })

  observeEvent(input$sp_basemap, {
    leafletProxy("prediction_map") %>%
      clearGroup("basetile") %>%
      add_basemap_tiles(input$sp_basemap)
  }, ignoreInit = TRUE)

  output$ctx_value_sp <- renderText({
    if (is.null(rv())) return("\u2014")
    df <- spatial_layer_data() %>% filter(subbasin == rv())
    if (nrow(df) == 0) return("no value")
    paste0(round(df$p50_ensemble[1], 1), " ", df$unit[1] %||% "")
  })

  output$ctx_rank_sp <- renderText({
    df <- spatial_layer_data() %>% arrange(desc(p50_ensemble))
    if (is.null(rv()) || nrow(df) == 0) return("\u2014")
    i <- which(df$subbasin == rv())
    if (length(i) == 0) return("\u2014")
    paste0(i[1], " of ", nrow(df))
  })

  output$ctx_range_sp <- renderText({
    df <- spatial_layer_data()
    if (nrow(df) == 0) return("\u2014")
    paste0(round(min(df$p50_ensemble, na.rm = TRUE), 1), " \u2013 ",
           round(max(df$p50_ensemble, na.rm = TRUE), 1))
  })


  ### NAVBAR 4 - DATA DOWNLOADER ###

  # Default the spatial layer to "Water bodies" once a modelled water-body
  # variable is picked, since that's the only layer with matching data.
  observeEvent(input$dl_variable, {
    if (is_wbchem_variable(input$dl_variable)) {
      updateSelectInput(session, "dl_spatial_layer", selected = "waterbodies")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dl_data_type, {
    if (input$dl_data_type == "tabular") {
      updateSelectInput(
        session,
        inputId = "dl_format",
        choices = c("CSV" = "csv", "XLSX" = "xlsx", "Parquet" = "parquet"),
        selected = "csv"
      )
    } else {
      updateSelectInput(
        session,
        inputId = "dl_format",
        choices = c("Shapefile (.zip)" = "shp", "GeoParquet" = "geoparquet", "GPKG" = "gpkg"),
        selected = "gpkg"
      )
    }
  }, ignoreInit = TRUE)

  downloader_tabular_all_data <- reactive({
    req(input$dl_variable)
    if (is_wbchem_variable(input$dl_variable)) {
      chem_key <- wbchem_key_from_variable(input$dl_variable)
      stat <- input$dl_chem_stat %||% "median"
      get_wb_chem_all_waterbodies_monthly(chem_key, stat)
    } else if (is_chem_variable(input$dl_variable)) {
      chem_key <- chem_key_from_variable(input$dl_variable)
      stat <- input$dl_chem_stat %||% "median"
      get_chem_all_subbasins_daily(chem_key, stat)
    } else {
      get_tabular_download(input$dl_variable)
    }
  })

  downloader_tabular_data <- reactive({
    # This page has no water-body picker, so a wbchem tabular download is
    # always all 336 modelled water bodies - unlike subbasin variables, which
    # filter to the currently rv()-selected subbasin.
    if (is_wbchem_variable(input$dl_variable)) {
      return(downloader_tabular_all_data())
    }
    req(rv())
    downloader_tabular_all_data() %>% filter(subbasin == rv())
  })

  downloader_spatial_data <- reactive({
    # "Water bodies" layer only makes sense with a wbchem variable and vice
    # versa (wbchem data has no subbasin/catchment mapping) - any mismatch
    # returns an empty result rather than erroring on a missing column.
    if (identical(input$dl_spatial_layer, "waterbodies") != is_wbchem_variable(input$dl_variable)) {
      return(subbasin_shp %>% filter(FALSE) %>%
               mutate(spatial_layer = input$dl_spatial_layer, variable = input$dl_variable))
    }

    if (identical(input$dl_spatial_layer, "waterbodies")) {
      tab_data <- downloader_tabular_all_data()
      tab_summary_by_wb <- tab_data %>%
        group_by(waterbody_id) %>%
        summarise(
          n_records = n(),
          value_mean = round(mean(value, na.rm = TRUE), 3),
          value_min = round(min(value, na.rm = TRUE), 3),
          value_max = round(max(value, na.rm = TRUE), 3),
          unit = dplyr::first(unit),
          .groups = "drop"
        )
      return(
        waterbody_opcat_shp %>%
          left_join(tab_summary_by_wb, by = c("water_body" = "waterbody_id")) %>%
          mutate(spatial_layer = input$dl_spatial_layer, variable = input$dl_variable)
      )
    }

    tab_data <- downloader_tabular_all_data()

    if (input$dl_spatial_layer == "catchment") {
      if (nrow(tab_data) == 0) {
        return(catchment_shp %>%
                 mutate(spatial_layer = input$dl_spatial_layer,
                        variable = input$dl_variable,
                        n_records = 0,
                        value_mean = NA_real_,
                        value_min = NA_real_,
                        value_max = NA_real_,
                        unit = NA_character_))
      }

      tab_summary <- tab_data %>%
        summarise(
          n_records = n(),
          value_mean = mean(value, na.rm = TRUE),
          value_min = min(value, na.rm = TRUE),
          value_max = max(value, na.rm = TRUE),
          unit = dplyr::first(unit)
        )

      return(catchment_shp %>%
               mutate(spatial_layer = input$dl_spatial_layer,
                      variable = input$dl_variable,
                      n_records = tab_summary$n_records,
                      value_mean = round(tab_summary$value_mean, 3),
                      value_min = round(tab_summary$value_min, 3),
                      value_max = round(tab_summary$value_max, 3),
                      unit = tab_summary$unit))
    }

    tab_summary_by_sub <- tab_data %>%
      mutate(subbasin = as.integer(subbasin)) %>%
      group_by(subbasin) %>%
      summarise(
        n_records = n(),
        value_mean = round(mean(value, na.rm = TRUE), 3),
        value_min = round(min(value, na.rm = TRUE), 3),
        value_max = round(max(value, na.rm = TRUE), 3),
        unit = dplyr::first(unit),
        .groups = "drop"
      )

    subbasin_shp %>%
      mutate(Id = as.integer(Id)) %>%
      left_join(tab_summary_by_sub, by = c("Id" = "subbasin")) %>%
      mutate(spatial_layer = input$dl_spatial_layer,
             variable = input$dl_variable)
  })

  # Context strip stats for the downloader
  downloader_preview_rows <- reactive({
    if (identical(input$dl_data_type, "tabular")) {
      if (!is_wbchem_variable(input$dl_variable) && is.null(rv())) return(0)
      nrow(downloader_tabular_data())
    } else {
      nrow(downloader_spatial_data())
    }
  })

  output$ctx_rows_dl <- renderText({ format(downloader_preview_rows(), big.mark = ",") })

  output$ctx_combos_dl <- renderText({
    if (!identical(input$dl_data_type, "tabular") || is.null(rv())) return("\u2014")
    df <- downloader_tabular_data()
    # Chemical downloads are a single daily series (no scenario/period).
    if (nrow(df) == 0 || !all(c("scenario", "period") %in% names(df))) return("\u2014")
    paste0(dplyr::n_distinct(df$scenario), " \u00d7 ", dplyr::n_distinct(df$period))
  })

  output$ctx_size_dl <- renderText({
    n <- downloader_preview_rows()
    kb <- max(round(n * 48 / 1024), 1)
    paste0(format(kb, big.mark = ","), " kB ", toupper(input$dl_format %||% "csv"))
  })

  output$dl_preview_note <- renderText({
    n <- downloader_preview_rows()
    if (n == 0) return("nothing selected yet")
    paste0("first 100 of ", format(n, big.mark = ","), " rows")
  })

  output$data_table <- DT::renderDataTable({
    if (input$dl_data_type == "tabular") {
      if (!is_wbchem_variable(input$dl_variable) && is.null(rv())) {
        return(DT::datatable(data.frame(Message = "Select a subbasin on the Map tab to build a download."),
                             rownames = FALSE, options = list(dom = "t")))
      }
      df_download <- downloader_tabular_data()
      if (nrow(df_download) == 0) {
        return(DT::datatable(data.frame(Message = "No downloader data available for this subbasin/variable."),
                             rownames = FALSE, options = list(dom = "t")))
      }
      return(DT::datatable(df_download, rownames = FALSE,
                           options = list(dom = "tp", pageLength = 100, scrollY = "100%",
                                          scrollCollapse = TRUE)))
    }

    df_spatial <- downloader_spatial_data()
    if (nrow(df_spatial) == 0) {
      return(DT::datatable(data.frame(Message = "No spatial data available for this subbasin."),
                           rownames = FALSE, options = list(dom = "t")))
    }

    df_spatial_tbl <- sf::st_drop_geometry(df_spatial)
    if (all(c("value_mean", "value_min", "value_max") %in% names(df_spatial_tbl))) {
      df_spatial_tbl <- df_spatial_tbl %>%
        filter(!(is.na(value_mean) & is.na(value_min) & is.na(value_max)))
    }

    if (nrow(df_spatial_tbl) == 0) {
      return(DT::datatable(data.frame(Message = "No spatial rows with values are available for this selection."),
                           rownames = FALSE, options = list(dom = "t")))
    }

    DT::datatable(df_spatial_tbl, rownames = FALSE,
                  options = list(dom = "tp", pageLength = 100, scrollY = "100%",
                                 scrollCollapse = TRUE))
  })

  output$download_data <- downloadHandler(
    filename = function() {
      ext <- switch(input$dl_format,
                    "shp" = "zip",
                    "geoparquet" = "parquet",
                    input$dl_format)
      prefix <- if (input$dl_data_type == "spatial") "ecomix_spatial" else "ecomix_tabular"
      layer_suffix <- if (input$dl_data_type == "spatial") paste0("_", input$dl_spatial_layer) else ""
      id_suffix <- if (input$dl_data_type == "spatial") {
        if (input$dl_spatial_layer == "subbasins") "all_subbasins"
        else if (input$dl_spatial_layer == "waterbodies") "all_waterbodies"
        else "catchment"
      } else if (is_wbchem_variable(input$dl_variable)) {
        "all_waterbodies"
      } else {
        paste0("subbasin_", rv())
      }
      var_label <- if (is_wbchem_variable(input$dl_variable)) wbchem_key_from_variable(input$dl_variable)
        else if (is_chem_variable(input$dl_variable)) chem_key_from_variable(input$dl_variable)
        else input$dl_variable
      paste0(prefix, layer_suffix, "_", id_suffix, "_", var_label, ".", ext)
    },
    content = function(file) {
      if (input$dl_data_type == "tabular") {
        df_download <- downloader_tabular_data()
        if (nrow(df_download) == 0) {
          stop("No tabular data available for the selected subbasin/variable.")
        }

        if (input$dl_format == "csv") {
          write.csv(df_download, file, row.names = FALSE)
        } else if (input$dl_format == "xlsx") {
          if (!requireNamespace("writexl", quietly = TRUE)) {
            stop("Package 'writexl' is required for XLSX downloads. Install with install.packages('writexl').")
          }
          writexl::write_xlsx(df_download, path = file)
        } else {
          arrow::write_parquet(df_download, sink = file)
        }
      } else {
        df_spatial <- downloader_spatial_data()
        if (nrow(df_spatial) == 0) {
          stop("No spatial data available for the selected subbasin.")
        }

        # Export all spatial outputs in British National Grid.
        df_spatial_export <- sf::st_transform(df_spatial, 27700)

        if (input$dl_format == "shp") {
          tmp_dir <- tempfile("ecomix_shp_")
          dir.create(tmp_dir)
          shp_path <- file.path(tmp_dir, "ecomix_spatial.shp")
          sf::st_write(df_spatial_export, dsn = shp_path, driver = "ESRI Shapefile", quiet = TRUE, delete_layer = TRUE)
          shp_files <- list.files(tmp_dir, full.names = TRUE)
          zip_tmp <- tempfile(fileext = ".zip")
          utils::zip(zipfile = zip_tmp, files = shp_files)
          file.copy(zip_tmp, file, overwrite = TRUE)
        } else if (input$dl_format == "gpkg") {
          sf::st_write(df_spatial_export, dsn = file, driver = "GPKG", quiet = TRUE, delete_dsn = TRUE)
        } else {
          sf::st_write(df_spatial_export, dsn = file, driver = "Parquet", quiet = TRUE, delete_dsn = TRUE)
        }
      }
    }
  )

}


### 3. Execution
app <- shinyApp(ui = ui, server = server)

# Allow runApp(appDir = ...) launchers to source this file without recursively
# starting a nested Shiny process.
if (identical(Sys.getenv("ECOMIX_AUTORUN", "1"), "1")) {
  shiny_host <- getOption("shiny.host", "127.0.0.1")
  shiny_port <- getOption("shiny.port", NULL)
  shiny::runApp(
    app,
    host = shiny_host,
    port = shiny_port,
    launch.browser = TRUE
  )
}

app
