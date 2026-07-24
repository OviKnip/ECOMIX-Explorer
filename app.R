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


## Load required datasets

# Study area
catchment_shp <- read_sf(dsn = here("data"), layer = "catchments_wgs")

# HYPE Subbasins (modelling units)
subbasin_shp <- read_sf(dsn = here("data"), layer = "subbasins_wgs")

# WFD surface water operational catchments (Environment Agency WFS),
# pre-filtered to the ones overlapping the study catchments, and each
# subbasin's dominant associated operational catchment (by shared area) -
# both produced by scripts/fetch_operational_catchments.R rather than
# fetched live, so the map doesn't depend on the WFS being reachable at app
# start.
opcat_shp <- read_sf(dsn = here("data"), layer = "operational_catchments_wgs")

df_subbasin_opcat <- read.csv(here("data/subbasin_operational_catchment.csv"))

# Subbasin polygons joined to their associated operational catchment, used to
# colour the Map page. A shared factor palette lets the operational catchment
# overlay and the subbasin fills use matching colours.
subbasin_opcat_shp <- subbasin_shp %>%
  left_join(df_subbasin_opcat, by = c("Id" = "subbasin"))

opcat_levels <- sort(unique(na.omit(subbasin_opcat_shp$opcat_name)))
pal_opcat <- colorFactor(
  palette = grDevices::rainbow(length(opcat_levels), s = 0.6, v = 0.85),
  domain = opcat_levels
)

# Table with climate information
df_stats_climate <- read.csv(here("data/subbasin_climate.csv"))
df_stats_climate$subbasin <- as.numeric(gsub("X", "", df_stats_climate$subbasin))

# Table with subbasin statistics
df_stats_lc <- read.csv(here("data/subbasin_lc.csv"))
df_stats_lc$subbasin <- as.numeric(gsub("X", "", df_stats_lc$subbasin))

# Table with monthly model predictions
#df_climate_data <- read.csv(here("data/Dummy_Data_Climate.csv"))
#df_climate_data$scenario[df_climate_data$scenario == "baseline"] <- "Baseline"
#df_climate_data$scenario[df_climate_data$scenario == "Projection"] <- "SSP585"
#df_climate_data_long <- reshape2::melt(df_climate_data, id.vars = c("scenario", "subbasin", "month"))

# Table with daily Water temperature (used for dummy testing plot)
df_temp <- read.csv(here("data/Dummy_Data_TT2.csv"))
df_temp$date <- as.Date(df_temp$date)

# ---- Chemical / physicochemical monitoring dataset ----
#
# Raw monitoring data is read as all-character columns so that "ND"
# (non-detect) values sit alongside numeric concentrations without read.csv
# silently coercing an entire analyte column to NA.
df_monitoring_raw <- read.csv(
  here("data/monitoring/comix_monitoring_data_Dashboard.csv"),
  colClasses = "character",
  check.names = FALSE
)
colnames(df_monitoring_raw)[1] <- "Site_id" # first header cell carries a stray BOM

# The file's final row is a footnote about the ND/NA convention, not a real
# monitoring record - drop any row without a parseable latitude.
df_monitoring_raw <- df_monitoring_raw[
  !is.na(suppressWarnings(as.numeric(df_monitoring_raw$Latitude))),
]

monitoring_meta_cols <- c("Site_id", "Site_full_name", "Latitude", "Longitude", "Week_Month", "Date")
monitoring_physchem_cols <- tail(setdiff(names(df_monitoring_raw), monitoring_meta_cols), 7)
monitoring_chemical_cols <- setdiff(names(df_monitoring_raw), c(monitoring_meta_cols, monitoring_physchem_cols))

df_monitoring_raw$Date <- as.Date(df_monitoring_raw$Date, format = "%d/%m/%Y")
df_monitoring_raw$Latitude <- as.numeric(df_monitoring_raw$Latitude)
df_monitoring_raw$Longitude <- as.numeric(df_monitoring_raw$Longitude)

# Strip the stray unit suffix baked into the "6PPD-Q" column name for display only.
monitoring_parameter_label <- function(x) sub("_ng L⁻¹$", "", x)

# One row per site, for the monitoring map.
df_monitoring_sites <- df_monitoring_raw %>%
  dplyr::select(Site_id, Site_full_name, Latitude, Longitude) %>%
  dplyr::distinct(Site_id, .keep_all = TRUE)

# Tidy long format: one row per site/date/parameter, with a detection status
# derived from the raw string ("ND" -> non-detect, blank -> no sample taken).
df_monitoring_long <- df_monitoring_raw %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(c(monitoring_chemical_cols, monitoring_physchem_cols)),
    names_to = "parameter",
    values_to = "value_raw"
  ) %>%
  dplyr::mutate(
    parameter_group = ifelse(parameter %in% monitoring_physchem_cols,
                              "Physicochemical parameter", "Organic micropollutant"),
    parameter_label = monitoring_parameter_label(parameter),
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

# Grouped choices for the Site Details time series chemical/parameter
# selectors below.
monitoring_parameter_choices <- list(
  "Organic micropollutants" = setNames(monitoring_chemical_cols, monitoring_parameter_label(monitoring_chemical_cols)),
  "Physicochemical parameters" = setNames(monitoring_physchem_cols, monitoring_physchem_cols)
)

# Site background information (dummy placeholder data - see file header of
# sitesInfo.txt). Parsed once at startup into a named list keyed by Site_id.
parse_site_info <- function(path) {
  lines <- readLines(path, encoding = "UTF-8")
  lines <- lines[!(grepl("^\\s*#", lines) & !grepl("^### ", lines))] # drop comment lines, keep block headers
  block_starts <- grep("^### ", lines)

  site_info <- list()
  for (i in seq_along(block_starts)) {
    site_id <- sub("^### ", "", lines[block_starts[i]])
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

    site_info[[site_id]] <- list(
      site_name = unname(values["Site"]),
      summary = unname(values["Summary"]),
      population = unname(values["Population (catchment)"]),
      landcover = setNames(as.numeric(values[landcover_idx]), landcover_names)
    )
  }
  site_info
}

monitoring_site_info <- parse_site_info(here("data/monitoring/sitesInfo.txt"))

# Updates

# Historical simulations at observation sites
db_name <- here("data/DB_Historical_Sim_Obs")
df_historical_observations <- safe_open_dataset(db_name) %>%
  collect()
df_observed_subbasins <- df_historical_observations %>% group_by(subbasin, variable) %>% slice(1) %>%
  dplyr::select(subbasin, variable)
df_historical_observations$date <- as.Date(df_historical_observations$date)

# HYPE Projections - read in during plot generation
#df_projections_year <-  open_dataset(here("data/DB_Proj_Year")) %>% collect()
#df_projections_month <-  open_dataset(here("data/DB_Proj_Month")) %>% collect()
#df_projections_percentile <-  open_dataset(here("data/DB_Proj_Percentiles")) %>% collect()
#df_projections_percentile$period <- paste0(df_projections_percentile$start_year, "-", df_projections_percentile$end_year)

# Keep Arrow dataset handles open once to avoid repeated metadata scans in renderers.
ds_proj_forcing <- safe_open_dataset(here("data/DB_Proj_Forcing"))
ds_proj_year <- safe_open_dataset(here("data/DB_Proj_Year"))
ds_proj_month <- safe_open_dataset(here("data/DB_Proj_Month"))
ds_proj_percentiles <- safe_open_dataset(here("data/DB_Proj_Percentiles"))

# Derive dynamic widget choices from the Hive-style partition directory names
# on disk (e.g. ".../ssp=SSP585/period=2070-2080/..."). This is a plain
# filesystem walk, so it stays fast even across datasets with tens of
# thousands of parquet files - unlike running distinct()/collect() through
# Arrow, which has to touch every fragment and is far too slow/memory-hungry
# to run at app startup on these dataset sizes.
get_partition_values <- function(dataset_dir, partition_key) {
  dirs <- list.dirs(dataset_dir, recursive = TRUE, full.names = FALSE)
  pattern <- paste0("(^|/)", partition_key, "=([^/]+)$")
  matched <- dirs[grepl(pattern, dirs)]
  if (length(matched) == 0) return(character(0))

  vals <- sub(paste0(".*", partition_key, "="), "", matched)
  vals <- utils::URLdecode(vals)
  vals <- unique(vals)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  sort(vals)
}

available_scenarios <- sort(unique(c(
  get_partition_values(here("data/DB_Proj_Forcing"), "ssp"),
  get_partition_values(here("data/DB_Proj_Year"), "ssp"),
  get_partition_values(here("data/DB_Proj_Month"), "ssp"),
  get_partition_values(here("data/DB_Proj_Percentiles"), "ssp")
)))

if (length(available_scenarios) == 0) {
  available_scenarios <- c("Baseline", "SSP126", "SSP585")
}

climate_period_choices <- get_partition_values(here("data/DB_Proj_Forcing"), "period")
if (length(climate_period_choices) == 0) {
  climate_period_choices <- c("2000-2022", "2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069", "2070-2080")
}

prediction_period_choices <- setdiff(get_partition_values(here("data/DB_Proj_Month"), "period"), "2000-2022")
if (length(prediction_period_choices) == 0) {
  prediction_period_choices <- c("2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069", "2070-2080")
}

# read map input
df_map_input <- read_parquet(here("data/Subbasin_Extremes.gz.parquet"))
df_map_input <- df_map_input %>% filter(prediction_percentile == 99.9,
                                        ssp == "SSP585",
                                        period == "2070-2080")

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

# Climate projections 
#df_ukcp_climate <- open_dataset(here("data/DB_Proj_Forcing")) %>% collect()


## Definition of widgets

# Widget to select one or more scenarios
widget_scenario <-  selectizeInput(
  inputId =  "scenario", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Choose multiple scenarios (press del to remove variable)",
  choices = available_scenarios,
  selected = if ("Baseline" %in% available_scenarios) "Baseline" else available_scenarios[1],
  multiple = TRUE)

# Widget for card 1 -  climate variable (drop down menu - only one selection)
widget_climate_variable <- selectInput(
  inputId = "climate_variable", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Select climate variables",
  choices =
    c("Precipitation", "Temperature"),
  selected = "Precipitation")

# Widget to select temporal resoluton
widget_climate_resolution <- selectInput(
  inputId = "climate_resolution", # This is used in the server part as reactive element (i.e. input$variable)
  label = NULL,
  choices =
    c("Monthly" = "monthly", 
      "Yearly" = "annual"),
  selected = "Monthly")

# Widget to select time periods
widget_climate_period <- selectInput(
  inputId = "climate_period", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Select the period(s)",
  choices = climate_period_choices,
  selected = if ("2000-2022" %in% climate_period_choices) "2000-2022" else climate_period_choices[1],
  multiple = TRUE)

# Widget for card 2 - observational variables 
# this widget has no input yet, because the input depends on the selected subbasin
widget_observed_variable <- selectizeInput(
  inputId = "observation_variable",
  label = "Choose a variable",
  choices = NULL,      # placeholder
  multiple = FALSE
)


## Widgets for card 3 - Tab 1

# HYPE output variable
# NOTE: this control is shown in four different places (Yearly, Monthly, and
# Distributions tabs, plus the Spatial Datasets panel). nav_panel() renders
# all tabs into the DOM at once, so reusing a single selectInput() object
# would create four elements sharing one HTML id - Shiny's client JS resolves
# ids via the first DOM match, so only one of the four would stay in sync
# with input$prediction_variable after a change. Each location therefore gets
# its own inputId.
prediction_variable_choices <- c(
  "Discharge" = "discharge",
  "Soil Moisture" = "Soil moisture",
  "Water Temperature" = "water temperature",
  "Susp. Sediments" =  "Susp. Sediments",
  "Inorganic Nitrogen" = "Inorganic Nitrogen"
)

widget_prediction_variable_yearly <- selectInput(
  inputId = "prediction_variable_yearly",
  label = "Select a variable",
  choices = prediction_variable_choices,
  selected = "Discharge")

widget_prediction_variable_monthly <- selectInput(
  inputId = "prediction_variable_monthly",
  label = "Select a variable",
  choices = prediction_variable_choices,
  selected = "Discharge")

widget_prediction_variable_dist <- selectInput(
  inputId = "prediction_variable_dist",
  label = "Select a variable",
  choices = prediction_variable_choices,
  selected = "Discharge")

widget_prediction_variable_spatial <- selectInput(
  inputId = "prediction_variable_spatial",
  label = "Select a variable",
  choices = prediction_variable_choices,
  selected = "Discharge")

# Output conditions - i.e. Prediction percentile 
widget_prediction_percentile <- selectInput(
  inputId = "prediction_percentile", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Select the conditions(s)",
  choices =
    c("Low (10th percentile)" = "p10",
      "Average (50th percentile)" = "p50",
      "High (90th percentile)" = "p90"),
  selected = "p50",
  multiple = TRUE)

# Prediction period
widget_prediction_period <- selectInput(
  inputId = "prediction_period", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Select the period(s)",
  choices = prediction_period_choices,
  selected = if ("2070-2080" %in% prediction_period_choices) "2070-2080" else prediction_period_choices[1],
  multiple = TRUE)

# Plot type (Absolute or relative change) 
widget_plot_type <- selectInput(
  inputId = "plot_type", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Select plot type",
  choices =
    c("Absolute", "Relative"),
  selected = "Absolute")


widget_download_variable <- selectInput(
  inputId = "dl_variable", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Choose a variable",
  choices =
    c("Precipitation" = "precip",
      "Temperature" = "temp",
      "Discharge" = "runoff",
      "Soil Moisture" = "soil_moisture",
      "Water Temperature" = "water_temperature",
      "Susp. Sediments" = "susp_sediments",
      "Inorganic Nitrogen" = "inorganic_nitrogen"),
  selected = "Precipitation")

widget_download_data_type <- selectInput(
  inputId = "dl_data_type",
  label = "Data type",
  choices = c("Tabular" = "tabular", "Spatial" = "spatial"),
  selected = "tabular"
)

widget_download_spatial_layer <- selectInput(
  inputId = "dl_spatial_layer",
  label = "Spatial layer",
  choices = c("Subbasins" = "subbasins", "Catchment" = "catchment"),
  selected = "subbasins"
)

widget_download_format <- selectInput(
  inputId = "dl_format",
  label = "Download format",
  choices = c("CSV" = "csv", "XLSX" = "xlsx", "Parquet" = "parquet"),
  selected = "csv"
)



### 1. User Interface

ui <- page_navbar(
  # General aesthetics
  id = "main_nav",
  title = "ECOMIX Explorer",
  bg = "#A26BCDFF", # Background color of the navbar
  inverse = TRUE, # This inverts the colors - looks nicer
  tags$style(HTML("\n    .de-summary-boxes .value-box,\n    .de-summary-boxes .bslib-value-box {\n      height: 78px !important;\n      min-height: 78px !important;\n      max-height: 78px !important;\n      overflow: hidden;\n    }\n    .de-summary-boxes .value-box .card-body,\n    .de-summary-boxes .bslib-value-box .card-body {\n      padding-top: 0.45rem;\n      padding-bottom: 0.45rem;\n    }\n  ")),
  
  ## Definition of the Tabs 
  
  # Panel 1: Map for subbasin selection
  nav_panel(title = "Map", 
            fluid = TRUE,
            
            # If map is not the full page enable this:
            # Uses a custom style script
            #div(class="outer",
            #    tags$head(
            #      # Include our custom CSS
            #      includeCSS("styles.css"), # File from: https://github.com/rstudio/shiny-examples/blob/main/063-superzip-example/styles.css
            #    ),
            #   # Add page content
            #    leafletOutput("basemap", width="100%", height="100%")
            #
            #    # Add information panel
            #   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            #                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
            #              width = 330, height = "auto",
            
            #              h2("ZIP explorer"),
            #     ),
            #   ),
            
            
            # Add the map
            leafletOutput("basemap", width="100%", height="100%"),
            
            # Add a panel with of the selected subbasin. 
            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                          width = 500, height = "auto", style = "background: white",
                          
                          # Title
                          h2("Subbasin information"),
                          
                          # Add summary statistics of the selected subbasin
                          uiOutput("selected_subbasin") # uiOutput to implement linebreaks
                          
                          
            ),
            
            # Credits
            tags$div(id="cite", 'Data compiled by Durham University (2026). Operational catchment boundaries © Environment Agency copyright and/or database right 2026, licensed under the Open Government Licence v3.0'

            )),

  ## Panel 1b: Monitoring data - site map
  nav_panel(title = "Monitoring",
            fluid = TRUE,

            # Map of all chemical-monitoring sites - click a marker to open its
            # detailed record on the "Site Details" tab.
            leafletOutput("monitoring_map", width = "100%", height = "100%"),

            tags$div(id = "cite", 'Data compiled by Durham University (2026)')),

  ## Panel 1c: Monitoring data - site detail view
  nav_panel(title = "Site Details",
            fluid = TRUE,

            # Lets the user switch sites directly from this tab, without
            # having to go back to the Monitoring map. Stays in sync with
            # marker clicks on that map (see the server-side observers).
            selectInput(
              inputId = "site_detail_selector",
              label = "Select monitoring site",
              choices = setNames(df_monitoring_sites$Site_id, df_monitoring_sites$Site_full_name),
              width = "300px"
            ),

            # Site background (left) + a small locator map (upper right)
            layout_columns(
              col_widths = c(8, 4),
              uiOutput("site_info_panel"),
              card(
                card_header("Site location"),
                leafletOutput("site_mini_map", height = "220px")
              )
            ),

            # Occurrence grid (kept close to square so row labels can be
            # bigger) alongside a vertical stack of time series for the four
            # chemicals with the greatest concentration range at this site.
            layout_columns(
              col_widths = c(6, 6),
              card(
                full_screen = TRUE,
                card_header("Chemical occurrence grid"),
                div(
                  style = "position: relative;",
                  plotOutput(
                    "site_chem_grid",
                    height = "900px",
                    hover = hoverOpts(id = "site_chem_grid_hover", delay = 60, delayType = "debounce")
                  ),
                  uiOutput("site_chem_grid_tooltip")
                )
              ),
              card(
                card_header("Time series"),
                helpText(
                  "Defaults to the four chemicals with the greatest concentration range at this site - ",
                  "change any of the four selectors below to plot a different chemical or parameter."
                ),
                layout_columns(
                  col_widths = c(3, 3, 3, 3),
                  selectInput("site_ts_chemical_1", "Panel 1", choices = monitoring_parameter_choices),
                  selectInput("site_ts_chemical_2", "Panel 2", choices = monitoring_parameter_choices),
                  selectInput("site_ts_chemical_3", "Panel 3", choices = monitoring_parameter_choices),
                  selectInput("site_ts_chemical_4", "Panel 4", choices = monitoring_parameter_choices)
                ),
                plotOutput("site_time_series", height = "820px")
              )
            )),


  ## Panel 2: Data explorer
  nav_panel(title = "Data Explorer", 
            fluid = TRUE,
            
            ##Add local (nav_page) sidebar layout and content
            
            # Define the elements in the sidebar
            layout_sidebar(
              sidebar = sidebar(
                title = "Scenario selection",
                position = "left",
                
                helpText("Some instructions here"),
                
                # Interactive widget that lets select (and deselect multiple scenarios)
                widget_scenario
              ),
              
              
              ## Add the page content here
              
              ## Heading Widgets - General Information
              div(
                class = "de-summary-boxes",
                layout_columns(
                  fill = FALSE,
                  value_box(
                    title = "Selected Subcatchment",
                    value = textOutput("text_subbasin"),
                    # showcase = bsicons::bs_icon("pin-map-fill")
                  ),
                  value_box(
                    title = "Upstream Area",
                    value = textOutput("text_upstream_area"),
                    # showcase = bsicons::bs_icon("hexagon")
                  ),
                  value_box(
                    title = "Average precipitation",
                    value = textOutput("text_precip"),
                    # showcase = bsicons::bs_icon("cloud-hail-fill")
                  ),
                  value_box(
                    title = "Annual Temperature",
                    value = textOutput("text_maat"),
                    #showcase = bsicons::bs_icon("brightness-high-fill")
                    # showcase = bsicons::bs_icon("thermometer-half")
                  )
                )
              ),
              
              
              ## Plot information
              # Output of the Plot cards
              layout_columns(
                
                # Climate Plot
                card(
                  full_screen = TRUE,
                  card_header("Climate"),
                  # Add a local widget to select the climate variable
                  layout_sidebar(
                    sidebar = sidebar(
                      #title = "Climate variable", 
                      
                      # Widget to select climate variable (drop down menu - only one selection)
                      widget_climate_variable,
                      widget_climate_resolution,
                      widget_climate_period
                    ),
                    plotOutput("climate_plot")
                  ),
                ), 
                
                # Plot to compare Simulations and Observations
                card(
                  full_screen = TRUE,
                  card_header("Simulations vs Observations"),
                  layout_sidebar(
                    sidebar = sidebar(
                      #title = "Observed variable", 
                      # Widget to select climate variable (drop down menu - only one selection)
                      widget_observed_variable
                    ),
                    plotOutput("observation_plot")
                  ),
                ), 
                
                # Projection plots - one card with multiple tabs
                navset_card_pill( 
                  full_screen = TRUE,
                  # First tab: Yearly output variables
                  nav_panel("Yearly",
                            #"Yearly projections", 
                            layout_sidebar(
                              sidebar = sidebar(
                                #title = "HYPE variable", 
                                # Widget to select the HYPE output variable (drop down menu - only one selection)
                                widget_prediction_variable_yearly,
                                widget_prediction_percentile,
                                widget_plot_type,
                              ),

                              plotOutput("projections_yearly_plot"),
                            ),
                  ),
                  
                  # Second tab: Monthly output variables
                  nav_panel("Monthly",
                            #"Monthly Projections",
                            layout_sidebar(
                              sidebar = sidebar(
                                #title = "HYPE variable", 
                                # Widget to select the HYPE output variable (drop down menu - only one selection)
                                widget_prediction_variable_monthly,
                                widget_prediction_period,
                                widget_prediction_percentile,
                                widget_plot_type,
                              ),

                              plotOutput("projections_monthly_plot"),
                            ),
                  ), 
                  
                  # Third tab: Cumulative frequency curves 
                  nav_panel("Distributions ", 
                            #"Cumulative Frequency Curves",
                            layout_sidebar(
                              sidebar = sidebar(
                                #title = "HYPE variable", 
                                # Widget to select the HYPE output variable (drop down menu - only one selection)
                                widget_prediction_variable_dist,
                                widget_prediction_period
                              ),

                              plotOutput("projections_cfc_plot"),
                            ),
                  ), 
                ), 
                
                # Define dimensions
                col_widths = c(6, 6, 12),
                row_heights = c(1, 2)
              ),
            ),
  ),
  
  # Panel 3: Spatial mapping
  nav_panel(title = "Spatial Datasets", 
            fluid = TRUE,
            
            # Define the elements in the sidebar
            layout_sidebar(
              sidebar = sidebar(
                title = "Data selection",
                position = "left",
                
                helpText("Some instructions here"),
                
                # Interactive widget that lets select a hype output variable
                widget_prediction_variable_spatial
              ),
              
              # Define the output map
              leafletOutput("prediction_map", width="100%", height="100%")
            ),
  ),
  
  # Panel 4: Download of data (or tables)
  nav_panel(title = "Data Downloader", 
            fluid = TRUE,
            
            # Define the elements in the sidebar
            layout_sidebar(
              sidebar = sidebar(
                title = "Data selection",
                position = "left",
                
                helpText("Some instructions here"),
                
                # Interactive widget that lets select (and deselect multiple scenarios)
                widget_download_variable,
                widget_download_data_type,
                widget_download_spatial_layer,
                widget_download_format,
                downloadButton("download_data", "Download Data")
              ),
              
              # Define the output table
              DT::dataTableOutput("data_table")
            ),
  ),
  
  ## PANEL 5: Food Web Dynamic Model (Embedded Julia Dash)
  nav_panel(
    title = "Food Web Dynamics",
    fluid = TRUE,
    tags$iframe(
      src = "http://127.0.0.1:8050",  
      height = "700px",
      width = "100%",
      frameborder = "0",
      style = "margin: 0;" 
    )
  ),
  
  ## PANEL 6: Help
  nav_panel(
    title = "Help",
    fluid = TRUE,
    includeHTML(here("help.htm"))
  ),

  nav_spacer(),

  ## Navigation menu
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Posit", href = "https://posit.co")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  )
)

### 2. Server 


## Interactive Map ##
server <- function(input, output, session) {
  
  
  ### NAVBAR 1 - MAP ###
  
  ## Function for selecting a subbasin
  
  # use reactive values to store the id from observing the shape click
  rv <- reactiveVal()

  # Selected monitoring site (Site_id), set by clicking a marker on the
  # Monitoring map; drives the Site Details tab.
  rv_site <- reactiveVal()

  # Expose the already-open Arrow dataset handles (opened once at startup) as
  # a reactive so the plot renderers below can access them through a
  # consistent projection_data$forcing / $year / $month / $percentiles API.
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
  
  # Track clicks
  observeEvent(input$basemap_shape_click, {
    rv(input$basemap_shape_click$id)
  })  

  # Open Data Explorer from map popup link and keep selected subbasin in sync.
  observeEvent(input$open_data_explorer, {
    rv(as.numeric(input$open_data_explorer))
    bslib::nav_select("main_nav", selected = "Data Explorer", session = session)
  })
  
  ## Reactive selection of observed variables
  observation_choices <- reactive({
    if (is.null(rv())) {
      c("Please select a subbasin")
    } else {
      
      # Check if observations are available for the subbasin
      df_tmp <- df_observed_subbasins %>% filter(subbasin == rv())
      if (nrow(df_tmp) == 0) {
        c("No Observations available")
      } else{
        df_tmp[['variable']]
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
  
  # Create the map. Subbasins are coloured by their associated WFD
  # operational catchment (see subbasin_opcat_shp above); the operational
  # catchment boundaries themselves are an optional overlay via the layers
  # control.
  output$basemap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -1.16, lat = 53.75, zoom = 8.5) %>%
      addPolygons(data = subbasin_opcat_shp,
                  fill = T, # Has to be filled to get the hitmarker
                  fillColor = ~ifelse(is.na(opcat_name), "#cccccc", pal_opcat(opcat_name)),
                  fillOpacity = 0.5,
                  color = "black",
                  opacity =  0.5,
                  weight = 1,
                  popup = ~paste0(
                    "<strong>Subbasin id: </strong>", Id,
                    "<br><strong>Operational catchment: </strong>", ifelse(is.na(opcat_name), "None", opcat_name),
                    "<br><a href='#' onclick=\"Shiny.setInputValue('open_data_explorer', '",
                    Id,
                    "', {priority: 'event'}); return false;\">Open in Data Explorer</a>"
                  ),
                  layerId = ~Id,
                  group = "Subbasins") %>%
      addPolygons(data = opcat_shp,
                  fill = FALSE,
                  color = ~pal_opcat(opcat_name),
                  weight = 3,
                  opacity = 0.9,
                  label = ~opcat_name,
                  group = "Operational catchments (WFS)") %>%
      addLayersControl(
        overlayGroups = "Operational catchments (WFS)",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Operational catchments (WFS)")
  })


  # Map information output
  output$selected_subbasin <- renderUI({

    # If no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")

    # If subbasin is selected
    # Subset data
    df_climate_tmp <- selected_climate()
    df_stats_lc_tmp <- selected_lc()
    df_opcat_tmp <- df_subbasin_opcat %>% filter(subbasin == rv())
    opcat_label <- if (nrow(df_opcat_tmp) == 0 || is.na(df_opcat_tmp$opcat_name[1])) "None" else df_opcat_tmp$opcat_name[1]

    HTML(paste("Selected polygon: ", rv(), "<br>",
               "Operational catchment: ", opcat_label, "<br>",
               "Upstream area: ", round(df_stats_lc_tmp$value[df_stats_lc_tmp$variable == "Upstream area"] / 1000000, 2), " km2 <br>",
               "Annual Precipitation: ", round(df_climate_tmp$precip[1], 0), "mm <br>",
               "Mean Annual Temperature: ", round(df_climate_tmp$maat[1], 2), " deg. C <br>",
               sep = ""))
  })


  ### NAVBAR 1B - MONITORING DATA ###

  # Map of all monitoring sites
  output$monitoring_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -1.16, lat = 53.75, zoom = 8) %>%
      addCircleMarkers(
        data = df_monitoring_sites,
        lng = ~Longitude, lat = ~Latitude,
        layerId = ~Site_id,
        radius = 7,
        color = "#A26BCDFF",
        weight = 2,
        fillOpacity = 0.8,
        popup = ~paste0("<strong>", Site_full_name, "</strong><br>Click marker to view details")
      )
  })

  # Track marker clicks and jump to the Site Details tab
  observeEvent(input$monitoring_map_marker_click, {
    rv_site(input$monitoring_map_marker_click$id)
    bslib::nav_select("main_nav", selected = "Site Details", session = session)
  })

  # The Site Details dropdown is another way to set rv_site() directly...
  observeEvent(input$site_detail_selector, {
    rv_site(input$site_detail_selector)
  })

  # ...and this keeps it showing the right site when rv_site() instead comes
  # from a marker click on the Monitoring map. It also resets the four time
  # series selectors to that site's top-range chemicals, so switching sites
  # gives a fresh, sensible default rather than carrying over the previous
  # site's picks (the user can still override any of the four afterwards).
  observeEvent(rv_site(), {
    updateSelectInput(session, "site_detail_selector", selected = rv_site())

    top4 <- selected_site_top_chemicals()$parameter
    if (length(top4) < 4) {
      top4 <- c(top4, setdiff(monitoring_chemical_cols, top4))[seq_len(4)]
    }
    for (i in seq_len(4)) {
      updateSelectInput(session, paste0("site_ts_chemical_", i), selected = top4[i])
    }
  }, ignoreNULL = TRUE)

  # Full monitoring record (all parameters, all weeks) for the selected site
  selected_site_long <- reactive({
    req(rv_site())
    df_monitoring_long %>% dplyr::filter(Site_id == rv_site())
  })

  # Site background panel - dummy placeholder data from sitesInfo.txt
  output$site_info_panel <- renderUI({
    if (is.null(rv_site())) {
      return(tags$p("Please select a site on the Monitoring map to see its details."))
    }

    info <- monitoring_site_info[[rv_site()]]
    if (is.null(info)) {
      return(tags$p(paste("No site information available yet for", rv_site())))
    }

    landcover_text <- paste(
      paste0(names(info$landcover), ": ", info$landcover, "%"),
      collapse = " | "
    )

    tagList(
      h2(info$site_name),
      p(info$summary),
      p(strong("Estimated catchment population: "), format(as.numeric(info$population), big.mark = ",")),
      p(strong("Land cover: "), landcover_text)
    )
  })

  # Small locator map for the selected site
  output$site_mini_map <- renderLeaflet({
    req(rv_site())
    site_row <- df_monitoring_sites %>% dplyr::filter(Site_id == rv_site())
    req(nrow(site_row) == 1)

    leaflet() %>%
      addTiles() %>%
      setView(lng = site_row$Longitude, lat = site_row$Latitude, zoom = 11) %>%
      addCircleMarkers(
        lng = site_row$Longitude, lat = site_row$Latitude,
        radius = 8, color = "#A26BCDFF", weight = 2, fillOpacity = 0.9,
        popup = site_row$Site_full_name
      )
  })

  # The four organic micropollutants with the greatest concentration range
  # (max - min, treating non-detects as 0) at the selected site over the
  # monitoring period, used to auto-pick the time series panels below.
  selected_site_top_chemicals <- reactive({
    selected_site_long() %>%
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

  # Chemical x week occurrence grid data for the selected site. Colour is a
  # log-scaled concentration relative to that parameter's own maximum at this
  # site (0-1), so a single high-concentration compound doesn't wash out the
  # colour scale for every other row. Non-detects sit at the bottom of the
  # scale (true zero); missing samples are shown as a distinct flat grey via
  # NA rather than being folded into the continuous scale.
  # Pulled out of the plot renderer so the hover tooltip below (via
  # nearPoints()) reads the exact same rows/factor levels as the plot.
  site_chem_grid_data <- reactive({
    df_grid <- selected_site_long() %>%
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

  output$site_chem_grid <- renderPlot({
    df_grid <- site_chem_grid_data()

    ggplot(df_grid, aes(x = Date, y = parameter_label, fill = color_value)) +
      geom_tile(color = "white", linewidth = 0.15) +
      scale_fill_viridis_c(
        name = "Relative\nconcentration",
        na.value = "grey85",
        limits = c(0, 1)
      ) +
      scale_x_date(expand = c(0, 0), date_labels = "%b %Y") +
      labs(
        x = "Sampling week", y = NULL,
        caption = "Colour = log-scaled concentration relative to this site's own maximum for that parameter. Grey = no sample taken that week."
      ) +
      theme_bw(base_size = 12) +
      theme(
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        panel.grid = element_blank()
      )
  })

  # Hover tooltip for the occurrence grid. nearPoints() maps the mouse
  # position back onto the same Date x / parameter_label y domain the plot
  # above was drawn with (both discrete factors and Date axes are supported),
  # so this stays correct even though each tile has no visible text of its own.
  output$site_chem_grid_tooltip <- renderUI({
    hover <- input$site_chem_grid_hover
    req(hover)

    point <- nearPoints(site_chem_grid_data(), hover,
                         xvar = "Date", yvar = "parameter_label",
                         threshold = 15, maxpoints = 1)
    if (nrow(point) == 0) return(NULL)

    detail_text <- switch(point$status[1],
      "Detected" = paste0("Value: ", round(point$value_num[1], 3)),
      "Non-detect" = "Not detected (below LOD)",
      "No sample" = "No sample taken"
    )

    style <- paste0(
      "position:absolute; z-index:1000; pointer-events:none; ",
      "left:", hover$coords_css$x + 12, "px; top:", hover$coords_css$y + 12, "px; ",
      "background-color: rgba(255,255,255,0.97); border:1px solid #999; ",
      "border-radius:4px; padding:6px 10px; font-size:12px; ",
      "box-shadow: 1px 1px 4px rgba(0,0,0,0.25); white-space: nowrap;"
    )

    div(
      style = style,
      tags$strong(point$parameter_label[1]), tags$br(),
      format(point$Date[1], "%d %b %Y"), tags$br(),
      detail_text
    )
  })

  # Stacked time series for the four chemicals/parameters chosen in the Panel
  # 1-4 selectors above (defaulted to the greatest-range chemicals by the
  # rv_site() observer above, but freely overridable by the user).
  # "No sample" rows have no value_num to plot as a point (there's nothing
  # detected or not-detected that week) - they only ever show up as a gap in
  # the line, so they're deliberately left out of the point colour scale
  # rather than added as a legend entry that could never actually render.
  output$site_time_series <- renderPlot({
    params <- vapply(seq_len(4), function(i) input[[paste0("site_ts_chemical_", i)]] %||% "", character(1))
    req(all(nzchar(params)))

    # Tag each panel with its own selector index so choosing the same
    # chemical in two panels still produces four distinct stacked plots,
    # each in the order its selector appears in (top to bottom).
    df_ts <- dplyr::bind_rows(lapply(seq_along(params), function(i) {
      selected_site_long() %>%
        dplyr::filter(parameter == params[i]) %>%
        dplyr::mutate(panel_label = paste0("Panel ", i, ": ", monitoring_parameter_label(params[i])))
    }))
    panel_levels <- paste0("Panel ", seq_along(params), ": ", monitoring_parameter_label(params))
    df_ts$panel_label <- factor(df_ts$panel_label, levels = panel_levels)

    ggplot(df_ts, aes(x = Date, y = value_num)) +
      geom_line(color = "#2171B5", na.rm = TRUE) +
      geom_point(aes(color = status), size = 1.6, na.rm = TRUE) +
      scale_color_manual(
        name = "Status",
        breaks = c("Detected", "Non-detect"),
        values = c("Detected" = "#2171B5", "Non-detect" = "#5B84B1FF")
      ) +
      scale_x_date(expand = expansion(mult = c(0.02, 0.06))) +
      facet_wrap(vars(panel_label), ncol = 1, scales = "free_y") +
      labs(
        x = "Date", y = NULL,
        caption = "Gaps indicate no sample taken."
      ) +
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank(), strip.text = element_text(face = "bold"))
  })


  ### NAVBAR 2 - PLOTS ###
  
  ## Heading Widgets - General Information
  output$text_subbasin <- renderText({
    if (is.null(rv())) return ("Please select a subbasin in the map tab")
    df_climate_tmp <- selected_climate()
    as.character(df_climate_tmp$subbasin[1])
  })
  output$text_upstream_area <- renderText({
    if (is.null(rv())) return (" ")
    df_stats_lc_tmp <- selected_lc()
    paste(round(df_stats_lc_tmp$value[df_stats_lc_tmp$variable == "Upstream area"] / 1000000, 2), "km²")
  })
  output$text_precip <- renderText({
    if (is.null(rv())) return (" ")
    df_climate_tmp <- selected_climate()
    paste(as.character(round(df_climate_tmp$precip[1], 0)), "mm")
  })
  output$text_maat <- renderText({
    if (is.null(rv())) return (" ")
    df_climate_tmp <- selected_climate()
    paste(as.character(round(df_climate_tmp$maat[1], 1)), "°C")
  })
  
  
  ## Plot 1:  Climate 
  output$climate_plot <- renderPlot({
    
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    projection_data <- projection_sources()

    sub_subbasin <- rv()
    sub_climate_variable <- unique(input$climate_variable)
    sub_climate_resolution <- input$climate_resolution[1]
    sub_scenarios <- unique(input$scenario)
    sub_periods <- unique(input$climate_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c("2000-2022", sub_periods)
    }
    
    # open data
    df_plot <- projection_data$forcing %>%
      filter(
        variable == sub_climate_variable,
        subbasin %in% sub_subbasin,
        ssp %in% sub_scenarios,
        time_aggregation == sub_climate_resolution) %>%
      collect_quiet()
    
    # Data wrangling
    if(sub_climate_resolution == "monthly") {
      df_plot <- df_plot %>% filter(period %in% sub_periods) %>%
        mutate("scenario" = paste0(ssp, " (", period, ")"))
      df_plot$xaxis <- df_plot$month
      xlab <- "Month"
    } 
    if(sub_climate_resolution == "annual") {
      df_plot$scenario <- df_plot$ssp
      df_plot$xaxis <- df_plot$year
      xlab <- "Year"
    } 
    ylab <- paste0(sub_climate_resolution, " ", sub_climate_variable, " [", unique(df_plot$unit), "]")
    
    # plotting
    ggplot(df_plot, aes(x = xaxis, y = p50, color = scenario, fill = scenario)) +
      geom_line() + 
      geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.2, linewidth = 0.05)+
      scale_color_jco()+
      scale_fill_jco()+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0)) +
      labs(x = xlab, y = ylab,  
           title = toupper(paste(sub_climate_resolution, sub_climate_variable))) + 
      theme_bw() +
      theme(text = element_text(size = 11),
            legend.position = "bottom",
            legend.title = element_blank(),
            plot.margin=unit(c(.2,.5,.2,.2),"cm"))
    
  })
  
  ## Plot 2 - Observations
  output$observation_plot <-  renderPlot({
    
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    # Filter subbasin 
    df_data <- selected_historical()
    
    # Subset the variable based on widget
    df_plot <- df_data[df_data$variable == input$observation_variable, ]
    
    ## Discharge plot
    if (input$observation_variable == "discharge") {
      
      # data wrangling 
      df_tmp <- df_plot %>% dplyr::select(-prediction_percentile, -variable, -sim_P10, -sim_P50, -sim_P90) %>% 
        rename("low" = obs_min, "med" = obs, "high" = obs_max) %>% mutate("type" = "Observation")
      df_plot <- df_plot %>% dplyr::select(-prediction_percentile, -variable, -obs_min, -obs_max, -obs) %>% 
        rename("low" = sim_P10, "med" = sim_P50, "high" = sim_P90) %>% mutate("type" = "Simulation")
      df_plot <- rbind(df_plot, df_tmp)
      
      # plotting
      ggplot(df_plot, aes(x = date, y = med, color = type, fill = type)) +
        geom_line() + 
        geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5, linewidth = 0.05)+
        scale_color_manual(values = c("#5B84B1FF", "#FC766AFF"))+
        scale_fill_manual(values = c("#5B84B1FF", "#FC766AFF"))+
        scale_x_date(expand = c(0,0))+
        scale_y_continuous(limits = c(0, max(df_plot$high)*1.1), expand = c(0,0)) +
        labs(x = "Year", y = "Discharge [m³/s]",  
             title = toupper(paste0(unique(df_plot$station_label), " (Station ", unique(df_plot$id_station), ")"))) + 
        theme_bw() +
        theme(text = element_text(size = 11),
              legend.position = "bottom",
              legend.title = element_blank(),
              plot.margin=unit(c(.2,.5,.2,.2),"cm"))
      
      # Substance Plotting
    } else{
      
      # plotting
      if (nrow(df_plot) == 0 || all(is.na(c(df_plot$sim_P90, df_plot$obs)))) {
        return(NULL)
      }
      upper <- max(c(df_plot$sim_P90, df_plot$obs), na.rm = TRUE)
      ylab <- paste0(unique(df_plot$variable), " [", unique(df_plot$unit), "]")
      ggplot(df_plot, aes(x = date, y = sim_P50)) +
        geom_line(color = "#FC766AFF") + 
        geom_ribbon(aes(ymin = sim_P10, ymax = sim_P90), fill = "#FC766AFF", alpha = 0.5, linewidth = 0.05)+
        geom_point(aes(x = date, y = obs), shape = 4, color = "grey3", size = 2) + 
        scale_x_date(expand = c(0,0))+
        scale_y_continuous(limits = c(0, upper*1.1), expand = c(0,0)) +
        labs(x = "Year", y = ylab,  title = toupper(paste0(unique(df_plot$station_label), " (Station ", unique(df_plot$id_station), ")"))) + 
        theme_bw() +
        theme(text = element_text(size = 11),
              legend.position = "bottom",
              legend.title = element_blank(),
              plot.margin=unit(c(.2,.5,.2,.2),"cm"))
    }
    
  })
  
  
  ## Plot 3:  Yearly Projections  
  output$projections_yearly_plot <- renderPlot({
    
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    projection_data <- projection_sources()

    # Filter dataset
    #df_plot <- df_projections_year %>% filter(subbasin == rv())
    
    # Subset the dataset based on widget inputs
    sub_subbasin <- rv()
    sub_variable <- input$prediction_variable_yearly[1]
    sub_scenarios <- unique(input$scenario)
    sub_percentiles <- unique(input$prediction_percentile)
    sub_plot_type <- unique(input$plot_type)
    
    # open database
        df_projections_year <- projection_data$year %>%
      filter(subbasin %in% sub_subbasin,
             hype_variable  %in% sub_variable,
             ssp %in% c("Baseline",sub_scenarios),
             prediction_percentile %in% sub_percentiles) %>%
      collect()
    
    ## Plotting of Absolute Projections
    # Absolute change
    if (sub_plot_type == "Absolute") {
      
      df_plot <- df_projections_year %>% filter(ssp %in% sub_scenarios)
      
      
      
      # Data wrangling
      df_plot <- df_plot[!(df_plot$ssp == "Baseline" & df_plot$year > 2020), ] # filter overlap
      df_plot$percentile_label <- factor(df_plot$prediction_percentile, levels = c("p10", "p50", "p90"),
                                         labels = c("Low (10th percentile)", "Average (50th percentile)", "High (90th percentile)"))
      
      
      ylab <- paste0(sub_scenarios, " [", unique(df_plot$unit), "]")
      ggplot(df_plot, aes(x = year, y = p50_ensemble, color = ssp, fill = ssp, linetype = percentile_label)) +
        geom_line() +
        geom_ribbon(aes(ymin = p10_ensemble, ymax = p90_ensemble), alpha = 0.2, linewidth = 0.05)+
        scale_x_continuous(expand = c(0,0)) +
        scale_color_jco()+
        scale_fill_jco()+
        labs(x = "Year", y = ylab,  title = toupper(sub_variable), linetype= "Frequency", fill = "Scenario", color = "Scenario") +
        theme_bw() +
        theme(text = element_text(size = 11),
              legend.position = "bottom",
              plot.margin=unit(c(.2,.5,.2,.2),"cm"))
    } else {
      
      # subset projections
      df_proj <- df_projections_year %>% filter(ssp != "Baseline")
      
      # subset baseline and aggregate the years to a single reference value
      df_base <- df_projections_year %>% filter(ssp == "Baseline") %>%
        group_by(subbasin, prediction_percentile) %>% 
        summarise("p10_base" = mean(p10_ensemble), "p50_base" = mean(p50_ensemble), "p90_base" = mean(p90_ensemble))
      
      # Calculate anomalies
      df_proj <- left_join(df_proj, df_base, by = c("subbasin", "prediction_percentile"))
      df_proj$p10_anomaly <- df_proj$p10_ensemble - df_proj$p10_base
      df_proj$p50_anomaly <- df_proj$p50_ensemble - df_proj$p50_base
      df_proj$p90_anomaly <- df_proj$p90_ensemble - df_proj$p90_base
      
      
      # uci bands - lower intervals may have larger anomalies than higher intervals
      df_proj$low_uci <- apply(df_proj[,c("p10_anomaly", "p50_anomaly", "p90_anomaly")], 1, min, na.rm = TRUE)
      df_proj$high_uci <- apply(df_proj[,c("p10_anomaly", "p50_anomaly", "p90_anomaly")], 1, max, na.rm = TRUE)      
      
      # Data wrangling
      df_plot <- df_proj
      ylab <- paste0("Change to baseline: ", sub_variable, " [", unique(df_plot$unit), "]")
      
      # plotting
      ggplot(df_plot, aes(x = year, color = ssp, fill = ssp, linetype = prediction_percentile)) +
        geom_line(aes(y = low_uci)) +
        geom_line(aes(y = high_uci)) +
        geom_ribbon(aes(ymin = low_uci, ymax = high_uci), alpha = 0.2, linewidth = 0.05)+
        scale_x_continuous(expand = c(0,0)) + 
        scale_color_jco()+
        scale_fill_jco()+
        labs(x = "Year", y = ylab,  title = toupper(paste(sub_variable, "anomalies (change to 2000-2020 baseline)")),
             linetype= "Frequency", fill = "Scenario", color = "Scenario") + 
        theme_bw() +
        theme(text = element_text(size = 11),
              legend.position = "bottom",
              plot.margin=unit(c(.2,.5,.2,.2),"cm"))
    }
  })
  
  ## Plot 4:  Monthly Projections  
  output$projections_monthly_plot <- renderPlot({
    
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    projection_data <- projection_sources()

    # Store widget inputs
    sub_subbasin <- rv()
    sub_variable <- input$prediction_variable_monthly[1]
    sub_scenarios <- unique(input$scenario)
    sub_percentiles <- unique(input$prediction_percentile)
    sub_periods <- unique(input$prediction_period)
    # add baseline period
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c("2000-2022", sub_periods)
    }
    sub_plot_type <- unique(input$plot_type)
    
        df_projections_month <- projection_data$month %>%
      filter(subbasin %in% sub_subbasin, 
             hype_variable  %in% sub_variable, 
             ssp %in% c("Baseline", sub_scenarios),
             prediction_percentile %in% sub_percentiles,
             period %in% sub_periods) %>%
      collect()
    
    # Absolute change plot
    if (sub_plot_type == "Absolute") {
      
      #Subset the dataset based on widget inputs
      df_plot <- df_projections_month %>% filter(ssp %in% sub_scenarios)
      
      # Data wrangling
      df_plot <- df_plot %>% mutate("scenario" = paste0(ssp, " (", period, ")"))
      df_plot$percentile_label <- factor(df_plot$prediction_percentile, levels = c("p10", "p50", "p90"),
                                         labels = c("Low (10th percentile)", "Average (50th percentile)", "High (90th percentile)"))
      
      
      ylab <- paste0(sub_variable, " [", unique(df_plot$unit), "]")
      ggplot(df_plot, aes(x = month, y = p50_ensemble, color = scenario, fill = scenario, linetype = percentile_label)) +
        geom_line() +
        geom_ribbon(aes(ymin = p10_ensemble, ymax = p90_ensemble), alpha = 0.2, linewidth = 0.05)+
        scale_x_continuous(expand = c(0,0)) + 
        scale_color_jco()+
        scale_fill_jco()+
        labs(x = "Month", y = ylab,  title = toupper(sub_variable), linetype= "Frequency", fill = "Scenario", color = "Scenario") + 
        theme_bw() +
        theme(text = element_text(size = 11),
              legend.position = "bottom",
              plot.margin=unit(c(.2,.5,.2,.2),"cm"))
      
      # Anomaly Plots
    } else{
      
      # subset projections
      df_proj <- df_projections_month %>% filter(ssp != "Baseline")
      
      # subset baseline and aggregate the years to a single reference value
      df_base <- df_projections_month %>% filter(ssp == "Baseline") %>%
        group_by(subbasin, month, prediction_percentile) %>% 
        summarise("p10_base" = mean(p10_ensemble), "p50_base" = mean(p50_ensemble), "p90_base" = mean(p90_ensemble))
      
      # Calculate anomalies
      df_proj <- left_join(df_proj, df_base, by = c("subbasin", "month", "prediction_percentile"))
      df_proj$p10_anomaly <- df_proj$p10_ensemble - df_proj$p10_base
      df_proj$p50_anomaly <- df_proj$p50_ensemble - df_proj$p50_base
      df_proj$p90_anomaly <- df_proj$p90_ensemble - df_proj$p90_base
      
      
      # uci bands - lower intervals may have larger anomalies than higher intervals
      df_proj$low_uci <- apply(df_proj[,c("p10_anomaly", "p50_anomaly", "p90_anomaly")], 1, min, na.rm = TRUE)
      df_proj$high_uci <- apply(df_proj[,c("p10_anomaly", "p50_anomaly", "p90_anomaly")], 1, max, na.rm = TRUE)
      #df_proj$p10_uci <- df_proj$p10_anomaly
      #df_proj$p10_uci[df_proj$p90_anomaly < df_proj$p10_anomaly] <- df_proj$p90_anomaly[df_proj$p90_anomaly < df_proj$p10_anomaly]
      #df_proj$p90_uci <- df_proj$p90_anomaly
      #df_proj$p90_uci[df_proj$p10_anomaly > df_proj$p90_anomaly] <- df_proj$p10_anomaly[df_proj$p10_anomaly > df_proj$p90_anomaly]
      
      # Data wrangling
      df_plot <- df_proj
      ylab <- paste0("Change to baseline: ", sub_variable, " [", unique(df_plot$unit), "]")
      
      # plotting
      ggplot(df_plot, aes(x = month, color = ssp, fill = ssp, linetype = prediction_percentile)) +
        geom_line(aes(y = low_uci)) +
        geom_line(aes(y = high_uci)) +
        geom_ribbon(aes(ymin = low_uci, ymax = high_uci), alpha = 0.2, linewidth = 0.05)+
        scale_x_continuous(expand = c(0,0)) + 
        scale_color_jco()+
        scale_fill_jco()+
        labs(x = "Month", y = ylab,  title = toupper(paste(sub_variable, "anomalies (change to 2000-2020 baseline)")),
             linetype= "Frequency", fill = "Scenario", color = "Scenario") + 
        theme_bw() +
        theme(text = element_text(size = 11),
              legend.position = "bottom",
              plot.margin=unit(c(.2,.5,.2,.2),"cm"))
    }
  })
  
  ## Plot 5:  Cumulative Frequency Curves
  output$projections_cfc_plot <- renderPlot({
    
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    projection_data <- projection_sources()

    # Subset the dataset based on widget inputs
    sub_subbasin <- rv()
    sub_variable <- input$prediction_variable_dist[1]
    sub_scenarios <- unique(input$scenario)
    sub_periods <- unique(input$prediction_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c("2000-2022", sub_periods)
    }
    
        df_plot <- projection_data$percentiles %>% 
      filter(subbasin %in% sub_subbasin, 
             hype_variable  %in% sub_variable, 
             ssp %in% sub_scenarios,
             period %in% sub_periods) %>%
      collect()
    
    # Data wrangling
    df_plot <- df_plot %>% mutate("scenario" = paste0(ssp, " (", period, ")"))
    ylab <- paste0(sub_variable, " [", unique(df_plot$unit), "]")
    
    # plotting
    ggplot(df_plot, aes(x = prediction_percentile, y = p50_ensemble, color = scenario, fill = scenario)) +
      geom_line() +
      geom_ribbon(aes(ymin = p10_ensemble, ymax = p90_ensemble), alpha = 0.2, linewidth = 0.05)+
      scale_x_continuous(expand = c(0,0), breaks = c(0,25,50,75,100), labels = c("0", "25", "50 (Median)", "75", "100")) + 
      scale_y_log10()+
      scale_color_jco()+
      scale_fill_jco()+
      labs(x = "Cummulative Frequency [%]", y = ylab,  title = toupper(paste("Cumulative Frequecy Curve for", sub_variable)), 
           fill = "Scenario", color = "Scenario") + 
      theme_bw() +
      theme(text = element_text(size = 11),
            legend.position = "bottom",
            plot.margin=unit(c(.2,.5,.2,.2),"cm"))
  })
  
  
  ## Dummy plot - used to test UI interactions 
  output$dummy_plot <-  renderPlot({
    
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    # Filter subbasin
    df_plot <- df_temp %>% filter(subbasin == rv())
    
    # generate the plot
    ggplot(df_plot, aes(x = date,  y = temperature)) +
      geom_line(color = "#2171B5") + 
      scale_x_date(expand = c(0,0)) + 
      labs(x = "Date", y = "Temperature [°C]") + 
      theme_bw()
    
  })
  
  ## Spatial variability map
  output$prediction_map <- renderLeaflet({

    sub_variable <- input$prediction_variable_spatial[1]
    df_sub <- df_map_input %>% filter(hype_variable == sub_variable)
    shp_map <- left_join(subbasin_shp, df_sub, by = c("Id" = "subbasin")) %>% filter(!is.na(hype_variable))
    
    pal <- colorNumeric(
      palette = "viridis", 
      domain = shp_map$p50_ensemble
    )
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -1.16, lat = 53.75, zoom = 8.5) %>%
      addPolygons(
        data = shp_map,
        fillColor = ~pal(p50_ensemble),
        fillOpacity = 0.7,
        color = "black",
        weight = 1
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = shp_map$p50_ensemble,
        title = "P99.9"
      )
    
  })
  
  ## Table in navbar 3.
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
    get_tabular_download(input$dl_variable)
  })

  downloader_tabular_data <- reactive({
    req(rv())
    downloader_tabular_all_data() %>% filter(subbasin == rv())
  })

  downloader_spatial_data <- reactive({
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

    # Subbasin spatial exports include all subbasins and per-subbasin summaries.
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

  output$data_table <-  DT::renderDataTable({
    if (input$dl_data_type == "tabular") {
      if (is.null(rv())) {
        return(data.frame("V1" = "Please select a subbasin by clicking on the map", "V2" = NA))
      }
      df_download <- downloader_tabular_data()
      if (nrow(df_download) == 0) {
        return(data.frame("V1" = "No downloader data available for this subbasin/variable", "V2" = NA))
      }
      return(df_download)
    }

    df_spatial <- downloader_spatial_data()
    if (nrow(df_spatial) == 0) {
      return(data.frame("V1" = "No spatial data available for this subbasin", "V2" = NA))
    }

    df_spatial_tbl <- sf::st_drop_geometry(df_spatial)
    if (all(c("value_mean", "value_min", "value_max") %in% names(df_spatial_tbl))) {
      df_spatial_tbl <- df_spatial_tbl %>%
        filter(!(is.na(value_mean) & is.na(value_min) & is.na(value_max)))
    }

    if (nrow(df_spatial_tbl) == 0) {
      return(data.frame("V1" = "No spatial rows with values are available for this selection", "V2" = NA))
    }

    df_spatial_tbl
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
        if (input$dl_spatial_layer == "subbasins") "all_subbasins" else "catchment"
      } else {
        paste0("subbasin_", rv())
      }
      paste0(prefix, layer_suffix, "_", id_suffix, "_", input$dl_variable, ".", ext)
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


# ### 3. Execution
# shinyApp(ui = ui, server = server)
# shinyApp(ui, function(input, output) {}) # For testing - creates no plots

# shiny::runApp()

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