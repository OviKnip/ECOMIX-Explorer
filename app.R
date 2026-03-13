# Define path of the project
path <- "D:/GITHUB/ECOMIX_Explorer/ECOMIX-Explorer/"
setwd(path) 

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

# Decimal numbers
options(scipen = 999)


## Load required datasets

# Study area
catchment_shp <- read_sf(dsn = here("data"), layer = "Wharfe_catchments_wgs")

# HYPE Subbasins (modelling units)
subbasin_shp <- read_sf(dsn = here("data"), layer = "Wharfe_subbasins_wgs")

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

# Updates

# Historical simulations at observation sites
db_name <- here("data/DB_Historical_Sim_Obs")
df_historical_observations <- open_dataset(db_name) %>%
  collect()
df_observed_subbasins <- df_historical_observations %>% group_by(subbasin, variable) %>% slice(1) %>%
  dplyr::select(subbasin, variable)
df_historical_observations$date <- as.Date(df_historical_observations$date)

# HYPE Projections - read in during plot generation
#df_projections_year <-  open_dataset(here("data/DB_Proj_Year")) %>% collect()
#df_projections_month <-  open_dataset(here("data/DB_Proj_Month")) %>% collect()
#df_projections_percentile <-  open_dataset(here("data/DB_Proj_Percentiles")) %>% collect()
#df_projections_percentile$period <- paste0(df_projections_percentile$start_year, "-", df_projections_percentile$end_year)

# read map input
df_map_input <- read_parquet(here("data/Subbasin_Extremes.gz.parquet"))
df_map_input <- df_map_input %>% filter(prediction_percentile == 99.9,
                                        ssp == "SSP585",
                                        period == "2070-2080")

# Climate projections 
#df_ukcp_climate <- open_dataset(here("data/DB_Proj_Forcing")) %>% collect()


## Definition of widgets

# Widget to select one or more scenarios
widget_scenario <-  selectizeInput(
  inputId =  "scenario", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Choose multiple scenarios (press del to remove variable)",
  choices =
    c("Baseline", "SSP126", "SSP585"),
  selected = "Baseline", 
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
  choices =
    c("2000-2022", "2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069", "2070-2080"),
  selected = "2000-2022",
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
widget_prediction_variable <- selectInput(
  inputId = "prediction_variable", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Select a variable",
  choices =
    c("Discharge" = "discharge",
      "Soil Moisture" = "Soil moisture",
      "Water Temperature" = "water temperature",
      "Susp. Sediments" =  "Susp. Sediments", 
      "Inorganic Nitrogen" = "Inorganic Nitrogen"),
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
  choices =
    c("2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069", "2070-2080"),
  selected = "2070-2080",
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
      "Discharge" = "runoff"),
  selected = "Precipitation")



### 1. User Interface

ui <- page_navbar(
  # General aesthetics
  title = "ECOMIX Explorer",
  bg = "#2D89C8",
  inverse = TRUE, # This inverts the colors - looks nicer
  
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
            tags$div(id="cite", 'Data compiled by Durham University (2026)'
                     
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
              layout_columns(
                fill = FALSE,
                value_box(
                  title = "Selected Subbasin",
                  value = textOutput("text_subbasin"),
                  showcase = bsicons::bs_icon("pin-map-fill")
                ),
                value_box(
                  title = "Upstream Area",
                  value = textOutput("text_upstream_area"),
                  showcase = bsicons::bs_icon("hexagon")
                ),
                value_box(
                  title = "Average precipitation",
                  value = textOutput("text_precip"),
                  showcase = bsicons::bs_icon("cloud-hail-fill")
                ),
                value_box(
                  title = "Annual Temperature",
                  value = textOutput("text_maat"),
                  #showcase = bsicons::bs_icon("brightness-high-fill")
                  showcase = bsicons::bs_icon("thermometer-half")
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
                                widget_prediction_variable,
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
                                widget_prediction_variable,
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
                                widget_prediction_variable,
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
                widget_prediction_variable
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
                widget_download_variable
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
  
  # Track clicks
  observeEvent(input$basemap_shape_click, {
    rv(input$basemap_shape_click$id)
  })  
  
  # Create Popup options
  popup_text <- paste0("<strong>Subbasin id: </strong>", 
                       subbasin_shp$Id)
  
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
  
  # Create the map (catchment part works)
  output$basemap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -1.16, lat = 53.75, zoom = 8.5) %>%
      addPolygons(data=subbasin_shp,
                  fill = T, # Has to be filled to get the hitmarker
                  fillOpacity = 0.01,
                  color = "black",
                  opacity =  0.5,
                  weight = 2,
                  popup = popup_text,
                  layerId = ~Id)
  })
  
  
  # Map information output
  output$selected_subbasin <- renderUI({
    
    # If no subbasin was selected 
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    # If subbasin is selected 
    # Subset data
    df_climate_tmp <- df_stats_climate %>% filter(subbasin == rv())
    df_stats_lc_tmp <- df_stats_lc %>% filter(subbasin == rv())
    
    HTML(paste("Selected polygon: ", rv(), "<br>",
               "Upstream area: ", round(df_stats_lc_tmp$value[df_stats_lc_tmp$variable == "Upstream area"] / 1000000, 2), " km2 <br>",
               "Annual Precipitation: ", round(df_climate_tmp$precip[1], 0), "mm <br>",
               "Mean Annual Temperature: ", round(df_climate_tmp$maat[1], 2), " deg. C <br>",
               sep = ""))
  })
  
  
  ### NAVBAR 2 - PLOTS ###
  
  ## Heading Widgets - General Information
  output$text_subbasin <- renderText({
    if (is.null(rv())) return ("Please select a subbasin in the map tab")
    df_climate_tmp <- df_stats_climate %>% filter(subbasin == rv())
    as.character(df_climate_tmp$subbasin[1])
  })
  output$text_upstream_area <- renderText({
    if (is.null(rv())) return (" ")
    df_stats_lc_tmp <- df_stats_lc %>% filter(subbasin == rv())
    paste(round(df_stats_lc_tmp$value[df_stats_lc_tmp$variable == "Upstream area"] / 1000000, 2), "km²")
  })
  output$text_precip <- renderText({
    if (is.null(rv())) return (" ")
    df_climate_tmp <- df_stats_climate %>% filter(subbasin == rv())
    paste(as.character(round(df_climate_tmp$precip[1]), 0), "mm")
  })
  output$text_maat <- renderText({
    if (is.null(rv())) return (" ")
    df_climate_tmp <- df_stats_climate %>% filter(subbasin == rv())
    paste(as.character(round(df_climate_tmp$maat[1]), 1), "°C")
  })
  
  
  ## Plot 1:  Climate 
  output$climate_plot <- renderPlot({
    
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    sub_subbasin <- rv()
    sub_climate_variable <- unique(input$climate_variable)
    sub_climate_resolution <- input$climate_resolution[1]
    sub_scenarios <- unique(input$scenario)
    sub_periods <- unique(input$climate_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c("2000-2022", sub_periods)
    }
    
    # open data
    df_plot <- open_dataset(here("data/DB_Proj_Forcing")) %>% 
      filter(
        variable == sub_climate_variable,
        subbasin %in% sub_subbasin, 
        ssp %in% sub_scenarios,
        time_aggregation == sub_climate_resolution) %>% 
      collect()
    
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
    df_data <- df_historical_observations %>% filter(subbasin == rv())
    
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
      upper <- max(c(df_plot$sim_P90, df_plot$obs), na.rm = T)
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
    
    # Filter dataset
    #df_plot <- df_projections_year %>% filter(subbasin == rv())
    
    # Subset the dataset based on widget inputs
    sub_subbasin <- rv()
    sub_variable <- input$prediction_variable[1]
    sub_scenarios <- unique(input$scenario)
    sub_percentiles <- unique(input$prediction_percentile)
    sub_plot_type <- unique(input$plot_type)
    
    # open database
    df_projections_year <- open_dataset(here("data/DB_Proj_Year")) %>%
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
      library(ggsci)
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
    
    # Store widget inputs
    sub_subbasin <- rv()
    sub_variable <- input$prediction_variable[1]
    sub_scenarios <- unique(input$scenario)
    sub_percentiles <- unique(input$prediction_percentile)
    sub_periods <- unique(input$prediction_period)
    # add baseline period
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c("2000-2022", sub_periods)
    }
    sub_plot_type <- unique(input$plot_type)
    
    df_projections_month <- open_dataset(here("data/DB_Proj_Month")) %>%
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
      library(ggsci)
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
    
    # Subset the dataset based on widget inputs
    sub_subbasin <- rv()
    sub_variable <- input$prediction_variable[1]
    sub_scenarios <- unique(input$scenario)
    sub_periods <- unique(input$prediction_period)
    if ("Baseline" %in% sub_scenarios) {
      sub_periods <- c("2000-2022", sub_periods)
    }
    
    df_plot <- open_dataset(here("data/DB_Proj_Percentiles")) %>% 
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
    
    sub_variable <- input$prediction_variable[1]
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
  output$data_table <-  DT::renderDataTable({
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return (data.frame("V1" = "Please select a subbasin by clicking on the map", "V2" = NA))
    
    # Subset to subbasin and variable
    df_data_long <- df_data_long %>% filter(subbasin == rv(), variable == input$dl_variable) %>%
      mutate("value" = round(value, 1)) %>% dplyr::select(subbasin, scenario, month, value)
  })
  
}


### 3. Execution
shinyApp(ui = ui, server = server)
#shinyApp(ui, function(input, output) {}) # For testing - creates no plots

