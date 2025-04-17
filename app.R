
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

# Decimal numbers
options(scipen = 999)

# Define path of the project
#path <- "D:/Work/Data/GITHUB/ECOMIX_Explorer/ECOMIX-Explorer/"


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
df_data <- read.csv(here("data/Dummy_Data_Climate.csv"))
df_data_long <- reshape2::melt(df_data, id.vars = c("scenario", "subbasin", "month"))

# Table with daily runoff
df_runoff <- read.csv(here("data/Dummy_Data_Runoff.csv"))
df_runoff$date <- as.Date(df_runoff$date)

# Table with daily Water temperature 
df_temp <- read.csv(here("data/Dummy_Data_TT2.csv"))
df_temp$date <- as.Date(df_temp$date)


## Definition of User Interface 

# Definition of cards (cards include content and are a way to organise the content of pages)
# for now we fill them with the same plot
cards <- list(
  card(
    full_screen = TRUE,
    card_header("Climate"),
    plotOutput("climate_plot")
  ),
  card(
    full_screen = TRUE,
    card_header("Hydrology"),
    plotOutput("runoff_plot")
  ),
  card(
    full_screen = TRUE,
    card_header("Hydrograph"),
    plotOutput("hydrograph_plot")
  ),
  card(
    full_screen = TRUE,
    card_header("Water Temperature"),
    plotOutput("temperature_plot")
  )
)
# Can be used in UI as followed: (1st row 2 plots, 2nd row 1 plot)
#layout_columns(
#  cards[[1]], cards[[2]]),
#cards[[3]]


## Definition of widgets

# Widget to select one or more scenarios
widget_scenario <-  selectizeInput(
  inputId =  "scenario", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Choose multiple scenarios (press del to remove variable)",
  choices =
    c("Baseline" = "baseline",
      "Projection" = "Projection"),
  selected = "baseline", 
  multiple = TRUE)

# Widget to select climate variable (drop down menu - only one selection)
widget_climate_variable <- selectInput(
  inputId = "variable", # This is used in the server part as reactive element (i.e. input$variable)
  label = "Choose a variable to display",
  choices =
    c("Precipitation" = "precip",
      "Temperature" = "temp"),
  selected = "Precipitation")

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
            tags$div(id="cite", 'Data compiled by Durham University (2024)'
                     
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
                      title = "Climate variable", 
                      
                      # Widget to select climate variable (drop down menu - only one selection)
                      widget_climate_variable
                    ),
                    plotOutput("climate_plot")
                  ),
                ), 
                
                # Hydrology Plot
                card(
                  full_screen = TRUE,
                  card_header("Monthly Runoff"),
                  plotOutput("runoff_plot")
                )
              ),
              # Hydrograph plot
              card(
                full_screen = TRUE,
                card_header("Hydrograph"),
                plotOutput("hydrograph_plot")
              ),
              # Temperature plot
              card(
                full_screen = TRUE,
                card_header("Water Temperature"),
                plotOutput("temperature_plot")
              )
            ),
  ),
  
  # Panel 3: Download of data (or tables)
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
  
  ## PANEL 4: Food Web Dynamic Model (Embedded Julia Dash)
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
  
  # Create the map (catchment part works)
  output$basemap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -1.16, lat = 53.75, zoom = 8.5) %>%
      addPolygons(data=catchment_shp,
                  fill = T, # Has to be filled to get the hitmarker
                  fillOpacity = 0.01,
                  color = "black", 
                  weight = 2) %>%
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
    
    # Filter subbasin
    df_data_long <- df_data_long %>% filter(subbasin == rv())
    
    # Subset the scenario based on widget
    df_data_long <- df_data_long[df_data_long$scenario %in% input$scenario, ]
    
    # Subset the variable based on widget
    df_plot <- df_data_long[df_data_long$variable == input$variable, ]
    
    # define plot label
    label <- input$variable
    label <- factor(label, levels = c("precip", "temp"), labels = c("Precipitation [mm]", "Temperature [°C]"))
    
    # generate the plot
    ggplot(df_plot, aes(x = month,  y = value, fill = scenario)) +
      geom_bar(stat = "identity", position = "dodge") + 
      scale_fill_aaas(name = "Scenario") +
      scale_x_continuous(expand = c(0,0), breaks = c(2, 6, 10), labels = c("Feb", "Jun", "Oct")) + 
      labs(x = "Month", y = label) + 
      theme_bw()
    
  })
  
  ## Plot 2 - Monthly runoff
  output$runoff_plot <-  renderPlot({
    
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    # Filter subbasin 
    df_data_long <- df_data_long %>% filter(subbasin == rv())
    
    # Subset the scenario based on widget
    df_data_long <- df_data_long[df_data_long$scenario %in% input$scenario, ]
    
    # Subset the variable based on widget
    df_plot <- df_data_long[df_data_long$variable == "runoff", ]
    
    
    # generate the plot
    ggplot(df_plot, aes(x = month,  y = value, color = scenario)) +
      geom_line() + 
      scale_color_aaas(name = "Scenario") +
      scale_x_continuous(expand = c(0,0), breaks = c(2, 6, 10), labels = c("Feb", "Jun", "Oct")) + 
      labs(x = "Month", y = "Runoff [m³/s]") + 
      theme_bw()
    
  })
  
  
  ## Plot 3 - Hydrograph
  output$hydrograph_plot <-  renderPlot({
    
    # Dont do anything if no subbasin was selected
    if (is.null(rv())) return ("Please select a subbasin by clicking on the map")
    
    # Filter subbasin
    df_plot <- df_runoff %>% filter(subbasin == rv())
    
    # generate the plot
    ggplot(df_plot, aes(x = date,  y = runoff)) +
      geom_line(color = "#2171B5") + 
      scale_x_date(expand = c(0,0)) + 
      labs(x = "Date", y = "Discharge [m³/s]") + 
      theme_bw()
    
  })
  
  ## Plot 4 - Temperature
  output$temperature_plot <-  renderPlot({
    
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
