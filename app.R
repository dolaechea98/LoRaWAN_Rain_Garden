library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(shinydashboard)
library(leaflet)

# =====================================================
# 1. LOAD DATA
# =====================================================
rsconnect::writeManifest()
df <- read_csv("lorawan_fake_dataset.csv", show_col_types = FALSE) %>%
  mutate(timestamp = ymd_hms(timestamp))

location_coords <- read_delim(
  "location_coordinates.csv",
  delim = ";",
  show_col_types = FALSE
)

if ("x" %in% names(location_coords)) {
  location_coords <- location_coords %>% rename(latitude = x)
}

if ("y" %in% names(location_coords)) {
  location_coords <- location_coords %>% rename(longitude = y)
}

if (!"location" %in% names(df)) {
  df <- df %>%
    mutate(location = rep(location_coords$location, length.out = n()))
}

all_locations <- sort(unique(df$location))

# =====================================================
# 2. UI
# =====================================================

ui <- dashboardPage(
  dashboardHeader(title = "LoRaWAN Green Infrastructure Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
      
      menuItem(
        "Monitoring", icon = icon("chart-line"),
        menuSubItem("Soil Moisture", tabName = "soil"),
        menuSubItem("Hydrology", tabName = "hydrology"),
        menuSubItem("Weather", tabName = "weather"),
        menuSubItem("Air Quality", tabName = "air"),
        menuSubItem("Water Quality", tabName = "water_quality")
      ),
      
      menuItem(
        "Spatial", icon = icon("map"),
        menuSubItem("Location Map", tabName = "location_map")
      ),
      
      menuItem(
        "Data", icon = icon("table"),
        menuSubItem("Raw Data", tabName = "raw")
      )
    ),
    
    selectizeInput(
      "selected_locations",
      "Select location(s):",
      choices = all_locations,
      selected = all_locations,
      multiple = TRUE,
      options = list(
        placeholder = "Choose one or more locations",
        plugins = list("remove_button")
      )
    ),
    
    dateRangeInput(
      "date_range",
      "Select date range:",
      start = min(as.Date(df$timestamp), na.rm = TRUE),
      end = max(as.Date(df$timestamp), na.rm = TRUE)
    ),
    
    actionButton("reset_filters", "Reset Filters")
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("mean_soil"),
          valueBoxOutput("mean_rain"),
          valueBoxOutput("mean_water")
        ),
        fluidRow(
          box(width = 12, title = "Precipitation", plotlyOutput("overview_precipitation_plot"))
        ),
        fluidRow(
          box(width = 12, title = "Soil Moisture at 5 cm", plotlyOutput("overview_soil_plot"))
        ),
        fluidRow(
          box(width = 12, title = "Surface Water Level", plotlyOutput("overview_water_plot"))
        )
      ),
      
      tabItem(
        tabName = "soil",
        fluidRow(
          box(width = 6, title = "Soil Moisture - 5 cm", plotlyOutput("soil_5cm_plot")),
          box(width = 6, title = "Soil Moisture - 15 cm", plotlyOutput("soil_15cm_plot"))
        ),
        fluidRow(
          box(width = 6, title = "Soil Moisture - 30 cm", plotlyOutput("soil_30cm_plot")),
          box(width = 6, title = "Soil Moisture - 50 cm", plotlyOutput("soil_50cm_plot"))
        )
      ),
      
      tabItem(
        tabName = "hydrology",
        fluidRow(
          box(width = 6, title = "Precipitation", plotlyOutput("precipitation_plot")),
          box(width = 6, title = "Precipitation Intensity", plotlyOutput("precip_intensity_plot"))
        ),
        fluidRow(
          box(width = 12, title = "Surface Water Level", plotlyOutput("surface_water_plot"))
        )
      ),
      
      tabItem(
        tabName = "weather",
        fluidRow(
          box(width = 6, title = "Temperature", plotlyOutput("temperature_plot")),
          box(width = 6, title = "Humidity", plotlyOutput("humidity_plot"))
        ),
        fluidRow(
          box(width = 6, title = "Pressure", plotlyOutput("pressure_plot")),
          box(width = 6, title = "Solar Radiation", plotlyOutput("solar_plot"))
        ),
        fluidRow(
          box(width = 6, title = "Wind Speed", plotlyOutput("wind_speed_plot")),
          box(width = 6, title = "Wind Direction", plotlyOutput("wind_direction_plot"))
        )
      ),
      
      tabItem(
        tabName = "air",
        fluidRow(
          box(width = 6, title = "CO2", plotlyOutput("co2_plot")),
          box(width = 6, title = "PM2.5", plotlyOutput("pm25_plot"))
        ),
        fluidRow(
          box(width = 12, title = "PM10", plotlyOutput("pm10_plot"))
        )
      ),
      
      tabItem(
        tabName = "water_quality",
        fluidRow(
          box(width = 6, title = "Water Temperature", plotlyOutput("water_temp_plot")),
          box(width = 6, title = "Conductivity", plotlyOutput("conductivity_plot"))
        )
      ),
      
      tabItem(
        tabName = "location_map",
        fluidRow(
          box(
            width = 12,
            title = "Selected Monitoring Locations",
            leafletOutput("location_map", height = 600)
          )
        )
      ),
      
      tabItem(
        tabName = "raw",
        fluidRow(
          box(
            width = 12,
            title = "Download Data",
            downloadButton("download_raw_data", "Export Data (CSV)")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Raw Sensor Dataset",
            DTOutput("data_table")
          )
        )
      )
    )
  )
)

# =====================================================
# 3. SERVER
# =====================================================

server <- function(input, output, session) {
  
  observeEvent(input$reset_filters, {
    updateSelectizeInput(
      session,
      "selected_locations",
      selected = all_locations
    )
    
    updateDateRangeInput(
      session,
      "date_range",
      start = min(as.Date(df$timestamp), na.rm = TRUE),
      end = max(as.Date(df$timestamp), na.rm = TRUE)
    )
  })
  
  filtered_df <- reactive({
    req(input$date_range)
    
    if (is.null(input$selected_locations) || length(input$selected_locations) == 0) {
      return(df[0, ])
    }
    
    df %>%
      filter(
        location %in% input$selected_locations,
        as.Date(timestamp) >= input$date_range[1],
        as.Date(timestamp) <= input$date_range[2]
      )
  })
  
  selected_location_coords <- reactive({
    if (is.null(input$selected_locations) || length(input$selected_locations) == 0) {
      return(location_coords[0, ])
    }
    
    location_coords %>%
      filter(location %in% input$selected_locations)
  })
  
  empty_plot <- function(message = "No data available for selected filters") {
    plotly_empty() %>%
      layout(
        annotations = list(
          text = message,
          x = 0.5,
          y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        )
      )
  }
  
  make_line_plot <- function(data, y_var, y_label) {
    if (nrow(data) == 0) return(empty_plot())
    
    p <- data %>%
      ggplot(aes(x = timestamp, y = .data[[y_var]], colour = location)) +
      geom_line() +
      labs(x = NULL, y = y_label, colour = "Location") +
      theme_minimal()
    
    ggplotly(p)
  }
  
  # =====================================================
  # LOCATION MAP
  # =====================================================
  
  output$location_map <- renderLeaflet({
    sites <- selected_location_coords()
    
    if (nrow(sites) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -3.1883, lat = 55.9533, zoom = 6)
      )
    }
    
    map <- leaflet(sites) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        label = ~location,
        popup = ~paste0(
          "<b>", location, "</b><br>",
          "Latitude: ", round(latitude, 5), "<br>",
          "Longitude: ", round(longitude, 5)
        ),
        radius = 8,
        fillOpacity = 0.8,
        stroke = TRUE
      )
    
    if (nrow(sites) == 1) {
      map %>%
        setView(
          lng = sites$longitude[1],
          lat = sites$latitude[1],
          zoom = 13
        )
    } else {
      map %>%
        fitBounds(
          lng1 = min(sites$longitude, na.rm = TRUE),
          lat1 = min(sites$latitude, na.rm = TRUE),
          lng2 = max(sites$longitude, na.rm = TRUE),
          lat2 = max(sites$latitude, na.rm = TRUE)
        )
    }
  })
  
  # =====================================================
  # VALUE BOXES
  # =====================================================
  
  output$mean_soil <- renderValueBox({
    if (nrow(filtered_df()) == 0) {
      return(valueBox("No data", "Mean Soil Moisture at 5 cm", icon = icon("seedling"), color = "yellow"))
    }
    
    valueBox(
      value = paste0(round(mean(filtered_df()$soil_moisture_5cm, na.rm = TRUE), 1), "%"),
      subtitle = "Mean Soil Moisture at 5 cm",
      icon = icon("seedling"),
      color = "green"
    )
  })
  
  output$mean_rain <- renderValueBox({
    if (nrow(filtered_df()) == 0) {
      return(valueBox("No data", "Mean Total Rainfall", icon = icon("cloud-rain"), color = "yellow"))
    }
    
    total_rain <- filtered_df() %>%
      group_by(location) %>%
      summarise(total = sum(precipitation, na.rm = TRUE), .groups = "drop") %>%
      summarise(mean_total = mean(total, na.rm = TRUE)) %>%
      pull(mean_total)
    
    valueBox(
      value = paste0(round(total_rain, 1), " mm"),
      subtitle = "Mean Total Rainfall per Selected Location",
      icon = icon("cloud-rain"),
      color = "blue"
    )
  })
  
  output$mean_water <- renderValueBox({
    if (nrow(filtered_df()) == 0) {
      return(valueBox("No data", "Mean Latest Surface Water Level", icon = icon("water"), color = "yellow"))
    }
    
    latest_water <- filtered_df() %>%
      group_by(location) %>%
      slice_max(order_by = timestamp, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      summarise(mean_latest = mean(surface_water_level, na.rm = TRUE)) %>%
      pull(mean_latest)
    
    valueBox(
      value = paste0(round(latest_water, 1), " cm"),
      subtitle = "Mean Latest Surface Water Level",
      icon = icon("water"),
      color = "aqua"
    )
  })
  
  # =====================================================
  # OVERVIEW
  # =====================================================
  
  output$overview_precipitation_plot <- renderPlotly({
    make_line_plot(filtered_df(), "precipitation", "Precipitation (mm / 5 min)")
  })
  
  output$overview_soil_plot <- renderPlotly({
    make_line_plot(filtered_df(), "soil_moisture_5cm", "Soil moisture (%)")
  })
  
  output$overview_water_plot <- renderPlotly({
    make_line_plot(filtered_df(), "surface_water_level", "Surface water level (cm)")
  })
  
  # =====================================================
  # SOIL MOISTURE
  # =====================================================
  
  output$soil_5cm_plot <- renderPlotly({
    make_line_plot(filtered_df(), "soil_moisture_5cm", "Soil moisture (%)")
  })
  
  output$soil_15cm_plot <- renderPlotly({
    make_line_plot(filtered_df(), "soil_moisture_15cm", "Soil moisture (%)")
  })
  
  output$soil_30cm_plot <- renderPlotly({
    make_line_plot(filtered_df(), "soil_moisture_30cm", "Soil moisture (%)")
  })
  
  output$soil_50cm_plot <- renderPlotly({
    make_line_plot(filtered_df(), "soil_moisture_50cm", "Soil moisture (%)")
  })
  
  # =====================================================
  # HYDROLOGY
  # =====================================================
  
  output$precipitation_plot <- renderPlotly({
    make_line_plot(filtered_df(), "precipitation", "Precipitation (mm / 5 min)")
  })
  
  output$precip_intensity_plot <- renderPlotly({
    make_line_plot(filtered_df(), "precip_intensity", "Precipitation intensity (mm/hr)")
  })
  
  output$surface_water_plot <- renderPlotly({
    make_line_plot(filtered_df(), "surface_water_level", "Surface water level (cm)")
  })
  
  # =====================================================
  # WEATHER
  # =====================================================
  
  output$temperature_plot <- renderPlotly({
    make_line_plot(filtered_df(), "temperature", "Temperature (°C)")
  })
  
  output$humidity_plot <- renderPlotly({
    make_line_plot(filtered_df(), "humidity", "Humidity (%)")
  })
  
  output$pressure_plot <- renderPlotly({
    make_line_plot(filtered_df(), "pressure", "Pressure (hPa)")
  })
  
  output$solar_plot <- renderPlotly({
    make_line_plot(filtered_df(), "solar_radiation", "Solar radiation (W/m²)")
  })
  
  output$wind_speed_plot <- renderPlotly({
    make_line_plot(filtered_df(), "wind_speed", "Wind speed (m/s)")
  })
  
  output$wind_direction_plot <- renderPlotly({
    make_line_plot(filtered_df(), "wind_direction", "Wind direction (degrees)")
  })
  
  # =====================================================
  # AIR QUALITY
  # =====================================================
  
  output$co2_plot <- renderPlotly({
    make_line_plot(filtered_df(), "co2", "CO2 (ppm)")
  })
  
  output$pm25_plot <- renderPlotly({
    make_line_plot(filtered_df(), "pm2_5", "PM2.5 (µg/m³)")
  })
  
  output$pm10_plot <- renderPlotly({
    make_line_plot(filtered_df(), "pm10", "PM10 (µg/m³)")
  })
  
  # =====================================================
  # WATER QUALITY
  # =====================================================
  
  output$water_temp_plot <- renderPlotly({
    make_line_plot(filtered_df(), "water_temp", "Water temperature (°C)")
  })
  
  output$conductivity_plot <- renderPlotly({
    make_line_plot(filtered_df(), "conductivity", "Conductivity (µS/cm)")
  })
  
  # =====================================================
  # RAW DATA
  # =====================================================
  
  output$data_table <- renderDT({
    datatable(
      filtered_df(),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$download_raw_data <- downloadHandler(
    filename = function() {
      paste0("lorawan_filtered_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_df(), file)
    }
  )
}

shinyApp(ui, server)