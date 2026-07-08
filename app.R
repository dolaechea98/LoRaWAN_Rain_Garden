library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(shinydashboard)
library(leaflet)
library(googlesheets4)
library(janitor)

# =====================================================
# 1. LOAD DATA FROM GOOGLE SHEETS
# =====================================================

gs4_deauth()

sheet_url <- "https://docs.google.com/spreadsheets/d/1fWM21UNofVthwWYZfPc-oE-Q8U5xDPMIr6IebxQ9WKg/edit?usp=sharing"
rsconnect::writeManifest()

sensors <- read_sheet(sheet_url, sheet = "sensors") %>%
  clean_names() %>%
  mutate(
    dev_eui = as.character(dev_eui),
    measurement_id = as.character(measurement_id),
    measurement_name = as.character(measurement_name),
    units = as.character(units)
  )

modules <- read_sheet(sheet_url, sheet = "modules") %>%
  clean_names() %>%
  mutate(
    module = as.character(module),
    module_name = as.character(module_name),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

read_measurement_sheet <- function(sheet_name) {
  read_sheet(sheet_url, sheet = sheet_name) %>%
    clean_names() %>%
    mutate(measurement_name = sheet_name)
}

df <- map_dfr(sensors$measurement_name, read_measurement_sheet) %>%
  clean_names() %>%
  mutate(
    timestamp = ymd_hms(timestamp, quiet = TRUE),
    dev_eui = as.character(dev_eui),
    measurement_id = as.character(measurement_id),
    measurement_value = as.numeric(measurement_value),
    measurement_name = as.character(measurement_name)
  ) %>%
  left_join(
    sensors,
    by = c("dev_eui", "measurement_id", "measurement_name")
  ) %>%
  mutate(
    module = str_extract(measurement_name, "^[A-Z]+[0-9]+"),
    measurement_group = str_remove(measurement_name, "^[A-Z]+[0-9]+_")
  ) %>%
  left_join(modules, by = "module") %>%
  filter(!is.na(timestamp))

all_modules <- sort(unique(df$module_name))
all_measurements <- sort(unique(df$measurement_group))

# =====================================================
# 2. UI
# =====================================================

ui <- dashboardPage(
  dashboardHeader(title = "Rain Garden LoRaWAN Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("Location Map", tabName = "map", icon = icon("map")),
      menuItem("Raw Data", tabName = "raw", icon = icon("table"))
    ),
    
    selectizeInput(
      "selected_modules",
      "Select module(s):",
      choices = all_modules,
      selected = all_modules,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    
    selectizeInput(
      "selected_measurements",
      "Select measurement(s):",
      choices = all_measurements,
      selected = all_measurements,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
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
          valueBoxOutput("latest_ph"),
          valueBoxOutput("latest_soil_moisture"),
          valueBoxOutput("latest_soil_temp")
        ),
        fluidRow(
          box(width = 12, title = "Latest Measurements", DTOutput("latest_table"))
        )
      ),
      
      tabItem(
        tabName = "timeseries",
        fluidRow(
          uiOutput("measurement_plot_grid")
        )
      ),
      
      tabItem(
        tabName = "map",
        fluidRow(
          box(width = 12, title = "Monitoring Locations", leafletOutput("location_map", height = 600))
        )
      ),
      
      tabItem(
        tabName = "raw",
        fluidRow(
          box(width = 12, title = "Download Data", downloadButton("download_data", "Export Filtered Data"))
        ),
        fluidRow(
          box(width = 12, title = "Raw Data", DTOutput("data_table"))
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
    updateSelectizeInput(session, "selected_modules", selected = all_modules)
    updateSelectizeInput(session, "selected_measurements", selected = all_measurements)
    updateDateRangeInput(
      session,
      "date_range",
      start = min(as.Date(df$timestamp), na.rm = TRUE),
      end = max(as.Date(df$timestamp), na.rm = TRUE)
    )
  })
  
  filtered_df <- reactive({
    req(input$date_range)
    
    df %>%
      filter(
        module_name %in% input$selected_modules,
        measurement_group %in% input$selected_measurements,
        as.Date(timestamp) >= input$date_range[1],
        as.Date(timestamp) <= input$date_range[2]
      )
  })
  
  latest_values <- reactive({
    filtered_df() %>%
      group_by(measurement_group, measurement_name, module_name, units) %>%
      slice_max(timestamp, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(timestamp, module_name, measurement_group, measurement_name, measurement_value, units)
  })
  
  empty_plot <- function(message = "No data available for selected filters") {
    plotly_empty() %>%
      layout(
        annotations = list(
          text = message,
          x = 0.5,
          y = 0.5,
          showarrow = FALSE
        )
      )
  }
  
  output$measurement_plot_grid <- renderUI({
    plot_data <- filtered_df()
    
    if (nrow(plot_data) == 0) {
      return(
        box(
          width = 12,
          title = "Sensor Time Series",
          plotlyOutput("empty_timeseries_plot", height = 400)
        )
      )
    }
    
    measurements <- plot_data %>%
      distinct(measurement_group) %>%
      arrange(measurement_group) %>%
      pull(measurement_group)
    
    plot_boxes <- lapply(measurements, function(meas) {
      plot_id <- paste0("plot_", make.names(meas))
      
      box(
        width = 6,
        title = meas,
        plotlyOutput(plot_id, height = 350)
      )
    })
    
    do.call(tagList, plot_boxes)
  })
  
  output$empty_timeseries_plot <- renderPlotly({
    empty_plot()
  })
  
  observe({
    plot_data_all <- filtered_df()
    
    measurements <- plot_data_all %>%
      distinct(measurement_group) %>%
      pull(measurement_group)
    
    lapply(measurements, function(meas) {
      plot_id <- paste0("plot_", make.names(meas))
      
      output[[plot_id]] <- renderPlotly({
        plot_data <- filtered_df() %>%
          filter(measurement_group == meas)
        
        if (nrow(plot_data) == 0) {
          return(empty_plot())
        }
        
        unit_label <- plot_data %>%
          filter(!is.na(units), units != "") %>%
          distinct(units) %>%
          pull(units) %>%
          first()
        
        if (is.na(unit_label) || is.null(unit_label)) {
          unit_label <- "Measurement value"
        }
        
        p <- ggplot(
          plot_data,
          aes(
            x = timestamp,
            y = measurement_value,
            colour = module_name,
            text = paste0(
              "Time: ", timestamp,
              "<br>Module: ", module_name,
              "<br>Measurement: ", measurement_name,
              "<br>Value: ", measurement_value, " ", units
            )
          )
        ) +
          geom_line(linewidth = 0.8) +
          geom_point(size = 1.2, alpha = 0.7) +
          labs(
            x = NULL,
            y = unit_label,
            colour = "Module"
          ) +
          theme_minimal()
        
        ggplotly(p, tooltip = "text") %>%
          layout(
            legend = list(
              orientation = "h",
              x = 0,
              y = -0.2
            ),
            margin = list(l = 60, r = 20, t = 20, b = 80)
          )
      })
    })
  })
  
  output$latest_table <- renderDT({
    datatable(
      latest_values(),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$latest_ph <- renderValueBox({
    value <- latest_values() %>%
      filter(str_detect(measurement_group, regex("pH", ignore_case = TRUE))) %>%
      slice_max(timestamp, n = 1, with_ties = FALSE)
    
    if (nrow(value) == 0) {
      return(valueBox("No data", "Latest pH", icon = icon("flask"), color = "yellow"))
    }
    
    valueBox(
      paste0(round(value$measurement_value[1], 2), " ", value$units[1]),
      "Latest pH",
      icon = icon("flask"),
      color = "aqua"
    )
  })
  
  output$latest_soil_moisture <- renderValueBox({
    value <- latest_values() %>%
      filter(str_detect(measurement_group, regex("Soil Moisture", ignore_case = TRUE))) %>%
      slice_max(timestamp, n = 1, with_ties = FALSE)
    
    if (nrow(value) == 0) {
      return(valueBox("No data", "Latest Soil Moisture", icon = icon("seedling"), color = "yellow"))
    }
    
    valueBox(
      paste0(round(value$measurement_value[1], 1), " ", value$units[1]),
      "Latest Soil Moisture",
      icon = icon("seedling"),
      color = "green"
    )
  })
  
  output$latest_soil_temp <- renderValueBox({
    value <- latest_values() %>%
      filter(str_detect(measurement_group, regex("Soil Temperature", ignore_case = TRUE))) %>%
      slice_max(timestamp, n = 1, with_ties = FALSE)
    
    if (nrow(value) == 0) {
      return(valueBox("No data", "Latest Soil Temperature", icon = icon("temperature-half"), color = "yellow"))
    }
    
    valueBox(
      paste0(round(value$measurement_value[1], 1), " ", value$units[1]),
      "Latest Soil Temperature",
      icon = icon("temperature-half"),
      color = "red"
    )
  })
  
  output$location_map <- renderLeaflet({
    sites <- modules %>%
      filter(module_name %in% input$selected_modules)
    
    if (nrow(sites) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron)
      )
    }
    
    leaflet(sites) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        label = ~module_name,
        popup = ~paste0(
          "<b>", module_name, "</b><br>",
          "Module: ", module, "<br>",
          "Latitude: ", latitude, "<br>",
          "Longitude: ", longitude
        ),
        radius = 8,
        fillOpacity = 0.8
      ) %>%
      fitBounds(
        lng1 = min(sites$longitude, na.rm = TRUE),
        lat1 = min(sites$latitude, na.rm = TRUE),
        lng2 = max(sites$longitude, na.rm = TRUE),
        lat2 = max(sites$latitude, na.rm = TRUE)
      )
  })
  
  output$data_table <- renderDT({
    datatable(
      filtered_df(),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("rain_garden_sensor_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_df(), file)
    }
  )
}

shinyApp(ui, server)
