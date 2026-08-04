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
# 1. CONFIGURATION
# =====================================================

gs4_deauth()

sheet_url <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  "1fWM21UNofVthwWYZfPc-oE-Q8U5xDPMIr6IebxQ9WKg/",
  "edit?usp=sharing"
)

# Ten minutes in milliseconds
refresh_interval_ms <- 600000


# =====================================================
# 2. DATA HELPER FUNCTIONS
# =====================================================

# Supports:
#   RG1_pH
#   RG1_Soil Moisture
#   RG1_Soil Moisture_15 cm
#
# Results:
#   module              = RG1
#   measurement_group   = Soil Moisture
#   measurement_layer   = 15 cm
parse_measurement_names <- function(measurement_names) {
  name_parts <- str_split_fixed(
    string = measurement_names,
    pattern = "_",
    n = 3
  )
  
  tibble(
    module = na_if(str_trim(name_parts[, 1]), ""),
    measurement_group = na_if(str_trim(name_parts[, 2]), ""),
    measurement_layer = na_if(str_trim(name_parts[, 3]), "")
  )
}


read_metadata_sheet <- function(sheet_name) {
  read_sheet(
    ss = sheet_url,
    sheet = sheet_name
  ) %>%
    clean_names()
}


read_measurement_sheet <- function(sheet_name) {
  tryCatch(
    {
      read_sheet(
        ss = sheet_url,
        sheet = sheet_name
      ) %>%
        clean_names() %>%
        mutate(
          measurement_name = sheet_name
        )
    },
    error = function(error) {
      warning(
        sprintf(
          "Could not read measurement sheet '%s': %s",
          sheet_name,
          conditionMessage(error)
        )
      )
      
      # Returning an empty tibble allows the remaining sheets to load.
      tibble()
    }
  )
}


load_google_sheet_data <- function() {
  
  # ---------------------------------------------------
  # Sensors metadata
  # ---------------------------------------------------
  
  sensors <- read_metadata_sheet("sensors") %>%
    transmute(
      dev_eui = str_trim(as.character(dev_eui)),
      measurement_id = str_trim(as.character(measurement_id)),
      measurement_name = str_trim(as.character(measurement_name)),
      units = as.character(units)
    ) %>%
    filter(
      !is.na(measurement_name),
      measurement_name != ""
    ) %>%
    distinct()
  
  
  # ---------------------------------------------------
  # Modules metadata
  # ---------------------------------------------------
  
  modules <- read_metadata_sheet("modules") %>%
    transmute(
      module = str_trim(as.character(module)),
      module_name = str_trim(as.character(module_name)),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    ) %>%
    filter(
      !is.na(module),
      module != ""
    ) %>%
    distinct(module, .keep_all = TRUE)
  
  
  # ---------------------------------------------------
  # Individual measurement sheets
  # ---------------------------------------------------
  
  measurement_sheet_names <- sensors %>%
    distinct(measurement_name) %>%
    arrange(measurement_name) %>%
    pull(measurement_name)
  
  measurements_raw <- map_dfr(
    measurement_sheet_names,
    read_measurement_sheet
  )
  
  if (nrow(measurements_raw) == 0) {
    return(
      list(
        df = tibble(),
        sensors = sensors,
        modules = modules
      )
    )
  }
  
  
  # ---------------------------------------------------
  # Clean measurements and add metadata
  # ---------------------------------------------------
  
  measurements <- measurements_raw %>%
    mutate(
      timestamp = ymd_hms(
        timestamp,
        quiet = TRUE,
        tz = "UTC"
      ),
      dev_eui = str_trim(as.character(dev_eui)),
      measurement_id = str_trim(as.character(measurement_id)),
      measurement_value = as.numeric(measurement_value),
      measurement_name = str_trim(as.character(measurement_name))
    ) %>%
    filter(!is.na(timestamp)) %>%
    left_join(
      sensors,
      by = c(
        "dev_eui",
        "measurement_id",
        "measurement_name"
      ),
      suffix = c("", "_metadata")
    )
  
  name_parts <- parse_measurement_names(
    measurements$measurement_name
  )
  
  df <- bind_cols(
    measurements,
    name_parts
  ) %>%
    left_join(
      modules,
      by = "module"
    ) %>%
    mutate(
      # When there is a third name component, use it to
      # identify the line within the same measurement graph.
      sensor_label = case_when(
        !is.na(measurement_layer) ~ measurement_layer,
        !is.na(module_name) ~ module_name,
        TRUE ~ module
      ),
      
      # Unique name for each plotted line.
      series_name = case_when(
        !is.na(measurement_layer) &
          !is.na(module_name) ~ paste(
            module_name,
            measurement_layer,
            sep = " - "
          ),
        
        !is.na(measurement_layer) ~ paste(
          module,
          measurement_layer,
          sep = " - "
        ),
        
        !is.na(module_name) ~ module_name,
        
        TRUE ~ module
      )
    ) %>%
    arrange(timestamp)
  
  list(
    df = df,
    sensors = sensors,
    modules = modules
  )
}


# =====================================================
# 3. USER INTERFACE
# =====================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Rain Garden LoRaWAN Dashboard"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("gauge-high")
      ),
      menuItem(
        "Time Series",
        tabName = "timeseries",
        icon = icon("chart-line")
      ),
      menuItem(
        "Location Map",
        tabName = "map",
        icon = icon("map")
      ),
      menuItem(
        "Raw Data",
        tabName = "raw",
        icon = icon("table")
      )
    ),
    
    selectizeInput(
      inputId = "selected_modules",
      label = "Select module(s):",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Loading modules...",
        plugins = list("remove_button")
      )
    ),
    
    selectizeInput(
      inputId = "selected_measurements",
      label = "Select measurement(s):",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Loading measurements...",
        plugins = list("remove_button")
      )
    ),
    
    dateRangeInput(
      inputId = "date_range",
      label = "Select date range:",
      start = Sys.Date(),
      end = Sys.Date()
    ),
    
    actionButton(
      inputId = "reset_filters",
      label = "Reset Filters"
    )
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
          box(
            width = 12,
            title = "Latest Measurements",
            DTOutput("latest_table")
          )
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
          box(
            width = 12,
            title = "Monitoring Locations",
            leafletOutput(
              "location_map",
              height = 600
            )
          )
        )
      ),
      
      tabItem(
        tabName = "raw",
        
        fluidRow(
          box(
            width = 12,
            title = "Download Data",
            downloadButton(
              "download_data",
              "Export Filtered Data"
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Raw Data",
            DTOutput("data_table")
          )
        )
      )
    )
  )
)


# =====================================================
# 4. SERVER
# =====================================================

server <- function(input, output, session) {
  
  # ---------------------------------------------------
  # Reactive data storage
  # ---------------------------------------------------
  
  data_store <- reactiveVal(NULL)
  loading_data <- reactiveVal(TRUE)
  data_error <- reactiveVal(NULL)
  filters_initialized <- reactiveVal(FALSE)
  previous_max_date <- reactiveVal(NULL)
  
  
  # ---------------------------------------------------
  # Load Google Sheets into reactive storage
  # ---------------------------------------------------
  
  load_data_into_store <- function() {
    loading_data(TRUE)
    data_error(NULL)
    
    tryCatch(
      {
        new_data <- load_google_sheet_data()
        
        if (nrow(new_data$df) == 0) {
          stop("No valid measurement records were loaded.")
        }
        
        data_store(new_data)
      },
      error = function(error) {
        data_error(conditionMessage(error))
        
        warning(
          paste(
            "Google Sheets refresh failed:",
            conditionMessage(error)
          )
        )
      },
      finally = {
        loading_data(FALSE)
      }
    )
  }
  
  
  # Perform the first data load only after the UI has
  # connected and completed its first browser flush.
  session$onFlushed(
    function() {
      load_data_into_store()
    },
    once = TRUE
  )
  
  
  # Refresh after the app has started, then every 10 minutes.
  refresh_timer <- reactiveTimer(
    intervalMs = refresh_interval_ms,
    session = session
  )
  
  observeEvent(
    refresh_timer(),
    {
      load_data_into_store()
    },
    ignoreInit = TRUE
  )
  
  
  # ---------------------------------------------------
  # Reactive accessors
  # ---------------------------------------------------
  
  current_data <- reactive({
    req(data_store())
    data_store()
  })
  
  df_live <- reactive({
    current_data()$df
  })
  
  modules_live <- reactive({
    current_data()$modules
  })
  
  
  # ---------------------------------------------------
  # Update filters after each successful data load
  # ---------------------------------------------------
  
  observeEvent(
    data_store(),
    {
      current_df <- data_store()$df
      
      req(nrow(current_df) > 0)
      
      module_choices <- current_df %>%
        filter(
          !is.na(module_name),
          module_name != ""
        ) %>%
        distinct(module_name) %>%
        arrange(module_name) %>%
        pull(module_name)
      
      measurement_choices <- current_df %>%
        filter(
          !is.na(measurement_group),
          measurement_group != ""
        ) %>%
        distinct(measurement_group) %>%
        arrange(measurement_group) %>%
        pull(measurement_group)
      
      available_dates <- as.Date(current_df$timestamp)
      
      minimum_date <- min(
        available_dates,
        na.rm = TRUE
      )
      
      maximum_date <- max(
        available_dates,
        na.rm = TRUE
      )
      
      if (!filters_initialized()) {
        
        selected_modules <- module_choices
        selected_measurements <- measurement_choices
        
        selected_start_date <- minimum_date
        selected_end_date <- maximum_date
        
        filters_initialized(TRUE)
        
      } else {
        
        selected_modules <- intersect(
          isolate(input$selected_modules),
          module_choices
        )
        
        selected_measurements <- intersect(
          isolate(input$selected_measurements),
          measurement_choices
        )
        
        # If a previous selection no longer exists, select all.
        if (length(selected_modules) == 0) {
          selected_modules <- module_choices
        }
        
        if (length(selected_measurements) == 0) {
          selected_measurements <- measurement_choices
        }
        
        current_date_range <- isolate(input$date_range)
        old_maximum_date <- previous_max_date()
        
        selected_start_date <- current_date_range[1]
        selected_end_date <- current_date_range[2]
        
        # Extend the end date automatically only when the user
        # had previously selected the most recent available date.
        if (
          !is.null(old_maximum_date) &&
          !is.na(selected_end_date) &&
          selected_end_date >= old_maximum_date
        ) {
          selected_end_date <- maximum_date
        }
        
        selected_start_date <- max(
          selected_start_date,
          minimum_date,
          na.rm = TRUE
        )
        
        selected_end_date <- min(
          selected_end_date,
          maximum_date,
          na.rm = TRUE
        )
      }
      
      previous_max_date(maximum_date)
      
      updateSelectizeInput(
        session = session,
        inputId = "selected_modules",
        choices = module_choices,
        selected = selected_modules,
        server = TRUE
      )
      
      updateSelectizeInput(
        session = session,
        inputId = "selected_measurements",
        choices = measurement_choices,
        selected = selected_measurements,
        server = TRUE
      )
      
      updateDateRangeInput(
        session = session,
        inputId = "date_range",
        start = selected_start_date,
        end = selected_end_date,
        min = minimum_date,
        max = maximum_date
      )
    },
    ignoreInit = TRUE
  )
  
  
  # ---------------------------------------------------
  # Reset filters
  # ---------------------------------------------------
  
  observeEvent(
    input$reset_filters,
    {
      current_df <- df_live()
      
      req(nrow(current_df) > 0)
      
      module_choices <- current_df %>%
        filter(
          !is.na(module_name),
          module_name != ""
        ) %>%
        distinct(module_name) %>%
        arrange(module_name) %>%
        pull(module_name)
      
      measurement_choices <- current_df %>%
        filter(
          !is.na(measurement_group),
          measurement_group != ""
        ) %>%
        distinct(measurement_group) %>%
        arrange(measurement_group) %>%
        pull(measurement_group)
      
      available_dates <- as.Date(current_df$timestamp)
      
      updateSelectizeInput(
        session = session,
        inputId = "selected_modules",
        choices = module_choices,
        selected = module_choices,
        server = TRUE
      )
      
      updateSelectizeInput(
        session = session,
        inputId = "selected_measurements",
        choices = measurement_choices,
        selected = measurement_choices,
        server = TRUE
      )
      
      updateDateRangeInput(
        session = session,
        inputId = "date_range",
        start = min(available_dates, na.rm = TRUE),
        end = max(available_dates, na.rm = TRUE)
      )
    }
  )
  
  
  # ---------------------------------------------------
  # Filtered data
  # ---------------------------------------------------
  
  filtered_df <- reactive({
    current_df <- df_live()
    
    req(nrow(current_df) > 0)
    req(input$date_range)
    req(length(input$selected_modules) > 0)
    req(length(input$selected_measurements) > 0)
    
    current_df %>%
      filter(
        module_name %in% input$selected_modules,
        measurement_group %in% input$selected_measurements,
        as.Date(timestamp) >= input$date_range[1],
        as.Date(timestamp) <= input$date_range[2]
      )
  })
  
  
  # ---------------------------------------------------
  # Latest values
  # ---------------------------------------------------
  
  latest_values <- reactive({
    filtered_df() %>%
      group_by(
        measurement_group,
        measurement_name,
        module,
        module_name,
        measurement_layer,
        sensor_label,
        series_name,
        units
      ) %>%
      slice_max(
        order_by = timestamp,
        n = 1,
        with_ties = FALSE
      ) %>%
      ungroup() %>%
      select(
        timestamp,
        module_name,
        measurement_group,
        measurement_layer,
        sensor_label,
        series_name,
        measurement_name,
        measurement_value,
        units
      )
  })
  
  
  # ---------------------------------------------------
  # Empty/loading plot helper
  # ---------------------------------------------------
  
  empty_plot <- function(
    message = "No data available for selected filters"
  ) {
    plotly_empty() %>%
      layout(
        annotations = list(
          text = message,
          x = 0.5,
          y = 0.5,
          showarrow = FALSE,
          font = list(size = 15)
        )
      )
  }
  
  
  # ---------------------------------------------------
  # Dynamic time-series chart grid
  # ---------------------------------------------------
  
  output$measurement_plot_grid <- renderUI({
    
    if (is.null(data_store())) {
      loading_message <- if (loading_data()) {
        "Loading sensor data from Google Sheets..."
      } else if (!is.null(data_error())) {
        paste("Unable to load data:", data_error())
      } else {
        "Waiting for sensor data..."
      }
      
      return(
        box(
          width = 12,
          title = "Sensor Time Series",
          tags$div(
            style = paste(
              "padding: 40px;",
              "text-align: center;",
              "font-size: 16px;"
            ),
            loading_message
          )
        )
      )
    }
    
    plot_data <- filtered_df()
    
    if (nrow(plot_data) == 0) {
      return(
        box(
          width = 12,
          title = "Sensor Time Series",
          plotlyOutput(
            "empty_timeseries_plot",
            height = 400
          )
        )
      )
    }
    
    measurements <- plot_data %>%
      distinct(measurement_group) %>%
      arrange(measurement_group) %>%
      pull(measurement_group)
    
    plot_boxes <- lapply(
      measurements,
      function(measurement) {
        plot_id <- paste0(
          "plot_",
          make.names(measurement)
        )
        
        box(
          width = 6,
          title = measurement,
          plotlyOutput(
            plot_id,
            height = 350
          )
        )
      }
    )
    
    do.call(tagList, plot_boxes)
  })
  
  
  output$empty_timeseries_plot <- renderPlotly({
    empty_plot()
  })
  
  
  observe({
    req(data_store())
    
    plot_data_all <- filtered_df()
    
    measurements <- plot_data_all %>%
      distinct(measurement_group) %>%
      pull(measurement_group)
    
    lapply(
      measurements,
      function(measurement) {
        
        local({
          current_measurement <- measurement
          
          plot_id <- paste0(
            "plot_",
            make.names(current_measurement)
          )
          
          output[[plot_id]] <- renderPlotly({
            
            plot_data <- filtered_df() %>%
              filter(
                measurement_group ==
                  current_measurement
              )
            
            if (nrow(plot_data) == 0) {
              return(empty_plot())
            }
            
            available_units <- plot_data %>%
              filter(
                !is.na(units),
                units != ""
              ) %>%
              distinct(units) %>%
              pull(units)
            
            unit_label <- if (
              length(available_units) > 0
            ) {
              available_units[1]
            } else {
              "Measurement value"
            }
            
            p <- ggplot(
              plot_data,
              aes(
                x = timestamp,
                y = measurement_value,
                colour = series_name,
                group = series_name,
                text = paste0(
                  "Time: ",
                  format(
                    timestamp,
                    "%Y-%m-%d %H:%M:%S"
                  ),
                  "<br>Module: ",
                  module_name,
                  "<br>Measurement: ",
                  measurement_group,
                  ifelse(
                    is.na(measurement_layer),
                    "",
                    paste0(
                      "<br>Layer: ",
                      measurement_layer
                    )
                  ),
                  "<br>Sensor: ",
                  sensor_label,
                  "<br>Value: ",
                  measurement_value,
                  " ",
                  units
                )
              )
            ) +
              geom_line(
                linewidth = 0.8,
                na.rm = TRUE
              ) +
              geom_point(
                size = 1.2,
                alpha = 0.7,
                na.rm = TRUE
              ) +
              labs(
                x = NULL,
                y = unit_label,
                colour = "Sensor"
              ) +
              theme_minimal()
            
            ggplotly(
              p,
              tooltip = "text"
            ) %>%
              layout(
                legend = list(
                  orientation = "h",
                  x = 0,
                  y = -0.2
                ),
                margin = list(
                  l = 60,
                  r = 20,
                  t = 20,
                  b = 80
                )
              )
          })
        })
      }
    )
  })
  
  
  # ---------------------------------------------------
  # Overview table
  # ---------------------------------------------------
  
  output$latest_table <- renderDT({
    datatable(
      latest_values(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  
  # ---------------------------------------------------
  # Overview value boxes
  # ---------------------------------------------------
  
  output$latest_ph <- renderValueBox({
    value <- latest_values() %>%
      filter(
        str_detect(
          measurement_group,
          regex(
            "^pH$",
            ignore_case = TRUE
          )
        )
      ) %>%
      slice_max(
        timestamp,
        n = 1,
        with_ties = FALSE
      )
    
    if (nrow(value) == 0) {
      return(
        valueBox(
          "No data",
          "Latest pH",
          icon = icon("flask"),
          color = "yellow"
        )
      )
    }
    
    valueBox(
      paste0(
        round(value$measurement_value[1], 2),
        " ",
        value$units[1]
      ),
      "Latest pH",
      icon = icon("flask"),
      color = "aqua"
    )
  })
  
  
  output$latest_soil_moisture <- renderValueBox({
    value <- latest_values() %>%
      filter(
        str_detect(
          measurement_group,
          regex(
            "^Soil Moisture$",
            ignore_case = TRUE
          )
        )
      ) %>%
      slice_max(
        timestamp,
        n = 1,
        with_ties = FALSE
      )
    
    if (nrow(value) == 0) {
      return(
        valueBox(
          "No data",
          "Latest Soil Moisture",
          icon = icon("seedling"),
          color = "yellow"
        )
      )
    }
    
    valueBox(
      paste0(
        round(value$measurement_value[1], 1),
        " ",
        value$units[1]
      ),
      "Latest Soil Moisture",
      icon = icon("seedling"),
      color = "green"
    )
  })
  
  
  output$latest_soil_temp <- renderValueBox({
    value <- latest_values() %>%
      filter(
        str_detect(
          measurement_group,
          regex(
            "^Soil Temperature$",
            ignore_case = TRUE
          )
        )
      ) %>%
      slice_max(
        timestamp,
        n = 1,
        with_ties = FALSE
      )
    
    if (nrow(value) == 0) {
      return(
        valueBox(
          "No data",
          "Latest Soil Temperature",
          icon = icon("temperature-half"),
          color = "yellow"
        )
      )
    }
    
    valueBox(
      paste0(
        round(value$measurement_value[1], 1),
        " ",
        value$units[1]
      ),
      "Latest Soil Temperature",
      icon = icon("temperature-half"),
      color = "red"
    )
  })
  
  
  # ---------------------------------------------------
  # Location map
  # ---------------------------------------------------
  
  output$location_map <- renderLeaflet({
    req(data_store())
    
    sites <- modules_live() %>%
      filter(
        module_name %in% input$selected_modules,
        !is.na(longitude),
        !is.na(latitude)
      )
    
    if (nrow(sites) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(
            providers$CartoDB.Positron
          )
      )
    }
    
    map <- leaflet(sites) %>%
      addProviderTiles(
        providers$CartoDB.Positron
      ) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        label = ~module_name,
        popup = ~paste0(
          "<b>",
          module_name,
          "</b><br>",
          "Module: ",
          module,
          "<br>",
          "Latitude: ",
          round(latitude, 6),
          "<br>",
          "Longitude: ",
          round(longitude, 6)
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
          zoom = 16
        )
    } else {
      map %>%
        fitBounds(
          lng1 = min(
            sites$longitude,
            na.rm = TRUE
          ),
          lat1 = min(
            sites$latitude,
            na.rm = TRUE
          ),
          lng2 = max(
            sites$longitude,
            na.rm = TRUE
          ),
          lat2 = max(
            sites$latitude,
            na.rm = TRUE
          )
        )
    }
  })
  
  
  # ---------------------------------------------------
  # Raw data table
  # ---------------------------------------------------
  
  output$data_table <- renderDT({
    datatable(
      filtered_df(),
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  
  # ---------------------------------------------------
  # Download filtered data
  # ---------------------------------------------------
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        "rain_garden_sensor_data_",
        Sys.Date(),
        ".csv"
      )
    },
    content = function(file) {
      write_csv(
        filtered_df(),
        file
      )
    }
  )
}


# =====================================================
# 5. RUN APP
# =====================================================

shinyApp(
  ui = ui,
  server = server
)
