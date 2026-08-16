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

# Refresh Google Sheets every 10 minutes.
refresh_interval_ms <- 600000

# Monitoring-page organization.
# To move/add a variable later, edit only this list.
monitoring_groups <- list(
  soil = c(
    "Soil Moisture",
    "Soil Temperature",
    "Electrical Conductivity",
    "pH"
  ),
  hydrology = c(
    "Rainfall Hourly",
    "Outflow A",
    "Outflow A (Total)"
  ),
  weather = c(
    "Air Temperature",
    "Air Humidity",
    "Light Intensity",
    "UV Index",
    "Barometric Pressure",
    "Wind Speed",
    "Wind Direction"
  )
)

monitoring_titles <- c(
  soil = "Soil Conditions",
  hydrology = "Hydrology",
  weather = "Weather"
)

monitoring_plot_order <- list(
  hydrology = c(
    "Rainfall Hourly",
    "Outflow A",
    "Outflow A (Total)"
  )
)

# TEMPORARY historical repair.
# TRUE converts historical blank Wind Speed values created by the old
# Apps Script (measurementValue || "") to 0 m/s.
# After historical Wind Speed blanks are permanently filled with 0 in
# Google Sheets, change this to FALSE so future missing values remain NA.
repair_historical_wind_speed_blanks <- TRUE


# =====================================================
# 2. DATA HELPER FUNCTIONS
# =====================================================

# Supports:
#   RG1_pH
#   RG1_Soil Moisture
#   RG1_Soil Moisture_15 cm
#
# Returns:
#   module              = RG1
#   measurement_group   = Soil Moisture
#   measurement_layer   = 15 cm
parse_measurement_names <- function(measurement_names) {
  name_parts <- str_split_fixed(
    measurement_names,
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
        mutate(measurement_name = sheet_name)
    },
    error = function(error) {
      warning(
        sprintf(
          "Could not read measurement sheet '%s': %s",
          sheet_name,
          conditionMessage(error)
        )
      )

      tibble()
    }
  )
}


load_google_sheet_data <- function() {

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

  modules <- read_metadata_sheet("modules") %>%
    transmute(
      module = str_trim(as.character(module)),
      module_name = str_trim(as.character(module_name)),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      elevation = as.numeric(elevation)
    ) %>%
    filter(
      !is.na(module),
      module != ""
    ) %>%
    distinct(module, .keep_all = TRUE)

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
      sensor_label = case_when(
        !is.na(measurement_layer) ~ measurement_layer,
        !is.na(module_name) ~ module_name,
        TRUE ~ module
      ),
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
# 3. PLOT HELPER FUNCTIONS
# =====================================================

make_timeseries_plot <- function(plot_data) {

  if (nrow(plot_data) == 0) {
    return(
      plotly_empty() %>%
        layout(
          annotations = list(
            text = "No data available for selected filters",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
    )
  }

  available_units <- plot_data %>%
    filter(
      !is.na(units),
      units != ""
    ) %>%
    distinct(units) %>%
    pull(units)

  unit_label <- if (length(available_units) > 0) {
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
        format(timestamp, "%Y-%m-%d %H:%M:%S"),
        "<br>Module: ", module_name,
        "<br>Measurement: ", measurement_group,
        ifelse(
          is.na(measurement_layer),
          "",
          paste0("<br>Layer: ", measurement_layer)
        ),
        "<br>Sensor: ", sensor_label,
        "<br>Value: ", measurement_value, " ", units
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
}


# =====================================================
# 4. WIND ROSE HELPER FUNCTIONS
# =====================================================

prepare_wind_data <- function(data) {

  wind_source <- data %>%
    filter(
      measurement_group %in% c(
        "Wind Speed",
        "Wind Direction"
      )
    ) %>%
    select(
      timestamp,
      module,
      module_name,
      measurement_group,
      measurement_value
    )

  if (repair_historical_wind_speed_blanks) {
    wind_source <- wind_source %>%
      mutate(
        measurement_value = if_else(
          measurement_group == "Wind Speed" &
            is.na(measurement_value),
          0,
          measurement_value
        )
      )
  }

  if (nrow(wind_source) == 0) {
    return(tibble())
  }

  wind_data <- wind_source %>%
    pivot_wider(
      id_cols = c(
        timestamp,
        module,
        module_name
      ),
      names_from = measurement_group,
      values_from = measurement_value,
      values_fn = mean
    )

  if (
    !"Wind Speed" %in% names(wind_data) ||
    !"Wind Direction" %in% names(wind_data)
  ) {
    return(tibble())
  }

  wind_data %>%
    rename(
      wind_speed = `Wind Speed`,
      wind_direction = `Wind Direction`
    ) %>%
    # Wind speed is required for all observations.
    # Wind direction may be NA for calm conditions and is only
    # required later for non-calm directional observations.
    filter(!is.na(wind_speed)) %>%
    mutate(
      wind_direction = wind_direction %% 360,
      is_calm = wind_speed <= 0,

      direction_bin = case_when(
        is.na(wind_direction) ~ NA_character_,
        wind_direction >= 348.75 | wind_direction < 11.25 ~ "N",
        wind_direction < 33.75 ~ "NNE",
        wind_direction < 56.25 ~ "NE",
        wind_direction < 78.75 ~ "ENE",
        wind_direction < 101.25 ~ "E",
        wind_direction < 123.75 ~ "ESE",
        wind_direction < 146.25 ~ "SE",
        wind_direction < 168.75 ~ "SSE",
        wind_direction < 191.25 ~ "S",
        wind_direction < 213.75 ~ "SSW",
        wind_direction < 236.25 ~ "SW",
        wind_direction < 258.75 ~ "WSW",
        wind_direction < 281.25 ~ "W",
        wind_direction < 303.75 ~ "WNW",
        wind_direction < 326.25 ~ "NW",
        TRUE ~ "NNW"
      ),

      direction_bin = factor(
        direction_bin,
        levels = c(
          "N", "NNE", "NE", "ENE",
          "E", "ESE", "SE", "SSE",
          "S", "SSW", "SW", "WSW",
          "W", "WNW", "NW", "NNW"
        )
      ),

      speed_bin = cut(
        wind_speed,
        breaks = c(
          0,
          1,
          2,
          3,
          5,
          8,
          Inf
        ),
        labels = c(
          ">0–1 m/s",
          "1–2 m/s",
          "2–3 m/s",
          "3–5 m/s",
          "5–8 m/s",
          ">8 m/s"
        ),
        include.lowest = FALSE,
        right = FALSE
      )
    )
}


make_wind_rose <- function(data) {

  if (nrow(data) == 0) {
    return(
      plotly_empty() %>%
        layout(
          annotations = list(
            text = "No paired wind data available",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
    )
  }

  total_observations <- nrow(data)
  calm_observations <- sum(
    data$is_calm,
    na.rm = TRUE
  )

  calm_percentage <- 100 *
    calm_observations /
    total_observations

  directional_data <- data %>%
    filter(
      !is_calm,
      !is.na(direction_bin),
      !is.na(speed_bin)
    )

  if (nrow(directional_data) == 0) {
    return(
      plotly_empty() %>%
        layout(
          annotations = list(
            list(
              text = paste0(
                "Calm: ",
                round(calm_percentage, 1),
                "%"
              ),
              x = 0.5,
              y = 0.55,
              showarrow = FALSE,
              font = list(size = 18)
            ),
            list(
              text = "No valid non-calm directional observations",
              x = 0.5,
              y = 0.45,
              showarrow = FALSE,
              font = list(size = 14)
            )
          )
        )
    )
  }

  rose_data <- directional_data %>%
    count(
      direction_bin,
      speed_bin,
      name = "observations"
    ) %>%
    complete(
      direction_bin,
      speed_bin,
      fill = list(observations = 0)
    ) %>%
    mutate(
      frequency = 100 *
        observations /
        nrow(directional_data)
    )

  speed_categories <- levels(
    directional_data$speed_bin
  )

  p <- plot_ly(
    type = "barpolar"
  )

  for (speed_category in speed_categories) {

    speed_data <- rose_data %>%
      filter(
        speed_bin == speed_category
      )

    p <- p %>%
      add_trace(
        r = speed_data$frequency,
        theta = as.character(
          speed_data$direction_bin
        ),
        name = speed_category,
        text = paste0(
          "Direction: ",
          speed_data$direction_bin,
          "<br>Wind speed: ",
          speed_category,
          "<br>Frequency among non-calm observations: ",
          round(speed_data$frequency, 1),
          "%",
          "<br>Observations: ",
          speed_data$observations
        ),
        hoverinfo = "text"
      )
  }

  p %>%
    layout(
      barmode = "stack",
      polar = list(
        angularaxis = list(
          direction = "clockwise",
          rotation = 90,
          categoryorder = "array",
          categoryarray = c(
            "N", "NNE", "NE", "ENE",
            "E", "ESE", "SE", "SSE",
            "S", "SSW", "SW", "WSW",
            "W", "WNW", "NW", "NNW"
          )
        ),
        radialaxis = list(
          title = "Frequency (%)",
          ticksuffix = "%"
        )
      ),
      annotations = list(
        list(
          text = paste0(
            "Calm: ",
            round(calm_percentage, 1),
            "%"
          ),
          x = 0.5,
          y = 1.08,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 14)
        )
      ),
      legend = list(
        title = list(
          text = "Wind speed"
        ),
        orientation = "h",
        x = 0,
        y = -0.15
      ),
      margin = list(
        l = 40,
        r = 40,
        t = 65,
        b = 90
      )
    )
}



# =====================================================
# 5. FAO-56 PENMAN-MONTEITH ET0 HELPERS
# =====================================================

# Saturation vapour pressure (kPa).
saturation_vapour_pressure <- function(temperature_c) {
  0.6108 * exp(
    (17.27 * temperature_c) /
      (temperature_c + 237.3)
  )
}


# Convert barometric pressure to kPa using the magnitude of
# the observed values. The current weather-station metadata
# records pressure in Pa, but this also tolerates hPa or kPa.
pressure_to_kpa <- function(pressure_value) {

  median_pressure <- median(
    pressure_value,
    na.rm = TRUE
  )

  if (!is.finite(median_pressure)) {
    return(pressure_value)
  }

  if (median_pressure > 2000) {
    pressure_value / 1000       # Pa -> kPa
  } else if (median_pressure > 200) {
    pressure_value / 10         # hPa -> kPa
  } else {
    pressure_value              # already kPa
  }
}


# Convert wind speed measured at height z to the FAO-56
# standard height of 2 m (FAO-56 Eq. 47).
wind_speed_to_2m <- function(wind_speed, measurement_height_m = 2) {

  if (
    is.na(measurement_height_m) ||
    measurement_height_m <= 0
  ) {
    return(wind_speed)
  }

  if (abs(measurement_height_m - 2) < 1e-9) {
    return(wind_speed)
  }

  wind_speed *
    4.87 /
    log(
      67.8 * measurement_height_m - 5.42
    )
}


# Extraterrestrial radiation Ra (MJ m-2 day-1) from
# latitude and day of year (FAO-56).
extraterrestrial_radiation <- function(
    latitude_deg,
    day_of_year
) {

  latitude_rad <- latitude_deg * pi / 180

  inverse_relative_distance <- 1 +
    0.033 *
    cos(
      2 * pi / 365 * day_of_year
    )

  solar_declination <- 0.409 *
    sin(
      2 * pi / 365 * day_of_year -
        1.39
    )

  sunset_hour_angle <- acos(
    pmin(
      1,
      pmax(
        -1,
        -tan(latitude_rad) *
          tan(solar_declination)
      )
    )
  )

  solar_constant <- 0.0820

  (
    24 * 60 / pi
  ) *
    solar_constant *
    inverse_relative_distance *
    (
      sunset_hour_angle *
        sin(latitude_rad) *
        sin(solar_declination) +
        cos(latitude_rad) *
        cos(solar_declination) *
        sin(sunset_hour_angle)
    )
}


# Prepare one daily meteorological record per weather station.
# Solar radiation is supplied separately by the dashboard slider.
prepare_daily_et_weather <- function(
    data,
    station_module,
    latitude,
    elevation,
    wind_height_m = 2
) {

  station_data <- data %>%
    filter(
      module == station_module,
      measurement_group %in% c(
        "Air Temperature",
        "Air Humidity",
        "Wind Speed",
        "Barometric Pressure"
      )
    ) %>%
    select(
      timestamp,
      measurement_group,
      measurement_value
    ) %>%
    mutate(
      date = as.Date(timestamp)
    )

  if (nrow(station_data) == 0) {
    return(tibble())
  }

  daily <- station_data %>%
    group_by(
      date,
      measurement_group
    ) %>%
    summarise(
      minimum = if (
        all(is.na(measurement_value))
      ) {
        NA_real_
      } else {
        min(
          measurement_value,
          na.rm = TRUE
        )
      },
      maximum = if (
        all(is.na(measurement_value))
      ) {
        NA_real_
      } else {
        max(
          measurement_value,
          na.rm = TRUE
        )
      },
      mean = if (
        all(is.na(measurement_value))
      ) {
        NA_real_
      } else {
        mean(
          measurement_value,
          na.rm = TRUE
        )
      },
      .groups = "drop"
    )

  daily_wide <- daily %>%
    pivot_wider(
      names_from = measurement_group,
      values_from = c(
        minimum,
        maximum,
        mean
      ),
      names_glue = "{.value}_{measurement_group}"
    )

  required_columns <- c(
    "minimum_Air Temperature",
    "maximum_Air Temperature",
    "mean_Air Temperature",
    "minimum_Air Humidity",
    "maximum_Air Humidity",
    "mean_Wind Speed",
    "mean_Barometric Pressure"
  )

  missing_columns <- setdiff(
    required_columns,
    names(daily_wide)
  )

  if (length(missing_columns) > 0) {
    return(tibble())
  }

  daily_wide %>%
    transmute(
      date = date,
      day_of_year = yday(date),

      t_min = .data[["minimum_Air Temperature"]],
      t_max = .data[["maximum_Air Temperature"]],
      t_mean = .data[["mean_Air Temperature"]],

      rh_min = .data[["minimum_Air Humidity"]],
      rh_max = .data[["maximum_Air Humidity"]],

      wind_speed_observed =
        .data[["mean_Wind Speed"]],

      pressure_kpa = pressure_to_kpa(
        .data[["mean_Barometric Pressure"]]
      ),

      wind_speed_2m = wind_speed_to_2m(
        wind_speed_observed,
        wind_height_m
      ),

      latitude = latitude,
      elevation = elevation
    ) %>%
    filter(
      !is.na(t_min),
      !is.na(t_max),
      !is.na(t_mean),
      !is.na(rh_min),
      !is.na(rh_max),
      !is.na(wind_speed_2m),
      !is.na(latitude),
      !is.na(elevation)
    )
}


# Daily FAO-56 reference evapotranspiration (ET0).
# solar_radiation_mj_m2_day is user-defined incoming solar
# radiation Rs and is applied as a scenario value to every
# day currently displayed.
calculate_fao56_et0 <- function(
    daily_weather,
    solar_radiation_mj_m2_day
) {

  if (nrow(daily_weather) == 0) {
    return(tibble())
  }

  sigma <- 4.903e-9
  albedo <- 0.23

  daily_weather %>%
    mutate(
      rs = solar_radiation_mj_m2_day,

      es_tmin = saturation_vapour_pressure(
        t_min
      ),
      es_tmax = saturation_vapour_pressure(
        t_max
      ),

      es = (
        es_tmin +
          es_tmax
      ) / 2,

      ea = (
        es_tmin *
          rh_max / 100 +
          es_tmax *
            rh_min / 100
      ) / 2,

      vapour_pressure_deficit =
        pmax(
          es - ea,
          0
        ),

      delta = 4098 *
        saturation_vapour_pressure(
          t_mean
        ) /
        (
          t_mean + 237.3
        )^2,

      pressure_kpa = if_else(
        is.na(pressure_kpa),
        101.3 *
          (
            (
              293 -
                0.0065 *
                  elevation
            ) /
              293
          )^5.26,
        pressure_kpa
      ),

      gamma =
        0.000665 *
          pressure_kpa,

      ra = extraterrestrial_radiation(
        latitude,
        day_of_year
      ),

      rso = (
        0.75 +
          2e-5 *
            elevation
      ) *
        ra,

      rs_rso_ratio = pmin(
        pmax(
          rs / rso,
          0
        ),
        1
      ),

      rns = (
        1 -
          albedo
      ) *
        rs,

      rnl = sigma *
        (
          (
            t_max + 273.16
          )^4 +
            (
              t_min + 273.16
            )^4
        ) /
        2 *
        (
          0.34 -
            0.14 *
              sqrt(
                pmax(
                  ea,
                  0
                )
              )
        ) *
        (
          1.35 *
            rs_rso_ratio -
            0.35
        ),

      rn = rns - rnl,

      # Daily soil heat flux G is taken as 0.
      et0_mm_day = (
        0.408 *
          delta *
          rn +
          gamma *
            (
              900 /
                (
                  t_mean +
                    273
                )
            ) *
            wind_speed_2m *
            vapour_pressure_deficit
      ) /
        (
          delta +
            gamma *
              (
                1 +
                  0.34 *
                    wind_speed_2m
              )
        ),

      # Negative daily ET0 values are not physically useful
      # for this reference-ET display.
      et0_mm_day = pmax(
        et0_mm_day,
        0
      )
    )
}


make_et0_plot <- function(et_data) {

  if (nrow(et_data) == 0) {
    return(
      plotly_empty() %>%
        layout(
          annotations = list(
            text = "Insufficient weather data for evapotranspiration calculation",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
          )
        )
    )
  }

  plot_data <- et_data %>%
    select(
      date,
      et0_mm_day,
      etc_mm_day,
      crop_coefficient,
      rs,
      t_mean,
      wind_speed_2m
    ) %>%
    pivot_longer(
      cols = c(
        et0_mm_day,
        etc_mm_day
      ),
      names_to = "et_type",
      values_to = "et_mm_day"
    ) %>%
    mutate(
      et_type = recode(
        et_type,
        et0_mm_day = "Reference ET₀",
        etc_mm_day = "Vegetation-adjusted ETc"
      )
    )

  p <- ggplot(
    plot_data,
    aes(
      x = date,
      y = et_mm_day,
      colour = et_type,
      group = et_type,
      text = paste0(
        "Date: ", date,
        "<br>Type: ", et_type,
        "<br>ET: ",
        round(et_mm_day, 2),
        " mm/day",
        "<br>Kc: ",
        round(crop_coefficient, 2),
        "<br>Solar radiation scenario: ",
        round(rs, 1),
        " MJ/m²/day",
        "<br>Mean temperature: ",
        round(t_mean, 1),
        " °C",
        "<br>Mean wind speed at 2 m: ",
        round(wind_speed_2m, 2),
        " m/s"
      )
    )
  ) +
    geom_line(
      linewidth = 0.9
    ) +
    geom_point(
      size = 2
    ) +
    labs(
      x = NULL,
      y = "Evapotranspiration (mm/day)",
      colour = NULL
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
        y = -0.15
      ),
      margin = list(
        l = 70,
        r = 20,
        t = 20,
        b = 80
      )
    )
}


# =====================================================
# 6. USER INTERFACE
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
        "Monitoring",
        icon = icon("chart-line"),
        startExpanded = TRUE,

        menuSubItem(
          "Soil Conditions",
          tabName = "soil",
          icon = icon("seedling")
        ),

        menuSubItem(
          "Hydrology",
          tabName = "hydrology",
          icon = icon("water")
        ),

        menuSubItem(
          "Weather",
          tabName = "weather",
          icon = icon("cloud-sun")
        )
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
        tabName = "soil",
        fluidRow(
          uiOutput("soil_plot_grid")
        )
      ),

      tabItem(
        tabName = "hydrology",

        fluidRow(
          box(
            width = 12,
            title = "FAO-56 Penman–Monteith Reference Evapotranspiration",

            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "et_station",
                  label = "Weather station:",
                  choices = NULL
                )
              ),

              column(
                width = 4,
                numericInput(
                  inputId = "et_wind_height",
                  label = "Wind measurement height (m):",
                  value = 2,
                  min = 0.5,
                  max = 10,
                  step = 0.1
                )
              ),

              column(
                width = 4,
                sliderInput(
                  inputId = "solar_radiation",
                  label = "Assumed daily solar radiation (MJ/m²/day):",
                  min = 0,
                  max = 35,
                  value = 10,
                  step = 0.5
                )
              )
            ),

            fluidRow(
              column(
                width = 4,
                sliderInput(
                  inputId = "crop_coefficient",
                  label = "Vegetation coefficient (Kc):",
                  min = 0.1,
                  max = 1.5,
                  value = 1.0,
                  step = 0.05
                )
              ),

              column(
                width = 8,
                tags$p(
                  style = "margin-top: 28px;",
                  paste(
                    "Kc adjusts reference evapotranspiration (ET₀)",
                    "to a vegetation-specific estimate (ETc = Kc × ET₀)."
                  )
                )
              )
            ),

            tags$p(
              style = "margin-top: 10px;",
              tags$strong("Scenario model: "),
              paste(
                "ET₀ is calculated from observed temperature,",
                "humidity, wind speed and barometric pressure,",
                "with the selected solar-radiation value applied",
                "to every displayed day. ETc then applies the",
                "user-selected vegetation coefficient Kc."
              )
            )
          )
        ),

        fluidRow(
          valueBoxOutput(
            "latest_et0",
            width = 6
          ),
          valueBoxOutput(
            "latest_etc",
            width = 6
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Reference and Vegetation-Adjusted Evapotranspiration",
            plotlyOutput(
              "et0_plot",
              height = 420
            )
          )
        ),

        fluidRow(
          uiOutput("hydrology_plot_grid")
        )
      ),

      tabItem(
        tabName = "weather",

        fluidRow(
          uiOutput("weather_plot_grid")
        ),

        fluidRow(
          uiOutput("wind_rose_grid")
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
# 7. SERVER
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
  # Data loading
  # ---------------------------------------------------

  load_data_into_store <- function() {
    loading_data(TRUE)
    data_error(NULL)

    tryCatch(
      {
        new_data <- load_google_sheet_data()

        if (nrow(new_data$df) == 0) {
          stop(
            "No valid measurement records were loaded."
          )
        }

        data_store(new_data)
      },
      error = function(error) {
        data_error(
          conditionMessage(error)
        )

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


  session$onFlushed(
    function() {
      load_data_into_store()
    },
    once = TRUE
  )


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
  # Filter helpers
  # ---------------------------------------------------

  get_filter_options <- function(current_df) {

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

    available_dates <- as.Date(
      current_df$timestamp
    )

    list(
      modules = module_choices,
      measurements = measurement_choices,
      min_date = min(
        available_dates,
        na.rm = TRUE
      ),
      max_date = max(
        available_dates,
        na.rm = TRUE
      )
    )
  }


  # ---------------------------------------------------
  # Update filters after each successful load
  # ---------------------------------------------------

  observeEvent(
    data_store(),
    {
      current_df <- data_store()$df

      req(nrow(current_df) > 0)

      options <- get_filter_options(
        current_df
      )

      if (!filters_initialized()) {

        selected_modules <- options$modules
        selected_measurements <- options$measurements
        selected_start_date <- options$min_date
        selected_end_date <- options$max_date

        filters_initialized(TRUE)

      } else {

        selected_modules <- intersect(
          isolate(
            input$selected_modules
          ),
          options$modules
        )

        selected_measurements <- intersect(
          isolate(
            input$selected_measurements
          ),
          options$measurements
        )

        if (length(selected_modules) == 0) {
          selected_modules <- options$modules
        }

        if (length(selected_measurements) == 0) {
          selected_measurements <-
            options$measurements
        }

        current_date_range <- isolate(
          input$date_range
        )

        old_maximum_date <- previous_max_date()

        selected_start_date <-
          current_date_range[1]

        selected_end_date <-
          current_date_range[2]

        if (
          !is.null(old_maximum_date) &&
          !is.na(selected_end_date) &&
          selected_end_date >= old_maximum_date
        ) {
          selected_end_date <-
            options$max_date
        }

        selected_start_date <- max(
          selected_start_date,
          options$min_date,
          na.rm = TRUE
        )

        selected_end_date <- min(
          selected_end_date,
          options$max_date,
          na.rm = TRUE
        )
      }

      previous_max_date(
        options$max_date
      )

      updateSelectizeInput(
        session = session,
        inputId = "selected_modules",
        choices = options$modules,
        selected = selected_modules,
        server = TRUE
      )

      updateSelectizeInput(
        session = session,
        inputId = "selected_measurements",
        choices = options$measurements,
        selected = selected_measurements,
        server = TRUE
      )

      updateDateRangeInput(
        session = session,
        inputId = "date_range",
        start = selected_start_date,
        end = selected_end_date,
        min = options$min_date,
        max = options$max_date
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

      options <- get_filter_options(
        current_df
      )

      updateSelectizeInput(
        session = session,
        inputId = "selected_modules",
        choices = options$modules,
        selected = options$modules,
        server = TRUE
      )

      updateSelectizeInput(
        session = session,
        inputId = "selected_measurements",
        choices = options$measurements,
        selected = options$measurements,
        server = TRUE
      )

      updateDateRangeInput(
        session = session,
        inputId = "date_range",
        start = options$min_date,
        end = options$max_date
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
        module_name %in%
          input$selected_modules,
        measurement_group %in%
          input$selected_measurements,
        as.Date(timestamp) >=
          input$date_range[1],
        as.Date(timestamp) <=
          input$date_range[2]
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
  # FAO-56 Penman-Monteith ET0
  # ---------------------------------------------------

  # Populate the ET weather-station selector using modules
  # that contain the meteorological variables required by
  # the model.
  observeEvent(
    data_store(),
    {

      current_df <- data_store()$df

      required_et_variables <- c(
        "Air Temperature",
        "Air Humidity",
        "Wind Speed",
        "Barometric Pressure"
      )

      et_stations <- current_df %>%
        filter(
          measurement_group %in%
            required_et_variables
        ) %>%
        distinct(
          module,
          module_name,
          measurement_group
        ) %>%
        group_by(
          module,
          module_name
        ) %>%
        summarise(
          n_required = n_distinct(
            measurement_group
          ),
          .groups = "drop"
        ) %>%
        filter(
          n_required ==
            length(
              required_et_variables
            )
        ) %>%
        arrange(module_name)

      if (nrow(et_stations) == 0) {
        return()
      }

      choices <- setNames(
        et_stations$module,
        et_stations$module_name
      )

      current_selection <- isolate(
        input$et_station
      )

      if (
        is.null(current_selection) ||
        !current_selection %in%
          et_stations$module
      ) {
        # WS1 is preferred when available because its
        # wind sensor is installed at the FAO standard 2 m.
        selected_station <- if (
          "WS1" %in% et_stations$module
        ) {
          "WS1"
        } else {
          et_stations$module[1]
        }
      } else {
        selected_station <-
          current_selection
      }

      updateSelectInput(
        session = session,
        inputId = "et_station",
        choices = choices,
        selected = selected_station
      )
    },
    ignoreInit = TRUE
  )


  et0_data <- reactive({

    req(data_store())
    req(input$et_station)
    req(input$date_range)
    req(input$solar_radiation)
    req(input$crop_coefficient)
    req(input$et_wind_height)

    module_row <- modules_live() %>%
      filter(
        module == input$et_station
      ) %>%
      slice_head(
        n = 1
      )

    req(nrow(module_row) == 1)
    req(!is.na(module_row$latitude))
    req(!is.na(module_row$elevation))

    station_weather <- df_live() %>%
      filter(
        module == input$et_station,
        as.Date(timestamp) >=
          input$date_range[1],
        as.Date(timestamp) <=
          input$date_range[2]
      )

    daily_weather <-
      prepare_daily_et_weather(
        data = station_weather,
        station_module =
          input$et_station,
        latitude =
          module_row$latitude[1],
        elevation =
          module_row$elevation[1],
        wind_height_m =
          input$et_wind_height
      )

    calculate_fao56_et0(
      daily_weather =
        daily_weather,
      solar_radiation_mj_m2_day =
        input$solar_radiation
    ) %>%
      mutate(
        crop_coefficient =
          input$crop_coefficient,
        etc_mm_day =
          crop_coefficient *
            et0_mm_day
      )
  })


  output$latest_et0 <-
    renderValueBox({

      et_data <- et0_data()

      if (nrow(et_data) == 0) {
        return(
          valueBox(
            "No data",
            "Latest ET₀",
            icon = icon("sun"),
            color = "yellow",
            width = 6
          )
        )
      }

      latest_row <- et_data %>%
        slice_max(
          date,
          n = 1,
          with_ties = FALSE
        )

      valueBox(
        paste0(
          round(
            latest_row$et0_mm_day[1],
            2
          ),
          " mm/day"
        ),
        paste0(
          "Latest ET₀ (",
          latest_row$date[1],
          ")"
        ),
        icon = icon("sun"),
        color = "yellow",
        width = 6
      )
    })


  output$latest_etc <-
    renderValueBox({

      et_data <- et0_data()

      if (nrow(et_data) == 0) {
        return(
          valueBox(
            "No data",
            "Latest vegetation-adjusted ET (ETc)",
            icon = icon("leaf"),
            color = "green",
            width = 6
          )
        )
      }

      latest_row <- et_data %>%
        slice_max(
          date,
          n = 1,
          with_ties = FALSE
        )

      valueBox(
        paste0(
          round(
            latest_row$etc_mm_day[1],
            2
          ),
          " mm/day"
        ),
        paste0(
          "Latest ETc (Kc = ",
          round(
            latest_row$crop_coefficient[1],
            2
          ),
          ")"
        ),
        icon = icon("leaf"),
        color = "green",
        width = 6
      )
    })


  output$et0_plot <-
    renderPlotly({

      make_et0_plot(
        et0_data()
      )
    })


  # ---------------------------------------------------
  # Generic monitoring-page helpers
  # ---------------------------------------------------

  monitoring_data <- function(group_key) {

    filtered_df() %>%
      filter(
        measurement_group %in%
          monitoring_groups[[group_key]]
      )
  }


  create_monitoring_grid <- function(group_key) {

    renderUI({

      if (is.null(data_store())) {

        loading_message <- if (loading_data()) {
          "Loading sensor data from Google Sheets..."
        } else if (!is.null(data_error())) {
          paste(
            "Unable to load data:",
            data_error()
          )
        } else {
          "Waiting for sensor data..."
        }

        return(
          box(
            width = 12,
            title = monitoring_titles[[group_key]],
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

      plot_data <- monitoring_data(
        group_key
      )

      # Wind variables are represented by wind roses,
      # not conventional time-series charts.
      measurements <- plot_data %>%
        filter(
          !measurement_group %in%
            c(
              "Wind Speed",
              "Wind Direction"
            )
        ) %>%
        distinct(measurement_group) %>%
        pull(measurement_group)
      
      if (group_key %in% names(monitoring_plot_order)) {
        
        preferred_order <-
          monitoring_plot_order[[group_key]]
        
        measurements <- c(
          intersect(
            preferred_order,
            measurements
          ),
          setdiff(
            measurements,
            preferred_order
          )
        )
        
      } else {
        
        measurements <- sort(
          measurements
        )
      }

      if (length(measurements) == 0) {

        # On the Weather page, wind-only data may still
        # produce a wind rose below, so do not show an
        # unnecessary empty-message box.
        if (
          group_key == "weather" &&
          nrow(plot_data) > 0
        ) {
          return(NULL)
        }

        return(
          box(
            width = 12,
            title = monitoring_titles[[group_key]],
            tags$div(
              style = paste(
                "padding: 40px;",
                "text-align: center;",
                "font-size: 16px;"
              ),
              "No measurements available for the selected filters."
            )
          )
        )
      }

      plot_boxes <- lapply(
        measurements,
        function(measurement) {

          plot_id <- paste0(
            group_key,
            "_plot_",
            make.names(measurement)
          )

          box(
            width = 12,
            title = measurement,
            plotlyOutput(
              plot_id,
              height = 350
            )
          )
        }
      )

      do.call(
        tagList,
        plot_boxes
      )
    })
  }


  output$soil_plot_grid <-
    create_monitoring_grid("soil")

  output$hydrology_plot_grid <-
    create_monitoring_grid("hydrology")

  output$weather_plot_grid <-
    create_monitoring_grid("weather")


  # Create all standard monitoring plots dynamically.
  observe({

    req(data_store())

    for (
      group_key in names(monitoring_groups)
    ) {

      group_data <- monitoring_data(
        group_key
      )

      measurements <- group_data %>%
        filter(
          !measurement_group %in%
            c(
              "Wind Speed",
              "Wind Direction"
            )
        ) %>%
        distinct(measurement_group) %>%
        pull(measurement_group)
      
      if (group_key %in% names(monitoring_plot_order)) {
        
        preferred_order <-
          monitoring_plot_order[[group_key]]
        
        measurements <- c(
          intersect(
            preferred_order,
            measurements
          ),
          setdiff(
            measurements,
            preferred_order
          )
        )
        
      } else {
        
        measurements <- sort(
          measurements
        )
      }

      if (length(measurements) == 0) {
        next
      }

      for (measurement in measurements) {

        local({

          current_group <- group_key
          current_measurement <- measurement

          plot_id <- paste0(
            current_group,
            "_plot_",
            make.names(
              current_measurement
            )
          )

          output[[plot_id]] <-
            renderPlotly({

              plot_data <- monitoring_data(
                current_group
              ) %>%
                filter(
                  measurement_group ==
                    current_measurement
                )

              make_timeseries_plot(
                plot_data
              )
            })
        })
      }
    }
  })


  # ---------------------------------------------------
  # Weather: wind roses
  # ---------------------------------------------------

  output$wind_rose_grid <- renderUI({

    req(data_store())

    wind_source <- monitoring_data(
      "weather"
    ) %>%
      filter(
        measurement_group %in%
          c(
            "Wind Speed",
            "Wind Direction"
          )
      )

    if (nrow(wind_source) == 0) {
      return(NULL)
    }

    wind_modules <- wind_source %>%
      distinct(
        module,
        module_name,
        measurement_group
      ) %>%
      group_by(
        module,
        module_name
      ) %>%
      summarise(
        has_speed = any(
          measurement_group == "Wind Speed"
        ),
        has_direction = any(
          measurement_group == "Wind Direction"
        ),
        .groups = "drop"
      ) %>%
      filter(
        has_speed,
        has_direction
      ) %>%
      arrange(module_name)

    if (nrow(wind_modules) == 0) {
      return(NULL)
    }

    wind_boxes <- lapply(
      seq_len(nrow(wind_modules)),
      function(i) {

        current_module <-
          wind_modules$module[i]

        current_name <-
          wind_modules$module_name[i]

        plot_id <- paste0(
          "wind_rose_",
          make.names(current_module)
        )

        box(
          width = 12,
          title = paste(
            "Wind Rose -",
            current_name
          ),
          plotlyOutput(
            plot_id,
            height = 470
          )
        )
      }
    )

    do.call(
      tagList,
      wind_boxes
    )
  })


  observe({

    req(data_store())

    wind_source <- monitoring_data(
      "weather"
    ) %>%
      filter(
        measurement_group %in%
          c(
            "Wind Speed",
            "Wind Direction"
          )
      )

    wind_modules <- wind_source %>%
      distinct(
        module,
        module_name,
        measurement_group
      ) %>%
      group_by(
        module,
        module_name
      ) %>%
      summarise(
        has_speed = any(
          measurement_group == "Wind Speed"
        ),
        has_direction = any(
          measurement_group == "Wind Direction"
        ),
        .groups = "drop"
      ) %>%
      filter(
        has_speed,
        has_direction
      )

    if (nrow(wind_modules) == 0) {
      return()
    }

    lapply(
      seq_len(nrow(wind_modules)),
      function(i) {

        local({

          current_module <-
            wind_modules$module[i]

          plot_id <- paste0(
            "wind_rose_",
            make.names(current_module)
          )

          output[[plot_id]] <-
            renderPlotly({

              module_data <- monitoring_data(
                "weather"
              ) %>%
                filter(
                  module == current_module,
                  measurement_group %in%
                    c(
                      "Wind Speed",
                      "Wind Direction"
                    )
                )

              paired_wind_data <-
                prepare_wind_data(
                  module_data
                )

              make_wind_rose(
                paired_wind_data
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
        round(
          value$measurement_value[1],
          2
        ),
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
        round(
          value$measurement_value[1],
          1
        ),
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
        round(
          value$measurement_value[1],
          1
        ),
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
        module_name %in%
          input$selected_modules,
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
  # Raw data
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
# 8. RUN APP
# =====================================================

shinyApp(
  ui = ui,
  server = server
)
