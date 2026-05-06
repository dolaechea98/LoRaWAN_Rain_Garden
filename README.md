# LoRaWAN Rain Garden Monitoring Dashboard

## Overview

This project presents an interactive Shiny dashboard for monitoring urban green infrastructure using a simulated city-scale LoRaWAN sensing network. The platform was developed as part of a dissertation project focused on real-time environmental monitoring, stormwater management, and flood-risk assessment in urban rain gardens and planters.

The dashboard integrates hydrological, meteorological, air quality, and water quality variables into a centralized monitoring platform with interactive visualization and spatial mapping capabilities.

The project is designed as a prototype for future integration with real-time low-power IoT sensor networks deployed across urban environments.

---

## Research Context

Urban green infrastructure such as rain gardens and bioretention systems can help mitigate stormwater runoff, reduce surface ponding, and improve urban resilience to extreme rainfall events.

This project explores:

- The use of LoRaWAN sensor networks for urban environmental monitoring
- Real-time visualization of environmental data

---

## Main Features

### Interactive Dashboard

The dashboard includes:

- Multi-location filtering
- Interactive time-series visualizations
- Spatial visualization of monitoring sites
- Downloadable filtered datasets
- Responsive Shiny interface

---

## Monitoring Sections

### Soil Moisture

- Soil moisture at:
  - 5 cm
  - 15 cm
  - 30 cm
  - 50 cm depth

### Hydrology

- Precipitation
- Precipitation intensity
- Surface water level

### Weather

- Temperature
- Humidity
- Pressure
- Solar radiation
- Wind speed
- Wind direction

### Air Quality

- CO2 concentration
- PM2.5
- PM10

### Water Quality

- Water temperature
- Electrical conductivity

### Spatial Monitoring

Interactive Leaflet map displaying selected monitoring locations.

---

## Potential Future Components

Planned future developments include:

- Real-time IoT sensor integration
- Automated threshold alerts
- Surface ponding risk estimation
- Flood probability prediction models
- Integration with SWMM or related urban hydrological modelling tools

---

## Technologies Used

### Programming Language

- R

### Main Libraries

- shiny
- tidyverse
- lubridate
- plotly
- leaflet
- DT
- shinydashboard

### Deployment

- Posit Connect Cloud

---

## Repository Structure

```text
.
├── app.R
├── lorawan_fake_dataset.csv
├── location_coordinates.csv
├── manifest.json
└── README.md
```

---

## Running the Application Locally

### Install Required Packages

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "lubridate",
  "plotly",
  "DT",
  "shinydashboard",
  "leaflet"
))
```

### Run the App

```r
shiny::runApp()
```

---

## Deployment

This application is configured for deployment through Posit Connect Cloud using:

```r
rsconnect::writeManifest()
```

---

## Author

David Alejandro Olaechea Dongo

MSc Data Science (Earth & Environment)  
Durham University

---

## License

This repository is intended for academic and research purposes.

---

## Status

In Progress
