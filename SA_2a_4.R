# SA_2a_4.R
# Plot objects for main VDS2526 report

# Load Packages
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(plotly)
library(leaflet)
library(htmltools)
library(tidyr)

# Load pollution data
fileList <- paste0("VDS2526_Madrid/madrid_", 2001:2018, ".csv")

all_data <- map_dfr(
  fileList,
  ~read_csv(.x, show_col_types = FALSE)
)

all_data <- all_data %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  )

# Load station metadata
stations <- read_csv(
  "VDS2526_Madrid/stations.csv",
  show_col_types = FALSE
)

# =========================
# Q2a data
# =========================

q2a_data <- all_data %>%
  filter(year == 2018) %>%
  group_by(station) %>%
  summarise(
    avg_NO2 = mean(NO_2, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_NO2))

# Color blind friendly colors
q2a_data <- q2a_data %>%
  mutate(
    pollution_level = case_when(
      avg_NO2 > 60 ~ ">60",
      avg_NO2 >= 45 ~ "45-60",
      avg_NO2 >= 30 ~ "30-45",
      avg_NO2 >= 15 ~ "15-30",
      TRUE ~ "<15"
    ),
    pollution_color = case_when(
      avg_NO2 > 60 ~ "#440154",
      avg_NO2 >= 45 ~ "#31688E",
      avg_NO2 >= 30 ~ "#35B779",
      avg_NO2 >= 15 ~ "#90d743",
      TRUE ~ "#FDE725"
    )
  )

# Order legend
q2a_data$pollution_level <- factor(
  q2a_data$pollution_level,
  levels = c(">60", "45-60", "30-45", "15-30", "<15")
)

# Join station locations
q2a_map_data <- q2a_data %>%
  left_join(
    stations,
    by = c("station" = "id")
  )

# =========================
# Q2a. 2018 Average NO2 Hotspot Map
# =========================

SA_q2a_hotspot <- leaflet(q2a_map_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = ~avg_NO2 / 3,
    color = ~pollution_color,
    fillColor = ~pollution_color,
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~paste(
      "<b>Station:</b>", name,
      "<br><b>Average NO2:</b>", round(avg_NO2, 2),
      "<br><b>Category:</b>", pollution_level
    )
  ) %>%
  addLegend(
    position = "topright",
    colors = c("#440154", "#31688E", "#35B779", "#90d743", "#FDE725"),
    labels = c(">60", "45-60", "30-45", "15-30", "<15"),
    title = "Avg. NO2 (µg/m3)",
    opacity = 1
  )

# =========================
# Q2a. 2018 Average NO2 Bar Chart
# =========================

SA_q2a_barchart <- plot_ly(
  data = q2a_data,
  x = ~avg_NO2,
  y = ~reorder(as.character(station), avg_NO2),
  type = "bar",
  orientation = "h",
  marker = list(
    color = q2a_data$pollution_color
  ),
  name = ~pollution_level,
  showlegend = TRUE,
  hoverinfo = "text",
  text = ~paste(
    "Station:", station,
    "<br>Average NO2:", round(avg_NO2, 2),
    "<br>Category:", pollution_level
  )
) %>%
  layout(
    title = "Average NO2 Levels by Station (2018)",
    xaxis = list(title = "Average NO2"),
    yaxis = list(title = "Station"),
    legend = list(
      title = list(text = "Avg. NO2 (µg/m3)")
    )
  )

# =========================
# Q4 data
# =========================

pollutants <- names(all_data)[
  !(names(all_data) %in% c("date", "year", "station"))
]

q4_data <- all_data %>%
  select(year, station, all_of(pollutants)) %>%
  pivot_longer(
    cols = all_of(pollutants),
    names_to = "pollutant",
    values_to = "value"
  ) %>%
  group_by(year, station, pollutant) %>%
  summarise(
    avg_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(avg_value)) %>%
  left_join(
    stations,
    by = c("station" = "id")
  ) %>%
  mutate(
    pollution_level = case_when(
      avg_value > 60 ~ ">60",
      avg_value >= 45 ~ "45-60",
      avg_value >= 30 ~ "30-45",
      avg_value >= 15 ~ "15-30",
      TRUE ~ "<15"
    ),
    pollution_level = factor(
      pollution_level,
      levels = c(">60", "45-60", "30-45", "15-30", "<15")
    )
  )

q4_data <- q4_data %>%
  mutate(
    pollution_color = case_when(
      pollution_level == ">60" ~ "#440154",
      pollution_level == "45-60" ~ "#31688E",
      pollution_level == "30-45" ~ "#35B779",
      pollution_level == "15-30" ~ "#90d743",
      pollution_level == "<15" ~ "#FDE725"
    )
  )

# =========================
# Q4. 2001-2018 Hotspot Map / Heatmap
# =========================

SA_q4_heatmap <- plot_ly() %>%
  add_trace(
    data = q4_data,
    type = "scattermapbox",
    mode = "markers",
    lon = ~lon,
    lat = ~lat,
    frame = ~year,
    marker = list(
      color = ~pollution_color,
      size = ~avg_value,
      opacity = 0.75
    ),
    text = ~paste(
      "Year:", year,
      "<br>Pollutant:", pollutant,
      "<br>Station:", name,
      "<br>Average:", round(avg_value, 2),
      "<br>Category:", pollution_level
    ),
    hoverinfo = "text",
    showlegend = FALSE,
    transforms = list(
      list(
        type = "filter",
        target = ~pollutant,
        operation = "=",
        value = "NO_2"
      )
    )
  ) %>%
  add_trace(
    type = "scattermapbox",
    mode = "markers",
    lon = NA,
    lat = NA,
    marker = list(color = "#440154", size = 12),
    name = ">60",
    showlegend = TRUE
  ) %>%
  add_trace(
    type = "scattermapbox",
    mode = "markers",
    lon = NA,
    lat = NA,
    marker = list(color = "#31688E", size = 12),
    name = "45-60",
    showlegend = TRUE
  ) %>%
  add_trace(
    type = "scattermapbox",
    mode = "markers",
    lon = NA,
    lat = NA,
    marker = list(color = "#35B779", size = 12),
    name = "30-45",
    showlegend = TRUE
  ) %>%
  add_trace(
    type = "scattermapbox",
    mode = "markers",
    lon = NA,
    lat = NA,
    marker = list(color = "#90d743", size = 12),
    name = "15-30",
    showlegend = TRUE
  ) %>%
  add_trace(
    type = "scattermapbox",
    mode = "markers",
    lon = NA,
    lat = NA,
    marker = list(color = "#FDE725", size = 12),
    name = "<15",
    showlegend = TRUE
  ) %>%
  layout(
    title = "Pollution Hotspots in Madrid, 2001-2018",
    mapbox = list(
      style = "open-street-map",
      zoom = 10,
      center = list(
        lon = mean(q4_data$lon, na.rm = TRUE),
        lat = mean(q4_data$lat, na.rm = TRUE)
      )
    ),
    legend = list(
      title = list(
        text = "Avg. concentration"
      ),
      traceorder = "normal"
    ),
    updatemenus = list(
      list(
        type = "dropdown",
        x = 0,
        y = 1.15,
        buttons = lapply(
          pollutants,
          function(p) {
            list(
              method = "restyle",
              args = list(
                list("transforms[0].value" = p),
                0
              ),
              label = p
            )
          }
        )
      )
    )
  )
