library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(plotly)
library(leaflet)
library(viridis)
library(purrr)
library(tidyverse)

# Load the yearly pollution summary dataset used for trend plots.
yearly <- read.csv("yearly_pollution.csv")

# Build the list of yearly Madrid pollution files from 2001 to 2018.
fileList <- paste0("VDS2526_Madrid/madrid_", 2001:2018, ".csv")

# Read all yearly CSV files and combine them into one long dataset.
all_data <- map_dfr(
  fileList,
  ~ read_csv(.x, show_col_types = FALSE)
)

# Convert the date column to Date format and extract the year for filtering/grouping later.
all_data <- all_data %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  )

# Load station metadata, especially station names and geographic coordinates.
stations <- read_csv("VDS2526_Madrid/stations.csv", show_col_types = FALSE)

# Reshape pollutants measured in µg/m³ from wide format into long format for plotting.
ug_long <- yearly %>%
  select(year, NO, NO_2, NOx, O_3, SO_2, PM10, PM25, OXY) %>%
  pivot_longer(-year, names_to = "pollutant", values_to = "concentration") %>%
  filter(!is.na(concentration), !is.na(pollutant))

# Order µg/m³ pollutants by their latest-year concentration so legends/lines appear consistently.
ug_order <- ug_long %>%
  group_by(pollutant) %>%
  filter(year == max(year)) %>%
  arrange(desc(concentration)) %>%
  pull(pollutant)
ug_long$pollutant <- factor(ug_long$pollutant, levels = ug_order)

# Reshape pollutants measured in mg/m³ into long format for separate plotting.
mg_long <- yearly %>%
  select(year, CO, TCH, CH4, BEN, EBE, MXY, PXY, TOL, NMHC) %>%
  pivot_longer(-year, names_to = "pollutant", values_to = "concentration") %>%
  filter(!is.na(concentration), !is.na(pollutant))

# Order mg/m³ pollutants by their latest-year concentration for consistent display.
mg_order <- mg_long %>%
  group_by(pollutant) %>%
  filter(year == max(year)) %>%
  arrange(desc(concentration)) %>%
  pull(pollutant)
mg_long$pollutant <- factor(mg_long$pollutant, levels = mg_order)

# Q1: Create interactive trend plots for pollutants up to the selected year.
plot_q1 <- function(yearlimit) {
  # Keep only data up to the year selected by the user.
  ug_long_filtered <- ug_long %>% filter(year <= yearlimit) #|> mutate(year = integer(year))
  mg_long_filtered <- mg_long %>% filter(year <= yearlimit) #|> mutate(year = integer(year))


  # First plot: pollutants measured in µg/m³.
  p1 <- ggplot(ug_long_filtered, aes(x = year, y = concentration, colour = pollutant)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(x = "year", y = "Concentration (µg/m³)", colour = NULL) +
    scale_x_continuous(breaks = scales::pretty_breaks(10)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    scale_color_viridis_d(option = "E")

  # Second plot: pollutants measured in mg/m³.
  p2 <- ggplot(mg_long_filtered, aes(x = year, y = concentration, colour = pollutant)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(x = "year", y = "Concentration (mg/m³)", colour = NULL) +
    scale_x_continuous(breaks = scales::pretty_breaks(10)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    scale_color_viridis_d(option = "E")

  # Convert both ggplots to interactive plotly charts and place them side by side.
  subplot(ggplotly(p1), ggplotly(p2), nrows = 1, shareY = FALSE, titleX = TRUE, titleY = TRUE)
}

# Q2a helper: calculate average NO2 by station and prepare colors/map data.
plot_q2a_data <- function(yearlimit) {
  # Filter to the selected year and calculate average NO2 for each station.
  q2a_data <- all_data %>%
    filter(year == yearlimit) %>%
    group_by(station) %>%
    summarise(
      avg_NO2 = mean(NO_2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_NO2))

  # Create readable breakpoints for grouping stations by NO2 level.
  no2_breaks <- pretty(q2a_data$avg_NO2, n = 5)

  # Build a color palette that maps each NO2 range to a color.
  pal <- colorBin(
    palette = rev(cividis(length(no2_breaks) - 1)),
    domain = q2a_data$avg_NO2,
    bins = no2_breaks,
    pretty = FALSE
  )

  # Add pollution categories and matching colors to each station.
  q2a_data <- q2a_data %>%
    mutate(
      pollution_level = cut(
        avg_NO2,
        breaks = no2_breaks,
        include.lowest = TRUE,
        dig.lab = 5
      ),
      pollution_color = pal(avg_NO2)
    )

  # Join pollution values with station coordinates so the data can be mapped.
  q2a_map_data <- q2a_data %>%
    left_join(stations, by = c("station" = "id"))

  list(
    q2a_data = q2a_data,
    q2a_map_data = q2a_map_data,
    pal = pal
  )
}

# Q2a: Create a leaflet hotspot map showing stations with higher/lower NO2 levels.
plot_q2a_hotspot <- function(yearlimit) {
  q2a <- plot_q2a_data(yearlimit)

  leaflet(q2a$q2a_map_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # addTiles() %>%
    # Each marker represents one station; size and color reflect average NO2 level.
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = ~ avg_NO2 / 3,
      color = ~pollution_color,
      fillColor = ~pollution_color,
      fillOpacity = 0.8,
      stroke = FALSE,
      popup = ~ paste(
        "<b>Station:</b>", name,
        "<br><b>Average NO2:</b>", round(avg_NO2, 2),
        "<br><b>Category:</b>", pollution_level
      )
    ) %>%
    addLegend(
      position = "topright",
      pal = q2a$pal,
      values = ~avg_NO2,
      title = "Avg. NO2 (µg/m3)",
      opacity = 1
    )
}

# Q2a: Create an interactive bar chart ranking stations by average NO2.
plot_q2a_barchart <- function(yearlimit) {
  q2a <- plot_q2a_data(yearlimit)

  # Horizontal bars make it easier to compare station rankings.
  plot_ly(
    data = q2a$q2a_data,
    x = ~avg_NO2,
    y = ~ reorder(as.character(station), avg_NO2),
    type = "bar",
    orientation = "h",
    color = ~pollution_level,
    colors = setNames(
      unique(q2a$q2a_data$pollution_color),
      unique(q2a$q2a_data$pollution_level)
    ),
    hoverinfo = "text",
    text = ~ paste(
      "Station:", station,
      "<br>Average NO2:", round(avg_NO2, 2),
      "<br>Category:", pollution_level
    )
  ) %>%
    layout(
      title = paste("Average NO2 Levels by Station", yearlimit),
      xaxis = list(title = "Average NO2"),
      yaxis = list(title = "Station"),
      legend = list(
        title = list(text = "Avg. NO2 (µg/m3)")
      )
    )
}

plot_q2b_spaghetti <- function(yearlimit) {}

# Q3a: Show how each pollutant changed relative to a chosen base year.
plot_q3a <- function(base_year = 2001, max_year = 2018) {
  # Convert yearly data to long format and calculate each pollutant as a percentage of its base-year value.
  relative_df <- yearly %>%
    select(!X) %>%
    pivot_longer(
      cols = -year,
      names_to = "pollutant",
      values_to = "value"
    ) %>%
    filter(year <= max_year) %>%
    group_by(pollutant) %>%
    mutate(
      base_value = value[year == base_year][1],
      # Index values: 100 means no change from the base year; above/below 100 means increase/decrease.
      relative_change = (value / base_value) * 100
    ) %>%
    ungroup() %>%
    filter(
      !is.na(relative_change),
      !is.nan(relative_change),
      is.finite(relative_change)
    )
  relative_df
  p_relative <- ggplot(
    relative_df,
    aes(
      x = year,
      y = relative_change,
      color = pollutant,
      group = pollutant,
      text = paste(
        "Year:", year,
        "<br>Pollutant:", pollutant,
        "<br>Relative:", round(relative_change, 1), "%"
      )
    )
  ) +
    # Reference line showing the base-year level.
    geom_hline(
      yintercept = 100,
      linetype = "dashed",
      linewidth = 1,
      color = "black"
    ) +
    geom_line(linewidth = 1.2, alpha = 0.9) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = seq(2001, 2018, 1)) +
    labs(
      title = "Relative Change in Pollutant Concentration (2001 = 100%)",
      x = "Year",
      y = "Relative Concentration (%)",
      color = "Pollutant"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      panel.grid.minor = element_blank()
    ) +
    scale_color_viridis_d(option = "E")

  ggplotly(p_relative, tooltip = "text")
}

# Q3b: Create a correlation heatmap showing relationships between pollutants.
plot_q3b <- function() {
  # Keep only numeric pollutant columns; metadata columns are removed.
  corr_data <- yearly %>%
    select(-any_of(c("X", "year", "date", "station"))) %>%
    select(where(is.numeric))

  # Calculate Pearson correlations using all available pairs of observations.
  corr_matrix <- cor(
    corr_data,
    use = "pairwise.complete.obs",
    method = "pearson"
  )

  # Hide the upper triangle because the correlation matrix is symmetrical.
  corr_masked <- corr_matrix
  corr_masked[upper.tri(corr_masked)] <- NA

  plot_ly(
    x = colnames(corr_masked),
    y = rownames(corr_masked),
    z = corr_masked,
    type = "heatmap",
    colorscale = "Cividis",
    opacity = 0.75,
    zmin = -1,
    zmax = 1,
    colorbar = list(
      title = "Correlation",
      titleside = "right",
      tickvals = c(-1, -0.5, 0, 0.5, 1),
      ticktext = c("-1", "-0.5", "0", "0.5", "1"),
      len = 0.75
    ),
    text = matrix(
      sprintf("%.2f", corr_masked),
      nrow = nrow(corr_masked),
      dimnames = list(rownames(corr_masked), colnames(corr_masked))
    ),
    hovertemplate = paste0(
      "<b>%{y} × %{x}</b><br>",
      "Pearson r = %{text}<br>",
      "<extra></extra>"
    ),
    xgap = 2,
    ygap = 2
  ) %>%
    add_annotations(
      x = rep(colnames(corr_masked), each = nrow(corr_masked)),
      y = rep(rownames(corr_masked), times = ncol(corr_masked)),
      text = matrix(
        ifelse(is.na(corr_masked), "", sprintf("%.2f", corr_masked)),
        nrow = nrow(corr_masked),
        dimnames = list(rownames(corr_masked), colnames(corr_masked))
      ),
      showarrow = FALSE,
      font = list(size = 10, color = "black"),
      xref = "x",
      yref = "y"
    ) %>%
    layout(
      title = list(
        text = "<b>Correlation Between Air Pollutants (2001–2018)</b>",
        font = list(size = 16),
        x = 0.5
      ),
      xaxis = list(
        title = "",
        tickangle = -45,
        tickfont = list(size = 11),
        showgrid = FALSE,
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 11),
        showgrid = FALSE,
        autorange = "reversed",
        fixedrange = TRUE
      ),
      margin = list(l = 80, r = 20, t = 60, b = 80),
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      font = list(family = "Arial, sans-serif")
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d"
      ),
      toImageButtonOptions = list(
        format = "png",
        filename = "correlation_heatmap_madrid",
        width = 900,
        height = 800,
        scale = 2
      )
    )
}

# Q4: Map hotspots for any selected pollutant and year.
plot_q4 <- function(yearlimit, selected_pollutant) {
  # Identify pollutant columns dynamically, excluding date/year/station identifiers.
  pollutants <- names(all_data)[
    !(names(all_data) %in% c("date", "year", "station"))
  ]

  # Filter to the selected year and pollutant, then calculate station-level average values.
  q4_data <- all_data %>%
    filter(year == yearlimit) %>%
    select(year, station, all_of(pollutants)) %>%
    pivot_longer(
      cols = all_of(pollutants),
      names_to = "pollutant",
      values_to = "value"
    ) %>%
    filter(pollutant == selected_pollutant) %>%
    group_by(year, station, pollutant) %>%
    summarise(
      avg_value = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(avg_value)) %>%
    left_join(
      stations,
      by = c("station" = "id")
    )

  # If there are no records for the selected pollutant/year, return an empty map with a clear message.
  if (nrow(q4_data) == 0) {
    return(
      plot_ly(
        type = "scattermapbox",
        mode = "markers",
        lon = numeric(0),
        lat = numeric(0)
      ) %>%
        layout(
          title = paste("Pollution Hotspots in Madrid,", yearlimit),
          mapbox = list(
            style = "carto-positron",
            zoom = 10,
            center = list(
              lon = -3.7038,
              lat = 40.4168
            )
          ),
          annotations = list(
            list(
              text = paste(
                "No data available for",
                selected_pollutant,
                "in",
                yearlimit
              ),
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(
                size = 20,
                color = "black"
              ),
              bgcolor = "rgba(255,255,255,0.85)",
              bordercolor = "black",
              borderwidth = 1,
              borderpad = 8
            )
          )
        )
    )
  }

  # Create value ranges for coloring the map markers.
  value_breaks <- pretty(q4_data$avg_value, n = 5)

  # Handle the edge case where all stations have the same value.
  if (length(value_breaks) < 2 || length(unique(q4_data$avg_value)) < 2) {
    single_value <- unique(q4_data$avg_value)[1]
    value_breaks <- c(single_value - 0.01, single_value + 0.01)
  }

  pal_q4 <- colorBin(
    palette = rev(cividis(length(value_breaks) - 1)),
    domain = q4_data$avg_value,
    bins = value_breaks,
    pretty = FALSE
  )

  # Add category, color, and scaled marker size for each station.
  q4_data <- q4_data %>%
    mutate(
      pollution_level = cut(
        avg_value,
        breaks = value_breaks,
        include.lowest = TRUE,
        dig.lab = 5
      ),
      pollution_color = pal_q4(avg_value),
      marker_size = scales::rescale(avg_value, to = c(15, 60))
    )

  # Prepare one legend entry per pollution category.
  legend_data <- q4_data %>%
    distinct(pollution_level, pollution_color) %>%
    arrange(pollution_level)

  # Main map layer: station markers positioned by longitude and latitude.
  p <- plot_ly(
    data = q4_data,
    type = "scattermapbox",
    mode = "markers",
    lon = ~lon,
    lat = ~lat,
    marker = list(
      color = q4_data$pollution_color,
      size = q4_data$marker_size,
      opacity = 0.90
    ),
    text = ~ paste(
      "Year:", year,
      "<br>Pollutant:", pollutant,
      "<br>Station:", name,
      "<br>Longitude:", round(lon, 4),
      "<br>Latitude:", round(lat, 4),
      "<br>Average:", round(avg_value, 2),
      "<br>Category:", pollution_level
    ),
    hoverinfo = "text",
    showlegend = FALSE
  )

  # Add invisible traces so Plotly can display a custom legend for the color categories.
  for (i in seq_len(nrow(legend_data))) {
    p <- p %>%
      add_trace(
        type = "scattermapbox",
        mode = "markers",
        lon = NA,
        lat = NA,
        marker = list(
          size = 15,
          color = legend_data$pollution_color[i],
          opacity = 0.90
        ),
        name = as.character(legend_data$pollution_level[i]),
        showlegend = TRUE,
        inherit = FALSE
      )
  }

  p %>%
    layout(
      title = paste("Pollution Hotspots in Madrid,", yearlimit),
      mapbox = list(
        style = "carto-positron",
        zoom = 10,
        center = list(
          lon = mean(q4_data$lon, na.rm = TRUE),
          lat = mean(q4_data$lat, na.rm = TRUE)
        )
      ),
      legend = list(
        title = list(
          text = "Avg. concentration"
        )
      )
    )
}

## Code of the trend monitoring plot between 2008-2028

#-----Refer to script q2b for data wrangling of the datasets here-----

# Load pre-processed monthly station-level data for the trend monitoring plot.
df_monthly <- readRDS("VDS2526_Madrid/q2b_month.RDS")

# Load pre-processed Madrid city average data for comparison against each station.
df_madrid <- readRDS("VDS2526_Madrid/q2b_madrid.RDS")

# Named vector used for dropdown labels: display names on the left, pollutant codes on the right.
# A name vector for the dropdowmn menu

pollutants <- setNames(c(
  "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
  "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
  "TCH", "CH4", "NMHC"
), c(
  "sulphur dioxide",
  "carbon monoxide",
  "nitric oxide",
  "nitrogen dioxide",
  "particles smaller than 2.5 μm",
  "particles smaller than 10 μm",
  "nitrous oxides",
  "ozone",
  "toluene (methylbenzene)",
  "benzene",
  "ethylbenzene",
  "m-xylene level",
  "p-xylene",
  "o-xylene",
  "total hydrocarbons",
  "methane level",
  "non-methane hydrocarbons"
))


## This function accepts two data frames and 03 aesthetic x: date, y: concentration
## name for the facetting

# Q2b: Build one faceted “spaghetti plot” comparing each station with the Madrid average.
panel_plot <- function(df1, df2, x, y, name) {
  # Lookup table used to convert pollutant codes into readable names and units for labels.
  pollutant <- data.frame(
    variable = c(
      "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
      "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
      "TCH", "CH4", "NMHC"
    ),
    description = c(
      "sulphur dioxide",
      "carbon monoxide",
      "nitric oxide",
      "nitrogen dioxide",
      "particles smaller than 2.5 μm",
      "particles smaller than 10 μm",
      "nitrous oxides",
      "ozone",
      "toluene (methylbenzene)",
      "benzene",
      "ethylbenzene",
      "m-xylene level",
      "p-xylene",
      "o-xylene",
      "total hydrocarbons",
      "methane level",
      "non-methane hydrocarbons"
    ),
    unit = c(
      "μg/m³",
      "mg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "μg/m³",
      "mg/m³",
      "mg/m³",
      "mg/m³"
    )
  )


  df1 |>
    ggplot(aes(x = !!sym(x), y = !!sym(y))) + # We declare the x and y aesthetics that will be used to highlight data
    geom_line(data = select(df1, -station), aes(group = station2), color = "grey80", linewidth = .9) + # Here we plot all the lines in  grey than will appear as a background on each panel
    geom_line(data = df2, color = "#1a85ff", linewidth = .4, linetype = 5) + # Here we plot in blue the line of the average monthly emissions on all the panels
    geom_text(aes(x = max(date, na.rm = T) - 1100, y = max(!!sym(y), na.rm = T), label = str_trunc(name, 15)), # We add the names of the stations on each panel
      size = 4, color = "grey45", hjust = 0, family = "Lato"
    ) +
    geom_line(aes(group = station), color = "#d41159", linewidth = .5) + # This code adds the highlighted line of the station corresponding to the facetted panel
    scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2018-04-01"))) + # Limiting the data within the date range collected
    labs(y = paste0("Average emissions of ", pollutant$description[pollutant$variable == y], " (", pollutant$unit[pollutant$variable == y], ")"), x = "Date") + # labels, with some html to label the title
    ggtitle(glue::glue("<b>Montly average air concentration of {pollutant$description[pollutant$variable == y]} in <span style = 'color:#d41159;'> Air Stations </span> compared to <span style = 'color:#1a85ff;'> the Average of the city emissions </span> </b><br>")) +
    facet_wrap(vars(station), ncol = 4) + # Facetting the plot per station
    theme_minimal(base_family = "Lato", base_size = 14) + # Adding some theme
    theme(
      axis.text = element_text(
        size = 14,
        color = "grey35",
        face = "bold",
        family = "Spline Sans"
      ),
      legend.position = "top",
      strip.text = element_blank(),
      legend.title = element_blank(),
      plot.background = element_rect(fill = "#FFFFFF", colour = NA),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 15, face = "bold"),
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox_simple(face = "bold", family = "Playfair Display")
    )
}


# using functionnal propgramming to produce a plot for each corresponding pollutant

# Generate and store one q2b panel plot for each pollutant.
plots_q2b <- purrr::map(
  c(
    "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
    "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
    "TCH", "CH4", "NMHC"
  ),
  ~ panel_plot(df_monthly, df_madrid, x = "date", y = .x, name = "name")
) |> setNames(nm = c(
  "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
  "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
  "TCH", "CH4", "NMHC"
))
