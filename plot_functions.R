library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(plotly)
library(leaflet)
library(viridis)
library(purrr)
library(tidyverse)

yearly <- read.csv("yearly_pollution.csv")

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

stations <- read_csv("VDS2526_Madrid/stations.csv",show_col_types = FALSE)

ug_long <- yearly %>%
  select(year, NO, NO_2, NOx, O_3, SO_2, PM10, PM25, OXY) %>%
  pivot_longer(-year, names_to = "pollutant", values_to = "concentration") %>%
  filter(!is.na(concentration),!is.na(pollutant))

ug_order <- ug_long %>%
  group_by(pollutant) %>%
  filter(year == max(year)) %>%
  arrange(desc(concentration)) %>%
  pull(pollutant)
ug_long$pollutant <- factor(ug_long$pollutant, levels = ug_order)

mg_long <- yearly %>%
  select(year, CO, TCH, CH4, BEN, EBE, MXY, PXY, TOL, NMHC) %>%
  pivot_longer(-year, names_to = "pollutant", values_to = "concentration") %>%
  filter(!is.na(concentration),!is.na(pollutant))

mg_order <- mg_long %>%
  group_by(pollutant) %>%
  filter(year == max(year)) %>%
  arrange(desc(concentration)) %>%
  pull(pollutant)
mg_long$pollutant <- factor(mg_long$pollutant, levels = mg_order)


plot_q1 <- function(yearlimit){
  
  ug_long_filtered <- ug_long %>% filter(year <= yearlimit)#|> mutate(year = integer(year))
  mg_long_filtered <- mg_long %>% filter(year <= yearlimit)#|> mutate(year = integer(year))
  
  p1 <- ggplot(ug_long_filtered, aes(x = year, y = concentration, colour = pollutant)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2) +
    labs(x = "year", y = "Concentration (µg/m³)", colour = NULL) +
    scale_x_continuous(breaks = scales::pretty_breaks(10))+
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) + scale_color_viridis_d(option = "E")
  
  p2 <- ggplot(mg_long_filtered, aes(x = year, y = concentration, colour = pollutant)) +
    geom_line(linewidth = 1.2) + geom_point(size = 2) +
    labs(x = "year", y = "Concentration (mg/m³)", colour = NULL) +
    scale_x_continuous(breaks = scales::pretty_breaks(10))+
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) + scale_color_viridis_d(option = "E")
  
  subplot(ggplotly(p1), ggplotly(p2), nrows = 1, shareY = FALSE, titleX = TRUE, titleY = TRUE)
}

#### 2a HOTSPOT & BARCHART ####

# Create and prepare data
plot_q2a_data <- function(yearlimit) {
  # filter data for the selected year
  q2a_data <- all_data %>%
    filter(year == yearlimit) %>% #group data
    # calculate average no2 concentration by station
    group_by(station) %>%
    summarise(
      avg_NO2 = mean(NO_2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Sort stations from highest to lowest
    arrange(desc(avg_NO2))
  
  #legend breaks
  no2_breaks <- pretty(q2a_data$avg_NO2, n = 5)
  
  # color palette (color blind friendly, cividis = blue & yellow)
  pal <- colorBin(
    palette = rev(cividis(length(no2_breaks) - 1)),
    domain = q2a_data$avg_NO2,
    bins = no2_breaks,
    pretty = FALSE
  )
  
  #assign pollution categories and colors
  q2a_data <- q2a_data %>%
    mutate(
      pollution_level = cut( # conc. categories
        avg_NO2,
        breaks = no2_breaks,
        include.lowest = TRUE,
        dig.lab = 5
      ),
      pollution_color = pal(avg_NO2) #colors baased on conc. level
    )
  
  #station coordinates
  q2a_map_data <- q2a_data %>%
    left_join(stations, by = c("station" = "id"))
  
  #return data & colors
  list(
    q2a_data = q2a_data,
    q2a_map_data = q2a_map_data,
    pal = pal
  )
}

#create hotspot map
plot_q2a_hotspot <- function(yearlimit) {
  q2a <- plot_q2a_data(yearlimit)
  
  leaflet(q2a$q2a_map_data) %>%
    #map background (clean background)
    addProviderTiles(providers$CartoDB.Positron)%>%
    # addTiles() %>%
    addCircleMarkers( #markers
      lng = ~lon,
      lat = ~lat,
      radius = ~avg_NO2 / 3, #marker size based on average no2
      color = ~pollution_color,
      fillColor = ~pollution_color, #marker size based on category
      fillOpacity = 0.8,
      stroke = FALSE,
      #pop up when hovering
      popup = ~paste( 
        "<b>Station:</b>", name,
        "<br><b>Average NO2:</b>", round(avg_NO2, 2),
        "<br><b>Category:</b>", pollution_level
      )
    ) %>%
    # DYNAMIC LEGEND (very importante)
    addLegend(
      position = "topright",
      pal = q2a$pal,
      values = ~avg_NO2,
      title = "Avg. NO2 (µg/m3)",
      opacity = 1
    )
}

#create bar chart
plot_q2a_barchart <- function(yearlimit) {
  q2a <- plot_q2a_data(yearlimit)
  
  plot_ly(
    data = q2a$q2a_data,
    #horizontal bars
    x = ~avg_NO2,
    y = ~reorder(as.character(station), avg_NO2),
    type = "bar",
    orientation = "h",
    color = ~pollution_level, #colors by pollution category
    colors = setNames(
      unique(q2a$q2a_data$pollution_color),
      unique(q2a$q2a_data$pollution_level)
    ),
    #hovver info
    hoverinfo = "text",
    text = ~paste(
      "Station:", station,
      "<br>Average NO2:", round(avg_NO2, 2),
      "<br>Category:", pollution_level
    )
  ) %>%
    #chart layout
    layout(
      title = paste("Average NO2 Levels by Station", yearlimit),
      xaxis = list(title = "Average NO2"),
      yaxis = list(title = "Station"),
      #leg3end info
      legend = list( 
        title = list(text = "Avg. NO2 (µg/m3)")
      )
    )
}

plot_q2b_spaghetti <- function(yearlimit) {
}


plot_q3a <- function(base_year = 2001, max_year=2018) {
  relative_df <- yearly %>%
    select(!X) %>%
    pivot_longer(
      cols = -year,
      names_to = "pollutant",
      values_to = "value"
    ) %>% filter(year <= max_year)%>%
    group_by(pollutant) %>%
    mutate(
      base_value = value[year == base_year][1],
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
    ) + scale_color_viridis_d(option = "E")
  
  ggplotly(p_relative, tooltip = "text")
}

plot_q3b <- function(){
  corr_data <- yearly %>% select(-any_of(c("X", "year", "date", "station"))) %>% select(where(is.numeric))
  
  corr_matrix <- cor(
    corr_data,
    use = "pairwise.complete.obs",
    method = "pearson"
  )
  
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

#### FANCIER & HOTSPOT WITH DROP DOWN ####

plot_q4 <- function(yearlimit, selected_pollutant) {
  
  #get data
  pollutants <- names(all_data)[
    !(names(all_data) %in% c("date", "year", "station"))
  ]

  #long format convversion
  q4_data <- all_data %>%
    filter(year == yearlimit) %>%
    select(year, station, all_of(pollutants)) %>%
    pivot_longer(
      cols = all_of(pollutants),
      names_to = "pollutant",
      values_to = "value"
    ) %>%
    filter(pollutant == selected_pollutant) %>% #filter to keep selected year
    #avg concentration by station
    group_by(year, station, pollutant) %>%
    summarise(
      avg_value = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    #clean up missing values
    filter(!is.na(avg_value)) %>%
    left_join(
      stations,
      by = c("station" = "id")
    )
  
  #IMPORTANTE when no data exists create and show message on map
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
  
  value_breaks <- pretty(q4_data$avg_value, n = 5) #breakpoints for pollutant/year
  
  #edge case when values are the same
  if (length(value_breaks) < 2 || length(unique(q4_data$avg_value)) < 2) {
    single_value <- unique(q4_data$avg_value)[1]
    value_breaks <- c(single_value - 0.01, single_value + 0.01)
  }
  
  #colorblind friendly palette
  pal_q4 <- colorBin(
    palette = rev(cividis(length(value_breaks) - 1)),
    domain = q4_data$avg_value,
    bins = value_breaks,
    pretty = FALSE
  )
  
  #categories, colors & markers
  q4_data <- q4_data %>%
    mutate(
      pollution_level = cut(
        avg_value,
        breaks = value_breaks,
        include.lowest = TRUE,
        dig.lab = 5
      ),
      pollution_color = pal_q4(avg_value),
      marker_size = scales::rescale(avg_value, to = c(15, 60)) #scale marker based on concentration (smallest to largest)
    )
  
  legend_data <- q4_data %>%
    distinct(pollution_level, pollution_color) %>%
    arrange(pollution_level)
  
  #base map
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
    # hover info
    text = ~paste(
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
  
  #IMPORTANTE dynamic legend (changes based on marker count)
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
  
  #final layoutgit 
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


df_monthly <- readRDS("VDS2526_Madrid/q2b_month.RDS")

df_madrid <- readRDS("VDS2526_Madrid/q2b_madrid.RDS")

# A name vector for the dropdowmn menu

pollutants <-  setNames(c(
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

panel_plot <- function(df1, df2, x, y, name){
  
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
  
  
  df1|> 
    ggplot(aes(x = !!sym(x), y = !!sym(y)))+ # We declare the x and y aesthetics that will be used to highlight data
    geom_line(data = select(df1, - station), aes(group = station2), color ="grey80", linewidth = .9 )+ # Here we plot all the lines in  grey than will appear as a background on each panel
    geom_line(data = df2 , color = "#1a85ff", linewidth = .4, linetype = 5)+ # Here we plot in blue the line of the average monthly emissions on all the panels
    geom_text(aes(x = max(date,na.rm = T)-1100, y = max(!!sym(y), na.rm = T), label = str_trunc(name, 15)), # We add the names of the stations on each panel
              size = 4, color = "grey45", hjust = 0, family = "Lato")+
    geom_line(aes(group = station), color = "#d41159", linewidth = .5)+  # This code adds the highlighted line of the station corresponding to the facetted panel
    scale_x_date(limits =c(as.Date("2008-01-01"), as.Date("2018-04-01")) )+ # Limiting the data within the date range collected 
    labs(y = paste0("Average emissions of ", pollutant$description[pollutant$variable == y], " (", pollutant$unit[pollutant$variable == y], ")"), x = "Date")+ # labels, with some html to label the title
    ggtitle(glue::glue("<b>Montly average air concentration of {pollutant$description[pollutant$variable == y]} in <span style = 'color:#d41159;'> Air Stations </span> compared to <span style = 'color:#1a85ff;'> the Average of the city emissions </span> </b><br>"))+
    facet_wrap(vars(station), ncol = 4)+ # Facetting the plot per station
    theme_minimal(base_family =  "Lato", base_size = 14) +# Adding some theme
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

plots_q2b <- purrr::map(c(
  "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
  "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
  "TCH", "CH4", "NMHC"),
  
  ~panel_plot(df_monthly, df_madrid, x = "date", y = .x , name = "name")
  
) |> setNames(nm = c(
  "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
  "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
  "TCH", "CH4", "NMHC"))

