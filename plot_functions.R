library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(plotly)

yearly <- read.csv("yearly_pollution.csv")

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
ug_long_filtered <- ug_long %>% filter(year <= yearlimit)
mg_long_filtered <- mg_long %>% filter(year <= yearlimit)
p1 <- ggplot(ug_long_filtered, aes(x = year, y = concentration, colour = pollutant)) +
  geom_line() + geom_point() +
  labs(x = "year", y = "Concentration (µg/m³)", colour = NULL) +
  theme_classic()

p2 <- ggplot(mg_long_filtered, aes(x = year, y = concentration, colour = pollutant)) +
  geom_line() + geom_point() +
  labs(x = "year", y = "Concentration (mg/m³)", colour = NULL) +
  theme_classic()

combined <- p1 + p2
combined
}


plot_q1 <- function(yearlimit){
  ug_long_filtered <- ug_long %>% filter(year <= yearlimit)
  mg_long_filtered <- mg_long %>% filter(year <= yearlimit)
  p1 <- ggplot(ug_long_filtered, aes(x = year, y = concentration, colour = pollutant)) +
    geom_line() + geom_point() +
    labs(x = "year", y = "Concentration (µg/m³)", colour = NULL) +
    theme_classic()
  
  p2 <- ggplot(mg_long_filtered, aes(x = year, y = concentration, colour = pollutant)) +
    geom_line() + geom_point() +
    labs(x = "year", y = "Concentration (mg/m³)", colour = NULL) +
    theme_classic()
  
  combined <- p1 + p2
  combined
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
    )
  
  ggplotly(p_relative, tooltip = "text")
}

plot_q3b <- function(){
  corr_data <- yearly %>%
    select(!X,!year)
  
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
    colorscale = list(
      list(0, "#d73027"),
      list(0.5, "#ffffff"),
      list(1, "#1a9850")
    ),
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
      text = sprintf("%.2f", as.vector(corr_masked)),
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
