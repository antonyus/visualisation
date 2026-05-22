# =========================================================
# Common plotting functions
# =========================================================

library(tidyverse)
library(lubridate)
library(plotly)
library(htmlwidgets)

# ---------------------------------------------------------
# Load and prepare data
# ---------------------------------------------------------
load_pollution_data <- function(path) {
    # List of pollutant columns used across all plots.
    pollutants <- c(
        "NMHC", "CH4", "CO", "SO_2", "NO",
        "NO_2", "PM25", "NOx", "O_3", "TOL",
        "BEN", "EBE", "MXY", "PXY", "OXY",
        "TCH", "PM10"
    )

    # Select only Madrid CSV files from years 2001–2018.
    files <- list.files(
        path,
        pattern = "^madrid_(200[1-9]|201[0-8])\\.csv$",
        full.names = TRUE
    )

    # Read all selected files into one data frame and extract the year
    # from the date column for later yearly aggregation.
    df <- files %>%
        map_dfr(read_csv, show_col_types = FALSE) %>%
        mutate(
            date = ymd_hms(date),
            year = year(date)
        )

    # Return both the prepared data and pollutant list so they can be reused.
    list(df = df, pollutants = pollutants)
}

# =========================================================
# Function 1: Yearly averages by pollutant groups
# =========================================================
plot_yearly_averages <- function(df, pollutants) {
    # Calculate the yearly mean for every pollutant, then convert the data
    # from wide format to long format so Plotly can draw one line per pollutant.
    yearly_avg <- df %>%
        group_by(year) %>%
        summarise(
            across(all_of(pollutants), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop"
        ) %>%
        pivot_longer(
            cols = -year,
            names_to = "pollutant",
            values_to = "value"
        ) %>%
        # Replace NaN values caused by fully missing years/pollutants.
        mutate(value = ifelse(is.nan(value), NA, value)) %>%
        drop_na()

    # Pollutants are split into groups because they use different units/scales.
    group1 <- c("NMHC", "CH4", "CO")
    group2 <- c(
        "SO_2", "NO", "NO_2", "PM10", "PM25", "NOx", "O_3",
        "TOL", "BEN", "EBE", "MXY", "PXY", "OXY", "TCH"
    )

    all_pollutants <- c(group1, group2)

    # Custom colour palette for consistent pollutant colours across traces.
    palette_vals <- c(
        "#3266ad", "#d85a30", "#1d9e75", "#c94b8e", "#73726c",
        "#ba7517", "#534ab7", "#185fa5", "#993c1d", "#0f6e56",
        "#3b6d11", "#a32d2d", "#854f0b", "#1d9e75", "#d85a30",
        "#3266ad", "#c94b8e"
    )

    color_map <- setNames(palette_vals[seq_along(all_pollutants)], all_pollutants)

    # Helper function that creates one line chart for a selected pollutant group.
    make_group_plot <- function(data, pollutant_group, y_title) {
        p <- plot_ly()

        # Add a separate trace for each pollutant in the group.
        for (pol in pollutant_group) {
            d <- data %>% filter(pollutant == pol)

            p <- add_trace(
                p,
                x = d$year,
                y = d$value,
                type = "scatter",
                mode = "lines+markers",
                name = pol,
                line = list(color = color_map[pol], width = 2),
                marker = list(color = color_map[pol], size = 6),
                legendgroup = pol,
                # Custom hover text makes the interactive chart easier to read.
                hovertemplate = paste0(
                    "<b>", pol, "</b><br>",
                    "Year: %{x}<br>",
                    "Value: %{y:.2f} ", y_title,
                    "<extra></extra>"
                )
            )
        }

        p
    }

    # Create two subplots with different y-axis units.
    p1 <- make_group_plot(yearly_avg, group1, "mg/m³")
    p2 <- make_group_plot(yearly_avg, group2, "µg/m³")

    # Combine both plots into one interactive figure.
    subplot(
        p1, p2,
        nrows = 1,
        shareX = FALSE,
        shareY = FALSE,
        titleX = TRUE,
        titleY = TRUE,
        margin = 0.05
    ) %>%
        layout(
            title = list(
                text = "<b>Air Pollution Yearly Averages — Madrid (2001–2018)</b>",
                font = list(size = 16),
                x = 0.5
            ),
            hovermode = "x",
            legend = list(
                orientation = "v",
                x = 1.01,
                y = 1,
                font = list(size = 11)
            ),
            xaxis = list(title = "Year", tickangle = -45, dtick = 1, tickfont = list(size = 9)),
            xaxis2 = list(title = "Year", tickangle = -45, dtick = 1, tickfont = list(size = 9)),
            xaxis3 = list(title = "Year", tickangle = -45, dtick = 1, tickfont = list(size = 9)),
            yaxis = list(title = "mg/m³"),
            yaxis2 = list(title = "µg/m³"),
            paper_bgcolor = "white",
            plot_bgcolor = "white",
            font = list(family = "Arial, sans-serif")
        ) %>%
        # Keep useful export options and remove unnecessary selection tools.
        config(
            displayModeBar = TRUE,
            modeBarButtonsToRemove = c("select2d", "lasso2d"),
            toImageButtonOptions = list(
                format = "png",
                filename = "air_pollution_madrid",
                width = 1800,
                height = 600,
                scale = 2
            )
        )
}

# =========================================================
# Function 2: Relative change plot
# =========================================================
plot_relative_change <- function(df, pollutants, base_year = 2001) {
    # First calculate yearly averages, because relative change is compared
    # year by year rather than from raw daily/hourly observations.
    yearly_avg <- df %>%
        group_by(year) %>%
        summarise(
            across(all_of(pollutants), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop"
        )

    relative_df <- yearly_avg %>%
        pivot_longer(
            cols = -year,
            names_to = "pollutant",
            values_to = "value"
        ) %>%
        group_by(pollutant) %>%
        mutate(
            # Base value is the concentration in the selected base year.
            base_value = value[year == base_year][1],
            # 100% means no change compared with the base year.
            relative_change = (value / base_value) * 100
        ) %>%
        ungroup() %>%
        # Remove invalid results caused by missing or zero base values.
        filter(
            !is.na(relative_change),
            !is.nan(relative_change),
            is.finite(relative_change)
        )

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
        # Reference line showing the base year level.
        geom_hline(
            yintercept = 100,
            linetype = "dashed",
            linewidth = 0.5,
            color = "black"
        ) +
        geom_line(linewidth = 0.8, alpha = 0.9) +
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
            plot.title = element_text(
                face = "bold",
                hjust = 0.5,
                size = 12
            ),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right",
            panel.grid.minor = element_blank()
        )

    # Convert ggplot into an interactive Plotly chart with custom tooltip text.
    ggplotly(p_relative, tooltip = "text")
}

# =========================================================
# Function 3: Correlation heatmap
# =========================================================
plot_correlation_heatmap <- function(df, pollutants) {
    # Use yearly averages so the heatmap compares long-term pollutant trends.
    yearly_avg <- df %>%
        group_by(year) %>%
        summarise(
            across(all_of(pollutants), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop"
        )

    corr_data <- yearly_avg %>%
        select(all_of(pollutants))

    # Pearson correlation measures how similarly pollutant concentrations
    # move over time. Pairwise complete observations handle missing values.
    corr_matrix <- cor(
        corr_data,
        use = "pairwise.complete.obs",
        method = "pearson"
    )

    # Mask the upper triangle to avoid showing duplicate correlations.
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
        # Text values are used both for hover labels and cell annotations.
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
        # Add the correlation values directly into the heatmap cells.
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
        # Disable navigation tools that are not needed for a fixed heatmap,
        # but keep the export-to-image option available.
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
