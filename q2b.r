
library(dplyr)
library(ggplot2)
library(gganimate)
library(tidyr)
library(stringr)


files <- file.path("data",list.files("data/")[-length(list.files("data/"))])

df <- plyr::rbind.fill(Map(read.csv, files))

stations <- read.csv(file.path("data", "stations.csv"))


df1116 <- do.call(rbind, Map(read.csv, files[11:16]))

df1718 <- do.call(rbind, Map(read.csv, files[17:18]))

df0103 <- do.call(rbind, Map(read.csv, files[1:3]))

df0410 <- do.call(rbind, Map(read.csv, files[4:10]))


cols1 <- intersect(colnames(df0103), colnames(df0410))

cols2 <- intersect(cols1, colnames(df1116))

cols3 <- intersect(cols2, colnames(df1718))

#df <- do.call(rbind, Map(\(x)x[cols3], list(df1718, df0103, df0410, df1116)))





df <- inner_join(df, stations, by = join_by(station == id))

df <- df |> transform(date = as.Date(date))|>
  transform(year = as.numeric(format.Date(date, format = "%Y")),
            month = factor(format.Date(date, format = "%m"), labels = month.abb)
)



df_monthly <- df |> filter(year >= 2008) |>group_by(year, month, station, name)|> 
  summarise(across(c("BEN", "CO", "EBE", "MXY", "NMHC", "NO_2", "NOx", "OXY", 
"O_3", "PM10", "PXY", "SO_2", "TCH", "TOL", "PM25", 
"NO", "CH4"), ~mean(.x, na.rm = T)), .groups = "drop")|>
  mutate(station2 = station,
  date = lubridate::make_date(year = year, month = month, day = 1L)
  )

df_madrid <- df_monthly|>  group_by(date)|>
  summarise(across(c("BEN", "CO", "EBE", "MXY", "NMHC", "NO_2", "NOx", "OXY", 
"O_3", "PM10", "PXY", "SO_2", "TCH", "TOL", "PM25", 
"NO", "CH4"), ~mean(.x, na.rm = T)), .groups = "drop")







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
  ggplot(aes(x = !!sym(x), y = !!sym(y)))+
  geom_line(data = select(df1, - station), aes(group = station2), color ="grey80", linewidth = .9 )+
  geom_line(data = df2 , color = "#1a85ff", linewidth = .4, linetype = 5)+
  geom_text(aes(x = max(date,na.rm = T)-1300, y = max(!!sym(y), na.rm = T)+1, label = str_trunc(name, 15)), 
  size = 4, color = "grey45", hjust = 0, fontfamily = "Lato")+
  geom_line(aes(group = station), color = "#d41159", linewidth = .5)+  
  scale_x_date(limits =c(as.Date("2008-01-01"), as.Date("2018-04-01")) )+
  labs(y = paste0("Emissions of ", pollutant$description[pollutant$variable == y], " (", pollutant$unit[pollutant$variable == y], ")"))+
    ggtitle(glue::glue("<b>Montly mean air concentration of {pollutant$description[pollutant$variable == y]} in <span style = 'color:#d41159;'> areas of Madrid </span> compared to <span style = 'color:#1a85ff;'> the mean city emissions </span> </b><br>"))+
  facet_wrap(vars(station), ncol = 5)+
      theme_minimal(base_family =  "Lato", base_size = 14) +
    theme(
      axis.text.x = element_text(
        size = 10,
        color = "#111111",
        face = "bold"
      ),
      legend.position = "top",
      strip.text = element_blank(),
      legend.title = element_blank(),
      plot.background = element_rect(fill = "#FFFFFF", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(),
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox_simple(face = "bold", family = "Playfair Display")
    )
}




panel_plot(df_monthly, df_madrid, x = "date", y = "CO", name = "name")

plots <- purrr::map(c(
    "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
    "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
    "TCH", "CH4", "NMHC"),

  ~panel_plot(df_monthly, df_madrid, x = "date", y = .x , name = "name")

  ) |> setNames(nm = c(
    "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
    "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
    "TCH", "CH4", "NMHC"))

purrr::walk2(plots,c(
    "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
    "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
    "TCH", "CH4", "NMHC") ,~ggsave(file.path("fig",paste0(.y,".png")), .x, 
  dpi = 720, width = 20, height = 10
  ))



