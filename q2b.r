######################################################################
# Instructions to wrangle data for the location trend monitoring plot#
######################################################################
library(dplyr)
library(ggplot2)
library(gganimate)
library(tidyr)
library(stringr)

# Loading the data files and merging them into one filr
files <- file.path("VDS2526_Madrid",list.files("VDS2526_Madrid/")[-length(list.files("VDS2526_Madrid/"))])

df <- plyr::rbind.fill(Map(read.csv, files))

stations <- read.csv(file.path("data", "stations.csv"))



# df1116 <- do.call(rbind, Map(read.csv, files[11:16]))
# 
# df1718 <- do.call(rbind, Map(read.csv, files[17:18]))
# 
# df0103 <- do.call(rbind, Map(read.csv, files[1:3]))
# 
# df0410 <- do.call(rbind, Map(read.csv, files[4:10]))


# cols1 <- intersect(colnames(df0103), colnames(df0410))
# 
# cols2 <- intersect(cols1, colnames(df1116))
# 
# cols3 <- intersect(cols2, colnames(df1718))

#df <- do.call(rbind, Map(\(x)x[cols3], list(df1718, df0103, df0410, df1116)))



# Adding labels to stations and creating a month variable

df <- inner_join(df, stations, by = join_by(station == id))

df <- df |> transform(date = as.Date(date))|>
  transform(year = as.numeric(format.Date(date, format = "%Y")),
            month = factor(format.Date(date, format = "%m"), labels = month.abb)
)

# Filtering station to remain with those between 2008 and 2018. 
# We compute the average of each pollutant for each month and station

df_monthly <- df |> filter(year >= 2008) |>group_by(year, month, station, name)|> 
  summarise(across(c("BEN", "CO", "EBE", "MXY", "NMHC", "NO_2", "NOx", "OXY", 
"O_3", "PM10", "PXY", "SO_2", "TCH", "TOL", "PM25", 
"NO", "CH4"), ~mean(.x, na.rm = T)), .groups = "drop")|>
  mutate(station2 = station,
  date = lubridate::make_date(year = year, month = month, day = 1L)
  )
# Compute the average pollutant concentration for each pollutant and each month

df_madrid <- df|> filter(year >= 2008) |> group_by(year, month)|>
  summarise(across(c("BEN", "CO", "EBE", "MXY", "NMHC", "NO_2", "NOx", "OXY", 
"O_3", "PM10", "PXY", "SO_2", "TCH", "TOL", "PM25", 
"NO", "CH4"), ~mean(.x, na.rm = T)), .groups = "drop")|>
  mutate(
  date = lubridate::make_date(year = year, month = month, day = 1L)
  )

#-------------------This part is described in the plot function script------------ 
df_monthly <- readRDS("data/q2b_month.RDS")

df_madrid <- readRDS("data/q2b_madrid.RDS")

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
  geom_text(aes(x = max(date,na.rm = T)-1100, y = max(!!sym(y), na.rm = T), label = str_trunc(name, 15)), 
  size = 4, color = "grey45", hjust = 0, fontfamily = "Lato")+
  geom_line(aes(group = station), color = "#d41159", linewidth = .5)+  
  scale_x_date(limits =c(as.Date("2008-01-01"), as.Date("2018-04-01")) )+
  labs(y = paste0("Average emissions of ", pollutant$description[pollutant$variable == y], " (", pollutant$unit[pollutant$variable == y], ")"), x = "Date")+
    ggtitle(glue::glue("<b>Montly average air concentration of {pollutant$description[pollutant$variable == y]} in <span style = 'color:#d41159;'> Air Stations </span> compared to <span style = 'color:#1a85ff;'> the Average of the city emissions </span> </b><br>"))+
  facet_wrap(vars(station), ncol = 4)+
      theme_minimal(base_family =  "Lato", base_size = 14) +
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






plots_q2b <- purrr::map(c(
    "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
    "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
    "TCH", "CH4", "NMHC"),

  ~panel_plot(df_monthly, df_madrid, x = "date", y = .x , name = "name")

  ) |> setNames(nm = c(
    "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
    "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
    "TCH", "CH4", "NMHC"))

# purrr::walk2(plots,c(
#     "SO_2", "CO", "NO", "NO_2", "PM25", "PM10", "NOx",
#     "O_3", "TOL", "BEN", "EBE", "MXY", "PXY", "OXY",
#     "TCH", "CH4", "NMHC") ,~ggsave(file.path("fig",paste0(.y,".png")), .x, 
#   dpi = 720, width = 20, height = 10
#   ))



