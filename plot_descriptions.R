library(htmltools)

q1_title <- HTML("
<h3>Evolution of Pollution in Madrid Over Time</h3>
<br>
")

q2_hotspot_title <- HTML("
<h3>Average NO₂ Hotspots Across Madrid from 2001 to 2018</h3>
<br>
")

q2_barchart_title <- HTML("
<h3>Average NO₂ Levels Across Madrid Stations from 2001 to 2018</h3>
<br>
")

q2_spaghetti_title <- HTML("
<h3>Trends in pollutants air concentrations between 2008 and 2018</h3>
<br>
")

q3a_title <- HTML("
<h3>Evolution of Different Pollutants</h3>
<br>
")

q3b_title <- HTML("
<h3>Correlation Between Pollutants</h3>
<br>
")

q4_title <- HTML("
<h3>Evolution of Pollution Hotspots in Madrid by Pollutant from 2001 to 2018</h3>
<br>
")




q1_description <- HTML("
<br>

<b>Description:</b><br>

This visualization shows the yearly average concentration of different air pollutants in Madrid between 2001 and 2018.  
Because the pollutants are measured on different scales (mg/m³, µg/m³, and PM10 on a separate scale), 
they are divided into three separate plots to improve readability and comparison.  
<br><br>
Some values are missing in the original dataset, but the overall trends and changes in pollutant concentrations 
over the 17-year period remain clearly visible.
<br><br>

<b>How to read the plot:</b><br>

The X-axis represents the year, while the Y-axis shows the average pollutant concentration.  
Each colored line corresponds to a specific pollutant, and the data points represent the yearly average 
value for that pollutant. Hovering over the points reveals the exact values for each year.
<br>
")

q2_hotspot_description <- HTML("
<br>

<b>Description:</b><br>

This visualization displays the spatial distribution of average NO₂ concentrations across
Madrid monitoring stations between 2001 and 2018. The map highlights geographic pollution
hotspots by representing monitoring stations as circles positioned at their recorded
locations throughout the city.  

Marker size represents the average NO₂ concentration measured at each station for the
selected year, while marker color reflects the relative pollution intensity using dynamically
generated concentration categories. Darker colors indicate higher concentrations, whereas
lighter colors correspond to lower pollution levels.  

The visualization allows users to explore how NO₂ pollution patterns vary spatially across
Madrid and identify stations that consistently experience elevated concentrations over time.

<br><br>

<b>How to read the plot:</b><br>

Use the year slider to update the visualization for a specific year between 2001 and 2018.
Each marker corresponds to a monitoring station included in the dataset. Hovering over a
marker displays additional information, including the station name, average NO₂ concentration,
and pollution category for the selected year.  

The legend updates dynamically based on the concentration values observed in the selected
year, allowing the color categories to adapt to changes in pollutant distribution over time.

")

q2_barchart_description <- HTML("

<b>Description:</b><br>

This bar chart compares average NO₂ concentrations across Madrid monitoring stations for
the selected year between 2001 and 2018. Stations are ranked according to their average
NO₂ concentration, allowing direct comparison of pollution levels across locations.  

Bar colors correspond to dynamically generated pollution categories that reflect the
relative intensity of NO₂ concentrations for the selected year. Darker colors indicate
higher pollution concentrations, while lighter colors represent lower concentrations.  

The visualization complements the hotspot map by emphasizing differences in pollutant
magnitude between stations and highlighting locations with consistently elevated NO₂ levels.

<br><br>

<b>How to read the plot:</b><br>

Use the year slider to update the chart for a specific year. Each horizontal bar represents
a monitoring station, while the bar length corresponds to the average NO₂ concentration
measured at that location.  

Hovering over a bar displays additional information, including the station name, average
NO₂ concentration, and pollution category associated with the selected year.

")

q2_spaghetti_description <- HTML("
<b>Description:</b><br>
This plot describe the trends in average monthly concentrations of pollutants in different locations of Madrid between 2008 and 2010. It helps in monitoring the concentrations of pollutants relative to different locations and the mean of the city. 
The colors highlight the location specified in the panel title (red), and the blue color shows the mean of the city.<br>
The vertical postition of lines channels the concentration of the pollutant while the horizontal position the dates. The the vertical position of the line at both ends of the plots indicate the improvement or worsening between w008 and 2010.<br>

<b>How to read the plot:</b><br>

Use the dropdown menu to filter the pollutant to monitor. All of the stations are presented by the grey lines.<br>
Under each panel, the line specific to the highlighted position is colored in red and the city average in blue.<br>


")

q3a_description <- HTML("
<br>

<b>Description:</b><br>

This visualization shows the relative change in pollutant concentrations over time, using 2001 as the baseline year (100%). Each following year is compared to the pollutant value measured in 2001.  
<br><br>
Values above the dashed reference line indicate higher concentrations compared to 2001, 
while values below the line indicate a decrease. Despite some missing measurements in the dataset, 
the overall continuity and long-term trends remain clearly visible.  
<br><br>
Only pollutants with a valid measurement available for the base year (2001) are included in this visualization.
<br><br>

<b>How to read the plot:</b><br>

The X-axis represents the year, while the Y-axis shows the relative pollutant concentration as a percentage compared to 2001.  
<br><br>
Each colored line represents a specific pollutant and illustrates how its concentration changed between 2001 and 2018. 
For example, TCH remained relatively stable over time, staying close to the 100% baseline, 
while most other pollutants show a significant decrease throughout the observed period.
<br>
")

q3b_description <- HTML("
<br>

<b>Description:</b><br>

This visualization shows the correlation between the different air pollutants measured in Madrid between 2001 and 2018. 
The correlations were calculated using the yearly average concentration values of each pollutant.  
<br><br>
Dark green tiles indicate a strong positive correlation, meaning that the concentrations of two pollutants tend to increase or decrease together.
Dark red tiles represent a strong negative correlation, meaning that when the concentration of one pollutant increases, the other tends to decrease. 
White or lighter-colored areas indicate weak or insignificant correlations.  
<br><br>
The correlations were computed using all available overlapping yearly measurements between pollutant pairs. 
Therefore, pollutants do not need to contain valid values for every single year in order to appear in the visualization.
<br><br>

<b>How to read the plot:</b><br>

Because the correlation matrix is symmetric, only the lower half of the heatmap is displayed in order to avoid duplicated information.  
<br><br>
The main diagonal contains only values equal to 1.00, since every pollutant is perfectly correlated with itself. 
Each remaining tile represents the Pearson correlation coefficient between two pollutants over the 2001–2018 period. 
The numerical value displayed inside each tile indicates the strength and direction of the correlation.
<br>
")

q4_description <- HTML("

<b>Description:</b><br>

This interactive visualization displays the spatial evolution of pollution hotspots across
Madrid monitoring stations between 2001 and 2018. Users can explore different pollutants
using the dropdown menu, allowing comparison of how pollutant distributions vary both
spatially and temporally throughout the city.  

Marker size represents the average concentration recorded at each monitoring station for
the selected pollutant and year, while marker color reflects relative pollution intensity
using dynamically generated concentration categories. Darker colors correspond to higher
pollution concentrations, whereas lighter colors indicate lower concentration levels.  

The visualization enables users to identify recurring pollution hotspots, observe how
pollutant concentrations shift over time, and compare spatial patterns between different
air pollutants measured in Madrid.

<br><br>

<b>How to read the plot:</b><br>

Use the year slider to display pollution measurements for a specific year between 2001
and 2018. The pollutant dropdown menu allows users to switch between available pollutants
included in the dataset.  

Each marker corresponds to a monitoring station location. Hovering over a marker displays
additional information, including the selected pollutant, station name, average
concentration value, and dynamically assigned pollution category for the selected year.

")