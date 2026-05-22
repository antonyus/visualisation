library(shiny)
library(bslib)
library(shinycssloaders)

source("plot_functions.R")
source("plot_descriptions.R")

ui <- page_navbar(
  theme = bs_theme(bootswatch = "minty" ,  base_font = font_google("Inter"), heading_font = font_google("DM Sans")),
  title = h1("Madrid Air Quality Monitor"),
  sidebar = sidebar(
    title = "Inputs",
    sliderInput("years",
                "Years until:",
                min = 2001L,
                max = 2018L,
                value = 2010,
                round = T, 
                ticks = T,
                
                step = 1L,
                animate = animationOptions(interval = 4 * 1000, loop = TRUE))
  ),nav_spacer(),
  nav_panel(
    title = "Evolution of Pollution",
    card(
      card_header(q1_title),
      withSpinner(plotlyOutput("q1")),
      q1_description
    )
  ),
  nav_panel(
    title = "Polluted Areas",
    
    card(min_height = "1000px",
         card_header(q2_hotspot_title),
         withSpinner(leafletOutput("q2a_hotspot")),
         q2_hotspot_description
    ),
    card(min_height = "1000px",
         card_header(q2_barchart_title),
         withSpinner(plotlyOutput("q2a_barchart")),
         q2_hotspot_description
    ),
    card(min_height = "1550px",
         card_header(q2_spaghetti_title),
         selectInput("pollutant", "Select a pollutant", choices = pollutants),
         withSpinner(plotOutput("q2b", height = "1000px")),
         q2_spaghetti_description
    )
    
  ),
  nav_panel(
    title = "Pollutant Evolution",
    
    card(min_height = "1000px",
         card_header(q3a_title),
         withSpinner(plotlyOutput("q3a", width = "100%")),
         q3a_description
    ),
    card(min_height = "1000px",
         card_header(q3b_title),
         withSpinner(plotlyOutput("q3b", width = "100%")),
         q3b_description
    )
    
  ),
  nav_panel(
    title = "Pollution Hotspots",
    card(
      card_header(q4_title),
      selectInput(
        inputId = "q4_pollutant",
        label = "Pollutant:",
        choices = c(
          "BEN", "CO", "EBE", "MXY", "NMHC", "NO_2", "NOx", "OXY",
          "O_3", "PM10", "PXY", "SO_2", "TCH", "TOL", "PM25", "NO", "CH4"
        )),
      withSpinner(plotlyOutput("q4", height = "600px")),
      
      q4_description
    )
  ))


server <- function(input, output){
  output$q1 <- renderPlotly({
    plot_q1(input$years)
  })
  output$q2a_hotspot <- renderLeaflet({
    plot_q2a_hotspot(input$years)
  })
  
  output$q2a_barchart <- renderPlotly({
    plot_q2a_barchart(input$years)
  })
  output$q3a <- renderPlotly({
    plot_q3a(max_year = input$years)
  })
  output$q3b <- renderPlotly({
    plot_q3b()
  })
  output$q4 <- renderPlotly({
    plot_q4(input$years, input$q4_pollutant)
  })
  
  output$q2b <- renderPlot(
    {plots_q2b[[input$pollutant]]},
    res = 96
  )
}

shinyApp(ui, server)
