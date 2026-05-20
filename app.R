library(shiny)
library(bslib)
library(shinycssloaders)

source("plot_functions.R")
source("plot_descriptions.R")

ui <- page_navbar(
  title = "Madrid Air Quality Monitor",
  sidebar = sidebar(
    title = "Inputs",
    sliderInput("years",
                "Years until:",
                min = 2001L,
                max = 2018L,
                value = 2010,
                step = 1L,
                animate = TRUE)
  ),nav_spacer(),
  nav_panel(
    title = "Evolution of Pollution",
    card(
      card_header(q1_title),
      withSpinner(plotOutput("q1")),
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
      withSpinner(plotlyOutput("q3a")),
      q3a_description
    ),
    card(min_height = "1000px",
      card_header(q3b_title),
      withSpinner(plotlyOutput("q3b")),
      q3b_description
    )
    
  ),
  nav_panel(
    title = "Pollution Hotspots",
    card(
      card_header(q4_title),
      withSpinner(plotlyOutput("q4", height = "600px")),
    )
  )
)

server <- function(input, output){
  output$q1 <- renderPlot({
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
    plot_q4(input$years)
  })
  
  output$q2b <- renderPlot(
    {plots_q2b[[input$pollutant]]},
    res = 96
  )
}

shinyApp(ui, server)
