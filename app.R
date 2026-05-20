library(shiny)
source("plot_functions.R")
source("plot_descriptions.R")

ui <- fluidPage((titlePanel("Group 22 visualization")),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("years",
                               "Years until:",
                               min = 2001,
                               max = 2018,
                               value = 2010,
                               step = 1,
                               animate = TRUE)),
                  mainPanel("Research questions",
                    tabsetPanel(
                      tabPanel("Q1",
                        q1_title,
                        plotlyOutput("q1"),
                        q1_description
                      ),
                      tabPanel("Q2",
                        q2_hotspot_title,
                        leafletOutput("q2a_hotspot"),
                        q2_hotspot_description,
                        q2_barchart_title,
                        plotlyOutput("q2a_barchart"),
                        q2_barchart_description,
                        q2_spaghetti_title,
                        q2_spaghetti_description
                      ),
                      tabPanel("Q3",
                        q3a_title,
                        plotlyOutput("q3a"),
                        q3a_description,
                        q3b_title,
                        plotlyOutput("q3b"),
                        q3b_description
                      ),
                      tabPanel(
                        "Q4",
                        q4_title,
                        plotlyOutput("q4", height = "600px"),
                        q4_description
                      )
                    )
                )
            )
)

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
    plot_q4(input$years)
  })
}



shinyApp(ui = ui,server=server)