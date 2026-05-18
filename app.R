library(shiny)
source("plot_functions.R")

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
                    tabPanel("Q1", plotOutput("q1")),
                    tabPanel('Q2', plotOutput("q2")),
                    tabPanel('Q3', plotlyOutput("q3a"),plotlyOutput("q3b")),
                    tabPanel('Q4', plotOutput("q4"))
                    )
                )
            )
)

server <- function(input, output){
  output$q1 <- renderPlot({
    plot_q1(input$years)
  })
  output$q3a <- renderPlotly({
    plot_q3a(max_year = input$years)
  })
  output$q3b <- renderPlotly({
    plot_q3b()
  })
}



shinyApp(ui = ui,server=server)