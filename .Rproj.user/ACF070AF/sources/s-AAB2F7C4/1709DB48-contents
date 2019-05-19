#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Weibull Distribution"),
  
  # Sidebar with a slider input for number of bins 

  
      sliderInput(inputId = "Number",
                  label = "Number of data points to generate:",
                  min = 1,
                  max = 2000,
                  value = 30),
      sliderInput(inputId = "Scale",
                  label = "Scale parameter:",
                  min = 1,
                  max = 10,
                  value = 4),
      sliderInput(inputId = "Shape",
                  label = "Shape parameter:",
                  min = 1,
                  max = 10,
                  value = 4),
  
 
   mainPanel(
     plotOutput("distPlot")
  ),
  verbatimTextOutput(outputId = "Summary")
  
    )






# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({
    rweibull(input$Number,input$Scale,input$Shape)
  })
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
   # x    <- rweibull(input$Number,input$Scale,input$Shape) 
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw density
    plot(density(data()), main = "weibull")
    
  })
  
  output$Summary <- renderPrint({
   # z  <- rweibull(input$Number,input$Scale,input$Shape)
    summary(data())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

