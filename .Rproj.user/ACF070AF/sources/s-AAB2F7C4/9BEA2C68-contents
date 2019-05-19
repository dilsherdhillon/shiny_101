#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Linear Models"),

   fileInput(inputId = "user_file", label = "Upload File", accept = c(".csv","text/csv")),
   
   #uiOutput(outputId = "File"),
   tableOutput("rawData"),
   
   uiOutput("outcome"), 
   
   uiOutput(outputId = "Fixed_ef"),
   
   actionButton(inputId = "run_model", label = "Run Model"),
   
   plotOutput("residuals"),
   
   verbatimTextOutput("coeff")
   
   #checkboxGroupInput(inputId = "outcome", label = "Outcome Variable", choices = "columns"),
   #checkboxGroupInput(inputId = "ind_vars", label = "Independent Variable", choices = "columns")
   
  
)








# Define server logic required to draw a histogram
server <- function(input, output) {
   model_dta <- eventReactive(input$user_file,{
     read_csv(input$user_file$datapath)
   })
   
   #model_dta1 <- reactive({
    # map_if(model_dta(), is_character,as_factor) %>% tbl_df()
   #})
   
   output$rawData <- renderTable({
     model_dta() %>% head
   })
     output$outcome <- renderUI({
       checkboxGroupInput(inputId = "outcome", label = "Outcome Variable", choices = colnames(model_dta()))
     })
     
     output$Fixed_ef <- renderUI({
       checkboxGroupInput(inputId = "fixed_effects", label = "Fixed Effects", choices = colnames(model_dta()))
     })

     
     model <- eventReactive(input$run_model,{

       lm(as.formula(paste(input$outcome," ~ ",paste(input$fixed_effects,collapse="+"))),data=model_dta())
     })
     
     
     output$coeff <- renderPrint({
       summary(model())
     })
    
     output$residuals <- renderPlot({
       plot(residuals(model()))
       })
}
# Run the application 
shinyApp(ui = ui, server = server)

