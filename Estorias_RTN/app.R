#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  sidebarLayout(
    sidebarPanel(
      h5("use case - embed a pdf user guide in the app - embed as a local pdf or from web URL")
    ), 
    mainPanel(
      tabsetPanel(
        # using iframe along with tags() within tab to display pdf with scroll, height and width could be adjusted
        tabPanel("Reference", 
                 tags$iframe(style="height:400px; width:100%; scrolling=yes", 
                             src="https://www.tesourotransparente.gov.br/ckan/dataset/ab56485b-9c40-4efb-8563-9ce3e1973c4b/resource/aaf7f695-9370-46ae-9d7c-1d5cb28b1db4/download/Metadados-Serie-RTN.pdf")),
        tabPanel("Summary"),
        tabPanel("Plot")
      )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)

