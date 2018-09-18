# PROJECT 1: HALEY TOWNSEND
# SHINY Dashboard using Beaver County, PA Crash Data (2015)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(reshape2)
library(plyr)
library(dplyr)
library(plotly)
library(shinythemes)

# Shiny Dashboard Section
header <- dashboardHeader()

sidebar <-dashboardSidebar(sidebarMenu())

body <- dashboardBody(tabItems())



# The UI just creates the combined dashboard page. 
ui <- dashboardPage(header, sidebar, body)
      


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   #output$distPlot <- renderPlot({

}

# Run the application 
shinyApp(ui = ui, server = server)

