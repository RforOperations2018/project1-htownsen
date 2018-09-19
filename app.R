# PROJECT 1: HALEY TOWNSEND
# SHINY Dashboard using Beaver County, PA Crash Data (2015)
# Due Sunday, 9/23/18 by Noon

# Instructions:
#Directions: It must include at least four (4) filters, three (3) single numeric based boxes/gauges, 
#one (1) datatable, and three (3) interactive and responsive charts. These elements should be placed 
#throughout a dashboard with at least three (3) pages with an analytical theme or question about the data. 
#On the server side your plots and tables must utilize the reactive function for any and all datasets.
#Your final app must work when deployed to shinyapps.io.


# Loading in the necessary libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(reshape2)
library(plyr)
library(dplyr)
library(plotly)
library(shinythemes)

# Loading in the csv file: Beaver County Crash Data from the WPRDC
df.load = read.csv("crashdata_beaver15.csv", strip.white = T)
# Make all the column headers lower cased, so they are easier to type
names(df.load) <- tolower(names(df.load))

pdf(NULL)

# Shiny Dashboard Section which defines the UI
# Starting with the header
header <- dashboardHeader(title ="Beaver County, PA Car Crash Data for 2015")

# Shiny sidebar
sidebar <-dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Distractions", icon = icon("mobile"), tabName = "dist", badgeLabel = "new page", badgeColor = "green"),
    menuItem("Weather", icon = icon("umbrella"), tabName = "weath", badgeLabel = "new page", badgeColor = "green"),
    menuItem("Behavior", icon = icon("beer"), tabName = "beh", badgeLabel = "new page", badgeColor = "green")))

  

body <- dashboardBody(tabItems())



# The UI just creates the combined dashboard page. 
ui <- dashboardPage(header, sidebar, body)
      


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   #output$distPlot <- renderPlot({

}

# Run the application 
shinyApp(ui = ui, server = server)

