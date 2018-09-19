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
# Rename factors: change day of week from ints to names
df.load$day_of_week = mapvalues(df.load$day_of_week, from = c(1, 2, 3, 4, 5, 6, 7), to = c("Sunday", "Monday", "Tuesday", 
                                                         "Wednesday", "Thursday", "Friday", "Saturday"))

day.options = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

pdf(NULL)

# Shiny Dashboard Section which defines the UI
# Starting with the header
header <- dashboardHeader(title ="Crash Dashboard")

# Shiny sidebar
sidebar <-dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Distractions", icon = icon("mobile"), tabName = "dist", badgeLabel = "new page", badgeColor = "green"),
    menuItem("Weather", icon = icon("umbrella"), tabName = "weath", badgeLabel = "new page", badgeColor = "green"),
    menuItem("Behavior", icon = icon("beer"), tabName = "beh", badgeLabel = "new page", badgeColor = "green"),
    
    # Day of the Week Selection: Filter/Input 1
    pickerInput("daySelect",
                label = "Day of the Week:",
                choices = day.options,
                options = list(`actions-box` = TRUE),
                # Select Saturday and Sunday as default
                selected = day.options[6:7],
                multiple = TRUE)
    
    # # Birth Selection
    # sliderInput("birthSelect",
    #             "Birth Year:",
    #             min = min(starwars.load$birth_year, na.rm = T),
    #             max = max(starwars.load$birth_year, na.rm = T),
    #             value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)),
    #             step = 1)
  )
)


body <- dashboardBody(tabItems(
  tabItem("dist",
          fluidRow(
            valueBoxOutput("crashes"),
            infoBoxOutput("bikes"),
            valueBoxOutput("deaths") # fatal_count
          ),
          fluidRow(
            tabBox(title = "Distractions",
                   width = 12,
                   tabPanel("Cell Phones", plotlyOutput("plotphone")),
                   tabPanel("Deer", plotlyOutput("plotdeer")))
          )
  ),
  tabItem("weath",
          fluidPage(
            box(title = "Selected Weather-Related Crashes", DT::dataTableOutput("table"), width = 12))
  ),
  tabItem("beh",
          fluidRow(
            valueBoxOutput("alcohol")
          ),
          fluidRow(
            tabBox(title = "Behaviors",
                   width = 12
              
            )
          )
          )
)
)


# The UI just creates the combined dashboard page. 
ui <- dashboardPage(header, sidebar, body)
      


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   #output$distPlot <- renderPlot({

}

# Run the application 
shinyApp(ui = ui, server = server)

