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

# Rename cell_phone indicator to "Yes" and "No"
df.load$cell_phone = mapvalues(df.load$cell_phone, from = c(0, 1), to = c("No", "Yes"))

# Rename deer_related indicator to "Yes" and "No"
df.load$deer_related = mapvalues(df.load$deer_related, from = c(0, 1), to = c("No", "Yes"))

# Rename weather events from numerics to actual levels
df.load$weather = mapvalues(df.load$weather, from = c(1, 2, 3, 4, 5), 
                            to = c("No Adverse Conditions", "Rain", "Sleet/Hail", "Snow", "Fog"))

# Rename aggressive_driving indicator to "Yes" and "No"
df.load$aggressive_driving = mapvalues(df.load$aggressive_driving, from = c(0, 1), to = c("No", "Yes"))

# Rename alcohol_related indicator to "Yes" and "No"
df.load$alcohol_related = mapvalues(df.load$alcohol_related, from = c(0, 1), to = c("No", "Yes"))

# Rename speeding indicator to "Yes" and "No"
df.load$speeding = mapvalues(df.load$speeding, from = c(0, 1), to = c("No", "Yes"))

# Rename tailgating indicator to "Yes" and "No"
df.load$tailgating = mapvalues(df.load$tailgating, from = c(0, 1), to = c("No", "Yes"))

pdf(NULL)

############################################################################################################################
# Shiny Dashboard Section which defines the UI
# Starting with the header
header <- dashboardHeader(title ="Crash Dashboard")

# Shiny sidebar
sidebar <-dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Distractions", icon = icon("mobile"), tabName = "dist", badgeLabel = "new page", badgeColor = "green"),
    menuItem("Weather", icon = icon("umbrella"), tabName = "weath", badgeLabel = "new page", badgeColor = "green"),
    menuItem("Behaviors", icon = icon("beer"), tabName = "beh", badgeLabel = "new page", badgeColor = "green"),
    menuItem("General", icon = icon("info"), tabName = "gen", badgeLabel = "new page", badgeColor = "yellow"),
    
    # Filter/Input 1: Day of the Week Selection
    pickerInput("daySelect",
                label = "Day of the Week:",
                choices = day.options,
                options = list(`actions-box` = TRUE),
                # Select Saturday and Sunday as default
                selected = day.options[6:7],
                multiple = TRUE),
    
    # Filter/Input 2: Slider for number of cars involved
    sliderInput("autoSelect",
                "Number of Cars Involved:",
                min = min(df.load$automobile_count, na.rm = T),
                max = max(df.load$automobile_count, na.rm = T),
                value = c(min(df.load$automobile_count, na.rm = T), max(df.load$automobile_count, na.rm = T)),
                step = 1)
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
            box(title = "Weather Conditions", status = "primary", plotlyOutput("plotweath"), width=8),
            # Filter/Input 3: Weather Conditions
            box(width=4,
              title = "Inputs", status = "warning",
              checkboxGroupInput("weathSelect", "Select Weather Events:", choices= c(
                "No Adverse Conditions" = "No Adverse Conditions",
                "Rain" = "Rain",
                "Snow" = "Snow",
                "Fog" = "Fog",
                "Sleet/Hail" = "Sleet/Hail"),
                selected = c("No Adverse Conditions", 
                             "Rain",
                             "Snow")
              )
            )
          )
  ),
  tabItem("beh",
          fluidRow(
            box(title = "Data Table: Option to Filter by Behaviors", status = "primary", DT::dataTableOutput("table"), width = 8),
            box(width=4,
                title="Inputs", status = "warning",
                tags$b("Select Behavior(s):"),
                # Many new single checkboxes, pulling from different columns
                checkboxInput("aggSelect",  "Aggresive Driving"),
                checkboxInput("beerSelect", "Alcohol Involved"),
                checkboxInput("speedSelect", "Speeding"),
                checkboxInput("tailSelect", "Tailgating")
          )
          )
),
  tabItem("gen",
          fluidPage(
            box(title = "General Crash Information", status = "primary", plotlyOutput("plotgen", height = 500), width=12)
          )
          )
)
)


# The UI just creates the combined dashboard page. 
ui <- dashboardPage(header, sidebar, body)
      


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # First Reactive Group, general, just includes two sidebar inputs 
  dfInput <- reactive({
    df <- df.load %>% 
      # Day of the week filter
      filter(day_of_week %in% input$daySelect) %>%
      # Slider for number of cars filter
      filter(automobile_count >= input$autoSelect[1] & automobile_count <= input$autoSelect[2])
    return(df)
  })
  
  # Second Reactive Group for Weather Page, includes main filters on sidebar
  wInput <- reactive({
    df <- df.load %>% 
      # Day of the week filter
      filter(day_of_week %in% input$daySelect) %>%
      # Slider for number of cars filter
      filter(automobile_count >= input$autoSelect[1] & automobile_count <= input$autoSelect[2]) %>%
      filter(weather %in% input$weathSelect)
    return(df)
  })
  
  # Third Reactive Group for Behaviors Page, includes main filters on sidebar
  bInput <- reactive({
    df <- df.load %>% 
      # Day of the week filter
      filter(day_of_week %in% input$daySelect) %>%
      # Slider for number of cars filter
      filter(automobile_count >= input$autoSelect[1] & automobile_count <= input$autoSelect[2])
    
    # Individual if statements for each single checkbox (indicator columns) for behaviors
    if (input$aggSelect==T) {
        df <- df %>% filter(aggressive_driving=="Yes")
      }
    if (input$beerSelect==T) {
      df <- df %>% filter(alcohol_related=="Yes")
    }
    if (input$speedSelect==T) {
      df <- df %>% filter(speeding=="Yes")
    }
    if (input$tailSelect==T) {
      df <- df %>% filter(tailgating=="Yes")
    }
    return(df)
  })
  
  # PLOT 1: Cell Phones Plot
  output$plotphone <- renderPlotly({
    d <- dfInput()
    ggplot(d, aes(x = cell_phone, color = cell_phone, fill = cell_phone)) + 
      geom_bar() + 
      theme(legend.position="none") +
      xlab("Cell Phone Related") +
      ylab("Number of Observations in the Data")
  })
  
  # PLOT 2: Deer Plot
  output$plotdeer <- renderPlotly({
    d <- dfInput()
    ggplot(d, aes(x = deer_related, color = deer_related, fill = deer_related)) + 
      geom_bar() + 
      theme(legend.position="none") +
      xlab("Deer Related Crash") +
      ylab("Number of Observations in the Data")
  })
  # Value Box 1: Total cars
  output$crashes <- renderValueBox({
    d <- dfInput()
    num <- sum(d$automobile_count, na.rm = T)
    valueBox(subtitle = "Total Automobiles", value = num, icon = icon("car"), color = "maroon")
  })
  
  # Value Box 2: Total Deaths
  output$deaths <- renderValueBox({
    d <- dfInput()
    num2 <- sum(d$fatal_count, na.rm = T)
    valueBox(subtitle = "Total Fatalities", value = num2, icon = icon("exclamation"), color = "orange")
  })
  
  # Info Box 1: Bikes
  output$bikes <- renderInfoBox({
    d <- dfInput()
    num3 <- sum(d$bicycle_count, na.rm = T)
    infoBox("Total Bikes", value = num3, subtitle = paste(nrow(d), "total crashes"), icon = icon("bicycle"), color = "purple")
  })
  
  # PLOT 3: Weather Plot
  output$plotweath <- renderPlotly({
    w <- wInput()
    ggplot(w, aes(x = weather, color = weather, fill = weather)) + 
      geom_bar() + 
      theme(legend.position="none") +
      xlab("Weather Condition(s)") +
      ylab("Number of Observations in the Data")
  })
  
  # Data Table filtered by behaviors
  output$table <- DT::renderDataTable({
    subset(bInput(), select = c(day_of_week, automobile_count, aggressive_driving, alcohol_related, speeding, tailgating))
  })
  
  # PLOT 4: General Line Plot
  output$plotgen <- renderPlotly({
    d <- dfInput()
    ggplot(d, aes(x = person_count, y = automobile_count)) + 
      geom_count() + 
      theme(legend.position="none") +
      xlab("Number of People Involved in Crash") +
      ylab("Number of Automobiles Involved in Crash")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

