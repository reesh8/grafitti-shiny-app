library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(DataExplorer)
library(funModeling)
library(Hmisc)

navbarPage(windowTitle = "NYC Graffiti",
           title = div(img(src="NYC.jpg",height = 30,
                           width = 50), "Graffiti"), 
           id="gra", theme = shinytheme("journal"), #themeSelector(),
           
           ########1st Panel##############
           
           tabPanel("Spot Map", icon = icon("map-pin"),
                    tags$style(".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                    div(class = "outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width = "100%", height = "100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      
                                      dateRangeInput("daterange", h3("Created Date range:"),
                                                     start  = "2018-09-03",
                                                     end    = "2019-09-02",
                                                     min    = "2018-09-03",
                                                     max    = "2019-09-02",
                                                     format = "mm/dd/yy",
                                                     separator = " - "),
                                      
                                      selectInput("variable", h3("Resolution Action:"),
                                                  c("Cleaning crew dispatched" = "Cleaning crew dispatched", 
                                                    "Cannot identify property owner" = "Cannot identify property owner",
                                                    "Graffiti is intentional." = "Graffiti is intentional.", 
                                                    "Graffiti Reported" = "Graffiti Reported", 
                                                    "Mail returned / wrong address" = "Mail returned / wrong address",
                                                    "Notice of Intent form sent" = "Notice of Intent form sent", 
                                                    "Property Research Required" = "Property Research Required",
                                                    "Public Property (Non-City)" ="Public Property (Non-City)",
                                                    "Site downloaded for cleaning" = "Site downloaded for cleaning",
                                                    "Site to be cleaned." = "Site to be cleaned.",
                                                    "Select All"), multiple=TRUE, selected = "Select All"),
                                      verbatimTextOutput("selected"),
                                      actionLink("empty", "empty choices"),
                                      
                                      checkboxGroupInput("status", label = h3("Status:"),
                                                         choices = c("Open" = "Open",
                                                                     "Closed" = "Closed", 
                                                                     "Pending" = "Pending"),
                                                         selected = "Open"))
                    )
           ),
           
           ########2nd Panel##############
           
           tabPanel("Heat Map", icon = icon("map-signs"),
                    tags$style(".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                    div(class = "outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map2", width = "100%", height = "100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 150, left = 0, right = 40, bottom = "auto",
                                      width = 400, height = "auto",
                                      br(),
                                      radioButtons("selectb", label = strong("Layers"),
                                                   choices = list("Count" = "count", "Close Rate" = "close_rate"), 
                                                   selected = "count"),
                                      
                                      p(strong("Click on a community district to see the time trend of number of graffiti complaints.")),
                                      
                                      plotOutput("month_trend", height = 280),
                                      plotOutput("police_plot", height = 280)
                                      
                        )
                    )
                    
           ),
           
           ########3rd Panel##############
           
           tabPanel("Frequencies", icon = icon("chart-bar"),
                    fluidRow(column(12,
                                    h1("Graffiti Frequencies"),
                                    p("The distribution of reported grafitti incidents vary by borough. There are more incidents in certain boroughs than other ones- this can be both due to the culture amongst different neighborhoods in the borough, but also can be due to other factors such as socioeconomic conditions. It is also important to note that just because there was a grafitti incident reported does not mean that there was actual grafitti. The third option, mapping, will show the different types of incidents that occured."),
                                    br(),
                                    h4("Types of charts"),
                                    p("Select the buttons on the left to choose the type of chart to display. Resolution shows the different types of outcomes after reporting the grafitti incident. Borough shows the frequency of grafitti reports in the different boroughs. Resolution shows the different types of outcomes by color based on the latitude and longitude"))),
                    hr(),
                    fluidRow(sidebarPanel(width = 3,
                                          h4("Resolution Action vs Borough"),
                                          helpText("Chose whether you would like to see the frequency of graffiti in terms of borough, resolution, or mapping."),
                                          radioButtons("type", NULL,
                                                       c("Resolution" = "Resolution",
                                                         "Borough" = "Borough",
                                                         "Mapping" = "Mapping"))),
                             mainPanel(plotOutput("freq", height = 500)))
           ),
           
           
           ########4th Panel##############
           
           tabPanel("About", icon = icon("info"),
                    fluidRow(),
                    fluidRow(),
                    hr()
           )
           
)