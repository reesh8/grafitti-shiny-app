library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(lubridate)
library(scales)
library(sf)
library(tigris)
library(nycgeo)

function(input, output, session) {
  
  # cleaned_NYPD_df <- read_csv("./output/cleaned_NYPD_Arrests.csv")
  # save(cleaned_NYPD_df, file = "./temp_app/cleaned_NYPD_Arrests.Rdata")
  #load("./temp_app/DSNY_Graffiti_Tracking.RData")
  #load("./temp_app/DSNY_Graffiti_Tracking.RData")
  load("DSNY_Graffiti_Tracking.RData")
  load("cleaned_NYPD_Arrests.Rdata")
  
  nhyc_cd_data <- cd_sf
  
  gra_df = gra_df %>% 
    janitor::clean_names() %>% 
    filter(!is.na(latitude) &  !is.na(longitude) & !is.na(city_council_district)) %>%
    mutate(cd_id = str_extract(community_board, "[[:digit:]]+"),
           borough_id = case_when(
             borough == "MANHATTAN" ~ "1",
             borough == "BRONX" ~ "2",
             borough == "BROOKLYN" ~ "3",
             borough == "QUEENS" ~ "4",
             borough == "STATEN ISLAND" ~ "5"),
           borough_cd_id = str_c(borough_id, cd_id))
  
  temp_df =  gra_df %>% 
    filter(status == "Closed") %>% 
    group_by(borough_cd_id, status) %>% 
    summarise(close_count = n()) %>%
    left_join(gra_df %>% 
                group_by(borough_cd_id) %>% 
                summarise(ttl_count = n()), by = "borough_cd_id") %>%  
    mutate(close_rate = close_count/ttl_count) %>%
    ungroup() 
  
  map_data <- geo_join(nhyc_cd_data, temp_df, "borough_cd_id",  "borough_cd_id", how = "inner") %>% 
    mutate(ttl_count = as.numeric(ttl_count), close_rate = as.numeric(close_rate))
  
  #############tab 1###################
  choices <- c("Select All", "Cleaning crew dispatched", 
               "Cannot identify property owner",
               "Graffiti is intentional.", 
               "Graffiti Reported", 
               "Mail returned / wrong address",
               "Notice of Intent form sent", 
               "Property Research Required",
               "Public Property (Non-City)",
               "Site downloaded for cleaning",
               "Site to be cleaned.")
  
  ######### Map Data ########
  
  Graffiti <- read.csv("DSNY_Graffiti_Tracking.csv")
  Graffiti_clean <- Graffiti %>% 
    filter(is.na(LATITUDE) == FALSE & is.na(LONGITUDE) == FALSE) %>% 
    filter(is.na(COMMUNITY_BOARD) == FALSE & is.na(CREATED_DATE) == FALSE & is.na(STATUS) == FALSE &
             is.na(RESOLUTION_ACTION) == FALSE & is.na(BOROUGH) == FALSE & is.na(POLICE_PRECINCT) == FALSE)
  
  Graffiti_grouped <- Graffiti_clean
  Graffiti_grouped$RESOLUTION_ACTION <- as.character(Graffiti_grouped$RESOLUTION_ACTION)
  Graffiti_grouped$RESOLUTION_ACTION[Graffiti_grouped$RESOLUTION_ACTION == "Cleaning crew dispatched.  Owner refused."] <- "Cleaning crew dispatched"
  Graffiti_grouped$RESOLUTION_ACTION[Graffiti_grouped$RESOLUTION_ACTION == "Cleaning crew dispatched.  Property cleaned."] <- "Cleaning crew dispatched"
  Graffiti_grouped$RESOLUTION_ACTION[Graffiti_grouped$RESOLUTION_ACTION == "Cleaning crew dispatched. No graffiti on property."] <- "Cleaning crew dispatched"
  Graffiti_grouped$RESOLUTION_ACTION[Graffiti_grouped$RESOLUTION_ACTION == "Cleaning crew dispatched.  Cannot locate property."] <- "Cleaning crew dispatched"
  Graffiti_grouped$RESOLUTION_ACTION[Graffiti_grouped$RESOLUTION_ACTION == "Further research is necessary to identify an owner for the property."] <- "Cannot identify property owner"
  Graffiti_grouped$RESOLUTION_ACTION[Graffiti_grouped$RESOLUTION_ACTION == "Notice of Intent to Clean and Forever graffiti free form sent"] <- "Notice of Intent form sent"
  
  observe({
    if ("Select All" %in% input$variable) {
      # choose all the choices _except_ "Select All"
      selected_choices <- setdiff(choices, "Select All")
      updateSelectInput(session, "variable", selected = selected_choices)
    }
  })
  observe({
    if(input$empty != 0){
      updateSelectInput(session, "variable", selected = "")
    }
  })
  df <- reactive({
    Graffiti_grouped %>%
      filter(as.Date(CREATED_DATE, "%m/%d/%Y") >= as.Date(input$daterange[1], "%m/%d/%Y") & 
               as.Date(CREATED_DATE, "%m/%d/%Y") <= as.Date(input$daterange[2], "%m/%d/%Y")) %>%
      filter(as.character(STATUS) == input$status)%>%
      filter(RESOLUTION_ACTION %in% input$variable) 
    
  })
  
  
  output$map <- renderLeaflet({
    m <- leaflet(data = df()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~LONGITUDE, 
                       lat = ~LATITUDE,
                       popup = paste("Police precinct:", df()$POLICE_PRECINCT, "<br>",
                                     "Status:", df()$STATUS, "<br>",
                                     "Closed Date:", df()$CLOSED_DATE), radius = 5, 
                       color = ~ ifelse(df()$STATUS == 'Open', 'red', 'blue'), 
                       clusterOptions = markerClusterOptions())
    m
  })
  #############tab 2 ###############
  
  output$map2 <- renderLeaflet({
    
    if(input$selectb == "count"){
      bins <- c(0, 200, 400, 600, 800, 1000, 1400, 1800)
      pal <- colorBin("Reds", domain = map_data$ttl_count, bins = bins)
      popup1 = paste0('<strong>Count: </strong><br>', map_data$ttl_count,
                      '<br><strong>Close Rate: </strong><br>', round(map_data$close_rate,2),
                      '<br><strong>Name: </strong><br>', map_data$cd_name)
      
      map_data %>%
        st_transform(., "+init=epsg:4326") %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(popup = popup1,
                    layerId=~borough_cd_id,
                    #label = ~ttl_count,
                    fillColor = ~pal(ttl_count),
                    color = 'grey', 
                    fillOpacity = .6,
                    weight = 1,
                    dashArray = "3") %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        addLegend(pal = pal, values = ~bins, opacity = 0.6, title = "Number of Complaints",
                  position = "bottomright")
    } else{
      bins2 <- c(0.3, 0.5, 0.7, 0.9, 1)
      pal2 <- colorBin("Blues", domain = map_data$close_rate, bins = bins2)
      popup2 = paste0('<strong>Count: </strong><br>', map_data$ttl_count,
                      '<br><strong>Close Rate: </strong><br>', round(map_data$close_rate,2), 
                      '<br><strong>Name: </strong><br>', map_data$cd_name)
      
      map_data %>%
        st_transform(., "+init=epsg:4326") %>%
        leaflet() %>%
        addTiles() %>%
        addPolygons(popup = popup2,
                    layerId=~borough_cd_id,
                    #label = ~ttl_count,
                    fillColor = ~pal2(close_rate),
                    color = 'grey', 
                    fillOpacity = .6,
                    weight = 1,
                    dashArray = "3") %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        addLegend(pal = pal2, values = ~bins2, opacity = 0.6, title = "Close rate",
                  position = "bottomright")
    }
  })
  
  ##############tab 3###############
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- fait
    hful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$ggplot1 <- renderPlot({
    ggplot(data = gra_df) +
      geom_point(mapping = aes(x = longitude, y = latitude, color = resolution_action))
  })
  
  output$freq <- renderPlot({
    if ( input$type == "Mapping" ) {
      ggplot(data = gra_df) +
        geom_point(mapping = aes(x = longitude, y = latitude, color = resolution_action))
    } else if ( input$type == "Resolution") {
      freq(gra_df$resolution_action)
    } else if ( input$type == "Borough") {
      freq(gra_df$borough)
    }
  })
  
  ################################
  observe({
    #whenever map item is clicked, becomes event
    event <- input$map2_shape_click
    if (is.null(event))
      return()
    
    output$month_trend <- renderPlot({
      #returns null if no borough selected
      if (is.na(event$id)) {
        return(NULL)
      }
      
      gra_df %>% mutate(shown_date = format.Date(as.Date(created_date, format = "%m/%d/%Y"), "%b %y"),
                        sort_date = format(as.Date(created_date, format = "%m/%d/%Y"), "%Y%m")) %>%
        mutate(cd_id = as.character(as.numeric(str_extract(city_council_district, "[[:digit:]]+")))) %>%
        filter(borough_cd_id == "101") %>%
        group_by(shown_date, sort_date) %>%
        dplyr::summarize(ttl = n()) %>%
        ungroup() %>%
        arrange(sort_date) %>%
        mutate(shown_date = factor(shown_date, levels = shown_date)) %>%
        ggplot(aes(x=shown_date, y = ttl)) +
        geom_bar(stat="identity", na.rm = TRUE, color="lightblue", fill = "lightblue") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Month", y = "Number of complaints", title = "Number of complaints monthly trend")
    })
  })
  
  observe({
    #whenever map item is clicked, becomes event
    event <- input$map2_shape_click
    if (is.null(event))
      return()
    
    output$police_plot <- renderPlot({
      #returns null if no borough selected
      if (is.na(event$id)) {
        return(NULL)
      }
      
      cleaned_NYPD_df %>% 
        mutate(shown_date = format.Date(ARREST_DATE, "%b %y"),
               sort_date = format.Date(ARREST_DATE, "%Y%m")) %>%
        filter(borough_cd_id == as.numeric(event$id)) %>%
        group_by(shown_date, sort_date) %>%
        dplyr::summarize(ttl = sum(Arrest.Count)) %>%
        ungroup() %>%
        arrange(sort_date) %>%
        mutate(shown_date = factor(shown_date, levels = shown_date)) %>%
        ggplot(aes(x=shown_date, y = ttl)) +
        geom_bar(stat="identity", na.rm = TRUE, color="orange", fill = "orange") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Month", y = "Number of arrests", title = "Number of arrests monthly trend")
    })
  })
}