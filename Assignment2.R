# Importing necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)

# Set default location
setwd("E:\\Academics\\Data science Monash\\Semester 1\\FIT 5147 Exploration\\Assignment 2 due 20170415\\Work")

# Fetch the CSV data & updating the column name of column "year"
excel_data <- read.csv("assignment-02-formatted-data.csv")
colnames(excel_data)[1] <- "year"

# Remove the % symbol from the bleaching data to make it numeric
excel_data$bleaching <- as.numeric(sub("%", "",excel_data$bleaching,fixed=TRUE))

# Plot static visualisation that shows for each kind of coral and for each site how the bleaching varies from year to year
ggplot(excel_data, aes(year, as.numeric(bleaching))) +
  geom_bar(stat = "identity") +               # Plot bar graph
  facet_grid(reorder(site,latitude)~type) +   # Sites should be ordered by latitude
  geom_smooth(span = 0.8, method = "lm") +    # Fit smoothers to the data
  labs(x = "Year", y = "Bleach")

# Shiny for interactive visualisation and Leaflet map
shinyApp(
  
  # Design UI in Shiny
  ui = fluidPage(

    # Output with seperate tabs for interactive visualisation and Leaflet map
    tabsetPanel(

      # First tab: Create UI for interactive visualisation
      tabPanel("Shiny interactive visualisation", 
               br(),
               sidebarLayout(
                 sidebarPanel(
                   
                   # Drop down list to select type of coral
                   selectInput("coral_type_input", "Coral type:",
                               choices = c(levels(excel_data$type))),
                   
                   # Drop down list to select site of coral
                   selectInput("coral_site_input", "Site:",
                               choices = c(levels(excel_data$site))),
                   
                   # Radio button to select type of smoother
                   radioButtons("smoother_input", "Smoother type:",
                                choices = c("lm","glm","gam","loess"))
                 ),
                 
                 # Panel to display the graph
                 mainPanel(
                   textOutput("no_graph_message"),
                   plotOutput("coral_data_graph"),
                   dataTableOutput("data_selected")
                 )
               )  
      ),
      
      # Second tab: Create UI for leaflet map
      tabPanel("Leaflet interactive map",
               br(),
               
               # Specify the area to show the map
               column(6,leafletOutput("map", height="500px")),
               
               # Specify the area to show the graph
               column(6,plotOutput("plot", height="500px"))
      )
      
    )
  ),
  
  # Design logic in Shiny to show desired output
  server = function(input, output) {
    
    # Filter data in filtered_data() as per selected options from the drop down lists
    filtered_data <- reactive({
      excel_data %>% filter(
        type == input$coral_type_input,
        site == input$coral_site_input)
    })
    
    # Display "null data" message if no data in filtered_data()
    output$no_graph_message <- renderText({
      if (nrow(filtered_data()) == 0) {
        "Sorry, data available for this site is null"}
    })
    
    # Plot bar graph for selected coral site & type
    output$coral_data_graph <- renderPlot({

      if (!(nrow(filtered_data()) == 0)) {
        ggplot(filtered_data(), aes(year, bleaching)) + 
          geom_bar(stat="identity", fill = "#FF6666") +
          scale_y_continuous(labels=function(x) paste0(x,"%")) + 
          geom_smooth(span = 0.8, 
                      method=input$smoother_input,   # Show smoother as per on graph as per user selection
                      lwd=0.7) +
          labs(x = "Year", y = "Bleach")
      }
    })
    

    # build data of location details of 8 sites
    data=data.frame(x=c(unique(excel_data$longitude)), 
                    y=c(unique(excel_data$latitude)), 
                    id=c(levels(excel_data$site)))
    
    # Fetching realtime click data
    data_of_click <- reactiveValues(clickedMarker=NULL)
    
    # Leaflet map with 8 sites marked on the map
    output$map <- renderLeaflet({
      leaflet() %>% 
        setView(lng=145.715 , lat =-16.091, zoom=5) %>%             # Set default view of the map
        addTiles(options = providerTileOptions(noWrap = TRUE)) %>%  
        addCircleMarkers(data=data, 
                         ~x , ~y, 
                         layerId=~id, 
                         radius=8 , 
                         color="black",  
                         fillColor="red", 
                         stroke = TRUE, 
                         fillOpacity = 0.8, 
                         label = ~id,
                         labelOptions = labelOptions(noHide = T))   # Formatted the UI of the site marker on map
    })
    
    # store the click position on the map
    observeEvent(input$map_marker_click,{
      data_of_click$clickedMarker <- input$map_marker_click
    })
    
    # A line graph showing bleaching of each coral over the years at respective selected site
    output$plot=renderPlot({
      
      # Storing the site id (name) of clicked location
      my_place=data_of_click$clickedMarker$id
      
      # Setting default location as 'Site01'
      if (length(my_place) == 0) {
        my_place <- "site01"
      }
      
      # Filtering data as per selected site
      if (my_place %in% c(levels(excel_data$site))) {
        filtered <- excel_data %>% filter(
          site == my_place
        )
        
        # Generate line graph next to map for the respective site clicked in the map
        ggplot(filtered, aes(year, bleaching)) + 
          geom_line(aes(color = type),size=2) +
          scale_y_continuous(labels=function(x) paste0(x,"%")) +
          ggtitle(paste("Graph of bleach % at", my_place, sep = " ")) +
          labs(x = "Year", y = "Bleach")
      }
    })
  }
)
