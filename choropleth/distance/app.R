library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

ui <- fluidPage(
  titlePanel("Visualizing trip lengths per taxi zone"),
  fluidRow(
    column(4,
           sidebarPanel(
             width=400,
             radioButtons("PU_DO", "Show distance values by:",
                          c("Origin" = "orig", "Destination" = "dest")))
    ),
    column(3,
           sidebarPanel(
             width=300,
             radioButtons("MN_MD", "Statistic:",
                          c("Mean distance" = "mn", "Median distance" = "md")))
    )
  ),
  leafletOutput("map", height=700, width=880),
)

server <- function(input, output, session) {
  
  # Read data
  taxis <- read.csv("../../yellow_tripdata_2019-01.csv",header=TRUE)
  
  
  # Select only those who payed by credit card
  subsetTrips <- taxis %>% 
    select(PULocationID, DOLocationID, trip_distance)
  
  
  
  
  # Calculate mean per Pick-up location ID
  tripMnOrig <- aggregate(x=subsetTrips$trip_distance, 
                         by=list(subsetTrips$PULocationID), 
                         FUN=mean) %>% 
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tripMnOrig <- tripMnOrig[1:263,]
  
  # Calculate mean per Drop-off location ID
  tripMnDest <- aggregate(x=subsetTrips$trip_distance, 
                         by=list(subsetTrips$DOLocationID), 
                         FUN=mean) %>%
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tripMnDest <- tripMnDest[1:263,]
  
  # Calculate median per Pick-up location ID
  tripMdOrig <- aggregate(x=subsetTrips$trip_distance, 
                         by=list(subsetTrips$PULocationID), 
                         FUN=median) %>% 
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tripMdOrig <- tripMdOrig[1:263,]
  
  # Calculate median per Drop-off location ID
  tripMdDest <- aggregate(x=subsetTrips$trip_distance, 
                         by=list(subsetTrips$DOLocationID), 
                         FUN=median) %>%
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tripMdDest <- tripMdDest[1:263,]
  
  
  # Read map
  shapeData <- spTransform(readOGR("../../data",'taxi_zones'), 
                           CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # Add new column to map
  readButtonPUDO <- reactive(input$PU_DO)
  readButtonMNMD <- reactive(input$MN_MD)
  
  # Create color palette (only for the origin)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.983504, 40.697824, zoom = 11)
  })
  
  observe({
    buttonPUDO <- readButtonPUDO()
    buttonMNMD <- readButtonMNMD()
    
    # Set the corresponding values depending on the selected buttons
    if (buttonPUDO == "orig" & buttonMNMD == "mn") {
      shapeData$TRIPD <- tripMnOrig$x
    } else if (buttonMNMD == "mn") {
      shapeData$TRIPD <- tripMnDest$x
    } else if (buttonPUDO == "orig") {
      shapeData$TRIPD <- tripMdOrig$x
    } else {
      shapeData$TRIPD <- tripMdDest$x
    }
    
    lims <- c(2, 4, 6, 8, 12)
    # Create bins for color map
    bins <- c(round(quantile(shapeData$TRIPD, 0, na.rm = TRUE)-0.005,2),
              round(lims,2),
              round(quantile(shapeData$TRIPD, 1, na.rm=TRUE)+0.005,2))
    
    pal <- colorBin("BuPu", domain = shapeData$TRIPD,
                    na.color = "#A9A9A9",
                    bins = bins)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data=shapeData, weight = 2, opacity = 1, color = "white",
                  fillColor = ~pal(shapeData$TRIPD), dashArray = "3", fillOpacity = 0.5,
                  label=paste0("Zone: ", shapeData$zone, ", Borough: ",
                               shapeData$borough, ", ID: ",shapeData$LocationID,
                               "Avg. Dist.: ", round(shapeData$TRIPD,2))) %>%
      clearControls() %>%
      addLegend(pal = pal, values = shapeData$TRIPD, opacity = 0.7, 
                title = NULL, position = "topleft")
  })
}

shinyApp(ui, server)