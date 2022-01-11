library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

ui <- fluidPage(
  leafletOutput("map", height=880, width=880),
)

server <- function(input, output, session) {
  # Read data
  taxis <- read.csv("../data/yellow_tripdata_2019-01.csv",header=TRUE)
  
  
  # Select only those who payed by credit card
  creditCardTrips <- taxis %>% 
    select(PULocationID, DOLocationID, payment_type, tip_amount) %>% 
    filter(payment_type == 1)
  
  # Calculate mean per Pick-up location ID
  tipByOrig <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$PULocationID), 
                         FUN=mean) %>% 
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipByOrig <- tipByOrig[1:263,]
  
  # Calculate mean per Drop-off location ID
  tipByDest <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$DOLocationID), 
                         FUN=mean) %>%
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipByDest <- tipByDest[1:263,]
  
  # Read map
  shapeData <- spTransform(readOGR("../data",'taxi_zones'), 
                           CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # Add new columns to map
  shapeData$TIP_ORIG <- tipByOrig$x
  shapeData$TIP_DEST <- tipByDest$x
  
  # Create color palette (only for the origin)
  palOrig <- colorNumeric("YlOrRd", domain = shapeData$TIP_ORIG,
                          na.color = "#A9A9A9")
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.983504, 40.697824, zoom = 11) %>%
      addPolygons(data=shapeData, weight = 2, opacity = 1, color = "black",
                  fillColor = ~palOrig(TIP_ORIG), dashArray = "3", 
                  fillOpacity = 0.5, label=~paste0("Zone: ", zone,
                                                   ", Borough: ", borough,
                                                   ", ID: ", LocationID,
                                                   ", Avg. Tip: ", 
                                                   round(TIP_ORIG,2))) %>%
    addLegend(pal = palOrig, values = shapeData$TIP_ORIG, opacity = 0.7, 
              title = NULL , position = "topleft")
  })
}

shinyApp(ui, server)