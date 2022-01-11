library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

ui <- fluidPage(
  radioButtons("PU_DO", "Show tip average per:",
               c("Origin" = "orig",
                 "Destination" = "dest")),
  leafletOutput("map", height=880, width=880),
)

server <- function(input, output, session) {

  # Read data
  taxis <- read.csv("../../yellow_tripdata_2019-01.csv",header=TRUE)
  
  
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
  
  # Add new column to map
  readButton <- reactive(input$PU_DO)
  
  palet <- reactive(colorNumeric("YlOrRd", domain = shapeData$TIP, 
                               na.color = "#A9A9A9"))
  
  values <- reactive(shapeData$TIP)
  
  # Create color palette (only for the origin)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.983504, 40.697824, zoom = 11)
  })
  
  observe({
    button <- readButton()
    
    if (button == "orig") {
      shapeData$TIP <- tipByOrig$x
    }
    else {
      shapeData$TIP <- tipByDest$x
    }
    
    pal <- palet()
    values <- values()
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data=shapeData, weight = 2, opacity = 1, color = "black",
                  fillColor = ~pal(TIP), dashArray = "3", fillOpacity = 0.5,
                  label=paste0("Zone: ", shapeData$zone, ", Borough: ",
                               shapeData$borough, ", ID: ",shapeData$LocationID,
                               "Avg. Tip: ", round(shapeData$TIP,2))) %>%
      clearControls() %>%
      addLegend(pal = pal, values = shapeData$TIP, opacity = 0.7, 
                title = NULL, position = "topleft")
  })
}

shinyApp(ui, server)