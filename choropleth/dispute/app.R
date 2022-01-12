library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

ui <- fluidPage(
  titlePanel("Visualizing zone per dispute percentage"),
  fluidRow(
    column(4,
           sidebarPanel(
             width=500,
             radioButtons("PU_DO", "Show dispute percentage by:",
                          c("Origin" = "orig", "Destination" = "dest")))
    )
  ),
  leafletOutput("map", height=700, width=880)
)

server <- function(input, output, session) {
  
  # Read data
  taxis <- read.csv("../../data/yellow_tripdata_2019-01.csv",header=TRUE)
  
  tripCountOrig <- tabulate(taxis$PULocationID)
  tripCountOrig <- tripCountOrig[1:263]
  
  tripCountDest <- tabulate(taxis$DOLocationID)
  tripCountDest <- tripCountDest[1:263]
  
  
  subsetTrips <- taxis %>%
    filter(payment_type == 4)
  
  dispCountOrig <- tabulate(subsetTrips$PULocationID)
  dispCountOrig <- dispCountOrig[1:263]
  
  dispCountDest <- tabulate(subsetTrips$DOLocationID)
  dispCountDest <- dispCountDest[1:263]
  
  percOrig <- dispCountOrig / tripCountOrig * 100
  percDest <- dispCountDest / tripCountDest * 100
  
  percOrig[sapply(percOrig, is.infinite)] <- NA
  percDest[sapply(percDest, is.infinite)] <- NA

  # Read map
  shapeData <- spTransform(readOGR("../../data",'taxi_zones'), 
                           CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # Add new column to map
  readButtonPUDO <- reactive(input$PU_DO)
  
  # Create color palette (only for the origin)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.983504, 40.697824, zoom = 11)
  })
  
  observe({
    buttonPUDO <- readButtonPUDO()
    
    # Set the corresponding values depending on the selected buttons
    if (buttonPUDO == "orig") {
      shapeData$PERC <- percOrig
    } else {
      shapeData$PERC <- percDest
    }
    
    
    # Create bins for color map
    lims <- c(0.1, 0.2, 0.3, 0.4, 0.7, 1, 2, 5)
    bins <- c(round(quantile(shapeData$PERC, 0, na.rm = TRUE)-0.005,2),
              round(lims,2),
              round(quantile(shapeData$PERC, 1, na.rm=TRUE)+0.005,2))
    
    pal <- colorBin("YlOrRd", domain = shapeData$PERC,
                    na.color = "#A9A9A9",
                    bins = bins)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data=shapeData, weight = 2, opacity = 1, color = "white",
                  fillColor = ~pal(shapeData$PERC), dashArray = "3", fillOpacity = 0.5,
                  label=paste0("Zone: ", shapeData$zone, ", Borough: ",
                               shapeData$borough, ", ID: ",shapeData$LocationID,
                               "Percentage: ", round(shapeData$PERC,2),"%")) %>%
      clearControls() %>%
      addLegend(pal = pal, values = shapeData$PERC, opacity = 0.7, 
                title = NULL, position = "topleft")
  })
}

shinyApp(ui, server)