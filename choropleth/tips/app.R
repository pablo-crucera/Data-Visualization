library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

ui <- fluidPage(
  titlePanel("Visualizing tips per taxi zone"),
  fluidRow(
    column(3,
           sidebarPanel(
             width=300,
             radioButtons("PU_DO", "Show tip values by:",
                          c("Origin" = "orig", "Destination" = "dest")))
    ),
    column(3,
           sidebarPanel(
             width=300,
             radioButtons("MN_MD", "Show:",
                          c("Mean tip" = "mn", "Median tip" = "md")))
    )
  ),
  leafletOutput("map", height=700, width=880),
)

server <- function(input, output, session) {
  
  # Read data
  taxis <- read.csv("../../data/yellow_tripdata_2019-01.csv",header=TRUE)
  
  
  # Select only those who payed by credit card
  creditCardTrips <- taxis %>% 
    select(PULocationID, DOLocationID, payment_type, tip_amount) %>% 
    filter(payment_type == 1)
  
  
  
  
  # Calculate mean per Pick-up location ID
  tipMnOrig <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$PULocationID), 
                         FUN=mean) %>% 
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipMnOrig <- tipMnOrig[1:263,]
  
  # Calculate mean per Drop-off location ID
  tipMnDest <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$DOLocationID), 
                         FUN=mean) %>%
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipMnDest <- tipMnDest[1:263,]
  
  # Calculate median per Pick-up location ID
  tipMdOrig <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$PULocationID), 
                         FUN=median) %>% 
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipMdOrig <- tipMdOrig[1:263,]
  
  # Calculate median per Drop-off location ID
  tipMdDest <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$DOLocationID), 
                         FUN=median) %>%
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipMdDest <- tipMdDest[1:263,]
  
  
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
      shapeData$TIP <- tipMnOrig$x
      lims <- c(0.5, 2, 3, 4.5, 7)
    } else if (buttonMNMD == "mn") {
      shapeData$TIP <- tipMnDest$x
      lims <- c(1.5, 2.5, 4, 6, 7.5)
    } else if (buttonPUDO == "orig") {
      shapeData$TIP <- tipMdOrig$x
      lims <- c(0.10, 1, 2, 3, 5)
    } else {
      shapeData$TIP <- tipMdDest$x
      lims <- c(0.5, 2, 4, 5.5, 7.5)
    }
    
    
    # Create bins for color map
    bins <- c(round(quantile(shapeData$TIP, 0, na.rm = TRUE)-0.005,2),
              round(lims,2),
              round(quantile(shapeData$TIP, 1, na.rm=TRUE)+0.005,2))
    
    pal <- colorBin("YlGn", domain = shapeData$TIP,
                    na.color = "#A9A9A9",
                    bins = bins)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data=shapeData, weight = 2, opacity = 1, color = "white",
                  fillColor = ~pal(shapeData$TIP), dashArray = "3", fillOpacity = 0.5,
                  label=paste0("Zone: ", shapeData$zone, ", Borough: ",
                               shapeData$borough, ", ID: ",shapeData$LocationID,
                               "Avg. Tip: ", round(shapeData$TIP,2))) %>%
      clearControls() %>%
      addLegend(pal = pal, values = shapeData$TIP, opacity = 0.7, 
                title = NULL, position = "topleft")
  })
}

shinyApp(ui, server)