library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)
library(here)
source("choropleth.R")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "TLC Taxi Trip Data"),
  dashboardSidebar(
    selectInput("select", h3("What to visualize?"),
                choices = c("Tips", "Trip length", "Disputes"),
                selected = "Tips"),
    
    conditionalPanel(
      condition = "input.select == 'Tips'" ,
      radioButtons("PU_DO", "Show tip values by:",
                   c("Origin" = "orig", "Destination" = "dest")),
      radioButtons("MN_MD", "Show:",
                   c("Mean tip" = "mn", "Median tip" = "md"))
    ),

    conditionalPanel(
      condition = "input.select == 'Trip length'",
      radioButtons("PU_DO2", "Show distance values by:",
                   c("Origin" = "orig", "Destination" = "dest"),
                   selected = "orig"),
      radioButtons("MN_MD2", "Statistic:",
                   c("Mean distance" = "mn", "Median distance" = "md"))
    ),
    
    conditionalPanel(
      condition = "input.select == 'Disputes'" ,
      radioButtons("PU_DO3", "Show dispute percentage by:",
                   c("Origin" = "orig", "Destination" = "dest"))
    )
  ),
  
  dashboardBody(
    leafletOutput("map", height=880, width=880)
  )
)

server <- function(input, output, session) {
  
  # Read data
  shapeData <- getData("../data/yellow_tripdata_2019-01.csv")
  
  # Define reactive expressions
  readSelector <- reactive(input$select)
  readButtonPUDO <- reactive(input$PU_DO)
  readButtonMNMD <- reactive(input$MN_MD)
  readButtonPUDO2 <- reactive(input$PU_DO2)
  readButtonMNMD2 <- reactive(input$MN_MD2)
  readButtonPUDO3 <- reactive(input$PU_DO3)
  
  # Create map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.983504, 40.697824, zoom = 11)
  })
  
  observe({
    selector <- readSelector()
    
    # Set the corresponding values depending on the input
    if (selector == "Tips") {
      buttonPUDO <- readButtonPUDO()
      buttonMNMD <- readButtonMNMD()
      if (buttonPUDO == "orig" & buttonMNMD == "mn") 
        lims <- c(0.5, 2, 3, 4.5, 7)    # Set intermediate bins for color map
      else if (buttonMNMD == "mn")
        lims <- c(1.5, 2.5, 4, 6, 7.5)
      else if (buttonPUDO == "orig")
        lims <- c(0.10, 1, 2, 3, 5)
      else
        lims <- c(0.5, 2, 4, 5.5, 7.5)
      
      colorPalette = "YlGn"             # Color palette for map
      strPlot = "Tip statistic: "       # String for map info
      units = " USD"                    # Units (for map info)
    } 
    
    else if (selector == "Trip length") {
      
      buttonPUDO <- readButtonPUDO2()
      buttonMNMD <- readButtonMNMD2()
      
      lims <- c(2, 4, 6, 8, 12)
      colorPalette = "BuPu"
      strPlot = "Trip length statistic: "
      units = " miles"
      
    }
    
    else {
      
      buttonPUDO <- readButtonPUDO3()
      buttonMNMD <- ""

      lims <- c(0.1, 0.2, 0.3, 0.4, 0.7, 1, 2, 5)
      colorPalette = "YlOrRd"
      strPlot = "Dispute percentage: "
      units = " %"
      
    }
    
    identif <- paste0(selector, buttonMNMD, buttonPUDO)
    
    # Create bins for color map
    bins <- c(round(quantile(shapeData@data[identif][,1], 0, na.rm = TRUE)
                    -0.005,2),
              round(lims,2),
              round(quantile(shapeData@data[identif][,1], 1, na.rm=TRUE)
                    +0.005,2))
    
    # Create palette for color map
    pal <- colorBin(colorPalette, domain = shapeData@data[identif][,1],
                    na.color = "#A9A9A9",
                    bins = bins)
    
    # Generate output map
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data=shapeData, weight = 2, opacity = 1, color = "white",
                  fillColor = ~pal(shapeData@data[identif][,1]),
                  dashArray = "3", fillOpacity = 0.5,
                  label=paste0("Zone: ", shapeData$zone, ", Borough: ",
                               shapeData$borough, ", ID: ",
                               shapeData$LocationID,", ", strPlot, 
                               round(shapeData@data[identif][,1],2), units)) %>%
      clearControls() %>%
      addLegend(pal = pal, values = shapeData@data[identif][,1], opacity = 0.7, 
                title = NULL, position = "topleft")
  })
}

shinyApp(ui, server)