library(shiny)
library(leaflet)
library(rgdal)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

ui <- fluidPage(
  leafletOutput("map", height=880, width=880),
)

server <- function(input, output, session) {
  shapeData <- spTransform(readOGR("../data",'taxi_zones'), 
                           CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(-73.983504, 40.697824, zoom = 11) %>%
    addPolygons(data=shapeData, weight = 2, opacity = 1, color = "red", 
                dashArray = "3", fillOpacity = 0.5, 
                label=~paste0("Zone: ", zone, ", Borough: ", borough,
                              ", ID: ", LocationID))
  })
}

shinyApp(ui, server)
