library(shiny)
library(leaflet)
library(rgdal)
library(chorddiag)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

taxi_data <- read.csv("../../yellow_tripdata_2019-01.csv")
taxi_zones <- read.csv("../data/taxi+_zone_lookup.csv")

zone_borough <- unclass(factor(taxi_zones$Borough))
boroughs <- attr(x = zone_borough, which = "levels")
mtrx <- matrix(0, nrow = length(boroughs), ncol = length(boroughs))
dimnames(mtrx) <- list(have = boroughs, prefer = boroughs)

# TODO: Modify matrix calculation to something more efficient
for (i in 1:nrows(taxi_data)) {
  origin <- taxi_data[i, "PULocationID"]
  destination <- taxi_data[i, "DOLocationID"]
  mtrx[zone_borough[origin], zone_borough[destination]] <-
    mtrx[zone_borough[origin], zone_borough[destination]] + 1
}

ui <- fluidPage(
  leafletOutput("map", height = 880, width = 880),
  chorddiagOutput("chord", height = 600)
)

server <- function(input, output, session) {
  shapeData <- spTransform(
    readOGR("../data", "taxi_zones"),
    CRS("+proj=longlat +datum=WGS84 +no_defs")
  )

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.983504, 40.697824, zoom = 11) %>%
      addPolygons(
        data = shapeData, weight = 2, opacity = 1, color = "red",
        dashArray = "3", fillOpacity = 0.5,
        label = ~ paste0(
          "Zone: ", zone, ", Borough: ", borough,
          ", ID: ", LocationID
        )
      )
  })

  output$chord <- renderChorddiag({
    chorddiag(mtrx, groupnameFontsize = 10, showTicks = F, groupnamePadding = 10)
  })
}

shinyApp(ui, server)
