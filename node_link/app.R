library(shiny)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(chorddiag)
library(igraph)
library(here)
library(dplyr)
library(gdata)
source("load_data.R")

data <- initial.data()
mtrx_boroughs <- get.matrix(data$df, "boroughOr", "boroughDst", data$boroughs)

ui <- fluidPage(
  leafletOutput("map", height = 880, width = 880),
  chorddiagOutput("chord", height = 600)
)

shapeData <- spTransform(
  readOGR(here("data", "taxi_zones.shp")),
  CRS("+proj=longlat +datum=WGS84 +no_defs")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.983504, 40.697824, zoom = 11) %>%
      addPolygons(
        data = shapeData, weight = 2, opacity = 1, color = "red",
        dashArray = "3", fillOpacity = 0.3,
        label = ~ paste0(
          "Zone: ", zone, ", Borough: ", borough,
          ", ID: ", LocationID
        )
      )
  })

  output$chord <- renderChorddiag({
    chorddiag(mtrx_boroughs,
      groupnameFontsize = 10, showTicks = F,
      groupnamePadding = 10
    )
  })
}

shinyApp(ui, server)


# Node-link diagram
# TODO: integrate diagram in Shiny
coord <- as.data.frame(coordinates(shapeData),
                             row.names = 1:nrow(shapeData))

trips_df <- aggregate(trips~PULocationID + DOLocationID, data$df, sum)
trips_df <- subset(trips_df, PULocationID!=265 & DOLocationID!=265 &
                     PULocationID != 264 & DOLocationID != 264)
origins <- trips_df[,c("PULocationID", "trips")]
destinations <- trips_df[,c("DOLocationID", "trips")]
destinations <- rename(destinations, location = DOLocationID)
origins <- rename(origins, location = PULocationID)

trips_df <- interleave(origins,destinations)
new_trips <- subset(trips_df, trips>20)

mapa <- leaflet(new_trips) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = shapeData, weight = 1, opacity = 1, color = "red",
    dashArray = "3", fillOpacity = 0.2,
    label = ~ paste0(
      "Zone: ", zone, ", Borough: ", borough,
      ", ID: ", LocationID
    )
  )

max_trips <- max(new_trips)

# TODO: find a better way to plot the lines
for (x in seq(1, nrow(new_trips), by=2)) {
  mapa <- mapa %>% addGeodesicPolylines(lng = ~coord[location, 1],
                                        lat = ~coord[location, 2],
                                        weight = ~(20 * trips/max_trips),
                                        color = "black",
                                        smoothFactor = 0.5,
                                        opacity =~min(trips/max_trips, 0.75),
                                        data = trips_df[x:(x+1), ])
}
