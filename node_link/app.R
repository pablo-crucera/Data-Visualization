library(shiny)
library(leaflet)
library(rgdal)
library(chorddiag)
library(igraph)
library(here)
source("load_data.R")

data <- initial.data()
mtrx_zones <- get.matrix(
  data$df, "PULocationID", "DOLocationID",
  data$zones$LocationID
)
mtrx_boroughs <- get.matrix(data$df, "boroughOr", "boroughDst", data$boroughs)

ui <- fluidPage(
  leafletOutput("map", height = 880, width = 880),
  chorddiagOutput("chord", height = 600)
)

server <- function(input, output, session) {
  shapeData <- spTransform(
    readOGR(here("data", "taxi_zones")),
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
    chorddiag(mtrx_boroughs,
      groupnameFontsize = 10, showTicks = F,
      groupnamePadding = 10
    )
  })
}

shinyApp(ui, server)

# Keep only those edges that represent at least a 0.5 per thousand trips
# mtrx_zones[mtrx_zones < 0.0005 * nrow(data$df)] <- 0
# mtrx_zones.gr <- graph_from_adjacency_matrix(mtrx_zones,
#   mode = "directed",
#   weighted = TRUE
# )
# mtrx_zones.visn <- toVisNetworkData(mtrx_zones.gr)
# mtrx_zones.visn$edges$value <- mtrx_zones.visn$edges$weight
# mtrx_zones.visn$nodes$label <-
# 
# visNetwork(mtrx_zones.visn$nodes, mtrx_zones.visn$edges) %>%
#   visIgraphLayout()
# l <- layout_with_drl(mtrx_zones.gr, options=list(simmer.attraction=0))
# plot(mtrx_zones.gr, layout=l, vertex.size=3, vertex.label=NA)
