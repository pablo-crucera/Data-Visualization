library(shiny)
library(shinydashboard)
library(chorddiag)
library(mapdeck)
source("load_data.R")

# Mapbox token
key <- "pk.eyJ1IjoiamF2aWVnYWwiLCJhIjoiY2t5ZDU0NGo1MDEyMTMwcXBqOWxuaWQ1aSJ9.R8Jpo0pPpa8Ow46YQry_Wg"
set_token(key)

styles <- c("dark", "light", "outdoors", "streets", "satellite", "satellite-streets")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "TLC Taxi Trip Data"),
  dashboardSidebar(
    selectInput("select", h3("Map style"),
      choices = styles,
      selected = "streets"
    ),
    sliderInput("slider",
      label = "Pitch:",
      min = 0, max = 60, value = 0, ticks = FALSE
    ),
    sliderInput("slider2",
      label = "Bearing:",
      min = -180, max = 180, value = 150, ticks = FALSE
    ),
    sliderInput("width",
      label = "Width:",
      min = 0, max = 100, value = 50, ticks = FALSE
    )
  ),
  dashboardBody(
    mapdeckOutput("map", height = 700)
  )

  # column(4, chorddiagOutput("chord"))

  # Old way
  # leafletOutput("map", height = 880, width = 880)
)

server <- function(input, output, session) {
  data <- get.data()

  max_amount <- max(data$trips$amount)
  data$trips$opacity <- 50 * data$trips$amount / max_amount

  output$map <- renderMapdeck(mapdeck(
    location = c(-73.983504, 40.697824),
    zoom = 10,
    pitch = 0,
    bearing = 150
  ) %>%
    add_geojson(
      data = data$shapeData,
      fill_opacity = 45,
      stroke_width = 50,
      palette = "inferno",
      auto_highlight = TRUE,
      layer_id = "shapes"
      # ) %>%
      # add_text(
      #   data = data$coord,
      #   lon = "V1",
      #   lat = "V2",
      #   fill_colour = "#000000",
      #   text = "zone",
      #   layer_id = "text",
      #   size = 16,
      #   #   brush_radius = 500
    ))

  observeEvent(
    {
      input$slider
      input$slider2
    },
    {
      mapdeck_update(map_id = "map") %>% mapdeck_view(
        location = c(-73.983504, 40.697824),
        zoom = 10,
        pitch = input$slider,
        bearing = input$slider2,
        duration = 2000,
        transition = "fly"
      )
    }
  )

  observeEvent(
    {
      input$select
    },
    {
      mapdeck_update(map_id = "map") %>%
        update_style(style = mapdeck_style(input$select))
    }
  )

  observeEvent(
    {
      input$width
    },
    {
      data$trips$opacity <- input$width * data$trips$amount / max_amount
      mapdeck_update(map_id = "map") %>%
        add_animated_arc(
          data = data$trips,
          origin = c("Orlng", "Orlat"),
          destination = c("Dstlng", "Dstlat"),
          stroke_width = "opacity",
          trail_length = 0.05,
          animation_speed = 0.01,
          palette = "magenta2green",
          brush_radius = 500,
          layer_id = "arcs",
          update_view = FALSE,
        )
    }
  )

  # output$chord <- renderChorddiag({
  #   chorddiag(data$mtrx_boroughs,
  #     groupnameFontsize = 10, showTicks = F,
  #     groupnamePadding = 10
  #   )
  # })

  # Old way
  # nyc_map <- leaflet() %>%
  #     addProviderTiles("CartoDB.Positron") %>%
  #     setView(-73.983504, 40.697824, zoom = 11) %>%
  #     addPolygons(
  #       data = data$shapeData, weight = 2, opacity = 1, color = "red",
  #       dashArray = "3", fillOpacity = 0.3,
  #       label = ~ paste0(
  #         "Zone: ", zone, ", Borough: ", borough,
  #         ", ID: ", LocationID
  #       )
  #     )
  #
  # addFlows would work better
  # max_trips <- max(data$trips)
  # for (x in seq(1, nrow(data$trips), by = 2)) {
  #   nyc_map <- nyc_map %>% addGeodesicPolylines(
  #     lng = ~lng,
  #     lat = ~lat,
  #     weight = ~ (20 * amount / max_trips),
  #     color = "black",
  #     smoothFactor = 0.5,
  #     opacity = ~ min(amount / max_trips, 0.75),
  #     data = data$trips[x:(x + 1), ]
  #   )
  # }

  # output$map <- renderLeaflet({nyc_map})
}

shinyApp(ui, server)
