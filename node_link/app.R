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
    sliderInput("pitch",
      label = "Pitch:",
      min = 0, max = 60, value = 0, ticks = FALSE
    ),
    sliderInput("bearing",
      label = "Bearing:",
      min = -180, max = 180, value = 150, ticks = FALSE
    ),
    sliderInput("width",
      label = "Width:",
      min = 0, max = 100, value = 50, ticks = FALSE
    ),
    selectInput("hour", h3("Hour"),
      choices = 0:23, selected = 17
    )
  ),
  dashboardBody(
    mapdeckOutput("map", height = 700),
    chorddiagOutput("chord")
  )
)

server <- function(input, output, session) {
  data <- get.data()

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
    ))

  observeEvent(
    {
      input$pitch
      input$bearing
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
      input$hour
    },
    {
      trips <- data$trips[[input$hour]][,]
      max_amount <- 4608 # Maximum value found 
      trips$opacity <- input$width * trips$amount / max_amount
      mapdeck_update(map_id = "map") %>%
        add_animated_arc(
          data = trips,
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

  # TODO: Add hour modification
  output$chord <- renderChorddiag({
    chorddiag(data$mtrx_boroughs,
      groupnameFontsize = 10, showTicks = F,
      groupnamePadding = 10
    )
  })
}

shinyApp(ui, server)
