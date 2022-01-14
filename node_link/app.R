library(shiny)
library(shinydashboard)
library(chorddiag)
library(mapdeck)
source("load_data.R")

# Mapbox token
key <- "pk.eyJ1IjoiamF2aWVnYWwiLCJhIjoiY2t5ZDU0NGo1MDEyMTMwcXBqOWxuaWQ1aSJ9.R8Jpo0pPpa8Ow46YQry_Wg"
set_token(key)

# Get week days
styles <- c("dark", "light", "outdoors", "streets", "satellite", "satellite-streets")
lc_time <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_US.UTF-8") # Needed to get week days in English
week_days <- weekdays(seq(Sys.Date(), Sys.Date() + 6, by = "days"))
Sys.setlocale("LC_TIME", lc_time)

hours <- sprintf("%02d", 0:23)
names(hours) <- sprintf("%02d:00", 0:23)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "TLC Taxi Trip Data"),
  dashboardSidebar(
    selectInput("styles", h3("Map style"),
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
    selectInput("day", h3("Week day"),
      choices = week_days, selected = "Monday"
    ),
    selectInput("hour", h3("Hour"),
      choices = hours, selected = 17
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
        pitch = input$pitch,
        bearing = input$bearing,
        duration = 2000,
        transition = "fly"
      )
    }
  )

  observeEvent(
    {
      input$styles
    },
    {
      mapdeck_update(map_id = "map") %>%
        update_style(style = mapdeck_style(input$styles))
    }
  )

  observeEvent(
    {
      input$width
      input$hour
      input$day
    },
    {
      # TODO: Move width calculation to load_data.R
      trips <- data$trips[[paste(input$hour, input$day, sep = ".")]][, ]
      max_factor <- 1000
      trips$width <- input$width * trips$amount / max_factor
      mapdeck_update(map_id = "map") %>%
        add_animated_arc(
          data = trips,
          origin = c("Orlng", "Orlat"),
          destination = c("Dstlng", "Dstlat"),
          stroke_width = "width",
          trail_length = 0.05,
          animation_speed = 0.01,
          palette = "magenta2green",
          brush_radius = 500,
          layer_id = "arcs",
          update_view = FALSE,
        )
    }
  )

  observeEvent(
    {
      input$hour
      input$day
    },
    {
      output$chord <- renderChorddiag({
        chorddiag(data$mtrcs_boroughs[[paste(input$hour,
          input$day,
          sep = "."
        )]][, ],
        groupnameFontsize = 10, showTicks = F,
        groupnamePadding = 10
        )
      })
    }
  )
}

shinyApp(ui, server)
