library(shiny)
library(shinydashboard)
library(chorddiag)
library(mapdeck)
source("load_data.R")

# Mapbox token
key <- "pk.eyJ1IjoiamF2aWVnYWwiLCJhIjoiY2t5ZDU0NGo1MDEyMTMwcXBqOWxuaWQ1aSJ9.R8Jpo0pPpa8Ow46YQry_Wg"
set_token(key)

# Get week days
styles <- c(
  "dark", "light", "outdoors", "streets", "satellite", "satellite-streets"
)
lc_time <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_US.UTF-8") # Needed to get week days in English
week_days <- weekdays(seq(Sys.Date(), Sys.Date() + 6, by = "days"))
Sys.setlocale("LC_TIME", lc_time)

hours <- sprintf("%02d", 0:23)
names(hours) <- sprintf("%02d:00-%02d:59", 0:23, 0:23)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "TLC Taxi Trip Data"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(
        2, box(
          title = "Time and week day", status = "primary", height = 700,
          solidHeader = TRUE, width = NULL,
          selectInput("day", h3("Week day"),
            choices = week_days, selected = "Monday"
          ),
          selectInput("hour", h3("Time"),
            choices = hours, selected = 17
          )
        )
      ),
      column(
        6, box(
          title = "Flow map", status = "primary",
          solidHeader = TRUE, height = 700, width = NULL,
          mapdeckOutput("map", height = 500),
          box(fluidRow(
            column(2, selectInput("styles", h3("Map style"),
              choices = styles,
              selected = "streets"
            )),
            column(2, sliderInput("pitch",
              label = "Pitch:",
              min = 0, max = 60, value = 0, ticks = FALSE
            )),
            column(2, sliderInput("bearing",
              label = "Bearing:",
              min = -180, max = 180, value = 150, ticks = FALSE
            )),
            column(2, sliderInput("width",
              label = "Width:",
              min = 0, max = 100, value = 50, ticks = FALSE
            )),
            column(2, sliderInput("length",
              label = "Length:",
              min = 0, max = 500, value = 50, ticks = FALSE
            )),
            column(2, sliderInput("speed",
              label = "Speed:",
              min = 0, max = 500, value = 50, ticks = FALSE
            ))
          ), width = NULL, status = "primary")
        )
      ),
      column(
        4, box(
          title = "Chord diagram", status = "primary", solidHeader = TRUE,
          chorddiagOutput("chord", height = 650),
          width = NULL, height = 700
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- get.data()

  output$map <- renderMapdeck(mapdeck(
    location = c(-73.983504, 40.697824),
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
    ) %>%
    mapdeck_view( # Needed for initial view in browser
      location = c(-73.983504, 40.697824),
      zoom = 10,
      pitch = 0,
      bearing = 150,
    ))

  observeEvent(
    {
      input$pitch
      input$bearing
    },
    {
      mapdeck_update(map_id = "map") %>% mapdeck_view(
        location = c(-73.983504, 40.697824),
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
      input$length
      input$speed
    },
    {

      trips <- data$trips[[paste(input$hour, input$day, sep = ".")]][, ]
      max_factor <- 1000
      trips$width <- input$width * trips$amount / max_factor
      mapdeck_update(map_id = "map") %>%
        add_animated_arc(
          data = trips,
          origin = c("Orlng", "Orlat"),
          destination = c("Dstlng", "Dstlat"),
          stroke_width = "width",
          trail_length = input$length * 0.001,
          animation_speed = input$speed * 0.0004,
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
