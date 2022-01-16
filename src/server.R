library(shiny)
library(shinydashboard)
library(chorddiag)
library(mapdeck)
library(here)
library(factoextra)
library(tidyr)
library(RColorBrewer)
library(data.table)
library(GGally)
source(here("src", "load_flow_data.R"))

# TODO: Unify data loading and data preprocessing (another file)

update_map <- function(pitch, bearing) {
  # TODO: Change location to current location
  mapdeck_update(map_id = "map") %>% mapdeck_view(
    location = c(-73.983504, 40.697824),
    pitch = pitch,
    bearing = bearing,
    duration = 2000,
    transition = "fly"
  )
}


shinyServer(function(input, output, session) {
  # Load all data
  data <- get.data()
  load(here("src", "clustering.RData"))

  # 1.- Trips flow
  pitch <- 0
  bearing <- 150
  rv_map <- reactiveValues(bearing = bearing, pitch = pitch)

  # TODO: Fix zones names in geojson. If not used, remove coord load
  output$map <- renderMapdeck(mapdeck(
    location = c(-73.983504, 40.697824),
    pitch = pitch,
    bearing = bearing
  ) %>%
    add_title(title = "NYC Taxi") %>%
    add_geojson(
      data = data$shapeData,
      fill_opacity = 45,
      stroke_width = 50,
      palette = "inferno",
      auto_highlight = TRUE,
      layer_id = "shapes",
    ) %>%
    # add_text(
    #   data = data$coord,
    #   lon = "V1",
    #   lat = "V2",
    #   fill_colour = "#000000",
    #   text = "zone",
    #   layer_id = "text",
    #   size = 16,
    #   brush_radius = 500
    # ) %>%
    mapdeck_view( # Needed for initial view in browser
      location = c(-73.983504, 40.697824),
      zoom = 10,
      pitch = pitch,
      bearing = bearing,
    ))

  observeEvent(
    {
      input$pitch_up
    },
    {
      rv_map$pitch <- max(rv_map$pitch - 10, 0)
      update_map(rv_map$pitch, rv_map$bearing)
    }
  )

  observeEvent(
    {
      input$pitch_down
    },
    {
      rv_map$pitch <- min(rv_map$pitch + 10, 60)
      update_map(rv_map$pitch, rv_map$bearing)
    }
  )

  observeEvent(
    {
      input$bearing_right
    },
    {
      rv_map$bearing <- rv_map$bearing - 10
      update_map(rv_map$pitch, rv_map$bearing)
    }
  )

  observeEvent(
    {
      input$bearing_left
    },
    {
      rv_map$bearing <- rv_map$bearing + 10
      update_map(rv_map$pitch, rv_map$bearing)
    }
  )

  # TODO: Update shapes palette depending on selected style
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

  # 2.- Tips

  # 3.- Clustering
  # TODO: Don't recalculate clusters each time, save them
  kmeans.re <- reactive(kmeans(taxi, centers = input$clusters, nstart = 20))

  observeEvent(
    {
      input$plot_type
    },
    {
      if (input$plot_type == "cluster") {
        output$cluster_plot <- renderPlot({
          kmeans.re <- kmeans.re()
          fviz_cluster(kmeans.re, data = taxi)
        })
      } else if (input$plot_type == "heatmap") {
        output$cluster_plot <- renderPlot({
          kmeans.re <- kmeans.re()
          center <- kmeans.re$centers
          # create data set with the cluster number
          cluster <- c(1:input$clusters)
          center_df <- data.frame(cluster, center)
          # Reshape the data
          center_reshape <- gather(
            center_df, features, values,
            VendorID:total_amount
          )
          head(center_reshape)

          # Create the palette
          hm.palette <- colorRampPalette(rev(brewer.pal(10, "RdYlGn")),
            space = "Lab"
          )

          # Plot the heat map
          ggplot(data = center_reshape, aes(
            x = features, y = cluster,
            fill = values
          )) +
            scale_y_continuous(breaks = seq(1, input$clusters, by = 1)) +
            geom_tile() +
            coord_equal() +
            scale_fill_gradientn(colours = hm.palette(90)) +
            theme_classic() +
            theme(axis.text.x = element_text(
              angle = 90, vjust = 0.5,
              hjust = 1
            ))
        })
      } else if (input$plot_type == "pairplot") {
        output$cluster_plot <- renderPlot({
          kmeans.re <- kmeans.re()
          taxi <- as.data.table(taxi)
          taxi[, cluster := as.factor(kmeans.re$cluster)]
          col <- c(
            "date_pickup", "time_pickup", "fare_amount", "total_amount",
            "trip_distance", "tip_amount", "extra"
          )
          ggpairs(taxi, aes(colour = cluster, alpha = 0.4), columns = col)
        })
      }
    }
  )
})
