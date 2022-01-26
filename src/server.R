library(shiny)
library(chorddiag)
library(mapdeck)
library(here)
library(factoextra)
library(tidyr)
library(RColorBrewer)
library(data.table)
library(GGally)
library(leaflet)
library(cluster)
library(caret)
library(dplyr)
library(purrr)
library(rlang)
library(shinyjs)
library(googlePolylines)
library(sp)
library(spatialwidget)

update_map <- function(pitch, bearing, ns, session) {
  # TODO: Update to current location with current zoom
  mapdeck_update(map_id = ns("map"), session = session) %>% mapdeck_view(
    location = c(-73.983504, 40.697824),
    pitch = pitch,
    bearing = bearing,
    duration = 2000,
    transition = "fly"
  )
}

flowServer <- function(id) {
  ns <- NS(id)

  moduleServer(
    id,
    function(input, output, session) {
      load(here("data", "trips_flow.RData"))

      styles <- c(
        "oranges", "inferno", "inferno", "inferno", "heat", "topo"
      )
      names(styles) <- c(
        "dark", "light", "outdoors", "streets", "satellite", "satellite-streets"
      )

      pitch <- 0
      bearing <- 0
      rv_map <- reactiveValues(bearing = bearing, pitch = pitch, ns = ns)

      # FIXME: Zones names in geojson. If not used, remove coord load
      output$map <- renderMapdeck(mapdeck(
        location = c(-73.983504, 40.697824),
        pitch = pitch,
        bearing = bearing
      ) %>%
        add_title(title = "NYC Taxi") %>%
        # add_text(
        #   data = coord,
        #   lon = "V1",
        #   lat = "V2",
        #   fill_colour = "#000000",
        #   text = "zone",
        #   layer_id = "text",
        #   size = 16,
        #   brush_radius = 500
        # ) %>%
        mapdeck_view(
          location = c(-73.983504, 40.697824),
          zoom = 10,
          pitch = pitch,
          bearing = bearing,
        )
      )

      observeEvent(
        {
          input$pitch_up
        },
        {
          rv_map$pitch <- max(rv_map$pitch - 10, 0)
          update_map(rv_map$pitch, rv_map$bearing, ns = ns, session = session)
        }
      )

      observeEvent(
        {
          input$pitch_down
        },
        {
          rv_map$pitch <- min(rv_map$pitch + 10, 60)
          update_map(rv_map$pitch, rv_map$bearing, ns = ns, session = session)
        }
      )

      observeEvent(
        {
          input$bearing_right
        },
        {
          rv_map$bearing <- rv_map$bearing - 10
          update_map(rv_map$pitch, rv_map$bearing, ns = ns, session = session)
        }
      )

      observeEvent(
        {
          input$bearing_left
        },
        {
          rv_map$bearing <- rv_map$bearing + 10
          update_map(rv_map$pitch, rv_map$bearing, ns = ns, session = session)
        }
      )

      # TODO: Do shapes update with current zoom
      observeEvent(
        {
          input$styles
        },
        {
          mapdeck_update(map_id = ns("map"), session = session) %>%
            update_style(style = mapdeck_style(input$styles)) %>%
            add_geojson(
              data = shapeData,
              fill_opacity = 25,
              stroke_width = 30,
              palette = styles[input$styles],
              auto_highlight = TRUE,
              layer_id = "shapes",
              update_view = FALSE,
            )
        }
      )

      observeEvent(
        {
          input$width
          input$hour
          input$day
          input$length
          input$speed
          input$flow
        },
        {
          # TODO: Check flow direction
          # TODO: Same origin and destination trips?
          trips <- trips[[paste(input$hour, input$day, sep = ".")]][, ]
          trips$width <- input$width * trips$busy_index

          # TODO: Add lines or arcs functionality: clear_animated_line?
          if (input$flow == "Arcs") {
            mapdeck_update(map_id = ns("map"), session = session) %>%
              # clear_animated_arc(layer_id = "lines") %>%
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
          # } else {
          #   mapdeck_update(map_id = ns("map"), session = session) %>%
          #     clear_animated_arc(layer_id = "arcs") %>%
          #     add_animated_line(
          #       data = trips,
          #       origin = c("Orlng", "Orlat"),
          #       destination = c("Dstlng", "Dstlat"),
          #       stroke_width = "width",
          #       trail_length = input$length * 0.001,
          #       animation_speed = input$speed * 0.0004,
          #       brush_radius = 500,
          #       layer_id = "lines",
          #       update_view = FALSE,
          #     )
          }
        }
      )

      observeEvent(
        {
          input$hour
          input$day
        },
        {
          output$chord <- renderChorddiag({
            chorddiag(mtrcs_boroughs[[paste(input$hour,
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
  )
}


statsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      load(here("data", "choropleth.RData"))

      # Create map
      output$choropleth_map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(-73.983504, 40.697824, zoom = 11)
      })
      
      # Define reactive expressions
      readSelector <- reactive(input$select)
      readButtonPUDO <- reactive(input$PU_DO)
      readButtonMNMD <- reactive(input$MN_MD)
      readButtonPUDO2 <- reactive(input$PU_DO2)
      readButtonMNMD2 <- reactive(input$MN_MD2)
      readButtonPUDO3 <- reactive(input$PU_DO3)
      
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
        leafletProxy("choropleth_map", session) %>%
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
  )
}

clusteringServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      load(here("data", "clustering.RData"))

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
                time_pickup:boroughDst.Unknown
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
                "total_amount", "trip_distance","tolls_amount",
                "passenger_count"
              )
              ggpairs(taxi, aes(colour = cluster, alpha = 0.4), columns = col)
            })
          }
        }
      )
    }
  )
}


shinyServer(function(input, output, session) {
  flowServer("trips_flow")
  statsServer("stats")
  clusteringServer("clustering")
})
