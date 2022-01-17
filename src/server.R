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

flowServer <- function(id){
  ns <- NS(id)

  moduleServer(
    id,
    function(input, output, session){
      load(here("data", "trips_flow.RData"))
      
      styles <- c(
        "oranges", "inferno", "inferno", "inferno", "heat", "topo"
      )
      names(styles) <- c(
        "dark", "light", "outdoors", "streets", "satellite", "satellite-streets"
      )
      
      pitch <- 0
      bearing <- 150
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
              fill_opacity = 45,
              stroke_width = 50,
              palette = styles[input$styles],
              auto_highlight = TRUE,
              layer_id = "shapes",
            ) %>% 
            mapdeck_view(
              location = c(-73.983504, 40.697824),
              zoom = 10,
              pitch = rv_map$pitch,
              bearing = rv_map$bearing,
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
        },
        {
          # TODO: Check flow direction
          # TODO: Same origin and destination trips?
          # TODO: Animated arcs or animated lines?
          trips <- trips[[paste(input$hour, input$day, sep = ".")]][, ]
          trips$width <- input$width * trips$busy_index
          mapdeck_update(map_id = ns("map"), session = session) %>%
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


tipsServer <- function(id) {
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
      
      # FIXME: Too many things in the observer could be calculated and stored previously
      observe({
        # Set the corresponding values depending on the input
        if (input$select == "Tips") {
          if (input$PU_DO == "orig" & input$MN_MD == "mn") {
            shapeData$VAL <- tips$tipMnOrig$x
            lims <- c(0.5, 2, 3, 4.5, 7)    # Set intermediate bins for color map
          } else if (input$MN_MD == "mn") {
            shapeData$VAL <- tips$tipMnDest$x
            lims <- c(1.5, 2.5, 4, 6, 7.5)
          } else if (input$PU_DO == "orig") {
            shapeData$VAL <- tips$tipMdOrig$x
            lims <- c(0.10, 1, 2, 3, 5)
          } else {
            shapeData$VAL <- tips$tipMdDest$x
            lims <- c(0.5, 2, 4, 5.5, 7.5)
          }
          
          colorPalette <- "YlGn"             # Color palette for map
          strPlot <- "Tip statistic: "       # String for map info
          units <- " USD"                    # Units (for  map info)
        } 
        
        else if (input$select == "Trip length") {
          if (input$PU_DO2 == "orig" & input$MN_MD2 == "mn") 
            shapeData$VAL <- dist$distMnOrig$x
          else if (input$MN_MD2 == "mn") shapeData$VAL <- dist$distMnDest$x 
          else if (input$PU_DO2 == "orig") shapeData$VAL <- dist$distMdOrig$x
          else shapeData$VAL <- dist$distMdDest$x
          
          lims <- c(2, 4, 6, 8, 12)
          
          colorPalette <- "BuPu"
          strPlot <- "Trip length statistic: "
          units <- " miles"
        }
        
        else {
          if (input$PU_DO3 == "orig") {
            shapeData$VAL <- disp$percOrig
          } else {
            shapeData$VAL <- disp$percDest
          }
          
          lims <- c(0.1, 0.2, 0.3, 0.4, 0.7, 1, 2, 5)
          
          colorPalette <- "YlOrRd"
          strPlot <- "Dispute percentage: "
          units <- " %"
        }
        
        # Create bins for color map
        bins <- c(round(quantile(shapeData$VAL, 0, na.rm = TRUE)-0.005,2),
                  round(lims,2),
                  round(quantile(shapeData$VAL, 1, na.rm=TRUE)+0.005,2))
        
        # Create palette for color map
        pal <- colorBin(colorPalette, domain = shapeData$VAL,
                        na.color = "#A9A9A9",
                        bins = bins)
        
        # Generate output map
        leafletProxy("choropleth_map") %>%
          clearShapes() %>%
          addPolygons(data=shapeData, weight = 2, opacity = 1, color = "white",
                      fillColor = ~pal(shapeData$VAL), dashArray = "3", 
                      fillOpacity = 0.5,
                      label=paste0("Zone: ", shapeData$zone, ", Borough: ",
                                   shapeData$borough, ", ID: ",
                                   shapeData$LocationID,", ", strPlot, 
                                   round(shapeData$VAL,2), units)) %>%
          clearControls() %>%
          addLegend(pal = pal, values = shapeData$VAL, opacity = 0.7, 
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
    }
  )
}


shinyServer(function(input, output, session) {
  flowServer("trips_flow")
  tipsServer("tips")
  clusteringServer("clustering")
})
