library(shiny)
library(cluster)
library(dplyr)
library(tidyr)
library(purrr)
library(factoextra) # clustering algorithms & visualization
library(RColorBrewer)
library(GGally)
library(rlang)
library(data.table)
library(spatialwidget)
library(shinydashboard)
library(sp)
library(rgdal)
library(googlePolylines)
library(mapdeck)
library(shinyjs)
load("data_cluster3.RData") 

styles <- c("cluster","heatmap","pairplot")
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "TLC Taxi Trip Data"),
  dashboardSidebar(
    selectInput("select", h3("Map style"),
                choices = styles,
                selected = "cluster"
    ),
    sliderInput("bins",
                label = "Number of clusters:",
                min = 1, max = 5, value = 3, ticks = FALSE
    )
  ),
  dashboardBody(
    plotOutput(outputId = "distPlot1", width = "100%")
  )
  
)

server <- function(input, output) {
  
  kmeans.re <- reactive(kmeans(taxi, centers = input$bins, nstart = 20))
  
  
  observeEvent(
    {
      input$select
    },
    {
      if(input$select == "cluster"){
        output$distPlot1 <- renderPlot({
          
          kmeans.re <- kmeans.re()
          fviz_cluster(kmeans.re, data = taxi)
          
        })
        
      }
      else if(input$select == "heatmap"){
        output$distPlot1 <- renderPlot({
          kmeans.re <- kmeans.re()
          center <- kmeans.re$centers
          # create dataset with the cluster number
          cluster <- c(1:input$bins)
          center_df <- data.frame(cluster, center)
          # Reshape the data
          center_reshape <- gather(center_df, features, values, VendorID:total_amount)
          head(center_reshape)
          
          # Create the palette
          hm.palette <- colorRampPalette(rev(brewer.pal(10, "RdYlGn")), space = "Lab")
          
          # Plot the heat map
          ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
            scale_y_continuous(breaks = seq(1, input$bins, by = 1)) +
            geom_tile() +
            coord_equal() +
            scale_fill_gradientn(colours = hm.palette(90)) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
          
        })
        
      }
      else if (input$select == "pairplot"){
        output$distPlot1 <- renderPlot({
          kmeans.re <- kmeans.re()
          taxi <- as.data.table(taxi)
          taxi[, cluster := as.factor(kmeans.re$cluster)]
          col <- c("date_pickup", "time_pickup" , "fare_amount", "total_amount", "trip_distance" , "tip_amount" ,"extra")
          ggpairs(taxi, aes(colour = cluster, alpha = 0.4), columns = col)
          
        })
        
      }
    })
  
  
  
}

shinyApp(ui, server)
