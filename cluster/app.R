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


### DEFINIR TAXI

taxi <- read.csv(file = "yellow_tripdata_2019-01.csv", header = TRUE)
# head(taxi)
# dim(taxi)
taxi <- sample_n(taxi, 1000, )
# summary(taxi)
# Look for NA values
sapply(taxi, function(x) sum(is.na(x)))
# We drop the las column because of the many NA
taxi <- subset(taxi, select = -congestion_surcharge)
# anyNA(taxi)

############################ Data preprocessing #################################
# For tpep_pickup_datetime and tpep_dropoff_datetime we will make columns for
# year, month, day, hour, min and second

taxi <- taxi %>% separate(tpep_pickup_datetime, c("year_pickup", "month_pickup", "other_pickup"), "-")
taxi <- taxi %>% separate(other_pickup, c("day_pickup", "time_pickup"), " ")
taxi <- taxi %>% separate(time_pickup, c("hour_pickup", "min_pickup", "sec_pickup"), ":")

taxi <- taxi %>% separate(tpep_dropoff_datetime, c("year_dropoff", "month_dropoff", "other_dropoff"), "-")
taxi <- taxi %>% separate(other_dropoff, c("day_dropoff", "time_dropoff"), " ")
taxi <- taxi %>% separate(time_dropoff, c("hour_dropoff", "min_dropoff", "sec_dropoff"), ":")


taxi <- transform(taxi,
  year_pickup = as.numeric(year_pickup), month_pickup = as.numeric(month_pickup),
  day_pickup = as.numeric(day_pickup), hour_pickup = as.numeric(hour_pickup), min_pickup = as.numeric(min_pickup),
  sec_pickup = as.numeric(sec_pickup)
)
taxi <- transform(taxi,
  VendorID = as.numeric(VendorID), passenger_count = as.numeric(passenger_count),
  day_dropoff = as.numeric(day_dropoff), hour_dropoff = as.numeric(hour_dropoff), min_dropoff = as.numeric(min_dropoff),
  sec_dropoff = as.numeric(sec_dropoff)
)
# Convert categorical to nominal

taxi$store_and_fwd_flag <- sapply(taxi$store_and_fwd_flag, unclass)

taxi <- transform(taxi,
  year_dropoff = as.numeric(year_dropoff), month_dropoff = as.numeric(month_dropoff),
  RatecodeID = as.numeric(RatecodeID), store_and_fwd_flag = as.numeric(store_and_fwd_flag), PULocationID = as.numeric(PULocationID),
  DOLocationID = as.numeric(DOLocationID), payment_type = as.numeric(payment_type)
)

taxi <- subset(taxi, select = -c(year_pickup, year_dropoff, month_pickup, month_dropoff, improvement_surcharge))
taxi <- scale(taxi)




# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Clustering the data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(
        inputId = "bins",
        label = "Number of clusters:",
        min = 1,
        max = 5,
        value = 3
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot1", width = "100%"),
      plotOutput(outputId = "distPlot2", width = "100%"),
      plotOutput(outputId = "distPlot3", width = "100%")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  kmeans.re <- reactive(kmeans(taxi, centers = input$bins, nstart = 20))

  output$distPlot1 <- renderPlot({
    kmeans.re <- kmeans.re()
    fviz_cluster(kmeans.re, data = taxi)
  })
  output$distPlot2 <- renderPlot({
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
  output$distPlot3 <- renderPlot({
    kmeans.re <- kmeans.re()
    taxi <- as.data.table(taxi)
    taxi[, cluster := as.factor(kmeans.re$cluster)]
    col <- colnames(taxi)
    col <- col[c(17, 12, 20, 21, 22, 11)]
    #
    # [1] "VendorID"              "day_pickup"            "hour_pickup"           "min_pickup"
    # [5] "sec_pickup"            "day_dropoff"           "hour_dropoff"          "min_dropoff"
    # [9] "sec_dropoff"           "passenger_count"       "trip_distance"         "RatecodeID"
    # [13] "store_and_fwd_flag"    "PULocationID"          "DOLocationID"          "payment_type"
    # [17] "fare_amount"           "extra"                 "mta_tax"               "tip_amount"
    # [21] "tolls_amount"         "total_amount"          "cluster"
    #
    ggpairs(taxi, aes(colour = cluster, alpha = 0.4), columns = col)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
