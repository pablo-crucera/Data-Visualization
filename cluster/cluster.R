# ¿Existen grupos diferenciados de tipos de trayectos?
# Clustering y scatterplot/pairplot, filtrar por variables

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
library(here)
library(rgdal)
library(gdata)

taxi <- read.csv(here("/home/julia/Escritorio/Visualization Practical Work/data/yellow_tripdata_2019-01.csv"),  header = TRUE)
taxi <- sample_n(taxi, 1000)
taxi <- subset(taxi, select = -congestion_surcharge)
save(taxi, file = "nsample.RData")

##cite
## https://uc-r.github.io/kmeans_clustering
## https://www.guru99.com/r-k-means-clustering.html

###############################################################################
########################## Clustering algorithm ################################
################################################################################

taxi <- read.csv(here("/home/julia/Escritorio/Visualization Practical Work/data/yellow_tripdata_2019-01.csv"),  header = TRUE)
# head(taxi)
# dim(taxi)
taxi <- sample_n(taxi, 1000)
# summary(taxi)
# Look for NA values
sapply(taxi, function(x)
  sum(is.na(x)))
# We drop the las column because of the many NA
taxi <- subset(taxi, select = -congestion_surcharge)
anyNA(taxi)

################################Preprocessing###############################################
taxi <- read.csv(here("/home/julia/Escritorio/Visualization Practical Work/data/yellow_tripdata_2019-01.csv"),  header = TRUE)
taxi <- sample_n(taxi, 1000)
taxi <- subset(taxi, select = -congestion_surcharge)

## pickup
taxi <- 
  taxi %>%
  mutate(tpep_pickup_datetime=lapply(tpep_pickup_datetime, as.character))


# converting to datetime object
taxi[["tpep_pickup_datetime"]] <- strptime(taxi[["tpep_pickup_datetime"]],
                                            format = "%Y-%m-%d %H:%M:%S")
taxi <- 
  taxi %>%
  mutate(tpep_pickup_datetime=lapply(tpep_pickup_datetime, as.numeric))


## dropoff 
taxi <- 
  taxi %>%
  mutate(tpep_dropoff_datetime=lapply(tpep_dropoff_datetime, as.character))


# converting to datetime object
taxi[["tpep_dropoff_datetime"]] <- strptime(taxi[["tpep_dropoff_datetime"]],
                                 format = "%Y-%m-%d %H:%M:%S")
taxi <- 
  taxi %>%
  mutate(tpep_dropoff_datetime=lapply(tpep_dropoff_datetime, as.numeric))


# Convert categorical to nominal

taxi$store_and_fwd_flag <- sapply(taxi$store_and_fwd_flag, unclass)


taxi <-
  transform(
    taxi,
    tpep_pickup_datetime = as.numeric(tpep_pickup_datetime),
    tpep_dropoff_datetime = as.numeric(tpep_dropoff_datetime),
    RatecodeID = as.numeric(RatecodeID),
    store_and_fwd_flag = as.numeric(store_and_fwd_flag),
    PULocationID = as.numeric(PULocationID),
    DOLocationID = as.numeric(DOLocationID),
    payment_type = as.numeric(payment_type)
  )


taxi <-
  subset(
    taxi,
    select = -c(
      improvement_surcharge
    )
  )


taxi <- scale(taxi)



############preprocessing 2####################################################
load("nsample.RData")

taxi <-
  taxi %>% separate(tpep_dropoff_datetime,
                    c("date_dropoff", "time_dropoff"),
                    " ")

taxi <-
  taxi %>% separate(tpep_pickup_datetime,
                    c("date_pickup", "time_pickup"),
                    " ")

############ pickup
taxi <- 
  taxi %>%
  mutate(date_pickup=lapply(date_pickup, as.character))


# converting to datetime object
taxi[["date_pickup"]] <- strptime(taxi[["date_pickup"]],
                                           format = "%Y-%m-%d")
taxi <- 
  taxi %>%
  mutate(date_pickup=lapply(date_pickup, as.numeric))

taxi <- 
  taxi %>%
  mutate(time_pickup=lapply(time_pickup, as.character))


# converting to datetime object
taxi[["time_pickup"]] <- strptime(taxi[["time_pickup"]],
                                           format = "%H:%M:%S")
taxi <- 
  taxi %>%
  mutate(time_pickup=lapply(time_pickup, as.numeric))



##### dropoff 
taxi <- 
  taxi %>%
  mutate(date_dropoff=lapply(date_dropoff, as.character))


# converting to datetime object
taxi[["date_dropoff"]] <- strptime(taxi[["date_dropoff"]],
                                  format = "%Y-%m-%d")
taxi <- 
  taxi %>%
  mutate(date_dropoff=lapply(date_dropoff, as.numeric))

taxi <- 
  taxi %>%
  mutate(time_dropoff=lapply(time_dropoff, as.character))


# converting to datetime object
taxi[["time_dropoff"]] <- strptime(taxi[["time_dropoff"]],
                                  format = "%H:%M:%S")
taxi <- 
  taxi %>%
  mutate(time_dropoff=lapply(time_dropoff, as.numeric))

# Convert categorical to nominal

taxi$store_and_fwd_flag <- sapply(taxi$store_and_fwd_flag, unclass)


taxi <-
  transform(
    taxi,
    time_dropoff = as.numeric(time_dropoff),
    date_dropoff = as.numeric(date_dropoff),
    time_pickup = as.numeric(time_pickup ),
    date_pickup  = as.numeric(date_pickup ),
    RatecodeID = as.numeric(RatecodeID),
    store_and_fwd_flag = as.numeric(store_and_fwd_flag),
    PULocationID = as.numeric(PULocationID),
    DOLocationID = as.numeric(DOLocationID),
    payment_type = as.numeric(payment_type)
  )


taxi <-
  subset(
    taxi,
    select = -c(
      improvement_surcharge
    )
  )


taxi <- scale(taxi)




############################Data preprocessing #################################
# For tpep_pickup_datetime and tpep_dropoff_datetime we will make columns for
# year, month, day, hour, min and second

taxi <-
  taxi %>% separate(tpep_pickup_datetime,
                    c("year_pickup", "month_pickup", "other_pickup"),
                    "-")
taxi <-
  taxi %>% separate(other_pickup, c("day_pickup", "time_pickup"), " ")
taxi <-
  taxi %>% separate(time_pickup, c("hour_pickup", "min_pickup", "sec_pickup"), ":")

taxi <-
  taxi %>% separate(tpep_dropoff_datetime,
                    c("year_dropoff", "month_dropoff", "other_dropoff"),
                    "-")
taxi <-
  taxi %>% separate(other_dropoff, c("day_dropoff", "time_dropoff"), " ")
taxi <-
  taxi %>% separate(time_dropoff,
                    c("hour_dropoff", "min_dropoff", "sec_dropoff"),
                    ":")


taxi <-
  transform(
    taxi,
    year_pickup = as.numeric(year_pickup),
    month_pickup = as.numeric(month_pickup),
    day_pickup = as.numeric(day_pickup),
    hour_pickup = as.numeric(hour_pickup),
    min_pickup = as.numeric(min_pickup),
    sec_pickup = as.numeric(sec_pickup)
  )
taxi <-
  transform(
    taxi,
    VendorID  = as.numeric(VendorID),
    passenger_count  = as.numeric(passenger_count),
    day_dropoff = as.numeric(day_dropoff),
    hour_dropoff = as.numeric(hour_dropoff),
    min_dropoff = as.numeric(min_dropoff),
    sec_dropoff = as.numeric(sec_dropoff)
  )
# Convert categorical to nominal

taxi$store_and_fwd_flag <- sapply(taxi$store_and_fwd_flag, unclass)

taxi <-
  transform(
    taxi,
    year_dropoff = as.numeric(year_dropoff),
    month_dropoff = as.numeric(month_dropoff),
    RatecodeID = as.numeric(RatecodeID),
    store_and_fwd_flag = as.numeric(store_and_fwd_flag),
    PULocationID = as.numeric(PULocationID),
    DOLocationID = as.numeric(DOLocationID),
    payment_type = as.numeric(payment_type)
  )

taxi <-
  subset(
    taxi,
    select = -c(
      year_pickup,
      year_dropoff,
      month_pickup,
      month_dropoff,
      improvement_surcharge
    )
  )


taxi <- scale(taxi)



str(taxi)

#
# ##########################Number of clusters ##################################
#
# # function to compute total within-cluster sum of square
# set.seed(123)
# wss <- function(k) {
#   kmeans(taxi, k, nstart = 10 )$tot.withinss
# }
#
# # Compute and plot wss for k = 1 to k = 15
# k.values <- 1:15
#
# # extract wss for 2-15 clusters
# wss_values <- map_dbl(k.values, wss)
#
# plot(k.values, wss_values,
#      type="b", pch = 19, frame = FALSE,
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares") # --> 2 clusters
#
# fviz_nbclust(taxi, kmeans, method = "wss", k=15) # --> 8, 10
# fviz_nbclust(taxi, kmeans, method = "silhouette") # --> 9 clusters
#
#
#
# ################################# Clustering #################################
source("load_cluster_data.R")
taxi<- get.data()

load("data_cluster3.RData")

kmeans.re <- kmeans(taxi, centers = 3, nstart = 20)
fviz_cluster(kmeans.re, data = taxi)


############################### Creat heat map from clusters ###################

center <- kmeans.re$centers
# create dataset with the cluster number

cluster <- c(1:3)
center_df <- data.frame(cluster, center)

# Reshape the data

center_reshape <-
  gather(center_df, features, values, VendorID:total_amount)
head(center_reshape)

# Create the palette
hm.palette <-
  colorRampPalette(rev(brewer.pal(10, 'RdYlGn')), space = 'Lab')

# Plot the heat map
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic() +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ))


############################### Creat pairplot for clusters ###################


kmeans.re <- kmeans(taxi, centers = 2, nstart = 20)

taxi <- as.data.table(taxi)
taxi[, cluster := as.factor(kmeans.re$cluster)]
#col = colnames(taxi)
col <- c("date_pickup", "time_pickup" , "fare_amount", "total_amount", "trip_distance" , "tip_amount" ,"extra")
#
# [1] "VendorID"           "date_pickup"        "time_pickup"        "date_dropoff"       "time_dropoff"      
# [6] "passenger_count"    "trip_distance"      "RatecodeID"         "store_and_fwd_flag" "PULocationID"      
# [11] "DOLocationID"       "payment_type"       "fare_amount"        "extra"              "mta_tax"           
# [16] "tip_amount"         "tolls_amount"       "total_amount"       "cluster"
#
ggpairs(taxi, aes(colour = cluster, alpha = 0.4), columns = col)



##################################COSAS RANDOM #################################


# hclust_avg <- hclust(distance, method = 'average')
# plot(hclust_avg)
#
#
#
# Fitting K-Means clustering Model
# to training dataset
# set.seed(240) # Setting seed
# kmeans.re <- kmeans(taxi, centers = 5, nstart = 20)
# fviz_cluster(kmeans.re, data = taxi)


#
# kmeans.re$cluster
#
# y_kmeans <- kmeans.re$cluster
# clusplot(taxi[, c("hour_pickup", "total_amount")],
#          y_kmeans,
#          main = paste("Cluster iris"),
#          xlab = 'hour',
#          ylab = 'Total_amount')
#
#
# # hierarchical
# distance <- get_dist(taxi)
# hclust_avg <- hclust(distance, method = 'average')
# plot(hclust_avg) # --> suggests 5 or 9 clusters


# The mtcars dataset:

data <- as.matrix(taxi)
# Default Heatmap
heatmap(taxi)

library("heatmaply")
heatmaply(taxi)
