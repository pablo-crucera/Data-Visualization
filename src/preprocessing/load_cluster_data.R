library(caret)
library(cluster)
library(dplyr)
library(tidyr)
library(purrr)
library(factoextra) 
library(RColorBrewer)
library(GGally)
library(rlang)
library(data.table)
library(here)
library(gdata)


get.data <- function() {
  taxi <- read.csv(here("data", "yellow_tripdata_2019-01.csv"))
  taxi_zones <- read.csv(here("data","taxi+_zone_lookup.csv"))
  taxi <- sample_n(taxi, 10000)
  taxi <- subset(taxi, select = -congestion_surcharge)
  
  # Deal with the datetime columns and create a date and time column
  # for pick up and drop off
  
  # Pick up
  taxi <-
    taxi %>% separate(tpep_pickup_datetime,
                      c("date_pickup", "time_pickup"),
                      " ")
  taxi <- 
    taxi %>%
    mutate(date_pickup=lapply(date_pickup, as.character))
  
  
  taxi[["date_pickup"]] <- strptime(taxi[["date_pickup"]],
                                    format = "%Y-%m-%d")
  taxi <- 
    taxi %>%
    mutate(date_pickup=lapply(date_pickup, as.numeric))
  
  taxi <- 
    taxi %>%
    mutate(time_pickup=lapply(time_pickup, as.character))
  
  
  taxi[["time_pickup"]] <- strptime(taxi[["time_pickup"]],
                                    format = "%H:%M:%S")
  taxi <- 
    taxi %>%
    mutate(time_pickup=lapply(time_pickup, as.numeric))
  
  
  # Drop off
  taxi <-
    taxi %>% separate(tpep_dropoff_datetime,
                      c("date_dropoff", "time_dropoff"),
                      " ")
  taxi <- 
    taxi %>%
    mutate(date_dropoff=lapply(date_dropoff, as.character))
  
  
  taxi[["date_dropoff"]] <- strptime(taxi[["date_dropoff"]],
                                     format = "%Y-%m-%d")
  taxi <- 
    taxi %>%
    mutate(date_dropoff=lapply(date_dropoff, as.numeric))
  
  taxi <- 
    taxi %>%
    mutate(time_dropoff=lapply(time_dropoff, as.character))
  
  
  taxi[["time_dropoff"]] <- strptime(taxi[["time_dropoff"]],
                                     format = "%H:%M:%S")
  taxi <- 
    taxi %>%
    mutate(time_dropoff=lapply(time_dropoff, as.numeric))
  
  taxi <-
    transform(
      taxi,
      time_dropoff = as.numeric(time_dropoff),
      date_dropoff = as.numeric(date_dropoff),
      time_pickup = as.numeric(time_pickup ),
      date_pickup  = as.numeric(date_pickup )
    )
  
  # Convert categorical to nominal 
  # VendorID -> binary: I can leave it like this
  taxi$VendorID<-(taxi$VendorID-1)
  
  # RatecodeID -> one hot encoding
  taxi <-
    transform(
      taxi,
      RatecodeID = as.factor( RatecodeID )
    )
  
  dmy <- dummyVars(" ~ .", data = taxi)
  taxi <- data.frame(predict(dmy, newdata = taxi))
  
  
  
  # store_and_fwd_flag -> already converted to binary
  
  # PULocation and DOLocation transform into one hot encoding by districts
  zone_borough <- unclass(factor(taxi_zones$Borough))
  boroughs <- attr(x = zone_borough, which = "levels")
  
  
  taxi$boroughOr <-  boroughs[zone_borough[taxi$PULocationID]]
  taxi$boroughDst <- boroughs[zone_borough[taxi$DOLocationID]]
  
  taxi <- subset(taxi, select = -c(PULocationID,DOLocationID))
  
  taxi <-
    transform(
      taxi,
      boroughOr = as.factor( boroughOr ),
      boroughDst = as.factor( boroughDst )
    )
  
  dmy <- dummyVars(" ~ .", data = taxi)
  taxi <- data.frame(predict(dmy, newdata = taxi))
  
  
  # payment_type one hot encoding
  taxi <-
    transform(
      taxi,
      payment_type = as.factor( payment_type )
    )
  
  dmy <- dummyVars(" ~ .", data = taxi)
  taxi <- data.frame(predict(dmy, newdata = taxi))
  
  
  # scale data
  
  taxi[c("date_pickup", "time_pickup", "date_dropoff",
         "time_dropoff", "passenger_count", "trip_distance",
         "fare_amount", "extra", "mta_tax", "tip_amount",
         "tolls_amount", "improvement_surcharge", 
         "total_amount")] <- 
    apply(taxi[c("date_pickup", "time_pickup", "date_dropoff",
                 "time_dropoff", "passenger_count", "trip_distance",
                 "fare_amount", "extra", "mta_tax", "tip_amount",
                 "tolls_amount", "improvement_surcharge", 
                 "total_amount")], MARGIN =2, FUN = function(X) (X - min(X))/diff(range(X)))
  
  # After analyzing the variables remove the unnecessary ones
  
  taxi <-select(taxi,-c(date_pickup,date_dropoff,improvement_surcharge,
                        mta_tax,RatecodeID.1,RatecodeID.2,RatecodeID.3,
                        RatecodeID.4, RatecodeID.5, VendorID, store_and_fwd_flag.N,store_and_fwd_flag.Y))
  
  save(taxi, file = here("data", "clustering.RData"))
}

get.data()
