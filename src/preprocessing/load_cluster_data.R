library(cluster)
library(dplyr)
library(tidyr)
library(purrr)
library(factoextra) # clustering algorithms & visualization
library(GGally)
library(rlang)
library(data.table)
library(here)
library(gdata)


get.data <- function() {
  load("nsample.RData")

  taxi <-
    taxi %>% separate(
      tpep_dropoff_datetime,
      c("date_dropoff", "time_dropoff"),
      " "
    )

  taxi <-
    taxi %>% separate(
      tpep_pickup_datetime,
      c("date_pickup", "time_pickup"),
      " "
    )

  ############ pickup
  taxi <-
    taxi %>%
    mutate(date_pickup = lapply(date_pickup, as.character))


  # converting to datetime object
  taxi[["date_pickup"]] <- strptime(taxi[["date_pickup"]],
    format = "%Y-%m-%d"
  )
  taxi <-
    taxi %>%
    mutate(date_pickup = lapply(date_pickup, as.numeric))

  taxi <-
    taxi %>%
    mutate(time_pickup = lapply(time_pickup, as.character))


  # converting to datetime object
  taxi[["time_pickup"]] <- strptime(taxi[["time_pickup"]],
    format = "%H:%M:%S"
  )
  taxi <-
    taxi %>%
    mutate(time_pickup = lapply(time_pickup, as.numeric))

  ##### dropoff
  taxi <-
    taxi %>%
    mutate(date_dropoff = lapply(date_dropoff, as.character))


  # converting to datetime object
  taxi[["date_dropoff"]] <- strptime(taxi[["date_dropoff"]],
    format = "%Y-%m-%d"
  )
  taxi <-
    taxi %>%
    mutate(date_dropoff = lapply(date_dropoff, as.numeric))

  taxi <-
    taxi %>%
    mutate(time_dropoff = lapply(time_dropoff, as.character))


  # converting to datetime object
  taxi[["time_dropoff"]] <- strptime(taxi[["time_dropoff"]],
    format = "%H:%M:%S"
  )
  taxi <-
    taxi %>%
    mutate(time_dropoff = lapply(time_dropoff, as.numeric))

  # Convert categorical to nominal
  taxi$store_and_fwd_flag <- sapply(taxi$store_and_fwd_flag, unclass)

  taxi <-
    transform(
      taxi,
      time_dropoff = as.numeric(time_dropoff),
      date_dropoff = as.numeric(date_dropoff),
      time_pickup = as.numeric(time_pickup),
      date_pickup  = as.numeric(date_pickup),
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

  save(taxi, file = here("data", "clustering.RData"))
}

get.data()
