library(here)
library(rgdal)
library(dplyr)
library(gdata)
library(geojsonio)
library(dttr2)

# TODO: Unify shapes loading with choropleth maps?

# Returns a matrix that represents the taxi trip flow between boroughs
get.boroughs.matrix <- function(df, zones) {
  zone_borough <- unclass(factor(zones$Borough))
  boroughs <- attr(x = zone_borough, which = "levels")

  df$boroughOr <- zone_borough[df$PULocationID]
  df$boroughDst <- zone_borough[df$DOLocationID]

  agg_df <- aggregate(amount ~ boroughOr + boroughDst, df, sum)
  mtrx <- matrix(0, nrow = length(boroughs), ncol = length(boroughs))
  mtrx[as.matrix(agg_df[, c(1, 2)])] <- agg_df$amount
  dimnames(mtrx) <- list(have = boroughs, prefer = boroughs)

  return(mtrx)
}

# Returns all data needed for the flow map
get.data <- function() {
  taxi_data <- read.csv(here("data", "yellow_tripdata_2019-01.csv"))
  taxi_data$amount <- rep(1, nrow(taxi_data)) # Auxiliary column

  # Add pick up hour and pick up date
  taxi_data$pickup_hour <- as.character(
    format(
      as.POSIXct(taxi_data$tpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S"),
      "%H"
    )
  )
  lc_time <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_US.UTF-8") # Needed to get week days in English
  taxi_data$pickup_day <- dtt_wday(as.POSIXct(taxi_data$tpep_pickup_datetime,
    format = "%Y-%m-%d %H:%M:%S"
  ))
  Sys.setlocale("LC_TIME", lc_time)

  shape <- spTransform(
    readOGR(here("data", "taxi_zones.shp")),
    CRS("+proj=longlat +datum=WGS84 +no_defs")
  )
  shapeData <- geojson_json(shape)

  # Add pick up and drop off zones coordinates
  coord <- as.data.frame(coordinates(shape), row.names = 1:nrow(shape))
  taxi_data$Orlng <- coord[taxi_data$PULocationID, 1]
  taxi_data$Orlat <- coord[taxi_data$PULocationID, 2]
  taxi_data$Dstlng <- coord[taxi_data$DOLocationID, 1]
  taxi_data$Dstlat <- coord[taxi_data$DOLocationID, 2]

  # Remove unknown zones and split per day and hour
  dfs <- taxi_data %>%
    subset(PULocationID != 265 & DOLocationID != 265 &
      PULocationID != 264 & DOLocationID != 264) %>%
    split(~ pickup_hour + pickup_day)

  # Get boroughs trip flow matrices
  taxi_zones <- read.csv(here("data", "taxi+_zone_lookup.csv"))
  mtrcs_boroughs <- lapply(dfs, get.boroughs.matrix, taxi_zones)

  # Get zone trip flow data frames
  trips <- lapply(dfs, function(i) {
    aggregate(amount ~ Orlng + Orlat + Dstlng + Dstlat, i, sum)
  })

  # Create busy index
  time_periods <- names(trips)
  max_amount <- max(unlist(lapply(trips, function(i) {
    max(i$amount)
  })))
  busy_list <- lapply(trips, function(i) {
    i$amount / max_amount
  })
  names(busy_list) <- rep("busy_index", length(busy_list))
  trips <- lapply(1:length(trips), function(i) {
    cbind(
      trips[[i]][, ],
      busy_list[i]
    )
  })
  names(trips) <- time_periods

  save(mtrcs_boroughs, shapeData, trips, coord,
    file = here("data", "trips_flow.RData")
  )
}

get.data()
