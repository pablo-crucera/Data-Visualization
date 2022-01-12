library(here)
library(visNetwork)

# Load data
initial.data <- function() {
  taxi_data <- read.csv(here("data", "yellow_tripdata_2019-01.csv"))
  taxi_zones <- read.csv(here("data", "taxi+_zone_lookup.csv"))
  
  # Get boroughs
  zone_borough <- unclass(factor(taxi_zones$Borough))
  boroughs <- attr(x = zone_borough, which = "levels")

  # Add auxiliary columns
  taxi_data$trips <- rep(1, nrow(taxi_data))
  taxi_data$boroughOr <- zone_borough[taxi_data$PULocationID]
  taxi_data$boroughDst <- zone_borough[taxi_data$DOLocationID]

  return(list(df = taxi_data, zones = taxi_zones, boroughs = boroughs))
}

# Get adjacency matrix
get.matrix <- function(df, origin, dest, places) {
  trips_df <- aggregate(formula(paste0("trips ~ ", origin, " + ", dest)), df, sum)
  mtrx <- matrix(0, nrow = length(places), ncol = length(places))
  mtrx[as.matrix(trips_df[, c(1, 2)])] <- trips_df$trips
  dimnames(mtrx) <- list(have = places, prefer = places)
 
  return(mtrx)
}
