library(here)
library(rgdal)
library(dplyr)
library(gdata)
library(geojsonio)

# Returns a matrix that represents the taxi trip flow between boroughs
get.boroughs.matrix <- function(df, zones) {
  # Get boroughs
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

# Returns a data frame where odd rows represent origins and even rows, destinations
get.trips <- function(df, coord) {
  hours <- 0:23
  
  df$tpep_pickup_datetime <- as.numeric(
    format(as.POSIXct(df$tpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S"),
           "%H"))
  
  df$Orlng <- coord[df$PULocationID, 1]
  df$Orlat <- coord[df$PULocationID, 2]
  df$Dstlng <- coord[df$DOLocationID, 1]
  df$Dstlat <- coord[df$DOLocationID, 2]
  
  dfs <- df %>% 
    subset(PULocationID != 265 & DOLocationID != 265 & 
                         PULocationID != 264 & DOLocationID != 264) %>% 
    split(~tpep_pickup_datetime)
  
  
  agg_dfs <- lapply(dfs, function(i) {
    aggregate(amount ~ Orlng + Orlat + Dstlng + Dstlat, i, sum)
  })

  return(agg_dfs)
}

# Returns all data the Shiny app needs
get.data <- function() {
  if (!file.exists(here("data.RData"))) {
    taxi_data <- read.csv(here("..", "data", "yellow_tripdata_2019-01.csv"))
    taxi_data$amount <- rep(1, nrow(taxi_data)) # Auxiliary column
    
    taxi_zones <- read.csv(here("..", "data", "taxi+_zone_lookup.csv"))
  
    mtrx_boroughs <- get.boroughs.matrix(taxi_data, taxi_zones)
  
    shape <- spTransform(
      readOGR(here("..", "data", "taxi_zones.shp")),
      CRS("+proj=longlat +datum=WGS84 +no_defs")
    )

    coord <- as.data.frame(coordinates(shape), row.names = 1:nrow(shape))
    
    shapeData <- geojson_json(shape)
    
    coord$zone <- taxi_zones$Zone[1:263]
    
    trips <- get.trips(taxi_data, coord)
    save(mtrx_boroughs, shapeData, trips, coord = coord, file = "data.RData")
  }
  else {
   load("data.RData") 
  }
  
  return(list(
    mtrx_boroughs = mtrx_boroughs, shapeData = shapeData,
    trips = trips, coord = coord
  ))
}
