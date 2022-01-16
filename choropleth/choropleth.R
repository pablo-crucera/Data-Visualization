library(tidyr)
library(dplyr)

# Preprocesses data for tips visualization
preprocessTips <- function(taxis) {
  
  # Select only those who payed by credit card
  creditCardTrips <- taxis %>% 
    select(PULocationID, DOLocationID, payment_type, tip_amount) %>% 
    filter(payment_type == 1)
  
  
  # Calculate mean per Pick-up location ID
  tipMnOrig <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$PULocationID), 
                         FUN=mean) %>% 
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipMnOrig <- tipMnOrig[1:263,]
  
  # Calculate mean per Drop-off location ID
  tipMnDest <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$DOLocationID), 
                         FUN=mean) %>%
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipMnDest <- tipMnDest[1:263,]
  
  # Calculate median per Pick-up location ID
  tipMdOrig <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$PULocationID), 
                         FUN=median) %>% 
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipMdOrig <- tipMdOrig[1:263,]
  
  # Calculate median per Drop-off location ID
  tipMdDest <- aggregate(x=creditCardTrips$tip_amount, 
                         by=list(creditCardTrips$DOLocationID), 
                         FUN=median) %>%
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  tipMdDest <- tipMdDest[1:263,]
  
  return(list(tipMnOrig = tipMnOrig, tipMnDest = tipMnDest,
              tipMdOrig = tipMdOrig, tipMdDest = tipMdDest))
}

# Preprocesses data for distance length visualization
preprocessDist <- function(taxis) {
  
  # Collect only required columns for faster execution
  subsetTrips <- taxis %>% 
    select(PULocationID, DOLocationID, trip_distance)
  
  # Calculate mean per Pick-up location ID
  distMnOrig <- aggregate(x=subsetTrips$trip_distance, 
                          by=list(subsetTrips$PULocationID), 
                          FUN=mean) %>% 
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  distMnOrig <- distMnOrig[1:263,]
  
  # Calculate mean per Drop-off location ID
  distMnDest <- aggregate(x=subsetTrips$trip_distance, 
                          by=list(subsetTrips$DOLocationID), 
                          FUN=mean) %>%
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  distMnDest <- distMnDest[1:263,]
  
  # Calculate median per Pick-up location ID
  distMdOrig <- aggregate(x=subsetTrips$trip_distance, 
                          by=list(subsetTrips$PULocationID), 
                          FUN=median) %>% 
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  distMdOrig <- distMdOrig[1:263,]
  
  # Calculate median per Drop-off location ID
  distMdDest <- aggregate(x=subsetTrips$trip_distance, 
                          by=list(subsetTrips$DOLocationID), 
                          FUN=median) %>%
    complete(Group.1 = first(Group.1):max(Group.1), fill = list(x = NA)) 
  
  distMdDest <- distMdDest[1:263,]
  
  return(list(distMnOrig = distMnOrig, distMnDest = distMnDest,
              distMdOrig = distMdOrig, distMdDest = distMdDest))
}


# Preprocesses data for dispute visualization
preprocessDispute <- function(taxis) {
  
  # Calculate occurrences of a taxi zone ID as Pick-up location
  tripCountOrig <- tabulate(taxis$PULocationID)
  tripCountOrig <- tripCountOrig[1:263]
  
  # Calculate occurrences of a taxi zone ID as Drop-off location
  tripCountDest <- tabulate(taxis$DOLocationID)
  tripCountDest <- tripCountDest[1:263]
  
  
  subsetTrips <- taxis %>%
    filter(payment_type == 4)
  
  # Calculate occurrences of disputes in a taxi zone ID as Pick-up location
  dispCountOrig <- tabulate(subsetTrips$PULocationID)
  dispCountOrig <- dispCountOrig[1:263]
  
  # Calculate occurrences of disputes in a taxi zone ID as Drop-off location
  dispCountDest <- tabulate(subsetTrips$DOLocationID)
  dispCountDest <- dispCountDest[1:263]
  
  # Calculate percentage of disputes in a taxi zone ID
  percOrig <- dispCountOrig / tripCountOrig * 100
  percDest <- dispCountDest / tripCountDest * 100
  
  # Set those values divided by 0 by 'not available' values
  percOrig[sapply(percOrig, is.infinite)] <- NA
  percDest[sapply(percDest, is.infinite)] <- NA
  
  return(list(percOrig = percOrig, percDest = percDest))
}