load("data_cluster3.RData")

# PCA
kmeans.re <- kmeans(taxi, centers = 3, nstart = 20)
fviz_cluster(kmeans.re, data = taxi)

# Heat map
center <- kmeans.re$centers
cluster <- c(1:3)
center_df <- data.frame(cluster, center)
center_reshape <-
gather(center_df, features, values, time_pickup:boroughDst.Unknown)

# Create the palette
hm.palette <-
  colorRampPalette(rev(brewer.pal(10, 'RdYlGn')), space = 'Lab')

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


# Pair plot

taxi <- as.data.table(taxi)
taxi[, cluster := as.factor(kmeans.re$cluster)]
col <- c("total_amount", "trip_distance","tolls_amount","passenger_count")

ggpairs(taxi, aes(colour = cluster, alpha = 0.4), columns = col)


