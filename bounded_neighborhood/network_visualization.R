library(ggplot2)
library(tidyverse)
library(sf)
library(Matrix)
library(gridExtra)

adj_matrices <- readRDS("adj_matrices.rds")
results_df <- readRDS("results_df.rds")
coordinates <- readRDS("coordinates.rds")


plot_network_metrics <- function(results_df) {
  
  #avg connections
  p1 <- ggplot(results_df, aes(x = k, y = avg_connections, colour = factor(distance_thresholds))) + 
    geom_line() +
    geom_point() + 
    theme_minimal() + 
    labs(
      title = "Average Connections vs value of K",
      x = "K (# of neighbours)",
      y = "Average Connections",
      color = "Distance\nThreshold (km)"
    ) + 
    theme(legend.position = "right")
  
  #plot isolated locations
  p2 <- ggplot(results_df, aes(x = k, y = isolated_locations, color = factor(distance_thresholds))) + 
    geom_line() +
    geom_point() + 
    theme_minimal() + 
    labs(
      title = "Isolated Locations vs value of K",
      x = "K (# of neighbours)",
      y = "Number of Isolated Locations",
      color = "Distance\nThreshold (km)"
    ) + 
    theme(legend.position = "right")
  
  grid.arrange(p1, p2, ncol = 1)
}

#visualize 2d graphical network of weather station locations provided a specific value of k and distance threshold
plot_spatial_network <- function(coordinates, adj_matrix, k, dist_threshold) {
  
  nodes <- data.frame(
    id = 1:nrow(coordinates),
    lat = coordinates$Impute_lat,
    long = coordinates$Impute_long
  )
  
  edges <- which(adj_matrix == 1, arr.ind = TRUE) %>%
    as.data.frame() %>%
    rename(from = row, to = col) %>%
    left_join(nodes, by = c("from" = "id")) %>%
    rename(from_lat = lat, from_long = long) %>%
    left_join(nodes, by = c("to" = "id")) %>%
    rename(to_lat = lat, to_long = long)
  
  ggplot() + 
    geom_segment(data = edges,
                aes(x = from_long, y = from_lat,
                    xend = to_long, yend = to_lat),
                alpha = 0.3) + 
    geom_point(data = nodes,
               aes(x = long, y = lat),
               color = "deepskyblue",
               size = 2) +
    theme_minimal() + 
    labs(
      title = sprintf("Spatial Network (k=%d, dist_threshold=%d km)", k, dist_threshold),
      x = "Longitude",
      y = "Latitude"
    ) + 
    coord_fixed()
  
}

#heatmap of connectivity
plot_connectivity_heatmap <- function(results_df) {
  ggplot(results_df, aes(x = factor(k), y = factor(distance_thresholds), fill = avg_connections)) + 
    geom_tile() + 
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(
      title = "Connectivity Heatmap",
      x = "K(number of neighours)",
      y = "Distance Threshold (km)",
      fill = "Average\n Connections"
    )
}

#plot_network_metrics(results_df)

#select which values of k and dist to visualize
adj_matrix <- adj_matrices[["k_10_dist_100"]]
#plot_spatial_network(coordinates, adj_matrix, k = 10, dist_threshold = 100)

plot_connectivity_heatmap(results_df)
