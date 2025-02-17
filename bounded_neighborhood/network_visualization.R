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

# Function to visualize 2D graphical network of weather station locations
# provided by a specific value of k and distance threshold
#   coordinates: latitude x longitude of weather stations, units:Decimal Distance
#   adj_matrix:
#   k: value representing max number of connections to 
#   dist_threshold: 
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

# Function to plot a heat map of connectivity
#   results_df: df that contains all attempts of station network building
#               for many specified values of k and distance threshold. 
#               Imported from bounded_neighborhood/build.R file. 
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


#select which values of k and dist to visualize
#from analyses, k=10 and dist=100 provide satisfactory results
adj_matrix <- adj_matrices[["k_10_dist_100"]]



