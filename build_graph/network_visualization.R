library(ggplot2)
library(tidyverse)
library(sf)
library(maps)
library(viridis)

# Function to safely load RDS files
safe_load_rds <- function(file_path, description) {
  tryCatch({
    cat(paste0("Loading ", description, "...\n"))
    readRDS(file_path)
  }, error = function(e) {
    cat(paste0("ERROR: Failed to load ", description, " from ", file_path, "\n"))
    cat(paste0("Error message: ", e$message, "\n"))
    cat("Please check that the file exists and was created correctly.\n")
    NULL
  })
}

edge_index <- safe_load_rds("build_graph/edge_index_matrix.rds", "edge index")
centroids <- safe_load_rds("build_graph/centroids.rds", "cluster centroids")
network_results <- safe_load_rds("build_graph/network_results.rds", "network results")

if(is.null(edge_index) || is.null(centroids) || is.null(network_results)) {
  stop("Missing required data files. Run build.R first to generate.")
}

#get specific network configuration
if("k_10_dist_700" %in% names(network_results)) {
  cluster_info <- network_results[["k_10_dist_700"]]
} else {
  first_key <- names(network_results)[1]
  cat(paste0("WARNING: k_10_dist_700 configuration not found, using ", first_key, " instead\n"))
  cluster_info <- network_results[[first_key]]
}

#convert edge index data to matrix format
edge_data <- as.matrix(edge_index)

#create nodes dataframe
nodes <- data.frame(
  id = 1:nrow(centroids),
  lat = centroids$Latitude,
  lon = centroids$Longitude,
  connections = cluster_info$connections
)

#create edges dataframe
tryCatch({
  #try standard approach first
  edges <- data.frame(
    from = edge_data[1, ],
    to = edge_data[2, ]
  )
}, error = function(e) {
  cat("Warning: Error creating edges dataframe, trying alternative method\n")
  #alternative approach -> use which() to get edges from adjacency matrix
  #load the optimal network adjacency matrix
  optimal_network <- readRDS("build_graph/network_results.rds")[["k_10_dist_700"]]$adjacency_matrix
  
  #convert to edge list format
  edge_pairs <- which(optimal_network == 1, arr.ind = TRUE)
  edges <- data.frame(
    from = edge_pairs[, 1],
    to = edge_pairs[, 2]
  )
})

#join with node coordinates
edges_with_coords <- edges %>%
  left_join(nodes, by = c("from" = "id")) %>%
  rename(from_lat = lat, from_lon = lon) %>%
  left_join(nodes, by = c("to" = "id")) %>%
  rename(to_lat = lat, to_lon = lon)

#get map data for USA
us_map <- map_data("usa")


cat("Creating visualization...\n")
p <- ggplot() +
  #add US map background
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "darkgray", alpha = 0.3) +
  
  #add edges (connections between stations)
  geom_segment(data = edges_with_coords,
               aes(x = from_lon, y = from_lat, xend = to_lon, yend = to_lat),
               alpha = 0.4, color = "steelblue", linewidth = 0.3) +
  
  #add nodes (weather station clusters)
  geom_point(data = nodes,
             aes(x = lon, y = lat, size = connections, color = connections),
             alpha = 0.7) +
  
  scale_color_viridis(option = "plasma", name = "Number of\nConnections") +
  
  scale_size_continuous(range = c(1, 5), name = "Number of\nConnections") +
  
  theme_minimal() +
  labs(
    title = "Weather Station Networks Across the United States",
    subtitle = paste0("Based on ", nrow(centroids), " station clusters with dynamic connectivity"),
    x = "Longitude",
    y = "Latitude",
    caption = "Clusters connected within 700km distance threshold with max 10 neighbors per cluster"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) +
  #set appropriate map limits
  coord_fixed(ratio = 1.3, xlim = c(-125, -65), ylim = c(25, 50))

ggsave("weather_station_network_map.png", p, width = 12, height = 8, dpi = 300)
cat("Visualization saved as 'weather_station_network_map.png'\n")

#summarize data
node_summary <- summary(nodes$connections)
cat("\nNetwork Summary:\n")
cat("Number of Station Clusters:", nrow(nodes), "\n")
cat("Number of Connections:", nrow(edges), "\n")
cat("Connections per Station (Min, Median, Max):", 
    min(nodes$connections), ",", median(nodes$connections), ",", max(nodes$connections), "\n")

print(p)



