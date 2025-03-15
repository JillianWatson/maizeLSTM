library(ggplot2)
library(tidyverse)
library(sf)
library(maps)
library(viridis)
library(ggrepel)


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
location_mapping <- safe_load_rds("build_graph/location_cluster_mapping.rds", "location to cluster mapping")
coordinates <- safe_load_rds("build_graph/coordinates.rds", "filtered coordinates")

if(is.null(edge_index) || is.null(centroids) || is.null(network_results)) {
  stop("Missing required data files. Run build.R first to generate.")
}


#diagnostic output
cat("\n----- DATA SUMMARY -----\n")
cat("Clusters:", nrow(centroids), "\n")
if(!is.null(location_mapping)) {
  cat("Stations:", nrow(location_mapping), "\n")
}
cat("Network connections:", dim(edge_index)[2], "\n")
cat("-----------------------\n")


#load specific network configuration
if("k_10_dist_700" %in% names(network_results)) {
  cluster_info <- network_results[["k_10_dist_700"]]
} else {
  first_key <- names(network_results)[1]
  cat(paste0("WARNING: k_10_dist_700 configuration not found, using ", first_key, " instead\n"))
  cluster_info <- network_results[[first_key]]
}

if(any(is.na(cluster_info$connections))) {
  cat("WARNING: There are NA values in the connections data\n")
}


#convert edge index to matrix format
tryCatch({
  edge_data <- as.matrix(edge_index)
  cat("Edge data successfully converted to matrix\n")
}, error = function(e) {
  cat("ERROR: Failed to convert edge_index to matrix:", e$message, "\n")
  stop("Cannot continue without valid edge data")
})


#calculate station count per cluster
if (!is.null(location_mapping)) {
  station_counts <- location_mapping %>%
    filter(!is.na(Cluster_id)) %>%
    group_by(Cluster_id) %>%
    summarise(Station_count = n()) %>%
    ungroup()
  
  #check that all centroids have a station count
  missing_clusters <- setdiff(centroids$Cluster_id, station_counts$Cluster_id)
  if(length(missing_clusters) > 0) {
    cat("WARNING:", length(missing_clusters), "clusters don't have station counts. Adding defaults.\n")
    
    #create entries for missing clusters with Station_count = 0
    missing_counts <- data.frame(
      Cluster_id = missing_clusters,
      Station_count = 0
    )
    
    #combine with existing station counts
    station_counts <- bind_rows(station_counts, missing_counts)
  }
  
  #create a simple naming scheme based on geographical position
  centroids_with_regions <- centroids %>%
    mutate(
      region = case_when(
        Longitude < -100 & Latitude > 40 ~ "NW",
        Longitude < -100 & Latitude <= 40 ~ "SW",
        Longitude >= -100 & Longitude < -85 & Latitude > 40 ~ "NC",
        Longitude >= -100 & Longitude < -85 & Latitude <= 40 ~ "SC",
        Longitude >= -85 & Latitude > 40 ~ "NE",
        Longitude >= -85 & Latitude <= 40 ~ "SE",
        TRUE ~ "Other"
      ),
      cluster_name = paste0(region, "-", Cluster_id)
    )
  
  #diagnostic information about centroids
  cat("\nCentroids summary - check for missing values:\n")
  print(summary(centroids))
} else {
  #Fallback if location_mapping is not available
  cat("Location to cluster mapping not found, using default station count of 1 for each centroid\n")
  station_counts <- data.frame(
    Cluster_id = centroids$Cluster_id,
    Station_count = 1
  )
  
  centroids_with_regions <- centroids %>%
    mutate(
      region = case_when(
        Longitude < -100 & Latitude > 40 ~ "NW",
        Longitude < -100 & Latitude <= 40 ~ "SW",
        Longitude >= -100 & Longitude < -85 & Latitude > 40 ~ "NC",
        Longitude >= -100 & Longitude < -85 & Latitude <= 40 ~ "SC",
        Longitude >= -85 & Latitude > 40 ~ "NE",
        Longitude >= -85 & Latitude <= 40 ~ "SE",
        TRUE ~ "Other"
      ),
      cluster_name = paste0(region, "-", Cluster_id)
    )
}


#create nodes df
nodes <- data.frame(
  id = centroids_with_regions$Cluster_id,
  lat = centroids_with_regions$Latitude,
  lon = centroids_with_regions$Longitude,
  region = centroids_with_regions$region,
  cluster_name = centroids_with_regions$cluster_name
) %>%
  left_join(station_counts, by = c("id" = "Cluster_id")) 

#add connections data, ensuring IDs align correctly
if (length(cluster_info$connections) == nrow(nodes)) {
  nodes$connections <- cluster_info$connections
} else {
  cat("WARNING: Connections data length does not match nodes data length\n")
  cat("Connections length:", length(cluster_info$connections), "\n")
  cat("Nodes count:", nrow(nodes), "\n")
  
  #match by cluster ID
  conn_df <- data.frame(
    id = 1:length(cluster_info$connections),
    connections = cluster_info$connections
  )
  
  nodes <- nodes %>% 
    left_join(conn_df, by = "id")
}

if(any(is.na(nodes))) {
  cat("WARNING: There are NA values in the nodes data\n")
  na_counts <- colSums(is.na(nodes))
  print(na_counts)
}

#create edges df
tryCatch({
  edges <- data.frame(
    from = edge_data[1, ],
    to = edge_data[2, ]
  )
  cat("Created edges dataframe with", nrow(edges), "connections\n")
  
}, error = function(e) {
  cat("Warning: Error creating edges dataframe, trying alternative method\n")
  cat("Error message:", e$message, "\n")
  
  #alternative approach -> use which() to get edges from adjacency matrix
  tryCatch({
    optimal_network <- network_results[["k_10_dist_700"]]$adjacency_matrix
    
    edge_pairs <- which(optimal_network == 1, arr.ind = TRUE)
    edges <- data.frame(
      from = edge_pairs[, 1],
      to = edge_pairs[, 2]
    )
    
    cat("Created edges dataframe with alternative method:", nrow(edges), "connections\n")
  }, error = function(e2) {
    cat("Error with alternative method too:", e2$message, "\n")
    edges <- data.frame(from = integer(0), to = integer(0))
    cat("WARNING: Created empty edges dataframe to prevent errors\n")
  })
})


#join with node coordinates
edges_with_coords <- edges %>%
  left_join(nodes, by = c("from" = "id")) %>%
  rename(from_lat = lat, from_lon = lon, from_region = region, from_name = cluster_name, 
         from_stations = Station_count, from_connections = connections) %>%
  left_join(nodes, by = c("to" = "id")) %>%
  rename(to_lat = lat, to_lon = lon, to_region = region, to_name = cluster_name,
         to_stations = Station_count, to_connections = connections)

edges_with_coords <- edges_with_coords %>%
  filter(!is.na(from_lat) & !is.na(from_lon) & !is.na(to_lat) & !is.na(to_lon))

cat("After filtering, there are", nrow(edges_with_coords), "edges with valid coordinates\n")

#load USA map
us_map <- map_data("usa")


#------------------ create visualizations -------------------#


cat("Creating Network visualization...\n")


valid_nodes <- nodes %>% 
  filter(!is.na(lat) & !is.na(lon) & !is.na(Station_count) & !is.na(connections))

cat("Using", nrow(valid_nodes), "out of", nrow(nodes), "nodes with valid coordinates\n")

if(nrow(valid_nodes) == 0) {
  stop("No valid nodes to plot. Cannot create visualization.")
}

if(nrow(edges_with_coords) == 0) {
  cat("WARNING: No valid edges to plot. Will create visualization with nodes only.\n")
}

p <- ggplot() +
  #map background
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "darkgray", alpha = 0.3)

#add edges
if(nrow(edges_with_coords) > 0) {
  p <- p + 
    geom_segment(data = edges_with_coords,
                 aes(x = from_lon, y = from_lat, xend = to_lon, yend = to_lat),
                 alpha = 0.6, color = "steelblue", linewidth = 0.5)
}

#add nodes
p <- p +
  geom_point(data = valid_nodes,
             aes(x = lon, y = lat, size = Station_count, color = connections),
             alpha = 0.8)

#add labels only if nodes have multiple stations
if(any(valid_nodes$Station_count >= 2)) {
  p <- p +
    geom_text_repel(
      data = valid_nodes %>% filter(Station_count >= 2),
      aes(x = lon, y = lat, label = cluster_name),
      size = 3,
      segment.alpha = 0.6,
      min.segment.length = 0,
      max.overlaps = 20,
      box.padding = 0.5
    )
}

#scale and themes
p <- p +
  scale_color_viridis(option = "plasma", name = "Number of\nConnections") +
  scale_size_continuous(range = c(2, 7), name = "Number of\nStations within a Cluster") +
  theme_minimal() +
  labs(
    title = "Weather Station Networks Across the United States",
    subtitle = paste0(nrow(valid_nodes), " station clusters with ", sum(valid_nodes$Station_count, na.rm = TRUE), " total weather stations"),
    x = "Longitude",
    y = "Latitude",
    caption = "Clusters connected within 700km distance threshold with max 10 neighbors per cluster.\nPoint size = number of stations in cluster. Color = number of connections to other clusters."
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) 

#navigate viewport errors
p <- p + coord_fixed(ratio = 1.3, xlim = c(-125, -65), ylim = c(25, 50))

tryCatch({
  ggsave("map_network_visuals/weather_station_network_map.png", p, width = 14, height = 10, dpi = 300)
  cat("Main visualization saved as 'weather_station_network_map.png'\n")
}, error = function(e) {
  cat("ERROR: Failed to save main visualization:", e$message, "\n")
})

#print final summary information
cat("\n----- NETWORK SUMMARY -----\n")
cat("Clusters:", nrow(valid_nodes), "\n")
cat("Stations:", sum(valid_nodes$Station_count, na.rm = TRUE), "\n")
cat("Connections:", nrow(edges_with_coords), "\n")
cat("Station distribution:", 
    paste(round(summary(valid_nodes$Station_count), 1), collapse=", "), 
    "(min, Q1, median, mean, Q3, max)\n")
cat("---------------------------\n")

#region information
cat("\nClusters by Region:\n")
print(table(valid_nodes$region))

#summary of the Station_count distribution
cat("\nStation Count Distribution:\n")
print(summary(valid_nodes$Station_count))


#---------Create a secondary visualization showing Station Count Distribution----------#


if(nrow(valid_nodes) > 0) {
  p2 <- ggplot(valid_nodes, aes(x = Station_count)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = mean(Station_count, na.rm = TRUE)), 
               color = "red", linetype = "dashed", linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Distribution of Stations per Cluster",
      x = "Number of Stations in Cluster",
      y = "Count",
      caption = paste0("Mean stations per cluster: ", round(mean(valid_nodes$Station_count, na.rm = TRUE), 2))
    )
  
  tryCatch({
    ggsave("map_network_visuals/station_count_distribution.png", p2, width = 8, height = 6, dpi = 300)
    cat("Station count distribution visualization saved as 'station_count_distribution.png'\n")
  }, error = function(e) {
    cat("ERROR: Failed to save station count distribution:", e$message, "\n")
  })
  
  #--------- Create region-based visualization -----------
  
  p3 <- ggplot(valid_nodes, aes(x = region)) +
    geom_bar(aes(fill = region)) +
    theme_minimal() +
    labs(
      title = "Clusters by Geographical Region",
      x = "Region",
      y = "Count",
      caption = "NW = Northwest, NE = Northeast, SW = Southwest, SE = Southeast, NC = North Central, SC = South Central"
    ) +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  tryCatch({
    ggsave("map_network_visuals/clusters_by_region.png", p3, width = 8, height = 6, dpi = 300)
    cat("Region visualization saved as 'clusters_by_region.png'\n")
  }, error = function(e) {
    cat("ERROR: Failed to save region visualization:", e$message, "\n")
  })
} else {
  cat("WARNING: No valid nodes available for histograms or bar charts\n")
}

