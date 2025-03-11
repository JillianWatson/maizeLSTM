library(sf)
library(Matrix)
library(tidyverse)


meta_data <- readRDS("data_wrangling/processing_meta_train.rds")
joined_dataset <- readRDS("feature_engineering/joined_dataset.rds")

valid_locations <- unique(joined_dataset$Location)

meta_data <- meta_data %>% rename(Location = SpatialLoc)

valid_metadata <- meta_data %>% filter(Location %in% valid_locations)

coordinates_duplicates <- valid_metadata %>% 
  ungroup() %>%
  distinct(Impute_lat, Impute_long, .keep_all = TRUE) %>%
  select(Impute_lat, Impute_long, Location)

cat("Original coordinates count:", nrow(coordinates_duplicates), "\n")

#filter coordinates df to select 1 representative coordinate pair
# per each Location ID
coordinates_filtered <- coordinates_duplicates %>%
  group_by(Location) %>%
  summarise(
    Impute_lat = median(Impute_lat),
    Impute_long = median(Impute_long)
  ) %>%
  ungroup()

cat("Reduced by:", nrow(coordinates_duplicates) - nrow(coordinates_filtered), "entries\n")


# Helper function to calculate distances (in km) between
  # 2 geographical points. 
calculate_distance_km <- function(lat1, long1, lat2, long2) {
  dist_lat <- abs(lat1 - lat2) * 110.574 
  avg_lat <- (lat1 + lat2) / 2
  dist_long <- abs(long1 - long2) * (111.320 * cos(avg_lat * pi/180))
  return(sqrt(dist_lat^2 + dist_long^2))
}



# Function to cluster weather stations based on distance proximity
  #coordinates: filtered df that contains lat x long coords of weather stations, units:Decimal Distance
  #threshold: distance (int) in km that determines if a location belongs to a cluster
cluster_weather_stations <- function(coordinates, threshold) {
  n_stations <- nrow(coordinates)
  
  #track which stations are assigned to clusters
  assigned <- rep(FALSE, n_stations)
  
  clusters <- list()
  
  for (i in 1:n_stations) {
    if (!assigned[i]) {
      #create new cluster if we haven't visited this location
      current_cluster <- c(i)
      assigned[i] <- TRUE
    
      for (j in 1:n_stations) {
        if (!assigned[j] && i != j) {
          total_distance <- calculate_distance_km(
            coordinates$Impute_lat[i],
            coordinates$Impute_long[i],
            coordinates$Impute_lat[j],
            coordinates$Impute_long[j]
          )
          #add location to cluster if it falls within threshold
          if (total_distance <= threshold) {
            current_cluster <- c(current_cluster, j)
            assigned[j] <- TRUE
          }
        }
      }
      clusters[[length(clusters) + 1]] <- current_cluster
    }
  }
  #cluster centroids dataframe
  cluster_centroids <- data.frame(
    Cluster_id = 1:length(clusters),
    Latitude = numeric(length(clusters)),
    Longitude = numeric(length(clusters)),
    Station_count = sapply(clusters, length)
  )
  #calculate average position (centroid)
  for (i in 1:length(clusters)) {
    stations_in_cluster <- clusters[[i]]
    cluster_centroids$Latitude[i] <- mean(coordinates$Impute_lat[stations_in_cluster])
    cluster_centroids$Longitude[i] <- mean(coordinates$Impute_long[stations_in_cluster])
  }
  #track original stations to clusters (as dataframe)
  station_to_cluster <- data.frame(
    Station_id = 1:n_stations,
    Original_index = 1:n_stations,
    Location = coordinates$Location,
    Cluster_id = NA
  )
  
  for (i in 1:length(clusters)) {
    station_to_cluster$Cluster_id[clusters[[i]]] <- i
  }
  
  return(list(
    clusters = clusters,
    cluster_centroids = cluster_centroids,
    station_to_cluster = station_to_cluster
  ))
}

  
  
# Function to generate distance network between clusters
  # coordinates: filtered lat x long coords of weather stations, units:Decimal Distance
  # k_max: an array of integers representing the max amount of connections 
  #        a node can have
  # distance_threshold: the upper limit condition that allows 2 nodes to form
  #                     a connection
create_distance_network <- function(coordinates, k_max, distance_threshold) {
  n_nodes <- nrow(coordinates)
  dist_matrix <- matrix(0, nrow = n_nodes, ncol = n_nodes)
  
  #calculate distances btwn all pairs of nodes
  for (i in 1:n_nodes) {
    for (j in 1:n_nodes) {
      if (i != j) {
        dist_matrix[i,j] <- calculate_distance_km(
          coordinates$Latitude[i],
          coordinates$Longitude[i],
          coordinates$Latitude[j],
          coordinates$Longitude[j]
        )
      } else {
        dist_matrix[i,j] <- Inf
      }
    }
  }
  
  #initialize adjacency matrix
  adj_matrix <- Matrix(0, nrow = n_nodes, ncol = n_nodes, sparse = TRUE)

  for (i in 1:n_nodes) {
    distances <- dist_matrix[i,]
    
    #find connections within distance threshold
    valid_neighbours <- which(distances <= distance_threshold & distances > 0)
    
    #make connection if found
    if(length(valid_neighbours) > 0 ) {
      #sort by distance
      sorted_nbrs <- valid_neighbours[order(distances[valid_neighbours])]
      #use nearest connections up to k_max
      k <- min(k_max, length(sorted_nbrs))
      connected_nbrs <- sorted_nbrs[1:k]
    
      #create connections  
      adj_matrix[i, connected_nbrs] <- 1
    }
  }
  #make adjacency matrix undirected
  undirected_adj_matrix <- (adj_matrix + t(adj_matrix))
  undirected_adj_matrix@x[] <- 1
  
  #calculate summary statistics
  connections_per_node <- rowSums(undirected_adj_matrix)
  avg_conn <- mean(connections_per_node)
  min_conn <- min(connections_per_node)
  max_conn <- max(connections_per_node)
  isolated <- sum(connections_per_node == 0)
  
  return(list(
    adjacency_matrix = undirected_adj_matrix,
    connections = connections_per_node,
    avg_connections = avg_conn,
    isolated_nodes = isolated
  ))
}


#Function to filter out outlier locations
#from previous analyses, a European station was located. Our geographic
#area of concern is the USA.
filter_outlier_clusters <- function(centroids) {
  
  #positive longitudinal values are indicative of European locations
  get_indices <- which(centroids$Longitude > 0) 
  
  if (length(get_indices) > 0) {
    europe_clusters <- centroids[get_indices, ]
    
    filtered_centroids <- centroids[-get_indices, ]
    return(list(
      filtered_centroids = filtered_centroids,
      removed_indices = get_indices
    ))
  } else {
    return(list(
      filtered_centroids = centroids,
      removed_indices = integer(0)
    ))
  }
}


#------------------ Main Execution ------------------#  

#create clusters from individual stations
cat("Performing station clustering...\n")
cluster_threshold_km <- 200  
clustering_results <- cluster_weather_stations(coordinates_filtered, cluster_threshold_km)

#call filtering function
cat("\nFiltering out outlier clusters\n")
filter_result <- filter_outlier_clusters(clustering_results$cluster_centroids)
filtered_centroids <- filter_result$filtered_centroids

#tracking df to map location IDs to clusters
location_cluster_mapping <- clustering_results$station_to_cluster

if (length(filter_result$removed_indices) > 0) {
  removed_cluster_ids <- filter_result$removed_indices
  #mark removed stations
  affected_stations <- which(clustering_results$station_to_cluster$Cluster_id %in% removed_cluster_ids)
  
  if (length(affected_stations) > 0) {
    cat("Removing ", length(affected_stations), "stations belonging to outlier clusters\n")
    #set Cluster_id to NA
    clustering_results$station_to_cluster$Cluster_id[affected_stations] <- NA
    location_cluster_mapping$Cluster_id[affected_stations] <- NA
  }
}
#filter out NA values 
if (any(is.na(filtered_centroids$Cluster_id))) {
  cat("Removing NA entries from filtered centroids\n")
  filtered_centroids <- filtered_centroids %>% filter(!is.na(Cluster_id))
}

cat("Filtering out stations with NA cluster IDs\n")
location_cluster_mapping <- location_cluster_mapping %>% filter(!is.na(Cluster_id))

#If outliers removed, reindex clusters to ensure consecutive IDs (no gaps)
if (length(filter_result$removed_indices) > 0) {
  cat("\nReindexing clusters to ensure consecutive IDs...\n")
  
  # Create mapping from old to new IDs
  old_cluster_ids <- filtered_centroids$Cluster_id
  new_cluster_ids <- 1:length(old_cluster_ids)
  id_mapping <- data.frame(
    old_id = old_cluster_ids,
    new_id = new_cluster_ids
  )
  
  # Update cluster IDs in centroids
  filtered_centroids$Cluster_id <- new_cluster_ids
  
  # Update cluster IDs in mapping dataframe
  location_cluster_mapping$Cluster_id <- id_mapping$new_id[match(location_cluster_mapping$Cluster_id, id_mapping$old_id)]
  
  cat("Reindexed clusters from", min(old_cluster_ids), "to", max(old_cluster_ids), 
      "now ranging from 1 to", max(new_cluster_ids), "\n")
}



#create network btwn clusters

#number of connections to try
k <- c(5, 10, 15, 20)

distance_thresholds <- c(350, 450, 500, 600, 700) #represented as kilometers

network_results <- list()
results_df <- data.frame()


#run network for different values of k and distance threshold
cat("\nCreating distance networks between clusters...\n")
for (value in k) {
  for (dist in distance_thresholds) {
    result <- create_distance_network(
      filtered_centroids,
      k_max = value,
      distance_threshold = dist
    )
    matrix_key <- paste("k", value, "dist", dist, sep = "_")
    network_results[[matrix_key]] <- result
    
    #evaluate connections
    avg_connections <- result$avg_connections
    isolated_clusters <- result$isolated_nodes
    
    results_df <- rbind(results_df, data.frame(
      k = value,
      distance_thresholds = dist,
      avg_connections = avg_connections,
      isolated_clusters = isolated_clusters
    ))
  }  
}

#print filtered cluster info
cat("\nAfter filtering, using", nrow(filtered_centroids), "valid clusters\n")

#print network results summary:
cat("\nNetwork parameters summary:\n")
print(results_df)

#create directory if it doesn't exist
if (!dir.exists("build_graph")) {
  dir.create("build_graph", recursive = TRUE)
}


#create edge index using optimized network parameters:
  #cluster threshold = 200 km
  #k_max = 10
  #distance threshold = 700 km

optimal_network <- network_results[["k_10_dist_700"]]$adjacency_matrix
edge_index <- which(optimal_network == 1, arr.ind = TRUE) %>%
  t()
  
#create a final mapping df that connects Locations to their final Cluster assignments
final_location_clusters <- location_cluster_mapping %>%
  select(Location, Cluster_id) %>%
  # Add cluster coordinates for reference
  left_join(
    filtered_centroids %>% 
      rename(Cluster_id = Cluster_id,
             Cluster_lat = Latitude,
             Cluster_long = Longitude),
    by = "Cluster_id"
  )

cat("\nLocation to cluster mapping sample (first 10 rows):\n")
print(head(final_location_clusters, 10))

#verify no NAs in the final data
cat("\nChecking for NAs in final datasets:\n")
cat("NAs in filtered_centroids:", sum(is.na(filtered_centroids)), "\n")
cat("NAs in final_location_clusters:", sum(is.na(final_location_clusters)), "\n")
cat("NAs in edge_index:", sum(is.na(edge_index)), "\n")

#save results
saveRDS(edge_index, "build_graph/edge_index_matrix.rds")
saveRDS(network_results, "build_graph/network_results.rds")
saveRDS(results_df, "build_graph/parameters_summary.rds")
saveRDS(coordinates_filtered, "build_graph/coordinates.rds")
saveRDS(filtered_centroids, "build_graph/centroids.rds")
saveRDS(final_location_clusters, "build_graph/location_cluster_mapping.rds")


#diagnostic data: cluster info before filtering
saveRDS(clustering_results, "build_graph/raw_clustering_results.rds")





