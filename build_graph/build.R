library(sf)
library(Matrix)
library(tidyverse)
library(torch)


meta_data <- readRDS("data_wrangling/processing_meta_train.rds")
joined_dataset <- readRDS("feature_engineering/joined_dataset.rds")

valid_locations <- unique(joined_dataset$Location)

meta_data <- meta_data %>% rename(Location = SpatialLoc)

valid_metadata <- meta_data %>% filter(Location %in% valid_locations)

coordinates <- valid_metadata %>% 
  ungroup() %>%
  distinct(Impute_lat, Impute_long, .keep_all = TRUE) %>%
  select(Impute_lat, Impute_long, Location)



# Helper function to calculate real distances (in km) between
  # 2 geographical points. 
calculate_distance_km <- function(lat1, long1, lat2, long2) {
  dist_lat <- abs(lat1 - lat2) * 110.574 
  avg_lat <- (lat1 + lat2) / 2
  dist_long <- abs(long1 - long2) * (111.320 * cos(avg_lat * pi/180))
  return(sqrt(dist_lat^2 + dist_long^2))
}



# Function to cluster weather stations based on proximity
  #coordinates: df that contains lat x long coords of weather stations, units:Decimal Distance
  #threshold: distance (float) in km that determines if a location belongs to a cluster
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
          #add location to cluster if it falls in threshold
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
  # coordinates: lat x long coords of weather stations, units:Decimal Distance
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
      connected_nbrs <- sorted_nbrs[1:min(k_max, length(sorted_nbrs))]
    
      #create connections  
      adj_matrix[i, connected_nbrs] <- 1
    }
  }
  
  return(adj_matrix)
}


#------------------ Main Execution ------------------#  

#Create clusters from individual stations
cat("Performing station clustering...\n")
cluster_threshold_km <- 25.0  
clustering_results <- cluster_weather_stations(coordinates, cluster_threshold_km)

cat("Created", nrow(clustering_results$cluster_centroids), "clusters from", nrow(coordinates), "stations\n")

# Display some cluster statistics
cluster_sizes <- table(clustering_results$station_to_cluster$cluster_id)
cat("Cluster sizes (stations per cluster):\n")
print(cluster_sizes)

#create network btwn clusters
#number of connections to try
k <- c(4,10,15,20,25)

distance_thresholds <- c(50.0, 100.0, 150.0, 200.0) #represented as kilometers

adj_matrices <- list()
results_df <- data.frame()

#run distance network for different values of k and distance threshold
cat("\nCreating distance networks between clusters...\n")
for (value in k) {
  for (dist in distance_thresholds) {
    adj_matrix <- create_distance_network(
      clustering_results$cluster_centroids,
      k_max = value,
      distance_threshold = dist
    )
    matrix_key <- paste("k", value, "dist", dist, sep = "_")
    adj_matrices[[matrix_key]] <- adj_matrix
    
    #evaluate connections
    avg_connections <- mean(rowSums(adj_matrix))
    isolated_clusters <- sum(rowSums(adj_matrix) == 0)
    
    results_df <- rbind(results_df, data.frame(
      k = value,
      distance_thresholds = dist,
      avg_connections = avg_connections,
      isolated_clusters = isolated_clusters
    ))
    
    #print results for each k
    cat(sprintf("\nResults for k = %d , distance threshold = %d km: ", value, dist))
    cat(sprintf("\nAverage connections per location: %.2f ", avg_connections))
    cat(sprintf("\nNumber of isolated locations: %d\n ", isolated_locations ))
    
  }  
}

# Create directory if it doesn't exist
if (!dir.exists("build_graph")) {
  dir.create("build_graph", recursive = TRUE)
}
  
#save results
saveRDS(adj_matrices, "build_graph/adj_matrices.rds")
saveRDS(results_df, "build_graph/results_df.rds")
saveRDS(coordinates, "build_graph/coordinates.rds")
saveRDS(clustering_results$cluster_centroids, "build_graph/cluster_centroids.rds")
saveRDS(clustering_results$station_to_cluster, "build_graph/station_to_cluster.rds")
saveRDS(clustering_results$clusters, "build_graph/clusters.rds")


#for gnn
#selected_adj_matrix <- adj_matrices[["k_10_dist_150"]]

edge_index <- which(selected_adj_matrix == 1, arr.ind = TRUE) %>%
  t() %>%
  torch_tensor()

saveRDS(edge_index, "build_graph/edge_index.rds")
  
  





