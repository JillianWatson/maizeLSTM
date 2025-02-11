library(sf)
library(Matrix)
library(tidyverse)
library(torch)

processing_meta_train <- readRDS("processing_meta_train.rds")

coordinates <- processing_meta_train %>% 
  ungroup() %>%
  distinct(Impute_lat, Impute_long, .keep_all = TRUE) %>%
  select(Impute_lat, Impute_long)

create_distance_network <- function(coordinates, k_max, distance_threshold) {
  n_locations <- nrow(coordinates)
  dist_matrix <- matrix(0, nrow = n_locations, ncol = n_locations)
  
  #location 1
  for (i in 1:n_locations) {
    #location 2
    for (j in 1:n_locations) {
      if (i != j) {
        
        lat1 <- coordinates$Impute_lat[i]
        long1 <- coordinates$Impute_long[i]
        lat2 <- coordinates$Impute_lat[j]
        long2 <- coordinates$Impute_long[j]
        
        #latitude kilometers per Decimal Distance (DD)
        dist_lat <- abs(lat1 - lat2) * 110.574 
        avg_lat <- (lat1 + lat2) / 2
        #longitude kilometers per DD
        dist_long <- abs(long1 - long2) * (111.320 * cos(avg_lat * pi/180))
        
        #calculate total distance
        dist_matrix[i,j] <- sqrt(dist_lat^2 + dist_long^2)
      }
    }
  }
  
  #initialize adjacency matrix
  adj_matrix <- Matrix(0, nrow = n_locations, ncol = n_locations, sparse = TRUE)

  for (i in 1:n_locations) {
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
  

#number of connections to try
k <- c(4,10,15,20,25)

distance_thresholds <- c(50, 100, 150,200) #represented as kilometers

adj_matrices <- list()
results_df <- data.frame()

#run distance network for different values of k and distance
for (value in k) {
  for (dist in distance_thresholds) {
    adj_matrix <- create_distance_network(
      coordinates,
      k_max = value,
      distance_threshold = dist
    )
    matrix_key <- paste("k", value, "dist", dist, sep = "_")
    adj_matrices[[matrix_key]] <- adj_matrix
    
    #evaluate connections
    avg_connections <- mean(rowSums(adj_matrix))
    isolated_locations <- sum(rowSums(adj_matrix) == 0)
    
    results_df <- rbind(results_df, data.frame(
      k = value,
      distance_thresholds = dist,
      avg_connections = avg_connections,
      isolated_locations = isolated_locations
    ))
    
    #print results for each k
    cat(sprintf("\nResults for k = %d , distance threshold = %d km: ", value, dist))
    cat(sprintf("\nAverage connections per location: %.2f ", avg_connections))
    cat(sprintf("\nNumber of isolated locations: %d\n ", isolated_locations ))
    
  }  
}
  
#for visualization file
saveRDS(adj_matrices, "bounded_neighborhood/adj_matrices.rds")
saveRDS(results_df, "bounded_neighborhood/results_df.rds")
saveRDS(coordinates, "bounded_neighborhood/coordinates.rds")


#for gnn
selected_adj_matrix <- adj_matrices[["k_10_dist_150"]]

edge_index <- which(selected_adj_matrix == 1, arr.ind = TRUE) %>%
  t() %>%
  torch_tensor()

saveRDS(edge_index, "bounded_neighborhood/edge_index.rds")
  
  





