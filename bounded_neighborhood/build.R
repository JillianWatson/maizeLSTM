library(sf)
library(Matrix)
library(tidyverse)
library(torch)

source("~/maizeLSTM/data_wrangling/data_impute.R")

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
        lat1 <- coordinates[i, "Impute_lat"]
        long1 <- coordinates[i, "Impute_long"]
        lat2 <- coordinates[j, "Impute_lat"]
        long2 <- coordinates[j, "Impute_long"]
        
        #latitude kilometers per DD
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
      
      adj_matrix[i, connected_nbrs] <- 1
    }
  }
  
  return(adj_matrix)
}
  

#number of connections
k <- c(3,4,5,7,9,10)

distance_thresholds <- c(50, 100, 150,200)

adj_matrices <- list()
results_df <- data.frame()

#run distance network for different values of k and distance
#TODO: store results for each matrix created, run metrics
for (value in k) {
  for (dist in distance_thresholds) {
    adj_matrix <- create_distance_network(
      coordinates,
      k_max = value,
      distance_threshold = dist
    )
    
  }  
}
  
  #for gnn
  edge_index <- which(adj_matrix == 1, arr.ind = TRUE) %>%
    t() %>%
    torch_tensor()
  
  #store results
  knn_results[[as.character(value)]] <- knn_
  adj_matrices[[as.character(value)]] <- adj_matrix
  
  #evaluate knn for each k
  avg_connections <- mean(rowSums(adj_matrix))
  isolated_locations <- sum(rowSums(adj_matrix) <= 1)
  
  #print results for each k
  cat(sprintf("\nResults for k = %d: ", value))
  cat(sprintf("\nAverage connections per location: %.2f ", avg_connections))
  cat(sprintf("\nNumber of isolated locations: %d\n ",isolated_locations ))
}



