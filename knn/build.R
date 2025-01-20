library(sf)
library(Matrix)
library(FNN)
library(tidyverse)
library(torch)

source("~/maizeLSTM/data_wrangling/data_impute.R")

#normalize coords

get_unique_locations <- processing_meta_train %>%
  distinct(Impute_long, Impute_lat, .keep_all = TRUE)

coordinates <- get_unique_locations %>%
  select(Impute_long, Impute_lat) %>%
  as.matrix()

k <- c(4,5,8,10,12,15)
knn_results <- list()
adj_matrices <- list()

for (value in k) {
  knn_ <- get.knn(coordinates, k=k)
  
  n_locations <- nrow(coordinates)
  adj_matrix <- matrix(0, nrow = n_locations, ncol = n_locations, sparse = TRUE)
  
  for(i in 1:n_locations) {
    neighbours <- knn_$nn.index[i,]
    adj_matrix[i, neighbours] <- 1
  }
}
