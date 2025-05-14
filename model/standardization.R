library(tidyverse)
#use training data to reduce data leakage
#script produces z-score distributed data for each cluster

cluster_values <- readRDS('model/aggregate_data/cluster_yearly_aggregates.rds')
cluster_splits <- readRDS('model/cluster_splits.rds')

#map cluster IDs to training years
training_by_cluster <- setNames(cluster_splits$train_years, cluster_splits$Cluster_id)

#mark rows that belong to training set
is_training <- rep(FALSE, nrow(cluster_values))
for (i in 1:nrow(cluster_values)) {
  cluster_id <- cluster_values$Cluster_id[i]
  year <- cluster_values$Year[i]
  
  #check for duplicate cluster-year combinations
  train_years <- training_by_cluster[[as.character(cluster_id)]]
  if (!is.null(train_years) && year %in% train_years) {
    is_training[i] <- TRUE
  }
}

#create training data subset
train_data <- cluster_values[is_training, ]

cat('Total rows in dataset: ', nrow(cluster_values), '\n')
cat('Rows in training data: ', nrow(train_data), '\n')


#columns from train_data to standardize
feature_columns <- c(
  'mean_temp_cluster',
  'mean_vpd_cluster',
  'mean_yield_cluster',
  'total_precip_mean_cluster',
  'total_gdd_mean_cluster'
)


#columns to be preserved
id_columns <- c('Cluster_id', 'Year', 'Location_count', "Station_count", "Latitude", "Longitude")

#calculate means and standard deviations of feature_columns
feature_stats <- data.frame(
  feature = feature_columns,
  mean = NA_real_,
  sd = NA_real_,
  min = NA_real_,
  max = NA_real_,
  train_only = TRUE
)

#fill feature_stats df
for (col in feature_columns){
  feature_stats$mean[feature_stats$feature == col] <- mean(train_data[[col]], na.rm = TRUE)
  feature_stats$sd[feature_stats$feature == col] <- sd(train_data[[col]], na.rm = TRUE)
  feature_stats$min[feature_stats$feature == col] <- min(train_data[[col]], na.rm = TRUE)
  feature_stats$max[feature_stats$feature == col] <- max(train_data[[col]], na.rm = TRUE)
  feature_stats$range[feature_stats$feature == col] <- max(train_data[[col]], na.rm = TRUE) - 
    min(train_data[[col]], na.rm = TRUE)
}

cat('\nFeature statistics to be used for standardization:\n')
print(feature_stats)


#create standardized data set using training parameters
standardized_data <- cluster_values

#apply standardization: (x - mean) / sd
for (col in feature_columns){
  col_mean <- feature_stats$mean[feature_stats$feature == col]
  col_sd <- feature_stats$sd[feature_stats$feature == col]
  
  #skip if sd is zero or very small to avoid division by zero
  if (col_sd < 1e-10) {
    warning('standard deviation for ', col, ' is near zero. Skipping standardization')
    next
  }
  
  standardized_data[[paste0('std_', col)]] <- (cluster_values[[col]] - col_mean) / col_sd
}

standardized_cluster_data <- standardized_data %>%
  select(all_of(id_columns),
         starts_with('std_'))

#save files
saveRDS(feature_stats, 'model/standardized_outputs/feature_stats.rds')
write_csv(feature_stats, 'py_model/data/feature_stats.csv')
saveRDS(standardized_data, 'model/standardized_outputs/all_cluster_data.rds')
saveRDS(standardized_cluster_data, 'model/standardized_outputs/standardized_data.rds')
write.csv(standardized_cluster_data, 'model/standardized_outputs/standardized_data.csv', row.names = FALSE)

