library(tidyverse)
library(lubridate)

#load data sets
joined_dataset <- readRDS("feature_engineering/joined_dataset.rds")
location_cluster_mapping <- readRDS("build_graph/location_cluster_mapping.rds")
filtered_centroids <- readRDS("build_graph/centroids.rds")
edge_index <- readRDS("build_graph/edge_index_matrix.rds")

#remove Station_count feature from location_cluster_mapping if it exists
if ("Station_count" %in% names(location_cluster_mapping)) {
  location_cluster_mapping <- location_cluster_mapping %>%
    select(-Station_count)
  cat("Removed Station_count from location_cluster_mapping\n")
}

#check that data sets loaded correctly
cat("Loaded joined_dataset with", nrow(joined_dataset), "rows and", ncol(joined_dataset), "columns\n")
cat("Loaded location_cluster_mapping with", nrow(location_cluster_mapping), "mappings\n")
cat("Loaded filtered_centroids with", nrow(filtered_centroids), "clusters\n")

#merge joined_dataset with location_cluster_mapping to associate each record with its cluster
cat("Merging joined_dataset with cluster information...\n")

cluster_dataset <- joined_dataset %>%
  left_join(location_cluster_mapping, by = "Location") %>%
  # Check for any unmatched locations
  mutate(has_cluster = !is.na(Cluster_id))

#for diagnostics
cluster_dataset %>% 
  write.table(file = "model/aggregate_data/pre_aggregate_cluster_output.txt", 
              sep = "\t",
              row.names = FALSE, 
              quote = FALSE
              )

#report any locations without cluster assignments
unmatched_locations <- cluster_dataset %>% 
  filter(!has_cluster) %>% 
  distinct(Location) %>% 
  pull(Location)

if (length(unmatched_locations) > 0) {
  cat("WARNING:", length(unmatched_locations), "locations could not be matched to clusters:\n")
  print(head(unmatched_locations, 10))
  
  #remove records without cluster assignments
  cluster_dataset <- cluster_dataset %>% filter(has_cluster)
  cat("Removed", sum(!cluster_dataset$has_cluster), "records without cluster assignments\n")
}


# Function that defines aggregation functions that will be performed on each cluster node
#  in the graph.
#  data: dataset which contains yield and weather parameters as well as cluster ID
#        for correct mapping of location data to its assigned cluster 
calculate_cluster_aggregates <- function(data) {
  
  #list of variables to aggregate
  agg_vars <- c(
    "mean_temp", "total_precip", "mean_vpd", "gdd_sum", "mean_yield"
  )
  
  #check which variables are in the data set
  available_vars <- intersect(agg_vars, names(data))
  
  if (length(available_vars) == 0) {
    stop("None of the expected variables found in the dataset")
  }
  
  cat("Aggregating the following variables across clusters:", paste(available_vars, collapse = ", "), "\n")
  
  #calculate aggregates by cluster and year
  aggregates <- data %>%
    filter(has_cluster) %>%
    group_by(Cluster_id, Year) %>%
    summarise(
      
      #use avg across locations
      mean_temp_cluster = mean(mean_temp, na.rm = TRUE),
      mean_vpd_cluster = mean(mean_vpd, na.rm = TRUE),
      mean_yield_cluster = mean(mean_yield, na.rm = TRUE),
      total_precip_mean_cluster = mean(total_precip, na.rm = TRUE),
      total_gdd_mean_cluster = mean(gdd_sum, na.rm = TRUE),
      
      #count metrics 
      
      #location_count varies by year within each cluster and indicates how many 
      #actual data points were available in that cluster for that specific year
      
      Location_count = n_distinct(Location),
      .groups = "drop"
    )
  
  return(aggregates)
}

#calculate yearly aggregates
yearly_aggregates <- calculate_cluster_aggregates(cluster_dataset)

#add cluster geographical information
yearly_aggregates_with_geo <- yearly_aggregates %>%
  left_join(
    filtered_centroids %>%
      select(Cluster_id, Latitude, Longitude, Station_count),
    by = "Cluster_id"
  )

target_aggregates <- yearly_aggregates_with_geo

cat("Generated", nrow(target_aggregates), "cluster-year aggregates for 2014-2021\n")
cat("Data spans", n_distinct(target_aggregates$Cluster_id), "clusters\n")

#check for any missing values in the aggregates
na_counts <- colSums(is.na(target_aggregates))
if (sum(na_counts) > 0) {
  cat("WARNING: Missing values detected in aggregates:\n")
  print(na_counts[na_counts > 0])
}

#create final dataset with graph structure information

#convert edge_index to a more usable format
edge_df <- data.frame(
  source = edge_index[1, ],
  target = edge_index[2, ]
)

saveRDS(target_aggregates, "model/aggregate_data/cluster_yearly_aggregates.rds")
saveRDS(edge_list, "model/aggregate_data/cluster_edge_list.rds")

cluster_model_data <- list(
  yearly_aggregates = target_aggregates,
  edge_df = edge_df,
  clusters = filtered_centroids,
  location_mapping = location_cluster_mapping
)

saveRDS(cluster_model_data, "model/aggregate_data/cluster_model_data.rds")

summary_stats <- target_aggregates %>%
  group_by(Year) %>%
  summarise(
    Clusters = n_distinct(Cluster_id),
    Records = n(),
    Avg_locations_per_cluster = mean(Location_count, na.rm = TRUE)
  )

write.csv(target_aggregates, "model/aggregate_data/cluster_yearly_aggregates.csv", row.names = FALSE)
write_csv(edge_df, "py_model/edge_df.csv")

cat("\nSummary statistics by year:\n")
print(summary_stats)

cat("\nCluster-level aggregation complete!\n")
cat("Results saved to 'model/aggregate_data/cluster_yearly_aggregates.rds'\n")
cat("Comprehensive model data saved to 'model/aggregate_data/cluster_model_data.rds'\n")