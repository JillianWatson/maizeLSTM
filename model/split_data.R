library(tidyverse)
library(data.table)
library(jsonlite)

cluster_values <- readRDS('model/aggregate_data/cluster_yearly_aggregates.rds')

LOOKBACK_STEPS <- 3
MIN_SEQ_LENGTH <- 3

cat('Beginning adaptive train/test/validation splits based on available data per cluster')

# Helper function to determine split for a cluster based on available data
calculate_split <- function(years, min_seq_length = MIN_SEQ_LENGTH) {
  n_years <- length(years)
  
  #use last year for test & second-last year for validation
  if (n_years >= 6){
    return(list(
      test = years[n_years],
      validation = years[n_years - 1],
      train = years[1:(n_years - 2)]
    ))
    #use last year for test & second-last year for validation
  } else if (n_years >= 4) {
    return(list(
      test = years[n_years],
      validation = years[n_years - 1],
      train = years[1:(n_years - 2)]
    ))
    #use last year for test, insufficient data for validation split, use the rest for training
  } else if (n_years >= min_seq_length) {
    return(list(
      test = years[n_years],
      validation = NULL,
      train = years[1:(n_years - 1)]
    ))
    #cluster has insufficient data & should not be used in model 
  } else {
    return(list(
      test = NULL,
      validation = NULL,
      train = years,
      insufficient = TRUE
    ))
  }
}

#sort by cluster and retrieve years of data per
cluster_years <- cluster_values %>% 
  select(Cluster_id, Year) %>%
  group_by(Cluster_id) %>%
  summarise(years = list(sort(Year)), .groups = 'drop')


#calculate the test/validation/train splits for each cluster
cluster_splits <- cluster_years %>%
  mutate(
    split = map(years, calculate_split),
    train_years = map(split, 'train'),
    validation_years = map(split, 'validation'),
    test_years = map(split, 'test'),
    insufficient = map_lgl(split, ~ !is.null(.x$insufficient) && .x$insufficient == TRUE),
    n_years = map_int(years, length),
    n_train = map_int(train_years, length),
    n_val = map_int(validation_years, ~ length(.x %||% integer(0))),
    n_test = map_int(test_years, ~length(.x %||% integer(0)))
  )

#print summary stats

cat('\nSummary of Splits\n')
cat('* Total amount of clusters: ', nrow(cluster_splits), '\n')
cat('* Clusters with insufficient data: ', sum(cluster_splits$insufficient), '\n')
cat('* Clusters with test data: ', sum(cluster_splits$n_test > 0), '\n')
cat('* Clusters with train data: ', sum(cluster_splits$n_train > 0), '\n')


#determine distribution of years across splits
year_distribution <- data.frame(Year = integer(), Test = integer(), Validation = integer(), Train = integer())

for (year in sort(unique(unlist(cluster_years$years)))) {
  test_count <- sum(sapply(cluster_splits$test_years, function(years) year %in% years))
  val_count <- sum(sapply(cluster_splits$validation_years, function(years) year %in% (years %||% integer(0))))
  train_count <- sum(sapply(cluster_splits$train_years, function(years) year %in% (years %||% integer(0))))
  
  year_distribution <- rbind(year_distribution, data.frame(
    Year = year,
    Test = test_count,
    Validation = val_count,
    Train = train_count
  ))
}

cat('\nDistribution of years across all splits\n')
print(year_distribution)

#df to hold split summary data
cluster_splits_df <- cluster_splits %>%
  select(Cluster_id, n_years, n_train, n_val, n_test, insufficient)

#convert list columns to JSON strings for easy python maneuvers 
cluster_splits_df$years_json <- sapply(cluster_splits$years, function(x) toJSON(x))
cluster_splits_df$test_years_json <- sapply(cluster_splits$test_years, function(x) toJSON(x %||% integer(0)))
cluster_splits_df$validation_years_json <- sapply(cluster_splits$validation_years, function(x) toJSON(x %||% integer(0)))
cluster_splits_df$train_years_json <- sapply(cluster_splits$train_years, function(x) toJSON(x))

#save to csv
write_csv(cluster_splits_df, 'py_model/data/cluster_splits.csv')
saveRDS(cluster_splits, 'model/cluster_splits.rds')

cat('\nCluster splits creation complete\n')
cat('* CSV saved to py_model/data/cluster_splits.csv\n')
cat('* RDS saved to model/cluster_splits.rds')

