library(tidyverse)
library(data.table)
library(jsonlite)

model_ready_data <- readRDS("model/model_ready_data.rds")

#define sequence generation parameters
LOOKBACK_STEPS <- 3  #number of previous time steps to use for prediction
MIN_SEQ_LENGTH <- 3  #minimum sequence length required to use a cluster

cat("Implementing adaptive train/test/validation split based on sequence length...\n")

# Function to determine split for a cluster based on its time sequence length
#  years: the years a cluster has recorded data for
#  min_seq_length: minimum number of years required to perform splitting
determine_split <- function(years, min_seq_length = MIN_SEQ_LENGTH) {
  sorted_years <- sort(years)
  n_years <- length(sorted_years)
  
  if (n_years >= 6) {
    #if clusters contain 6+ years of data: take last year for test,
    #second-last for validation
    return(list(
      test = sorted_years[n_years],
      validation = sorted_years[n_years - 1],
      train = sorted_years[1:(n_years - 2)]
    ))
  } else if (n_years >= 4) {
    #if clusters contain 4-5 years of data: take last year for test,
    #second-last for validation
    return(list(
      test = sorted_years[n_years],
      validation = sorted_years[n_years - 1],
      train = sorted_years[1:(n_years - 2)]
    ))
  } else if (n_years >= min_seq_length) {
    #if clusters contain exactly 3 years of data: last year for test,
    #rest for training, no validation sequence
    return(list(
      test = sorted_years[n_years],
      validation = NULL,
      train = sorted_years[1:(n_years - 1)]
    ))
  } else {
    #if clusters have insufficient data: can't be used for prediction
    return(list(
      test = NULL,
      validation = NULL,
      train = sorted_years,
      insufficient = TRUE
    ))
  }
}

#filter for unique years within each cluster
cluster_years <- model_ready_data %>%
  select(Cluster_id, Year) %>%
  group_by(Cluster_id) %>%
  summarise(years = list(sort(unique(Year))), .groups = "drop")

#calculate the split for each cluster
cluster_splits <- cluster_years %>%
  mutate(
    split = map(years, determine_split),
    train_years = map(split, "train"),
    validation_years = map(split, "validation"),
    test_years = map(split, "test"),
    insufficient = map_lgl(split, ~ !is.null(.x$insufficient) && .x$insufficient == TRUE),
    n_years = map_int(years, length),
    n_train = map_int(train_years, length),
    n_val = map_int(validation_years, ~ length(.x %||% integer(0))),
    n_test = map_int(test_years, ~ length(.x %||% integer(0)))
  )

#summary stats
cat("\nSplit summary:\n")
cat("- Total clusters:", nrow(cluster_splits), "\n")
cat("- Clusters with insufficient data:", sum(cluster_splits$insufficient), "\n")
cat("- Clusters with test data:", sum(cluster_splits$n_test > 0), "\n")
cat("- Clusters with validation data:", sum(cluster_splits$n_val > 0), "\n")

#year distribution across splits
year_distribution <- data.frame(Year = integer(), Train = integer(), Validation = integer(), Test = integer())

for (year in sort(unique(unlist(cluster_years$years)))) {
  train_count <- sum(sapply(cluster_splits$train_years, function(years) year %in% years))
  val_count <- sum(sapply(cluster_splits$validation_years, function(years) year %in% (years %||% integer(0))))
  test_count <- sum(sapply(cluster_splits$test_years, function(years) year %in% (years %||% integer(0))))
  
  year_distribution <- rbind(year_distribution, data.frame(
    Year = year, 
    Train = train_count,
    Validation = val_count,
    Test = test_count
  ))
}

cat("\nYear distribution across splits:\n")
print(year_distribution)

# Function to generate sequences for a given set of years and features
#   data: standardized data for each cluster, from model_ready_data
#   cluster_id: ID number for a specific cluster (1-19)
#   years: the years a cluster has recorded data for
#   lookback: number of previous timesteps to use for making predictions
generate_sequences <- function(data, cluster_id, years, lookback = LOOKBACK_STEPS) {

  cluster_data <- data %>%
    filter(Cluster_id == cluster_id, Year %in% years) %>%
    arrange(Year)
  
  if (nrow(cluster_data) < lookback + 1) {
    return(NULL)
  }
  
  sequences <- list()
  
  #generate sequences using sliding window approach
  for (i in 1:(nrow(cluster_data) - lookback)) {
    input_years <- cluster_data$Year[i:(i + lookback - 1)]
    target_year <- cluster_data$Year[i + lookback]
    
    #extract input features (X) and target variable (y)
    X <- cluster_data %>%
      filter(Year %in% input_years) %>%
      select(starts_with("std_")) %>%
      as.matrix()
    
    y <- cluster_data %>%
      filter(Year == target_year) %>%
      select(std_mean_yield_cluster) %>%
      as.matrix()
    
    sequences[[length(sequences) + 1]] <- list(
      cluster_id = cluster_id,
      input_years = input_years,
      target_year = target_year,
      X = X,
      y = y
    )
  }
  
  return(sequences)
}


#generate training sequences
cat("\nGenerating training sequences...\n")
training_sequences <- list()

#only include clusters with sufficient training data
valid_train_clusters <- cluster_splits %>%
  filter(n_train >= LOOKBACK_STEPS, !insufficient)

for (i in 1:nrow(valid_train_clusters)) {
  cluster_id <- valid_train_clusters$Cluster_id[i]
  train_years <- valid_train_clusters$train_years[[i]]
  
  sequences <- generate_sequences(model_ready_data, cluster_id, train_years)
  
  if (!is.null(sequences)) {
    training_sequences <- c(training_sequences, sequences)
  }
}
cat("Generated", length(training_sequences), "training sequences\n")


#generate validation sequences
cat("\nGenerating validation sequences...\n")
validation_sequences <- list()

#only include clusters with validation data and sufficient training history
valid_val_clusters <- cluster_splits %>%
  filter(n_val > 0, n_train >= LOOKBACK_STEPS - 1)

for (i in 1:nrow(valid_val_clusters)) {
  cluster_id <- valid_val_clusters$Cluster_id[i]
  val_year <- valid_val_clusters$validation_years[[i]]
  train_years <- valid_val_clusters$train_years[[i]]
  
  #get last (LOOKBACK_STEPS - 1) years from training for input sequence
  if (length(train_years) >= LOOKBACK_STEPS - 1) {
    input_years <- tail(train_years, LOOKBACK_STEPS - 1)
    all_years <- c(input_years, val_year)
    
    X <- model_ready_data %>%
      filter(Cluster_id == cluster_id, Year %in% input_years) %>%
      arrange(Year) %>%
      select(starts_with("std_")) %>%
      as.matrix()
    
    y <- model_ready_data %>%
      filter(Cluster_id == cluster_id, Year == val_year) %>%
      select(std_mean_yield_cluster) %>%
      as.matrix()
    
    validation_sequences[[length(validation_sequences) + 1]] <- list(
      cluster_id = cluster_id,
      input_years = input_years,
      target_year = val_year,
      X = X,
      y = y
    )
  }
}
cat("Generated", length(validation_sequences), "validation sequences\n")


#generate test sequences
cat("\nGenerating test sequences...\n")
test_sequences <- list()

#only include clusters with test data
valid_test_clusters <- cluster_splits %>%
  filter(n_test > 0)

for (i in 1:nrow(valid_test_clusters)) {
  cluster_id <- valid_test_clusters$Cluster_id[i]
  test_year <- valid_test_clusters$test_years[[i]]
  val_years <- valid_test_clusters$validation_years[[i]] %||% integer(0)
  train_years <- valid_test_clusters$train_years[[i]]
  
  #combine the last (LOOKBACK_STEPS - 1) years from train and validation for input
  input_years <- c(
    tail(train_years, max(0, LOOKBACK_STEPS - length(val_years) - 1)),
    val_years
  )
  
  if (length(input_years) >= LOOKBACK_STEPS - 1) {
    input_years <- tail(input_years, LOOKBACK_STEPS - 1)
    all_years <- c(input_years, test_year)

    X <- model_ready_data %>%
      filter(Cluster_id == cluster_id, Year %in% input_years) %>%
      arrange(Year) %>%
      select(starts_with("std_")) %>%
      as.matrix()
    
    y <- model_ready_data %>%
      filter(Cluster_id == cluster_id, Year == test_year) %>%
      select(std_mean_yield_cluster) %>%
      as.matrix()
    
    test_sequences[[length(test_sequences) + 1]] <- list(
      cluster_id = cluster_id,
      input_years = input_years,
      target_year = test_year,
      X = X,
      y = y
    )
  }
}
cat("Generated", length(test_sequences), "test sequences\n")


#sequence and split information
sequence_data <- list(
  training = training_sequences,
  validation = validation_sequences,
  test = test_sequences,
  cluster_splits = cluster_splits,
  parameters = list(
    lookback_steps = LOOKBACK_STEPS,
    min_seq_length = MIN_SEQ_LENGTH
  )
)


#validate train/test/validation sequences for proper cluster-level separation
validation_errors <- 0

#ensure no overlap within the same cluster
for (cluster_id in unique(sapply(training_sequences, function(x) x$cluster_id))) {
  train_years_for_cluster <- unique(unlist(
    lapply(Filter(function(x) x$cluster_id == cluster_id, training_sequences), 
           function(x) c(x$input_years, x$target_year))
  ))
  
  test_years_for_cluster <- unique(unlist(
    lapply(Filter(function(x) x$cluster_id == cluster_id, test_sequences), 
           function(x) x$target_year)
  ))
  
  overlap <- intersect(train_years_for_cluster, test_years_for_cluster)
  
  if (length(overlap) > 0) {
    cat("ERROR: For cluster", cluster_id, "- Test years found in training sequences:", 
        paste(overlap, collapse=", "), "\n")
    validation_errors <- validation_errors + 1
  }
}

cat("Cluster-level validation completed with", validation_errors, "errors\n")

cat("Begin exporting sequence splits for python model")


# Function to convert sequence (as list) to data frame for exporting to csv
#  sequence: list which contains test, train, or validation data
sequences_to_df <- function(sequence){
  
  if (length(sequence) == 0) {
    return(NULL)
  }
  
  seq_df <- data.frame()
  
  for (i in 1:length(sequence)) {
    seq <- sequence[[i]]
    
    #skip if any required field is missing or empty
    if (is.null(seq$cluster_id) || is.null(seq$target_year) || 
        length(seq$target_year) == 0 || is.null(seq$input_years) || 
        is.null(seq$X) || is.null(seq$y)) {
      cat("WARNING: Sequence", i, "is missing required fields. Skipping.\n")
      next
    }
    
    input_years <- seq$input_years
    n_input_years <- length(input_years)
    
    cat("Processing sequence", i, "for cluster", seq$cluster_id, 
        "with target year", seq$target_year, "- X dim:", dim(seq$X), "\n")
    
    row_data <- data.frame(
      sequence_id = i,
      cluster_id = seq$cluster_id,
      target_year = seq$target_year[1]
    )
    
    for (j in 1:n_input_years){
      col_name <- paste0("input_year_", j)
      row_data[[col_name]] <- input_years[j]
    }
    
    #flatten matrices by row, do not transpose to preserve temporal structure
    if (is.matrix(seq$X)) {
      features_flat <- as.vector(seq$X)
    } else if (is.vector(seq$X)) {
      features_flat <- seq$X  #already a vector
    } else {
      cat("WARNING: Unexpected X type for sequence", i, ". Skipping.\n")
      next
    }
    
    for (j in 1:length(features_flat)) {
      col_name <- paste0("feature_", j)
      row_data[[col_name]] <- features_flat[j]
    }
    
    
    #add target value
    if (length(seq$y) > 0) {
      row_data$target_value <- seq$y[1]
    } else {
      cat("WARNING: Sequence", i, "has empty target value. Skipping.\n")
      next
    }
    
    seq_df <- rbind(seq_df, row_data)
    
  }
  
  return(seq_df)
}


#cluster split information
cluster_splits_df <- cluster_splits %>% 
  select(Cluster_id, n_years, n_train, n_val, n_test, insufficient)

#insert years as their own cols (convert lists to strings)
cluster_splits_df$years_json <- sapply(cluster_splits$years, function(x) toJSON(x))
cluster_splits_df$train_years_json <- sapply(cluster_splits$train_years, function(x) toJSON(x))
cluster_splits_df$validation_years_json <- sapply(cluster_splits$validation_years, function(x) toJSON(x %||% integer(0)))
cluster_splits_df$test_years_json <- sapply(cluster_splits$test_years, function(x) toJSON(x %||% integer(0)))

#parameter data
params_df <- data.frame(
  lookback_steps = LOOKBACK_STEPS,
  min_seq_length = MIN_SEQ_LENGTH
)


#export each component to CSV
train_df <- sequences_to_df(training_sequences)
validation_df <- sequences_to_df(validation_sequences)
test_df <- sequences_to_df(test_sequences)


#save to csv
if (!is.null(train_df) && nrow(train_df) > 0) {
  write_csv(train_df, "py_model/train_sequences.csv")
  cat("- Saved", nrow(train_df), "training sequences\n")
} else {
  cat("WARNING: No valid training sequences to export\n")
}

if (!is.null(validation_df) && nrow(validation_df) > 0) {
  write_csv(validation_df, "py_model/validation_sequences.csv")
  cat("- Saved", nrow(validation_df), "validation sequences\n")
} else {
  cat("WARNING: No valid validation sequences to export\n")
}

if (!is.null(test_df) && nrow(test_df) > 0) {
  write_csv(test_df, "py_model/test_sequences.csv")
  cat("- Saved", nrow(test_df), "test sequences\n")
} else {
  cat("WARNING: No valid test sequences to export\n")
}
write_csv(cluster_splits_df, "py_model/cluster_splits.csv")
write_csv(params_df, "py_model/sequence_parameters.csv")


