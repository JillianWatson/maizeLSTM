library(tidyverse)
library(data.table)
library(jsonlite)

#all standardized data for clusters 
standardized_data <- readRDS('model/standardized_outputs/standardized_data.rds')
#determines train/test/validation splits for each cluster
cluster_splits <- readRDS('model/cluster_splits.rds')


#sequence generation parameters
LOOKBACK_STEPS <- 3 # number of previous time steps to use for sliding window prediction
MIN_SEQ_LENGTH <- 3

# Function to generate training sequences for a given set of years and features using the sliding window approach
generate_sequences <- function(data, cluster_id, years, lookback = LOOKBACK_STEPS) {
  cluster_data <- data %>%
    filter(Cluster_id == cluster_id, Year %in% years) %>%
    arrange(Year)
  
  if (nrow(cluster_data) < lookback + 1) {
    return(NULL)
  }
  
  sequences <- list()
  
  #use sliding window approach (window is size 3 years)
  for (i in 1:(nrow(cluster_data) - lookback)) {
    input_years <- cluster_data$Year[i:(i + lookback - 1)]
    target_year <- cluster_data$Year[i + lookback]
    
    #get input features (x) and target variable (y)
    X <- cluster_data %>%
      filter(Year %in% input_years) %>%
      select(starts_with('std_')) %>%
      as.matrix()
    
    y <- cluster_data %>%
      filter(Year == target_year) %>%
      select(std_mean_yield_cluster) %>%
      as.matrix()
    
    sequences[[length(sequences) + 1]] <- list(
      cluster_id = cluster_id,
      input_years = input_years,
      target_year = target_year,
      X=X,
      y=y
    )
  }
  return(sequences)
}

cat('\nGenerating Training Sequences\n')

training_sequences <- list()
valid_training_clusters <- cluster_splits %>%
  filter(n_train >= LOOKBACK_STEPS, !insufficient)

for (i in 1:nrow(valid_training_clusters)) {
  cluster_id <- valid_training_clusters$Cluster_id[i]
  train_years <- valid_training_clusters$train_years[[i]]
  
  sequences <- generate_sequences(standardized_data, cluster_id, train_years)
  
  if (!is.null(sequences)) {
    training_sequences <- c(training_sequences, sequences)
  }
}

cat('Generated ', length(training_sequences), ' training sequences\n')

#view the first 3 training sequences
for (i in 1:3) {
  cat("\n===== Training Sequence", i, "=====\n")
  cat("Cluster ID:", training_sequences[[i]]$cluster_id, "\n")
  cat("Input Years:", paste(training_sequences[[i]]$input_years, collapse=", "), "\n")
  cat("Target Year:", training_sequences[[i]]$target_year, "\n")
  cat("X matrix shape:", nrow(training_sequences[[i]]$X), "rows x", ncol(training_sequences[[i]]$X), "columns\n")
  cat("X matrix content:\n")
  print(training_sequences[[i]]$X)
  cat("Target value (y):", training_sequences[[i]]$y, "\n")
  cat("\n")
}


cat('\nGenerating Validation Sequences\n')

#done manually for now, can adapt into function calls later...

validation_sequences <- list()
#use clusters with sufficient training history and validation data
valid_validation_clusters <- cluster_splits %>%
  filter(n_val > 0, n_train >= LOOKBACK_STEPS)

for (i in 1:nrow(valid_validation_clusters)) {
  cluster_id <- valid_validation_clusters$Cluster_id[i]
  val_year <- valid_validation_clusters$validation_years[[i]]
  train_years <- valid_validation_clusters$train_years[[i]]
  
  #get last LOOKBACKSTEPS (3) years of training data for input sequences
  if (length(train_years) >= LOOKBACK_STEPS) {
    input_years <- tail(train_years, LOOKBACK_STEPS)
    
    X <- standardized_data %>% 
      filter(Cluster_id == cluster_id, Year %in% input_years) %>%
      arrange(Year) %>%
      select(starts_with('std_')) %>%
      as.matrix()
    
    y <- standardized_data %>%
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

cat('Generated', length(validation_sequences), 'validation sequences\n')

#view the first 3 validation sequences
for (i in 1:min(3, length(validation_sequences))) {
  cat("\n===== Validation Sequence", i, "=====\n")
  cat("Cluster ID:", validation_sequences[[i]]$cluster_id, "\n")
  cat("Input Years:", paste(validation_sequences[[i]]$input_years, collapse=", "), "\n")
  cat("Target Year:", validation_sequences[[i]]$target_year, "\n")
  cat("X matrix shape:", nrow(validation_sequences[[i]]$X), "rows x", ncol(validation_sequences[[i]]$X), "columns\n")
  cat("X matrix content:\n")
  print(validation_sequences[[i]]$X)
  cat("Target value (y):", validation_sequences[[i]]$y, "\n")
  cat("\n")
}



