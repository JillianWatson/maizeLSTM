library(tidyverse)
library(data.table)
library(jsonlite)
library(dplyr)

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
  #previous workflow used LOOKBACK_STEPS -1 in the validation section, which 
  #generated 16 sequences. Current workflow generates 13 val-sequences.
  filter(n_val > 0, n_train >= LOOKBACK_STEPS)

for (i in 1:nrow(valid_validation_clusters)) {
  cluster_id <- valid_validation_clusters$Cluster_id[i]
  val_year <- valid_validation_clusters$validation_years[[i]]
  train_years <- valid_validation_clusters$train_years[[i]]
  
  #get last LOOKBACKSTEPS (3) years of training data for input sequences
  #old workflow used 3-1 (using 3 lookback steps keeps consistency with train sequences)
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


cat('\nGenerating Test Sequences\n')

test_sequences <- list()
valid_test_clusters <- cluster_splits %>%
  filter(n_test > 0)

for (i in 1:nrow(valid_test_clusters)) {
  cluster_id <- valid_test_clusters$Cluster_id[i]
  test_year <- valid_test_clusters$test_years[[i]]
  val_years <- valid_test_clusters$validation_years[[i]] %||% integer(0)
  train_years <- valid_test_clusters$train_years[[i]]
  
  #takes all available years between 2 sets
  all_input_years <- c(train_years, val_years)
  available_years <- all_input_years[all_input_years < test_year]
  
  if (length(available_years) >= LOOKBACK_STEPS) {
    #use most recent 3 years (LOOKBACK_STEPS = 3) prior to the test year
    input_years <- tail(sort(available_years), LOOKBACK_STEPS)
    
    if (length(input_years) == LOOKBACK_STEPS) {
      X <- standardized_data %>%
        filter(Cluster_id == cluster_id, Year %in% input_years) %>%
        arrange(Year) %>%
        select(starts_with('std_')) %>%
        as.matrix()
      
      y <- standardized_data %>%
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
}

cat('Generated', length(test_sequences), 'test sequences\n')

#view the first 3 test sequences
for (i in 1:min(3, length(test_sequences))) {
  cat("\n===== test Sequence", i, "=====\n")
  cat("Cluster ID:", test_sequences[[i]]$cluster_id, "\n")
  cat("Input Years:", paste(test_sequences[[i]]$input_years, collapse=", "), "\n")
  cat("Target Year:", test_sequences[[i]]$target_year, "\n")
  cat("X matrix shape:", nrow(test_sequences[[i]]$X), "rows x", ncol(test_sequences[[i]]$X), "columns\n")
  cat("X matrix content:\n")
  print(test_sequences[[i]]$X)
  cat("Target value (y):", test_sequences[[i]]$y, "\n")
  cat("\n")
}


#Function to convert sequences to data frames
seq_to_df <- function(sequences) {
  if (length(sequences) == 0) {
    return(NULL)
  }
  
  all_columns <- c('sequence_id', 'cluster_id', 'target_year', 'lookback_steps')
  result_rows <- list()
  
  feature_names <- c('mean_temp', 'mean_vpd', 'mean_yield', 
                     'total_precip_mean', 'total_gdd_mean')
  
  max_lookback <- max(sapply(sequences, function(seq) length(seq$input_years)))
  
  #add standard input_year columns
  for (j in 1:max_lookback) {
    all_columns <- c(all_columns, paste0('input_year_', j))
  }
  
  #add standard feature columns
  for (step in 1:max_lookback) {
    time_step_name <- paste0('t_minus_', step)
    for(feat in feature_names) {
      all_columns <- c(all_columns, paste0(time_step_name, '_', feat))
    }
  }
  
  #add target column
  all_columns <- c(all_columns, 'target_value')
  
  seq_df <- data.frame(matrix(NA, nrow = 0, ncol = length(all_columns)))
  colnames(seq_df) <- all_columns
  
  #process each sequence
  for(i in 1:length(sequences)) {
    seq <- sequences[[i]]
    
    if (is.null(seq$cluster_id) || is.null(seq$target_year) ||
        length(seq$target_year) == 0 || is.null(seq$input_years) ||
        is.null(seq$X) || is.null(seq$y)) {
      cat('Warning: Seq ', i, ' is missing required fields. Skipping.\n')
      next
    }
    
    row_data <- as.list(rep(NA, length(all_columns)))
    names(row_data) <- all_columns
    
    row_data$sequence_id <- i
    row_data$cluster_id <- seq$cluster_id
    row_data$target_year <- seq$target_year[1]
    row_data$lookback_steps <- length(seq$input_years)
    
    #fill input years
    for (j in 1:length(seq$input_years)) {
      col_name <- paste0('input_year_', j)
      row_data[[col_name]] <- seq$input_years[j]
    }
    
    #create standard feature names (t-3, t-2, t-1) where t is the target year
    if (is.matrix(seq$X)) {
      x_matrix <- seq$X
      
      for (step in 1:nrow(x_matrix)) {
        time_step_name <- paste0('t_minus_', nrow(x_matrix) - step + 1)
        
        for (feat in 1:ncol(x_matrix)) {
          #col name description
          get_feat_name <- feature_names[feat]
          col_name <- paste0(time_step_name, '_', get_feat_name)
          
          row_data[[col_name]] <- x_matrix[step, feat]
        }
      }
    } 
    
    row_data$target_value <- seq$y[1]
    
    seq_df[i, ] <- as.data.frame(row_data, stringsAsFactors = FALSE)
  }
  
  return(seq_df)
}


train_df <- seq_to_df(training_sequences)
validation_df <- seq_to_df(validation_sequences)
test_df <- seq_to_df(test_sequences)

timestamp <- format(Sys.time(), '%Y%m%d')

write_csv(train_df, paste0('py_model/data/train_sequences_', LOOKBACK_STEPS, 'yr_', timestamp, '.csv'))
write_csv(validation_df, paste0('py_model/data/validation_sequences_', LOOKBACK_STEPS, 'yr_', timestamp, '.csv'))
write_csv(test_df, paste0('py_model/data/test_sequences_', LOOKBACK_STEPS, 'yr_', timestamp, '.csv'))

#save seq parameters:
params_df <- data.frame(
  lookback_steps = LOOKBACK_STEPS,
  min_seq_length = MIN_SEQ_LENGTH,
  train_target_years = paste(unique(sapply(training_sequences, function(x) x$target_year)), collapse = ','),
  validation_target_year = paste(unique(sapply(validation_sequences, function(x) x$target_year)), collapse = ','),
  test_target_year = paste(unique(sapply(test_sequences, function(x) x$target_year)), collapse = ','),
  time_created = Sys.time()
)

write_csv(params_df, 'py_model/data/sequence_parameters.csv')


