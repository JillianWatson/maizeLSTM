library(tidyverse)

cluster_yearly_aggregates <- readRDS("model/cluster_yearly_aggregates.rds")

#data summary before standardization
cat("Original data summary:\n")
print(summary(cluster_yearly_aggregates))

#columns to standardize
feature_cols <- c(
  "mean_temp_cluster", 
  "mean_vpd_cluster", 
  "mean_yield_cluster", 
  "total_precip_mean_cluster", 
  "total_gdd_mean_cluster"
)

#columns to preserve as-is (identifiers, counts, and geographical information)
id_cols <- c("Cluster_id", "Year", "Location_count", "Station_count", "Latitude", "Longitude")

missing_cols <- setdiff(c(feature_cols), names(cluster_yearly_aggregates))
if (length(missing_cols) > 0) {
  stop("The following columns are missing from the data: ", paste(missing_cols, collapse = ", "))
}

#calculate global means and standard deviations for conversions
feature_stats <- data.frame(
  feature = c(feature_cols),
  mean = NA_real_,
  sd = NA_real_,
  min = NA_real_,
  max = NA_real_,
  range = NA_real_
)

for (col in c(feature_cols)) {
  feature_stats$mean[feature_stats$feature == col] <- mean(cluster_yearly_aggregates[[col]], na.rm = TRUE)
  feature_stats$sd[feature_stats$feature == col] <- sd(cluster_yearly_aggregates[[col]], na.rm = TRUE)
  feature_stats$min[feature_stats$feature == col] <- min(cluster_yearly_aggregates[[col]], na.rm = TRUE)
  feature_stats$max[feature_stats$feature == col] <- max(cluster_yearly_aggregates[[col]], na.rm = TRUE)
  feature_stats$range[feature_stats$feature == col] <- max(cluster_yearly_aggregates[[col]], na.rm = TRUE) - 
    min(cluster_yearly_aggregates[[col]], na.rm = TRUE)
}

cat("\nFeature statistics for standardization:\n")
print(feature_stats)

#create standardized dataset
standardized_data <- cluster_yearly_aggregates

#apply standardization: (x - mean) / sd
for (col in c(feature_cols)) {
  col_mean <- feature_stats$mean[feature_stats$feature == col]
  col_sd <- feature_stats$sd[feature_stats$feature == col]
  col_range <- feature_stats$range[feature_stats$feature == col]
  
  #special handling for features with small ranges (VPD)
  if (col == "mean_vpd_cluster") {
    cat(sprintf("VPD statistics - mean: %.4f, sd: %.4f, range: %.4f\n", col_mean, col_sd, col_range))
  }
  
  #skip if standard deviation is zero or very small
  if (col_sd < 1e-10) {
    warning("Standard deviation for ", col, " is near zero (", col_sd, "). Skipping standardization.")
    next
  }
  
  #still use z-score standardization for all ranges. log any potential concerns
  if (col_range < 1.0 && col != "mean_vpd_cluster") {
    cat(sprintf("Note: %s has a small range (%.4f). Standardization will proceed, but monitor for any issues.\n", 
                col, col_range))
  }
  
  standardized_data[[paste0("std_", col)]] <- (cluster_yearly_aggregates[[col]] - col_mean) / col_sd
}

#add original and standardized columns
cat("\nColumns in standardized dataset:\n")
print(names(standardized_data))

#check for NaNs or Infs in standardized columns
std_cols <- paste0("std_", c(feature_cols))
has_problems <- FALSE

for (col in std_cols) {
  n_na <- sum(is.na(standardized_data[[col]]))
  n_inf <- sum(is.infinite(standardized_data[[col]]))
  
  if (n_na > 0 || n_inf > 0) {
    cat("WARNING: Column", col, "has", n_na, "NAs and", n_inf, "infinite values\n")
    has_problems <- TRUE
  }
}

if (!has_problems) {
  cat("No NAs or infinite values found in standardized columns\n")
}

#create a data set with only standardized features for modeling
model_ready_data <- standardized_data %>%
  select(
    #keep identifier columns
    all_of(id_cols),
    all_of(std_cols)
  )

#check for missing values in final data set
na_counts <- colSums(is.na(model_ready_data))
if (sum(na_counts) > 0) {
  cat("WARNING: Missing values detected in model_ready_data:\n")
  print(na_counts[na_counts > 0])
}

saveRDS(feature_stats, "model/feature_stats.rds")
write_csv(feature_stats, "py_model/standardized_data_stats.csv")
saveRDS(standardized_data, "model/standardized_yearly_aggregates.rds")
saveRDS(model_ready_data, "model/model_ready_data.rds")

write.csv(model_ready_data, "model/standardized_data.csv", row.names = FALSE)

cat("\nStandardization complete!\n")
cat("- Full data with standardized features saved to: model/standardized_yearly_aggregates.rds\n")
cat("- Model-ready data with only standardized features saved to: model/model_ready_data.rds\n")
cat("- Feature statistics saved to: model/feature_stats.rds\n")

#summary stats before and after standardization for verification
cat("\nBefore standardization (original features):\n")
feature_summary <- cluster_yearly_aggregates %>%
  select(all_of(c(feature_cols))) %>%
  summary()
print(feature_summary)

cat("\nDetailed stats for VPD:\n")
vpd_stats <- cluster_yearly_aggregates %>%
  summarise(
    vpd_mean = mean(mean_vpd_cluster, na.rm = TRUE),
    vpd_sd = sd(mean_vpd_cluster, na.rm = TRUE),
    vpd_min = min(mean_vpd_cluster, na.rm = TRUE),
    vpd_max = max(mean_vpd_cluster, na.rm = TRUE),
    vpd_median = median(mean_vpd_cluster, na.rm = TRUE),
    vpd_q25 = quantile(mean_vpd_cluster, 0.25, na.rm = TRUE),
    vpd_q75 = quantile(mean_vpd_cluster, 0.75, na.rm = TRUE)
  )
print(vpd_stats)

cat("\nAfter standardization (standardized features):\n")
std_feature_summary <- model_ready_data %>%
  select(all_of(std_cols)) %>%
  summary()
print(std_feature_summary)

#correlations btwn non-standardized and standardized features
cat("\nCorrelations between original and standardized features:\n")
for (i in seq_along(c(feature_cols))) {
  orig_col <- c(feature_cols)[i]
  std_col <- paste0("std_", orig_col)
  
  corr <- cor(standardized_data[[orig_col]], standardized_data[[std_col]], use = "complete.obs")
  cat(sprintf("%-25s and %-25s: %f\n", orig_col, std_col, corr))
}

cat("\nData ready for model development!\n")