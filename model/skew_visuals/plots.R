library(tidyverse)
library(moments)
library(gridExtra) 
library(ggplot2)

standardized_data <- readRDS("model/standardized_yearly_aggregates.rds")

#list of standardized features to check
std_features <- c(
  "std_mean_temp_cluster",
  "std_mean_vpd_cluster",
  "std_mean_yield_cluster",
  "std_total_precip_mean_cluster",
  "std_total_gdd_mean_cluster"
)

skewness_values <- data.frame(
  feature = std_features,
  skewness = NA_real_
)

#calculate skew on each feature
for (feature in std_features) {
  skew_val <- skewness(standardized_data[[feature]], na.rm = TRUE)
  skewness_values$skewness[skewness_values$feature == feature] <- skew_val
  
  cat(sprintf("Skewness of %s: %.4f\n", feature, skew_val))
}

#bar plot for skew of each standardized feature
skewness_plot <- ggplot(skewness_values, aes(x = reorder(feature, skewness), y = skewness)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Skewness of Standardized Features",
       x = "Feature",
       y = "Skewness") +
  theme_minimal()

#density plots for each standardized feature
plots <- list()

for (feature in std_features) {
  p <- ggplot(standardized_data, aes_string(x = feature)) +
    geom_density(fill = "lightblue", alpha = 0.7) +
    labs(title = paste("Distribution of", feature),
         x = feature,
         y = "Density") +
    theme_minimal()
  
  plots[[feature]] <- p
}

ggsave("model/skew_visuals/feature_skewness.png", skewness_plot, width = 10, height = 6)
cat("Skewness bar plot saved to model/skew_visuals/feature_skewness.png\n")

for (feature in std_features) {
  ggsave(paste0("model/skew_visuals/", feature, "_distribution.png"), plots[[feature]], width = 8, height = 6)
  cat(paste0("Distribution plot for ", feature, " saved\n"))
}

#display original feature skewness for comparison
orig_features <- c(
  "mean_temp_cluster",
  "mean_vpd_cluster",
  "mean_yield_cluster",
  "total_precip_mean_cluster",
  "total_gdd_mean_cluster"
)

#calculate skewness for each original feature
orig_skewness_values <- data.frame(
  feature = orig_features,
  skewness = NA_real_
)

for (feature in orig_features) {
  skew_val <- skewness(standardized_data[[feature]], na.rm = TRUE)
  orig_skewness_values$skewness[orig_skewness_values$feature == feature] <- skew_val
  
  cat(sprintf("Skewness of original %s: %.4f\n", feature, skew_val))
}

#create comparison table
comparison <- data.frame(
  original_feature = orig_features,
  original_skewness = orig_skewness_values$skewness,
  standardized_feature = std_features,
  standardized_skewness = skewness_values$skewness
)

print(comparison)

#save comparison to CSV file
write.csv(comparison, "model/skew_visuals/skewness_comparison.csv", row.names = FALSE)
cat("Skewness comparison saved to model/skew_visuals/skewness_comparison.csv\n")

#grouped bar chart for comparison
comparison_long <- comparison %>%
  select(original_feature, original_skewness, standardized_skewness) %>%
  rename(feature = original_feature) %>%
  pivot_longer(cols = c(original_skewness, standardized_skewness),
               names_to = "type",
               values_to = "skewness")

#clean up feature names for display
comparison_long$feature <- gsub("_cluster", "", comparison_long$feature)
comparison_long$feature <- gsub("mean_", "", comparison_long$feature)
comparison_long$feature <- gsub("total_", "", comparison_long$feature)

comparison_plot <- ggplot(comparison_long, 
                          aes(x = reorder(feature, skewness), 
                              y = skewness, 
                              fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  coord_flip() +
  labs(title = "Comparison of Skewness: Original vs. Standardized Features",
       x = "Feature",
       y = "Skewness",
       fill = "Type") +
  scale_fill_manual(values = c("original_skewness" = "darkblue", 
                               "standardized_skewness" = "lightgreen"),
                    labels = c("original_skewness" = "Original", 
                               "standardized_skewness" = "Standardized")) +
  theme_minimal()

ggsave("model/skew_visuals/skewness_comparison.png", comparison_plot, width = 10, height = 6)
cat("Skewness comparison plot saved to model/skew_visuals/skewness_comparison.png\n")

cat("\nSkewness analysis complete\n")