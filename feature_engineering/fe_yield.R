library(dplyr)

processing_yield_train <- readRDS("data_wrangling/processing_yield_train.rds")

Annual_Yields <- processing_yield_train %>%
  group_by(SpatialLoc, Year) %>%
  summarise(
    mean_yield = mean(Impute_Yield, na.rm = TRUE),
    std_yield = sd(Impute_Yield, na.rm = TRUE),
    count = n(),
    min_yield = min(Impute_Yield, na.rm = TRUE),
    max_yield = max(Impute_Yield, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  rename(Location = SpatialLoc)


saveRDS(Annual_Yields, "feature_engineering/Annual_Yields.rds")
