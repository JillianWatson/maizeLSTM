
# TODO: analyse relations btwn dependent and independent variables

Yield <- readRDS("feature_engineering/Yield-tojoin.rds")
Weather <- readRDS("feature_engineering/Wx-tojoin.rds")

join_data <- function(Yield, Weather) {
  joined_data <- Yield %>% left_join(Weather, by = c("Location", "Year"))
  
  missing_weather <- joined_data %>%
    filter(is.na(mean_temp) | is.na(total_precip) | is.na(mean_vpd) | is.na(gdd_sum))
  
  if(nrow(missing_weather) > 0) {
    warning("Found yield observations without matching weather data: ")
    print(missing_weather %>% select(Location, Year) %>% distinct())
  }
  
  # Summary of joined data
  cat("\nJoin Summary:\n")
  cat("- Total observations from sequential_yields:", nrow(sequential_yields), "\n")
  cat("- Total observations in joined dataset:", nrow(joined_data), "\n")
  
  # Check if all yield observations were preserved
  if (nrow(joined_data) == nrow(sequential_yields)) {
    cat("✓ All sequential yield observations were preserved in the join\n")
  } else {
    cat("WARNING: Some yield observations were lost in the join\n")
    cat("Expected:", nrow(sequential_yields), "observations, but got:", nrow(joined_data), "\n")
  }
  
  return(joined_data)
}

joined_dataset <- join_data(Yield, Weather)
saveRDS(joined_dataset, "feature_engineering/joined_dataset.rds")

#Prep data for NN: matrix, scale, etc. 
lstm_sequences <- function(GS_Wx, Annual_Yields, seq_len = 3) {
  
  
  #TODO: scale features
  
  
}