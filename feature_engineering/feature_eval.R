library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

GS_Wx <- readRDS("feature_engineering/GS_Wx.rds")
Annual_Yields <- readRDS("feature_engineering/Annual_Yields.rds")

# Function to help determine scale transformations needed for wx features
#   GS_Wx: growth season weather data frame
wx_feature_distribution <- function(GS_Wx){
  convert_long <- GS_Wx %>%
    select("mean_temp", "mean_vpd", "total_precip", "gdd_sum") %>%
    gather(key = "feature", value = "value")
  
  feature_skew <- convert_long %>%
    group_by(feature) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      sd  = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      range = max - min,
      skewness = moments::skewness(value, na.rm = TRUE)
    )
  return(feature_skew)
}

# TODO: filter for minimum consecutive sequences >= 3
# TODO: cross correlation analysis btwn dependent and indepndent variables
# TODO: volatility, trend, autocorrelation examination on yield

# Function to determine if there exists non-matching entries in both df's
join_compatibiliy <- function(GS_Wx, Annual_Yields) {
  wx_size <- nrow(GS_Wx)
  yld_size <- nrow(Annual_Yields)
  
  Wx_loc <- unique(GS_Wx$Location)
  yld_loc <- unique(Annual_Yields$Location)
  Wx_yr <- unique(GS_Wx$Year)
  yld_yr <- unique(Annual_Yields$Year)

  #number of observations
  wx_counts <- GS_Wx %>%
    group_by(Location, Year) %>%
    summarise(wx_count = n(), .groups = 'drop')
  
  yld_counts <- Annual_Yields %>%
    group_by(Location, Year) %>%
    summarise(yld_count = n(), .groups = 'drop')
  
  join_analysis <- full_join(wx_counts, yld_counts, by = c("Location", "Year")) %>%
    mutate(
      wx_present = !is.na(wx_count),
      yld_present = !is.na(yld_count)
    )
  
  join_stats <- list(
    datasets_size = list(
      weather_observations = wx_size,
      yield_observations = yld_size,
      size_difference = yld_size - wx_size
    ),
    join_summary = join_analysis %>%
      summarise(
        total_combinations = n(),
        complete_matches = sum(wx_present & yld_present),
        only_weather = sum(wx_present & !yld_present),
        only_yield = sum(!wx_present & yld_present),
        match_pct = round(100 * complete_matches / total_combinations, 2)
      ) 
    )
  return(join_stats)
}

# Function to print mismatched observations and subsequent deletion
find_mismatched_observations <- function(GS_Wx, Annual_Yields) {
  # Create location-year combinations for weather data
  wx_combinations <- GS_Wx %>%
    select(Location, Year) %>%
    distinct()
  
  # Create location-year combinations for yield data
  yield_combinations <- Annual_Yields %>%
    select(Location, Year) %>%
    distinct()
  
  # Find yield observations without corresponding weather data
  mismatched_observations <- yield_combinations %>%
    anti_join(wx_combinations, by = c("Location", "Year")) %>%
    arrange(Location, Year)
  
  return(mismatched_observations)
}

# Find and print the mismatches
mismatches <- find_mismatched_observations(GS_Wx, Annual_Yields)
print("Observations in yield data without corresponding weather data:")
print(mismatches)

# TODO: delete mismatches

#Prep data for NN
lstm_sequences <- function(GS_Wx, Annual_Yields, seq_len = 3) {
  
  join_data <- GS_Wx %>%
    inner_join(Annual_Yields, by = c("SpatialLoc", "Year"))
  
  #TODO: scale features
  #TODO: create sequences per each location
  
  
}