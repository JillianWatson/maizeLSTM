library(dplyr)
library(lubridate)

processing_wx_train <- readRDS("data_wrangling/processing_wx_train.rds")

generate_features <- function(df, start, end) {
  
  gs_features <- df %>%
    mutate(
      Month = month(Date),
      is_growing_season = Month >= start & Month <= end,
      vpd = calculate_vpd(T2M, RH2M),
      daily_gdd = calculate_gdd(T2M_MAX, T2M_MIN, base_temp = 10)
    ) %>%
    
    filter(is_growing_season) %>%
    group_by(SpatialLoc, Year) %>%
    summarise(
      mean_temp = mean(T2M, na.rm = TRUE),
      total_precip = sum(PRECTOTCORR, na.rm = TRUE),
      mean_vpd = mean(vpd, na.rm = TRUE),
      gdd_sum = sum(daily_gdd, na.rm = TRUE),
      n_days = n(),
      .groups = 'drop'
    ) %>%
    
    rename(Location = SpatialLoc)
  
  return(gs_features)
}


#helper function to calculate growing degree days
calculate_gdd <- function(tmax, tmin, base_temp = 10) {
  
  tmax <- pmin(tmax, 30)
  tavg <- (tmax + tmin) / 2
  gdd <- pmax(tavg - base_temp, 0)
  return(gdd)
}

# Helper function to calculate VPD
# temp_c: Uses wet bulb temperature in Celsius 
# rh_percent: relative humidity (%) represented as a value between 0-100
calculate_vpd <- function(temp_c, rh_percent) {
  
  # Calculate saturation vapor pressure (kPa)
  es <- 0.6108 * exp((17.27 * temp_c) / (temp_c + 237.3))
  # Calculate actual vapor pressure
  ea <- es * (rh_percent / 100)
  
  vpd <- es - ea
  return(vpd)
}

GS_Wx <- generate_features(processing_wx_train, 5, 10)

saveRDS(GS_Wx, "feature_engineering/GS_Wx.rds")

