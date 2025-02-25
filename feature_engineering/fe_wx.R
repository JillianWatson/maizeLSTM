library(dplyr)
library(lubridate)

processing_wx_train <- readRDS("data_wrangling/processing_wx_train.rds")

# Function to engineer weather features for lstm model
#   df: processing_wx_train 
#   start, end: integer values representing desired start and end months of
#               growth season
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


# Helper function to calculate growing degree days
#   tmax, tmin: maxima and minima temps for a given day. Units in Celsius
#   base_temp: 10 degrees Celsuis as per professional standards
calculate_gdd <- function(tmax, tmin, base_temp = 10) {
  
  tmax <- pmin(tmax, 30)
  tavg <- (tmax + tmin) / 2
  gdd <- pmax(tavg - base_temp, 0)
  return(gdd)
}

# Helper function to calculate Vapour Pressure Deficit
#   temp_c: Uses wet bulb temperature in Celsius 
#   rh_percent: relative humidity (%) represented as a value between 0-100
calculate_vpd <- function(temp_c, rh_percent) {
  
  # Calculate saturation vapor pressure (kPa)
  es <- 0.6108 * exp((17.27 * temp_c) / (temp_c + 237.3))
  # Calculate actual vapor pressure
  ea <- es * (rh_percent / 100)
  
  vpd <- es - ea
  return(vpd)
}

#Function to Impute missing years of wx data in order to join with sequential
#yield data
impute_missing_years <- function(GS_Wx) {
  #copy the original data
  imputed_Gs_Wx <- GS_Wx
  
  locations_for_imputation <- list(
    "ILH1" = c(2017, 2020),
    "INH1" = c(2017),
    "TXH2" = c(2015, 2016, 2017)
  )
  
  for (loc in names(locations_for_imputation)) {
    cat("Processing location: ", loc, "\n")
  
    loc_data <- GS_Wx %>%
      filter(Location == loc) %>%
      arrange(Year)
  
    available_years <- loc_data$Year
    
    for (missing_year in locations_for_imputation[[loc]]) {
      years_prev <- available_years[available_years < missing_year]
      years_after <- available_years[available_years > missing_year]
      
      if (length(years_prev) > 0 && length(years_after) > 0) {
        closest_prev <-max(years_prev)
        closest_after <- min(years_after)
        
        #data from closest years
        previous_data <- loc_data %>% filter(Year == closest_prev)
        after_data <- loc_data %>% filter(Year == closest_after)
        
        #linear interpolation based on time difference
        weight_after <- (missing_year - closest_prev) / (closest_after - closest_prev)
        weight_before <- 1 - weight_after
        
        imputed_row <- previous_data
        imputed_row$Year <- missing_year
        
        #interpolate numerical columns
        numerical_cols <- c("mean_temp", "mean_vpd", "total_precip", "gdd_sum")
        for (col in numerical_cols) {
          imputed_row[[col]] <- previous_data[[col]] * weight_before + after_data[[col]] * weight_after
        }
        imputed_row$imputed <- TRUE
        imputed_Gs_Wx <- bind_rows(imputed_Gs_Wx, imputed_row)
        
        cat("    Imputed using years", closest_prev, "and", closest_after, "\n")
        
      } else if (length(years_prev) > 0) {
        # If we only have data before the missing year, use the most recent
        closest_year <- max(years_prev)
        imputed_row <- loc_data %>% filter(Year == closest_year)
        imputed_row$Year <- missing_year
        imputed_row$imputed <- TRUE
        imputed_GS_Wx <- bind_rows(imputed_GS_Wx, imputed_row)
        
        cat("    Imputed using previous year", closest_year, "\n")
        
      } else if (length(years_after) > 0) {
        # If we only have data after the missing year, use the earliest
        closest_year <- min(years_after)
        imputed_row <- loc_data %>% filter(Year == closest_year)
        imputed_row$Year <- missing_year
        imputed_row$imputed <- TRUE
        imputed_GS_Wx <- bind_rows(imputed_GS_Wx, imputed_row)
        
        cat("    Imputed using following year", closest_year, "\n")
        
      } else {
        warning(paste("Cannot impute year", missing_year, "for location", loc, "- no reference data"))
      }
      
    }
  }
  
  imputed_Gs_Wx <- imputed_Gs_Wx %>%
    arrange(Location, Year)
  imputed_Gs_Wx$imputed <- ifelse(is.na(imputed_Gs_Wx$imputed), FALSE, imputed_Gs_Wx$imputed)
  return (imputed_Gs_Wx) 
}


#used in feature_eval.R
GS_Wx <- generate_features(processing_wx_train, 5, 10)
saveRDS(GS_Wx, "feature_engineering/GS_Wx.rds")

#used in final_dataset.R
WxFinal_forjoin <- impute_missing_years(GS_Wx)
saveRDS(WxFinal_forjoin, "feature_engineering/Wx-tojoin.rds")

