library(stats)

processing_wx_train <- readRDS("processing_wx_train.rds")

########## data frame with prefix 'processing' are to be used in this file ###########

#TODO: feature engineer new wx df based on below stats

# start: starting month for desired growth period, a value between 1-12
# end: ending month for desired growth period, a value between 1-12
generate_annual_features <- function(df, start, end) {
  
  location_code <- unique(df$SpatialLoc)
  
  annual_features <- lapply(location_code, function(loc) {
    
    location_data <- df %>% filter(SpatialLoc == loc) %>%
      mutate(
        Month = month(Date),
        is_growing_season = Month >= start & Month <= end,
        
        #calculate vapor pressure deficit
        vpd = calculate_vpd(T2M, RH2M)
      )
    
    growth_szn_stats <- location_data %>%
      filter(is_growing_season) %>%
      group_by(Year) %>%
      summarise(
        #temperature
        gs_temp_mean = mean(T2M, na.rm = TRUE),
        gs_temp_max = mean(T2M_MAX, na.rm = TRUE),
        gs_temp_min = mean(T2M_MIN, na.rm = TRUE),
        gs_temp_range = mean(T2M_MAX - T2M_MIN, na.rm = TRUE),
        
        #moisture
        gs_precip_tot = sum(PRECTOTCORR, na.rm = TRUE),
        gs_precip_days = sum(PRECTOTCORR > 1, na.rm = TRUE),
        gs_rh_mean = mean(RH2M, na.rm = TRUE),
        gs_vpd_mean = mean(vpd, na.rm = TRUE),
        
        #radiation 
        gs_par_tot = sum(ALLSKY_SFC_PAR_TOT, na.rm = TRUE),
        gs_dwn_mean = mean(ALLSKY_SFC_SW_DWN, na.rm = TRUE),
        gs_dni_mean = mean(ALLSKY_SFC_SW_DNI, na.rm = TRUE),
        
        #soil moisture
        gs_soil_moisture_mean = mean(GWETPROF, na.rm = TRUE),
        gs_rootzone_moisture_mean = mean(GWETROOT, na.rm = TRUE),
        gs_surface_wetness_mean = mean(GWETTOP, na.rm = TRUE),
        
        #stressors (heat, drought)
        gs_heat_stress = sum(T2M_MAX > 30, na.rm = TRUE),
        gs_drought_stress = sum(GWETROOT < 0.3, na.rm = TRUE)
      ) 
    
    annual_stats <- location_data %>%
      group_by(Year) %>%
      summarise(
        
        #temperature extremes
        annual_temp_max = max(T2M_MAX, na.rm = TRUE),
        annual_temp_min = min(T2M_MIN, na.rm = TRUE),
        
        #moisture
        annaul_precip_tot = sum(PRECTOTCORR, na.rm = TRUE),
        annual_precip_days = sum(PRECTOTCORR > 1, na.rm = TRUE),
        
        #radiation
        annual_par_tot = sum(ALLSKY_SFC_PAR_TOT, na.rm = TRUE),
        
        #wind
        annual_wind_mean = mean(WS2M, na.rm = TRUE)
      )
    
    features <- growth_szn_stats %>%
      left_join(annual_stats, by = "Year")
    return(features)
  })
  
  names(annual_features) <- location_code
  return(annual_features)
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


generate_annual_features(processing_wx_train, 5, 10)
