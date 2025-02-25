library(ggplot2)
library(dplyr)
library(tidyr)
library(trend)
library(tseries)

#read in necessary data
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



# Function to analyse yield trends for each location, using the longest
# consecutive sequence available
#   min_consec_seq: minimum consecutive sequences allowed
analyse_yield_sequences <- function(Annual_Yields, min_consec_seq = 3) {
  
  # Helper function to find longest consecutive sequence of data in a single 
  # location
  longest_consec_seq <- function(location) {
    years <- sort(location$Year)
    
    if (length(years) <= 1) {
      
      warning(paste("Only 1 data input for location: ", location$Location[1]))
      return(years)
    }
    
    year_diffs <- diff(years)
    gap_position <- which(year_diffs > 1)
    
    if (length(gap_position) == 0) {
      
      warning(paste("Complete consecutive sequences for location: ", location$Location[1]))
      return(years)
    }
    
    #split sequences where gaps occur
    seq_start <- c(1, gap_position + 1)
    seq_end <- c(gap_position, length(years))
    
    #find longest sequence
    seq_lengths <- seq_end - seq_start + 1
    get_longest_index <- which.max(seq_lengths)
    longest_sequence <- years[seq_start[get_longest_index]: seq_end[get_longest_index]]
    
    return(longest_sequence)
  }
  
  
  # Function for examining yield volatility, autocorr, trend analysis for a 
  # single location
  location_analysis <- function(location_data) {
    loc_id <- location_data$Location[1]
    
    consecutive_years <- longest_consec_seq(location_data)
    get_consec_data <- location_data %>%
      filter(Year %in% consecutive_years) %>%
      arrange(Year)
    
    if (nrow(get_consec_data) < min_consec_seq ) {
      
      warning(paste("Location: ", loc_id, " did not meet minimum seq criteria"))
      return(NULL)
    }
    
    #minimum sequence condition met, begin analyses
    
    time_series_object <- ts(get_consec_data$mean_yield,
                            start = min(get_consec_data$Year),
                            frequency = 1)
    num_obs <- length(time_series_object)
    
    #calculate volatility
    volatility <- sd(get_consec_data$mean_yield, na.rm = TRUE)
    cv <- volatility / mean(get_consec_data$mean_yield, na.rm = TRUE) * 100
    
    #calculate year-over-year changes
    if (num_obs > 1) {
      
      annual_change <- diff(get_consec_data$mean_yield) / get_consec_data$mean_yield[-length(get_consec_data$mean_yield)] * 100
      volatility_annual_change <- sd(annual_change, na.rm = TRUE)
      max_annual_change <- max(abs(annual_change), na.rm = TRUE)
      positive_change <- sum(annual_change > 0, na.rm = TRUE) / length(annual_change)
    } else {
      
      annual_change <- NA
      volatility_annual_change <- NA
      max_annual_change <- NA
      positive_change <- NA
    }
    
    #calculate trend analysis:
    
    #linear regression
    if (num_obs >= 3) {
      years_numeric <- 1:length(time_series_object)
      trend_model <- lm(time_series_object ~ years_numeric)
      trend_slope <- coef(trend_model)[2]
      #p-value
      trend_significance <- summary(trend_model)$coefficients[2,4] 
      
      tryCatch({
        time_series_result <- sens.slope(time_series_object)
        theil_sen_slope <- time_series_result$estimates
        theil_sen_p_val <- time_series_result$p.value
      }, error = function(e){
            theil_sen_slope <- NA
            theil_sen_p_val <- NA
      })
      
      spearman_result <- cor.test(years_numeric, time_series_object, method = "spearman")
      spearman_rho <- spearman_result$estimate
      spearman_p_val <- spearman_result$p.value
      
    } else {
        trend_slope <- NA
        trend_significance <- NA
        theil_sen_slope <- NA
        theil_sen_p_val <- NA
        spearman_rho <- NA
        spearman_p_val <- NA
    }
    
    #count consecutive increases/decreases
    if (num_obs >= 3) {
      yield_diff <- as.numeric(diff(time_series_object))
      if (length(yield_diff) > 0) {
        tryCatch({
          runs <- rle(yield_diff > 0)
          n_runs <- length(runs$lengths)
          longest_run <- max(runs$lengths)
          avg_run_length <- mean(runs$lengths)
        }, error = function(e){
          warning(paste("Failed to compute runs analysis for location: ", loc_id))
          n_runs <- NA
          longest_run <- NA
          avg_run_length <- NA
        })
      } else {
        n_runs <- NA
        longest_run <- NA
        avg_run_length <- NA
      }
    } else {
      n_runs <- NA
      longest_run <- NA
      avg_run_length <- NA
    }  
    
    #autocorrelation analysis
    
    if (num_obs >= 4) {
      #set limit for smaller samples
      lag_max <- min(num_obs -1 ,3)
      acf_result <- tryCatch({
        acf(time_series_object, lag.max = lag_max, plot = FALSE)
      }, error = function(e){
        list(acf = rep(NA, lag_max + 1))
      })
      
      ac_lag1 <- if(length(acf_result$acf) > 1) acf_result$acf[2] else NA
    
      if (num_obs >= 6) {
        ljung_box <- tryCatch({
          Box.test(time_series_object, lag = min(2, num_obs - 2), type = 'Ljung-Box')
        }, error = function(e){
          list(p.value = NA)
        })
        ljung_box_p_val <- ljung_box$p.value
      } else {
        ljung_box_p_val <- NA
      }  
    } else {
      ac_lag1 <- NA
      ljung_box_p_val <- NA
      acf_result <- list(acf = NA)
    }
    
    #performance of metrics
    
    trend_reliability <- case_when(
      num_obs >= 10 ~ "Good",
      num_obs >= 6 ~ "Fair",
      num_obs >= 3 ~ "Poor",
      TRUE ~ "Insufficient"
    )
    
    autocorr_reliability <- case_when(
      num_obs >= 20 ~ "Good",
      num_obs >= 10 ~ "Fair",
      num_obs >= 4 ~ "Poor",
      TRUE ~ "Insufficient"
    )
    
    return(list(
      location = loc_id,
      n_years_total = nrow(location_data),
      n_years_consecutive = num_obs,
      consecutive_start = min(get_consec_data$Year),
      consecutive_end = max(get_consec_data$Year),
      consecutive_years = get_consec_data$Year,
      
      trend_reliability = trend_reliability,
      autocorrelation_reliability = autocorr_reliability,
      
      volatility = volatility,
      cv_percent = cv,
      volatility_annual = volatility_annual_change,
      max_annual_change = max_annual_change,
      positive_change_ratio = positive_change,
      
      trend_slope = trend_slope,
      trend_p_value = trend_significance,
      theil_sen_slope = theil_sen_slope,
      theil_sen_p_value = theil_sen_p_val,
      spearman_rho = spearman_rho,
      spearman_p = spearman_p_val,
      
      num_of_runs = n_runs,
      longest_run = longest_run,
      avg_run_length = avg_run_length,
      
      ac_lag1 = ac_lag1,
      ljung_box_p_value = ljung_box_p_val,
      
      ts_data = list(
        years = get_consec_data$Year,
        yields = get_consec_data$mean_yield,
        acf = acf_result$acf
      )
    ))  
  }
  
  locations <- unique(Annual_Yields$Location)
  results <- list()
  skipped_info <- data.frame(
    Location = character(),
    Total_years = numeric(),
    Max_consecutive = numeric(),
    Year_available = character(),
    Reason = character(),
    stringsAsFactors = FALSE
  )
  
  for (loc in locations) {
    loc_data <- Annual_Yields %>% filter(Location == loc)
    years <- sort(loc_data$Year)
    
    if (length(years) > 1){
      year_diffs <- diff(years)
      gap_position <- which(year_diffs > 1)
      
      if (length(gap_position) == 0) {
        max_consecutive <- length(years)
        consecutive_years <- years
      } else {
        seq_start <- c(1, gap_position + 1)
        seq_end <- c(gap_position, length(years))
        seq_lengths <- seq_end - seq_start + 1
        get_longest_index <- which.max(seq_lengths)
        max_consecutive <- seq_lengths[get_longest_index]
        consecutive_years <- years[seq_start[get_longest_index]: seq_end[get_longest_index]]
      }
    }else {
      max_consecutive <- length(years)
      consecutive_years <- years
    }
    
    if (length(years) < min_consec_seq || max_consecutive < min_consec_seq) {
      reason <- if (length(years) < min_consec_seq){
        "Insufficient total years"
      }else {
        "Insufficient consecutive years"
      }
      skipped_info <- rbind(skipped_info, data.frame(
        Location = loc,
        Total_years = length(years),
        Max_consecutive = max_consecutive,
        Years_available = paste(years, collapse = ", "),
        Reason = reason,
        stringsAsFactors = FALSE
      ))
      next
    }
    results[[as.character(loc)]] <- location_analysis(loc_data)
  }

  n_skipped <- nrow(skipped_info)
  
  if (n_skipped > 0) {
    warning(paste("Skipped", n_skipped, "locations due to having lack of data"))
  }
  
  results$skipped_info <- skipped_info
  
  valid_sequences <- list()
  for (loc in names(results)) {
    if (loc != "skipped_info") {
      if (!is.null(results[[loc]])) {
        valid_sequences[[loc]] <- results[[loc]]$consecutive_years
      }
    }
  }
  
  results$valid_sequences <- valid_sequences
  
  return(results) 
}



# Function to filter Annual_Yields to keep valid consecutive sequences only
filter_annual_yields <- function(Annual_Yields, Yield_analysis) {
  valid_sequences <- Yield_analysis$valid_sequences
  
  valid_combinations <- data.frame(
    Location = character(),
    Year = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (loc in names(valid_sequences)) {
    if (!is.null(valid_sequences[[loc]])) {
      years <- valid_sequences[[loc]]
      valid_combinations <- rbind(valid_combinations,
                                data.frame(
                                  Location = loc,
                                  Year = years,
                                  stringsAsFactors = FALSE
                                ))
    }
  }
  cat("Found", nrow(valid_combinations), "valid location-year combinations across", 
      length(unique(valid_combinations$Location)), "locations\n")
  filtered_data <- Annual_Yields %>%
    inner_join(valid_combinations, by = c('Location', 'Year'))
  cat("Filtered Annual Yields df from", nrow(Annual_Yields), "to", nrow(filtered_data), "rows\n")
  
  return(filtered_data)
  
}



# Function to print mismatched observations btwn data frames, if any
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



#Function Calls
mismatches <- find_mismatched_observations(GS_Wx, Annual_Yields)

Yield_analysis <- analyse_yield_sequences(Annual_Yields)

skipped_locations <- Yield_analysis$skipped_info

sequential_yields <- filter_annual_yields(Annual_Yields, Yield_analysis)
saveRDS(sequential_yields, "feature_engineering/Yield-tojoin.rds")



