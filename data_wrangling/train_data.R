library(dplyr)
library(lubridate)
library(zoo)
library(readr)
library(tidyr)
library(purrr)

#loading of raw meta data, used for weather station coordinates 
raw_meta_train <- read.csv("https://de.cyverse.org/anon-files/iplant/home/shared/commons_repo/curated/GenomesToFields_GenotypeByEnvironment_PredictionCompetition_2023/Training_data/2_Training_Meta_Data_2014_2021.csv", na.strings = c("", "NA"))

#loading of raw weather data, correct datatypes
raw_wx_train <- read.csv("https://de.cyverse.org/anon-files/iplant/home/shared/commons_repo/curated/GenomesToFields_GenotypeByEnvironment_PredictionCompetition_2023/Training_data/4_Training_Weather_Data_2014_2021.csv", na.strings = c("", "NA"))
raw_wx_train$Date <- as.Date(as.character(raw_wx_train$Date), format = "%Y%m%d")

#loading raw yield data, correct datatypes
raw_yield_train <- read.csv("https://de.cyverse.org/anon-files/iplant/home/shared/commons_repo/curated/GenomesToFields_GenotypeByEnvironment_PredictionCompetition_2023/Training_data/1_Training_Trait_Data_2014_2021.csv", na.strings = c("", "NA"))
raw_yield_train <- raw_yield_train %>% select("Env","Date_Planted", "Date_Harvested", "Yield_Mg_ha")
#for consistency, rename 'Date_Harvested' label in Yield data frame to 'Date'
names(raw_yield_train)[names(raw_yield_train) == "Date_Harvested"] <- "Date"

# Function to Look for initial cleansing indicators in raw data frames
#   df: raw_wx_train or raw_yield_train data frame
initial_summary <- function (df) {
  na_counts <- sapply(df, function(x) sum(is.na(x)))
  tot_rows <- nrow(df)
  
  stat_summ <- data.frame(
    NA_counts = na_counts,  
    NA_pct = numeric(length(df)),
    Tot_rows = tot_rows,
    Mean = numeric(length(df)),
    Variance = numeric(length(df)),
    Std = numeric(length(df)),
    Coeff_var = numeric(length(df)),
    #for outlier detection, will be +-3 from standard deviation
    Upper_bound = numeric(length(df)),
    Lower_bound = numeric(length(df))
  )
  
  outliers_list <- list()
  
  for (col in names(df)) {
   if(is.numeric(df[[col]])) {
     col_data <- df[[col]]
     removed_na_data <- col_data[!is.na(col_data)]
     
     stat_summ[col, "NA_pct"] <- round(mean(is.na(col_data)) * 100, 2)
     stat_summ[col, "Mean"] <- round(mean(removed_na_data, na.rm = TRUE), 2)
     stat_summ[col, "Variance"] <- round(var(removed_na_data, na.rm = TRUE), 2)
     stat_summ[col, "Std"] <- round(sd(removed_na_data, na.rm = TRUE), 2)
     stat_summ[col, "Upper_Bound"] <- round(stat_summ[col, "Mean"] + 3 * stat_summ[col, "Std"], 2)
     stat_summ[col, "Lower_Bound"] <- round(stat_summ[col, "Mean"] - 3 * stat_summ[col, "Std"], 2)
     
     outliers_list[[col]] <- removed_na_data[removed_na_data > stat_summ[col, "Upper_Bound"] | 
                                               removed_na_data < stat_summ[col, "Lower_Bound"]]
     
     #calculate co-variance
     mean_val <- mean(removed_na_data, na.rm = TRUE)
     if (mean_val != 0){
       cv <- (sd(removed_na_data, na.rm = TRUE) / mean_val) * 100
       stat_summ[col, "Coeff_var"] <- round(cv, 2)
     } else{
       stat_summ[col, "Coeff_var"] <- NA
     }
   }
  }
  return(list(summary = stat_summ, outliers = outliers_list))
}

saveRDS(raw_meta_train, "data_wrangling/raw_meta_train.rds")
saveRDS(raw_wx_train, "data_wrangling/raw_wx_train.rds")
saveRDS(raw_yield_train, "data_wrangling/raw_yield_train.rds")


