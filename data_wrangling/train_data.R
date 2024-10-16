library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(lubridate)

# loading of raw weather data, correct datatypes

raw_wx_train <- read.csv("https://de.cyverse.org/anon-files/iplant/home/shared/commons_repo/curated/GenomesToFields_GenotypeByEnvironment_PredictionCompetition_2023/Training_data/4_Training_Weather_Data_2014_2021.csv", na.strings = c("", "NA"))
raw_wx_train$Date <- as.Date(as.character(raw_wx_train$Date), format = "%Y%m%d")

#loading raw yield data, correct datatypes

raw_yield_train <- read.csv("https://de.cyverse.org/anon-files/iplant/home/shared/commons_repo/curated/GenomesToFields_GenotypeByEnvironment_PredictionCompetition_2023/Training_data/1_Training_Trait_Data_2014_2021.csv", na.strings = c("", "NA"))
raw_yield_train <- raw_yield_train %>% select("Env", "Date_Harvested", "Yield_Mg_ha")
raw_yield_train$Date_Harvested <- as.Date(raw_yield_train$Date_Harvested, format = "%m/%d/%y") %>% format("%Y-%m%-%d")

#look for initial cleansing indicators  
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
    Coeff_var = numeric(length(df))
  )
  
  for (col in names(df)) {
   if(is.numeric(df[[col]])) {
     col_data <- df[[col]]
     removed_na_data <- col_data[!is.na(col_data)]
     
     stat_summ[col, "NA_pct"] <- round(mean(is.na(col_data)) * 100, 2)
     stat_summ[col, "Mean"] <- round(mean(removed_na_data, na.rm = TRUE), 2)
     stat_summ[col, "Variance"] <- round(var(removed_na_data, na.rm = TRUE), 2)
     stat_summ[col, "Std"] <- round(sd(removed_na_data, na.rm = TRUE), 2)
     
     mean_val <- mean(removed_na_data, na.rm = TRUE)
     if (mean_val != 0){
       cv <- (sd(removed_na_data, na.rm = TRUE) / mean_val) * 100
       stat_summ[col, "Coeff_var"] <- round(cv, 2)
     } else{
       stat_summ[col, "Coeff_var"] <- NA
     }
   }
  }
  print(stat_summ)
}

wx_na <- initial_summary(raw_wx_train)
yld_na <- initial_summary(raw_yield_train)  

