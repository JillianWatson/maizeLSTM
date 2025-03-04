library(stats)
library(lubridate)
library(tidyverse)


processing_yield_train <- readRDS("data_wrangling/processing_yield_train.rds")

# Helper Function to manually edit various date formats in processing_yield df
# date_vector: pass Date feature from a data frame
parse_raw_dates <- function(date_vector) {
  
  if(any(is.na(date_vector))) {
    cat("Note: NA value found\n")
  }
  
  parsed_dates <- sapply(date_vector, function(x){
    
    if(is.na(x)) return(NA)
    
    x <- trimws(x)
    
    tryCatch({
      
      #format 4/7/16 (mm/dd/yy)
      if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", x)) {
        parts <- strsplit(x, "/")[[1]]
        month <- as.numeric(parts[1])
        day <- as.numeric(parts[2])
        year <- as.numeric(parts[3])
        
        year <- year+ 2000
        return(ymd(sprintf("%04d-%02d-%02d", year, month, day)))
      }
      
      #format 4/25/2018 (mm/dd/yyyy)
      else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x)) {
        parts <- strsplit(x, "/")[[1]]
        month <- as.numeric(parts[1])
        day <- as.numeric(parts[2])
        year <- as.numeric(parts[3])
        return(ymd(sprintf("%04d-%02d-%02d", year, month, day)))
      }
      
      #format 2021-05-22 (yyyy-mm-dd)
      
      else if (grep1("^\\d{4}-\\d{2}-\\d{2}$", x)) {
        return(ymd(x))
      }
      
      else {
        warning(sprintf("Unrecognized date format: %s", x))
        return(NA)
      }
      
    }, error = function(e) {
      warning(sprintf("Error parsing date: %s", x))
      return(NA)
    })
  })
  
  result <- as.Date(parsed_dates)
  return(result)
  
}

Planted_Dates <- unique(processing_yield_train$Date_Planted)
Harvested_Dates <- unique(processing_yield_train$Date)

Planted_parsed <- parse_raw_dates(Planted_Dates)
Harvested_parsed <- parse_raw_dates(Harvested_Dates)

names(Planted_parsed) <- NULL
names(Harvested_parsed) <- NULL


#determine most common growth season among data (start and end months)
startof_season <- data.frame(Date_Planted = Planted_parsed) %>% mutate(
  planted_month = month(Date_Planted),
  month_name = month(Date_Planted, label = TRUE)
) %>%
  filter(!is.na(planted_month)) %>%
  count(planted_month, month_name) %>%
  arrange(desc(n)) %>%
  slice(1)
  
endof_season <- data.frame(Date_Harvested = Harvested_parsed) %>% mutate(
  harvested_month = month(Date_Harvested),
  month_name = month(Date_Harvested, label = TRUE)
) %>%
  filter(!is.na(harvested_month)) %>%
  count(harvested_month, month_name) %>%
  arrange(desc(n)) %>%
  slice(1)





