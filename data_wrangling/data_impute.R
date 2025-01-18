source("~/maizeLSTM/data_wrangling/train_data.R")

###### Data used in this file has NOT been normalized ######

# Helper Function to extract location codes for easier grouping based on spatial
# patterns
# env: Env field in data frames

extract_location <- function(env) {
  location <- sub("_\\d{4}", "", env)
  return(location)
}

###################### YIELD DATA ##########################

#First Transform Step 
#
#Create year column
#Yield values do not have seasonal trends and are recorded on one day per year
#we will use year column for organizational purposes rather the date column

processing_yield_train <- raw_yield_train %>%
  mutate(
    Year = as.numeric(sub(".*_(\\d{4})$", "\\1", Env))
  )

#Second Transform Step
#
#Imputation of NA Yield values using predictive mean matching 

processing_yield_train <- processing_yield_train %>% group_by(Env) %>%
  mutate(Impute_Yield = ifelse(is.na(Yield_Mg_ha),rollapply(Yield_Mg_ha, width=7, FUN=mean, na.rm=TRUE,
         fill = "extend", align = "center"), 
         Yield_Mg_ha)
  ) %>%
  # for leading NA values, use first non-NA value
  mutate(  
    first_val = first(na.omit(Impute_Yield)),
    Impute_Yield = ifelse(is.na(Impute_Yield), first_val, Impute_Yield)       
  )  %>%
  select(-first_val)


#Third Transform Step
#
#extract spatial location code

processing_yield_train <- processing_yield_train %>%
  mutate(
    SpatialLoc = extract_location(Env)
  ) %>%
  arrange(SpatialLoc, Year) %>%
  group_by(SpatialLoc, Year)

####################### WEATHER DATA ########################

processing_wx_train <- raw_wx_train %>%
  mutate(
    SpatialLoc = extract_location(Env)
  ) %>%
  mutate(
    Year = as.numeric(sub(".*_(\\d{4})$", "\\1", Env))
  ) %>%
  arrange(SpatialLoc, Year) %>%
  group_by(SpatialLoc, Year)

####################### TODO: META DATA ############################

#First Transform Step
#
#Extract location codes
processing_meta_train <- raw_meta_train %>%
  mutate(
    SpatialLoc = extract_location(Env)
  ) %>%
  arrange(SpatialLoc, Year) %>%
  group_by(SpatialLoc, Year)

#Second Transform Step
#
#Remove unnecessary fields for analysis 

processing_meta_train <- 
  processing_meta_train %>%
  select("Year", "SpatialLoc", "Weather_Station_Latitude..in.decimal.numbers.NOT.DMS.", "Weather_Station_Longitude..in.decimal.numbers.NOT.DMS.")

#Third Transform Step
#
#Rename Lat x Long fields

names(processing_meta_train)[names(processing_meta_train) == "Weather_Station_Latitude..in.decimal.numbers.NOT.DMS."] <- "Latitude"
names(processing_meta_train)[names(processing_meta_train) == "Weather_Station_Longitude..in.decimal.numbers.NOT.DMS."] <- "Longitude"

#Forth Transform Step
#
#fill missing coordinates with first available values within the same spatial location

#create new imputed fields for easy viewing of changes
processing_meta_train$Impute_lat <- processing_meta_train$Latitude
processing_meta_train$Impute_long <- processing_meta_train$Longitude

for (loc in unique(processing_meta_train$SpatialLoc)) {
  current_loc <- processing_meta_train[processing_meta_train$SpatialLoc == loc, ]
    
  na_rows <- which(is.na(current_loc$Latitude) | is.na(current_loc$Longitude))
    
  if(length(na_rows) > 0) {
    for (row in na_rows) {
      #fill NA coordinates from first available coord set within the same spatial group
      get_next <- current_loc[which(!is.na(current_loc$Latitude) & !is.na(current_loc$Longitude))[1], ]
      
      #if coordinates not available in immediate group(SpatialLoc), use prefix of spatial code to impute NA values
      if (is.null(get_next) || nrow(get_next) == 0) {
        
        #save prefix of current group
        loc_prefix <- substr(loc, 1, nchar(loc)-1)
        
        #search for matching prefixes among all groups
        find_similar_loc <- processing_meta_train[grep(paste0("^", loc_prefix), processing_meta_train$SpatialLoc), ]
        
        #update value of coordinates from matched prefix code
        get_next <- find_similar_loc[which(!is.na(find_similar_loc$Latitude) & !is.na(find_similar_loc$Longitude))[1], ]
      }  
      #
      if (!is.null(get_next) && nrow(get_next) > 0){
          
          # Fill in the NA values with found coordinates
          processing_meta_train$Impute_lat[processing_meta_train$SpatialLoc == loc][row] <- get_next$Latitude
          processing_meta_train$Impute_long[processing_meta_train$SpatialLoc == loc][row] <- get_next$Longitude
      } 
    }
  }
}

#TXH4 being stubborn, manually imputing from TXH3 SpatialLoc
txh3_coords <- processing_meta_train[processing_meta_train$SpatialLoc =="TXH3", ]
txh3_most_recent <- txh3_coords[which(!is.na(txh3_coords$Latitude) & !is.na(txh3_coords$Longitude))[1], ]

processing_meta_train$Impute_lat[processing_meta_train$SpatialLoc == "TXH4"][row] <- txh3_most_recent$Latitude
processing_meta_train$Impute_long[processing_meta_train$SpatialLoc == "TXH4"][row] <- txh3_most_recent$Longitude


######################## Check Matching Keys between selected data frames ######################

# key formally exists as 'Env' in raw data, now re categorized to 'SpatialLoc'
# in any processed data frames

find_matching_keys <- function(df1, df2) {

  match <- full_join(
    data.frame(
      key = unique(df1$SpatialLoc),
      exists_df_one = TRUE
    ),
    data.frame(
      key = unique(df2$SpatialLoc),
      exists_df_two = TRUE
    ),
    by = "key"
  ) %>%
    mutate(
      exists_df_one = ifelse(is.na(exists_df_one), FALSE, exists_df_one),
      exists_df_two = ifelse(is.na(exists_df_two), FALSE, exists_df_two)
    ) %>%
    
    group_by(exists_df_one, exists_df_two) %>%
    summarize(count = n())
}





