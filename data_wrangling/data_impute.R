raw_meta_train <- readRDS("raw_meta_train.rds")
raw_wx_train <- readRDS("raw_wx_train.rds")
raw_yield_train <- readRDS("raw_yield_train.rds")

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

write.csv(processing_yield_train, "Yield.csv", row.names = FALSE)

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

####################### META DATA ############################

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

#from manual analysis of meta data (City, Farm), fixed some imputed coordinates for 
#better representation of location

#NEH1_2016 - Nebraska Lincoln University and Area. Similar to NEH1_2015
neh1 <- processing_meta_train[processing_meta_train$SpatialLoc =="NEH1", ]

neh1_2015 <- neh1[which(!is.na(neh1$Latitude) & !is.na(neh1$Longitude))[2], ]

processing_meta_train$Impute_lat[processing_meta_train$SpatialLoc == "NEH1"][3] <- neh1_2015$Latitude
processing_meta_train$Impute_long[processing_meta_train$SpatialLoc == "NEH1"][3] <- neh1_2015$Longitude

#NEH2_2018 - Wahoo, Nebraska(Lincoln University), Similar to NEH1_2020
neh1_2020 <-neh1[which(!is.na(neh1$Latitude) & !is.na(neh1$Longitude))[3], ]

processing_meta_train$Impute_lat[processing_meta_train$SpatialLoc == "NEH2"][3] <- neh1_2020$Latitude
processing_meta_train$Impute_long[processing_meta_train$SpatialLoc == "NEH2"][3] <- neh1_2020$Longitude

#NEH2_2019 - Nebraska Lincoln University and Area, Similar to NEH1_2020
processing_meta_train$Impute_lat[processing_meta_train$SpatialLoc == "NEH2"][4] <- neh1_2020$Latitude
processing_meta_train$Impute_long[processing_meta_train$SpatialLoc == "NEH2"][4] <- neh1_2020$Longitude

#NEH3_2017 - Nebraska Lincoln University and Area. Similar to NEH1_2015
processing_meta_train$Impute_lat[processing_meta_train$SpatialLoc == "NEH3"][3] <- neh1_2015$Latitude
processing_meta_train$Impute_long[processing_meta_train$SpatialLoc == "NEH3"][3] <- neh1_2015$Longitude

#NEH3_2020 - North Platte, Nebraska. Similar to NEH2_2014
neh2 <- processing_meta_train[processing_meta_train$SpatialLoc =="NEH2", ]

neh2_2014 <- neh2[which(!is.na(neh2$Latitude) & !is.na(neh2$Longitude))[1], ]

processing_meta_train$Impute_lat[processing_meta_train$SpatialLoc == "NEH3"][4] <- neh2_2014$Latitude
processing_meta_train$Impute_long[processing_meta_train$SpatialLoc == "NEH3"][4] <- neh2_2014$Longitude

#TXH2_2019 - College Station, Texas. Similar to TXH2_2020
txh2 <- processing_meta_train[processing_meta_train$SpatialLoc =="TXH2", ]

txh2_2020 <- txh2[which(!is.na(txh2$Latitude) & !is.na(txh2$Longitude))[2], ]

processing_meta_train$Impute_lat[processing_meta_train$SpatialLoc == "TXH2"][6] <- txh2_2020$Latitude
processing_meta_train$Impute_long[processing_meta_train$SpatialLoc == "TXH2"][6] <- txh2_2020$Longitude

#TXH4_2019 - Lubbock, Texas. Similar to TXH2_{2014-2018}
txh2_2014 <- txh2[which(!is.na(txh2$Latitude) & !is.na(txh2$Longitude))[1], ]

processing_meta_train$Impute_lat[processing_meta_train$SpatialLoc == "TXH4"][1] <- txh2_2014$Latitude
processing_meta_train$Impute_long[processing_meta_train$SpatialLoc == "TXH4"][1] <- txh2_2014$Longitude


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

saveRDS(processing_meta_train, "processing_meta_train.rds")
saveRDS(processing_wx_train, "processing_wx_train.rds")
saveRDS(processing_yield_train, "processing_yield_train.rds")




