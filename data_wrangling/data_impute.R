source("~/maizeLSTM/data_wrangling/train_data.R")

###################### YIELD DATA ##########################

# create a year column
# the yield values do not have seasonal trends and are recorded on one day each year. we will use
# the year column for organizational purposes rather than the date column

# first transform step 

processing_yield_train <- raw_yield_train %>%
  mutate(
    Year = as.numeric(sub(".*_(\\d{4})$", "\\1", Env))
    )

# create a column to group data according to the first 4 four chars of 'env', or
# group based on spatial patterns

extract_location <- function(env) {
  location <- sub("_\\d{4}", "", env)
  return(location)
}

# second transform step

processing_yield_train <- processing_yield_train %>%
  mutate(
    SpatialLoc = extract_location(Env)
  ) %>%
  arrange(Env, Year) %>%
  group_by(Env)


# imputation of NA Yield values using predictive mean matching 
# third transform step

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

####################### WEATHER DATA ########################

processing_wx_train <- raw_wx_train %>%
  mutate(
    SpatialLoc = extract_location(Env)
  ) %>%
  arrange(SpatialLoc) %>%
  group_by(SpatialLoc)


