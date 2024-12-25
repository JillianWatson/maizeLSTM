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
  arrange(SpatialLoc, Year) %>%
  group_by(SpatialLoc)


# linear imputation of NA Yield values
# third transform step

processing_yield_train <- processing_yield_train %>%
  mutate(
    Yield_Impute = na.approx(Yield_Mg_ha, na.rm = FALSE)
  )

####################### WEATHER DATA ########################

processing_wx_train <- raw_wx_train %>%
  mutate(
    SpatialLoc = extract_location(Env)
  ) %>%
  arrange(SpatialLoc) %>%
  group_by(SpatialLoc)


