library(dplyr)

#loading and initial cleansing of raw weather testing data

raw_wx_test <- read.csv("https://de.cyverse.org/anon-files/iplant/home/shared/commons_repo/curated/GenomesToFields_GenotypeByEnvironment_PredictionCompetition_2023/Testing_data/4_Testing_Weather_Data_2022.csv", na.strings = c("", "NA"))
cols_for_removal = c("GWETPROF","GWETROOT","ALLSKY_SFC_PAR_TOT", "ALLSKY_SFC_SW_DNI")
clean_wx_test <- raw_wx_test %>% select(-all_of(cols_for_removal))
clean_wx_test$Date = as.Date(as.character(clean_wx_test$Date), format = "%Y%m%d")









