raw_wx_testing <- read.csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_GenotypeByEnvironment_PredictionCompetition_2023/Training_data/4_Training_Weather_Data_2014_2021.csv", na.strings = c("", "NA"))

cols_for_removal <- c("GWETPROF","GWETROOT","ALLSKY_SFC_PAR_TOT", "ALLSKY_SFC_SW_DNI")

library(dplyr)
clean_wx_testing <- raw_wx_testing %>% select(-all_of(cols_for_removal))

clean_wx_testing$Date <- as.Date(as.character(clean_wx_testing$Date), format = "%Y%m%d")

