
raw_wx <- read.csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_GenotypeByEnvironment_PredictionCompetition_2023/Testing_data/4_Testing_Weather_Data_2022.csv", na.strings = c("", "NA"))

cols_for_removal <- c("GWETPROF","GWETROOT","ALLSKY_SFC_PAR_TOT", "ALLSKY_SFC_SW_DNI")

library(dplyr)
clean_wx <- raw_wx %>% select(-all_of(cols_for_removal))

clean_wx$Date <- as.Date(as.character(clean_wx$Date), format = "%Y%m%d")


