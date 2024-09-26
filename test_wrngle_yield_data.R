
raw_yld <- read.csv("https://de.cyverse.org/anon-files//iplant/home/shared/commons_repo/curated/GenomesToFields_GenotypeByEnvironment_PredictionCompetition_2023/Testing_data/Test_Set_Observed_Values_ANSWER.csv", na.strings = c("", "NA"))

hybrid <- c("Hybrid")

library(dplyr)
clean_yld <- raw_yld %>% select(-all_of(hybrid))



