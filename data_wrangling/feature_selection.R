source("test_wrngle_wx_data.R")

wx_Na_removed = na.omit(clean_wx)
nums = sapply(wx_Na_removed, is.numeric) 
corr_matrix = cor(wx_Na_removed[, nums])
write.csv(corr_matrix, file = "correlation_mat.csv", row.names = TRUE)

library(caret)
highCorr = sum(abs(corr_matrix[upper.tri(corr_matrix)]) > .950)

temp = wx_Na_removed[, nums]

nzv = nearZeroVar(temp)
print(nzv)
