library(keras)
library(tensorflow)

# to be determined
features <- c()

#look back == sequence length
look_back <- 12

forecast_horizon <- 1

CropPredictor <- function(look_back, growth_szn_months = list(start = 5, end = 10)){}


build_model <- function(predictor, n_features) {
  model <- keras_model_sequential() %>%
    layer_lstm(units = 64,
               input_shape = c(predictor$sequence_length, n_features),
               return_sequences = TRUE) %>%
    layer_dropout(rate = 0.2) %>%
    layer_lstm(units = 32) %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss = "mse",
    metrics = c("mae")
  )
  
  predictor$model <- model
  return(predictor)
}







             

