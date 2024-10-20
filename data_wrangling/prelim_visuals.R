library(ggplot2)
library(purrr)
library(dplyr)
library(lubridate)

source("~/maizeLSTM/data_wrangling/train_data.R")

#monthly distribution over all years 
plot_monthly_boxplots <- function(df) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  #new month column for easier grouping
  df$Month <- factor(month(df$Date, label = TRUE), levels = month.abb)
    
  plots <- map(numeric_cols, function(col) {
    ggplot(df, aes(x = Month, y = .data[[col]])) +
      geom_boxplot() +
      labs(title = paste("Monthly Boxplot of", col, "(All Years Combined"),
            x = "Month",
            y = col) +
      theme_minimal() 
    })
    
    names(plots) <- numeric_cols
    return(plots)
  }

#visualize distributions of each numerical field over entire data set, NOT subdivided by month, year, location, etc.
plot_histograms <- function(df) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  plots <- map(numeric_cols, function(col) {
    data <- df[[col]]
    
    #use freedman-diaconis for binning 
    iqr <- IQR(data, na.rm = TRUE)
    n <- sum(!is.na(data))
    bin_width <- 2 * iqr * n^(-1/3)
    
    data_range <- diff(range(data, na.rm = TRUE))
    bins <- ceiling(data_range / bin_width)
    
    ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(bins = bins, fill = "skyblue", color = "black") +
      labs(title = paste("Histogram of", col), 
           x = col, 
           y = "Count",
           subtitle = paste("Bins:", bins)) +
      theme_minimal()
  })
  
  names(plots) <- numeric_cols
  return(plots)
}

