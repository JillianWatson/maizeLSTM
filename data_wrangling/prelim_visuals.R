library(ggplot2)
library(purrr)
library(dplyr)
library(lubridate)

source("~/maizeLSTM/data_wrangling/train_data.R")

#monthly distribution over all years, all locations 
plot_monthly_boxplots <- function(df) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  #create month column for easier grouping
  df$Month <- factor(month(df$Date, label = TRUE), levels = month.abb)
    
  plots <- map(numeric_cols, function(col) {
    #remove assigning 'p <-' below to view original PRECTOTCORR box plot 
    p <- ggplot(df, aes(x = Month, y = .data[[col]])) +
      labs(title = paste("Monthly Boxplot of", col, "(All Years Combined"),
            x = "Month",
            y = col) +
        theme_minimal() 
    
    #initial scaling for PRECTOTCORR rendered difficult results for interpretation 
    #remove if/else/return(p) block to view original box plot 
    if (col == "PRECTOTCORR") {
      p <- p + geom_boxplot() + scale_y_sqrt() + 
        labs(y = paste(col, "(square root scale)"))
    } else {
      p <- p + geom_boxplot()
    }
    return(p)
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

#plot_monthly_boxplots(raw_wx_train)
#plot_histograms(raw_wx_train)
#plot_monthly_boxplots(raw_yield_train)
#plot_histograms(raw_yield_train)
