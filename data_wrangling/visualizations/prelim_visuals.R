library(ggplot2)
library(purrr)
library(dplyr)
library(lubridate)

source("~/maizeLSTM/data_wrangling/train_data.R")

# Monthly distribution over all years, all locations 
# df: raw_wx_train or raw_yield_train data frame
plot_monthly_boxplots <- function(df) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  #create month column for easier grouping
  df$Month <- factor(month(df$Date, label = TRUE), levels = month.abb)
    
  plots <- map(numeric_cols, function(col) {
    #remove assigning 'p <-' below to view original PRECTOTCORR box plot 
    p <- ggplot(df, aes(x = Month, y = .data[[col]])) +
      labs(title = paste("Monthly Boxplot of", col, "(All Years Combined)"),
            x = "Month",
            y = col) +
        theme_minimal() 
    
    #initial scaling for PRECTOTCORR rendered difficult results for interpretation 
    #comment out if/else/return(p) block to view original box plot 
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

# Visualize distributions of each numerical fields over entire data set,
# NOT subcategorized by month, year, location, etc.
# df: raw_wx_train or raw_yield_train data frame
plot_histograms <- function(df) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  plots <- map(numeric_cols, function(col) {
    data <- df[[col]]
    
    if (col == "PRECTOTCORR") {
      
      #customize bins to handle zero-inflation
      breaks <- c(0, 1, 2.5, 5, 7.5, 15, 30, 50)
      
      ggplot(df, aes(x = .data[[col]])) + 
        geom_histogram(breaks = breaks, fill = "cornflowerblue", color = "black") + 
        labs(title = paste("Histogram of", col),
             x = col,
             y = "Count") +
        scale_x_continuous(breaks = c(0, 2.5, 5, 7.5, 10, 20, 30, 40, 50)) +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else{
    
      #use freedman-diaconis for binning 
      iqr <- IQR(data, na.rm = TRUE)
      n <- sum(!is.na(data))
      bin_width <- 2 * iqr * n^(-1/3)
    
      data_range <- diff(range(data, na.rm = TRUE))
      bins <- ceiling(data_range / bin_width)
    
      ggplot(df, aes(x = .data[[col]])) +
        geom_histogram(bins = bins, fill = "cornflowerblue", color = "black") +
        labs(title = paste("Histogram of", col), 
            x = col, 
            y = "Count",
            subtitle = paste("Bins:", bins)) + 
        theme_minimal()
    }
  })
  
  names(plots) <- numeric_cols
  return(plots)
}

#select df for visualization

#plot_monthly_boxplots(raw_wx_train)
#plot_histograms(raw_wx_train)
#plot_monthly_boxplots(raw_yield_train)
#plot_histograms(raw_yield_train)
