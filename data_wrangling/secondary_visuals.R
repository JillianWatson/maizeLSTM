library(ggplot2)
library(patchwork)
source("~/maizeLSTM/data_wrangling/data_impute.R")

### using unique SpatialLoc key, plot distribution of each key over x amnt of years ###

Spatial_temporal_yearly_plots <- function(df) {
  
  get_fields <- names(df)[!names(df) %in% c("Env", "Year", "Date", "SpatialLoc")]
  #organize base environments and parameters
  plot_list <- list()
  
  for (loc in unique(df$SpatialLoc)) {
    current_env <- df %>% filter(SpatialLoc == loc)
    #produce stats for plots
    for (param in get_fields) {
      param_stats <- current_env %>%
        group_by(Year) %>%
        summarise(
          mean_val = mean(get(param), na.rm = TRUE),
          median_val = median(get(param), na.rm = TRUE),
          sd_val = sd(get(param), na.rm = TRUE),
          q25 = quantile(get(param), 0.25, na.rm = TRUE),
          q75 = quantile(get(param), 0.75, na.rm = TRUE),
          n_samples = n(),
          .groups = "drop"
        )  
      #build plots    
      p <- ggplot(param_stats, aes(x=factor(Year))) + 
        
        geom_rect(aes(
          xmin = as.numeric(factor(Year)) - 0.25,
          xmax = as.numeric(factor(Year)) + 0.25,
          ymin = q25,
          ymax = q75 
        ), fill = "cornflowerblue", alpha = 0.5) + 
        
        geom_line(aes(
          x = as.numeric(factor(Year)),
          y = mean_val,
          group = 1
        ), color = "darkseagreen", linewidth = 0.5) +
        
        geom_point(aes(y = median_val), color = "deeppink1", size = 2) + 
        
        geom_errorbar(aes(
          ymin = mean_val - sd_val,
          ymax = mean_val + sd_val
        ), width = 0.2, alpha = 0.5) + labs(
          title = paste(loc, "-", param),
          x = "Year",
          y = param,
          caption = paste("blue boxes: IQR\n",
                          "green line: Mean trend\n",
                          "pink points: Median\n",
                          "error bars: +-1 Standard Dev")
        ) + 
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10)
        )
      #append each env plots to plot list
      plot_list[[paste(loc, param, sep = "_")]] <- p
    }  
  }
  
  return(plot_list)
}

Spatial_temporal_yearly_plots(processing_wx_train)
