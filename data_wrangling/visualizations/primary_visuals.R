library(ggplot2)
library(patchwork)

processing_wx_train <- readRDS("processing_wx_train.rds")
processing_yield_train <- readRDS("processing_yield_train.rds")

########### Data used in this file has not been normalized #############

# Using unique SpatialLoc key, plot yield distribution of each key over x amnt
# of years
# df: processing_yield_train data frame
Spatial_annual_Yield <- function(df) {
    plot_list <- list()
    
    get_data <- names(df)[names(df) %in% c("Impute_Yield")]
    
    for (loc in unique(df$SpatialLoc)) {
      current_loc <- df %>% filter(SpatialLoc == loc)
      
      for (data in get_data) {
        yield_stats <- current_loc %>% 
          group_by(Year) %>%
          summarise(
            mean_yield = mean(get(data), na.rm = TRUE),
            median_yield = median(get(data), na.rm = TRUE),
            sd_yield = sd(get(data), na.rm = TRUE),
            q25 = quantile(get(data), 0.25, na.rm = TRUE),
            q75 = quantile(get(data), 0.75, na.rm = TRUE),
            n_samples = n(),
            .groups = "drop"
          )
        
        #build plots    
        p <- ggplot(yield_stats, aes(x=factor(Year))) + 
          
          geom_rect(aes(
            xmin = as.numeric(factor(Year)) - 0.25,
            xmax = as.numeric(factor(Year)) + 0.25,
            ymin = q25,
            ymax = q75 
          ), fill = "darkolivegreen1", alpha = 0.5) +
          
          geom_line(aes(
            x = as.numeric(factor(Year)),
            y = mean_yield,
            group = 1
          ), color = "blueviolet", linewidth = 0.5) +
          
          geom_point(aes(y = median_yield), color = "deeppink1", size = 2) + 
          
          geom_errorbar(aes(
          ymin = mean_yield - sd_yield,
          ymax = mean_yield + sd_yield
        ), width = 0.2, alpha = 0.5) + labs(
          title = paste(loc, "-", data),
          x = "Year",
          y = data,
          caption = paste("green boxes: IQR\n",
                          "purple line: Mean trend\n",
                          "pink points: Median\n",
                          "error bars: +-1 Standard Dev")
          ) +
          theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 10)
          )
        #append each locations plots to plot list
        plot_list[[paste(loc, data, sep = "_")]] <- p
      }
    }
  return(plot_list)  
}


# Using unique SpatialLoc key, plot weather pattern distribution of each key 
# over x amnt of years
# df: processing_wx_train dataframe
Spatiotemporal_annual__Wx <- function(df) {
  
  get_fields <- names(df)[!names(df) %in% c("Env", "Year", "Date", "SpatialLoc")]
  #organize base environments and parameters
  plot_list <- list()
  
  for (loc in unique(df$SpatialLoc)) {
    current_loc <- df %>% filter(SpatialLoc == loc)

    for (param in get_fields) {
      param_stats <- current_loc %>%
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
      #append each locations plots to plot list
      plot_list[[paste(loc, param, sep = "_")]] <- p
    }  
  }
  return(plot_list)
}


# Plotting each weather parameter to yield 
# df_yield: processing_yield_train
# df_wx: processing_wx_train

correlation_annual <- function(df_yield, df_wx) {
  
  wx_params <- names(df_wx)[!names(df_wx) %in% c("Env", "Year", "Date", "SpatialLoc")]
  
  correlation_plot_list <- list()
  
  for (loc in unique(df_yield$SpatialLoc)) {
    loc_yield <- df_yield %>% filter(SpatialLoc == loc)
    loc_wx <- df_wx %>% filter(SpatialLoc == loc)
    
    for (param in wx_params) {
      annual_wx <- loc_wx %>%
        group_by(Year) %>%
        summarise(
          wx_val = mean(get(param), na.rm = TRUE),
          .groups = "drop"
        )
      
      annual_yield <- loc_yield %>%
        group_by(Year) %>%
        summarise(
          yield_val = Impute_Yield,
          .groups = "drop"
        )
      
      join_data <- inner_join(annual_yield, annual_wx, by = "Year")
      
      #generate plots
      #TODO: incorporate color association per each year for wx params
      
      p <- ggplot(join_data, aes(
        x = yield_val,
        y = wx_val)) + 
        
        geom_point(shape = 1, fill = "cadetblue3") + 
        geom_smooth(method = "lm", color = "darkslategrey") +
        labs(
          title = paste(loc, "-", param, "Vs Yield"),
          x = "Yield",
          y = param
        ) +
        theme_minimal()
      
      correlation_plot_list[[paste(loc, param, sep = "_")]] <- p
      
    }
  }
  
  return(correlation_plot_list)
}


#generate all wx plots
Spatiotemporal_annual__Wx(processing_wx_train)

#generate all yield plots
Spatial_annual_Yield(processing_yield_train)

#generate all relationship plots
correlation_annual(processing_yield_train, processing_wx_train)


# Helper function display plots for specified location
# location_code: a code found under SpatialLoc field in data frames 

display_key_plots <- function(plot_list, location_code) {
  loc_plots <- plot_list[grep(paste0("^", location_code), names(plot_list))]
  wrap_plots(loc_plots, ncol = 2)
}
