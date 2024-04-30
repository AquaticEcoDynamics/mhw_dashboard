# Script to hold functions and other functionality for demoMHW app

# Functions ---------------------------------------------------------------

# Function for ensuring 366 DOY for non-leap years
leap_every_year <- function(x) {
  ifelse(yday(x) > 59 & leap_year(x) == FALSE, yday(x) + 1, yday(x))
}

# Function for processing data
process_data <- function(df) {
  df <- df %>% relocate(analysed_sst, .after = time) %>%
    rename(t = time, temp = analysed_sst) %>%
    select(t, temp) %>%
    mutate(t = as.Date(t))
  return(df)
}

# Time plot wrapper
time_plot <- function(time_span, df, time_highlight){
  
  # Check that ts has a 't' and 'temp' column
  col_t_error <- "Ensure uploaded time series has a date column 't'."
  col_temp_error <- "Ensure uploaded time series has a temperature column 'temp'."
  col_error <- NULL
  if(!"t" %in% colnames(df)) 
    col_error <- col_t_error
  if(!"temp" %in% colnames(df)){
    if(is.null(col_error)){
      col_error <- col_temp_error
    } else {
      col_error <- paste0(col_t_error,"\n",col_temp_error)
    }
  }
  
  # Plot error message
  if(!is.null(col_error)){
    timePlot <- ggplot() + 
      geom_text(aes(x = 1, y = 1, label = col_error), size = 12) +
      labs(x = NULL, y = "Temperature [°C]") + theme_bw(base_size = 30)
    return(timePlot)
  }
  
  # Get time series and modify t columns
  df <- df |> 
    mutate(all = "All",
           month = lubridate::month(t, label = TRUE),
           year = lubridate::year(t),
           doy = leap_every_year(t),
           day = t)
  
  # Set t column to timeSelect
  if(time_span == "All"){
    df$t <- df$all
  } else if(time_span == "Month"){
    df$t <- df$month
  } else if(time_span == "Year"){
    df$t <- df$year
  } else if(time_span == "DOY"){
    df$t <- df$doy
  } else if(time_span == "Day"){
    df$t <- df$day
  }
  
  if(time_span == "DOY"){
    box_line_width = 0.1 
  } else if(time_span == "Year") {
    box_line_width = 1
  } else if(time_span == "Month") {
    box_line_width = 2
  } else if(time_span == "All") {
    box_line_width = 3
  }
  
  # Base plot
  timePlot <- ggplot(data = df, aes(x = t, y = temp)) + 
    labs(x = NULL, y = "Temperature [°C]") + theme_bw(base_size = 30)
  
  # Add points
  if(time_highlight == "None")
    timePlot <- timePlot + geom_point(position = "jitter")
  if(time_highlight == "Month")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = month)) + 
    scale_colour_viridis_d() + guides(colour = guide_legend(override.aes = list(shape = 15, size = 10)))
  if(time_highlight == "Year")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = year)) + 
    scale_colour_viridis_c(option = "A")
  if(time_highlight == "DOY")
    timePlot <- timePlot + geom_point(position = "jitter", aes(colour = doy)) + 
    scale_colour_viridis_c(option = "B")
  
  # Add boxplots
  if(time_span != "Day"){
    timePlot <- timePlot + geom_boxplot(aes(group = t), linewidth = box_line_width,
                                        colour = "grey", alpha = 0.4, outlier.shape = NA)
  }
  
  # Scale x axis
  if(time_span == "All")  timePlot <- timePlot + scale_x_discrete(expand = c(0, 0))
  if(time_span %in% c("Year", "DOY"))  timePlot <- timePlot + scale_x_continuous(expand = c(0, 0))
  if(time_span == "Day")  timePlot <- timePlot + scale_x_date(expand = c(0, 0))
  
  # Exit
  timePlot
}

# exceedance plot
exceedance_plot <- function(thresh, df){
  
  # Check that ts has a 't' and 'temp' column
  col_t_error <- "Ensure uploaded time series has a date column 't'."
  col_temp_error <- "Ensure uploaded time series has a temperature column 'temp'."
  col_error <- NULL
  if(!"t" %in% colnames(df)) 
    col_error <- col_t_error
  if(!"temp" %in% colnames(df)){
    if(is.null(col_error)){
      col_error <- col_temp_error
    } else {
      col_error <- paste0(col_t_error,"\n",col_temp_error)
    }
  }
  
  # Plot error message
  if(!is.null(col_error)){
    timePlot <- ggplot() + 
      geom_text(aes(x = 1, y = 1, label = col_error), size = 12) +
      labs(x = NULL, y = "Temperature [°C]") + theme_bw(base_size = 30)
    return(exceedancePlot)
  }
  exc <- exceedance(df, threshold = thresh)
  
  # Base plot
  exceedancePlot <- ggplot(data = exc$threshold, aes(x = t)) +
    geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = FALSE) +
    geom_line(aes(y = temp, colour = "temp")) +
    geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
    scale_colour_manual(name = "Legend", values = c("temp" = "black", "thresh" = "forestgreen")) +
    scale_fill_manual(name = "Event Colour", values = c("all" = "salmon")) +
    guides(colour = guide_legend(override.aes = list(fill = NA))) +
    scale_x_date(date_labels = "%b %Y") +
    labs(x = "", y = "Temperature [\u00B0C]") +
    theme_bw(base_size = 30) +
    theme(plot.caption = element_text(hjust = 0))
  
  # Exit
  exceedancePlot
}