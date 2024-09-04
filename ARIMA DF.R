# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forecast)  # For ARIMA modeling and forecasting
library(lubridate) # For date formatting
library(gridExtra) # For arranging plots

# Load the data
data <- read.csv("D:/Inflation_FINAL.csv")

# Specify the column names that you want to convert to time series
columns <- c("D1", "E1", "F1", "G1", "H1", "I1", "J1", "K1")

# Initialize an empty list to store the time series and forecasts
time_series_list <- list()
forecast_list <- list()

# Convert each column to a time series object
for (col in columns) {
  ts_object <- ts(data[[col]], frequency = 4, start = c(1995, 1))  # Adjust the start date accordingly
  time_series_list[[col]] <- ts_object
}

# Forecast horizon (5 years = 20 quarters)
forecast_horizon <- 20

# Initialize an empty list to store plots
plots <- list()

# Initialize an empty data frame to store all forecasts
all_forecasts_df <- data.frame()

# Fit ARIMA model, forecast, and plot for each time series
for (i in 1:length(time_series_list)) {
  series <- time_series_list[[i]]
  
  # Fit ARIMA model
  fit <- auto.arima(series, seasonal = TRUE)
  
  # Forecast the next 20 quarters
  forecast_result <- forecast(fit, h = forecast_horizon)
  
  # Store the forecast
  forecast_list[[columns[i]]] <- forecast_result
  
  # Create the plot
  p <- autoplot(series) +
    autolayer(forecast_result$mean, series = paste("Forecast", columns[i]), PI = FALSE, linetype = "dashed") +
    ggtitle(paste("Time Series:", columns[i])) +
    theme_minimal()
  
  plots[[i]] <- p
  
  # Prepare forecast data with dates and series names
  forecast_dates <- time(forecast_result$mean)
  forecast_values <- as.numeric(forecast_result$mean)
  forecast_df <- data.frame(Date = as.yearqtr(forecast_dates), Series = columns[i], Forecast = forecast_values)
  
  # Filter forecast data starting from Q4 2021
  filtered_forecast_df <- forecast_df %>%
    filter(Date >= as.yearqtr("2021 Q4"))
  
  # Append the filtered forecast data to the combined data frame
  all_forecasts_df <- rbind(all_forecasts_df, filtered_forecast_df)
}

# Save the combined forecasts to a CSV file
write.csv(all_forecasts_df, "Forecasts_Q4.csv", row.names = FALSE)

# Optionally, display the saved data frame
head(all_forecasts_df)
