# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data
data <- read.csv("D:/Inflation_FINAL.csv")

# Specify the column names that you want to convert to time series
columns <- c("D1", "E1", "F1", "G1", "H1", "I1", "J1", "K1")

# Initialize an empty list to store the time series objects
time_series_list <- list()

# Loop through each column and create a time series object
for (col in columns) {
  # Convert the data frame to a time series object
  ts_object <- ts(data[[col]], frequency = 4, start = c(1995, 5))
  
  # Store the time series object in the list with the column name as the key
  time_series_list[[col]] <- ts_object
}

# Convert the list of time series to a data frame
df <- data.frame(time = time(time_series_list[[1]]), series = time_series_list[[1]])

for(i in 2:length(time_series_list)) {
  temp_df <- data.frame(time = time(time_series_list[[i]]), series = time_series_list[[i]], id = i)
  df <- bind_rows(df, temp_df)
}

# Use the predefined column names (D1, E1, ..., K1) as labels
df <- df %>%
  mutate(series_id = rep(columns, each = length(time_series_list[[1]])))

# Plot using ggplot
ggplot(df, aes(x = time, y = series, color = series_id)) +
  geom_line() +
  labs(title = "Time Series Plot",
       x = "Time(in years)",
       y = "Forecasted Value",
       color = "Forecaster") +
  theme_minimal() +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5, face = "bold")  # Center and bold the title
  )

