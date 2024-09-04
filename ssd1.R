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
so_stodom(na.exclude(time_series_list$D1),na.exclude(time_series_list$H1))



# Generate all combinations of pairs
combinations <- combn(length(time_series_list), 2)

# Convert to a more readable format with column names
combinations_df <- data.frame(t(combinations))

# Add column names
colnames(combinations_df) <- c("Series1", "Series2")

# Replace the indices with the actual names of the time series
combinations_df$Series1 <- columns[combinations_df$Series1]
combinations_df$Series2 <- columns[combinations_df$Series2]

so_stodom(na.exclude(time_series_list[combinations_df$Series1]),na.exclude(time_series_list[combinations_df$Series2]))
for(i in seq_along(combinations_df$Series2)){
  print(time_series_list[combinations_df$Series2][[i]])
}

for(i in seq_along(combinations_df$Series1)){
  so_stodom(time_series_list[combinations_df$Series1][[i]],time_series_list[combinations_df$Series2][[i]]))
}

# Initialize an empty data frame to store the results
results_sdf <- data.frame(
  Series1 = character(),
  Series2 = character(),
  Result = I(list()),  # Initialize as a list column to hold the results
  stringsAsFactors = FALSE
)

# Loop through each combination and calculate the so_stodom value
for(i in seq_len(nrow(combinations_df))){
  # Get the names of the series
  series1_name <- combinations_df$Series1[i]
  series2_name <- combinations_df$Series2[i]
  
  # Retrieve the corresponding time series from the list
  ts1 <- na.exclude(time_series_list[[series1_name]])
  ts2 <- na.exclude(time_series_list[[series2_name]])
  
  # Calculate the result using the so_stodom function
  result <- so_stodom(ts1, ts2)
  
  # Append the results to the data frame
  results_sdf <- rbind(
    results_sdf, 
    data.frame(Series1 = series1_name, Series2 = series2_name, Result = I(list(result)), stringsAsFactors = FALSE)
  )
}

# Print the final results data frame
print(results_sdf)

# Print the results in tabular form
print(results_sdf)
d<-so_stodom(na.exclude(time_series_list[[1]]),na.exclude(time_series_list[[2]]))

