# Install and load the necessary package
install.packages("tseries")  # Uncomment if not already installed
library(tseries)

# Load your data
data <- read.csv("D:/Inflation_FINAL.csv")


# Replace missing values (NA) with 0
data[is.na(data)] <- 0

# Specify the column names for which you want to perform the ADF test
columns <- c("D1", "E1", "F1", "G1", "H1", "I1", "J1", "K1")

# Load necessary libraries
library(tseries)

# Load the data
data <- read.csv("D:/Inflation_FINAL.csv")

# Specify the column names that you want to convert to time series
columns <- c("D1", "E1", "F1", "G1", "H1", "I1", "J1", "K1")

# Initialize an empty data frame to store the results
pp_results <- data.frame(
  Series = character(),
  Test_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each column and perform the PP test
for (col in columns) {
  # Convert the selected column to a time series object
  ts_data <- ts(data[[col]], frequency = 4, start = c(1995, 1))  # Adjust the start date as needed
  
  # Perform the PP test
  pp_test_result <- pp.test(ts_data)
  
  # Extract the test statistic and p-value
  test_statistic <- pp_test_result$statistic
  p_value <- pp_test_result$p.value
  
  # Append the results to the data frame
  pp_results <- rbind(pp_results, data.frame(
    Series = col,
    Test_Statistic = test_statistic,
    P_Value = p_value
  ))
}

# Print the organized results
print(pp_results)






pp_results <- list()
pp_test <- ur.pp(ts_data, type = "Z-alpha", model = "trend", lags = "long")



# Initialize an empty data frame to store the results
pp_results <- data.frame(
  Series = character(),
  Test_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
  
)

# Loop through each column and perform the ADF test
for (col in columns) {
  # Convert the selected column to a time series object
  ts_data <- ts(data[[col]], frequency = 4, start = c(1995, 5))
  
  # Perform the ADF test
  adf_test_result <- adf.test(ts_data, alternative = "stationary")
  
  # Extract the test statistic and p-value
  test_statistic <- adf_test_result$statistic
  p_value <- adf_test_result$p.value
  
  # Append the results to the data frame
  adf_results <- rbind(adf_results, data.frame(
    Series = col,
    Test_Statistic = test_statistic,
    P_Value = p_value
    
  ))
}

# Print the organized results
print(adf_results)


# Specify the column names
columns <- c("D1", "E1", "F1", "G1", "H1", "I1", "J1", "K1")

# Convert columns to time series objects and calculate densities
density_plots <- lapply(columns, function(col) {
  # Convert the column to a time series object
  ts_data <- ts(data[[col]], frequency = 4, start = c(1995, 1))
  
  # Calculate density
  density_data <- density(ts_data, na.rm = TRUE)
  
  # Convert density object to data frame for ggplot2
  density_df <- data.frame(
    x = density_data$x,
    y = density_data$y,
    series = col
  )
  
  # Create a ggplot object
  ggplot(density_df, aes(x = x, y = y, color = series)) +
    geom_line(size = 1) +
    labs(title = paste("Density of", col), x = "Value", y = "Density") +
    theme_minimal() +
    theme(legend.position = "none")
})

# Combine all individual plots into one cascading plot
library(gridExtra)

# Arrange plots in a single column
combined_plot <- do.call(grid.arrange, c(density_plots, list(ncol = 1)))

# Display the combined plot
print(combined_plot)


# Convert columns to time series objects and compute densities
density_list <- lapply(columns, function(col) {
  # Convert the column to a time series object
  ts_data <- ts(data[[col]], frequency = 4, start = c(1995, 1))
  
  # Calculate density
  density_data <- density(ts_data, na.rm = TRUE)
  
  # Return density data
  list(x = density_data$x, y = density_data$y, label = col)
})

# Set up the plotting area with multiple plots
par(mfrow = c(ceil(length(columns) / 2), 2), mar = c(4, 4, 2, 1) + 0.1)

# Plot each density in the layout
for (density_data in density_list) {
  plot(density_data$x, density_data$y, type = 'l', main = paste("Density of", density_data$label),
       xlab = "Value", ylab = "Density", col = "blue", lwd = 2)
}







