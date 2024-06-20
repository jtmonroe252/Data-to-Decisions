## Chapter 18: Machine Learning

# Install and load necessary libraries
install.packages("tidyverse")
install.packages("forecast")

library(tidyverse)
library(forecast)

# Load the sales data
sales_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/sales_data.csv") %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))

# View the first few rows of the dataset
head(sales_data)

# Summarize the sales data
summary(sales_data)

# Plot historical sales data
ggplot(sales_data, aes(x = Date, y = Sales)) +
  geom_line(color = "blue") +
  labs(title = "Historical Sales Data", x = "Date", y = "Sales") +
  theme_minimal()

# Convert sales data to a time series object
sales_ts <- ts(sales_data$Sales, start = c(2020, 1), frequency = 12)

# Plot the time series data
plot(sales_ts, main = "Monthly Sales Data", ylab = "Sales", xlab = "Time")

# Decompose the time series into trend, seasonal, and irregular components
sales_decomp <- decompose(sales_ts)

# Plot the decomposed components
plot(sales_decomp)


# Fit a simple exponential smoothing model
ses_model <- ses(sales_ts)

# Forecast future sales
ses_forecast <- forecast(ses_model, h = 3)

# Plot the forecast
plot(ses_forecast, main = "Simple Exponential Smoothing Forecast", ylab = "Sales", xlab = "Time")

# Fit a Holt-Winters exponential smoothing model
hw_model <- HoltWinters(sales_ts)

# Forecast future sales
hw_forecast <- forecast(hw_model, h = 3)

# Plot the forecast
plot(hw_forecast, main = "Holt-Winters Exponential Smoothing Forecast", ylab = "Sales", xlab = "Time")

# Fit an ARIMA model to the time series data
arima_model <- auto.arima(sales_ts)

# Forecast future sales
arima_forecast <- forecast(arima_model, h = 3)

# Plot the forecast
plot(arima_forecast, main = "ARIMA Forecast", ylab = "Sales", xlab = "Time")



# Calculate accuracy metrics for each model
ses_accuracy <- accuracy(ses_forecast)
hw_accuracy <- accuracy(hw_forecast)
arima_accuracy <- accuracy(arima_forecast)

# Combine accuracy metrics into a single data frame
accuracy_metrics <- data.frame(
  Model = c("SES", "Holt-Winters", "ARIMA"),
  MAE = c(ses_accuracy[1, "MAE"], hw_accuracy[1, "MAE"], arima_accuracy[1, "MAE"]),
  RMSE = c(ses_accuracy[1, "RMSE"], hw_accuracy[1, "RMSE"], arima_accuracy[1, "RMSE"])
)

print(accuracy_metrics)

# Plot the accuracy metrics
ggplot(accuracy_metrics, aes(x = Model, y = MAE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Accuracy Comparison (MAE)", x = "Model", y = "Mean Absolute Error") +
  theme_minimal()

ggplot(accuracy_metrics, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Accuracy Comparison (RMSE)", x = "Model", y = "Root Mean Squared Error") +
  theme_minimal()
