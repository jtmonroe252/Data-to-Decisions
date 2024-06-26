## Chapter 19: Final Project

# Load necessary libraries
install.packages("tidyverse")
install.packages("forecast")

library(tidyverse)
library(forecast)


# Load the sales data
sales_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/sales_data.csv")

# Convert sales data to a time series object
sales_ts <- ts(sales_data$Sales, start = c(2020, 1), frequency = 12)

# Fit an ARIMA model
arima_model <- auto.arima(sales_ts)

# Forecast future sales
arima_forecast <- forecast(arima_model, h = 3)

# Prepare forecast data for export
forecast_data <- data.frame(
  Date = seq.Date(from = as.Date("2023-01-01"), by = "month", length.out = 3),
  Forecast = as.numeric(arima_forecast$mean)
)

# Save forecast data to a CSV file
write.csv(forecast_data, "data/forecast_data.csv", row.names = FALSE)
