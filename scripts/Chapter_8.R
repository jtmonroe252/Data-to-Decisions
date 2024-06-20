## Chapter 8: Time Series Analysis


# Load necessary libraries
library(quantmod)
library(forecast)
library(ggplot2)

# Get historical stock data for GOOG
getSymbols("GOOG", src = "yahoo", from = "2010-01-01", to = "2023-01-01")
goog_prices <- Cl(GOOG)

# Plot the data
autoplot(goog_prices, main = "Google Stock Prices") + 
  ylab("Price") + xlab("Time")


# Perform ADF test
adf_test <- adf.test(goog_prices)
print(adf_test)

# If the series is not stationary, difference the data
goog_diff <- diff(goog_prices, differences = 1) %>%
  na.omit()
adf_test_diff <- adf.test(goog_diff)
print(adf_test_diff)

goog_arima <- auto.arima(goog_prices)
summary(goog_arima)


# Forecast using ARIMA
goog_forecast_arima <- forecast(goog_arima, h = 30)
autoplot(goog_forecast_arima, main = "Google Stock Price Forecast using ARIMA")


goog_ets <- ets(goog_prices)
summary(goog_ets)

# Forecast using Exponential Smoothing
goog_forecast_ets <- forecast(goog_ets, h = 30)
autoplot(goog_forecast_ets, main = "Google Stock Price Forecast using ETS")


# Perform the Augmented Dickey-Fuller test
adf_test <- adf.test(goog_prices)
print(adf_test)


# Difference the series to make it stationary
goog_diff <- diff(goog_prices, differences = 1)

# Plot the differenced series
plot(goog_diff, main = "Differenced GOOG Closing Prices")

# Fit the ARIMA model
goog_arima <- auto.arima(goog_diff)

# Print the model summary
summary(goog_arima)

# Forecast the next 30 days
goog_forecast <- forecast(goog_arima, h = 30)

# Plot the forecast
plot(goog_forecast, main = "GOOG 30-Day Forecast")
