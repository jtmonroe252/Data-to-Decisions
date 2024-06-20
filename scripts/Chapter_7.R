## Chapter 7: Financial Data

# Install required packages if not already installed
install.packages("tidyverse")
install.packages("quantmod")

# Load the libraries
library(tidyverse)
library(quantmod)


# Get historical stock data for AAPL, GOOG, and MSFT
getSymbols(c("AAPL", "GOOG", "MSFT"), from = "2020-01-01", to = "2023-01-01")

# View the first few rows of the data
head(AAPL)
head(GOOG)
head(MSFT)


# Calculate daily returns
aapl_returns <- dailyReturn(Cl(AAPL))
goog_returns <- dailyReturn(Cl(GOOG))
msft_returns <- dailyReturn(Cl(MSFT))

# View summary statistics of the returns
summary(aapl_returns)
summary(goog_returns)
summary(msft_returns)


# Create a data frame with the returns
returns_df <- data.frame(
  Date = index(aapl_returns),
  AAPL = coredata(aapl_returns),
  GOOG = coredata(goog_returns),
  MSFT = coredata(msft_returns)
) %>%
  rename("AAPL" = 2, GOOG = 3, MSFT = 4)

# Plot the daily returns
ggplot(returns_df, aes(x = Date)) +
  geom_line(aes(y = AAPL, color = "AAPL")) +
  geom_line(aes(y = GOOG, color = "GOOG")) +
  geom_line(aes(y = MSFT, color = "MSFT")) +
  labs(title = "Daily Returns of AAPL, GOOG, and MSFT", x = "Date", y = "Return")
	


# Calculate 20-day and 50-day moving averages
aapl_sma20 <- SMA(Cl(AAPL), n = 20)
aapl_sma50 <- SMA(Cl(AAPL), n = 50)

# Plot the closing prices and moving averages
chartSeries(AAPL, TA = "addSMA(20);addSMA(50)", theme = chartTheme("white"))



# Calculate 20-day and 50-day exponential moving averages
aapl_ema20 <- EMA(Cl(AAPL), n = 20)
aapl_ema50 <- EMA(Cl(AAPL), n = 50)

# Plot the closing prices and exponential moving averages
chartSeries(AAPL, TA = "addEMA(20);addEMA(50)", theme = chartTheme("white"))


# Calculate the rolling standard deviation (volatility)
aapl_volatility <- runSD(Cl(AAPL), n = 20)

# Plot the volatility
chartSeries(aapl_volatility, theme = chartTheme("white"), name = "AAPL Volatility")


