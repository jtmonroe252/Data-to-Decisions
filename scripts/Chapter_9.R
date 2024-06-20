##Chapter 9: Financial Calculations and Analysis

# Load necessary libraries
library(quantmod)
library(dplyr)

# Get financial data for GOOG
getSymbols("GOOG", src = "yahoo", from = "2010-01-01", to = "2023-01-01")
goog_data <- as.data.frame(GOOG) %>%
  rownames_to_column(var="Date") %>%
  mutate(Date = as.Date(Date))

# Extract the necessary columns for analysis
goog_prices <- goog_data %>% select(Date, GOOG.Adjusted)

# Calculate P/E Ratio
# Assuming 'earnings_per_share' is a known constant for simplicity
earnings_per_share <- 5.0  # Placeholder value
pe_ratio <- goog_prices$GOOG.Adjusted / earnings_per_share

# Add P/E ratio to the data frame
goog_prices <- goog_prices %>% mutate(PE_Ratio = GOOG.Adjusted / earnings_per_share)

# Plot P/E Ratio
plot(goog_prices$Date, goog_prices$PE_Ratio, type = "l", col = "blue", 
     main = "GOOG P/E Ratio Over Time", xlab = "Date", ylab = "P/E Ratio")


# Calculate Dividend Yield
# Assuming 'dividend_per_share' is a known constant for simplicity
dividend_per_share <- 2.0  # Placeholder value
dividend_yield <- dividend_per_share / goog_prices$GOOG.Adjusted

# Add Dividend Yield to the data frame
goog_prices <- goog_prices %>% mutate(Dividend_Yield = dividend_per_share / GOOG.Adjusted)

# Plot Dividend Yield
plot(goog_prices$Date, goog_prices$Dividend_Yield, type = "l", col = "green", 
     main = "GOOG Dividend Yield Over Time", xlab = "Date", ylab = "Dividend Yield")

# WACC Calculation
# Assumptions for simplicity
cost_of_equity <- 0.08  # 8% cost of equity
cost_of_debt <- 0.04    # 4% cost of debt
tax_rate <- 0.21        # 21% tax rate
equity_market_value <- 1000000000  # Placeholder value
debt_market_value <- 500000000     # Placeholder value
total_market_value <- equity_market_value + debt_market_value

# WACC formula
wacc <- (equity_market_value / total_market_value) * cost_of_equity + 
  (debt_market_value / total_market_value) * cost_of_debt * (1 - tax_rate)

print(paste("WACC:", round(wacc, 4)))


# DCF Calculation
# Assumptions for simplicity
free_cash_flow <- 20000000  # Placeholder value for the first year
growth_rate <- 0.03         # 3% growth rate
years <- 5                  # Forecasting for 5 years

# Calculate present value of cash flows
dcf <- sum(sapply(1:years, function(t) {
  free_cash_flow * (1 + growth_rate)^(t-1) / (1 + wacc)^t
}))

print(paste("DCF Value:", round(dcf, 2)))


