## Chapter 12: Sales Forecasting and Pipeline Analysis

# Install and load necessary libraries
install.packages("tidyverse")
install.packages("forecast")

library(tidyverse)
library(forecast)

# Load the sales data
sales_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/sales_data.csv") %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"),
         Sales = as.numeric(Sales))

# View the first few rows of the dataset
head(sales_data)


  # Summarize the sales data
  summary(sales_data)

# Plot the historical sales data
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


  # Fit an ARIMA model to the time series data
  sales_arima <- auto.arima(sales_ts)

# Print the model summary
summary(sales_arima)

# Forecast future sales
sales_forecast <- forecast(sales_arima, h = 12)

# Plot the forecast
plot(sales_forecast, main = "Sales Forecast for Next 12 Months", ylab = "Sales", xlab = "Time")


# Load the sales pipeline data
pipeline_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/pipeline_data.csv")

# View the first few rows of the dataset
head(pipeline_data)


  # Summarize the pipeline data
  summary(pipeline_data)

# Plot the sales pipeline stages
ggplot(pipeline_data, aes(x = Stage, y = Amount)) +
  geom_bar(stat = "summary", fun = "sum", fill = "skyblue") +
  labs(title = "Sales Pipeline Stages", x = "Stage", y = "Total Amount") +
  theme_minimal()


# Calculate conversion rates between stages
# Order the stages
stage_order <- c("Prospecting", "Qualification", "Needs Analysis", "Proposal", "Negotiation", "Closed Won", "Closed Lost")

# Summarize the total amount per stage and ensure correct order
pipeline_summary <- pipeline_data %>%
  group_by(Stage) %>%
  summarise(Total_Amount = sum(Amount)) %>%
  mutate(Stage = factor(Stage, levels = stage_order)) %>%
  arrange(Stage)

# Calculate conversion rates between stages
pipeline_summary <- pipeline_summary %>%
  mutate(Previous_Amount = lag(Total_Amount),
         Conversion_Rate = Total_Amount / lag(Total_Amount)) %>%
  replace_na(list(Conversion_Rate = 0))

# Display the summary with conversion rates
pipeline_summary


  # Calculate the length of the sales cycle for each deal
  pipeline_data %>%
  mutate(Close_Date = as.Date(Close_Date, "%m/%d/%Y" ),
         Open_Date = as.Date(Open_Date, "%m/%d/%Y" )) %>%  
  mutate(Cycle_Length = Close_Date - Open_Date) %>%
  summarise(Average_Cycle_Length = mean(Cycle_Length, na.rm = TRUE))
  
  
  