## Chapter 15

# Install and load necessary libraries
install.packages("tidyverse")
install.packages("lpSolve")
install.packages("forecast")

library(tidyverse)
library(lpSolve)
library(forecast)

# Load the supply chain data
supply_chain_data <- read.csv("data/supply_chain_data.csv") %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))

# View the first few rows of the dataset
head(supply_chain_data)

# Summarize the supply chain data
summary(supply_chain_data)

# Plot inventory levels over time
ggplot(supply_chain_data, aes(x = Date, y = Inventory_Level)) +
  geom_line(color = "blue") +
  labs(title = "Inventory Levels Over Time", x = "Date", y = "Inventory Level") +
  theme_minimal()

# Plot historical demand data
ggplot(supply_chain_data, aes(x = Date, y = Demand)) +
  geom_line(color = "green") +
  labs(title = "Historical Demand Data", x = "Date", y = "Demand") +
  theme_minimal()

# Fit a time series model for demand forecasting
demand_ts <- ts(supply_chain_data$Demand, start = c(2020, 1), frequency = 12)
demand_forecast <- forecast::auto.arima(demand_ts)

# Plot the demand forecast
plot(forecast(demand_forecast, h = 12), main = "Demand Forecast for Next 12 Months", ylab = "Demand", xlab = "Time")

# Define the costs and constraints for the optimization problem
costs <- supply_chain_data$Cost
constraints <- matrix(c(1, 1, 1, 1, 1, 1), nrow = 1)
rhs <- 1000  # Example constraint, such as a budget limit
direction <- "<="

# Solve the linear programming problem
lp_solution <- lp("min", costs, constraints, direction, rhs)

# View the solution
lp_solution


# Extract the optimized inventory levels
optimized_inventory <- lp_solution$solution

# Add the optimized inventory levels to the data
supply_chain_data$Optimized_Inventory <- optimized_inventory



# Load the process data
process_data <- read.csv("data/process_data.csv")

# View the first few rows of the dataset
head(process_data)


# Summarize the process data
summary(process_data)

# Plot process cycle time
ggplot(process_data, aes(x = Process_Step, y = Cycle_Time)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Process Cycle Time by Step", x = "Process Step", y = "Cycle Time") +
  theme_minimal()

# Identify the process steps with the highest cycle time
bottlenecks <- process_data %>%
  arrange(desc(Cycle_Time)) %>%
  head(3)

print(bottlenecks)

# Suggest improvements for the identified bottlenecks
improvements <- data.frame(
  Process_Step = bottlenecks$Process_Step,
  Suggested_Improvement = c("Automate step", "Reduce handoffs", "Improve training")
)

print(improvements)








# View the updated data
head(supply_chain_data)

