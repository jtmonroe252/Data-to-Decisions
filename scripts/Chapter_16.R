##Chapter 16: Optimizing Supply Chain

# Load necessary libraries
install.packages("tidyverse")
install.packages("lpSolve")
install.packages("forecast")

library(tidyverse)
library(lpSolve)
library(forecast)


# Load the sample supply chain data
supply_chain_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/supply_chain_data.csv") %>%
  mutate(Date = as.Date(Date))

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



# Define the costs
costs <- supply_chain_data$Cost

# Define a simple constraint: meet at least a portion of the demand
# In this simple example, we ensure that the sum of the inventory levels is at least the total demand
total_demand <- sum(supply_chain_data$Demand)
constraints <- matrix(1, nrow = 1, ncol = length(costs))
rhs <- total_demand * 0.5  # Example: meet at least 50% of the total demand
direction <- ">="


# Solve the linear programming problem
lp_solution <- lp("min", costs, constraints, direction, rhs)

# Check if the solution is optimal
if (lp_solution$status == 0) {
  # Extract the optimized inventory levels
  optimized_inventory <- lp_solution$solution
  
  # Add the optimized inventory levels to the data
  supply_chain_data$Optimized_Inventory <- optimized_inventory
  
  # Display the first few rows with the optimized inventory
  head(supply_chain_data)
} else {
  print("The linear programming problem did not find an optimal solution.")
}

# Print the optimized inventory levels
print(supply_chain_data$Optimized_Inventory)


# Add more constraints: ensure that each period's inventory level meets its demand
constraints <- diag(length(costs))
rhs <- supply_chain_data$Demand
direction <- rep(">=", length(costs))

# Solve the linear programming problem with the new constraints
lp_solution <- lp("min", costs, constraints, direction, rhs)

# Check if the solution is optimal
if (lp_solution$status == 0) {
  # Extract the optimized inventory levels
  optimized_inventory <- lp_solution$solution
  
  # Add the optimized inventory levels to the data
  supply_chain_data$Optimized_Inventory <- optimized_inventory
  
  # Display the first few rows with the optimized inventory
  head(supply_chain_data)
} else {
  print("The linear programming problem did not find an optimal solution.")
}

# Print the optimized inventory levels
print(supply_chain_data$Optimized_Inventory)


# View the updated data
head(supply_chain_data)

# Load the process data
process_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/process_data.csv")

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

