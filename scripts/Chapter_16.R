## Chapter 16

# Install and load necessary libraries
install.packages("tidyverse")

library(tidyverse)

# Load the process data
process_data <- read.csv("data/process_data.csv")

# View the first few rows of the dataset
head(process_data)


# Summarize the process data
summary(process_data)

# Plot process cycle time by step
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

# Create a detailed improvement plan
improvement_plan <- data.frame(
  Process_Step = c("Step A", "Step B", "Step C"),
  Current_Cycle_Time = c(10, 15, 20),
  Improved_Cycle_Time = c(7, 10, 12),
  Improvement_Method = c("Automation", "Streamlining", "Training")
)

print(improvement_plan)

# Monitor the progress of the improvement plan
progress_data <- data.frame(
  Date = seq(as.Date("2023-01-01"), by = "month", length.out = 12),
  Cycle_Time_A = c(10, 9, 8, 7, 7, 7, 6, 6, 6, 6, 6, 6),
  Cycle_Time_B = c(15, 14, 13, 12, 12, 12, 11, 11, 11, 10, 10, 10),
  Cycle_Time_C = c(20, 19, 18, 17, 16, 15, 14, 13, 12, 12, 12, 12)
)

# Plot the progress over time
progress_data_long <- progress_data %>%
  pivot_longer(cols = starts_with("Cycle_Time"), names_to = "Process_Step", values_to = "Cycle_Time")

ggplot(progress_data_long, aes(x = Date, y = Cycle_Time, color = Process_Step)) +
  geom_line() +
  labs(title = "Cycle Time Improvement Over Time", x = "Date", y = "Cycle Time") +
  theme_minimal()


# Define key metrics for monitoring progress
metrics <- data.frame(
  Metric = c("Cycle Time", "Error Rate", "Throughput"),
  Definition = c("Time to complete each process step", "Number of errors per step", "Units produced per time period")
)

print(metrics)


# Establish baseline metrics before improvements
baseline_metrics <- data.frame(
  Metric = c("Cycle Time", "Error Rate", "Throughput"),
  Baseline = c(10, 5, 100)
)

print(baseline_metrics)

# Simulate data collection for monitoring progress
monitoring_data <- data.frame(
  Date = seq(as.Date("2023-01-01"), by = "month", length.out = 12),
  Cycle_Time = c(10, 9, 8, 7, 7, 7, 6, 6, 6, 6, 6, 6),
  Error_Rate = c(5, 4.5, 4, 3.5, 3.5, 3, 2.5, 2.5, 2, 2, 2, 2),
  Throughput = c(100, 105, 110, 115, 115, 120, 125, 130, 135, 140, 145, 150)
)

# View the monitoring data
head(monitoring_data)

# Plot the progress of key metrics over time
monitoring_data_long <- monitoring_data %>%
  pivot_longer(cols = -Date, names_to = "Metric", values_to = "Value")

ggplot(monitoring_data_long, aes(x = Date, y = Value, color = Metric)) +
  geom_line() +
  labs(title = "Process Improvement Monitoring", x = "Date", y = "Value") +
  theme_minimal()