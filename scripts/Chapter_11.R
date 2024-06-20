## Chapter 11: A/B Testing and Campaign Analysis

# Install and load necessary libraries
install.packages("tidyverse")
install.packages("broom")

library(tidyverse)
library(broom)

# Load the A/B test dataset
ab_test_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/ab_test_data.csv")

# View the first few rows of the dataset
head(ab_test_data)


  # Summarize the data
  summary(ab_test_data)

# Calculate conversion rates for each group
ab_test_data %>%
  group_by(Group) %>%
  summarise(Conversion_Rate = mean(Converted) * 100) -> ad_test_data


  # Plot conversion rates
  ggplot(ab_test_data, aes(x = Group, y = Converted)) +
  geom_bar(stat = "summary", fun="mean", fill = "skyblue") +
  labs(title = "Conversion Rates by Group", x = "Group", y = "Conversion Rate") +
  theme_minimal()


  # Perform a t-test to compare conversion rates
  t_test_result <- t.test(Converted ~ Group, data = ab_test_data)

# View the t-test results
tidy(t_test_result) %>%
pivot_longer(
  cols = c(estimate, estimate1, estimate2, statistic, p.value, parameter, conf.low, conf.high),
  names_to = "metric",
  values_to = "value"
)


# Summarize the data by group
ab_test_data %>%
  group_by(Group) %>%
  summarise_at(vars(Metric1, Metric2, Metric3), list(mean = mean, sd = sd))


  # Plot multiple metrics by group
  ab_test_metrics_long <- ab_test_data %>%
  pivot_longer(cols = Metric1:Metric3, names_to = "Metric", values_to = "Value")

ggplot(ab_test_metrics_long, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  facet_wrap(~ Metric, scales = "free") +
  labs(title = "A/B Test Metrics by Group", x = "Group", y = "Value") +
  theme_minimal()


  # Perform t-tests for multiple metrics
  t_test_results <- ab_test_metrics_long %>%
  group_by(Metric) %>%
  summarise(t_test = list(tidy(t.test(Value ~ Group))))

# View the t-test results
t_test_results %>%
  unnest(t_test)