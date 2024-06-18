## Chapter 14


# Install and load necessary libraries
install.packages("tidyverse")
install.packages("survival")

library(tidyverse)
library(survival)

# Load the employee turnover data
turnover_data <- read.csv("data/turnover_data.csv")

# View the first few rows of the dataset
head(turnover_data)


  # Summarize the turnover data
  summary(turnover_data)

# Plot turnover rates by department
ggplot(turnover_data, aes(x = Department, fill = as.factor(Turnover))) +
  geom_bar(position = "fill") +
  labs(title = "Turnover Rates by Department", x = "Department", y = "Proportion", fill = "Turnover") +
  theme_minimal()


  # Fit a survival model to analyze turnover
  surv_object <- Surv(turnover_data$Tenure, turnover_data$Turnover)
surv_model <- survfit(surv_object ~ Department, data = turnover_data)

# Plot the survival curves
plot(surv_model, col = rainbow(length(unique(turnover_data$Department))), lwd = 2,
     xlab = "Tenure (months)", ylab = "Survival Probability", main = "Employee Survival Analysis by Department")
legend("topright", legend = unique(turnover_data$Department), col = rainbow(length(unique(turnover_data$Department))), lwd = 2)


  # Fit a logistic regression model to predict turnover
  logit_model <- glm(Turnover ~ Age + Department + Years_at_Company + Job_Satisfaction, data = turnover_data, family = "binomial")

# Print the model summary
summary(logit_model)


  # Calculate feature importance for the logistic regression model
  importance <- summary(logit_model)$coefficients

# View the feature importance
print(importance)


  # Plot feature importance
  feature_importance <- data.frame(Feature = rownames(importance), Importance = importance[, "Estimate"])
ggplot(feature_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Feature Importance in Predicting Turnover", x = "Feature", y = "Importance") +
  coord_flip() +
  theme_minimal()
