## Chapter 13

# Install and load necessary libraries
install.packages("tidyverse")

library(tidyverse)

# Load the employee performance data
employee_data <- read.csv("data/employee_data.csv")

# View the first few rows of the dataset
head(employee_data)


  # Summarize the employee data
  summary(employee_data)

# Plot performance scores
ggplot(employee_data, aes(x = Department, y = Performance_Score)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Employee Performance Scores by Department", x = "Department", y = "Performance Score") +
  theme_minimal()

  
  # Calculate correlation between performance score and other variables
  cor_matrix <- cor(employee_data %>% select_if(is.numeric))

# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "circle")


  # Fit a linear regression model to predict performance score
  lm_model <- lm(Performance_Score ~ Age + Years_at_Company + Training_Hours, data = employee_data)

# Print the model summary
summary(lm_model)


  # Identify top performers
  top_performers <- employee_data %>%
  arrange(desc(Performance_Score)) %>%
  head(10)

# View the top performers
print(top_performers)

  # Plot top performers
  ggplot(top_performers, aes(x = reorder(paste(Last_Name, First_Name, sep=", "), Performance_Score), y = Performance_Score, fill = Department)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Performers", x = "Employee", y = "Performance Score") +
  coord_flip() +
  theme_minimal()