## Chapter 5: Advanced Statistical Techniques

# Load necessary libraries
library(tidyverse)
install.packages("ggfortify")
library(ggfortify)

# Load the cleaned dataset
customer_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/cleaned_customer_data.csv") %>%
  select(-X)

# View the first few rows of the dataset
head(customer_data)

# Conduct a t-test to compare purchase amounts between genders
t_test_result <- t.test(Purchase_Amount ~ Gender, data = customer_data)

# View the t-test result
t_test_result


# Conduct a chi-square test to examine the association between Gender and Above_Average_Purchase
table_data <- table(customer_data$Gender, customer_data$Above_Average_Purchase)
chi_square_result <- chisq.test(table_data)

# View the chi-square test result
chi_square_result


# Fit a linear regression model to predict Purchase_Amount based on Age and Gender
linear_model <- lm(Purchase_Amount ~ Age + Gender, data = customer_data)

# View the model summary
summary(linear_model)


# Fit a multiple regression model to predict Purchase_Amount, calculating Days_as_Customer
customer_data %>%
  mutate(Registration_Date =  as.Date(Registration_Date)) %>%
  mutate(Days_as_Customer = as.numeric(as.Date("2024-06-11") - Registration_Date)) %>%
  na.omit() -> customer_data
  
multiple_model <- lm(Purchase_Amount ~ Age + Gender + Days_as_Customer, data = customer_data)

# View the model summary
summary(multiple_model)


# Scale the data
scaled_data <- scale(customer_data[, c("Age", "Purchase_Amount", "Days_as_Customer")])

# Determine the optimal number of clusters using the Elbow method
set.seed(123)
wss <- function(k) {
  kmeans(scaled_data, k, nstart = 10)$tot.withinss
}
k.values <- 1:10
wss_values <- map_dbl(k.values, wss)

# Plot the Elbow method
plot(k.values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

# Apply k-means clustering with the optimal number of clusters (e.g., k = 3)
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

# Add cluster assignment to the original data
customer_data$Cluster <- kmeans_result$cluster

# View the clustered data
head(customer_data)


# Perform PCA on the scaled data
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# View the summary of the PCA result
summary(pca_result)

# Plot the PCA
autoplot(pca_result, data = customer_data, colour = 'Cluster', label = TRUE, label.size = 3)


# Fit a logistic regression model to predict Above_Average_Purchase
customer_data %>%
  mutate(Above_Average_Purchase = as.factor(Above_Average_Purchase)) -> customer_data

logistic_model <- glm(Above_Average_Purchase ~ Age + Gender, data = customer_data, family = "binomial")

# View the model summary
summary(logistic_model)
