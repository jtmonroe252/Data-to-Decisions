## Chapter 6

# Load necessary libraries
library(tidyverse)

# Load the cleaned dataset
customer_data <- read.csv("data/cleaned_customer_data.csv")

# View the first few rows of the dataset
head(customer_data)

# Create a scatter plot of Age vs. Purchase_Amount
ggplot(customer_data, aes(x = Age, y = Purchase_Amount)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Purchase Amount", x = "Age", y = "Purchase Amount")


# Create a bar chart of the number of customers by Gender
ggplot(customer_data, aes(x = Gender)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Bar Chart of Customers by Gender", x = "Gender", y = "Count")


# Create a histogram of Purchase_Amount
ggplot(customer_data, aes(x = Purchase_Amount)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Purchase Amounts", x = "Purchase Amount", y = "Frequency")

# Create a box plot of Purchase_Amount by Gender
ggplot(customer_data, aes(x = Gender, y = Purchase_Amount)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of Purchase Amount by Gender", x = "Gender", y = "Purchase Amount")


# Enhanced scatter plot with color and size aesthetics
ggplot(customer_data, aes(x = Age, y = Purchase_Amount, color = Gender, size = Purchase_Amount)) +
  geom_point(alpha = 0.7) +
  labs(title = "Enhanced Scatter Plot of Age vs. Purchase Amount", x = "Age", y = "Purchase Amount") +
  theme_minimal()

# Adding themes and customizing axis labels
ggplot(customer_data, aes(x = Gender, y = Purchase_Amount, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Box Plot of Purchase Amount by Gender", x = "Gender", y = "Purchase Amount") +
  theme_classic() +
  scale_fill_brewer(palette = "Pastel1")

# Faceted scatter plot by Gender
ggplot(customer_data, aes(x = Age, y = Purchase_Amount)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Gender) +
  labs(title = "Faceted Scatter Plot of Age vs. Purchase Amount by Gender", x = "Age", y = "Purchase Amount")


# Install and load plotly
install.packages("plotly")
library(plotly)

# Create an interactive scatter plot
p <- ggplot(customer_data, aes(x = Age, y = Purchase_Amount, color = Gender)) +
  geom_point() +
  labs(title = "Interactive Scatter Plot of Age vs. Purchase Amount", x = "Age", y = "Purchase Amount")

ggplotly(p)


# Adding annotations to a scatter plot
ggplot(customer_data, aes(x = Age, y = Purchase_Amount)) +
  geom_point() +
  annotate("text", x = 50, y = 200, label = "High spenders", color = "red") +
  labs(title = "Scatter Plot with Annotations", x = "Age", y = "Purchase Amount")



