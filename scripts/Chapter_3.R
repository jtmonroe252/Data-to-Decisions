## Chapter 3

# Load necessary libraries
install.packages("tidyverse")
library(tidyverse)

# Load the dataset
customer_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/customer_data.csv")


# View the first few rows of the dataset
head(customer_data)


# Check for missing values
summary(customer_data)

# Visualize missing data
install.packages("naniar")
library(naniar)
vis_miss(customer_data)


# Remove rows with any missing values
cleaned_data <- na.omit(customer_data)

# Verify the changes
summary(cleaned_data)


# Impute missing values with the mean for numeric columns
customer_data$Age[is.na(customer_data$Age)] <- mean(customer_data$Age, na.rm = TRUE)
customer_data$Purchase_Amount[is.na(customer_data$Purchase_Amount)] <- mean(customer_data$Purchase_Amount, na.rm = TRUE)

# Verify the changes
summary(customer_data)

# Check for duplicate rows
duplicates <- customer_data[duplicated(customer_data),]

# Remove duplicate rows
customer_data <- customer_data[!duplicated(customer_data),]

# Verify the changes
summary(customer_data)


# Convert all names to title case
customer_data$Name <- str_to_title(customer_data$Name)

# Correct typos or inconsistencies in categorical data
customer_data$Gender <- ifelse(customer_data$Gender == "M", "Male", 
                               ifelse(customer_data$Gender == "F", "Female", customer_data$Gender))

# Verify the changes
head(customer_data)


# Convert dates to a Date format
customer_data$Registration_Date <- as.Date(customer_data$Registration_Date, format="%m/%d/%Y")


# Normalize numeric data
customer_data$Normalized_Age <- scale(customer_data$Age)
customer_data$Normalized_Purchase_Amount <- scale(customer_data$Purchase_Amount)

# Verify the changes
head(customer_data)


# Check for outliers in numerical data
boxplot(customer_data$Purchase_Amount, main="Purchase Amount")

# Remove outliers if necessary
Q1 <- quantile(customer_data$Purchase_Amount, 0.25)
Q3 <- quantile(customer_data$Purchase_Amount, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

customer_data <- customer_data %>%
  filter(Purchase_Amount >= lower_bound & Purchase_Amount <= upper_bound)

# Verify the changes
boxplot(customer_data$Purchase_Amount, main="Purchase Amount after Removing Outliers")


write.csv(customer_data, 'data/cleaned_customer_data.csv')

