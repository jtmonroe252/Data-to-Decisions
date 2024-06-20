## Chapter 4: Data Manipulation

# Load necessary libraries
library(tidyverse)

# Load the cleaned dataset
customer_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/cleaned_customer_data.csv") %>%
  select(-X)


# View the first few rows of the dataset
head(customer_data)

# Filter customers who are older than 30
customers_above_30 <- customer_data %>% filter(Age > 30)

# Filter customers with purchase amounts greater than $100
high_value_customers <- customer_data %>% filter(Purchase_Amount > 100)

# View the filtered data
head(customers_above_30)
head(high_value_customers)

# Select only the Name, Age, and Purchase_Amount columns
selected_columns <- customer_data %>% select(Name, Age, Purchase_Amount)

# View the selected columns
head(selected_columns)


# Create a new variable indicating whether the purchase amount is above the average and Years_as_Customer
average_purchase <- mean(customer_data$Purchase_Amount)
customer_data <- customer_data %>%
  mutate(Above_Average_Purchase = ifelse(Purchase_Amount > average_purchase, "Yes", "No")) %>%
  mutate(Customer_Since = as.POSIXlt(as.Date(Registration_Date, "%m/%d/%Y")),
         Current = as.POSIXlt(Sys.Date())) %>%
  mutate(Days_as_Customer = Current -  Customer_Since) %>%
  na.omit()


# View the updated dataset
head(customer_data)


# Summarize the average purchase amount and the number of customers
summary_stats <- customer_data %>%
  summarise(Average_Purchase = mean(Purchase_Amount), Total_Customers = n())

# Group by gender and summarize the average purchase amount and total customers
gender_summary <- customer_data %>%
  group_by(Gender) %>%
  summarise(Average_Purchase = mean(Purchase_Amount), Total_Customers = n())

# View the summary statistics
summary_stats
gender_summary


# Load additional dataset with customer details
customer_details <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/customer_details.csv")

# Perform an inner join on the Customer_ID column
merged_data <- customer_data %>%
  select(Customer_ID, Name, Age, Gender) %>%
  inner_join(customer_details, by = "Customer_ID")

# View the merged dataset
head(merged_data)


# Convert data from wide to long format
long_data <- customer_data %>%
  pivot_longer(cols = starts_with("Purchase"), names_to = "Purchase_Type", values_to = "Amount")

# Convert data from long to wide format
wide_data <- long_data %>%
  pivot_wider(names_from = Purchase_Type, values_from = Amount)

# View the reshaped data
head(long_data)
head(wide_data)

write.csv(customer_data, 'data/cleaned_customer_data.csv')

