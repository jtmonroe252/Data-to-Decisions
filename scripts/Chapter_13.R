## Chapter 13: Performance Metrics and Customer Value

# Install and load necessary libraries
install.packages("tidyverse")

library(tidyverse)

# Load the performance data
performance_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/performance_data.csv")

# View the first few rows of the dataset
head(performance_data)


  # Summarize the performance data
  summary(performance_data)

# Plot the total sales by salesperson
  ggplot(performance_data, aes(x = Salesperson, y = Total_Sales)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Total Sales by Salesperson", x = "Salesperson", y = "Total Sales") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  

  # Calculate the sales conversion rate
  performance_data %>%
  summarise(Conversion_Rate = mean(Deals_Closed / Leads_Generated) * 100)


  # Calculate the average deal size
  performance_data %>%
  summarise(Average_Deal_Size = mean(Total_Sales / Deals_Closed))


  # Calculate the quota attainment percentage
  performance_data %>%
  summarise(Quota_Attainment = mean(Total_Sales / Sales_Quota) * 100)


# Load the customer data
customer_data <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/customer_data.csv")

# View the first few rows of the dataset
head(customer_data)


  # Summarize the customer data
  summary(customer_data)
  
  # Plot the total revenue by customer
  customer_data %>%
    mutate(Count = 1) %>%
    group_by(Customer_ID) %>%
    summarise(Total_Transactions = sum(Count, na.rm=TRUE), Purchase_Amount = sum(Purchase_Amount, na.rm=TRUE)) -> customer_data
    

ggplot(customer_data, aes(x = Customer_ID, y = Purchase_Amount)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Total Revenue by Customer", x = "Customer ID", y = "Total Revenue") +
  theme_minimal()


# Calculate the average purchase value & the average purchase frequency rate
  
customer_details <- read.csv("https://raw.githubusercontent.com/jtmonroe252/Data-to-Decisions/main/data/customer_details.csv") %>%
  mutate(count = 1) %>%
  group_by(Customer_ID) %>%
  summarise(Purchase_Amount = mean(Amount), Total_Transactions = sum(count), .groups = 'drop') %>%
  mutate(Average_Purchase_Value = Purchase_Amount / Total_Transactions,
         Average_Frequency_Rate = mean(Total_Transactions)) %>%
  mutate(Customer_Value = Average_Purchase_Value * Average_Frequency_Rate)

# View the CLV for each customer
head(customer_details %>% select(Customer_ID, Customer_Value))

