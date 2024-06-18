## Chapter 9

# Install required packages if not already installed
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")

# Load the libraries
library(tidyverse)
library(cluster)
library(factoextra)

# Load the customer data
customer_data <- read.csv("data/cleaned_customer_data.csv") %>%
  select(Customer_ID, Normalized_Age, Normalized_Purchase_Amount) %>%
  mutate(Customer_ID = as.numeric(Customer_ID)) %>%
  as.data.frame()

# View the first few rows of the dataset
head(customer_data)


  # Check for missing values
  sum(is.na(customer_data))

# Remove rows with missing values
customer_data <- na.omit(customer_data)

# Scale the data
scaled_data <- scale(customer_data[, -1])  # Assuming the first column is customer ID


  # Calculate the total within-cluster sum of square (WSS) for different numbers of clusters
  wss <- function(k) {
    kmeans(scaled_data, k, nstart = 10)$tot.withinss
  }

# Plot the Elbow method
k.values <- 1:10
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")


  # Apply k-means with the optimal number of clusters (e.g., k = 3)
  set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

# Add cluster assignment to the original data
customer_data$Cluster <- kmeans_result$cluster

# View the data with cluster assignments
head(customer_data)


  # Visualize the clusters using factoextra
  fviz_cluster(kmeans_result, data = scaled_data,
               palette = "jco",
               geom = "point",
               ellipse.type = "convex",
               ggtheme = theme_minimal())


  # Calculate summary statistics for each cluster
  customer_data %>%
  group_by(Cluster) %>%
  summarise(across(everything(), list(mean = mean, sd = sd)))


  # Profiling based on cluster means
  cluster_profiles <- customer_data %>%
  group_by(Cluster) %>%
  summarise_all(list(mean = mean))

print(cluster_profiles)


  # Visualize the profiles of each cluster
  customer_data_long <- customer_data %>%
  pivot_longer(cols = -Cluster, names_to = "Variable", values_to = "Value")

ggplot(customer_data_long, aes(x = Variable, y = Value, fill = factor(Cluster))) +
  geom_boxplot() +
  facet_wrap(~Cluster) +
  theme_minimal() +
  labs(title = "Customer Segment Profiles", x = "Variable", y = "Value", fill = "Cluster")