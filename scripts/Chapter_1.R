##Chapter 1

# Basic arithmetic operations
2 + 2
5 - 3
4 * 3
8 / 2

# Assigning values to variables
x <- 10
y <- 5

# Basic operations with variables
sum <- x + y
difference <- x - y
product <- x * y
quotient <- x / y

# Print values
print(sum)
print(difference)
print(product)
print(quotient)

# Create a numeric vector
numbers <- c(1, 2, 3, 4, 5)
print(numbers)

# Perform operations on vectors
sum_numbers <- sum(numbers)
mean_numbers <- mean(numbers)
print(sum_numbers)
print(mean_numbers)

data <- data.frame(
  Name = c("John", "Jane", "Alex", "Emily"),
  Age = c(28, 24, 30, 22),
  Department = c("Finance", "Marketing", "Sales", "Operations")
)
print(data)

# Access data frame columns
print(data$Name)
print(data$Age)

# Summary statistics
summary(data)

# Install the dplyr package
install.packages("dplyr")

# Load the dplyr package
library(dplyr)

# Use dplyr to manipulate data
data_filtered <- filter(data, Age > 25)
print(data_filtered)







