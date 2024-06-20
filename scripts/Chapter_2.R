##Chapter 2

# Incorrect
print("Hello, World!"
      
# Correct
print("Hello, World!")

# Incorrect
print(x)

# Correct
x <- 10
print(x)


# Incorrect
x <- "10"
sum(x)

# Correct
x <- as.numeric("10")
sum(x)


# Incorrect
library(ggplot2)
ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()

# Correct
install.packages("ggplot2")
library(ggplot2)
ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()


# Incorrect
plot(mtcars$mpg, main = "MPG vs. Weight")

# Correct
plot(mtcars$mpg, mtcars$wt, main = "MPG vs. Weight")



# Incorrect
x <- c(1, 2, 3)
x[4]

# Correct
x <- c(1, 2, 3)
if (length(x) >= 4) {
  print(x[4])
} else {
  print("Index out of bounds")
}


# Clean up memory
rm(list = ls())
gc()






