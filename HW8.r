# Question 7.3
set.seed(123)  # For reproducibility

# Parameters
a <- 5    # Intercept
b <- 3    # Linear coefficient
c <- 0.2  # Quadratic coefficient
n <- 100  # Number of data points
sigma <- 3  # Standard deviation of the errors

# Generate data
x <- runif(n, 0, 50)  # Uniformly sample x in the range [0, 50]
error <- rnorm(n, mean = 0, sd = sigma)  # Errors from normal distribution
y <- a + b * x + c * x^2 + error  # Nonlinear relationship

# (a) Fit a linear regression model
fit <- lm(y ~ x)  # Linear regression (ignoring the quadratic term)
summary(fit)

# (b) Scatterplot and regression line
plot(x, y, main = "Scatterplot of Data with Best-Fit Linear Regression",
     xlab = "x", ylab = "y", pch = 16, col = "blue")
abline(fit, col = "red", lwd = 2)  # Best-fit linear regression line


# Question 7.5
# Simulation function to estimate mean and standard error
simulate_convergence <- function(n, true_mean = 50, true_sd = 10) {
  set.seed(123)  # For reproducibility
  
  # Generate a random sample of size n from a normal distribution
  sample_data <- rnorm(n, mean = true_mean, sd = true_sd)
  
  # Estimate the mean
  estimated_mean <- mean(sample_data)
  
  # Compute the standard error of the mean
  standard_error <- sd(sample_data) / sqrt(n)
  
  return(list(estimate = estimated_mean, se = standard_error))
}

# Sample sizes to simulate
sample_sizes <- c(10, 30, 100, 300, 1000, 3000, 10000, 30000)

# Perform the simulation for each sample size
results <- sapply(sample_sizes, function(n) simulate_convergence(n), simplify = FALSE)

# Extract the estimates and standard errors
estimates <- sapply(results, function(res) res$estimate)
standard_errors <- sapply(results, function(res) res$se)

# Display the results in a table format
data.frame(Sample_Size = sample_sizes, Estimate = estimates, Standard_Error = standard_errors)

# Plotting the standard error vs sample size
plot(sample_sizes, standard_errors, type = "b", log = "x",
     main = "Convergence of Standard Error",
     xlab = "Sample Size (log scale)", ylab = "Standard Error",
     pch = 16, col = "blue")


# Question 7.10
# Load necessary libraries
library(ggplot2)

# Load the dataset
df <- read.csv("Walmart_Store_sales.csv")

# Fit the linear regression model
model <- lm(Weekly_Sales ~ Temperature, data = df)

# Print the summary of the model
summary(model)

# Create a scatter plot with the fitted line
ggplot(df, aes(x = Temperature, y = Weekly_Sales)) +
  geom_point(alpha = 0.5) +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +  # Fitted line
  labs(title = "Weekly Sales vs. Temperature",
       x = "Temperature (°F)",
       y = "Weekly Sales ($)") +
  theme_minimal()
