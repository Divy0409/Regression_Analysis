# Question 1

simulate_and_fit <- function(a, b, n, sigma) {
  # (i) Simulate data points
  x <- runif(n, 0, 100)  # Uniformly sample x from range (0, 100)
  error <- rnorm(n, mean = 0, sd = sigma)  # Errors drawn from normal distribution
  y <- a + b * x + error  # Simulate y based on the model y = a + bx + error
  
  # (ii) Fit linear regression
  fit <- lm(y ~ x)  # Fit linear regression y = a + bx
  
  # Print out the fitted regression summary
  print(summary(fit))
  
  # (iii) Scatterplot of the data and fitted regression line
  plot(x, y, main = "Simulated Data with Fitted Regression Line",
       xlab = "x", ylab = "y", pch = 16, col = "blue")
  abline(fit, col = "red", lwd = 2)  # Add the fitted regression line
  
  # (iv) Report how many standard errors the slope estimate is from zero
  slope_estimate <- coef(fit)["x"]
  slope_se <- summary(fit)$coefficients["x", "Std. Error"]
  se_from_zero <- abs(slope_estimate / slope_se)
  cat("The slope estimate is", se_from_zero, "standard errors from zero.\n")
  
  # Return the simulated data points
  return(data.frame(x = x, y = y))
}

# Check the function with given values
set.seed(123)  # For reproducibility
a <- 100
b <- 0.5
n <- 500
sigma <- 100

simulate_and_fit(a, b, n, sigma)




# Question 2


simulate_and_fit_1000 <- function(a, b, n, sigma, n_simulations = 1000) {
  # Vector to store how many standard errors the slope estimate is from zero
  se_from_zero_values <- numeric(n_simulations)
  
  # Vector to store p-values for power calculation
  p_values <- numeric(n_simulations)
  
  for (i in 1:n_simulations) {
    # Simulate data
    x <- runif(n, 0, 100)  # Uniformly sample x from range (0, 100)
    error <- rnorm(n, mean = 0, sd = sigma)  # Errors drawn from normal distribution
    y <- a + b * x + error  # Simulate y based on the model y = a + bx + error
    
    # Fit linear regression
    fit <- lm(y ~ x)
    
    # Calculate how many standard errors the slope estimate is from zero
    slope_estimate <- coef(fit)["x"]
    slope_se <- summary(fit)$coefficients["x", "Std. Error"]
    se_from_zero_values[i] <- abs(slope_estimate / slope_se)
    
    # Store the p-value for testing slope != 0 (for power calculation)
    p_values[i] <- summary(fit)$coefficients["x", "Pr(>|t|)"]
  }
  
  # Plot a histogram of the 1000 values of standard errors from zero
  hist(se_from_zero_values, main = "Histogram of Standard Errors from Zero",
       xlab = "Standard Errors from Zero", breaks = 30, col = "lightblue")
  
  # Estimate the power of the test
  # Power is the proportion of p-values less than 0.05 (rejecting the null hypothesis)
  power <- mean(p_values < 0.05)
  cat("Estimated power of the test (proportion of times the null is rejected):", power, "\n")
  
  return(se_from_zero_values)
}

# Check the function with the given values
set.seed(123)  # For reproducibility
a <- 100
b <- 0.5
n <- 500
sigma <- 100

# Run the simulation 1000 times
se_from_zero_values <- simulate_and_fit_1000(a, b, n, sigma, n_simulations = 1000)



# Question 3


simulate_and_fit_1000 <- function(a, b, n, sigma, n_simulations = 1000) {
  # Vector to store how many standard errors the slope estimate is from zero
  se_from_zero_values <- numeric(n_simulations)
  
  # Vector to store p-values for power calculation
  p_values <- numeric(n_simulations)
  
  for (i in 1:n_simulations) {
    # Simulate data
    x <- runif(n, 0, 100)  # Uniformly sample x from range (0, 100)
    error <- rnorm(n, mean = 0, sd = sigma)  # Errors drawn from normal distribution
    y <- a + b * x + error  # Simulate y based on the model y = a + bx + error
    
    # Fit linear regression
    fit <- lm(y ~ x)
    
    # Calculate how many standard errors the slope estimate is from zero
    slope_estimate <- coef(fit)["x"]
    slope_se <- summary(fit)$coefficients["x", "Std. Error"]
    se_from_zero_values[i] <- abs(slope_estimate / slope_se)
    
    # Store the p-value for testing slope != 0 (for power calculation)
    p_values[i] <- summary(fit)$coefficients["x", "Pr(>|t|)"]
  }
  
  # Plot a histogram of the 1000 values of standard errors from zero
  hist(se_from_zero_values, main = "Histogram of Standard Errors from Zero (n=50)",
       xlab = "Standard Errors from Zero", breaks = 30, col = "lightblue")
  
  # Estimate the power of the test
  # Power is the proportion of p-values less than 0.05 (rejecting the null hypothesis)
  power <- mean(p_values < 0.05)
  cat("Estimated power of the test (n=50):", power, "\n")
  
  return(se_from_zero_values)
}

# Check the function with the smaller sample size (n = 50)
set.seed(123)  # For reproducibility
a <- 100
b <- 0.5
n <- 50
sigma <- 100

# Run the simulation 1000 times for n = 50
se_from_zero_values <- simulate_and_fit_1000(a, b, n, sigma, n_simulations = 1000)



# Question 4


# Import the data
heights_data <- read.table("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/PearsonLee/data/Heights.txt", header=TRUE)

# View the first few rows
head(heights_data)

# Part 1: Regression on the entire dataset (Baseline)
fit_all <- lm(daughter_height ~ mother_height, data = heights_data)
summary(fit_all)

# Part (a): Regression for mothers' heights less than the mean
mean_mother_height <- mean(heights_data$mother_height)
fit_mothers_below_mean <- lm(daughter_height ~ mother_height, data = heights_data[heights_data$mother_height < mean_mother_height, ])
summary(fit_mothers_below_mean)



# Question 5


# Part (b): Regression for daughters' heights less than the mean
mean_daughter_height <- mean(heights_data$daughter_height)
fit_daughters_below_mean <- lm(daughter_height ~ mother_height, data = heights_data[heights_data$daughter_height < mean_daughter_height, ])
summary(fit_daughters_below_mean)



# Question 6

# Comparison of coefficients
coef_all <- coef(fit_all)
coef_mothers <- coef(fit_mothers_below_mean)
coef_daughters <- coef(fit_daughters_below_mean)

cat("\nCoefficients (All Data):\n", coef_all)
cat("\nCoefficients (Mothers' Heights < Mean):\n", coef_mothers)
cat("\nCoefficients (Daughters' Heights < Mean):\n", coef_daughters)
  