# Load necessary libraries
library(rstanarm)
library(ggplot2)

# Question 9.10

# Set seed for reproducibility
set.seed(123)

# 9.10 (a): Simulate data with n = 100 and compute least squares and Bayesian estimates
simulate_and_estimate <- function(n) {
  # Generate x values uniformly from (-1, 1)
  x <- runif(n, -1, 1)
  
  # True parameters for the simulation
  a_true <- 1
  b_true <- 0.1
  sigma_true <- 0.5
  
  # Simulate y values using true parameters
  y <- a_true + b_true * x + rnorm(n, mean = 0, sd = sigma_true)
  
  # Least squares estimates using lm
  lm_fit <- lm(y ~ x)
  a_lm <- coef(lm_fit)[1]
  b_lm <- coef(lm_fit)[2]
  
  # Bayesian estimates using stan_glm with specified priors
  bayes_fit <- stan_glm(y ~ x, 
                        prior_intercept = normal(0, 1),
                        prior = normal(0, 0.2),
                        prior_aux = exponential(1),
                        data = data.frame(x = x, y = y), 
                        seed = 123)
  a_bayes <- coef(bayes_fit)[1]
  b_bayes <- coef(bayes_fit)[2]
  
  # Return both estimates for comparison
  return(list(a_lm = a_lm, b_lm = b_lm, a_bayes = a_bayes, b_bayes = b_bayes))
}

# Part (a) results with n = 100
estimates_100 <- simulate_and_estimate(100)
cat("Part (a) Results with n = 100:\n")
cat("Least Squares Estimate of a:", estimates_100$a_lm, "\n")
cat("Least Squares Estimate of b:", estimates_100$b_lm, "\n")
cat("Bayesian Estimate of a:", estimates_100$a_bayes, "\n")
cat("Bayesian Estimate of b:", estimates_100$b_bayes, "\n\n")

# 9.10 (b): Repeat simulations with different values of n and plot results
n_values <- floor(exp(1:11))
log_n_values <- log(n_values)

# Storage for estimates across different values of n
a_lm_values <- numeric(length(n_values))
b_lm_values <- numeric(length(n_values))
a_bayes_values <- numeric(length(n_values))
b_bayes_values <- numeric(length(n_values))

# Loop over n_values to get estimates for each n
for (i in seq_along(n_values)) {
  estimates <- simulate_and_estimate(n_values[i])
  a_lm_values[i] <- estimates$a_lm
  b_lm_values[i] <- estimates$b_lm
  a_bayes_values[i] <- estimates$a_bayes
  b_bayes_values[i] <- estimates$b_bayes
}

# Plot Bayesian estimates for a and b as functions of log(n)
plot(log_n_values, a_bayes_values, type = "b", col = "blue", pch = 16,
     xlab = "log(n)", ylab = "Estimate of a", main = "Bayesian Estimate for a vs. log(n)")

plot(log_n_values, b_bayes_values, type = "b", col = "blue", pch = 16,
     xlab = "log(n)", ylab = "Estimate of b", main = "Bayesian Estimate for b vs. log(n)")


# 9.10 (c): Calculate distance from Bayesian to least squares estimates & halfway points
a_distance <- abs(a_bayes_values - a_lm_values)
b_distance <- abs(b_bayes_values - b_lm_values)

# Plot absolute distance for a and b
plot(log_n_values, a_distance, type = "b", col = "blue", pch = 16,
     xlab = "log(n)", ylab = "Absolute Distance for a", 
     main = "Absolute Distance for a between Bayesian and LS")

plot(log_n_values, b_distance, type = "b", col = "blue", pch = 16,
     xlab = "log(n)", ylab = "Absolute Distance for b", 
     main = "Absolute Distance for b between Bayesian and LS")

# Calculate approximate n where Bayesian estimate is halfway between prior mean and LS estimate
a_halfway_n <- n_values[which.min(abs(a_bayes_values - (0 + a_lm_values) / 2))]
b_halfway_n <- n_values[which.min(abs(b_bayes_values - (0 + b_lm_values) / 2))]

# Print halfway point results for Part (c)
cat("\nPart (c) Results:\n")
cat("Approximate n where Bayesian estimate for a is halfway between prior mean and LS estimate:", a_halfway_n, "\n")
cat("Approximate n where Bayesian estimate for b is halfway between prior mean and LS estimate:", b_halfway_n, "\n")

# Print the n values where Bayesian estimate approximately equals least squares estimate
cat("Approximate n where Bayesian estimate equals least squares for a:", n_values[which.min(a_distance)], "\n")
cat("Approximate n where Bayesian estimate equals least squares for b:", n_values[which.min(b_distance)], "\n")


# Question 9.11 

# 9.11 Fit a Bayesian linear regression model with informative priors

# Load the dataset
df <- read.csv("Walmart_Store_sales.csv")
head(df)
# Fit the linear regression model
model <- lm(Fuel_Price ~ Temperature, data = df)

# Print the summary of the model
cat("\nSummary of Linear Regression Model:\n")
print(summary(model))

# Create a scatter plot with the fitted line using ggplot2
ggplot(df, aes(x = Temperature, y = Fuel_Price)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +  # Fitted line
  labs(title = "Fuel_Price vs. Temperature",
       x = "Temperature (°F)",
       y = "Fuel Price ($)") +
  theme_minimal()

# Prior mean of 0.0036 and standard deviation of 0.001 based on linear regression findings
prior_slope <- normal(location = 0.0036, scale = 0.001, autoscale = FALSE)

# Set an informative prior for the intercept around the estimated value with some flexibility
prior_intercept <- normal(location = 3.14, scale = 0.05)

# Fit the Bayesian linear regression model
bayesian_model <- stan_glm(Fuel_Price ~ Temperature, data = df, prior = prior_slope, prior_intercept = prior_intercept)

# Print the summary of the Bayesian model
summary(bayesian_model)

# Create a scatter plot with the fitted line
ggplot(df, aes(x = Temperature, y = Fuel_Price)) +
  geom_point(alpha = 0.5) +  
  geom_abline(intercept = coef(bayesian_model)["(Intercept)"], slope = coef(bayesian_model)["Temperature"], color = "blue") +
  labs(title = "Bayesian Model: Fuel Price vs. Temperature",
       x = "Temperature (°F)",
       y = "Fuel Price ($)") +
  theme_minimal()

