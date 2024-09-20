# Load necessary libraries
library(caret)

# Load the data from a .txt file
helicopter_data <- read.table("Helicopter_Data.txt", header = TRUE)

# Fit the linear model with Width and Length as predictors for Time
model <- lm(Time ~ Width + Length, data = helicopter_data)

# Summarize the model to see the coefficients and statistical significance
summary(model)

# Predict the values using the model
predictions <- predict(model, helicopter_data)

# Evaluate the model performance using Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((helicopter_data$Time - predictions)^2))
print(paste("RMSE:", rmse))

# Plot the predicted vs actual values
plot(helicopter_data$Time, predictions, main = "Predicted vs Actual", 
     xlab = "Actual Time", ylab = "Predicted Time")
abline(0, 1, col = "red")


# Add squared terms for Width and Length
helicopter_data$Width_squared <- helicopter_data$Width^2
helicopter_data$Length_squared <- helicopter_data$Length^2

# Fit the quadratic model
quadratic_model <- lm(Time ~ Width + Length + Width_squared + Length_squared, data = helicopter_data)

# Summarize the quadratic model
summary(quadratic_model)

# Predict values using the quadratic model
quadratic_predictions <- predict(quadratic_model, helicopter_data)

# Evaluate model performance using RMSE
quadratic_rmse <- sqrt(mean((helicopter_data$Time - quadratic_predictions)^2))
print(paste("Quadratic Model RMSE:", quadratic_rmse))

# Plot Predicted vs Actual
plot(helicopter_data$Time, quadratic_predictions, 
     xlab = "Actual Time", ylab = "Predicted Time", 
     main = "Predicted vs Actual (Quadratic Model)")
abline(0, 1, col = "red")

# Fit the interaction model
interaction_model <- lm(Time ~ Width * Length, data = helicopter_data)

# Summarize the interaction model
summary(interaction_model)

# Predict values using the interaction model
interaction_predictions <- predict(interaction_model, helicopter_data)

# Evaluate model performance using RMSE
interaction_rmse <- sqrt(mean((helicopter_data$Time - interaction_predictions)^2))
print(paste("Interaction Model RMSE:", interaction_rmse))

# Plot Predicted vs Actual for Interaction Model
plot(helicopter_data$Time, interaction_predictions, 
     xlab = "Actual Time", ylab = "Predicted Time", 
     main = "Predicted vs Actual (Interaction Model)")
abline(0, 1, col = "red")

# Fit the combined quadratic and interaction model
combined_model <- lm(Time ~ Width * Length + I(Width^2) + I(Length^2), data = helicopter_data)

# Summarize the combined model
summary(combined_model)

# Predict values using the combined model
combined_predictions <- predict(combined_model, helicopter_data)

# Evaluate model performance using RMSE
combined_rmse <- sqrt(mean((helicopter_data$Time - combined_predictions)^2))
print(paste("Combined Quadratic and Interaction Model RMSE:", combined_rmse))

# Plot Predicted vs Actual for Combined Model
plot(helicopter_data$Time, combined_predictions, 
     xlab = "Actual Time", ylab = "Predicted Time", 
     main = "Predicted vs Actual (Combined Model)")
abline(0, 1, col = "red")

