# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
walmart_sales <- read.csv("Walmart_Store_sales.csv")

# Create a scatter plot for Temperature vs Fuel Price
ggplot(walmart_sales, aes(x = Temperature, y = Fuel_Price)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add a linear trend line
  labs(title = "Temperature vs Fuel Price",
       x = "Temperature (°F)",
       y = "Fuel Price ($)") +
  theme_minimal()

# Convert Date column to Date type (assuming your format is "dd-mm-yyyy")
walmart_sales$Date <- as.Date(walmart_sales$Date, format = "%d-%m-%Y")

# Extract quarter information from Date
walmart_sales$Quarter <- quarters(walmart_sales$Date)

# Check the unique values of Quarter to ensure it's created correctly
print(unique(walmart_sales$Quarter))

# Aggregate sales by quarter
quarterly_sales <- walmart_sales %>%
  group_by(Quarter) %>%
  summarise(Quarterly_Sales = sum(Weekly_Sales, na.rm = TRUE))  # Handle NA values

# Print the aggregated data to check
print(quarterly_sales)

# Plot: Quarterly Sales Growth
ggplot(quarterly_sales, aes(x = Quarter, y = Quarterly_Sales)) +
  geom_bar(stat = "identity", fill = "#abbd07") +
  labs(title = "Quarterly Sales Growth",
       x = "Quarter", 
       y = "Total Sales") +
  theme_minimal()

# Total weekly sales over time
weekly_sales_over_time <- walmart_sales %>%
  group_by(Date) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE))

ggplot(weekly_sales_over_time, aes(x = as.Date(Date), y = Total_Weekly_Sales)) +
  geom_line(color = "blue") +
  labs(title = "Total Weekly Sales Over Time",
       x = "Date", 
       y = "Total Weekly Sales") +
  theme_minimal()

