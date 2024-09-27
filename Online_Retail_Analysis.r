# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset (replace 'your_path' with the actual path to your CSV file)
walmart_sales <- read.csv("Walmart_Store_sales.csv")

# Quick exploration of the data
str(walmart_sales)
summary(walmart_sales)
head(walmart_sales)

# Calculate total sales by store
store_sales <- walmart_sales %>%
  group_by(Store) %>%
  summarise(Total_Sales = sum(Weekly_Sales))

# Find the store with maximum sales
max_sales_store <- store_sales %>%
  filter(Total_Sales == max(Total_Sales))

print(max_sales_store)

# Calculate standard deviation of sales by store
store_sd <- walmart_sales %>%
  group_by(Store) %>%
  summarise(Sales_SD = sd(Weekly_Sales))

# Find the store with maximum standard deviation
max_sd_store <- store_sd %>%
  filter(Sales_SD == max(Sales_SD))

print(max_sd_store)

# Coefficient of mean to standard deviation
store_sd <- store_sd %>%
  inner_join(walmart_sales %>% group_by(Store) %>% summarise(Sales_Mean = mean(Weekly_Sales)), by = "Store") %>%
  mutate(Coeff_Var = Sales_SD / Sales_Mean)

print(store_sd)

# Filter data for Q3 2012
walmart_sales$Date <- as.Date(walmart_sales$Date, format="%Y-%m-%d")
q3_2012_sales <- walmart_sales %>%
  filter(Date >= as.Date("2012-07-01") & Date <= as.Date("2012-09-30")) %>%
  group_by(Store) %>%
  summarise(Q3_Sales = sum(Weekly_Sales))

# Calculate growth rate compared to Q2
q2_2012_sales <- walmart_sales %>%
  filter(Date >= as.Date("2012-04-01") & Date <= as.Date("2012-06-30")) %>%
  group_by(Store) %>%
  summarise(Q2_Sales = sum(Weekly_Sales))

# Merge Q2 and Q3 sales, and calculate growth rate
sales_growth <- inner_join(q2_2012_sales, q3_2012_sales, by = "Store") %>%
  mutate(Growth_Rate = (Q3_Sales - Q2_Sales) / Q2_Sales * 100)

print(sales_growth)

# Average sales in non-holiday weeks
non_holiday_sales <- walmart_sales %>%
  filter(Holiday_Flag == 0) %>%
  summarise(Mean_Sales = mean(Weekly_Sales))

# Average sales during holidays
holiday_sales <- walmart_sales %>%
  filter(Holiday_Flag == 1) %>%
  group_by(Store) %>%
  summarise(Holiday_Avg_Sales = mean(Weekly_Sales)) %>%
  filter(Holiday_Avg_Sales > non_holiday_sales$Mean_Sales)

print(holiday_sales)


# Monthly view of sales
monthly_sales <- walmart_sales %>%
  group_by(month = format(Date, "%Y-%m")) %>%
  summarise(Monthly_Sales = sum(Weekly_Sales))

print(monthly_sales)

# Semester view of sales
walmart_sales$Semester <- ifelse(as.numeric(format(walmart_sales$Date, "%m")) <= 6, "H1", "H2")

semester_sales <- walmart_sales %>%
  group_by(Semester) %>%
  summarise(Semester_Sales = sum(Weekly_Sales))

print(semester_sales)

# Filter data for Store 1
store1_sales <- walmart_sales %>%
  filter(Store == 20)

# Create a new variable for the number of days since the start
store1_sales <- store1_sales %>%
  mutate(Days = as.numeric(Date - min(Date)))

# Build the linear regression model
model <- lm(Weekly_Sales ~ Days + CPI + Unemployment + Fuel_Price, data = store1_sales)

# Summarize the model
summary(model)

# Predict future sales
store1_sales$Predicted_Sales <- predict(model, store1_sales)

# Plot actual vs predicted sales
ggplot(store1_sales, aes(x = Date)) +
  geom_line(aes(y = Weekly_Sales), color = "blue") +
  geom_line(aes(y = Predicted_Sales), color = "red") +
  labs(title = "Actual vs Predicted Sales for Store 1")

# Group by store and calculate the average unemployment rate per store
unemployment_per_store <- walmart_sales %>%
  group_by(Store) %>%
  summarise(Avg_Unemployment = mean(Unemployment, na.rm = TRUE))

# Plotting the unemployment rate per store
ggplot(unemployment_per_store, aes(x = factor(Store), y = Avg_Unemployment)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Unemployment Rate per Store", 
       x = "Store", 
       y = "Average Unemployment Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plotting Temperature vs Weekly Sales
ggplot(walmart_sales, aes(x = Temperature, y = Weekly_Sales)) +
  geom_point(color = "darkgreen", alpha = 0.6) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear trend line
  labs(title = "Effect of Temperature on Weekly Sales",
       x = "Temperature (°F)",
       y = "Weekly Sales") +
  theme_minimal()

# Create temperature bins for better visualization
walmart_sales$Temp_Bins <- cut(walmart_sales$Temperature, breaks = seq(min(walmart_sales$Temperature), max(walmart_sales$Temperature), by = 10))

# Plotting Temperature effect on Weekly Sales using a box plot
ggplot(walmart_sales, aes(x = Temp_Bins, y = Weekly_Sales)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Effect of Temperature on Weekly Sales (Box Plot)",
       x = "Temperature Bins (°F)",
       y = "Weekly Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert Holiday_Flag to a factor for better labeling
walmart_sales$Holiday_Flag <- factor(walmart_sales$Holiday_Flag, 
                                     labels = c("Non-Holiday Week", "Holiday Week"))

# Plot Holiday Week vs Normal Week sales
ggplot(walmart_sales, aes(x = Holiday_Flag, y = Weekly_Sales)) +
  geom_boxplot(fill = c("lightgreen", "lightcoral"), color = "darkgreen") +
  labs(title = "Comparison of Sales: Holiday Week vs Non-Holiday Week",
       x = "Week Type",
       y = "Weekly Sales") +
  theme_minimal()














# Plot Holiday Week vs Non-Holiday Week sales
walmart_sales$Holiday_Flag <- factor(walmart_sales$Holiday_Flag, labels = c("Non-Holiday Week", "Holiday Week"))

ggplot(walmart_sales, aes(x = Holiday_Flag, y = Weekly_Sales)) +
  geom_boxplot(fill = c("lightgreen", "lightcoral"), color = "darkgreen") +
  labs(title = "Comparison of Sales: Holiday Week vs Non-Holiday Week", 
       x = "Week Type", 
       y = "Weekly Sales") +
  theme_minimal()

# Plotting Temperature vs Weekly Sales
ggplot(walmart_sales, aes(x = Temperature, y = Weekly_Sales)) +
  geom_point(color = "darkgreen", alpha = 0.6) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear trend line
  labs(title = "Effect of Temperature on Weekly Sales",
       x = "Temperature (°F)",
       y = "Weekly Sales") +
  theme_minimal()

# Plotting Unemployment Rate per Store
unemployment_per_store <- walmart_sales %>%
  group_by(Store) %>%
  summarise(Avg_Unemployment = mean(Unemployment, na.rm = TRUE))

ggplot(unemployment_per_store, aes(x = factor(Store), y = Avg_Unemployment)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Unemployment Rate per Store", 
       x = "Store", 
       y = "Average Unemployment Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))










# Filter out rows with NA values in CPI
walmart_sales_filtered <- walmart_sales %>%
  filter(!is.na(CPI))
# Create CPI bins to categorize the CPI values
walmart_sales$CPI_Bins <- cut(walmart_sales$CPI, 
                              breaks = seq(min(walmart_sales$CPI), max(walmart_sales$CPI), by = 5), 
                              include.lowest = TRUE)

# Create a box plot to visualize Weekly Sales distribution across CPI bins
ggplot(walmart_sales, aes(x = CPI_Bins, y = Weekly_Sales)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Distribution of Weekly Sales Across CPI Bins",
       x = "CPI Bins",
       y = "Weekly Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Aggregate sales by store
store_sales <- walmart_sales %>%
  group_by(Store) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales))

# Plot: Total Weekly Sales by Store (Barplot)
ggplot(store_sales, aes(x = factor(Store), y = Total_Weekly_Sales)) +
  geom_bar(stat = "identity", fill = "#08313f") +
  labs(title = "Total Weekly Sales by Store",
       x = "Store", 
       y = "Total Weekly Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Aggregate sales by store
store_sales <- walmart_sales %>%
  group_by(Store) %>%
  summarise(Total_Weekly_Sales = sum(Weekly_Sales))

# Plot: Total Weekly Sales by Store (Barplot)
ggplot(store_sales, aes(x = factor(Store), y = Total_Weekly_Sales)) +
  geom_bar(stat = "identity", fill = "#076486") +
  labs(title = "Total Weekly Sales by Store",
       x = "Store", 
       y = "Total Weekly Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

