# Nina McConkey
# MIS581 Mod 6

#----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(caret)

# Setting the workspace
setwd("C:/Users/ninad/Desktop/CGU Global/MIS581/capstone data")

# Importing the data
sales <- read.csv("Product-Sales-Region.csv", header=TRUE)

# Checking for missing values
colSums(is.na(sales))
colSums(sales == "")

# Separate set for numerical values
sales_nums <- sales %>% select(c(Quantity, UnitPrice, Discount, TotalPrice, 
                                 Returned, ShippingCost))

# Converting transaction date to date variable and a month variable
sales$TransactionDate <- as.Date(sales$TransactionDate)
sales$Month <- month(sales$TransactionDate, label = TRUE)

# Generating summary statistics for numerical values
summary(sales_nums)

# Standard Deviations
sd(sales_nums$Quantity)
sd(sales_nums$UnitPrice)
sd(sales_nums$Discount)
sd(sales_nums$TotalPrice)
sd(sales_nums$Returned)
sd(sales_nums$ShippingCost)

# Boxplot for Total Price
ggplot(sales, aes(y = TotalPrice)) +
  geom_boxplot() +
  labs(title = "Boxplot of Total Price",
       y = "Total Price") +
  theme_minimal()

# Histogram for Total Price
ggplot(sales, aes(x = TotalPrice)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = 'Histogram of Total Price',
       x = 'Total Price',
       y = 'Frequency') +
  theme_minimal()

# Barchart for Quantity
ggplot(sales_nums, aes(x = Quantity)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(
    title = "Barchart of Quantity",
    x = "Quantity",
    y = "Frequency"
  ) +
  theme_minimal()

# Scatterplots
plot(sales_nums$Quantity,
     sales_nums$TotalPrice,
     xlab = "Quantity",
     ylab = "Total Price",
     main = "Quantity vs Total Price")

# MAYBE DONT INCLUDE, LOOKS DUMB
plot(sales_nums$Discount,
     sales_nums$TotalPrice,
     xlab = "Discount",
     ylab = "Total Price",
     main = "Discount vs Total Price")

# Correlation matrix
cor(sales_nums)

# Frequency Tables for Categorical variables
table(sales$Region)
table(sales$Product)
table(sales$StoreLocation)
table(sales$Salesperson)

#----------------------------------------------------------------
## For RQ1 Sales Forecasting
# Total sales by month
monthly_sales <- sales %>%
  mutate(YearMonth = floor_date(TransactionDate, unit = "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    TotalSales = sum(TotalPrice, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YearMonth)
print(monthly_sales)

# Total sales line chart
ggplot(monthly_sales, aes(x = YearMonth, y = TotalSales)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Total Sales",
    x = "Month",
    y = "Total Sales"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Making a time series
sales_ts <- ts(
  monthly_sales$TotalSales,
  start = c(2023, 1),
  frequency = 12
)
print(sales_ts)

# Dividing into test and train sets
train_ts <- window(
  sales_ts,
  end = c(2024, 12)
)

test_ts <- window(
  sales_ts,
  start = c(2025, 1)
)

# Basic Forecast
naive_model <- snaive(
  train_ts,
  h = length(test_ts)
)
print(naive_model)

# Plot of predicted vs actual
autoplot(naive_model) +
  autolayer(test_ts, series = "Actual Sales") +
  labs(
    title = "Seasonal Naive Forecast Compared with Actual Sales",
    x = "Year",
    y = "Monthly Total Sales"
  ) +
  theme_minimal()

# Making an ETS Forecasting Model
ets_model <- ets(train_ts)
summary(ets_model)

ets_forecast <- forecast(
  ets_model,
  h = length(test_ts)
)
print(ets_forecast)

# Plotting ETS forecast
autoplot(ets_forecast) +
  autolayer(test_ts, series = "Actual Sales") +
  labs(
    title = "ETS Forecast Compared with Actual Sales",
    x = "Year",
    y = "Monthly Total Sales"
  ) +
  theme_minimal()

# Evaluating model accuracy
naive_accuracy <- accuracy(naive_model, test_ts)
ets_accuracy <- accuracy(ets_forecast, test_ts)
print(naive_accuracy)
print(ets_accuracy)

# Comparison
forecast_comparison <- data.frame(
  Month = monthly_sales$YearMonth[
    monthly_sales$YearMonth >= as.Date("2025-01-01")
  ],
  ActualSales = as.numeric(test_ts),
  PredictedSales = as.numeric(ets_forecast$mean)
)

# Comparison graph
ggplot(forecast_comparison, aes(x = Month)) +
  geom_line(aes(y = ActualSales, linetype = "Actual")) +
  geom_point(aes(y = ActualSales)) +
  geom_line(aes(y = PredictedSales, linetype = "Predicted")) +
  geom_point(aes(y = PredictedSales)) +
  labs(
    title = "Actual and Predicted Monthly Sales",
    x = "Month",
    y = "Total Sales",
    linetype = "Sales"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------------------------------
##For RQ2 Seasonality
# Monthly sales summaries
monthly_summary <- sales %>%
  mutate(Month = month(TransactionDate, label = TRUE)) %>%
  group_by(Month) %>%
  summarise(
    MeanSales = mean(TotalPrice),
    MedianSales = median(TotalPrice),
    SDSales = sd(TotalPrice),
    TotalSales = sum(TotalPrice)
  )
print(monthly_summary)

# ANOVA for months
anova_month <- aov(TotalPrice ~ Month, data = sales)
summary(anova_month)

# Running time series decomposistion
decomp <- decompose(sales_ts)
plot(decomp)

#----------------------------------------------------------------
##For RQ3 Discounts
# Creating a T/F discount variable
sales$Discounted <- ifelse(sales$Discount > 0,
  "Yes",
  "No")

# Discount boxplot
ggplot(sales,
  aes(x = Discounted,
  y = TotalPrice,
  fill = Discounted)) +
geom_boxplot()

# Creating a regression model
rq3 <- lm(TotalPrice ~ Discount, data = sales)
summary(rq3)

t.test(TotalPrice ~ Discounted,
       data = sales)


