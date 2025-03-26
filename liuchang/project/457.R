# Load necessary libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)

# Load datasets
cocoa_prices <- read.csv("00-data/Price.csv", stringsAsFactors = FALSE)
ghana_weather <- read.csv("00-data/Climate.csv", stringsAsFactors = FALSE)

# Convert date formats
cocoa_prices$Date <- as.Date(cocoa_prices$Date, format='%d/%m/%Y')
ghana_weather$DATE <- as.Date(ghana_weather$DATE)

# Clean price data (remove commas and convert to numeric)
cocoa_prices$Price <- as.numeric(gsub(",", "", cocoa_prices$ICCO.daily.price..US..tonne.))

# Select and arrange data
cocoa_prices <- cocoa_prices %>% select(Date, Price) %>% arrange(Date)

# Aggregate weather data by date (mean values for daily observations)
ghana_weather <- ghana_weather %>%
  group_by(DATE) %>%
  summarise(across(c(PRCP, TAVG, TMAX, TMIN), mean, na.rm = TRUE))

# Merge cocoa prices with weather data
cocoa_data <- left_join(cocoa_prices, ghana_weather, by = c("Date" = "DATE"))
cocoa_data <- na.omit(cocoa_data) # Remove missing values



# 🟢 1️⃣ **Time Series Plot of Cocoa Prices**
ggplot(cocoa_data, aes(x = Date, y = Price)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Cocoa Price Over Time",
       x = "Date", 
       y = "Cocoa Price (USD/tonne)") +
  theme_minimal()
library(corrplot)
library(gridExtra)
# 🟢 2️⃣ **Histogram & Density Plot of Cocoa Prices**
p1 <- ggplot(cocoa_data, aes(x = Price)) +
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +
  labs(title = "Histogram of Cocoa Prices",
       x = "Cocoa Price (USD/tonne)", 
       y = "Frequency") +
  theme_minimal()

p2 <- ggplot(cocoa_data, aes(x = Price)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Cocoa Prices",
       x = "Cocoa Price (USD/tonne)", 
       y = "Density") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2) # Display both plots side by side

# 🟢 3️⃣ **Boxplot to Identify Outliers in Cocoa Prices**
ggplot(cocoa_data, aes(y = Price)) +
  geom_boxplot(fill = "blue", alpha = 0.5, outlier.color = "red") +
  labs(title = "Boxplot of Cocoa Prices",
       y = "Cocoa Price (USD/tonne)") +
  theme_minimal()


# 🟢 4️⃣ **Correlation Heatmap between Cocoa Prices & Weather Variables**
cor_matrix <- cor(cocoa_data %>% select(Price, PRCP, TAVG, TMAX, TMIN), use = "complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 1, number.cex = 0.8)

# 🟢 5️⃣ **Seasonal Decomposition to Analyze Trend, Seasonality, and Residuals**
price_ts <- ts(cocoa_data$Price, start = c(year(min(cocoa_data$Date)), month(min(cocoa_data$Date))), frequency = 365)
decomp <- stl(price_ts, s.window = "periodic")

plot(decomp)








# Split data into training and testing sets
train_size <- floor(0.8 * nrow(cocoa_data))
train_data <- cocoa_data[1:train_size, ]
test_data <- cocoa_data[(train_size + 1):nrow(cocoa_data), ]

# Fit ETS model
ets_model <- ets(train_data$Price)
ets_model 
# Fit ARIMAX model with external regressors
external_regressors_train <- train_data %>% select(PRCP, TAVG, TMAX, TMIN)
external_regressors_test <- test_data %>% select(PRCP, TAVG, TMAX, TMIN)

arimax_model <- auto.arima(train_data$Price, xreg = as.matrix(external_regressors_train), seasonal = FALSE)
arimax_model



# Fit SARIMAX model with seasonal component
sarimax_model <- auto.arima(train_data$Price, xreg = as.matrix(external_regressors_train), seasonal = TRUE)
sarimax_model


# Forecasting for test set
ets_forecast <- forecast(ets_model, h = nrow(test_data))
arimax_forecast <- forecast(arimax_model, xreg = as.matrix(external_regressors_test), h = nrow(test_data))
arimax_forecast 
sarimax_forecast <- forecast(sarimax_model, xreg = as.matrix(external_regressors_test), h = nrow(test_data))

# Convert forecast outputs to data frames for plotting
forecast_df <- data.frame(
  Date = test_data$Date,
  Actual = test_data$Price,
  ETS_Forecast = as.numeric(ets_forecast$mean),
  ARIMAX_Forecast = as.numeric(arimax_forecast$mean),
  SARIMAX_Forecast = as.numeric(sarimax_forecast$mean)
)

# Plot actual vs predicted values
ggplot(forecast_df, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual Price"), size = 1) +
  geom_line(aes(y = ETS_Forecast, color = "ETS Forecast"), linetype = "dashed", size = 1) +
  geom_line(aes(y = ARIMAX_Forecast, color = "ARIMAX Forecast"), linetype = "dotted", size = 1) +
  geom_line(aes(y = SARIMAX_Forecast, color = "SARIMAX Forecast"), linetype = "dotdash", size = 1) +
  labs(title = "Cocoa Price Forecast vs Actual Prices",
       x = "Date",
       y = "Cocoa Price (USD/tonne)",
       color = "Legend") +
  scale_color_manual(values = c("Actual Price" = "blue", "ETS Forecast" = "red", 
                                "ARIMAX Forecast" = "green", "SARIMAX Forecast" = "purple")) +
  theme_minimal()






### Fit ETS Models
ets_model_1 <- ets(train_data$Price)  # Automatically select best ETS model

### Fit Two Parametric Models (ARIMA and SARIMA)
external_regressors <- train_data %>% select(PRCP, TAVG, TMAX, TMIN)
arimax_model <- auto.arima(train_data$Price, xreg = as.matrix(external_regressors), seasonal = FALSE)
sarimax_model <- auto.arima(train_data$Price, xreg = as.matrix(external_regressors), seasonal = TRUE)

### Forecasting for Test Set
ets_forecast_1 <- forecast(ets_model_1, h = nrow(test_data))
arimax_forecast <- forecast(arimax_model, xreg = as.matrix(test_data %>% select(PRCP, TAVG, TMAX, TMIN)), h = nrow(test_data))
sarimax_forecast <- forecast(sarimax_model, xreg = as.matrix(test_data %>% select(PRCP, TAVG, TMAX, TMIN)), h = nrow(test_data))

### Compare Model Performance
ets_accuracy_1 <- accuracy(ets_forecast_1, test_data$Price)
ets_accuracy_1
arimax_accuracy <- accuracy(arimax_forecast, test_data$Price)
arimax_accuracy
sarimax_accuracy <- accuracy(sarimax_forecast, test_data$Price)
sarimax_accuracy












