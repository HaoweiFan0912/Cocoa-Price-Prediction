### Load required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(xgboost)
library(caret)
library(slider)
library(rugarch)

### Load and Aggregate Cocoa Price Data to Monthly
cocoa_prices <- read.csv("Daily Prices_ICCO.csv", stringsAsFactors = FALSE)
cocoa_prices$Date <- as.Date(cocoa_prices$Date, format='%d/%m/%Y')
cocoa_prices$Price <- as.numeric(gsub(",", "", cocoa_prices$ICCO.daily.price..US..tonne.))
cocoa_prices <- cocoa_prices %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Price = mean(Price, na.rm = TRUE)) %>%
  ungroup()

### Load and Aggregate Ghana Weather Data to Monthly
ghana_weather <- read.csv("Ghana_data.csv", stringsAsFactors = FALSE)
ghana_weather$DATE <- as.Date(ghana_weather$DATE)
ghana_weather <- ghana_weather %>%
  mutate(YearMonth = floor_date(DATE, "month")) %>%
  group_by(YearMonth) %>%
  summarise(across(c(PRCP, TAVG, TMAX, TMIN), mean, na.rm = TRUE))

### Merge and Clean Monthly Data
cocoa_data <- left_join(cocoa_prices, ghana_weather, by = "YearMonth") %>%
  mutate(log_price = log(Price),
         diff_log_price = c(NA, diff(log_price))) %>%
  drop_na()

### Plot Monthly Time Series
ggplot(cocoa_data, aes(x = YearMonth)) +
  geom_line(aes(y = Price), color = "steelblue") +
  labs(title = "Monthly Cocoa Prices", y = "Price", x = "Date") +
  theme_minimal()

### Split Data into Training and Testing Sets
train_size <- floor(0.8 * nrow(cocoa_data))
train_data <- cocoa_data[1:train_size, ]
test_data <- cocoa_data[(train_size + 1):nrow(cocoa_data), ]

### Fit ETS and ARIMA Models
ets_model_1 <- ets(train_data$diff_log_price)
ets_model_2 <- ets(train_data$diff_log_price, model = "ZZZ")
external_regressors <- train_data %>% select(PRCP, TAVG, TMAX, TMIN)
arimax_model <- auto.arima(train_data$diff_log_price, xreg = as.matrix(external_regressors), seasonal = FALSE)
sarimax_model <- auto.arima(train_data$diff_log_price, xreg = as.matrix(external_regressors), seasonal = TRUE)

### Forecasting
test_xreg <- as.matrix(test_data %>% select(PRCP, TAVG, TMAX, TMIN))
ets_forecast_1 <- forecast(ets_model_1, h = nrow(test_data))
ets_forecast_2 <- forecast(ets_model_2, h = nrow(test_data))
arimax_forecast <- forecast(arimax_model, xreg = test_xreg, h = nrow(test_data))
sarimax_forecast <- forecast(sarimax_model, xreg = test_xreg, h = nrow(test_data))

### Accuracy Comparison
ets_accuracy_1 <- accuracy(ets_forecast_1, test_data$diff_log_price)
ets_accuracy_2 <- accuracy(ets_forecast_2, test_data$diff_log_price)
arimax_accuracy <- accuracy(arimax_forecast, test_data$diff_log_price)
sarimax_accuracy <- accuracy(sarimax_forecast, test_data$diff_log_price)

print("ETS Model 1 Performance:")
print(ets_accuracy_1)
print("ETS Model 2 Performance:")
print(ets_accuracy_2)
print("ARIMAX Model Performance:")
print(arimax_accuracy)
print("SARIMAX Model Performance:")
print(sarimax_accuracy)

### Back-transform forecasted values
reconstruct_log_prices <- function(last_log_price, diffs) {
  cumsum(c(last_log_price, diffs))[-1]
}

last_log_price <- tail(train_data$log_price, 1)
n <- nrow(test_data)
forecast_dates <- test_data$YearMonth

ets1_log_forecast <- reconstruct_log_prices(last_log_price, ets_forecast_1$mean)
ets2_log_forecast <- reconstruct_log_prices(last_log_price, ets_forecast_2$mean)
arimax_log_forecast <- reconstruct_log_prices(last_log_price, arimax_forecast$mean)
sarimax_log_forecast <- reconstruct_log_prices(last_log_price, sarimax_forecast$mean)

ets1_price_forecast <- exp(ets1_log_forecast)
ets1_price_forecast
ets2_price_forecast <- exp(ets2_log_forecast)
ets2_price_forecast
arimax_price_forecast <- exp(arimax_log_forecast)
arimax_price_forecast 
sarimax_price_forecast <- exp(sarimax_log_forecast)
sarimax_price_forecast



forecast_df <- bind_rows(
  tibble(Date = forecast_dates, Forecast = ets1_price_forecast, Model = "ETS Model 1"),
  tibble(Date = forecast_dates, Forecast = ets2_price_forecast, Model = "ETS Model 2"),
  tibble(Date = forecast_dates, Forecast = arimax_price_forecast, Model = "ARIMAX"),
  tibble(Date = forecast_dates, Forecast = sarimax_price_forecast, Model = "SARIMAX")
) %>% drop_na()

forecast_df

ggplot() +
  geom_line(data = cocoa_data, aes(x = YearMonth, y = Price), color = "black", linewidth = 1.2) +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast, color = Model, linetype = Model), linewidth = 1.2) +
  labs(title = "Monthly Forecasts vs Actual Cocoa Prices", y = "Price", x = "Date") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(
    "ETS Model 1" = "blue",
    "ETS Model 2" = "green",
    "ARIMAX" = "red",
    "SARIMAX" = "purple"
  )) +
  scale_linetype_manual(values = c(
    "ETS Model 1" = "solid",
    "ETS Model 2" = "dashed",
    "ARIMAX" = "twodash",
    "SARIMAX" = "dotdash"
  ))




### Load and Aggregate Cocoa Price Data to Monthly
cocoa_prices <- read.csv("Daily Prices_ICCO.csv", stringsAsFactors = FALSE)
cocoa_prices$Date <- as.Date(cocoa_prices$Date, format='%d/%m/%Y')
cocoa_prices$Price <- as.numeric(gsub(",", "", cocoa_prices$ICCO.daily.price..US..tonne.))
cocoa_prices <- cocoa_prices %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Price = mean(Price, na.rm = TRUE)) %>%
  ungroup()

### Load and Aggregate Ghana Weather Data to Monthly
ghana_weather <- read.csv("Ghana_data.csv", stringsAsFactors = FALSE)
ghana_weather$DATE <- as.Date(ghana_weather$DATE)
ghana_weather <- ghana_weather %>%
  mutate(YearMonth = floor_date(DATE, "month")) %>%
  group_by(YearMonth) %>%
  summarise(across(c(PRCP, TAVG, TMAX, TMIN), mean, na.rm = TRUE))

### Merge and Clean Monthly Data
cocoa_data <- left_join(cocoa_prices, ghana_weather, by = "YearMonth") %>%
  mutate(log_price = log(Price),
         diff_log_price = c(NA, diff(log_price))) %>%
  drop_na()

### Plot Monthly Time Series
ggplot(cocoa_data, aes(x = YearMonth)) +
  geom_line(aes(y = Price), color = "steelblue") +
  labs(title = "Monthly Cocoa Prices", y = "Price", x = "Date") +
  theme_minimal()

### Create Lag Features
generate_lags <- function(data, lags = 1:7) {
  for (lag in lags) {
    data[[paste0("lag_", lag)]] <- dplyr::lag(data$log_price, lag)
  }
  return(data)
}
cocoa_data_lagged <- generate_lags(cocoa_data) %>% drop_na()

### Linear Regression Model
lm_data <- cocoa_data_lagged %>%
  select(YearMonth, log_price, starts_with("lag_"), PRCP, TAVG, TMAX, TMIN)
train_size <- floor(0.8 * nrow(lm_data))
train_lm <- lm_data[1:train_size, ]
test_lm <- lm_data[(train_size + 1):nrow(lm_data), ]
lm_model <- lm(log_price ~ ., data = train_lm %>% select(-YearMonth))
lm_pred_log <- predict(lm_model, newdata = test_lm)
lm_pred_price <- exp(lm_pred_log)
lm_results <- tibble(
  Date = test_lm$YearMonth,
  Actual = exp(test_lm$log_price),
  Predicted = lm_pred_price
)

### Plot Regression Results
ggplot(lm_results, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "red") +
  geom_line(aes(y = Predicted), color = "blue") +
  labs(title = "Linear Regression Forecast vs Actual Prices (Monthly)", y = "Price", x = "Date") +
  theme_minimal()

### Evaluate Regression
lm_rmse <- sqrt(mean((lm_results$Actual - lm_results$Predicted)^2))
print(paste("Linear Regression RMSE:", round(lm_rmse, 2)))

### ARCH/GARCH Model (Monthly)
log_returns <- diff(log(cocoa_data$Price))
log_returns <- na.omit(log_returns)
train_size <- floor(0.8 * length(log_returns))
train_returns <- log_returns[1:train_size]
test_returns <- log_returns[(train_size + 1):length(log_returns)]
test_dates <- cocoa_data$YearMonth[(train_size + 2):(length(log_returns) + 1)]
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)
garch_fit <- ugarchfit(spec = garch_spec, data = train_returns)
garch_forecast <- ugarchforecast(garch_fit, n.ahead = length(test_returns))
predicted_returns <- as.numeric(fitted(garch_forecast))
last_train_price <- cocoa_data$Price[train_size + 1]
forecast_prices <- last_train_price * exp(cumsum(predicted_returns))
garch_df <- tibble(
  Date = test_dates,
  Price = forecast_prices
)

### Plot GARCH Forecast
ggplot() +
  geom_line(data = cocoa_data, aes(x = YearMonth, y = Price), color = "black") +
  geom_line(data = garch_df, aes(x = Date, y = Price), color = "red") +
  labs(title = "GARCH Forecast vs Actual Prices (Monthly)", y = "Price", x = "Date") +
  theme_minimal()

### Walk-Forward Validation with XGBoost (Monthly)
cocoa_data_lagged <- generate_lags(cocoa_data)
cocoa_data_lagged <- cocoa_data_lagged %>% drop_na()
initial_size <- floor(0.8 * nrow(cocoa_data_lagged))
forecast_horizon <- nrow(cocoa_data_lagged) - initial_size
predictions <- c()
actuals <- c()
dates <- c()
for (i in 1:forecast_horizon) {
  train_data <- cocoa_data_lagged[1:(initial_size + i - 1), ]
  test_data <- cocoa_data_lagged[(initial_size + i), ]
  x_train <- train_data %>% select(starts_with("lag_"), PRCP, TAVG, TMAX, TMIN)
  y_train <- train_data$log_price
  x_test <- test_data %>% select(starts_with("lag_"), PRCP, TAVG, TMAX, TMIN)
  dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
  dtest <- xgb.DMatrix(data = as.matrix(x_test))
  model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror", verbose = 0)
  pred_log <- predict(model, dtest)
  pred_price <- exp(pred_log)
  predictions <- c(predictions, pred_price)
  actuals <- c(actuals, exp(test_data$log_price))
  dates <- c(dates, test_data$YearMonth)
}
xgb_walk_df <- tibble(Date = dates,
                      Actual = actuals,
                      Predicted = predictions)
ggplot(xgb_walk_df, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black") +
  geom_line(aes(y = Predicted), color = "orange") +
  labs(title = "XGBoost Walk-Forward Forecast vs Actual (Monthly)", y = "Price", x = "Date") +
  theme_minimal()
xgb_walk_rmse <- sqrt(mean((xgb_walk_df$Actual - xgb_walk_df$Predicted)^2))
print(paste("XGBoost Walk-Forward RMSE:", round(xgb_walk_rmse, 2)))








