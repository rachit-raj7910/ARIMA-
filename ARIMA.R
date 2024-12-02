# Load necessary libraries
#install.packages("forecast")
library(readr)
library(forecast)
library(tseries)

# Step 1: Load the data
data <- read_csv("C:/Users/rachi/Downloads/yearly-number-of-objects-launched-into-outer-space.csv")
data
# Replace 'path/to/your/' with the actual path to your file

# Inspect the data structure
head(data)

# Step 2: Convert data to a time series
launches_ts <- ts(data$`No. of Objects Launched`, start = 1975, end = 2023 ,frequency = 1)
launches_ts

# Step 3: Plot the data to check for trend and stationarity
plot(launches_ts, main = "No. of Objects Launched(1975-2023)", 
     xlab = "Year", ylab = "No. of Objects Launched")

# Step 4: Check stationarity using the Augmented Dickey-Fuller Test
adf_test <- adf.test(launches_ts)
print(adf_test)

# If non-stationary (p-value > 0.05), difference the series and check again
if(adf_test$p.value > 0.05) {
  launches_ts_diff <- diff(launches_ts)
  
  # Suppress warning and run the test again
  adf_test_diff <- suppressWarnings(adf.test(launches_ts_diff))
  
  # Print results and check if p-value is below 0.05 (indicating stationarity)
  print(adf_test_diff)
}

# Step 5: Determine ARIMA model order (p, d, q) using auto.arima()
model <- auto.arima(launches_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(model)

# Step 6: Diagnostic checks
# Plot residuals and ACF of residuals
checkresiduals(model)

# Step 7: Forecast for 2024
forecast_2024 <- forecast(model, h = 1)
print(forecast_2024)
plot(forecast_2024, main = "Forecast for No. of Objects Launched in 2024")

# Save the forecasted result
write.csv(data.frame(forecast_2024), "forecast_2024.csv")






# Step 3: Plot the data to check for trend and stationarity
plot(launches_ts, xlim = c(1975, 2024), ylim = range(c(launches_ts, forecast_2024$mean)), 
     main = "No. of Objects Launched by India (1975-2024)", 
     xlab = "Year", ylab = "No. of Objects Launched")

# Add forecasted point for 2024
points(2024, forecast_2024$mean, col = "blue", pch = 19) # forecasted value as a point
lines(c(2023, 2024), c(tail(launches_ts, 1), forecast_2024$mean), col = "blue", lty = 2) # connect last data point to forecast

# Add confidence intervals for 2024
segments(2024, forecast_2024$lower[2], 2024, forecast_2024$upper[2], col = "red", lwd = 2) # 95% CI
segments(2024, forecast_2024$lower[1], 2024, forecast_2024$upper[1], col = "orange", lwd = 2) # 80% CI

legend("topleft", legend = c("Historical Data", "Forecast (2024)", "80% CI", "95% CI"),
       col = c("black", "blue", "orange", "red"), lty = c(1, 2, 1, 1), pch = c(NA, 19, NA, NA), bty = "n")
















