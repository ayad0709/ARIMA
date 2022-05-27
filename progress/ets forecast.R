## ----warning=FALSE,message=FALSE-----------------------------------------
# Load package
library(fpp2)

# Plot the series
autoplot(a10) +
  ggtitle("Original a10 series")

# Four values of lambda in Box-Cox
a10 %>% BoxCox(lambda = 0.5) %>% autoplot() + ggtitle("Box-Cox transformation (lamba = 0.5)")
a10 %>% BoxCox(lambda = 0.1) %>% autoplot() + ggtitle("Box-Cox transformation (lamba = 0.2)")
a10 %>% BoxCox(lambda = 0) %>% autoplot() + ggtitle("Box-Cox transformation (lamba = 0)")
a10 %>% BoxCox(lambda = -1 ) %>% autoplot() + ggtitle("Box-Cox transformation (lamba = -1)")


## ------------------------------------------------------------------------
# Automatic selection of Box-Cox lamba parameter
BoxCox.lambda(a10)


## ------------------------------------------------------------------------
# Plot US female murder rate
autoplot(wmurders)

# Plot ACF
ggAcf(wmurders)

# Plot differenced murder rate
autoplot(diff(wmurders))

# Plot ACF of differenced murder rate
ggAcf(diff(wmurders))

## ------------------------------------------------------------------------
# Plot h02 data
autoplot(h02) +
  ggtitle("Original h02 series")

# Take logs and seasonal differences of h02
difflogh02 <- diff(log(h02), lag = 12)

# Plot difflogh02
autoplot(difflogh02)

# Plot ACF
ggAcf(difflogh02) +
  ggtitle("ACF of log(h02) series after 12-month seasonal differencing")

## ------------------------------------------------------------------------
# Take lag-1 difference
ddifflogh02 <- diff(difflogh02, lag = 1)
autoplot(ddifflogh02)

# Plot ACF
ggAcf(ddifflogh02) +
  ggtitle("ACF of lag-1 differences of [log(h02) 12-month differenced]")

## ------------------------------------------------------------------------
# Fit an automatic ARIMA model
fit <- auto.arima(austa)

# Check that the residuals look like white noise
checkresiduals(fit)

# Summarize the model
summary(fit)

# Plot forecasts of fit
fit %>% forecast(h = 10) %>% autoplot()

## ------------------------------------------------------------------------
austa %>% Arima(order = c(0,1,1), include.constant = FALSE) %>% forecast() %>% autoplot() +
  ggtitle("ARIMA(0,1,1) model with no drift (simple exponential smoothing)")

austa %>% Arima(order = c(2,1,3), include.constant = TRUE) %>% forecast() %>% autoplot() +
  ggtitle("ARIMA(2,1,3) model with drift")

austa %>% Arima(order = c(0,0,1), include.constant = TRUE) %>% forecast() %>% autoplot() +
  ggtitle("ARIMA(0,0,1) model with constant (first-order moving average)")

austa %>% Arima(order = c(0,2,1), include.constant = FALSE) %>% forecast() %>% autoplot() +
  ggtitle("ARIMA(0,2,1) model with no constant (linear exponential smoothing)")

## ------------------------------------------------------------------------
# Set up forecast functions
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

# Compute CV errors
e1 <- tsCV(austa, fets, h = 1)
e2 <- tsCV(austa, farima, h = 1)

# Compute MSE
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

# Plot 10-year forecasts using the best model class
austa %>% farima(h = 10) %>% autoplot()

## ------------------------------------------------------------------------
# Check variance of log-transformed series
h02 %>% log() %>% autoplot()

# Fit a seasonal ARIMA model
fit <- auto.arima(h02, lambda = 0)

# Summarize
summary(fit)

# Plot 2-year forecasts
fit %>% forecast(h = 24) %>% autoplot() + 
  ggtitle("2 year forecast of monthly corticosteroid drug sales in Australia")

## ------------------------------------------------------------------------
# Find ARIMA model for euretail
fit1 <- auto.arima(euretail)
summary(fit1)

# Don't use a stepwise search
fit2 <- auto.arima(euretail, stepwise = FALSE)
summary(fit2)

# Compute 2-year forecasts
fit2 %>% forecast(h = 8) %>% autoplot() +
  ggtitle("2 year forecast of quarterly retail trade: Euro area")

## ------------------------------------------------------------------------
# Create training series
train <- window(qcement, start = c(1988, 1), end = c(2007, 4))

# Fit ARIMA and ETS model
fit1 <- auto.arima(train)
fit2 <- ets(train)

# Check white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)

# Forecast
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)

# Compare RMSE
accuracy(fc1, qcement)
accuracy(fc2, qcement)

