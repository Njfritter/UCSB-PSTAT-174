# File was saved under "train"
train <- read.csv("~/Downloads/train.csv")
view(train)

require(MASS)
require(data.table)
require(forecast)
require(astsa)
require(alr3)
require(tseries)
require(ggplot2)
require(base)


# PART 1
# Create a data table
walmart_data <- data.table(train)


# We only will look at the first department and store
# So let's subset it
stores1 <- walmart_data[Store == 1]

stores_dept_1 <- stores1[Dept == 1]

# Let's also create a time series object
walmart_ts <- ts(stores_dept_1$Weekly_Sales, start = c(2010, 5), frequency = 52)


# Now let's plot this 
plot.ts(walmart_ts, main = "Time Series of Walmart Data", ylab = "Weekly Sales")

# The data is very volatile
# With peaks and bounds all over the place


# PART 2
# Let decompose the time series into the 
# seasonal, trend and residual components
decomp <- stl(walmart_ts, s.window = "periodic")

# There is a strong seasonal component
# We will "difference" the data to see if this helps
# This will remove the trend in the data
diff_stores_dept_1 <- diff(walmart_ts)


# Let's add some lag
diff_stores_dept_1_lag <- diff(diff_stores_dept_1, lag = 52)


# Now let's plot this and check it out
plot.ts(diff_stores_dept_1, main = "Time Series Difference of Walmart Data", ylab = "Differences")
# Looking like this helps with stationarity

# Let's turn this into a time series object
diff_ts <- ts(diff_stores_dept_1, start = c(2010, 5), frequency = 52)

# And let's decompose it
decomp_diff <- stl(diff_ts, s.window = "periodic")

# And time to plot it
plot(decomp_diff <- stl(diff_ts, s.window = "periodic"))

# As we can see the seasonal part is almost exactly the same as the differenced data
# This indicates very strong seasonality
# Meaning a transformation would not be very helpful
# And instead I can address this with the seasonal part of the ARIMA model (P, D, Q)


# PART 3
# Now for the ACF and PACF of the regular and differenced time series
acf_pacf <- acf2(walmart_ts)
acf_pacf2 <- acf2(diff_stores_dept_1)

# The ACF/PACF for both aren't nice or pretty 
# ACF/PACF for the original data indicates an ARMA(3, 3)
# ACF/PACF for differenced data indicates an ARMA(3, 2)


# PART 4
# Now let's do auto.arima on the regular and differenced
# Time series 

auto_arima <- auto.arima(walmart_ts, trace = TRUE)


# Using the regular time series data, the best model found
# Was an ARIMA(0,0,1)(0,1,0)[52] 
# And an AIC value of 1879.49
# All coefficients significant

# auto.arima() will only give us a base model to go off of
# So let's try different possible models
# More models were generated than what is shown here
# I went ahead with the 12 best


arima(walmart_ts, order = c(1, 0, 1), seasonal = list(order = c(0, 1, 0), period = 52))

# This is an ARMA(1, 0, 1)(0, 1, 0) model, so it has one AR and one MA term
# And a seasonal difference component of 1
# The ar1 coefficient value of 0.1027 and S.E. of 0.1441
# And a ma1 coefficient value of 0.6171 and S.E. of 0.1088
# The AIC is 1881

arima(walmart_ts, order = c(3, 0, 3), seasonal = list(order = c(0, 1, 0), period = 52))
# This is an ARMA(3, 0, 3)(0, 1, 0) model
# This is the model guess based off of the ACF/PACF graphs
# AIC is 1868.16
# Not all coefficients significant

arima(walmart_ts, order = c(1, 0, 2), seasonal = list(order = c(0, 1, 0), period = 52))
# The AIC is 1871.34
# All coefficients significant

arima(walmart_ts, order = c(2, 0, 0), seasonal = list(order = c(0, 1, 0), period = 52))
# The AIC value is 1869.41!
# We will forecast this one
# All coefficients significant

arima(walmart_ts, order = c(6, 0, 3), seasonal = list(order = c(0, 1, 0), period = 52))
# AIC is 1868.71
# This one is promising
# But not all coefficients significant

arima(walmart_ts, order = c(3, 0, 6), seasonal = list(order = c(0, 1, 0), period = 52))
# The AIC value for this is 1868.33! 
# Not all coefficients significant

arima(walmart_ts, order = c(3, 0, 5), seasonal = list(order = c(1, 1, 1), period = 52))
# Now we will try adding in a seasonal AR and MA component
# The AIC is 1868.2
# This one was VERY close and very promising
# However, the seasonal AR term was not significant
# So this model could not be used

arima(walmart_ts, order = c(3, 0, 2), seasonal = list(order = c(1, 1, 1), period = 52))
# The AIC is 1864.82
# Not all coefficients significant

arima(walmart_ts, order = c(1, 0, 2), seasonal = list(order = c(0, 2, 0), period = 52))
# This is an ARMA(1, 0, 2)(0, 2, 0) model so now we are introducing a differencing seasonal factor of 2
# The AIC is only 836.43; however, the sigma^2 estimate is almost double what the previous
# model had with the lowest sigma^2 value 
# And not all the coefficient values are significant

arima(walmart_ts, order = c(2, 0, 1), seasonal = list(order = c(0, 2, 0), period = 52))
# The AIC is 833.83
# All coefficients significant

arima(walmart_ts, order = c(2, 0, 2), seasonal = list(order = c(0, 2, 0), period = 52))
# The AIC is 832.69
# Not all coefficients significant

arima(walmart_ts, order = c(0, 0, 2), seasonal = list(order = c(0, 2, 0), period = 52))
# The AIC is 834.44
# All coefficients significant


# PART 5
# Final three ARIMA models based on AIC and coefficient significance

arima_A <- arima(walmart_ts, order = c(2, 0, 0), seasonal = list(order = c(0, 1, 0), period = 52))
arima_B <- arima(walmart_ts, order = c(2, 0, 1), seasonal = list(order = c(0, 2, 0), period = 52))
arima_C <- arima(walmart_ts, order = c(0, 0, 2), seasonal = list(order = c(0, 2, 0), period = 52))


# Let's forecast the three models along with the
# Auto-arima, decomposed and differenced data
# Just to make sure a transformation is not necessary
for_decomp <- forecast(decomp)
for_arimaA <- forecast(arima(walmart_ts, order = c(2, 0, 0), seasonal = list(order = c(0, 1, 0), period = 52)))
for_arimaB <- forecast(arima(walmart_ts, order = c(2, 0, 1), seasonal = list(order = c(0, 2, 0), period = 52)))
for_arimaC <- forecast(arima(walmart_ts, order = c(0, 0, 2), seasonal = list(order = c(0, 2, 0), period = 52)))


# Let's plot these models
plot(for_decomp)
plot(for_arimaA)
plot(for_arimaB)
plot(for_arimaC)

# And take one last look at the decomposed data
for_decomp$model
# The AIC is 3124.854, also not a good sign

# Get accuracy of forecasts
accuracy(for_arimaA)
accuracy(for_arimaB)
accuracy(for_arimaC)

# Residual Disgnostics
tsdiag(arima_A)
tsdiag(arima_B)
tsdiag(arima_C)

# Since Model A has the best standardized residuals and least significant lag terms
# We will pursue this model

# PART 7
# Getting the final forecast for the model we will use

arima_final <- arima_A
forecast(arima_final)
plot(forecast(arima_final))

# Now to get to predicting! 
# Let's take the last ten values off
walmart_ts_without_ten <- ts(walmart_ts[-c(134:143)], start = c(2010, 5), frequency = 52)
walmart_ts_without_ten

# And save the last ten
walmart_ts_last_ten <- ts(walmart_ts[-c(1:133)], start = c(2012, 34), frequency = 52)
walmart_ts_last_ten

# Now let's forecast the last 10 values along with 10 future values using sarima.for()
# Because my arima model has a seasonal component it works
sarima.for(walmart_ts_without_ten, n.ahead = 20, 2, 0, 0, P = 0, D = 1, Q = 0, S = 52)
points(walmart_ts_last_ten, col = "black")
lines(walmart_ts_last_ten, col = "black")
legend("topleft", c("Actual Values", "Predicted Values", "Lower/Upper Bounds"), lwd = c(1, 1.5, 2), lty = 1:3, col = c("black", "red", "blue"), inset=0.02)



