library(tidyverse)
library(fpp2) # Plot and Forecast Data
set.seed(506)


# Get Data
dgf_vol <- read_csv("Lab 2/DGF Volume.csv", 
                    col_types = cols(Date = col_date(format = "%m/%d/%Y")))



MyTS <- ts(dgf_vol$Volume, start = c(2018, 7), frequency = 12)
MyTS


plot(MyTS)

# GGplot - Time Serries Plot
autoplot(MyTS) +
  labs(title = "DGF Sales Volume Over Time",
       x = "Time",
       y = "Sales volume") +
  theme_minimal()

#############################################################
# create 2 partitions
# training is from July 2018 to December 2021
# validation is from Jan 2022 to May 2022

train <- window(MyTS, start = c(2018, 7), end = c(2021, 12))
train
validation <- window(MyTS, start = c(2022, 1))
validation

# Plot the Training and Validation Set
autoplot(train) +
  labs(title = "DGF Sales Volume Over Time",
       x = "Time",
       y = "Sales volume") +
  theme_minimal() +
  autolayer(validation, color = "blue")


# naive model -
# This takes the very last value of the training set.
n_model <- naive(train, h = 5, level = 95)


# Plot the Training Model with the Naive Model.
autoplot(train) +
  labs(title = "DGF Sales Volume Over Time",
       x = "Time",
       y = "Sales volume") +
  theme_minimal() +
  autolayer(n_model, color = "#006d2c", series = "Naive") +
  autolayer(validation, color = "blue", series = "Actual")


##################
# Model: seasonal naive -
# This takes a seasonal naive value.
sn_model <- snaive(train, h = 5)

autoplot(train) +
  labs(title = "DGF Sales Volume Over Time",
       x = "Time",
       y = "Sales volume") +
  theme_minimal() +
  autolayer(sn_model, color = "#006d2c", series = "Seasonal Naive") +
  autolayer(validation, color = "blue", series = "Actual")


##################
# Model season & trend
st_model <- tslm(train ~ trend + season)

summary(st_model)

st_forecast <- forecast(st_model, h = 5)

autoplot(train) +
  labs(title = "DGF Sales Volume Over Time",
       x = "Time",
       y = "Sales volume") +
  theme_minimal() +
  autolayer(st_forecast, color = "#006d2c", series = "season trend") +
  autolayer(validation, color = "blue", series = "Actual")


###############################
# Measure the errors

accuracy(st_forecast)
accuracy(n_model)
accuracy(sn_model)


#MAE
mean(abs(n_model$residuals), na.rm = T)

# RMSE
sqrt(mean(n_model$residuals^2, na.rm = T))





















