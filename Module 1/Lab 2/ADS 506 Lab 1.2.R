library(tidyverse)
library(fpp2)
set.seed(506)


dgf_vol <- read_csv("Data/DGF Volume.csv", 
                    col_types = cols(Date = col_date(format = "%m/%d/%Y")))



MyTS <- ts(dgf_vol$Volume, start = c(2018, 7), frequency = 12)
MyTS


plot(MyTS)

autoplot(MyTS) +
  labs(title = "DGF Sales Volume Over Time",
       x = "Time",
       y = "Sales volume") +
  theme_minimal()


# create 2 partitions
# training is from July 2018 to December 2021
# validaton is from Jan 2022 to May 2022

train <- window(MyTS, start = c(2018, 7), end = c(2021, 12))

validation <- window(MyTS, start = c(2022, 1))

autoplot(train) +
  autolayer(validation, color = "blue")


# naive model
n_model <- naive(train, h = 5, level = 95)

autoplot(train) +
  autolayer(n_model, color = "#006d2c", series = "Naive") +
  autolayer(validation, color = "blue", series = "Actual")

# seasonal naive
sn_model <- snaive(train, h = 5)

autoplot(train) +
  autolayer(sn_model, color = "#006d2c", series = "Seasonal Naive") +
  autolayer(validation, color = "blue", series = "Actual")



# season & trend
st_model <- tslm(train ~ trend + season)

summary(st_model)

st_forecast <- forecast(st_model, h = 5)

autoplot(train) +
  autolayer(st_forecast, color = "#006d2c", series = "season trend") +
  autolayer(validation, color = "blue", series = "Actual")



accuracy(st_forecast)
accuracy(n_model)
accuracy(sn_model)


mean(abs(n_model$residuals), na.rm = T)

sqrt(mean(n_model$residuals^2, na.rm = T))





















