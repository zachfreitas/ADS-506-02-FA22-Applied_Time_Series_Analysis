library(fpp2)
library(zoo)
library(readr)
library(dplyr)

set.seed(506)

SeoulBikeData <- read_csv("Lab 4/SeoulBikeData.csv", col_types = cols(Date = col_date(format = "%d|%m|%Y")))

head(SeoulBikeData)


CanadianWorkHours <- read_csv("Data/CanadianWorkHours.csv")

head(CanadianWorkHours)



bikes <- ts(SeoulBikeData$BikesRented, frequency = 7)

# visualize the data to see if there is seasonality, trending or cycles.
autoplot(bikes)

# Take a look at the ACF to see Autocorrelations
acf(bikes)




# ACF plot determines ma()
# pacf plot determines ar()

# take a first order difference to make the data stationary.
# to determine the ar() part of the ARIMA model p
acf(diff(bikes))

# to determine the ma()
pacf(diff(bikes))





# our arima model
my_arima <- arima(bikes, order = c(1, 1, 3))
summary(my_arima)


auto_model <- auto.arima(bikes)
summary(auto_model)


# yt = b0 + b1xt + b2x2t + b3x3t

# yt = b0 + b1yt-2 + b2yt-2+ b3yt-3+ ....

# our model 
# yt = b0 + b1yt-1 + Oeyt-1 + Oeyt-2 + Oeyt-3


# what about external predictors
head(SeoulBikeData)

training_set <- SeoulBikeData[1:335, ] %>% select(-Date)

test_set <- SeoulBikeData[336:365, ] %>% select(-Date)


outcome_v <- training_set$BikesRented
predictors <- as.matrix(training_set[, 1:4])

# auto arima function
auto_arima_reg_model <- auto.arima(outcome_v, xreg = predictors)
summary(auto_arima_reg_model)


# RMSE
sqrt(mean(auto_arima_reg_model$residuals^2))


my_arima_reg_model <- Arima(outcome_v, order = c(1,1,3), xreg = predictors)
summary(my_arima_reg_model)


# RMSE
sqrt(mean(my_arima_reg_model$residuals^2))


# forecasting

my_predictors <- as.matrix(test_set[, 1:4])

my_forecast <- forecast(auto_arima_reg_model, xreg = my_predictors)

my_forecast

autoplot(ts(SeoulBikeData$BikesRented), color = 'red') +
  autolayer(my_forecast, alpha = .3) +
  coord_cartesian(xlim = c(300, 370))








































