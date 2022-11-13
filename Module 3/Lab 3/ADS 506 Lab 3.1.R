library(fpp2)
library(zoo)
library(readr)

set.seed(506)

# Load Data
AustralianWines <- read_csv("Lab 3/AustralianWines.csv")

# Create Time Series Object
reds <- ts(AustralianWines$Red, start = c(1980, 1), frequency = 12)

# Plot this
autoplot(reds)

# Use this to see seasonality
# Autocorrelation Function
acf(reds, lag.max = 48)


# moving average
# Help Menu
?rollmean

# Create rolling moving average.
reds.ma <- rollmean(reds, k = 4, align = "right")

reds.ma

autoplot(reds, series = "Actual") +
  autolayer(reds.ma, series = "MovingAvg") +
  theme_classic()



#################################

library(quantmod)
getSymbols("AAPl", from = "2015-1-1", to = "2015-12-31")

AAPL

# Get the Closing Price and make into a time series object
apple <- ts(AAPL$AAPL.Close)

# Training and test partitions
apple.train <- window(apple, end = 241)
apple.test <- window(apple, start = 242)

# or 

# apple.train <- window(apple, end = "2015-12-15")
# apple.test <- window(apple, start = "2015-12-16")


# Count the number of observations
length(apple.test)

# Exponential Smoothing Model
apple.model <- ses(apple.train, alpha = .97, level = c(.95))
apple.pred <- forecast(apple.model, h = 10)

# Show Forecast
autoplot(apple, series = "actual") +
  autolayer(apple.pred, series = "predicted", alpha = .4) +
  theme_classic() +
  coord_cartesian(xlim = c(210, 251))

# Next Period Perdicitons
autoplot(apple.train, series = "Training") +
  autolayer(apple.model$fitted, series = "Model") +
  theme_classic() +
  coord_cartesian(xlim = c(201, 241))


# choose alpha with the lowest RMSE

summary(apple.model)

x <- summary( ses(apple.train, alpha = .97, level = c(.95)) )
x$model$fit$value


# Larger Alpha suggest yesterday's value is more important than past values.

#.8 rmse = 0.4996302 
#.5 rmse = 0.5457429 
#.9 rmse = 0.4947192 
#.97 rmse = 0.493726035453134

# Accuracy Improvement.
(0.493726035453134 - 0.5457429)/0.5457429

# Loop to see Optimal Setting.
sequence <- seq(0.01,0.99,0.01)
for (val in sequence){
  x <- summary( ses(apple.train, alpha = val, level = c(.95)) )
  rmse <- (mean(x$residuals^2)^0.5)
  print(cat(paste("Alpha:", val,"\nRMSE", rmse)))
  }














