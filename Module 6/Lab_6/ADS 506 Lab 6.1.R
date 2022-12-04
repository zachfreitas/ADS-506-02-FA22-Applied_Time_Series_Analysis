library(dplyr)
library(fpp2)
library(readr)
library(caret)


er_arrivals <- read_csv('Lab_6/ER Arrivals.csv', 
                        col_types = cols(Date = col_datetime(format = '%m/%d/%Y %H:%M')))

head(er_arrivals)




arrivals <- ts(er_arrivals$Arrivals, frequency = 24)
arrivals



autoplot(arrivals)

train <- window(arrivals, end = c(16, 24))
test <- window(arrivals, start = c(17, 1))

pacf(train, lag.max = 72)
acf(train, lag.max = 72)

# we need to take at least seasonal difference

pacf(diff(diff(train, lag = 24)), lag.max = 72)
acf(diff(diff(train, lag = 24)), lag.max = 72)

my_model <- arima(train, order = c( 3, 1 ,1 ), seasonal = c(1, 1, 1))
my_model

sqrt(mean(my_model$residuals^2))

er.arima.pred <- forecast(my_model, h = 24)
accuracy(er.arima.pred, test)



# Neural networks

# non-seasonal lags
# ar(p)
# p = 3

# seasonal Lags
# P = 1

# size = (p + P + 1)/2  rounded to nearest integer
# 3 + 1 + 1 = 5 / 2 = 3
# This is a heuristic

er.nnetar <- nnetar(train, p = 3, P = 1, size = 3)
summary(er.nnetar)

er.nnetar.pred <- forecast(er.nnetar, h = 24)
er.nnetar.pred

accuracy(er.arima.pred, test)
accuracy(er.nnetar.pred, test)


autoplot(arrivals, series = "Actual") +
  coord_cartesian(xlim = c(15, 18)) +
  autolayer(er.nnetar$fitted, series = "NN") +
  autolayer(er.nnetar.pred, series = "NN Prediction") +
  autolayer(er.arima.pred, series = "ARIMA", PI = F) +
  guides(color = guide_legend(title = "Series")) +
  scale_color_manual(values = c(Actual = "black", NN = "red", 
                                `NN Prediction` = "green",
                                ARIMA = "purple")) +
  theme_classic() +
  theme(legend.position = "top") 






library(quantmod)

# pfizer

getSymbols("PFE", from = "2022-01-01", to = "2022-7-31")

PFE

pfizer <- PFE$PFE.Close

p.train <- window(pfizer, end = "2022-06-30")
p.test <- window(pfizer, start = "2022-07-01")

head(p.test)


p.nnetar <- nnetar(p.train)
summary(p.nnetar)

p.nnetar.pred <- forecast(p.nnetar, h = 20)
p.nnetar.pred

autoplot(ts(PFE$PFE.Close), series = "Pfizer") +
  autolayer(p.nnetar$fitted, series = "fitted") +
  autolayer(p.nnetar.pred, series = "predicted") +
  guides(color = guide_legend(title = "Series")) +
  scale_color_manual(values = c(Pfizer = "black", 
                                fitted = "red", 
                                predicted = "green")) +
  theme_classic() +
  theme(legend.position = "top") +
  coord_cartesian(xlim = c(100, 150))


accuracy(p.nnetar.pred, p.test)


































