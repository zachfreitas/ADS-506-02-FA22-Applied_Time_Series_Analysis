library(dplyr)
library(fpp2)
library(readr)




# remove trends and/or seasonality
# easiest way is use differencing

# Differencing is subtracting 2 values.  
#In Time Series we take the difference between 2 consecutive values.

# a first difference looks like Yt - Yt-1

# if we wanted to remove seasonality then we would take k difference
# Yt - Yt-k


er_arrivals <- read_csv('Lab 3/ER Arrivals.csv', 
                        col_types = cols(Date = col_datetime(format = '%m/%d/%Y %H:%M')))

head(er_arrivals)

# Convert to time Series Object
arrivals <- ts(er_arrivals$Arrivals)

# View the data
autoplot(arrivals)

# Remove Seasonality with differencing. 
# 24 Hours
diff.once <- diff(arrivals, lag = 24)

# Double Differencing or Second Order Difference Example.

# Plot the deseasonalized data
autoplot(diff.once)

# Double Differencing or Second Order Difference Example.

diff.double <- diff(diff(arrivals, lag = 24), lag = 1)

# Plot the deseasonalized data
autoplot(diff.double)

# Differencing - Detrending it, then lag = 1
# Differencing - Deseasonalized it, then lag = the seasonal period.



# Test Train Partitioning
arrival.diff.train <- window(diff.once, end = 384)
arrival.diff.test <- window(diff.once, start = 385)

# Create Modeling and Forecast.
# Use Exponential Smoothing
?ets
# 'ANN"Error Additive, no trend, no season
arrival.diff.model <- ets(arrival.diff.train, model = 'ANN', alpha = .1) 
arrival.diff.pred <- forecast(arrival.diff.model, h = 24)

accuracy(arrival.diff.model)
accuracy(arrival.diff.pred)

# Plot the model forecast.
autoplot(arrival.diff.train, series = 'Training') +
  autolayer(arrival.diff.test, series = 'Actual') +
  autolayer(arrival.diff.pred, series = 'Prediction', alpha = .4) +
  theme_classic() +
  coord_cartesian(xlim = c(375, 415))


# to convert back to original scale
arrival.diff.pred.og <- ts(arrivals[384] + cumsum(arrival.diff.pred$mean), start = 385)

autoplot(arrivals) +
  autolayer(arrival.diff.pred.og, color = 'red') +
  theme_classic() +
  coord_cartesian(xlim = c(375, 415))



##########################################################################
##########################################################################
# regression
##########################################################################


arrivals <- ts(er_arrivals$Arrivals, frequency = 24)

arrival.lm.train <- window(arrivals, end = c(16, 24))
arrival.lm.test <- window(arrivals, start = c(17, 1))

arrival.lm.model <- tslm(arrival.lm.train ~ season, arrival.lm.train)

summary(arrival.lm.model)


arrival.lm.pred <- forecast(arrival.lm.model, h = 24)

autoplot(arrival.lm.train, series = 'train') +
  autolayer(arrival.lm.test, series = 'actual') +
  autolayer(arrival.lm.pred, series = 'prediction', alpha = .4) +
  theme_classic() +
  coord_cartesian(xlim = c(16, 18))





# Another Example

er_arrivals <- read_csv('Lab 3/ER Arrivals.csv', 
                        col_types = cols(Date = col_datetime(format = '%m/%d/%Y %H:%M')))


arrivals.dum <- er_arrivals %>%
  mutate(Day = factor(format(Date, '%a'),
                      levels = c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'))) %>%
  select(-Date) %>%
  ts(. , frequency = 24)


arrivals.dum.train <- window(arrivals.dum, end = c(16, 24))
arrivals.dum.test <- window(arrivals.dum, start = c(17, 1))

arrivals.dum.lm <- tslm(Arrivals ~ Day + season, arrivals.dum.train)
summary(arrivals.dum.lm)


nextday <- data.frame(Day = rep(3, 24))


arrivals.dum.pred <- forecast(arrivals.dum.lm, 
                              newdata = nextday)


?autoplot

autoplot(arrivals.dum.train, series = 'Training') +
  autolayer(arrivals.dum.test, series = 'Test') +
  autolayer(arrivals.dum.pred, series = 'Prediction', alpha = .4) +
  theme_classic() +
  coord_cartesian(xlim = c(16, 18))
























