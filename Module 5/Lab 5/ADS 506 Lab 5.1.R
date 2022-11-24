library(dplyr)
library(fpp2)
library(readr)
library(caret) # For the confusion Matrix


er_arrivals <- read_csv('Lab 5/ER Arrivals.csv', 
                        col_types = cols(Date = col_datetime(format = '%m/%d/%Y %H:%M')))

head(er_arrivals)

arrivals <- ts(er_arrivals$Arrivals, frequency = 24)

arrivals

autoplot(arrivals)

train <- window(arrivals, end = c(16, 24))
test <- window(arrivals, start = c(17, 1))

auto_model <- auto.arima(train)

# Partial Auto Correlations - First Number
pacf(train, lag.max = 72)

# Auto Correlations - Third Number
acf(train, lag.max = 72)

# we need to take at least seasonal difference

pacf(diff(diff(train), lag = 24), lag.max = 72)
acf(diff(diff(train), lag = 24), lag.max = 72)

pacf(diff(diff(train, lag = 24)), lag.max = 72)
acf(diff(diff(train, lag = 24)), lag.max = 72)

my_model <- arima(train, order = c( 3, 1 ,1 ), seasonal = c(1, 1, 1))
my_model


sqrt(mean(my_model$residuals^2))
sqrt(mean(auto_model$residuals^2))

accuracy(forecast(my_model, h = 24), test)
accuracy(forecast(auto_model, h = 24), test)


# will the 17th day have above average visits per hour? Y/N
# let get the avg visit per hour

my_mean <- er_arrivals %>%
  filter(Date < as.Date('2015-11-17')) %>%
  mutate(hour = format(Date, '%H')) %>%
  group_by(hour) %>%
  summarise(mean_visits = round(mean(Arrivals), 0))
  
  
new_set <- er_arrivals %>%
  select(Arrivals) %>%
  cbind(., my_mean) %>%
  mutate(above_mean = ifelse(Arrivals > mean_visits, 1, 0)) %>%
  select(Arrivals, above_mean)


head(new_set, 24)


# look at ARIMA model
my_model

new_set <- new_set %>%
  mutate(s.lag = lag(Arrivals, 24),
         lag1 = lag(Arrivals, 1),
         lag2 = lag(Arrivals, 2),
         lag3 = lag(Arrivals, 3)) %>%
  na.omit()


length(new_set$Arrivals)

new_train <- new_set[1:360, ]
new_test <- new_set[361:384, ]

er_aboveAvg <- glm(above_mean ~ Arrivals + s.lag + lag1 + lag2 + lag3, new_train,
                   family = "binomial")

er_pred <- predict(er_aboveAvg, newdata = new_test, type = "response")
er_pred


confusionMatrix(as.factor(round(er_pred,0)), as.factor(new_test$above_mean))

cbind(new_test, er_pred)

confusionMatrix(as.factor(round(er_pred+.05,0)), as.factor(new_test$above_mean))





