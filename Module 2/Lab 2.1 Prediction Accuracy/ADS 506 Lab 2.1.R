library(tidyverse)
library(fpp2)

set.seed(506)

er_arrivals <- read_csv("Lab 2.1 Prediction Accuracy/Data/ER Arrivals.csv", 
                        col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
View(er_arrivals)



arrivals <- ts(er_arrivals$Arrivals, frequency = 24)
arrivals

# let's plot this
autoplot(arrivals) +
  labs(title = "Time Series Plot of ER Arrivals",
       x = "Time",
       y = " Arrivals")


mean_arr <- mean(er_arrivals$Arrivals)

# plot the mean
autoplot(arrivals) +
  labs(title = "Time Series Plot of ER Arrivals",
       x = "Time",
       y = " Arrivals") +
  geom_hline(yintercept = mean_arr, color = 'green', size = 1) +
  theme_classic()


# Create Test and Train 

train <- window(arrivals, end = c(16, 24)) # End on 16th day and 24th hour.
test <- window(arrivals, start = c(17,1))# Start on 17th day and 1st hour.


# models
mean24 <- meanf(train, h = 24)
naive24 <- rwf(train, h = 24)
snaive24 <- snaive(train, h = 24) # Seasonal Naive
helpme <- forecast(train, h = 24) # Default is Exponential Smoothing


# Eliminates Bias and Over-fitting.

Combination <- (snaive24[["mean"]] + 
                  helpme[["mean"]])/2

WeightedCombination <- (naive24[["mean"]] * 0.05 +
                  snaive24[["mean"]] *0.2 + 
                  helpme[["mean"]] * 0.75)


autoplot(train) + 
  autolayer(test, color = "black", linetype="dashed") +
  autolayer(helpme, color = "red", PI = F) +
  autolayer(mean24, color = "purple", PI = F) +
  autolayer(snaive24, color = "green", PI = F) +
  autolayer(Combination, color = "blue") +
  autolayer(WeightedCombination, color = "black") +
  labs(title = "Time Series Plot of ER Arrivals",
       x = "Time",
       y = " Arrivals") +
  coord_cartesian(xlim = c(15, 18))



# test

accuracy(mean24, test)
accuracy(naive24, test)
accuracy(snaive24, test)
accuracy(helpme, test)
accuracy(Combination, test)
accuracy(WeightedCombination, test)



