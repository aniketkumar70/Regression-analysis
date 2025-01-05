#Forecast Accuracy for a Given Data Set

library(forecast)

data("AirPassengers")

class(AirPassengers)

frequency(AirPassengers)

series <- AirPassengers

head(series,10)

plot(series, col ="darkblue",ylab = "passegner on airplane")

boxplot(split(series,cycle(series)),names = month.abb,col = "gold")

# training set 
# use data from 1949 to 1956 for forecasting 
train_set = window(series,start = 1949 , end =c(1956,12))
train_set

# test set 
# use remaining data from 1957 to 1960 to test accuracy 
test_set = window(series, start = 1957,end = c(1960,12))
test_set

plot(train_set,main="AirPassengers" ,ylab = "" , xlab = "Months")

# plot forecasting for 4 years according to four methods
lines(meanf(train_set,h=48)$mean, col = 4)
lines(rwf(train_set,h=48)$mean,col =2)
lines(rwf(train_set,drift = T , h=48)$mean,col =3)
lines(snaive(train_set , h=48)$mean,col =5)

legend("topleft",lty = 1, col = c(4,2,3,5),
       legend = c("Mean_method", "Naive method","Drift method","Seasonal naive method"),bty = "n")


# test set
lines(test_set , col = "red")

# accuracy for forecasting of train_set (forecasted data) on test_set (original data used as test set) 
# the best model had the lowest error (particularly the MAPE, Mean absolute percentage error) 

# mean method 
accuracy(meanf(train_set,h=48),test_set)

#naive method
accuracy(rwf(train_set,h=48),test_set)

#drift method 
accuracy(rwf(train_set,drift = T,h=48),test_set)

#Seasonal naive method
accuracy(snaive(train_set,h=48),test_set)

# plot test set only with the predictions 
# calculate the forecasting 

train.set.mean = meanf(train_set , h = 48)$mean
train.set.naive = rwf(train_set , h = 48)$mean
train.set.drift = rwf(train_set ,drift = T, h = 48)$mean
train.set.snaive = snaive(train_set , h = 48)$mean

# plot the test set 
plot(test_set , main = "AirPassengers" , ylab = "", xlab = "Months", ylim = c(200,600))

# plot forecasting for 4 years according to four methods
lines(train.set.mean,col = 4)
lines(train.set.naive,col = 2)
lines(train.set.drift,col = 3)
lines(train.set.snaive,col = 5)
legend("topleft", lty = 1, col =c(4,2,3,5), legend = c("Mean method","Naive 
method","Drift method", "Seasonal naïve method"),bty = "n")
