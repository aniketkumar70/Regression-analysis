## experiment  : Develop confidence interval and testing the simple and multiple linear regression

library(ggplot2)

data(cars)
head(cars,10)

# fitting linear model
lin_mod <- lm(dist ~ speed , data = cars)
lin_mod

#prediction for new data-set
new_speed <- data.frame(speed=c(13,20,25,10))
new_speed

# predicting values for new speed
predict(lin_mod , newdata = new_speed)


## Confidence interval
predict(lin_mod , newdata = new_speed,interval = "confidence")

## Prediction interval
predict(lin_mod , newdata = new_speed,interval = "prediction")


## Visualizing Confidence and Prediction interval Bands
pred_int <- predict(lin_mod , interval = "prediction")
mydata <- cbind(cars,pred_int)
mydata

p <- ggplot(mydata,aes(speed,dist))+
  geom_point()+
  stat_smooth(method = lm)

## Adding prediction intervals
 p + geom_line(aes(y=lwr),color = "red" , linetype = "dashed")+geom_line(aes(y=upr),color = "red" , linetype = "dashed")
 

## Confidence and Prediction Interval for Multiple Linear regression
data("stackloss")
head(stackloss,5)


#Multiple Linear Regression
Mlin_mod <- lm(stack.loss ~ . , data = stackloss) 
Mlin_mod

## New data for stackloss
new_data <- data.frame(Air.Flow = c(72,82,69,77),
                       Water.Temp = c(20,25,18,22),
                       Acid.Conc. = c(85,90,88,92))
new_data

## Prediction on the new data 
predict(Mlin_mod, newdata = new_data , interval = "confidence")
predict(Mlin_mod, newdata = new_data , interval = "prediction")


# • Install and load the package "datarium".
# • Import marketing data set from datarium package.
# • Fit a linear model for sales on impact of the amount of money spent on advertising media YouTube.
# • Find the confidence and Prediction interval using arbitrary values for the response variable and visualize the confidence and prediction interval bands.
# • Fit a multiple linear model for sales on impact of the amount of money spent on advertising medias YouTube and Facebook. Also 
