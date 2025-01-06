## Experiment : Diagnostic measures and outliers detection, Durbin Watson test , variable selection and model building

## Aim : to perform Diagnostic measures and outliers detection, Durbin Watson test , variable selection and model building
library(lmtest)

data <- ggplot2::mpg

head(data,5)


nrow(data)
ncol(data)
hwy <- data$hwy
summary(hwy)

# Distribution of hwy (target variable)
hist(hwy, xlab = 'hwy', main = "histogram of hwy", breaks =sqrt(nrow(data)) )

# Getting outliers 
out <- boxplot.stats(data$hwy)$out
out_ind <- which(data$hwy %in% c(out))
out_ind

# removing outliers
boxplot(data$hwy , ylab = "hwy")
mtext(paste("outliers", paste(out,collapse = ",")))

## Durbin Watson Test
# Ho : no correlation between residuals
# H1 : residuals are autocorrelated

data("mtcars")
head(mtcars,5)

model <- lm(mpg ~ disp+ wt , data = mtcars)
summary(model)

dwtest(model)
## there is auto-correlation between residuals


## Variable selection and model building
## All possible regression -> 2^k

model2 <- lm(mpg ~ disp+hp+wt+qsec , data = mtcars)
summary(model2)

k1 <- ols_step_all_possible(model2)
plot(k1)

k1$result



## Best subset regression
# Large R2 value and smalllest MSE and MAlloc's cp or AIC

model <- lm(mpg ~ disp+ hp+wt+qsec, data = mtcars)
summary(model)
k2 <- ols_step_best_subset(model)
k2

plot(k2)

## StepWise Forword Regression
k3 <- ols_step_forward_p(model , details = T)
plot(k3)

## Stepwise backward regression
k4 <- ols_step_backward_p(model , details = T)
k4
plot(k4)


## Mixed Selection regression
k5 <- ols_step_both_p(model , details = T)
k5
plot(k5)


## Stepwise AIC forword Regression
k6 <- ols_step_forward_aic(model , details = T)
k6
plot(k6)


## Stepwise AIC backword Regression
k7 <- ols_step_backward_aic(model , details = T)
k7
plot(k7)

## AIC Mixed Selection regression
k8 <- ols_step_both_aic(model , details = T)
k8
plot(k8)

