### Experiment : MLR : estimation of parameter , fitting of the model , error analysis , model validation , variable selection and testing

library(MASS)

data("mtcars")
head(mtcars,5)
# variable names
names(mtcars)


## fitting model
model <- lm(mpg ~ . ,data = mtcars)
model

# Summary of the model
summary(model)

# confidence Interval
confint(model, level = 0.95)

# Hypothesis Testing
# for Disp
teststat <- coef(summary(model))[3,1]/coef(summary(model))[3,2]
teststat

#### p_value
2*pt(teststat , 21 , lower.tail = F)

# Test for intercept
teststat1 <- coef(summary(model))[1,1]/coef(summary(model))[1,2]
teststat1

#### p_value
2*pt(teststat1 , 21 , lower.tail = F)

## Model Assumptions
residual <- model$residuals
data.frame(residual)
hist(residual)
qqnorm(residual)
qqline(residual)
residual

## Homoscedasticity
plot(model$residuals~mtcars$disp)
abline(0,0)

# Residual Analysis
plot(model)


## Model Transformation

model1 <- lm(mpg ~ cyl +log(disp) + log(hp) + drat + wt + qsec + vs + am + gear + carb , data = mtcars)
summary(model1)

## Model Assumptions
residual <- model1$residuals
data.frame(residual)
hist(residual)
qqnorm(residual)
qqline(residual)
residual

## Homoscedasticity
plot(model1$residuals~mtcars$disp)
abline(0,0)

# Residual Analysis
plot(model1)


## reducing the Model ( AIC) for model

stepAIC(model)


# stepAIC model as final

model2 <- lm(formula = mpg ~ wt + qsec + am, data = mtcars)
summary(model2)


## reducing the Model ( AIC) for model1
stepAIC(model1)

model3 <- lm(formula = mpg ~ log(disp) + gear + carb, data = mtcars)
summary(model3)


# interaction model 
model4 <- lm(mpg~ qsec + wt*am , data = mtcars)
summary(model4)

AIC(model4)
stepAIC(model4)
