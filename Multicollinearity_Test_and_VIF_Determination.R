## experiment : problem of multi-collinearity and determination of Varinace Inflation Factor (VIF)
# AIM : to perform the test pertaining to multi-collinearity and determination of VIF for given dataset.

## Using Correlation
library(tidyverse)
library(corrplot)
library(olsrr)
mydata <- mtcars %>% select(mpg,cyl,disp,hp,wt)
head(mydata,5)

model <-lm(mpg ~. , data = mydata)
summary(model)

# ploting correlation matrix
corrplot(cor(mydata), method = "number")
## here cyl and disp indicates multi-collinearity == 0.90

## Using Varinace Inflation Factor (VIF)
# measures inflation in coefficient of independaent variable duw to collinearity among other independent variable. VIF == 1 , regression is not inflated. VIF == 5, requires further investigation.VIF == 10 , indicate multi-collinearity

ols_vif_tol(model)

## Using EigenValues and Condition Index
# 15 <= CI <= 30 : multi-collinearity 

ols_eigen_cindex(model)

## we conclude there is multicollinearity btw cyl and disp
