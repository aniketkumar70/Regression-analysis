# Experiment: Validating Simple linear regression using t, F and p- test.

data("cars")
cars

scatter.smooth(x = cars$speed, 
               y = cars$dist, 
               main = "Dist vs Speed")

# divide graph area in 2 columns
par(mfrow=c(1,2)) 

boxplot(cars$speed,
        main="Speed",
        sub=paste("Outlier rows: ",
                  boxplot.stats(cars$speed)$out))

boxplot(cars$dist,
        main="Distance",
        sub=paste("Outlier rows: ",
                  boxplot.stats(cars$dist)$out))

cor(cars$speed,cars$dist)

linear_Model = lm(dist ~ speed, data=cars)
linear_Model

modelSummary=summary(linear_Model)
modelSummary

modelCoeffs=modelSummary$coefficients
modelCoeffs

n=nrow(cars)
alpha=0.05
beta.estimate=modelCoeffs["speed", "Estimate"]
beta.estimate
std.error=modelCoeffs["speed", "Std. Error"]
std.error

# Classical Approach
t_value=beta.estimate/std.error
t_value
t_table=qt((alpha/2),n-2,lower.tail = F)
t_table

if(t_value>t_table) 
  print("reject the null hypothesis that the co-efficient of the predictor is zero") else 
    print("accept the null hypothesis")

# p-value approach
p_value=2*pt(-abs(t_value),df=nrow(cars)-ncol(cars))
p_value

f=summary(linear_Model)$fstatistic
f
f_table=qf(.95, df1=f[2], df2=f[3])
f_table

confint(linear_Model)


############
# EXERCISE #
############

library(datarium)
data("marketing")
marketing

scatter.smooth(x = marketing$youtube, 
               y = marketing$sales, 
               main = "Youtube Marketing vs Sales")

# divide graph area in 2 columns
par(mfrow=c(1,2)) 

boxplot(marketing$youtube,
        main="Youtube",
        sub=paste("Outlier rows: ",
                  boxplot.stats(marketing$youtube)$out))

boxplot(marketing$sales,
        main="Sales",
        sub=paste("Outlier rows: ",
                  boxplot.stats(marketing$sales)$out))

cor(marketing$youtube, marketing$sales)

linear_Model = lm(sales ~ youtube, data=marketing)
linear_Model

modelSummary=summary(linear_Model)
modelSummary

modelCoeffs=modelSummary$coefficients
modelCoeffs

n=nrow(marketing)
alpha=0.05
beta.estimate=modelCoeffs["youtube", "Estimate"]
beta.estimate
std.error=modelCoeffs["youtube", "Std. Error"]
std.error

# Classical Approach
t_value=beta.estimate/std.error
t_value
t_table=qt((alpha/2),n-2,lower.tail = F)
t_table

if(t_value>t_table) 
  print("reject the null hypothesis that the co-efficient of the predictor is zero") else 
    print("accept the null hypothesis")

# p-value approach
p_value=2*pt(-abs(t_value),df=nrow(cars)-ncol(cars))
p_value

f=summary(linear_Model)$fstatistic
f
f_table=qf(.95, df1=f[2], df2=f[3])
f_table

confint(linear_Model)
