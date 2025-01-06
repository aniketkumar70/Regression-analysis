## Experiment-1
# Correlation Analysis using scatter diagram, Karl Pearson's correlation coefficient and drawing inferrence for given data-set.

data1 = iris
library(tidyverse)
ggplot(data1,aes(x = data1$Sepal.Width,y = data1$Sepal.Length)) + 
  facet_wrap(~Species,scales="free_x") +
  geom_point() +
  geom_smooth(formula=y~x,method="lm",se=FALSE)+
  labs(x = "Sepal Width" , y = "Sepal Length" , 
       title = "Sepal Length vs. Sepal Width in iris",
       subtitle = "Grouped by Species")

data2 =mtcars
cor1 = cor.test(data2$wt,data2$mpg,method="pearson")
cor1

data3 = data2[,c(1,3,4,5)]
data3 
cor2 = round(cor(data3,method="pearson"),4)
cor2
library(corrplot)
cor3 = corrplot(cor2,type="lower",order="hclust",tl.col="blue",tl.srt=45)
cor3

library(PerformanceAnalytics)
data4 = data2[,c(1,3,4,5)]
cor4 = chart.Correlation(data4,histogram=TRUE,method="pearson")
cor4

library(ggcorrplot)
cor6 = round(cor(data4),4)
cor6
ggcorrplot(cor6)
ggcorrplot(cor6,hc.order=TRUE,type="lower",lab=TRUE)
cor5 = cor(data4)
cor5
symnum(cor5,abbr.colnames = F)

cor6 = round(cor(data4),4)
cor6

## Excercise

data5 = trees
data5

ggplot(data5,aes(x = data5$Girth,y = data5$Height)) + 
  geom_point() +
  geom_smooth(formula=y~x,method="lm",se=FALSE)+
  labs(x = "Sepal Width" , y = "Sepal Length" , 
       title = "Girth vs Height in trees dataset")

# Interpretation: There is positive correlation between "Girth" and "Height" i.e with increase in "Girth" there is increase in "Height"

data6 = data5[,c(2,3)]
cor7 = chart.Correlation(data6,histogram=TRUE,method="pearson")
cor7

# Interpretation: The pearson's correlation coefficient between "Height" and "Volume" is 0.60 i.e intermediate positive correlation.

data7 = head(data5,5)
cor8 = round(cor(data7,method="pearson"),4)
cor8

cor9 = corrplot(cor8,type="lower",order="hclust",tl.col="blue",tl.srt=45)
cor9

cor10 = round(cor(data7),4)
cor10
ggcorrplot(cor10)
ggcorrplot(cor10,hc.order=TRUE,type="lower",lab=TRUE)



