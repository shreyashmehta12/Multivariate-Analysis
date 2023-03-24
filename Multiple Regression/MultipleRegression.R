library(readr)
library(ggplot2)
library(dplyr)
library(factoextra)
library(corrplot)

data <- read.csv("D:/Rutgers MITA/Spring 2023/MVA/Multiple Regression/Factor-Hair-Revised.csv", header = TRUE, sep = ",")
head(data)
dim(data)
str(data)
names(data)
summary(data)

library(GGally)
data1 <- subset(data, select = -c(1))
summary(data1)

ggpairs(data=data1)

library(corrplot)
datamatrix<-cor(data1)
corrplot(datamatrix, method = "number")
"1. CompRes and DelSpeed are highly correlated
2. OrdBilling and CompRes are highly correlated
3. WartyClaim and TechSupport are highly correlated
4. CompRes and OrdBilling are highly correlated
5. OrdBilling and DelSpeed are highly correlated
6. Ecom and SalesFImage are highly correlated"

model0 = lm(Satisfaction~., data1)
summary(model0)

"
As in our model the adjusted R-squared: 0.7774, meaning that independent variables explain 78% of the variance of the dependent variable, only 3 variables are significant out of 11 independent variables.
The p-value of the F-statistic is less than 0.05(level of Significance), which means our model is significant. This means that, at least, one of the predictor variables is significantly related to the outcome variable.
"
library(MASS)

anova(model0)
coefficients(model0)

confint(model0,level=0.95)
fitted(model0) " fitted() function extracts the predicted values for the data points that were used to fit the model"
residuals(model0) "residuals are the differences between the observed values of the response variable and the predicted values"
vcov(model0)
cov2cor(vcov(model0))
plot(model0)
temp<- influence.measures(model0)
temp
"influence.measures() function to extract various measures of influence and leverage"

library(caTools)
library(car)
vif(model0)
"The vif() function will compute the VIF for each predictor variable in the model, 
and return a vector of VIF values. A VIF value of 1 indicates that there is no correlation between the predictor variable 
and the other predictor variables, while a VIF value greater than 1 indicates that there is some degree of correlation. 
In general, a VIF value greater than 5 or 10 indicates a high degree of correlation, 
and may suggest that the predictor variable should be removed from the model."

qqPlot(model0, main="QQ Plot")

step <- stepAIC(model0, direction="both")
"The stepAIC() function fits a series of nested linear regression models, starting with the null model
(i.e., a model with no predictor variables), and adds or removes one predictor variable at a time, 
based on the direction argument (forward, backward, or both), until the AIC criterion is minimized."
