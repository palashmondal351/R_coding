# LINEAR REGRESSION AND ANOVA
# In this cahapter we will discuss on linear regression. In statistics, modeling is where we get
# down to businessm Models quantnify the relationships between our variables. Modls let us make 
#predictions. A simple linear regression is the basic model. Our mission is to fit the model. 

#########      *********************************************************     #########
#IS THE MODEL STATISTICALLY SIGNIFICANT?
#check the F statistic at the bottom of the summary

#ARE THE COEFFICIENT SIGNIFICANT?
#check t-statistic and p-values in teh summary or check their confidence intervals. 

#IS THE MODEL USEFUL?
#check the R2 value in the sumary

#DOES THE MODEL FIT THE DATA WELL?
#plot the resjiduals and check the regression diagnostics.

#DEST THE DAT SATISFY THE ASSUMPTIONS BEHHIND LINEAR REGRESSION?
#check whether the diagnostics confirm that a linear model is reasonable for your data.

#*****************************************************************************#

#PERFORMING SIMPLE LINEAR REGRESSION
#Y= b0 + b1(i)*X + e;  y = beta 0 + beta i * X  + zeta
# beta 0: intercept; beta i: coefficient
lm(cars$speed ~  cars$dist) # ~ : tield

#11.2 PERFORMING MULTIPLE LINEAR REGRESSIOIN
lm(attitude$rating ~ attitude$complaints + attitude$privileges + attitude$learning)
lm(rating ~ complaints + privileges + learning + critical + advance, data = attitude)

#11.3 GETTING REGRESSION STATISTICS
m=lm(rating ~ complaints + privileges + learning + critical + advance, data = attitude)
anova(m) #anova table
coefficients(m) #model coefficient
coef(m) # same as coefficient
confint(m) # confidence intervals for the regression coefficient
deviance(m) # residual sum of squars
effects(m) #vector of othogonal effects
fitted(m) #vector of fitted y values 
residuals(m) #model residuals
resid(m) # same as residuals (call as RSS)
summary(m) #key statistics,R2, the F-Statistic (Most Important), residuals standard error. 
vcov(m) #variance-convariance matrix of the main parameters. 

#11.4 UNDERSTANDING THE REGRESSION SUMMARY
# In regression summary most important things is F-statistic
#In summary coefficient Pr(>|t|) is known as p value is the probablity. It gueses the likehood
#that the coefficient is not significance, so smaller is better. Big is bad because it indicates
# likehood of insignificance. 
# conventional p value is less than 0.05 is good. 

# R2 calculate the model quality, bigger is better. Use the adjust R2 rather than basic R2.
# F-statistic tells you whether the model is significant or insignificant. 
# Most people look at the R2 statistic first, The statistician wisely starts with the F
# statistic, if the model is not significant then nothing else matters. 

#11.5 PERFORMING LINEAR REGRESSION WITHOUT AN INTERCEPT
lm(cars$speed ~ cars$dist + 0)
confint(lm(cars$speed ~ cars$dist + 0))

#11.6 PERFORMING LINEAR REGRESSION WITH INTERACTION TERMS. 
# in regressin, an interaction occurs when the product of two predictor variablse is also
# a significant predictor. 
m=lm(rating ~ complaints + privileges + complaints*learning + critical*advance, data = attitude)
summary(m)

#11.7 SELECTING THE BEST REGRESSION VARIABLES
full.model=lm(rating ~ complaints + privileges + complaints*learning, data=attitude)
summary(full.model)
reduce.model=step(full.model, direction = 'backward')   # 'forward' for forward selection. 
# if you have too many candidate variable then use forward stepwise selection. 
# real world problem work with boh forward and backward and took decision on that. 
summary(reduce.model)

#11.8 REGRESSION ON A SUBSET OF YOUR DATA.
lm(rating ~ complaints + privileges, data=attitude, subset = 1:floor(length(rating)/2))

#11.9 USING AN EXPRESSION INSIDE A REGRESSION FORMULA. 
fmodel= lm(rating ~ complaints + I(privileges^2) + complaints*learning, data=attitude)
summary(fmodel)

#11.10 REGRESSION ON POLYNOMIAL. 
fmodel= lm(rating ~ poly(complaints, 3), subset = 1:25, data=attitude) # complaints of degree 3
fmodel2=lm(rating ~ complaints + I(complaints^2)+ I(complaints^3), data=attitude )
summary(fmodel)
summary(fmodel2)

#11.11 REGRESSION ON TRANSFORM DATA
# if the data are not in linear relationship. 
fmodel=lm(log(rating) ~ complaints, data=attitude )
fmodel2=lm(sqrt(rating) ~ complaints, data=attitude )
fmodel3=lm(log(rating) ~ log(complaints), data=attitude )
summary(fmodel)
summary(fmodel2)
summary(fmodel3)

#11.12 FINDING THE BEST POWER TRANSFORMATION (BOX-COX PROCEDURE)
# to find the best power(lambda) of response variable.
library(MASS)
X=1:100
eps=rnorm(length(X), sd=5)
Y=(x+eps)^(-1/1.5)
m=lm(Y~X)
summary(m)
plot(m,which=1) # Plot a graph on fitted vs residual.
bc=boxcox(m)
which.max(bc$y) # maximum possible lambda value
lambda=bc$x[which.max(bc$y)] # position of corrosponding lambda value
lambda

# 11.13 FORMATING CONFIDENCE INTERVALS FOR REGRESSION COEFFIECIENTS.
m=lm(cars$speed ~ cars$dist)
confint(m)
confint(m, level = 0.99) # overwrite the confidence interval from 95% to 99% 

#11.14 PLOTTING REGRESSION RESIDUALS
# you want to display of yours regression residuals
m=lm(cars$speed ~ cars$dist)
plot(m, which = 1) #residuals vs fitted plot
plot(m, which = 2) # standard residuals (Normal Q-Q) plot
plot(m, which = 3) #under root standard residuals (scale vs localization) plot
plot(m, which = 4) #obj. number vs cook's distance plot
plot(m, which = 5) #residuals vs leverage plot
plot(m, which = 6) #cook's dist. vs. Leverage plot


#11.15 DIAGNOSING A LINEAR REGRESSION
#m=lm(cars$speed ~ cars$dist)
#plot(m)
#library(car)
#outlierTest(m)

#11.16 IDENTIFYING THE INFLUENTIAL OBSERVATIONS
# you want to indetify the observations that are having the most influence on the
#regression model. if we deleted an infulential observation then observation would 
#significantly change the fitted regression model.
m=lm(airquality$Temp ~ airquality$Wind)
influence.measures(m) # will found the influented observation with asterics(*)

#11.17 TESTING RESIDUALS FOR AUTOCORRELATION(DURBIIN-WATSON TEST)
#SPECIAL NOTE: call a library is harmful in RScript, if the library is call before then use
#detach(package:packagename) to detach the package 
detach(package:lmtest)
library(lmtest)
m=lm(airquality$Temp ~ airquality$Wind)
dwtest(m)

#11.18 PREDICTING NEW VALUES
# you want to predict new values from your regression model
m=lm(airquality$Temp ~ airquality$Wind + airquality$Month + airquality$Day)
preds=data.frame(u=3.1, v=4.0, w=5.5)
predict(m, newdata=preds)

#11.19 FORMATING PREDICTION INTERVALS
# If you want to know the distribution of prediction interval
m=lm(airquality$Temp ~ airquality$Wind)
preds=data.frame(u=3.1)
predict(m, newdata=preds, interval = "prediction")

#11.20 PERFORMING ONE-WAY ANOVA
#your data is divided into groups, and the groups are normally distributed. you want to know
# if the groups have significantly different means. 
oneway.test(extra ~ group, data=sleep)
oneway.test(extra ~ group, data=sleep, var.equal = TRUE)

#11.21 CREATING AN INTERACTION PLOT
library(faraway)
data(rats)
interaction.plot(rats$poison, rats$treat, rats$time, col = c('green'), lwd=2)

#11.22 FINDING DIFFERENCE BETWEEN MEANS OF GROUPS
#if your data is divide into diferent  groups and have different means you want to know of 
# those groups mean an their difference. 
m = aov(rats$time ~ rats$treat)
TukeyHSD(m)
plot(TukeyHSD(m), col='brown')

#11.23 PERFORMING ROBUST ANOVA (KRUSKAL-WALLIS TEST):
#when your data is divided into subgroups and each group have different median and those are
#significantly different. 
kruskal.test(rats$time ~ rats$poison)

#11.24 COMPARING MODELS BY USING ANOVA
# you have two models of the dame data nad you want ot know whether they produce different
#results
m1=lm(Cars93$Price ~ Cars93$Min.Price)
m2=lm(Cars93$Price ~ Cars93$Min.Price + Cars93$Max.Price)
m3=lm(Cars93$Price ~ Cars93$Min.Price + I((Cars93$Min.Price)^2))
anova(m1,m2,m3) # m2 model p value is significantly good rather than othrs(m1, m3)
