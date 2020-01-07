# Linear Regression and Analysis of Varience(ANOVA)
v=lm(cars$speed ~ cars$dist)
v1=lm(rating ~ complaints + privileges+learning, 
      data = attitude)
# linear regression without an intercept  
wi=lm(cars$speed ~ cars$dist + 0)
summary(wi)
# Linear regression with interaction terms
m=lm(rating ~ privileges + complaints+ complaints*privileges,
     data = attitude)
# choosing best regression variable
m=lm(rating ~ privileges + complaints+ complaints*privileges,
     data = attitude)
rm=step(m, direction = 'backward')
bm=step(m, direction = 'forward', trace = 0)
#sub-set selectin and expression in linear regression
m=lm(rating ~ privileges + complaints+ complaints*privileges,
     data = attitude, subset = 1:floor(length(rating/2)))

m=lm(rating ~ privileges + I(complaints + complaints*privileges)^2,
     data = attitude)
#Polynomial regression
pr3= lm(rating ~ poly(complaints, 3), subset = 1:25, data=attitude) # complaints of degree 3
pm2= lm(rating ~ poly(complaints, 2)+
          poly(complaints, 3), subset = 1:25,
        data=attitude) 
#regression on transform data
# there is no relationship b/w input and output data
p1=lm(log(attitude$rating) ~ attitude$complaints)
p2=lm(sqrt(attitude$rating) ~ attitude$complaints)
p3=lm(log(attitude$rating) ~ log(attitude$complaints))
# choose best transformatin of the data using boxcox 
library(MASS)
x=1:100
eps=rnorm(length(x), sd=5)
y=(x+eps)^(-1/1.5)
m=lm(y ~ x)
summary(m)
plot(m, which = 1)
bc=boxcox(m)
which.max(bc$x)
lambda=bc$x[which.max(bc$x)]
lambda
# Linear regeression confidence interval
m=lm(cars$speed ~ cars$dist)
confint(m, level = .99)

# regression residuals
m=lm(cars$speed ~ cars$dist,)
plot(m,which = 1)
plot(m,which = 2)
plot(m,which = 3)
plot(m,which = 4)
plot(m,which = 5)

# identifying the influential observation
m=lm(airquality$Temp ~ airquality$Wind)
influence.measures(m)

#auto correlation residuals for DURBIN-WATSON
library(lmtest)
m=lm(airquality$Temp ~ airquality$Wind)
dwtest(m, alternative = 'two.sided')

# predicting new value
m=lm(airquality$Temp ~ airquality$Wind + airquality$Month +
       airquality$Day)
preds=data.frame(u=3.1, v=4.0, w=5.5)
predict(m,newdata = preds)

# interaction plot
library(faraway)
data("rats")
interaction.plot(rats$poison, rats$treat, rats$time,
                 col = 'blue', lwd=3)

# difference between means of groups
m=aov(rats$time ~ rats$treat)
TukeyHSD(m)
plot(TukeyHSD(m), col='red')

# Robust ANOVA for Kruskal- Wallis
kruskal.test(rats$time ~ rats$poison)

# comparing model for anova and choose the best model
m1=lm(Cars93$Price ~ Cars93$Min.Price)
m2=lm(Cars93$Price ~ Cars93$Min.Price + Cars93$Max.Price)
m3=lm(Cars93$Price ~ Cars93$Min.Price + I(Cars93$Max.Price)^2)
anova(m1,m2)
anova(m3,m2)
anova(m1, m2, m3)
