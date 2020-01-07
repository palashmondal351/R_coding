## Byyond BASIC NUMERICS AND STATISTICS

#13.1 Minimizing or Maximizing a single-parameter Function
k=function(x) 3*x^4  - 2*x^2 - 4*x + 5 
optimize(f, lower = -20, upper = 20) # by default it wil ffind the mininum value
# objective, the value of the function at that point.
optimize(k, lower = -20, upper = 20, maximum = TRUE)

#13.2 minimizing or maximizing a multiparameter function

fr <- function(x) {   ## Rosenbrock Banana function
  x1 <- x[1];x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
grr <- function(x) { ## Gradient of 'fr'
  x1 <- x[1]; x2 <- x[2]
  c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
    200 *      (x2 - x1 * x1))
}
optim(c(-1.2,1), fr) # find minimum value
optim(c(-1.2,1), fr, control = list(fnscale=-1)) # find maximum value

#13.3 CALCULATING EIGENVALUES AND EIGENVECTORS
fibmat=matrix(1:9, nrow = 3, ncol = 3)
eigen(fibmat) # will return values as eigen values and vectors as eigen vectors. 

#13.4 PERFORMING PRINCIPAL COMPONENT ANALYSIS
# for principal component analysis there is two library is there prcomp and princomp.
r=prcomp( ~ Orange$age + Orange$circumference)
print(r)
summary(r)
plot(r) # to view in bar chart
predict(r) # to rotate of your data

#13.5 PERFORMING SIMPLE ORTHOGONAL REGRESSION
#create orthogonal regression where varience x,y are treated symmetrically. 
r= prcomp( ~ Orange$age + Orange$circumference)
slope=r$rotation[2,1] / r$rotation[1,1]
intercept= r$center[2] - slope*r$center[1]
print(slope)
print(intercept)

#13.6 FINDING CLUSTER IN YOUR DATA
means=sample(c(-3,0,3), 99, replace=TRUE)
x=rnorm(99, mean = means)
tapply(x, factor(means), mean)
d=dist(x) # finding distance
hc=hclust(d) #hierarchical cluster
clust=cutree(hc, k=3) #extract three largest cluster
head(clust, 20)
tapply(x, clust, mean)
plot(x ~ factor(means), main='original clusters', xlab='cluster mean')
plot(x ~ factor(clust), main='Idenitfied clusters', xlab='cluster number')

#13.7 PREDICTING A BINARY-VALUE VARIABLE (LOGISTIC REGRESSION)
data(pima, package = 'faraway')
b=factor(pima$test)
m=glm(b ~ diastolic + bmi, family = binomial, data=pima)
summary(m)
m.red=glm(b ~ bmi, family = binomial, data=pima) #reduce model
newdata=data.frame(bmi=32.0)
# use this model and calculate probablity with BMI=32 will test positive for diabetes
predict(m.red, type = 'response', newdata = newdata) 
# predict probablity for another respone
newdata=data.frame(bmi=quantile(pima$bmi, .90))
predict(m.red, type='response', newdata = newdata)

#13.8 BOOSTRAPPING A STATITIC
#you want to estimate a confidence interval for the statistic
library(boot)
stat=function(pima, indices) {
  r=prcomp( ~ x+y, data=data, subset=indices )
  slope=r$rotation[2,1] / r$rotation[1,1]
  return(slope)
}
boot.data=data.frame(x=pima$glucose,y=pima$diastolic)
reps=boot(pima, stat, R=999)
## This code showing error

# 13.9 FACTOR ANALYSIS
#factor analysis to discover what your variabls have in common. 
diff=cbind(Cars93$Min.Price, Cars93$Price, Cars93$Max.Price, Cars93$EngineSize,Cars93$AirBags,
           Cars93$Cylinders, Cars93$Horsepower)
plot(prcomp(diff))
factanal(diff, factors=2)






