# Getting Data 
CAR_CRASH<- read.csv("infinizifl.csv")

# Analysising the Data.
str(CAR_CRASH)
summary(CAR_CRASH)

# Question 1

TOD<- function(h,m){ 
  return (h*100+m)
}
TimeOD <- TOD(CAR_CRASH$Hour,CAR_CRASH$Minutes)
CAR_CRASH_Modified <- data.frame(CAR_CRASH,TimeOD)  
head(CAR_CRASH_Modified)

#Question 1(b) 
Start_time=min(TimeOD)
End_time=max(TimeOD)

light_Col_Car <-subset(CAR_CRASH_Modified, CAR_CRASH_Modified$Colour=="light")
light_Col_Car_Density <-density(light_Col_Car$TimeOD, from = Start_time, to = End_time)
plot(light_Col_Car_Density, main="Plot of Light Colour Car Crash Kernal Density Estimate")

dark_Col_Car <-subset(CAR_CRASH_Modified, CAR_CRASH_Modified$Colour=="dark")
dark_Col_Car_Density <-density(dark_Col_Car$TimeOD, from = Start_time, to = End_time)
plot(I(dark_Col_Car_Density), main="Plot of Dark Colour Car Crash Kernal Density Estimate")

#Question 1(c)

pred<-function(time){
  f = density(CAR_CRASH_Modified$TimeOD, from=Start_time, to=End_time)
  f_light = density(CAR_CRASH_Modified$TimeOD[CAR_CRASH_Modified$Colour=="light"],from=Start_time, to=End_time)
  f_dark = density(CAR_CRASH_Modified$TimeOD[CAR_CRASH_Modified$Colour=="dark"],from=Start_time, to=End_time)
  p = mean(CAR_CRASH_Modified$Colour=="dark")
  plot(f$x, f_dark$y*p/(p*f_dark$y+(1-p)*f_light$y), type='l', xlab='time', ylab='P(dark|time)')
  abline(v=time,col='blue')
  abline(h=0.5,lty=3)
  #prob<-f1$y*p/(p*f1$y+(1-p)*f0$y)
  p_dark<-f_dark$y[round(f_dark$x)==time]*p/(p*f_dark$y[round(f_dark$x)==time]+(1-p)*f_light$y[round(f_light$x)==time])
  
  return(ifelse(p_dark<0.5,'light','dark'))
  
}

pred(1200)


#Question 2

#2 component Mixtures
comp_mix2 = normalmixEM(TimeOD, k=2)
plot(comp_mix2, whichplots=2)

#3 component Mixtures
comp_mix3 = normalmixEM(TimeOD, k=3)
plot(comp_mix3, whichplots=2)

#four component Mixtures
comp_mix4 = normalmixEM(TimeOD, k=4)
plot(comp_mix4,whichplots=2)

btstrp = boot.comp(TimeOD, max.comp = 4, mix.type = "normalmix", B=100)

#Making the mix model

loglik1 = sum(dnorm(TimeOD, mean(TimeOD), sd(TimeOD), log=TRUE)) 

#Trying AIC method

aic = c(-2*loglik1+2*2, -2*comp_mix2$loglik+2*(3*2-1), -2*comp_mix3$loglik+2*(3*3-1),  -2*comp_mix4$loglik+2*(3*4-1))
aic

#Trying BIC method
n = length(TimeOD) 
bic = c(-2*loglik1+log(n)*2, -2*comp_mix2$loglik+log(n)*(3*2-1), -2*comp_mix3$loglik+log(n)*(3*3-1),-2*comp_mix4$loglik+2*(3*4-1))
bic

#Question 3(a)

library(MASS)
CAR_CRASH_2D_KDE = kde2d(CAR_CRASH_Modified$Latitude, CAR_CRASH_Modified$Longitude)
contour(CAR_CRASH_2D_KDE)

#Question 3(b)
x<- cbind(CAR_CRASH_Modified$Longitude, CAR_CRASH_Modified$Latitude)
Normal_mixture_2D <- mvnormalmixEM(x, k=5)
plot(Normal_mixture_2D, whichplots=2)

#Question 3(c)

km <- kmeans(x,5)
par(mfrow=c(1,2))
plotcluster(x,km$cluster, main = 'Normal Density Curve')
plot(Normal_mixture_2D, whichplots=2)
cl1 = apply(Normal_mixture_2D$posterior,1,which.max)
cl2 = kmeans(x,5)$cluster
#cross clustering membership among normal density and K-means
cross_cluster_membership<-addmargins(table(cl1,cl2))
cross_cluster_membership
