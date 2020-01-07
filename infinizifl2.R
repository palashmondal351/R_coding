# Getting Data 
CAR_CRASH<- read.csv("assignment1.csv")

# Question 1(a)
CAR_CRASH$TimeofDay= CAR_CRASH$Hour*100 + CAR_CRASH$Minutes

#Question 1(b) 
mn=min(CAR_CRASH$TimeofDay)
mx=max(CAR_CRASH$TimeofDay)
lc_car <-subset(CAR_CRASH, CAR_CRASH$Colour=="light")
lc_car_Density <-density(lc_car$TimeofDay,from = mn,to = mx)
plot(lc_car_Density, xlab = 'Time of the Day (Minute)',
     main="Light Colour Car Crash Kernal Density",lwd=2, col='blue' )

dc_car <-subset(CAR_CRASH, CAR_CRASH$Colour=="dark")
dc_car_Density <-density(dc_car$TimeofDay,from = mn,to = mx)
plot(dc_car_Density, xlab = 'Time of the Day (Minute)',
     main="Dark Colour Car Crash Kernal Density",lwd=2, col='brown')

#Question 1(c)
pred<-function(time){
  f = density(CAR_CRASH$TimeofDay, from=mn, to=mx)
  f_light = density(CAR_CRASH$TimeofDay[CAR_CRASH$Colour=="light"],from=mn, to=mx)
  f_dark = density(CAR_CRASH$TimeofDay[CAR_CRASH$Colour=="dark"],from=mn, to=mx)
  p = mean(CAR_CRASH$Colour=="dark")
  plot(f$x, f_dark$y*p/(p*f_dark$y+(1-p)*f_light$y), main = 'Bayes KDE with TimeofDay',
       type='l', xlab='Time', ylab='P(dark|time)', col='blue',lwd=2)
  abline(v=time, lty='dashed',  col='red')
  abline(h=0.5,lty='dashed', col='red')
  p_dark = f_dark$y[round(f_dark$x)==time]*p/(p*f_dark$y[round(f_dark$x)==time]+(1-p)*f_light$y[round(f_light$x)==time])
  return (ifelse(p_dark<0.5,'light','dark'))
}
r=sample( mn:mx,1)
pred(r)

#Question 2
#2 component Mixtures
library(mixtools) # install mixtools package in system
comp_mix2 = normalmixEM(CAR_CRASH$TimeofDay, k=2) #k=plot
plot(comp_mix2, whichplots=2) # whichplot=2: density plot

#3 component Mixtures
comp_mix3 = normalmixEM(CAR_CRASH$TimeofDay, k=3)
plot(comp_mix3, whichplots=2) 

#4 component Mixtures
comp_mix4 = normalmixEM(CAR_CRASH$TimeofDay, k=4)
plot(comp_mix4,whichplots=2)
# parametric bootstrap for sequential testing of 4 component
btstrp = boot.comp(CAR_CRASH$TimeofDay, max.comp = 4, mix.type = "normalmix", B=100)

#Making the mix model
loglik1 = sum(dnorm(CAR_CRASH$TimeofDay, mean(CAR_CRASH$TimeofDay), sd(CAR_CRASH$TimeofDay), log=TRUE)) 

#Trying AIC method, 𝐴𝐼𝐶=−2log𝐿+2𝐾
aic = -2*c(loglik1+2*2, comp_mix2$loglik+2*(3*2-1), comp_mix3$loglik+2*(3*3-1),  comp_mix4$loglik+2*(3*4-1))
aic

#Trying BIC method,BIC=-2logL + Klogn
n = length(CAR_CRASH$TimeofDay) 
bic = c(-2*loglik1+2*2, -2*comp_mix2$loglik+2*(3*2-1), -2*comp_mix3$loglik+2*(3*3-1),  -2*comp_mix4$loglik+2*(3*4-1))
bic
# AIC and BIC Comparison plot
par(mfrow=c(1,1))
plot(aic, type='l', col='blue',lwd=2, ylab ='Component Value', main = 'ACI and BIC comparison')
lines(bic, type='l', col='red')
labels=c('AIC', 'BIC')
legend(2.5, 22000,  inset=0.05, labels, lwd=2, col=c('blue','red'))

##Question 3
#Question 3(a)
library(MASS) #need to install MASS package in system
CAR_CRASH_2D_KDE = kde2d(CAR_CRASH$Latitude, CAR_CRASH$Longitude)
contour(CAR_CRASH_2D_KDE, main='Car crashed density',xlab='Latitude', ylab='Longitude', col='blue')

##########################################################################
#Question 3(b)
x = cbind(CAR_CRASH$Longitude, CAR_CRASH$Latitude)
#(EM Algorithm For Mixtures Of Multivariate Normals)
Normal_mixture_2D = mvnormalmixEM(x, k=5, epsilon = 1e-03, maxit = 100)
plot(Normal_mixture_2D, whichplots=2)

#Question 3(c)
km <- kmeans(x,5)
par(mfrow=c(1,2))
library(cluster)
library(fpc)
plot(x, km$cluster, main ='K-means density curve')  #k-means cluster
plot(Normal_mixture_2D, whichplots=2) #mixture model cluster

cl1 = apply(Normal_mixture_2D$posterior,1,which.max) # normal mixture cluster membership
cl2 = kmeans(x,5)$cluster #k-means cluster membership
#cross cluster membership of cl1, cl2
cross_cluster_membership<-addmargins(table(cl1,cl2))
cross_cluster_membership

# Here notice that the K-means cluster is very close similar for individuals. 
#qualatitive difference between two cluster is that- in K-means cluster choose the cluster 
# in a certain region which points is similar characteristic only but those points are 
# far away those points are not consider in the cluster. So in a cluster points those values 
# are belong for away k-means clustering are not considering to make a cluster group. 
#Any query write at palashmondal351[.at]gmail.com

#In normal density distribution cluster making on the number not by sorrunding in a region
# so all the data points are similar behaviour are consider in a cluster it will not ignore
#any data points those characteristic is same but position is far away from the centroid. 
