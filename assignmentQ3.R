# Problem: 3
#3.1 create data with given condition
xcord=rnorm(1000, mean=0, sd=2)
ycord=rnorm(1000, mean=0, sd=2)
library(base)
plot(xcord, ycord, pch=19,col=c('blue','red'), xlim=c(-10,10), ylim=c(-10,10),
     panel.first=grid()) # visuliazise the data points


k=append(xcord,ycord)
mean=mean(k) # mean
s=sd(k) # standard deviation
var=s/mean # varience
# calculate the beta distribution parameters
Betaparam = function(mean, var) {
  alpha = ((1 - mean) / var - (1 / mean)) * mean ^ 2
  beta = alpha * (1 / mean - 1)
  return(params = list(alpha = alpha, beta = beta))}
alphabeta=Betaparam(mean, var)
alpha=as.numeric(alphabeta[1])
beta=as.numeric(alphabeta[2])
d=sort(as.double(k))
# prior distribution
plot(d, dbeta(d, alpha,beta), main='Prior Distribution of dbeta')
legend('topright',inset=0.05, c('alpha\n beta'),lty=1,col=2,lwd = 2,title = 'Legend')
y1=dbeta(d, abs(alpha),abs(beta))
plot(d, y1, type='l', lwd=6, col='blue', main='Prior distribution')
meanval=alpha/(alpha+beta)
#maxval=(alpha-1)/(alpha+beta-2)
abline(v=meanval, col='red', lwd=2)
######################
#3.2
X=1:length(k)
Y=(X+k)^(-1/1.5) 
m=lm(Y~X)
plot(m,which=1) 
bc=boxcox(m)
which.max(bc$y) # maximum possible likelihood value
maxlikehood=bc$x[which.max(bc$y)] # position of corrosponding lambda value
maxlikehood
plot(boxcox(m), xlab = 'Distributed vaues', ylab = 'Loglikehood', pch=21,col='blue')
par(new=TRUE)
plot(2.35,0.95, pch=19, col='red',xlab = '',ylab = '', main = 'Likehood Estimation')
##########################
mean=mean(k) 
s=sd(k) 
var=s/mean 
# calculate the beta distribution parameters
Betaparam = function(mean, var) {
  alpha = ((1 - mean) / var - (1 / mean)) * mean ^ 2
  beta = alpha * (1 / mean - 1)
  return(params = list(alpha = alpha, beta = beta))}
alphabeta=Betaparam(mean, var)
alpha=as.numeric(alphabeta[1])
beta=as.numeric(alphabeta[2])
library(klaR)
library(LearnBayes)
prior=c(alpha,beta)  # Beta distribution
posterior=c(mean(xcord), mean(ycord))
triplot(prior,posterior)
cat('Posterior value of given condition is:', posterior[1],'and', posterior[2])
######################################################################
#3.4
mean=mean(k) # mean
s=sd(k) # standard deviation
var=s/mean # varience
# calculate the beta distribution parameters
Betaparam = function(mean, var) {
  alpha = ((1 - mean) / var - (1 / mean)) * mean ^ 2
  beta = alpha * (1 / mean - 1)
  return(params = list(alpha = alpha, beta = beta))}
alphabeta=Betaparam(mean, var)
alpha=as.numeric(alphabeta[1])
beta=as.numeric(alphabeta[2])
library(klaR)
library(LearnBayes)
prior=c(alpha,beta)  # Beta distribution
posterior=c(mean(xcord), mean(ycord))
cat('Posterior value of given condition is:', posterior[1],'and', posterior[2])

################################################
#3.5
xcord=rnorm(9, mean=0.43, sd=1)
ycord=rnorm(9, mean=0.09, sd=1)
k=append(xcord,ycord)

x=1:length(k)
Y=(x+k)^(-1/1.5) 

m=lm(Y~x, drop.unused.levels = TRUE)
plot(m,which=1) 
bc=boxcox(m)
which.max(bc$y) # maximum possible likelihood value
maxlikehood=bc$x[which.max(bc$y)] # position of corrosponding lambda value
maxlikehood
plot(boxcox(m), xlab = 'Distributed vaues', ylab = 'Loglikehood')
par(new=TRUE)
plot(2.35,0.95, pch=19, col='red',xlab = '',ylab = '', main = 'Likehood Estimation')
