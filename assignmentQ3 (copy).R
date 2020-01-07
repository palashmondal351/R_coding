# Problem: 3
#Problem: 3.1 create data with given condition
coordinatex=rnorm(1000, mean=0, sd=2); coordinatey=rnorm(1000, mean=0, sd=2)
library(base)
plot(coordinatex, coordinatey, pch=19,col=c('green','red'), xlim=c(-10,10), ylim=c(-10,10),
     panel.first=grid())
k=append(coordinatex,coordinatey)
mean=mean(k) ; s=sd(k) ; var=s/mean 
# BETA DISTRIBUTION
Betaparam = function(mean, var) {
  valalpha = ((1 - mean) / var - (1 / mean)) * mean ^ 2
  valuebeta = valalpha * (1 / mean - 1)
  return(params = list(valalpha = valalpha, valuebeta = valuebeta))}
valalphabeta=Betaparam(mean, var)
valalpha=as.numeric(alphabeta[1]); valuebeta=as.numeric(alphabeta[2])
d=sort(as.double(k))
# PRIOR DISTRIBUTION
plot(log(d), dbeta(d, valalpha,valuebeta), main='Prior Distribution of dbeta function')
legend('topright',inset=0.05, c('alpha\n beta'),lty=1,col=2,lwd = 2,title = 'Legend')
y1=dbeta(d, abs(valalpha),abs(valuebeta))
plot(d, y1, type='l', lwd=3, col='blue', main='Prior distribution of the data')
meanval=valalpha/(valalpha+valuebeta);  abline(v=meanval, col='red')

#Problem: 3.2
X=1:length(k); Y=(X+k)^(-1/1.5) ; m=lm(Y~X)
plot(m,which=1, col='orange') 
bc=boxcox(m)
which.max(bc$y) # MPL value
maxlikehood=bc$x[which.max(bc$y)] # lambda value for MLE
plot(boxcox(m), xlab = 'Distributed function vaues', ylab = 'Loglikehood values', col='green')
par(new=TRUE)
plot(2.35,0.95, pch=19, col='red',xlab = '',ylab = '', main = 'Likehood Estimation')

mean=mean(k)n; s=sd(k); var=s/mean
Betaparam = function(mean, var) {
  valalpha = ((1 - mean) / var - (1 / mean)) * mean ^ 2
  valuebeta = valalpha * (1 / mean - 1)
  return(params = list(valalpha = valalpha, valuebeta = valuebeta))}
alphabeta=Betaparam(mean, var)
valalpha=as.numeric(alphabeta[1]); valuebeta=as.numeric(alphabeta[2])
library(klaR)
library(LearnBayes)
prior=c(valalpha,valuebeta)
posterior=c(mean(coordinatex), mean(coordinatey))
triplot(prior,posterior)
cat('Posterior value of given condition is:', posterior[1],'and', posterior[2])

#problem: 3.4
mean=mean(k); s=sd(k); var=s/mean
Betaparam = function(mean, var) {
  valalpha = ((1 - mean) / var - (1 / mean)) * mean ^ 2
  valuebeta = valalpha * (1 / mean - 1)
  return(params = list(valalpha = valalpha, valuebeta = valuebeta))}
alphabeta=Betaparam(mean, var)
valalpha=as.numeric(alphabeta[1]); valuebeta=as.numeric(alphabeta[2])

prior=c(valalpha,valuebeta);  posterior=c(mean(coordinatex), mean(coordinatey))
cat('Posterior value of given condition is:', posterior[1],'and', posterior[2])

#Problem: 3.5
coordinatex=rnorm(9, mean=0.43, sd=1); coordinatey=rnorm(9, mean=0.09, sd=1)
k=append(coordinatex,coordinatey)
x=1:length(k); Y=(x+k)^(-1/1.5) 
m=lm(Y~x, drop.unused.levels = TRUE)
plot(m,which=1) 
bc=boxcox(m)
which.max(bc$y)
maxlikehood=bc$x[which.max(bc$y)] 
plot(boxcox(m), xlab = 'Distributed vaues', ylab = 'Loglikehood')
par(new=TRUE)
plot(2.35,0.95, pch=19, col='red',  main = 'Likehood Estimation')

