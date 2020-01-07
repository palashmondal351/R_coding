#4.a
data=read.csv('ftse.csv')
X=sort(data$logReturn)
Y=c(range(data$logReturn)[1], range(data$logReturn)[2])
plot(X,dexp(X, rate = 1/2),main='Exponential Distribution',type ='l',ylim=ylim,
     lwd=3, col='blue' )
plot(X,dnorm(X,mean=mean(X),sd=sd(data$logReturn)), main = 'Normal Distribution',
     ylim = ylim, col='blue',pch=20)
############################################################
#4.b
plot(data$logReturn ~ seq(1,length(data$logReturn)), xlab='Serial Number', ylab= 'PD values')
likelihood=function(n,y,theta){return(theta^y*(1-theta)^(n-y))}
theta=data$logReturn
plot(log(theta),likelihood(lengths(data$logReturn),mean(data$logReturn),theta), 
     ylab='Pd value', xlab='Sequence',lty=8, main='MLE test result of data')
library(MASS)
len=1:length(data$logReturn)
wide=(len+data$logReturn)^(-1/1.5) 
lmodel=lm(wide~len)
  plot(lmodel,which=2) 
bcox=boxcox(lmodel)
which.max(bcox$y)
mlhood=bcox$x[which.max(bcox$y)] 
aic=AIC(lmodel)
cat('Caculated maximum likelihood value is:', mlhood, 'and AIC:', aic)
#Conclusion: Both the case for mle and AIC the value is obtain is negative and their
# is large difference between them. So far we come to the conclusion that large difference
# is there inbetween AIC and MLE
#NOTE: In this program so many package need to be install and there is many vision
#some packages have different version. For guidance palashmondal351[at]google[dot][com]
# or you can install(sas) then find findFn(function name) that will help too. 
#############################################
#4c & &d
mean=mean(data$logReturn) # mean
s=sd(data$logReturn) # standard deviation
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
prior=c(alpha,beta) 
posterior=c(mean(data$logReturn), mean(data$Day))
triplot(prior,posterior)
# After calculating the Posterior distribution with the big values of mean, it is 
# notice that the aic value in section b is 8 times bigger, where both the values is
# negative in nature. 