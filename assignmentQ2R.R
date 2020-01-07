#Problem: 2.a
data=read.csv('pd.csv')
mean=mean(data$pd) 
s=sd(data$pd) 
var=s/mean # varience
# calculate the beta distribution parameters
Betaparam = function(mean, var) {
  alpha = ((1 - mean) / var - (1 / mean)) * mean ^ 2
  beta = alpha * (1 / mean - 1)
  return(params = list(alpha = alpha, beta = beta))}
alphabeta=Betaparam(mean, var)
alpha=as.numeric(alphabeta[1])
beta=as.numeric(alphabeta[2])
d=sort(as.double(data$pd))
# prior distribution  
plot(log(d), dbeta(d, alpha,beta), main='Prior Distribution of dbeta',
     ylab="Density",xlab ='Data', type ="l", col=4,lwd=4)
legend('topright',inset=0.05, c('alpha\n beta'),lty=1,col=4,lwd = 4,title = 'Legend')

# Problem: 2b
library(klaR)
library(LearnBayes)
prior=c(alpha,beta)  # Beta distribution
posterior=c(13,20-13)   # observe 13 successes out of 20
triplot(prior,posterior)

#Problem: 2c
#Given conditional probablity
cd=(3/4 - mean(d))
plot(data$pd ~ seq(1,length(data$pd)), xlab='Sequence Number', ylab= 'PD values', 
     col='blue', pch=10, main='Data Distribution plot')
likelihood=function(n,y,theta){return(theta^y*(1-theta)^(n-y))}
plot(theta,likelihood(lengths(theta),mean(theta),theta), col='green',
     ylab='Pd value', xlab='Sequence Number', main='MLE of Pd Data')
abline(v=mean(data$pd)/length(data$pd))
commondiff=abs(cd-v)

cat('Difference between the probability of given condition and MLE is:', commondiff)
# For any record if any person is consume alchohol 3 out of 4 then the preson consume 
# alchohol probability is 0.41 and on MLE the probability is 0.18. So the common 
# difference is 0.23. 