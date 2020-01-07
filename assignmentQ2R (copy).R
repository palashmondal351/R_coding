library(klaR)
library(LearnBayes)
#Problem: 2.a
pddata=read.csv('pd.csv')
meanval=mean(pddata$pd);  stdiv=sd(pddata$pd) ;  varience=stdiv/meanval
# calculate the beta distribution parameters
Betaparam = function(meanval, varience) {
  Betaalpha = ((1 - meanval) / varience - (1 / meanval)) * meanval ^ 2
  Betabeta = Betaalpha * (1 / meanval - 1)
  return(params = list(Betaalpha = Betaalpha, Betabeta = Betabeta))}
alphabeta=Betaparam(meanval, varience)
Betaalpha=as.numeric(alphabeta[1]); Betabeta=as.numeric(alphabeta[2])
d=sort(as.double(pddata$pd))
# prior distribution
plot(log(d), dbeta(d, Betaalpha,Betabeta), main='Prior Distribution of dbeta',
     ylab="Density function",xlab ='Data distribution', type ='l', col='red', lwd=3)
legend('topright',inset=0.05, c('ALPHA\n BETA'),lty=1,col='red',lwd = 2,title = 'Legend')

# Problem: 2b
prior=c(Betaalpha,Betabeta)  # Beta distribution
# observe 13 successes out of 20
posterior=c(13,20-13)   
triplot(prior,posterior)

#Problem: 2c
cd=(3/4 - mean(d))
plot(pddata$pd ~ seq(1,length(pddata$pd)), xlab='Sequence', ylab= 'PD values')
likelihood=function(n,y,theta){return(theta^y*(1-theta)^(n-y))}
theta=pddata$pd
plot(theta,likelihood(lengths(pddata$pd),mean(pddata$pd),theta), col='blue',
     ylab='Pd values in data', xlab='Sequence', main='maximum likelihood estimation in data')
abline(v=mean(pddata$pd)/length(pddata$pd))
likehooddiff=v
commondiff=abs(cd-likehooddiff)

cat('Difference of probabilities as condition and maximum likelihood estimation:', commondiff)
# with the given record a person is consume alchohol 3 out of 4 then, the preson consume 
# alchohol probability: 0.41 and on maximum likelihood estimation probability:0.18. and common 
# difference between them 0.23.