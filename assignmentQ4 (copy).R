#Question: 4.a
library(klaR)
library(LearnBayes)
library(MASS)
data=read.csv('ftse.csv')
X=sort(data$logReturn)
Y=c(range(data$logReturn)[1], range(data$logReturn)[2])
plot(X,dexp(X, rate = 1/2),main='Exponential Distribution',type ='l',ylim=ylim,
     lwd=3, col='red' )
plot(X,dnorm(X,meanval=mean(X),sd=sd(data$logReturn)), main = 'Normal Distribution',
     ylim = ylim, col='red')
#******************************************************************
#question: 4.b
plot(data$logReturn ~ seq(1,length(data$logReturn)), xlab='Serial Number', ylab= 'PD values')
likelihood=function(n,y,theta){return(theta^y*(1-theta)^(n-y))}
theta=data$logReturn
plot(log(theta),likelihood(lengths(data$logReturn),mean(data$logReturn),theta), 
     col='blue', ylab='Pd value', xlab='Sequence',lty=8, main='Maximum Likelihood Extimatio test result of data')
len=1:length(data$logReturn)
wide=(len+data$logReturn)^(-1/1.5) 
lmodel=lm(wide~len)
plot(lmodel,which=2) 
bcox=boxcox(lmodel)
which.max(bcox$y)
mlhood=bcox$x[which.max(bcox$y)] 
aic=AIC(lmodel)
cat('Caculated maximum likelihood value is:', mlhood, 'and Akaike information criterion:', aic)
#Conclusion: for case for Maximum Likelihood Extimatio and Akaike information criterion value are obtain are -ve and their
# is quite big difference. So we conclusion that large difference is there inbetween Akaike information criterion 
# and Maximum Likelihood Estimation
#NOTE: In this assignment many packages are need to be install and  many visions
#some packages has different version. For guidance palashmondal351[at]google[dot][com]
# You can install(sas) then find findFn(function name) that will help too for which 
# package in need for a particular function. 
#**********************************************************
#4c and  d(together in combination)
meanval=mean(data$logReturn) ; s=sd(data$logReturn); varience=s/meanval 
Betaparam = function(meanval, varience) {
  avalue = ((1 - meanval) / varience - (1 / meanval)) * meanval ^ 2
  bvalue = avalue * (1 / meanval - 1)
  return(params = list(avalue = avalue, bvalue = bvalue))}
alphabeta=Betaparam(meanval, varience)
avalue=as.numeric(alphabeta[1]); bvalue=as.numeric(alphabeta[2])
PRIORDISTRIBUTION=c(avalue,bvalue) 
POSTERIORDISTRIBUTION=c(mean(data$logReturn), mean(data$Day))
triplot(PRIORDISTRIBUTION,POSTERIORDISTRIBUTION)
# calculating the Posterior distribution taking big values of mean from 
#problem segment a, it is notice that the aic value in section ii is 8 times larger,
#and both values is -ve in nature. 

