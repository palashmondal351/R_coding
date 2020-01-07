
# 1.a problem(optional for understanding in a bar plot)
a=0;b=38; x=32
range=seq(a,b,0.1)
y=dunif(range, a, b)
plot(range, y, type = 'l', ylim = c(0, max(y)+0.1), main = 'Drug Effectivenss')

cord.a=c(a, seq(a, x, 0.1),x)
cord.b=c(0, dunif(seq(a, x, 0.1), a, b),0)
polygon(cord.a, cord.b, col='green')

cord.c=c(x, seq(x, b, 0.1),b)
cord.d=c(0, dunif(seq(x, b, 0.1), a, b),0)
polygon(cord.c, cord.d, col='red')



#area of left and right
punif(x,a,b)*100  #benifited people in percentage
(1-punif(x,a,b))*100  
####################################################################
#Problem: 1.a 
# posterior distribution 
datamean=38/2
mu=datamean #mean
sigma=5 # standard deviation
x=32
range=seq(0, datamean*2, 0.01)
y=dnorm(range, mu, sigma)
plot(range, y, main='Data Distribution', type='l', ylim = c(0, max(y)+0.001), axes=FALSE)
axis(1, at=seq(mu-3*sigma, mu+3*sigma,sigma))
# area for x values benifit people
cord.a=c(0, seq(min(range),x, 0.01),x)
cord.b=c(0, dnorm(seq(min(range), x, 0.01), mu, sigma),0)
polygon(cord.a, cord.b, col='orange')
# area for not benifit people
cord.c=c(x, seq(x,max(range), 0.01),b)
cord.d=c(0, dnorm(seq(x,max(range), 0.01), mu, sigma),0)
polygon(cord.c, cord.d, col='blue')

#area of left and right
bmean1=punif(x,0,38)*100 #benifited people in percentage
nbmean1=100-bmean1


#################################################
# Problem 1.b
datamean=44/2
mu1=datamean #mean
sigma1=5 # standard deviation
x=39
range=seq(0, datamean*2, 0.01)
y=dnorm(range, mu1, sigma1)
plot(range, y, main='Data Distribution', type='l', ylim = c(0, max(y)+0.01), axes=FALSE)
axis(1, at=seq(mu1-3*sigma1, mu1+3*sigma1,sigma1))
# area for x values benifit people
cord.a=c(0, seq(min(range),x, 0.01),x)
cord.b=c(0, dnorm(seq(min(range), x, 0.01), mu1, sigma1),0)
polygon(cord.a, cord.b, col='green')
# area for not benifit people
cord.c=c(x, seq(x,max(range), 0.01),b)
cord.d=c(0, dnorm(seq(x,max(range), 0.01), mu1, sigma1),0)
polygon(cord.c, cord.d, col='red')

#area of left and right
bmean2=punif(x,0,44)*100 #benefited people in percentage
nbmean2=100-bmean2

#**************Difference of Benifitted people from Drug  *****************
bmean= bmean2-bmean1
cat('Benefited percentage of second drug over first one is:',bmean,'%')

# **********Credible interval difference of 95% for both*****************
intdiff=qnorm(0.95,(mu+mu1)/2,(sigma+sigma1)/2)
cat('Creditable interval difference of 95% is:', intdiff)
