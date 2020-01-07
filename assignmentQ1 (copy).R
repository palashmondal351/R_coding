###################################################
# 1.a (optional understanding)
a=0;benefit=38; total=32;range=seq(a,benefit,0.1)
y=dunif(range, a, benefit)
plot(range, y, type = 'l', ylim = c(0, max(y)+0.1), main = 'Drug Effectivenss on patients')
coda=c(a, seq(a, total, 0.1),total); codb=c(0, dunif(seq(a, total, 0.1), a, benefit),0)
polygon(coda, codb, col='orange')
ccordinate=c(total, seq(total, benefit, 0.1),benefit)
dcordinate=c(0, dunif(seq(total, benefit, 0.1), a, benefit),0)
polygon(ccordinate, dcordinate, col='blue')
punif(total,a,benefit)*100
(1-punif(total,a,benefit))*100  

# 1.a 
datamean=mean(seq(1, 38));mu=datamean ;sigma=5; total=32
range=seq(0, datamean*2, 0.01); y=dnorm(range, mu, sigma)
plot(range, y, main='Data Distribution', type='l', ylim = c(0, max(y)+0.01), axes=FALSE)
axis(1, at=seq(mu-3*sigma, mu+3*sigma,sigma))

coda=c(0, seq(min(range),total, 0.01),total)
codb=c(0, dnorm(seq(min(range), total, 0.01), mu, sigma),0)
polygon(coda, codb, col='orange')

ccordinate=c(total, seq(total,max(range), 0.01),benefit)
dcordinate=c(0, dnorm(seq(tatal,max(range), 0.01), mu, sigma),0)
polygon(ccordinate, dcordinate, col='blue')

bmean1=punif(tatal,0,38)*100 #benifited people in percentage
nbmean1=100-bmean1

#################################################
# Problem 1.b
datamean=44/2; mu1=datamean; sigma1=5 ; tatal=39
range=seq(0, datamean*2, 0.01)
y=dnorm(range, mu1, sigma1)
plot(range, y, main='Data Distribution', type='l', ylim = c(0, max(y)+0.01), axes=FALSE)
axis(1, at=seq(mu1-3*sigma1, mu1+3*sigma1,sigma1))
#total values benifit people
coda=c(0, seq(min(range),tatal, 0.01),tatal)
codb=c(0, dnorm(seq(min(range), tatal, 0.01), mu1, sigma1),0)
polygon(coda, codb, col='orange')
#not benifit people
ccordinate=c(tatal, seq(tatal,max(range), 0.01),benefit)
dcordinate=c(0, dnorm(seq(tatal,max(range), 0.01), mu1, sigma1),0)
polygon(ccordinate, dcordinate, col='red')

#area of left and right
bmean2=punif(tatal,0,44)*100 #benefited people in percentage
nbmean2=100-bmean2
bmean2
nbmean2
#Difference of Benifitted 
bmean= bmean2-bmean1
cat('percentage of Benefited second drug over first one is:',bmean,'%')

#95% Credible interval difference of 
intdiff=qnorm(0.95,(mu+mu1)/2,(sigma+sigma1)/2)
cat('Creditable interval difference of 95% is:', intdiff)
  