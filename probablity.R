#PROBABLITY
choose(5, 3)
choose(1:5, 3)
runif(10)
runif(5, min=-5, max = 5)
rnorm(1)
rnorm(1, mean = 100, sd=15)
rbinom(1,size = 10, prob = 0.5)
rexp(1, rate = 0.1)

set.seed(165)
runif(10)
set.seed(165)
runif(10)
runif(10)

library( MASS)
data("Cars93")
sample(Cars93$Price, 10)
sample(c('H','T'), 10, replace = TRUE)
sample(c('H','T'), 10, replace = TRUE, prob = c(0.2,0.8))
rbinom(10,1,0.8)

# PERMUTATION USING R
sample(1:10)
dbinom(7, size = 10, prob = 0.5)
pbinom(7, size = 10, prob=0.5)
pnorm(66, mean = 70, sd=3)
pexp(20,rate = 1/40)
qnorm(0.05, mean = 100, sd=15)

# PLOTING DENSITY FUNCTION
x=seq(from=-3, to=+3, length.out = 100)
par(fg='black')
plot(x, dnorm(x))
par(mfrow=c(2,2))
X=seq(from=0, to=10, length.out = 100)
ylim=c(0,0.6)
plot(X,dunif(X,min = 2, max = 4), main = 'Uniform', type = 'l', ylim = ylim)
plot(X,dnorm(X,mean =3, sd=1), main = 'Normal', type = 'l', ylim = ylim)
plot(X,dexp(X, rate = 1/2), main = 'Exponential', type = 'l', ylim = ylim)
plot(X,dgamma(X,shape = 2, rate = 1), main = 'Gamma', type = 'l', ylim = ylim)











