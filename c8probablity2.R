# CHAPTER: PROBABLITY

# Names of Distributions: 
# dnorm- Normal Density, pnorm- Normal Distribution function, qnorm- Normal quantile function
# rnorm- Normal random variable, binorm- Binomal, geom- Geometeric, hyper- Hypergeometric
# nbinorm- Negative binomial, pois- Poisson, gamma- Gamma, logis- Logistic, norm- Normal

#8.1 COUNTING THE NUMBER OF COMBINATIONS
choose(5,3)  # way of combination 3 out of 5

# GENERATING COMBINATIONS
combn(1:5,3)  # all combinatins of the numbers 1 through 5 taken 3 at a time.

# 8.3 GENERATING RANDOM NUMBER
runif(10) # to generate 10 random number
runif(5,min = -5,max = 5)
rnorm(1) # one standard normal variate
rnorm(1,mean=100, sd=15)
rbinom(1,size = 10, prob = 0.5) # one binomal variate
rpois(1,lambda = 10) # one poison variate
rexp(1,rate = 0.1)
rgamma(1,shape = 2,rate = 0.1)

# 8.4 GENERATING REPRODUCIABLE RANDOM NUMBERS
set.seed(165)
runif(10)
set.seed(165) # Initialize the random from where it will start
runif(10) # calling the initialize after calling the set.seed funcition. 

#8.5 GENERATING A RANDOM SAPLE
library(MASS)
data(Cars93)
sample(Cars93$Min.Price,10) # 10 random sample, each run it wil give different result

# 8.6  Generating random Sequence
sample(c('H','T'),10, replace = TRUE)
sample(c('H','T'),10, replace = TRUE, prob = c(0.2,0.8))
rbinom(10,1,0.8) # random binomial of 10 number of probablity 0.8

# 8.7 RANDOMLY PERMUTING A VECTOR
sample(1:10)

#8.8 CALCULATING PROBABLITIES FOR DISTRIBUTION PROBABLITIES. 
dbinom(7,size = 10, prob = 0.5) # simple probablity of binomial distribution
pbinom(7,size = 10, prob = 0.5) # cumulative probablity of binomial distribution

#8.9 CALCULATING PROBABLITIES OF CONTINEOUS DISTRIBUTIONS
pnorm(66, mean = 70, sd=3) # normal probablity
pexp(20, rate = 1/40) # exponential probablity

# 8.10 CONVERTING PROBABILISTIC TO QUANTILES 
qnorm(0.05, mean = 100, sd=15) # quantiles functions for normal distributions

#8.11 PLOTING A DENSITY FUNCTION
x=seq(from=-3, to=+3, length.out=100)
plot(x,dnorm(x)) # plotting x sequence with dnorm(x) 
par(mflow=c(2,2))
X=seq(from=0, to=10, length.out=100)
ylim=c(0,0.6)
# Gamma function
plot(X,dunif(X,min = 2, max = 4),main = 'Uniform', type =  'l', ylim = ylim) 
plot(X,dnorm(X, mean = 3, sd = 1),main = 'Normal', type =  'l', ylim = ylim) 
plot(X,dexp(X, rate = 1/2),main = 'Exponential', type =  'l', ylim = ylim) 
plot(X,dgamma(X,shape = 2, rate = 1),main = 'Gamma', type =  'l', ylim = ylim) 
