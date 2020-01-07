# GRAPHICS
par(mfrow=c(1,1))
plot(Cars93[4:5], main='Min price vs. Price', xlab= 'Min price', ylab='Price', 
     col=c('red','green'))

plot(Cars93$Min.Price, Cars93$Max.Price)
plot(cars)
plot(cars, main='Speed vs. Distance', xlab='speed', ylab='distance')
grid()
points(cars)

with(iris, plot(Petal.Length, Petal.Width))
with(iris, plot(Petal.Length, Petal.Width,
     pch=as.integer(Species), col=c('red', 'blue')))

f=factor(iris$Species)
with(iris, plot(Petal.Length, Petal.Width, pch=as.integer(f)))
legend(1,2.5, as.character(levels(f)), pch=1:length(levels(f)))

library(faraway)
data("strongx")
m=lm(crossx ~ energy, data = strongx)
plot(crossx ~ energy, data = strongx)
abline(m)

head(iris)
plot(iris[,1:4])
data(Cars93, package = 'MASS')
coplot(Cars93$Horsepower ~ Cars93$MPG.city | Cars93$Origin)


data("airquality")
hights=tapply(airquality$Temp, airquality$Month, mean)
barplot(hights)
barplot(hights, main='Mean temp. by month', names.arg = c('May', 'June', 'July', 'August', 'Sept'),
        ylab = 'Temp in Deg. F')

library(gplots)
attach(airquality)
heights=tapply(Temp, Month, mean)
lower=tapply(Temp, Month, function(v)t.test(v)$conf.int[1])
upper=tapply(Temp, Month, function(v)t.test(v)$conf.int[2])
barplot2(heights, plot.ci=TRUE,ci.l=lower, ci.u=upper)

barplot2(heights, plot.ci=TRUE,ci.l=lower, ci.u=upper, ylim = c(50,90), xpad=FALSE,
         main = 'Mean temp by month', names.arg = c('May', 'June', 'July', 'August', 'Sept'),
         ylab = 'Temp (deg. F)')



barplot(c(3,5,4), col = c('green', 'red','blue'))
rel.hts=rank(heights)/ length(heights)
grays=gray(1-rel.hts)
barplot(heights , col = grays, main = 'Mean temp by month', names.arg = c('May', 'June', 'July', 'August', 'Sept'),
        ylab = 'Temp (deg. F)')

plot(pressure)
plot(pressure, type='l', lty='solid')
plot(pressure, type='l', lty='dotted')
plot(pressure, type='l', lty='dashed')
plot(pressure, type='l', lty='longdash')
plot(pressure, type='l', lty='dotdash', lwd=3, col='red')

xlim=range(c(Cars93$Min.Price,Cars93$Max.Price))
ylim=range(c(cars$speed,cars$dist))
plot(Cars93$Min.Price, Cars93$Max.Price, type = 'l', xlim = xlim, ylim = ylim)
lines(cars$speed,cars$dist, lty='dashed')

plot(Cars93$Min.Price, Cars93$Price)
m=mean(Cars93$Price)
m2=mean(Cars93$Min.Price)
m3=m2+c(-1:4)*sd(Cars93$Min.Price)
abline(h=m, col='green', lwd=3)
abline(v=m2, col='red', lwd=3)
abline(h=m3, lty='dotted',col='orange', lwd=3)

boxplot(airquality$Temp)
data(UScereal, package = 'MASS')
boxplot(sugars ~ shelf, data = UScereal)

hist(Cars93$MPG.city)
hist(Cars93$MPG.city, 20)
samp=rgamma(500, 2, 2)
hist(samp, 20, prob=T)
lines(density(samp), col='red', lwd=3)
plot(table(Cars93$Max.Price), type='h', lwd=3, ylab = 'Freq')
plot(table(Cars93$Max.Price)/length(Cars93$Max.Price), type='h', lwd=3, ylab = 'Freq')

qqnorm(Cars93$Price, main='Q-Q Plot and price')
qqline(Cars93$Price)
qqnorm(log(Cars93$Price), main = 'qq plot')
qqline(log(Cars93$Price))
 
RATE=1/10
y=rexp(Cars93$Price, rate=RATE)
plot(qexp(ppoints(y), rate = RATE), sort(y))
abline(a=0, b=1)

colors= ifelse(x>=0, 'blue','red')
plot(x, type = 'h', lwd=3, col=colors)
plot(x, type = 'h', lwd=3, col=c('gree', 'red'))

curve(sin, -3,3, lwd=3, col='green')
curve(dnorm, -3.5,3.5, lwd=3, col='green')
f=function(x) exp(-abs(x))*sin(2*pi*x)
curve(f,-5,5, main='Dampend sin wave curve', col='blue')

curve(sin, -3,3, lwd=3, col='green')
par(ask=FALSE)
curve(dnorm, -3.5,3.5, lwd=3, col='green')

par(mfrow=c(2,2))
quantile=seq(from=0, to=1, length.out = 30)
plot(quantile, dbeta(quantile,2,4), type = 'l', main = 'first')
plot(quantile, dbeta(quantile,4,2), type = 'l', main = 'second')
plot(quantile, dbeta(quantile,1,1), type = 'l', main = 'third')
plot(quantile, dbeta(quantile,0.5,0.5), type = 'l', main = 'forth')

dev.set()

png('multipleplot.png')
par(mfrow=c(2,2))
quantile=seq(from=0, to=1, length.out = 30)
plot(quantile, dbeta(quantile,2,4), type = 'l', main = 'first')
plot(quantile, dbeta(quantile,4,2), type = 'l', main = 'second')
plot(quantile, dbeta(quantile,1,1), type = 'l', main = 'third')
plot(quantile, dbeta(quantile,0.5,0.5), type = 'l', main = 'forth')
dev.off()

par('lty')
par('bg')
par(bg='white')
par('fg')
par(mfrow=c(1,1))
par(lwd=1)
curve(sin, -3,3)


















