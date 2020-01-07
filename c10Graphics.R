## THIS CHAPTER WE WILL TALK ABOUT GRAPHICS.
# Graphics is a gretat strength of R. The graphics package is the part of standard distribution
# and  many useful functions for creating a variety of graphic displays. 

# plot Cars93 dataset of column 4 and 5. 
plot(Cars93[4:5], main='Min.price vs Price',xlab="Minprice",ylab="Price",col=c('red','green')) 

# NOTE ON GRAPHICS FUNCTION:
# It's important to understand the distance between high-level and low-level graph.
# A high-level graphics function starts a new graph.
# A low-level grahics function can't start a new graph. Rather it can adds something to an 
#existing graph: points, lines, text, adornments and so forth.
"
plot - generating ploting function
boxplot - create a box plot
hist - create a histogram
qqnorm -  create a quantile - quantile(Q0Q) plot. 
curve - graph a function
points - add points
lines - add lines
abline - add a straight line
segments - add line segments
polygon - add a closed polygon
text - add text

YOU MUST CALL A HIGH-LEVEL GRAPHICS ROUTINE BEFORE CALLING A LOW-LEVEL GRAPHICS ROUTINE. 
"
# 10.1 CREATING A SCATTER PLOT.
plot(Cars93$Min.Price, Cars93$Max.Price)
plot(cars) # where car data has two column

#10.2 ADDING A TITLE AND LABELS
plot(cars, main='Speed Vs. Distance', xlab='Speed', ylab='Distance')

#10.3 ADDING A GRID
plot(cars, main='Speed Vs. Distance', xlab='Speed', ylab='Distance')
grid()
points(cars)

#10.4 CREATING A SCATTER PLOT OF MULTIPLE GROUPS
with(iris, plot(Petal.Length, Petal.Width))
with(iris, plot(Petal.Length, Petal.Width, pch=as.integer(Species), col=c('red','blue')))

#10.5 ADDING A LEGEND
# legenda little box that decodes the graphic for the viewer. 
f=factor(iris$Species)
with(iris, plot(Petal.Length, Petal.Width, pch=as.integer(f)))
legend(1,2.5, as.character(levels(f)), pch=1:length(levels(f)))
# first two argument is the  coordinate of the legend box

#10.6 PLOTTING THE REGRESSION LINE OF A SCATTER PLOT.
library(faraway) # import library first
data(strongx) # import data from faraway.
m=lm(crossx~energy, data=strongx)
plot(crossx~energy, data=strongx)
abline(m)

# 10.7 PLOTTING ALL VARIABLES AGAINEST ALL OTHERS VARIABLES
head(iris)
plot(iris[,1:4])

# 10.8 CREATING ONE SCATTER PLOT FOR EACH FACTOR LEVEL. 
data(Cars93, package = 'MASS')
coplot(Cars93$Horsepower ~ Cars93$MPG.city | Cars93$Origin)

## 10.9 CREATING A BAR-CHART
data(airquality)
heights=tapply(airquality$Temp, airquality$Month, mean)
barplot(heights)
barplot(heights,main = "Mean Temp. by Month",
        names.arg = c("May","June","July", "Aug.", "Sep."),
        ylab = "Temp(deg. F)")

#10.10 ADDING CONFIDENCE INTERVALS TO A BAR CHART
library(gplots)
attach(airquality) # attache for search path
heights=tapply(Temp, Month, mean)
lower=tapply(Temp, Month, function(v) t.test(v)$conf.int[1])
# conf.int[1] is a vector of confidence interval of lower level and upper level
upper=tapply(Temp, Month, function(v) t.test(v)$conf.int[2])
barplot2(heights, plot.ci=TRUE, ci.l=lower, ci.u=upper)
barplot2(heights, plot.ci=TRUE, ci.l=lower, ci.u=upper,ylim = c(50, 90), xpd=FALSE,
        main = 'Mean Temp. By Month',
        names.arg = c('May', 'June','July', 'Aug.','Sep.'),
        ylab = 'Temp (deg. F)')

# 10.11 COLORING A BAR CHART
barplot(c(3,5,4), col = c('red','green', 'blue'))
data("airquality")
heights=tapply(airquality$Temp, airquality$Month, mean)
rel.hts=rank(heights)/length(heights)
grays=gray(1-rel.hts)
barplot(heights, col = grays)

rel.hts=(heights - min(heights)) / (max(heights) - min(heights))
grays=gray(1-rel.hts)
barplot(heights, col=grays, ylim = c(50,90), xpd = FALSE, main = 'Mean Temp. by Month',
        names.arg = c('May','June','July', 'Aug.','Sep.'),
        ylab = 'Temp (deg. F)')


#10.12 PLOTTING A LINE FROM X AND Y POINTS
plot(pressure) # pressure is a built-in data set
plot(pressure, type='l')

#10.13 CHANGING THE TYPE, WIDTH, OR COLOR OF A LINE. 
plot(pressure, type='l', lty='solid') #lty for line type. 
plot(pressure, type='l', lty='dotted')
plot(pressure, type='l', lty='dashed')
plot(pressure, type='l', lty='dotdash')
plot(pressure, type='l', lty='longdash')
plot(pressure, type='l', lty='solid', lwd=2) # lwd for line width
plot(pressure, type='l', lty='dashed', lwd=2, col='red')
plot(pressure, type='l', lty='dotdash', lwd=3, col='green')

#10.14 PLOTTING MULTIPLE DATASET
xlim=range(c(Cars93$Min.Price,Cars93$Max.Price))
ylim=range(c(cars$speed,cars$dist))
plot(Cars93$Min.Price, Cars93$Max.Price, type = 'l', xlim = xlim, ylim = ylim)
lines(cars$speed,cars$dist, lty='dashed')

#par(mfrow=c(2,1)) # par will divide the section as mfrow of 2 row and 1 column (mfcol can be)
plot(Cars93$Min.Price, Cars93$Price)
plot(Cars93$Max.Price, Cars93$Price)

#10.15 ADDING VARTICAL AND HORIZONTIAL LINE
# abline will drow vertical line(v), and horizontial line(h) respectively. 
plot(Cars93$Min.Price,Cars93$Price)
m=mean(Cars93$Price)
m2=mean(Cars93$Min.Price)
m3=m2 + c(-1:4)*sd(Cars93$Min.Price)
abline(h=m, col='orange')
abline(v=m2,col='green')
abline(h=m3, lty='dotted',lwd=2, col='red')

# 10.16 CREATING A BOX PLOT
boxplot(airquality$Temp) 

#10.17 CREATING ONE BOX PLOT FOR EACH FACTOR LEVEL
data(UScereal, package = 'MASS')
boxplot(sugars~shelf, data=UScereal)

#10.18 CREATING A HISTOGRAM
data(Cars93 , package='MASS')
hist(Cars93$MPG.city)
hist(Cars93$MPG.city,20) # 20 number accomodate the suggestion. 

# 10.19 ADDING A DENSITY ESTIMATE TO A HISTOGRAM. 
# you have histogram and you want to add a curve to illustrate the apperent density
samp=rgamma(500, 2,2)
hist(samp, 20, prob=T) # will give probablity in y axis
lines(density(samp), col='red', lwd=2)

#10.20 CREATING A DISCRET HISTOGRAM
plot(table(Cars93$Max.Price), type = 'h', lwd=2, ylab = 'Freq')
plot(table(Cars93$Max.Price)/ length(Cars93$Max.Price), type = 'h', lwd=2, ylab = 'Freq')

#10.21 CREATING A NORMAL QUANTILE-QUANTILE (Q-Q) PLOT.
# to know wheather the data is normally distributed or not. 
qqnorm(Cars93$Price, main='Q-Q Plot: Price')
qqline(Cars93$Price)

qqnorm(log(Cars93$Price), main='Q-Q Plot: log(Price)')
qqline(log(Cars93$Price))

# 10.22 CREATING OTHERS QUANTILE-QUANTILE PLOTS
# if the data is not normally distributed
RATE=1/10
y = rexp(Cars93$Price,rate = RATE) 
plot(qexp(ppoints(y),rate=RATE), sort(y)) # ppoints for probablity plotting
abline(a=0, b=1)

#10.23 PLOTING A VARIABLE IN MULTIPLE COLORS
colors=ifelse(x>=0, 'blue','red')
plot(x, type = 'h', lwd=3, col=colors)
plot(x, type = 'l', lwd=3, col=c('blue','green')) # willn't work,it'll use first color only 

#10.24 GRAPHING A FUNCTION
curve(sin,-3,3, lwd=2, col='green')
curve(dnorm,-3.5,3.5)
f=function(x) exp(-abs(x)) * sin(2*pi*x)
curve(f,-5,5, main='Dampened sine wave', col='red')

# 10.25 PAUSING BETWEEN PLOTS
#if you want to pause plots to overcome out of overlaping problem.
# global graphics option ask=TRUE, R will pause for each new plots
curve(sin,-3,3, lwd=2, col='green')
par(ask=TRUE) # pause the previous plot, hit enter in console to get the new plot. 
curve(dnorm,-3.5,3.5)

#10.26 DISPLYING SEVERAL FIGURES ON ONE PAGE
par(mfrow=c(2,2)) # will create 2*2 grid plot (mfcol will fill plot in column wise)
Quantile=seq(from=0, to=1, length.out = 30)
plot(Quantile, dbeta(Quantile, 2, 4),type = 'l', main = 'first')
plot(Quantile, dbeta(Quantile, 4, 2),type = 'l', main = 'second')
plot(Quantile, dbeta(Quantile, 1, 1),type = 'l', main = 'third')
plot(Quantile, dbeta(Quantile, 0.5, 0.5),type = 'l', main = 'fourth')

#10.27 OPENING ADDITIONAL GRAPHICS WINDOWS
# use win.graph fuction to open a second graphics window
dev.set() # will return the number of active window. 
 
# 10.28 WRITING YOUR PLLOT TO A FILE
png('multipleplot.png')
par(mfrow=c(2,2)) # will create 2*2 grid plot (mfcol will fill plot in column wise)
Quantile=seq(from=0, to=1, length.out = 30)
plot(Quantile, dbeta(Quantile, 2, 4),type = 'l', main = 'first')
plot(Quantile, dbeta(Quantile, 4, 2),type = 'l', main = 'second')
plot(Quantile, dbeta(Quantile, 1, 1),type = 'l', main = 'third')
plot(Quantile, dbeta(Quantile, 0.5, 0.5),type = 'l', main = 'fourth')
dev.off() # close the graphics file
par(mfrow=c(1,1)) 
#10.29 CHANGING GRAPHICAL PARAMETERS
#changing the graphics default setting like, color, line width, background color. 
par('lty') # it will show default line type
par('bg')
# to change follow the example
par(bg='lightblue')
par(bg='white')
par(mfrow=c(1,1))
curve(sin,-3, 3) # you may notice that the background color is change to lightyellow. 
#par(fg='black')
par(lwd=2)
