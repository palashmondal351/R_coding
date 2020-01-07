##14. TIME SERIES ANALYSIS

#TIME SERIES ANALYSIS HAS COME A HOT TOPIC WITH THE RISE OF 
#QUANTITATIVE FINANCE AND AUTOMATED TRADING OF SECURITIES.
#MOST COMMON PACKAGE OF TIME SERIES IS ZOO OR XTS PACKAGES.
#EVERY OBSERVATION IN TIME SERIES HAS AN ASSOCIATED DATE OR TIME. 
  
#14.1 REPRESNTING TIME SERIES DATA. 
library(zoo)
library(xts)
price=c(132.45, 130.85, 130.00, 129.55, 130.85)
dates=as.Date(c('2010-01-04','2010-01-05','2010-01-06','2010-01-07','2010-01-08'))
ibm.daily=zoo(price, dates)
print(ibm.daily)
ibm.daily2=xts(price, dates) #just convert the type from zoo to xts
print(ibm.daily2) #vartically representation of the data. 
coredata(ibm.daily) # to extract the data 
index(ibm.daily) # to extract the index

#14.2 PLOTTING TIME SERIES DATA
#if you plot th object with scerrens=1 R will plot the two time seeriees together
#in one plot
price=c(c(132.45, 130.85, 130.00, 129.55, 130.85)
        ,c(132.45, 150.85, 140.00, 139.55, 140.85))
dates=as.Date(c('2010-01-04','2010-01-05','2010-01-06','2010-01-07','2010-01-08'))
ibm.daily3=zoo(price, dates)
plot(ibm.daily3, screens = 1)
plot(ibm.daily3, screens = c(1,2)) # print both time series separately. 
xlab='Date'
ylab='Relative Price'
main='IBM: Historical vs. Inflationn-adjusted'
lty=c('dotted', 'solid')
ylim=range(coredata(ibm.daily))
plot(ibm.daily, screens = c(1,2), lty=lty, main = main, xlab = xlab,
     ylab = ylab, ylim = ylim)
plot(ibm.daily, screens = 1, lty=lty, main = main, xlab = xlab, 
     ylab = ylab, ylim = ylim)
legend(as.Date('1970-01-01'), 140,
       c('Hist', 'Daily-Adj'),
       lty = c('dotted', 'solid'))

rainfall1 <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
rainfall2 <- 
  c(655,1306.9,1323.4,1172.2,562.2,824,822.4,1265.5,799.6,1105.6,1106.7,1337.8)
combined.rainfall <-  matrix(c(rainfall1,rainfall2),nrow = 12)
rainfall.timeseries <- ts(combined.rainfall,start = c(2012,1),frequency = 12)
print(rainfall.timeseries)
plot(rainfall.timeseries, main = "Multiple Time Series")

#14.3 Extracting the oldest or newest observations
#you want to see only the oldest or newest observation of your time series data
rainfall1 <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
rainfall2 <- 
  c(655,1306.9,1323.4,1172.2,562.2,824,822.4,1265.5,799.6,1105.6,1106.7,1337.8)
combined.rainfall <-  matrix(c(rainfall1,rainfall2),nrow = 12)
rainfall.timeseries <- ts(combined.rainfall,start = c(2012,1),frequency = 12)
print(rainfall.timeseries)
plot(rainfall.timeseries, main = "Multiple Time Series")
head(ibm.daily)
tail(ibm.daily)

#14.4 SUBSETTING A TIME SERIES
#you want to select one or more elements from a time sseries
ibm.daily
ibm.daily[2]
ibm.daily[2:5]
ibm.daily[as.Date('2010-01-05')]
#select by vector of object
dates=seq(as.Date('2010-01-04'), as.Date('2010-01-08'), by=2) 
ibm.daily[dates]
# for range of date
window(ibm.daily, start = as.Date('2010-01-04'), end = as.Date('2010-01-08'))

#14.5 MARGIN SEVERAL TIME SERIES
price1=c(232.45, 230.85, 230.00, 229.55, 230.85)
dates1=as.Date(c('2011-01-04','2011-01-05','2011-01-06','2011-01-07','2011-01-08'))
data1=zoo(price1, dates1)

price2=c(242.45, 240.85, 240.00, 249.55, 240.85)
dates2=as.Date(c('2011-01-02','2011-01-03','2011-01-04','2011-01-05','2011-01-06'))
data2=zoo(price2, dates2)
merge(data1,data2) #merge of two data set
na.locf(merge(data1, data2), all=FALSE) #intersection of all dates. 

#14.6 FILLING OR PADDING A TIME SERIES
price3=c(242.45, 240.85, 240.00, 249.55, 240.85)
dates3=as.Date(c('1970-01-02','1971-01-03','1972-01-04','1972-01-05','1972-01-06'))
data3=zoo(price3, dates3)
dates=seq(from=as.Date('1970-01-01'), to=as.Date('1972-12-31'), by=1)
empty=zoo(dates)
filled.data3=merge(data3, empty, all=TRUE)
filled.data3
#field the empty value with most recent values.
filled.data3=na.locf(merge(data3, empty, all=TRUE))
filled.data3

#14.7 LAGGING A TIME SERIES 
#shift a time series in time either forward or backward.
data3
lag(data3, k=+1, na.pad=TRUE) #shift the data forward by 1 day
#shift the data of backward by one day and fill the empty valuewith NA 
lag(data3,k=-1, na.pad = TRUE)

#14.8 COMPUTING SUCCESSIVE DIFFERENCES
data3
diff(data3)
diff(data3,lag=2) 

#14.9 PERFORMING CALCULATION ON TIME SERIES
data3
diff(data3)
diff(data3) / data3
100*(diff(data3) / data3)
log(data3)
diff(log(data3))

#14.10 COMPUTING A MOVING AVERAGE
# compute the moving average of time series
ma=rollmean(data3, k=2)
ma
ma2=rollmean(data3, k=2, align = 'left') # use data that available on that day
ma2

#14.11 APPLYING A FUNCTION BY CALENDAR PERIOD
apply.monthly(as.xts(data3), mean)
# calcualte volatily, means changing standard deviation of daily log return
apply.monthly(as.xts(diff(log(data3))), sd) 
#daily number of estimated annualized volatily:
library(quantmod)
getSymbols('PG', form='2016-09-27', to='2017-09-27', auto.assign = TRUE)
close=PG$PG.Close
mean(close)
ts.plot(close)

#14.12 APPLYING A ROLLING FUNCTION
# calculate a function point and move to the next data point
rollapply(close, 50, sd, by=3, align = 'right')
#14.13 PLOTTING THE AUTO-CORRELATON FUNCTION
acf(PG$PG.Adjusted)

#14.14 TESTING A TIME SERIES FOR AUTOCORRELATION
Box.test(data3) #p value small means it is significance correlations. 

#14.15 Plotting the partial Autocorrelation function
pacf(PG$PG.High)

#14.16 FINDING LAG CORRELATIONS BETWEEN TWO TIME SERIES
require(graphics)
acf(lh)  #data: Luteinizing Hormone in Blood Samples
acf(lh, type = "covariance")
pacf(lh)
acf(ldeaths) #data: Monthly Deaths from Lung Diseases in the UK
acf(ldeaths, ci.type = "ma")
acf(ts.union(mdeaths, fdeaths))
ccf(mdeaths, fdeaths, ylab = "cross-correlation")

#14.17 DETRENDING A TIME SERIES
# Your time series data contanis a traind that you want to remove. 
# treand means that they gradually slope upward or downward over time. 
m=lm(coredata(PG$PG.Open) ~ index(PG$PG.Open))
detr=zoo(resid(m), index(PG$PG.Open))
plot(m)
plot(detr)

#14.18 FITTING AN ARIMA MODEL
# ARIMA - Autoregressive integrated moving average
# ARIMA model involves three steps
#1. identify the model order. 
#2. fit the model to the data, giving the cofficients
#3. apply diagnostic measure to validate the model.
#model consist three integer. p-autoaggressive coefficient, d-degree of differencing,
#q- number of moving average coefficient. 
library(forecast)
library(ggplot2)
arima(data3, order = c(2,1,2))
# auto.arima and arima includes the fitted cofficients and the 
# standard errors(s.e) for each coefficient
m=arima(PG$PG.Open, order = c(2,1,2))
confint(m)

#14.19 REMOVING INSIGNIFICANT ARIMA COEFFICIENTS
m=arima(PG$PG.Open, order = c(2,1,2))
confint(m)
m=arima(PG$PG.Open, order = c(2,1,2), fixed = c(NA,0,NA,0))
m
confint(m)

#14.20 RUNNING DIAGNOSTICS ON AN ARIMA MODEL
#diagnosis test to valid the model. 
m=arima(PG$PG.Open, order = c(2,1,2), fixed = c(0,NA,0,NA), transform.pars =FALSE)
tsdiag(m) # show a graph of 3 plot. 

#14.21 MAKING FORECASTS FROM AN ARIMA MODEL
#you have ARIMA model for your time series, 
#you want to forecast the next few observations in the series. 
predict(m)
predict(m,n.ahead = 10)

#14.22 TESTING FOR MEAN REVERSION
# you want to know if your time series is mean reverting
# Large p values not reverting
library(tseries)
adf.test(coredata(PG$PG.Open))
adf.test(coredata(PG$PG.Close))
plot(PG$PG.Open)
plot(PG$PG.Close)

library(fUnitRoots)
adfTest(coredata(PG$PG.Open), type = 'nc')

#14.23 SOOTHING A TIME SERIES
#you have noisy time series. you want to smooth the data to elemimate the noise
library(KernSmooth)
t=seq(from=-10, to=10, length.out = 201)
noise=rnorm(201)
y=sin(t)+noise

gridsize=length(y)
bw=dpill(t,y, gridsize = gridsize)
lp=locpoly(x=t, y=y, bandwidth = bw, gridsize = gridsize)
smooth=lp$y
plot(y)
plot(smooth)
