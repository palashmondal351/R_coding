# Getting Data 
CAR_CRASH<- read.csv("infinizifl.csv")

# Question 1(a)
CAR_CRASH$TimeofDay= CAR_CRASH$Hour*100 + CAR_CRASH$Minutes
head(CAR_CRASH)

#Question 1(b) 
mn=min(CAR_CRASH$TimeofDay)
mx=max(CAR_CRASH$TimeofDay)
lc_car <-subset(CAR_CRASH, CAR_CRASH$Colour=="light")
lc_car_Density <-density(lc_car$TimeofDay,from = mn,to = mx)
plot(lc_car_Density, xlab = 'Time of the Day (Minute)',
     main="Light Colour Car Crash Kernal Density",lwd=2, col='blue' )

dc_car <-subset(CAR_CRASH, CAR_CRASH$Colour=="dark")
dc_car_Density <-density(dc_car$TimeofDay,from = mn,to = mx)
plot(dc_car_Density, xlab = 'Time of the Day (Minute)',
     main="Dark Colour Car Crash Kernal Density",lwd=2, col='brown')
##########################################################################
#Question 1(c)

pred<-function(time){
  f = density(CAR_CRASH$TimeofDay, from=mn, to=mx)
  f_light = density(CAR_CRASH$TimeofDay[CAR_CRASH$Colour=="light"],from=mn, to=mx)
  f_dark = density(CAR_CRASH$TimeofDay[CAR_CRASH$Colour=="dark"],from=mn, to=mx)
  p = mean(CAR_CRASH$Colour=="dark")
  plot(f$x, f_dark$y*p/(p*f_dark$y+(1-p)*f_light$y), type='l', xlab='Time', ylab='P(dark|time)')
  abline(v=time, col='blue')
  abline(h=0.5,lty=3)
  #prob<-f1$y*p/(p*f1$y+(1-p)*f0$y)
  p_dark = f_dark$y[round(f_dark$x)==time]*p/(p*f_dark$y[round(f_dark$x)==time]+(1-p)*f_light$y[round(f_light$x)==time])
  
  return (ifelse(p_dark<0.5,'light','dark'))
}
pred(1200)

