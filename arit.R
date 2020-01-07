#!/usr/R/R practice --slave

argv=commandArgs(TRUE)
x=as.numeric(argv[1])
y=as.numeric(argv[2])

cat("X=",x,"\n")
cat("Y=",y,"\n")
cat("X+Y=",x+y,"\n")
cat("X*Y=",x*y,"\n")
cat("X%Y=",x%%y,"\n")
cat("X+Y=",x/y,"\n")