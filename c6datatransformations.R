v=c(40,2,83,28,58) # is a form of vector
f=factor(c('A', 'C', 'C', 'B','C')) # is a factor is levels A,B,C

#6.1 Splitting a vector into groups 
library(MASS)
data(Cars93)
split(Cars93$MPG.city, Cars93$Origin) # in data orgins has two levels 
g=split(Cars93$MPG.city, Cars93$Origin)  
median(g[[1]]) # will find the median of USA level which has only one column
median(g[[2]])

#6.2 Apply a function to each list element
lapply(Cars93$Origin, length) # lapply will return result in list
sapply(Cars93$Origin, length) # sapply will return result in vector where s- simplified
sapply(Cars93$Price, mean)
sapply(Cars93$Price, range)

#6.3 Applying a function to every row
apply(mat,1,mean) # apply use row name of your matrix, 1 for row
apply(mat,1,range)

# 6.4 Applying a function to every column
apply(mat,1,mean) # apply use row name of your matrix, 1 for row
apply(mat,1,range)

# 6.5 Applying a function to a group of data
sum(Cars93$Min.Price)
mean(Cars93$Max.Price)
tapply(Cars93$Min.Price, Cars93$Max.Price, sum)

# 6.6 Applying a function to a groups of Rows

by(Cars93,Cars93$Model,summary)
# we are building  a linear models for Width as a function of models of various catagory.
models=by(Cars93,Cars93$Model,function(df) lm(Width ~ Length + Wheelbase, data=df))
# Observe that the parameter to our function is a data frame, so we can use it as the
# data argument of lm. The result is a two-element list of linear models. When we print
# the list, we see model of Model of each brand.
print(models)
# confint function to each list element and see the confidence intervals 
#for each model's coeffieients
lapply(models,confint) 

#6.7 Applying a Function to Paralel Vectors or Lists
gcd(c(1,2,3),c(9,6,3)) # this is not a vectorize 
mapply(gcd,c(1,2,3),c(9,6,3)) # to make vectorize and doing element wise gcd using mapply.


