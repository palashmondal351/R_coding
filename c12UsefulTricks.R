## THIS IS CHAPTER 12 OF USEFULL TRICKS 

#12.1  PEEKING AT YOUR DATA
head(randu) # randu is a dataset
tail(randu) # last five row it will display

#12.2 WIDEN YOUR OUTPUT
# to see the actual number of column data
# minimum values of width is 10 and by default it prints outputs 80 char.
options(width = 10)

#12.3 PRINTING THE RESULT OF AN ASSIGNMENT
#sum the rows or columns of your data
rowSums(randu) # row_wise summation
colSums(randu)
tail(rbind(randu, Totals=colSums(randu)))

#12.5 PRINTING DATA IN COLUMNS
# print the several parallel data vector in a columns format
print(cbind(randu$x, randu$z, Total=randu$x+randu$z))

#12.6 BINNING YOUR DATA
# split your data into interval called binning 
x=rnorm(1:1000)
breaks=(c(-3,-2,-1,0,1,2,3))
f=cut(x,breaks)
summary(f)
f=cut(x,breaks, labels = c('Bottom', 'Low', 'Neg', 'Pos', 'High', 'Top'))
summary(f)

#12.7 FINDING THE POSITION OF PARTICULAR VALUE
#you want to know a particluar value occur in the content
vec=c(100,20,30,5,2,5,6,8,90,70,50)
match(50, vec)
which.min(vec) # position of smallest element

#12.8 SELECTING EVERY NTH ELEMENT OF A VECTOR
#you want to select every nth element of a vector
v=Cars93$Min.Price
seq_along(v %% 0.3)
v[seq_along(v %% 0.3==0)]
v[c(FALSE, TRUE)]

#12.9 FINDING PAIRWISE MINIMUM OR MAXIMUM
pmin(1:5, 5:1)
pmax(1:5, 5:1)
wheelbase=Cars93$Wheelbase-100
pmax(wheelbase, 0) # to make negative value to 0

#12.10 GENERATING ALL COMBINATIONS OF SEVERAL FACTORS
#generate all the combinations of two or more data frames. 
expand.grid(randu$x, randu$y)
expand.grid(Cars93$Model, Cars93$Price)

#12.11 Flatten a data frame
# To process all the element together
mean(ChickWeight$weight) # make sure all the dataframe value is numeric
mean(as.matrix(ChickWeight[,1:3]))

#12.12 SORTING A DATA FRAME
print(ChickWeight)
order(ChickWeight$weight) # how to rearange weight is ascending order
ChickWeight[order(ChickWeight$weight),] # order rearranging by weight

#12.13 SORTING BY TWO COLUMN
ChickWeight[order(ChickWeight$weight, ChickWeight$Time),] # Inside sorted weitht time is sorted

#12.14 STRIPPING ATTRIBUTES FROM A VARIABLE
# attributes(x)=NULL
# attributes(x,'attributename')=NULL
m=lm(ChickWeight$weight ~ ChickWeight$Time)
slope=coef(m)[2]
slope
str(slope)
attributes(slope)=NULL
str(slope) # here you can notic that name attributs is gone
slope  # now the number prints without lables

#12.15 REVEALING THE STRUCTURE OF AN OBJECT
# our function returning something and you want to look indide int and want to learn more. 
m=lm(ChickWeight$weight ~ ChickWeight$Time)
print(m)
class(m) #determine the object of class
mode(m) # reveles the understructure sturcture
names(m)
m$coefficients
str(m) # internal sturcture of any valriable

#12.16 TRIMING YOUR CODE
#to know the program execution tiem of your code
system.time(sum(rnorm(10000000)))

#12.17 SUPPRESSING WARNINGS ERROR MESSAGES
library(tseries)
adf.test(ChickWeight$weight) # it will give output with warning message
suppressWarnings(adf.test(ChickWeight$weight)) # it will suppress the warning message
warning(adf.test(ChickWeight$weight)) # show only warning message

#12.18 TAKING FUNCTION ARGUMENTS FROM A LIST
#you want to pass the data to a function but the function does not acdept a list
vec=c(1,3,5,7,9)
mean(vec)
vect=list(1,3,5,7)
mean(vect) # list can't calculate mean, need to convert it as a vector
mean(unlist(vect))

#12.19 DEFINE YOUR OWN BINARY OPERATOR
# any text between % % will take as a binary operator
'%+-%' = function(x, margin) x + c(-1,+1)*margin #less portable in others environment
100 %+-% 1.96*15








