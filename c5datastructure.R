# THIS CHAPTER ABOUT "DATA STRUCTURE"

# In R does data Structure in different way instead of traditioal way.
v=c(10,20,30)
names(v)=c('Palash', 'Jaya', 'Shivani')
print(v)
print(v['Jaya'])

# R list are hetorogeneous and it's index start from 1. 
mode(pi)
mode(c(2.7128,3.1415)) # numeric
mode(list('Palash','Jaya','Shivani')) #list
d=as.Date('09-09-19')
mode(d)
length(d)
class(d)

#MATRIX 
a=1:6
# print(a[1:6]) # print(a) both will give same answer
dim(a)=c(2,3) # it turns the a element in a vector form
print(a)
b=list(1,2,3,4,5,6)
dim(b)=c(2,3)
print(b)
# NOTE: Matrix fill column value first. 
c=1:12
dim(c)=c(2,3,2) #creates an 2 dimensional matrix of size 2,3
print(c)
# appending data to the data frame
v=c(1,2,3,4,5)
v=c(v,6) # appended 6 to v
print(v)
w=c(11,12,13,14,15)
v=c(v,w)
print(v)
v[20]=20 # in 20 position we are adding 20.
print(v)
# inserting data into a vector
append(v,w,after = 7) # w will be appended after 7 position
v1=c(1,2,3,4,5)
v2=c(11,22,33) # here v2 will be recicle one time of 11, 22
print(v1+v2)
(1:5)+10  # addition 10 with each element

# factor in R
f=factor(c('win','loss','win','win','tie','loss','win'))
print(f) # will print the catagory of type 3, 'win', 'loss', 'tie'.

# creating a list
lst=list(0.5,0.841,0.977)
print(lst)
lst=list(first=0.5, mid=0.841,last=0.977) # naming the list element
print(lst)
# selecting list element by name
coder=list(Gennady=1, Pert=2, Palash= 3)
print(coder[['Gennady']]) # print(coder$Gennady) both statement are same
# Removing an element form a list
print(coder)
coder$Pert=NULL # will remove the Pert form the list
#Fatten a list into a vector
elelist=list(c(1,2,3,4,5,9,8,7,6))
mean(unlist(elelist)) # list can't do operation on mean so you need to do unlist
                      # and make a vector

# Removing null element from a list
lis=list(c(1:6))
lis[10]=10
print(lis) #now list have NA element
lis[sapply(lis, is.null)]=NULL 
# Removing list element with condition
l=list(-5,-3,-1,0,1,3,5,7,9, 0,NA)
print(l)
l[l<0]=NULL # Remove all the negative value form list
l[is.na(l)]=NULL # Remove all the NA value form list
print(l)

# MATRIX
m=c(1:12)
mat=matrix(m,3,4,byrow = TRUE) # Matrix follws by default column order
print(mat)
mat=matrix(NA,3,4) # NA element matrix of 3*4 dimension
print(mat)
# matrix operation
t(mat) # transpose of matrix
solve(mat) # matrix inversion
diag(4) # diagonal identity matrix of 4*4
mat2=matrix(c(11:22),3,4)
mat * mat2 # mat %*% mat2 matrix element wise multiplication

# given row and column name to a matrix
rownames(mat)=c('r1','r2','r3')
colnames(mat)=c('c1','c2','c3','c4')
print(mat)

# selecting row or columns in a marix
print(mat[1,]) # first row of matrix mat
print(mat[,3]) # third column of matrix mat
print(mat[1,,drop=FALSE]) # first row of matrix mat wih element position
print(mat[,3,drop=FALSE]) # third column of matrix mat with element position

# Data frame from column data
r4=data.frame(c1=13,c2=14,c3=15,c4=16,c5=17) # row that we want to append in mat data set
mat=rbind(mat,r4)
print(mat) # get a appended row data set or new row newrow
c5=c(101,201,301,401,501) # append a new column to the data 
mat=cbind(mat,c5)
print(mat)
# selecting data from column position
mat[[1]] # first element of first column
mat[c(1:3),] # row 1-3 of all column
mat[,1] # first row
mat[,c(1:3)] # column 1-3 of all rows

# LOAD DATA FROM PACKAGE Cars93 data from MASS packages
library(MASS)
data("Cars93")

# selecting row and column more easily by name and condition apply. 

# select Model from Car93 data where MPG.city> 30
subset(Cars93,select = Model, subset = (MPG.city > 30)) 
subset(Cars93,select=c(Model, Manufacturer), subset = c(MPG.highway > median(MPG.highway))) 

#changing name of data frame column and row
print(mat)
colnames(mat)=c('col1','col2','col3','col4','col5')
rownames(mat)=c('row1','row2','row3','row4')

# Excluding column by its name
subset(mat,select = -col3) # col3 frame will be excluded from the data. 

#combining by rows and columns
rownames(mat2)=c('row1','row2','row3')
colnames(mat2)=c('col1','col2','col3','col4')
rbind(mat[1:3,1:4],mat2) # column should be match
cbind(mat[1:3,1:3],mat2) # row should be match

# marge data frame by common name
margedata=merge(mat[1:3,1:4],mat2,by='col3') # marge my 'col3' 

# Data frame operation
z=(mat$col1 - mean(mat$col1)) / sd(mat$col1) 
print(z)

# Data type conversion
as.numeric(3.14)
as.integer(3.14)
as.double(3.14)
as.character(3.14)

#convertion one sturcture data type to another
as.data.frame(v) # v converted to data frame
as.data.frame.array(v) # error
as.data.frame.character(v)
as.data.frame.list(v)
as.matrix(v)
as.vector(v)

