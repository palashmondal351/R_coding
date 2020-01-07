#9. GENERAL STATISTIC 

# In statistic there is two hypothesis 1. null hypothesis and 2. p-value
#generally p value 0.05, 

#9.1 SUMMARIZING YOUR DATA
summary(Cars93)
lapply(Cars93,summary) # To summarize list of vector

#9.3 TABULATING FACTORS AND CREATING CONTINGENCY TABLES
table(Cars93$Price) #table function produce count of one factor.
table(Cars93$Horsepower)
# cross-terbulation that how many times row-column combinatin occured
table(Cars93$Price,Cars93$Horsepower) 

#9.4 TESTING CATEGORICAL VARIABLES FOR INDEPENDENCE
#small p value means the two factor has some relationship. 
summary(table(Cars93$Price,Cars93$Horsepower))

#9.5 CALCULATING QUANTILES (AND QUARTILES) OF A DATASET. 
#quantiles function can tell you which observation delimits the lower 5% of the data. 
quantile(Cars93$Price,0.05)
quantile(Cars93$Price,c(0.05,0.95))
quantile(Cars93$Price)

#9.6 INVERTING A QUANTILE
#you want to know that what fraction of the data less than x
mean(Cars93$Price<30)

#9.7 CONVERTING DATA TO Z SCORE
#Z Score of the data means normalizing the data.
scale(Cars93$Price)
(50-mean(Cars93$Price)) / sd(Cars93$Price) # want to normalize 50

#9.8 TESTING THE MEAN OF SAMPLE(tTest)
# you have sample population and you want to know if the mean of the population would be 50
x=rnorm(50, mean = 100, sd= 15)
t.test(x,mu=95)

# 9.9 FORMATING A CONFIDENCE INTERVAL FOR A MEAN.
# you want to determine confidence interval for the population mean. 
x=rnorm(50, mean = 100, sd= 15)
t.test(x)
# you can rise the confidence of interval by setting conf.level= 0.99
t.test(x,conf.level = 0.99)

#9.10 FORMATING A CONFIDENCE INTERVAL FOR A MEDIAN. 
wilcox.test(Cars93$Price, conf.int=TRUE)
wilcox.test(Cars93$Price, conf.level = 0.99, conf.int=TRUE) # median conf level is 99%

# 9.11 TESTING A SAMPLE PROPORTION
#Test the sample data hypothesis aginest p using sample data. 
prop.test(Cars93$Price,93,0.5)
prop.test(11, 20, 0.5, alternative = 'greater')

# 9.12 FORMATING A CONFIDENCE INTERVAL FOR A PROPORTION
prop.test(6,9)

# 9.13 TESTING FOR NARMALITY
#determine wheather the data form normality distributed population. 
shapiro.test( Cars93$Price)

#9.14 TESTING FOR RUNS
# to know is the sequence is random or not
library(tseries)
s=sample(c(0,1),100, replace = TRUE)
runs.test(as.factor(s))

#9.15 COMPARING THE MEAN OF TWO SAMPLE
t.test(Cars93$Price,Cars93$Horsepower)
t.test(Cars93$Price, Cars93$Horsepower, paired = TRUE)

#9.16 COMPARING THE LOCATIONS OF TWO SAMPLES NONPARAMETRICALLY
wilcox.test(Cars93$Price, Cars93$Horsepower, paired = TRUE)

# 9.17 TESTING THE CORRELATION OF SIGNIFICANCE
cor.test(Cars93$Price, Cars93$Horsepower)
cor.test(Cars93$Price, Cars93$Horsepower, method = "spearman")
cor(Cars93$Price, Cars93$Horsepower)
cor.test(Cars93$Price, Cars93$Horsepower)

#9.18 TESTING GROUP FOR EQUAL PROPORTIONS
success=c(14,10)
trials=c(38,40)
prop.test(success, trials)

#9.19 PERFORMAING PAIRWISE COMPARISONS BETWEEN GROUPS MEANS
pairwise.t.test(Cars93$Price, Cars93$Horsepower)

#9.20 TESTING TWO SAMPLES FOR THE SAME DISTRIBUTION
ks.test(Cars93$Price, Cars93$Horsepower)

# cp=Cars93$Price
# sum(cp)
# u=unique(Cars93$Type)
# print(length(u))

