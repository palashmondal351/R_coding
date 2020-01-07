## READING DATA FORM HTML TABELS
# To use the library XML you need to install using command- install.package("XML")
# NOTE: To install the R Packages run first "sudo apt-get install libcurl4-openssl-dev libxml2-dev"
# for XML and Openssl. Set your default path as "/home/palash/R/Rpractice" by using teh command 
# .libPaths("/home/palash/R/Rpractice")

library(XML)
url="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
tbl=readHTMLTable(url)
View(tbl)

## 4.12 Reading file with a complex structure
connection=readLines("Geo_information.txt",n=10)
View(connection)

#AN EXAMPLE OF STATLAB DATA. 

# read data from online, skip=35 lines, readline=23, year read as integer. 
world.series=scan("http://lib.stat.cmu.edu/datasets/wseries",skip = 35,nlines = 23,
                  what = list(year=integer(0),pattern=character(0)),
                  )
world.series$year
# storing list element accroding to the year. 
perm=order(world.series$year)
# storing the list element accroding to the year,pattern
world.series=list(year=world.series$year[perm],
                  pattern=world.series$pattern[perm])

world.series$year
world.series$pattern

## SAVING AND TRANSPORTING OBJECTS
dput(world.series$year,file = "yearData.txt")
dput(world.series$pattern,file = "patternData.txt")

plot(world.series$year) # to plot the year of world.series data




