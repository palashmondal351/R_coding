# 7.1 Length of a string
nchar('palash')
s=c('palash','Mondal','RcookBook','Statistic')
nchar(s) # length of string in vector 
#NOTE: length will return the length of a vector

# 7.2 concatinate a string
paste('R','Python')
v=c('R','Python')
paste(v,'is a', 'language')
paste(v,'is a', 'language', collapse = ' and ') # join both sentence with and

# 7.3 EXTRACTING SUB-STRING
substr('R programming', 1, 20) # index start from 1 
substr(v,1,2) # return result inbetween start and end index inclusive
city=c('New Delhi, DHL', 'Kolkata, CCU','Bengaluru, BLR')
substr(city,nchar(city)-2,nchar(city))

#7.4 Spliting a string accroding to delimiter
s='/home/palash/R/Rprogramming'
strsplit(s,"/")
st='It is a string and programmng on string is amazing and i like that on string operation'
sub('string','STRING',st) #sub replace only first occurence
gsub('string','STRING',st) #gsub replace all occurence

#7.6 SEEING A SPECIAL CHARACTER IN A STRING
s='first\rsecond\n'
nchar(s) # return total length of the string
cat(s) # will return printable string only

# 7.7 Generating all pairwise combinations of strings
c1=c('Kolkata','Hydrabad', 'Bengalore')
c2=c('CCU','HYD', 'BLR')
outer(c1,c2,paste,sep='-')

#7.8 Getting the current date
Sys.Date()
class(Sys.Date())

#Convert a string into a Date
as.Date(Sys.Date())
as.Date("9/12/2019", foramt='%m%d%Y')

#Converting year, month, date into date
ISOdate(2019,9,29) #date as an international format of GMT
as.Date(ISOdate(2019,9,12))
year=c(2017,2018,2019)
month=c(1,2,3)
date=c(14,15,16)
ISOdate(year,month,date)
as.Date(ISOdate(year,month,date))

#7.12 Getting the Julian Date
d=as.Date('2010-03-15')
as.integer(d) # julian date start from first day of 1970
julian(d)

# 7.13 Extracting the part of Date
d=as.Date('2010-03-15')
p=as.POSIXlt(d)
p$mday
p$mon
p$year+1900

# 7.14 Creating a sequence of Dates
s=as.Date('2019-09-01')
e=as.Date('2019-09-12')
seq(from=s, to=e, by=1) # date of 1-12 of 09 month of 2019
seq(from=s,by='month',length.out = 12) # each month first date of 1 year
seq(from=s,by='3 month',length.out = 4) # quartly date of one year











