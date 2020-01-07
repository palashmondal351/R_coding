# how to read a csv file
dff=read.csv("facebook.csv",TRUE,",")
class(dff)
head(dff)
View(head(dff))
View(str(dff))

## Read csv from web data
rcsv=read.csv("http://insight.dev.schoolwires.com/HelpAssets/C2Assets/C2Files/C2ImportCalEventSample.csv")
View(head(rcsv))

## Read txt file from online
rtxtt=read.table("https://www.w3.org/TR/PNG/iso_8859-1.txt",TRUE,",")
View(head(rtxtt))