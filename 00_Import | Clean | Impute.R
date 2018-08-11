library(tidyquant)
library(skimr)
library(mice)

#Import
mydata<-read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data',header = FALSE,
stringsAsFactors = TRUE,na.strings = " ?")

mydata2<-read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test',header = FALSE,
stringsAsFactors = TRUE,na.strings =" ?",skip = 1)

#Bind
adult <-rbind(mydata,mydata2)

#Import the column names
col_names <- read.csv2('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/old.adult.names',skip = 60,
nrows = 15,col.names = 'colnames')

#Clean them
col_names$colnames <- gsub(':.+|-','',col_names$colnames)

#And fill the headers of "adult" data set

colnames(adult)<-col_names$colnames

adult<-adult %>% select(-educationnum)

#Dealing with NAs

library(VIM)

#Which columns have NAs

skim(adult) 

adult %>% complete.cases() %>% sum() / nrow(adult) 

md.pattern(adult)

#Visualising the structure of NAs

aggr(adult, col=c('navyblue','yellow'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(adult), cex.axis=.7,
     gap=3, ylab=c("Missing data","Pattern"))

#Use mice for filling the NAs

tempdata<-mice(adult, m=5, maxit=5,method = 'pmm',seed = 500)

adult<-complete(tempdata,1)

