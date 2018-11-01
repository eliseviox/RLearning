#Load Libraries
library(RODBC)
library(sqldf)
library(reshape2)
library(ggplot2)
library(lubridate)
library(sendmailR)
library(dplyr)
library(RcppRoll)
library(caret)
library(data.table)
library(knitr)

#Create "fake" datatable
donor<-c(rep("A", 6), rep("B",6), rep("C",6))
stim<-rep(c("un","w","p","s","sw","sb"),3)
ifng<-rnorm(18, 2)
tnfa<-rnorm(18,10)
fake<-as.data.table(cbind(donor,stim,ifng,tnfa))
fake$ifng=as.numeric(as.character(fake$ifng))
fake$tnfa=as.numeric(as.character(fake$tnfa))
#Print "fake" datatable 
library(knitr)
kable(fake)

#Create Split, Apply, Combine Function
subtractun<-function (datatable) {
  #Split full data table into smaller data tables for each individual donor 
  y<- split(fake, donor)
  #Subtract out unstim
  newifng<-unlist(lapply(y,function(x)(x$ifng-x$ifng[x$stim=="un"])))
  newtnfa<-unlist(lapply(y,function(x)(x$tnfa-x$tnfa[x$stim=="un"])))
  #Merge donor and stim condition to create new sample ID
  newsampleid<- paste(donor, stim, sep= "_")
  #Create new datatable with unstim substractions applied to all donors 
  datatable <- as.data.table(cbind(newsampleid, newifng, newtnfa))
  #Print datatable
  datatable
  }

#Apply function to data table of interest
subtractun(fake)