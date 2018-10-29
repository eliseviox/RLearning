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
datatable <- fake
column<- "ifng" 

for (i in 1:ncol(fake)){
  if(is.numeric(fake[[i]])){
    subtractun<-function (datatable,column) {
      #Split full data table into smaller data tables for each individual donor 
      y<- split(datatable, donor)
      #Subtract out unstim
      newcolumn<-unlist(lapply(y,function(g)
        (g[,..column]- as.matrix(subset(g[,..column], g$stim=="un"))[1,1])
      ))
      ##Merge donor and stim condition to create new sample ID
      ###newsampleid<- paste(donor, stim, sep= "_")
      #Create new datatable with unstim substractions applied to all donors 
      datatable[,column] <- newcolumn
      datatable
    }
  }
}
#Apply function to data table of interest
subtractun(fake, "ifng")


##combine 
#for i in 1:ncol (datatable)
#if numeric
#apply our function