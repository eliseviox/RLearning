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

#Set working directory
setwd("/Users/eviox/Documents/Emory_IMP/Rotations/Day_Lab/Luminex/Luminex_RawData")

#Read in files
files = list.files(pattern="*.csv")
newfiles = c("a.csv","b.csv")

#Data formatting all files
cleandata <- function(importname, exportname) 
  
{
  
  #Read in file
  datasetv1 <- read.csv(importname, header=TRUE, skip=4, sep=",")
  
  #Create new header from Rows 1 and 2
  n <- names(datasetv1)
  row1 <- as.matrix (datasetv1[1,])
  new_head<- paste(n, row1, sep= "_")
  colnames(datasetv1)<- (c(new_head))
  
  #Remove "unknown" Analyte..Sample entries for empty Luminex wells
  datasetv1$Analyte..Sample_[grepl("Unknown",datasetv1$Analyte..Sample_)]<-""
  
  #Rename "Analyte..Sample" to "Sample"
  datasetv2 <- rename(datasetv1, Location=X.Location_, Sample=Analyte..Sample_)%>%
    filter(Sample!="")%>%
    select(-X_)
  
  #Eliminate <, >, ↓ from table
  datasetv3<- data.frame(lapply(datasetv2,function(x) {
    gsub("<|>|↓|↑","", x)
  }))
  
  #Generate New CSV
  write.csv (datasetv3, file=exportname)
  
}

#Apply For Loop Function to Raw Data Files
cleandata("Luminex_PracticeFile1_Raw.csv", "/Users/eviox/Documents/Emory_IMP/Rotations/Day_Lab/Luminex/Luminex_CleanData/Luminex_PracticeFile1_Clean.csv")
cleandata("Luminex_PracticeFile2_Raw.csv", "/Users/eviox/Documents/Emory_IMP/Rotations/Day_Lab/Luminex/Luminex_CleanData/Luminex_PracticeFile2_Clean.csv")

#Re-read in New CSV to convert factor to numeric
##Set working directory
setwd("/Users/eviox/Documents/Emory_IMP/Rotations/Day_Lab/Luminex/Luminex_CleanData")

##Read in files
cleaneddata = list.files(pattern="*.csv")
fakeluminex<-read.csv(file="Luminex_PracticeFile1_Clean.csv", header=TRUE, sep=",")

#Create Split, Apply, Combine Function
datatable <- fakeluminex
column<- c("GM.CSF_pg.ml","IFNg_pg.ml","IL.10_pg.ml", "IL.17a_pg.ml","IL.22_pg.ml","IP.10_pg.ml", "MIP.1b_pg.ml", "TNFa_pg.ml")

for (i in 1:ncol(fakeluminex)){
  if(is.numeric(fakeluminex[[i]])){
subtractun<-function (datatable,column) {
  #Split full data table into smaller data tables for each individual donor 
  y<- split(datatable,Donor_)
  #Subtract out unstim
  newcolumn<-unlist(lapply(y,function(g)
    (g[,..column]- as.matrix(subset(g[,..column], g$Sample=="unstim"))[1,1])
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
subtractun(fakeluminex, "IFNg_pg.ml")


##combine 
#for i in 1:ncol (datatable)
#if numeric
#apply our function