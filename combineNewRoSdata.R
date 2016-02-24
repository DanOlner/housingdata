#combine new RoS data into one sheet.
#New RoS data is in CSVs, in matching groups of four files: master, address, applicant and granter.
#Not yet sure what matches up with what, column-wise...
library(plyr)
library(pryr)

#Can use diff named groups of files
#http://stackoverflow.com/questions/2851327/converting-a-list-of-data-frames-into-one-data-frame-in-r
#Take each of the four groups and combine

#Testing for which groups do/do not have column matches
#132 files with "master" in name
listOf <- lapply(Sys.glob(paste("JessieExtDrive/Data/RoS/LPD2/*master*.csv",sep="")), read.csv)

#For master, it appears it's only the first two that don't match the rest.
#Ah wait: 110 - has some random additional columns. That might not be a problem for combining.
lapply(listOf, colnames)

#Function for getting dataframe of each type from the folder
getType <- function(type) {

  # listOf <- lapply(Sys.glob(paste("JessieExtDrive/Data/RoS/Test/*",type,"*.csv",sep="")), read.csv)
  listOf <- lapply(Sys.glob(paste("JessieExtDrive/Data/RoS/LPD2/*",type,"*.csv",sep="")), read.csv)
  df <- ldply(listOf, data.frame)
  return(df)

}#end function

types <- c("master","address","applicant","granter")

#Get!
for(type in types) {
  
  saveRDS(getType(type),paste("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/",type,".rds",sep=""))  
  
}

#The master file contains different fields for a couple of the first ones and the rest.
#Most fields overlap, but those first couple have "price", and the rest have:
# "Consideration" "Value" "Land.Classification" "Date.of.Entry" "Subject.Prefix" "Minimum.Easting"    
# "Minimum.Northing" "Maximum.Easting" "Maximum.Northing"

#There are also a bunch of empty NA columns that get picked up from one odd file. They're not needed
#So reload, drop them, save again.
master <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/master.rds")
master <- master[,1:19]
saveRDS(master, "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/master.rds")

#Check: do price and consideration both mean there is a price for all obs?
#Yes.
all(nrow(master[is.na(master$Consideration),]) + nrow(master[is.na(master$Price),])==nrow(master))

#clear memory
rm(list = ls(all = TRUE))
# test <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/address.rds")

