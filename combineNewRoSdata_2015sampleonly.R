#combine new RoS data into one sheet.
#New RoS data is in CSVs, in matching groups of four files: master, address, applicant and granter.
#2015 sample only - looking to see if these application IDs
#account for any matching in Judita's alignment file.
#I'm currently missing some from the -2014 data.
library(plyr)
library(pryr)

#Can use diff named groups of files
#http://stackoverflow.com/questions/2851327/converting-a-list-of-data-frames-into-one-data-frame-in-r
#Take each of the four groups and combine

#Function for getting dataframe of each type from the folder
getType <- function(type) {

  # listOf <- lapply(Sys.glob(paste("JessieExtDrive/Data/RoS/Test/*",type,"*.csv",sep="")), read.csv)
  listOf <- lapply(Sys.glob(paste("JessieExtDrive/Data/RoS/CVJB/*",type,"*.csv",sep="")), read.csv)
  df <- ldply(listOf, data.frame)
  return(df)

}#end function

# types <- c("master","address","applicant","granter")
# 
# #Get!
# for(type in types) {
#   
#   saveRDS(getType(type),paste("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/",type,".rds",sep=""))  
#   
# }

#Combine just the address files to check on ID match with Judita data
saveRDS(getType("address"),"JessieExtDrive/Data/RoS/RoSNew_combined2015only_fromCVJBfolder/address.rds")  

#Load back in, do some checks
address <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combined2015only_fromCVJBfolder/address.rds")

#Get Judita alignment file
alignment <- read.csv("JuditasLinkingFiles/smi_alignment_v2.txt",sep=" ", header=F)

#How many matching IDs?
address_uniques <- data.frame(unique(address$Application.number))
alignment_uniques <- data.frame(unique(alignment$V6))

#548 - which, when added to the 99272 previously found, gives all unique RoS IDs.
length(alignment_uniques[alignment_uniques[,1] %in% address_uniques[,1],])


#clear memory
rm(list = ls(all = TRUE))
# test <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/address.rds")

