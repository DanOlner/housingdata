library(readstata13)
library(plyr)

#Checking first file----
oldRoS <- read.dta13('C:/Users/SMI2/Dropbox/WindFarms/Address Matching/data_geocodes_matched.dta')

#Argh, no title number! Let's check if postID is unique to properties
postIDs <- subset(oldRoS, 
                         duplicated(oldRoS[,"postID"])
                         |duplicated(oldRoS[,"postID"],fromLast = T))

postIDs <- postIDs[order(postIDs$postID),]

#No, can be e.g. flats in same building. ID?
#Noooo, that's just an ID!
length(unique(oldRoS$id))
length(unique(oldRoS$postID))

#check original files
#rm(list=ls())

#geocode unique to property, not postcode?
#Yup
chk <- oldRoS[,c('Eastings','Northings','frozen_pcode_04')]
chk <- chk[order(chk$frozen_pcode_04),]


#Checking the other RoS files----
oldRoS4 <- read.dta13('C:/Data/Housing/StephanHeblich_oldRoS/original/postIDTOrosID_merged_1to21.dta')

length(unique(oldRoS4$id))
length(unique(oldRoS4$rosID))
length(unique(oldRoS4$postID))

#Ah ha: this one appears to have title number in
oldRoS2 <- read.dta13('C:/Data/Housing/StephanHeblich_oldRoS/original/SC_AllAddresses.dta')

#Oh no it doesn't! It's postcode.
oldRoS2$PC[sample(1:nrow(oldRoS2),250)]


#oldRoS <- read.dta13('C:/Data/Housing/StephanHeblich_oldRoS/original/sale.dta')
colnames(oldRoS)

#via windfarms duke folder
#oldRoS <- read.dta13('StephanHeblich_oldRoS/ROSCleanData_Matched/RoS19902010_not_geocoded_match.dta')

RoS_oldJessie <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted.rds")

#OK, let's try doing some matching on a few fields
colnames(oldRoS)
colnames(RoS_oldJessie)

class(oldRoS$year_txt)
class(RoS_oldJessie$year)
#so we need a matching year text field
RoS_oldJessie$year_txt <- as.character(RoS_oldJessie$year)

rossub <- oldRoS[,c('Eastings','Northings','strno','frozen_pcode_04')]
# rossub <- oldRoS[,c('Eastings','Northings','strno','street','town','frozen_pcode_04')]

colnames(rossub)[colnames(rossub)=="frozen_pcode_04"] <- "pcode"

#This first completely floods memory...
#mrg <- join(RoS_oldJessie, rossub, by =c('strno','pcode'), type='left')
mrg <- join(RoS_oldJessie, rossub, by =c('strno','pcode'), type='left', match='first')

#We have some matches
nrow(mrg[!is.na(mrg$Eastings),])



















