#Processing file.
#Filter New RoS master file down to single-instance title/dates
#Process dates of entry, including imputing missing
#Merge with address file
library(plyr)
library(pryr)
library(zoo)

#Main file to use.
#Only addition here to processing done in 'combineNewRoSdata' is
#application date field has been formatted in a new field
master <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/master.rds")

#####################
# CORRECT EARLY FIELDS WITH NO LAND CLASSIFICATION
# ADD IN RESIDENTIAL
#The raw data contains two months at the start with different fields. 
#It *doesn't* have a land.classification field, so can't tell which are residential.
#RoS data is still that way, it's not fixable from their end. 
#But they've supplied a sheet containing only residential properties
#Can use this to mark applications as residential before the following filter
earlyResidFlags <- read.csv("JessieExtDrive/Data/RoS/RoS_newMisc/CopySheffieldApr-May2003-1.csv")

#mark master land.classification for those application numbers
#Tested in "Testing first-two-month residential flag"
#examineProcess_RoS_data.R
#It matches with 5 beyond the two first months but they're R already
#And the rest of the NA replacements check out.
#20730 gain an R field.
#28319
# nrow(master[is.na(master$Land.Classification),])

master[master$Application.number %in% earlyResidFlags$Application_ID,
           c('Land.Classification')] <- "R"

#7589
# nrow(master[is.na(master$Land.Classification),])

########################
# FILTERS
#
#Filter out just to the categories that we want to use
#Which were: residential, then FR/DW (first registration or dealings with whole)
#Note: 'residential' this filters out the first two months with the different fields
#As it doesn't have a Land.Classification field
#This may change if we get updated original data...
master_resid <- master[master$Land.Classification=="R" 
                       & !is.na(master$Land.Classification),]

master_resid_DWFR <- master_resid[(master_resid$Application.type=="FR" |
                                     master_resid$Application.type=="DW")
                                  & !is.na(master_resid$Land.Classification),]

#Drop deed code fields 48 and 49. Via Sue:
#Notice of Payment of Improvement Grant
#Notice of Payment of Repairs Grant
#Council works
master_filtered <- master_resid_DWFR[master_resid_DWFR$Deed.codes!="48"
                                     & master_resid_DWFR$Deed.codes!="49",]

#Keep just the latter
# rm(list = c('master','master_resid','master_resid_DWFR'))


#######################
# REPLACE MISSING DATES OF ENTRY
# WITH IMPUTED VALUE FROM APPLICATION DATE
# See examineProcess_RoS_data.R
# Section "MORE DATE OF ENTRY TESTING: GET IMPUTED DATE FOR MISSING FROM APPLICATION DATE"
# Three weeks prior is a reasonable mean difference in existing date of entries vs application dates

# 0       2       8      10 
# 20144   18101   79271 1125693 
#Converting to character makes NAs two-string...
table(nchar(as.character(master_filtered$Date.of.Entry)))

#So we're targetting 0 and 2: 38245 obs
#Will have to process each section separately then re-bind
#In order to avoid hassle with processing in the next section

#Get the NA/empty date of entries for processing here
missings <- master_filtered[master_filtered$Date.of.Entry == "" 
                            |is.na(master_filtered$Date.of.Entry),]

#Using TEMP2 to temporarily store processed date of entries
#Imputed date is three weeks prior to application date based on mean
#of distance of correct entries
missings$TEMP2 <- missings$applicationDateFormatted - 21

#Mark with flag indicating these are imputed
missings$imputedDateOfEntry <- 1

#All present and correct.
# table(nchar(as.character(missings$TEMP2)))
#On to processing the rest.

#Get rid of empty/NA date of entries before processing the existing date of entries in the next section
master_filtered <- master_filtered[nchar(as.character(master_filtered$Date.of.Entry))!=0,]

#For testing, I just need to temporarily drop date of entries with NA
master_filtered <- master_filtered[!is.na(master_filtered$Date.of.Entry),]

########################
########################
# PROCESS DATES-OF-ENTRY
#

#For different formatting of date field (some have 4-digit year, some 2.)
master_filtered$strLength <- nchar(as.character(master_filtered$Date.of.Entry))

#Consistent date format, before filtering out nonsensical ones / fixing fixable ones.
#For those with four digit year
master_filtered$dateOfEntryFormatted <- as.Date(master_filtered$Date.of.Entry, format='%d/%m/%Y')

#Two-digit years (small %y rather than %Y)
master_filtered$dateOfEntryFormatted[master_filtered$strLength==8] <- 
  as.Date(master_filtered$Date.of.Entry[master_filtered$strLength==8], format='%d/%m/%y')

#For viewing...
master_filtered <- master_filtered[order(master_filtered$dateOfEntryFormatted),]

#Year-only field from formatted date of entry
#For easily finding the weird ones
master_filtered$year <- substr(master_filtered$dateOfEntryFormatted,1,4)

#257
# nrow(master_filtered[master_filtered$year=='0200',])
#Not obvious for any of these what the correct year would be
#Dropping for now. Could impute later based on application date
master_filtered <- master_filtered[master_filtered$year!='0200',]

#For the remaining entries, assume the first two digits of the year are incorrect
#Remove, then format date based on two-digit year
#Some will be wrong but those ones are problematic to match anyway and should probably be dropped

#Mark the remaining entries that have non-sensible dates 1, 0 otherwise
master_filtered$sillyDates <- 0 + (
  master_filtered$dateOfEntryFormatted < '1800-01-01'
  | master_filtered$dateOfEntryFormatted > '2016-01-01')

#825 silly remaining
#table(master_filtered$sillyDates)

#Copy over all dates before over-writing the silly ones with something more sensible
#Needs to not be date field or the next part adds back an "00" at the start
master_filtered$TEMPprocessing_dateOfEntryFormatted <- as.character(master_filtered$dateOfEntryFormatted)

#For the silly ones, get rid of the first two digits of the year
master_filtered$TEMPprocessing_dateOfEntryFormatted[master_filtered$sillyDates==1] <- 
  substr(master_filtered$dateOfEntryFormatted[master_filtered$sillyDates==1],3,10)

#825 sillies still there, minus their first two digits...
# table(nchar(as.character(master_filtered$TEMPprocessing_dateOfEntryFormatted)))

#Get strLength again for new field
master_filtered$strLength <- nchar(as.character(master_filtered$TEMPprocessing_dateOfEntryFormatted))

#Convert back to date in two stages, as above
master_filtered$TEMP2 <- as.Date(master_filtered$TEMPprocessing_dateOfEntryFormatted, format='%Y-%m-%d')

#Two-digit years
master_filtered$TEMP2[master_filtered$strLength==8] <- 
  as.Date(master_filtered$TEMPprocessing_dateOfEntryFormatted[master_filtered$strLength==8], format='%y-%m-%d')

#1903-2067
# range(master_filtered$TEMP2)
# hist(master_filtered$TEMP2, breaks='year')

#order for inspection
master_filtered <- master_filtered[order(master_filtered$TEMP2),]

#Remaining dates of entry outside of a reasonable timeframe
#Theories:
#Some are from first registrations so sell dates may be very old
#Others look like wrong data entry
#But between them it's hard to be sure what the correct year could be, so best exclude
#for deciding range to drop ->
table(
master_filtered$TEMP2 < '1990-01-01'
| master_filtered$TEMP2 > '2016-01-01')

################################
# JOIN PROCESSED DATES OF ENTRY
# WITH IMPUTED DATES OF ENTRY
colnames(master_filtered)
colnames(missings)

#drop columns from master_filtered no longer required
master_filtered <- master_filtered[,c(1:20,26)]

#Add in a flag field for whether imputed
master_filtered$imputedDateOfEntry <- 0

#Combine
master_does <- rbind(master_filtered,missings)

#Rename date of entry field
names(master_does)[names(master_does)=='TEMP2'] <- 'DateOfEntryFormatted'

#       0       1 
# 1204707   38245
table(master_does$imputedDateOfEntry)

#Save this! 
#Haven't restricted dates of entry any more now - their range is still 1903-2067
#But let's save
# saveRDS(master_does,"JessieExtDrive/Misc_RoS_R_Saves/master_filter_datesOfEntryProcessed.rds")
master_does <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/master_filter_datesOfEntryProcessed.rds")

##########################################
# FILTER DUPLICATE TITLE/APPLICATION DATES 
# See examine_Process_RoS_data.R
# "SOME MORE CHECKS AFTER CREATING master_filter_datesOfEntryProcessed.rds"

master_does$Application_yearmon <- as.yearmon(master_does$applicationDateFormatted)

#14315 less obs in total
master_does_singleInstances <- subset(master_does, 
               !duplicated(master_does[,c("Title.number","Application_yearmon")])
               & !duplicated(master_does[,c("Title.number","Application_yearmon")],fromLast = T))

#Seems to be right
#nrow(unique(master_does_singleInstances[,c("Title.number","Application_yearmon")]))

#Title/yearmon single instances saved
saveRDS(master_does_singleInstances,
        "JessieExtDrive/Misc_RoS_R_Saves/master_filter_datesOfEntryProcessed_singleInstances.rds")
master_does_singleInstances <- 
  readRDS("JessieExtDrive/Misc_RoS_R_Saves/master_filter_datesOfEntryProcessed_singleInstances.rds")


######################
# Add extra fields from the address file
# "Master" does have addresses but all in one column
# Address file has the address parts broken into separate fields
address <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/address.rds")

#112 don't match
nrow(master_does_singleInstances[master_does_singleInstances[,2] %in% address[,2],])

#get unique address apps (a lot of sequence repeats)
address_uniques_twofields <- address[row.names(unique(address[,c('Title.number','Application.number')])),]

#Confirming same number of matches using unique
nrow(master_does_singleInstances[master_does_singleInstances[,2] %in% address_uniques_twofields[,2],])

#Keep all the master: the 112 non-matching fields will just have NA for new address fields
names(master_does_singleInstances)
names(address)

#Mark original date fields
names(master_does_singleInstances)[names(master_does_singleInstances)=='Application.date'] <- 'orig.application.date.field'
names(master_does_singleInstances)[names(master_does_singleInstances)=='Date.of.Entry'] <- 'orig.date.of.entry.field'

#Slightly more non-matches (307) by matching on both title and application
#But I'd rather do that than match across just title.
#Keeping all master records
mdsi_plus_address <- merge(master_does_singleInstances, 
                           address_uniques_twofields[,c(1:18)], 
                           by=c('Title.number','Application.number'),
                           all.x = T)

saveRDS(mdsi_plus_address, "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mdsi_plus_address.rds")
mdsi_plus_address <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mdsi_plus_address.rds")

#Save as CSV also
write.csv(mdsi_plus_address, "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mdsi_plus_address.csv")


