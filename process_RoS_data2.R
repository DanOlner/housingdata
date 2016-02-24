library(readstata13)
library(plyr)
library(pryr)
library(zoo)

#This version: having done all the testing in examineProcess_RoS_data.R
#Can get the processing boiled down to its essentials here.
#Have to repeat the tests cos it looks like I used the wrong date field
#I used application date, not date of entry. 
#Date of entry: "The whole rights of ownership pass or are deemed to pass as at the date of entry"
#So in theory is the actual sell date, which is what should match against date in old RoS
#But I'm puzzled by the number that did match... we shall see!

master <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/master.rds")
address <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/address.rds")
applicant <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/applicant.rds")
granter <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/granter.rds")

#save CSV versions for Sue
# write.csv(master, "JessieExtDrive/Data/RoS/CSVversions_RoSNew_Master_Address_Applicant_Granter/master.csv")
# write.csv(address, "JessieExtDrive/Data/RoS/CSVversions_RoSNew_Master_Address_Applicant_Granter/address.csv")
# write.csv(applicant, "JessieExtDrive/Data/RoS/CSVversions_RoSNew_Master_Address_Applicant_Granter/applicant.csv")
# write.csv(granter, "JessieExtDrive/Data/RoS/CSVversions_RoSNew_Master_Address_Applicant_Granter/granter.csv")

RoS_old <- read.dta13("JessieExtDrive/STATA_analysis/RoS/RoS19902010.dta")
#use date-formatted-properly version
RoS_old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted.rds")

#How many matching applicants and granters do we have?
#(In terms of application ID)
app_uniques <- data.frame(unique(applicant$Application.number))
granter_uniques <- data.frame(unique(granter$Application.number))
master_uniques <- data.frame(unique(master$Application.number))
address_uniques <- data.frame(unique(address$Application.number))

#Before starting on dates, let's do the filtering just to the categories that we want to use
#Which were...
#Note: just noticed, this filters out the first two months with the different fields
#As it doesn't have a Land.Classification field
#(Which is why the NAs disappear below)
master_resid <- master[master$Land.Classification=="R" 
                       & !is.na(master$Land.Classification),]


master_resid_DWFR <- master_resid[(master_resid$Application.type=="FR" |
                                     master_resid$Application.type=="DW")
                                  & !is.na(master_resid$Land.Classification),]

#Processing date of entry to be consistent format
#For ease of viewing
masterdatesonly <- master_resid_DWFR[,c("Application.date","Date.of.Entry")]

#28319
nrow(master[is.na(master$Date.of.Entry),])
#0. Excellent! (Or not - see above re. NAs from first two files)
nrow(master_resid_DWFR[is.na(master_resid_DWFR$Date.of.Entry),])

#Now, what's the format look like?
master_resid_DWFR$strLength <- nchar(as.character(master_resid_DWFR$Date.of.Entry))

#0       8      10 
#41971   79273 1125718 
table(master_resid_DWFR$strLength)

#Just looking at 8s
# master_resid_DWFR[master_resid_DWFR$strLength==8,c('Date.of.Entry')]

#What are the zeroes? Testing various columns
master_resid_DWFR[nchar(as.character(master_resid_DWFR$Date.of.Entry))==0,c("Consideration")]

#Drop deed code fields 48 and 49. Via Sue:
#Notice of Payment of Improvement Grant
#Notice of Payment of Repairs Grant
#Council works
#So filter those out, what do the dates look like then?
master_filtered <- master_resid_DWFR[master_resid_DWFR$Deed.codes!="48"
                                     & master_resid_DWFR$Deed.codes!="49",]

###########################
# DATE OF ENTRY EXPLORATION

#Now what about dates of entry?
#0       8      10 
#20144   79271 1125693 
table(master_filtered$strLength)

#And what are the zeroes now?
# noDateOfEntryDeedCodes <- data.frame(
#   as.character(master_filtered[nchar(as.character(master_filtered$Date.of.Entry))==0,c("Deed.codes")]))
noDateOfEntryDeedCodes <- 
master_filtered[nchar(as.character(master_filtered$Date.of.Entry))==0,
                c("Application.date","Deed.codes","Consideration","Price", "Value")]

#20144. All of them.
nrow(noDateOfEntryDeedCodes[is.na(noDateOfEntryDeedCodes$Price),])

#table not working well! Count then order
#Via plyr
deedCounts <- count(noDateOfEntryDeedCodes, c('Deed.codes'))
deedCounts <- deedCounts[order(-deedCounts$freq),]

ConsiderationTypes <- count(noDateOfEntryDeedCodes, c('Deed.codes'))

#How many non-date-of-entries do we have left if we remove those
#with no data at all in the consideration or price fields
nrow(noDateOfEntryDeedCodes[noDateOfEntryDeedCodes$Consideration=="",])

#Let's check - for the Date.of.Entries we *do* have - what their spread is,
#How they differ from application date.
#So that means first filtering then formatting for consistency as before
#(Because some are 2-digit year, some 4)
#1. Keep only those with a date
#Use strlength field (length of Date.of.Entry field) to do this
master_filtered_DoEs <- master_filtered[master_filtered$strLength!=0,]

#Now format consistently.
#Just 8 and 10 now
table(master_filtered_DoEs$strLength)

#Rename current dateFormatted to show its source
names(master_filtered_DoEs)[names(master_filtered_DoEs)=="dateFormatted"] <- "applicationDateFormatted"

#Copied from below: Now format date based on length of application.Date field
master_filtered_DoEs$dateOfEntryFormatted <- as.Date(master_filtered_DoEs$Date.of.Entry, format='%d/%m/%Y')

#Do for 8 digits
master_filtered_DoEs$dateOfEntryFormatted[master_filtered_DoEs$strLength==8] <- 
  as.Date(master_filtered_DoEs$Date.of.Entry[master_filtered_DoEs$strLength==8], format='%d/%m/%y')

master_filtered_DoEs <- master_filtered_DoEs[order(master_filtered_DoEs$dateOfEntryFormatted),]

hist(master_filtered_DoEs$dateOfEntryFormatted, breaks='year')

#That gets us most of the way, but there are still lots of extremely odd entries in there. 
#Will have to go through and figure out how to get them to something consistent.
#What kinds of weirdness do we have?
#0103-11-07 is 2003, 0104 is 2004 etc. Easy nuff.
#So 01 is 19, 02 is 2000.

#Before attempting to filter
#Let's pull out a chunk of date of entries that *are* formatted correctly
#And see how they compare, distance-wise, to the application date
# subset_masterf <- master_filtered_DoEs[master_filtered_DoEs$dateOfEntryFormatted > '2004-01-01'
#                                        & master_filtered_DoEs$dateOfEntryFormatted < '2014-12-31',]
#Only matching application date range
#Actually we don't lose too many: 15819
subset_masterf <- master_filtered_DoEs[
  master_filtered_DoEs$dateOfEntryFormatted > min(master_filtered_DoEs$applicationDateFormatted)
  & master_filtered_DoEs$dateOfEntryFormatted < max(master_filtered_DoEs$applicationDateFormatted),]

#OK, so comparing distances there
# subset_masterf$dateDistance <- abs(subset_masterf$applicationDateFormatted-subset_masterf$dateOfEntryFormatted)
#date of entry minus application date: 
#that's the number of days from application to (in theory) actual sale
subset_masterf$dateDistance <- subset_masterf$dateOfEntryFormatted-subset_masterf$applicationDateFormatted

#Mostly looking pretty close
#http://stackoverflow.com/questions/7393001/histogram-of-date-differences
plot(table(subset_masterf$dateDistance))

#let's just look at the subset near to the correct date
plot(table(subset_masterf$dateDistance[subset_masterf$dateDistance < 365
                                       & subset_masterf$dateDistance > -365]))

#And excluding the sensible chunk to make the unsensible ones have more y-axis
plot(table(subset_masterf$dateDistance[subset_masterf$dateDistance > 30
                                       | subset_masterf$dateDistance < -(365)]))

#Quick DW/FR check... not hugely different, at least for this dataset.
plot(table(subset_masterf$dateDistance[subset_masterf$Application.type=="DW"]))
plot(table(subset_masterf$dateDistance[subset_masterf$Application.type=="FR"]))

#Hang on. That's odd. Have I got the sums wrong?
#How many date of entries are *after* application date and vice versa?
nrow(subset_masterf[subset_masterf$dateOfEntryFormatted > subset_masterf$applicationDateFormatted,])
nrow(subset_masterf[subset_masterf$dateOfEntryFormatted < subset_masterf$applicationDateFormatted,])

#Hmm. Let's just restrict that to ones in a sensible year range just to make sure...
nrow(subset_masterf[subset_masterf$dateOfEntryFormatted > subset_masterf$applicationDateFormatted
                    & (subset_masterf$dateDistance < 365
                       & subset_masterf$dateDistance > -365),])
nrow(subset_masterf[subset_masterf$dateOfEntryFormatted < subset_masterf$applicationDateFormatted
                    & (subset_masterf$dateDistance < 365
                       & subset_masterf$dateDistance > -365),])

#Oh OK, that makes sense:
#Applications are put in, the date of entry (actual sale)
#Will be on the form before that date. Ones after then must presumably be wrong entries

#########################
# DATE OF ENTRY FILTERING

#While waiting on other things (just those two months early data) let's get some code written
#for keeping as many of the dates of entry as I can
#So let's look at all dates of entry *outside* the range of the application dates
#15230 obs
outside_range <- master_filtered_DoEs[
  master_filtered_DoEs$dateOfEntryFormatted < min(master_filtered_DoEs$applicationDateFormatted)
  | master_filtered_DoEs$dateOfEntryFormatted > max(master_filtered_DoEs$applicationDateFormatted),]

outside_range <- outside_range[order(outside_range$dateOfEntryFormatted),]

#Let's just look at actually sensible dates outside the application range
#1900 being the earliest in the 'sensible' dates
hist(outside_range$dateOfEntryFormatted[
  outside_range$dateOfEntryFormatted > '1800-01-01'
  & outside_range$dateOfEntryFormatted < '2016-01-01'], breaks='month', freq = T)

hist(outside_range$dateOfEntryFormatted[
  outside_range$dateOfEntryFormatted > '1990-01-01'
  & outside_range$dateOfEntryFormatted < '2016-01-01'], breaks='month', freq = T)

#For those outside of sensible, what do we have?
daftDates <- outside_range[outside_range$dateOfEntryFormatted < '1800-01-01'
  | outside_range$dateOfEntryFormatted > '2016-01-01',]

#Add in column that's just the weird year
daftDates$weirdYear <- substr(daftDates$dateOfEntryFormatted,1,4)

#Only 1000 of these.
# samp <- daftDates[sample(1:nrow(daftDates), 200),
#                   c('Date.of.Entry','dateOfEntryFormatted','applicationDateFormatted','weirdYear')]
# 
# samp <- samp[order(samp$weirdYear),]
# samp

#Looking at the full 1000...
daftDates <- daftDates[order(daftDates$weirdYear),]
daftDates[1:500,c('Date.of.Entry','dateOfEntryFormatted','applicationDateFormatted','weirdYear')]
daftDates[501:nrow(daftDates),c('Date.of.Entry','dateOfEntryFormatted','applicationDateFormatted','weirdYear')]

#So there are some obvious ones to fix. Others are less obvious.
#0107, 0112 etc. Can just drop the 01 and then treat as 2-digit year or replace with 20
#0200: no way of knowing what year it is.
#It will *probably* be close to application month but the year could be wrong
#How many of those are there?
#257
nrow(daftDates[daftDates$weirdYear=='0200',])

#reformat based on first two year digits
daftDates$dateOfEntryProcessed_n_Formatted <- ""

#Remove all 0200

#This leaves 3 others with 2023+ years. But I'll filter those out at the end.
daftDates$dateOfEntryProcessed_n_Formatted[substr(daftDates$weirdYear,1,2)=='01'] <- 
  paste("20",substr(daftDates$dateOfEntryFormatted[substr(daftDates$weirdYear,1,2)=='01'],3,10), sep="")






###########
# OLD BUMPH

#Dates in master are mixed between 4-digit year and 2-digit. Bonza.
# master$strLength <- nchar(as.character(master$Application.date))
#mostly 10, 10$ are 8, and 16 length. Only 64 of those with no data in. Drop em!
# table(master$strLength)
# master[master$strLength == 8,]
# master <- master[master$strLength != 16,]
#Master saved without those missing 64. Carry on without!

#Now format date based on length of application.Date field
# master$dateFormatted <- as.Date(master$Application.date, format='%d/%m/%Y')
# master$dateFormatted[master$strLength==8] <- as.Date(master$Application.date[master$strLength==8], format='%d/%m/%y')
# #resave and drop strlength
# master <- master[,c(1:20)]
# saveRDS(master, "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/master.rds")





colnames(master)
colnames(address)
colnames(applicant)
colnames(granter)

#Store unique ID counts
ids <- data.frame(type = c("master","address","applicant","granter"), 
                  application_number = 0,
                  title_number = 0)

ids[1,2] = length(unique(master$Application.number))
ids[2,2] = length(unique(address$Application.number))
ids[3,2] = length(unique(applicant$Application.number))
ids[4,2] = length(unique(granter$Application.number))

ids[1,3] = length(unique(master$Title.number))
ids[2,3] = length(unique(address$Title.number))
ids[3,3] = length(unique(applicant$Title.number))
ids[4,3] = length(unique(granter$Title.number))

write.csv(ids, "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/uniqueIDsApplication_n_TitleNum.csv")


# matches <- merge(app_uniques, granter_uniques, 
#                  by.x=colnames(app_uniques)[1],
#                  by.y=colnames(granter_uniques)[1]
#                  )

#Ah - rather easier way of doing that!
#unique applicants and granter pairs: 679,998
#1,614,440 applicants vs 680,010 granters, so nearly all granters.
length(app_uniques[app_uniques[,1] %in% granter_uniques[,1],])
length(granter_uniques[granter_uniques[,1] %in% app_uniques[,1],])

#Let's just check the title ID matching.
app_uniques_title <- data.frame(unique(applicant$Title.number))
granter_uniques_title <- data.frame(unique(granter$Title.number))

#How many match?
#602812. applicants vs granters is 2466024 vs 983374
#So we haven't even got all granters' buyers.
#Update: no, that's not what I need to be looking at.
#Stick to application comparison above.
matches2 <- merge(app_uniques_title, granter_uniques_title, 
                 by.x=colnames(app_uniques_title)[1],
                 by.y=colnames(granter_uniques_title)[1]
                 )

#find applicants that have no matching granter
#http://r.789695.n4.nabble.com/R-Merge-how-can-I-keep-discarded-values-td812165.html
orphanapps <- subset(applicant, is.element(applicant$Application.number, granter$Application.number) == F)
#Check that's the right number of unique applicant numbers
#nrow(data.frame(unique(orphanapps$Application.number)))

#Or just try and pick one out of granters, see if it's there... Newp!
granter[granter$Application.number %in% orphanapps$Application.number[1],]


#############################
# What's the match level like in Judita's RoS / Zoopla match file?
alignment <- read.csv("JuditasLinkingFiles/smi_alignment_v2.txt",sep=" ", header=F)

#Any duplicate RoS?
#Yup, plenty. 99820 unique out of 163015 matches
length(unique(alignment$V6))

#How many match in the master new-RoS file?
# master_uniques <- data.frame(unique(master$Application.number))
alignment_uniques <- data.frame(unique(alignment$V6))

#99272
length(alignment_uniques[alignment_uniques[,1] %in% master_uniques[,1],])

#How many match from the old RoS file?
RoSold_uniques <- data.frame(unique(RoS_old$regist))

#None. It's not involved at all. Judita's list just matches against new RoS
length(alignment_uniques[alignment_uniques[,1] %in% RoSold_uniques[,1],])

#Do old RoS IDs match new RoS title IDs?
master_uniques_title <- data.frame(unique(master$Title.number))

#499155 out of ~1.6 million new-RoS master title IDs
length(master_uniques_title[master_uniques_title[,1] %in% RoSold_uniques[,1],])

#I think Judita matched against the address file, is that right...?
#If so, unique RoS IDs should all be accounted for
master_uniques <- data.frame(unique(master$Application.number))
address_uniques <- data.frame(unique(address$Application.number))

#99272 again. Hur.
length(alignment_uniques[alignment_uniques[,1] %in% address_uniques[,1],])

testy <- merge(alignment_uniques,address_uniques,
               by.x=colnames(alignment_uniques)[1],
               by.y=colnames(address_uniques)[1])

#Yup, still 99272! Which suggests I don't have all the data, doesn't it?
#Judita may also have matched against the 2015 data.

#What's the date spread of the new RoS data?
master$codedDates <- as.Date(master$Application.date,"%d/%m/%Y")

#length(master[is.na(master$codedDates),])

#Remove nas - buggering with the minmax!
dates <- master$codedDates[!is.na(master$codedDates)]

min(dates)
max(dates)

#Do the alignment file RoS application numbers all have granters?
#meaning they're all sales? They should be.
#Newp: 44845
length(alignment_uniques[alignment_uniques[,1] %in% granter_uniques[,1],])

#Old/new check: for the new RoS entries in alignment file
#How many matching old-RoS entries are there? 
#I know there are ~500K in total but there'll be a lot less
#Get unique RoS entries from alignment file
align_ros_uniqueIDs <- data.frame(unique(alignment$V6))

#use address uniques but get unique title IDs
address_uniques_titles <- data.frame(unique(address$Title.number))

#We already know how many titles match across new and old RoS...
length(address_uniques_titles[address_uniques_titles[,1] %in% RoSold_uniques[,1],])

#Keep only address entries with a matching application ID from the alignment file
address_match_align <- address[address$Application.number %in% align_ros_uniqueIDs[,1],]

#Keep just the remaining unique title numbers
address_match_align_unique_titles <- data.frame(unique(address_match_align$Title.number))

#How many of those match old RoS title number?
#52389
length(address_match_align_unique_titles[address_match_align_unique_titles[,1] %in% RoSold_uniques[,1],])

#clear memory
rm(list = ls(all = TRUE))

###############
# MISC CHECKINZ

#How much of the new-RoS data is geocoded?
#96.84% in the master
100-((sum(is.na(master$Minimum.Easting))/nrow(master))*100)

#64.46%
100-((sum(is.na(address$Eastings))/nrow(address))*100)

#How many master/applicant matches? And how many of those geocoded?
masters_w_applicationMatch <- master[master[,3] %in% app_uniques[,1],]
#96.84%. So the same then. Well.
100-((sum(is.na(masters_w_applicationMatch$Minimum.Easting))/nrow(masters_w_applicationMatch))*100)

#I think it's only the earlier master files/dates that don't have geocoding.
masters_without_geocoding <- master[is.na(master$Minimum.Easting),]

#are there any missing...? Nope.
sum(is.na(masters_without_geocoding$Application.date))

#What's the date range?
masters_without_geocoding$Application.date <- 
  as.Date(masters_without_geocoding$Application.date, format='%d/%m/%Y')

range(masters_without_geocoding$Application.date, na.rm=T)

#We gained some NAs! Dates must have been coded wrong.
#I've seen some that didn't have the full 4-digit year...
sum(is.na(masters_without_geocoding$Application.date))

#Vast majority are in 2003.
hist(table(masters_without_geocoding$Application.date))

########################################################
# Check how the application codes match against applicants vs granters
# Starting to think that those are not actually always both parties in a sale
#So what I want:
#A new copy of the master file with a couple of additional fields
#Or possibly one field with 'pair', 'applicant only' and 'granter only'
#Soooo... how to do that??

#Assuming I've got my list of unique application IDs from above...
master_flag <- master

#http://stackoverflow.com/questions/17415045/set-the-flag-if-a-column-values-are-matching-with-those-in-another-vector
master_flag$hasGranter <- 0 + (master_flag$Application.number %in% granter_uniques[,1])

master_flag$hasApplicant <- 0 + (master_flag$Application.number %in% app_uniques[,1])

#Then one field covering both for tabling
master_flag$appgrantermatches <- "neither"
master_flag$appgrantermatches[master_flag$hasGranter == 1 & master_flag$hasApplicant == 1] <- "both"
master_flag$appgrantermatches[master_flag$hasGranter == 1 & master_flag$hasApplicant == 0] <- "granter only"
master_flag$appgrantermatches[master_flag$hasGranter == 0 & master_flag$hasApplicant == 1] <- "applicant only"

table(master_flag$appgrantermatches, master_flag$Application.type)
write.csv(table(master_flag$appgrantermatches, master_flag$Application.type), "application_type_vs_data_type.csv")

#what's the date range of some of those?
#(After some faffy date reformatting done at the top...)
range(master_flag$dateFormatted[master_flag$appgrantermatches=="applicant only"])

#################
# GEOCODING STUFF

#Checking geocoding of unique address data
#Trying this to get unique rows...
#http://stackoverflow.com/questions/9944816/unique-on-a-dataframe-with-only-selected-columns
#address_uniquewhole <- address[row.names(unique(address[,"Application.number"])),]

#What's geocoding like in each?
address_uniquewhole <- address[unique(address$Application.number),]

#Plenty missing still. Huh.
table(is.na(address_uniquewhole$Northings))


#################################
# Check matching records in old and new RoS
# FRs (first registrations) in the new data. 
# If these are FRs in all the data, the title numbers should not be present in the old. 
# These should be properties / titles appearing in the RoS for the first time.
# DWs: opposite. The title numbers for these should appear previously. Do they appear both in the new and old?
# Old and new: are there matching applications? That is, do the same title registrations appear in both?
# Old doesn't have application numbers so would have to match via rough date.
# I should be able to find some way of checking this by inspection before attempting to match them all.

#FRs. Easy enough: do FR title numbers appear at all in RoS_old?
fr_titlenos <- master_flag[master_flag$Application.type=='FR',2]

#Yes they do: 169072.
#Which is not going to be a problem as long as those are also
#the first time they appear in RoS_old. They should be matching records.
nrow(RoS_old[RoS_old$regist %in% fr_titlenos,])

#I think RoS goes back to 1979 - so there may be actual FRs before this 'old' data
#But *if* FR only applied to new, there would be *some* earlier title numbers in the old
#To check, I'll need to date-ify the date field in old

#Oh, I've done this before... yearmon is a problem!
#http://stackoverflow.com/questions/6242955/converting-year-and-month-yyyy-mm-format-to-a-date-in-r
# RoS_old$selldate_formatted <- read.zoo(text = as.character(RoS_old$selldate, FUN = as.yearmon)
#Let's just add a day so it's comparable to new.

#Oh good, more stupid! December appears as '00'. Obviously.
#So. Convert those to december. Um.
#It'll be easier to paste the month/date fields together than regex the selldate field
# RoS_old$selldate_new <- paste(as.character(RoS_old$month), as.character(RoS_old$year),sep="")
#Nope, that won't work either! Year is single digit if "09"

#So back to regex solution. Change 00 to 12.
#There's probably a way to do this in one line but I'm going to split the string
#To get the year separately
#(substring direct assignment via <- doesn't work here)
#If the last two digits in selldate are 00
#replace with 12. Or keep original
RoS_old$selldate_fixDecember <- ifelse(substring(RoS_old$selldate,5)=="00",
                                       paste(substring(RoS_old$selldate,1,4),"12",sep=""),
                                       RoS_old$selldate)

#OK! Now it should format to date consistently. Add day and format...
RoS_old$selldate_add_day <- paste(as.character(RoS_old$selldate_fixDecember),"01",sep="")
RoS_old$selldate_formatted <- as.Date(RoS_old$selldate_add_day, format='%Y%m%d')

#check it's formatted correctly... yup
range(RoS_old$selldate_formatted)

#Drop temp columns
colnames(RoS_old)
RoS_old <- RoS_old[,c(1:18,21)]

#Save a copy.
saveRDS(RoS_old,"JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted.rds")

#Hmm: crash very visible in the *number* of sales, if not the prices
hist(RoS_old$selldate_formatted, breaks='years', freq=T)
hist(master$dateFormatted, breaks='years', freq=T)

#############################
# LINK OLD / NEW RoS ON TITLE
# Section in WindFarmViewShedPlanning2016.docx: "Repeat sales across new/old data"

#Using only month/year in each.
#That should allow me to merge on title and date
#I should then keep all other records from both
#Maybe start by adding some dummy columns in so I know which is old and new after the merge
master$RoSversion <- "new"
RoS_old$RoSversion <- "old"

#What columns to keep from merge?
#Well I can come back and rerun, so let's stick to the minimum for now
#To save memory
#Old has a nice straightforward price field (probably Jessie's doing!)
#New has three separate muddled fields. 
#Keep those for now so I can look at the comparison
colnames(master)
colnames(RoS_old)

#Create yearmon columns for both
#http://www.inside-r.org/packages/cran/zoo/docs/as.yearmon
RoS_old$yearmon <- as.yearmon(RoS_old$selldate_formatted)
master$yearmon <- as.yearmon(master$dateFormatted)

#Title number+yearmon should be a unique ID
#Hmm. Not even close
# length(unique(c(master$Title.number,master$yearmon)))
#This looks like the right way to do that!
#Much closer to accurate
nrow(unique(master[,c("Title.number","yearmon")]))

#Let's look at those duplicate title+yearmons
dups <- subset(master, 
               duplicated(master[,c("Title.number","yearmon")])
               |duplicated(master[,c("Title.number","yearmon")],fromLast = T))

dups <- dups[order(dups$Title.number),]

#sanity check. This is really hurting my head!
#Uniques from master will include each of the unique values from the duplicates...
#And the sums work out. We do have the dups correct.
nrow(unique(dups[,c("Title.number","yearmon")]))

#REPEAT ALL THAT BUT LOOK AT ONLY "LAND CLASSIFICATION = RESIDENTIAL".
#The others are very likey not to be relevant
#While also perhaps being most of the random transfers of bits of land
#Lots of NAs, let's drop those too
#http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame
#Actually, complete.cases didn't work...
master_resid <- master[master$Land.Classification=="R" 
                       & !is.na(master$Land.Classification),]

#How many unique titles/dates now?
#1426175. Good number.
nrow(unique(master_resid[,c("Title.number","yearmon")]))

#So what's left now?
dups2 <- subset(master_resid, 
               duplicated(master_resid[,c("Title.number","yearmon")])
               |duplicated(master_resid[,c("Title.number","yearmon")],fromLast = T))

#So how many unique titles for each of those are we looking at?
#Whole master duplicates: 32911
length(unique(dups$Title.number))
#Residental dups only: 22481 properties.
length(unique(dups2$Title.number))

#Let's take a look
dups2 <- dups2[order(dups2$Title.number),]

#Still a mix of types
table(dups2$Application.type)

#Let's just check that the application numbers are all unique
nrow(dups) - length(unique(dups$Application.number))
nrow(dups2) - length(unique(dups2$Application.number))
#There are a tiny number with the same application number. 
#55 and 26 respectively. They shouldn't exist.

#Well, let's look! Probably actual duplicates, right?
dupdup <- subset(dups, 
                 duplicated(dups[,c("Application.number")])
                 |duplicated(dups[,c("Application.number")],fromLast = T))

dupdup <- dupdup[order(dupdup$Application.number),]
#Yeah, they're just double entries. Don't need 'em.
#(Will need to filter out from the whole lot before using)

#What about if, as well as using residential only
#we stick to FR and DW? Other cats are bits of land moving back and forth
#So not much use for consistent repeat sales anyway.
master_resid_DWFR <- master_resid[(master_resid$Application.type=="FR" |
                                     master_resid$Application.type=="DW")
                                            & !is.na(master_resid$Land.Classification),]

#Duplicates in this?
nrow(unique(master_resid_DWFR[,c("Title.number","yearmon")]))

#Not many but let's have a look...
dups3 <- subset(master_resid_DWFR, 
                duplicated(master_resid_DWFR[,c("Title.number","yearmon")])
                |duplicated(master_resid_DWFR[,c("Title.number","yearmon")],fromLast = T))

#7114 titles, ~15K in total.
length(unique(dups3$Title.number))

dups3 <- dups3[order(dups3$Title.number),]

#So a lot of them are things like sharing the same title between various different people.
#The others it's difficult to know - so I suspect the best thing to do is exclude them.
#So that's not just keeping uniques - I don't want *any* with duplicates.
#(Unique would keep one of them.)

#OK so we've got filtered-down master_resid_DWFR
#And dups3 is all the duplicates from that
#I now want all single-instance title/date combos
#i.e. everything that's *not* in dups3
#For which I can just tweak the subsettin, right...?
master_resid_DWFR_singleInstances <- subset(master_resid_DWFR, 
                !duplicated(master_resid_DWFR[,c("Title.number","yearmon")])
                & !duplicated(master_resid_DWFR[,c("Title.number","yearmon")],fromLast = T))

#Is that the right size? Yup!
nrow(master_resid_DWFR) - nrow(master_resid_DWFR_singleInstances)

#And are they all single instances? Yup!
table(duplicated(master_resid_DWFR_singleInstances[,c("Title.number","yearmon")]))

#Save that, then I can drop the above data for now.
saveRDS(master_resid_DWFR_singleInstances,
        "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/masterFiltered_residential_DWFR_singleInstances.rds")

###############################
# CHECKING RoS_old BEFORE MERGE

#Same issues - do we have unique title/data pairs? Or is there some filtering work to do before merging?
#so - duplicate status of regist+yearmon?
#2068338 unique pairs vs 2774599 rows.
nrow(unique(RoS_old[,c("regist","yearmon")]))

#OK, let's look at dups again!
oldDups <- subset(RoS_old, 
                  duplicated(RoS_old[,c("regist","yearmon")])
                  |duplicated(RoS_old[,c("regist","yearmon")],fromLast = T))

#Saving a copy of that so I can easily come back to it. Takes a looong time to run.
saveRDS(oldDups, "JessieExtDrive/Misc_RoS_R_Saves/RoS_old_duplicate_registYearMonpairs.rds")
oldDups <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/RoS_old_duplicate_registYearMonpairs.rds")

#8573 unique title numbers... 715K rows?? Hmm.
length(unique(oldDups$regist))

oldDups <- oldDups[order(oldDups$yearmon,oldDups$regist),]

#oh! regist has a lot of...
table(is.na(oldDups$regist))#none
#694781. Blimey. I might need to look at the other RoS_old files to see if that's the case there too.
#Reduces RoS_old to ~2 million from ~2.7 million.
table(oldDups$regist[oldDups$regist==""])
#20520 duplicates with actual title numbers.
nrow(oldDups) - length(oldDups$regist[oldDups$regist==""])
#Let's look at those...
oldDupsTitles <- oldDups[oldDups$regist!="",]

#8572 uniques (same as above minus the "")
length(unique(oldDupsTitles$regist))
oldDupsTitles <- oldDupsTitles[order(oldDupsTitles$yearmon,oldDupsTitles$regist),]

#Columns are annoying me. Let's look at a reduced version
colnames(oldDupsTitles)
oldDupsTitles <- oldDupsTitles[,c(4:9,13,16:17,21)]

#Well, a lot of those have the same "regist" for entirely different addresses
# *on the same date*.
#So while it's not a huge number and dropping would be fine...
#I think I just want to check that regist is what I think it is.
#OK, done some of that. Let's just see what a merge looks like -
#We should have same addresses.

#Reload filtered master
# master_resid_DWFR_singleInstances <- 
#   readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/masterFiltered_residential_DWFR_singleInstances.rds")
#Less ridiculous name plz.
mrds <- 
  readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/masterFiltered_residential_DWFR_singleInstances.rds")

#We have no address in there. An address might be useful! Loaded from top.
colnames(mrds)
colnames(address)

#We'll probably be missing some in the match but just for checking porpoises
#close! 1231874. 102 short of whole single-instance master.
nrow(mrds[mrds[,2] %in% address[,2],])

#keep only sequence one. Just want first record of each.
# address1 <- address[address$Sequence == ]

#Oh good! Something very odd with address sequence field. Some are HUGE. Let's look... *sigh*
address <- address[order(address$Title.number,address$Sequence),]

#OK, the sequence field just doesn't make sense for a lot of those. Who knows what happened?
#I should still be able to get the address from unique application/title fields.
# address_uniques_twofields <- data.frame(unique(address$Title.number,address$Application.number))
address_uniques_twofields <- address[row.names(unique(address[,c('Title.number','Application.number')])),]

#Let's just check how many of those match against master
#1231874 - same again. Good good.
nrow(mrds[mrds[,2] %in% address_uniques_twofields[,2],])

#IMPORTANT BIT! FILTERED MASTER AND UNIQUE APP/TITLE PAIR ADDRESSES MERGED!
#Merge in unique application/title pair addresses (272 will be dropped from master)
mrds_plus_address <- merge(mrds[,c(2:21)], address_uniques_twofields[,c(1:12,14:15,17:18)], by=c('Title.number','Application.number'))

#And let's save that because I just managed to crash it before completion. Impressive!
#IMPORTANT BIT! FILTERED MASTER AND UNIQUE APP/TITLE PAIR ADDRESSES SAVED!
saveRDS(mrds_plus_address, "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mrds_plus_address.rds")
mrds_plus_address <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mrds_plus_address.rds")
#Save as CSV for shared use.
#Write locally first cos shared is SLOOOW
# write.csv(mrds_plus_address,
#           "M:/SMI_Share/IT Support/Research Data/LDOES_Jessie/Processed_RegistersOfScotlandDataNew_RoS_master_plus_address_filtered_resid_DW_FR.csv")
# write.csv(mrds_plus_address,
#           "C:/Data/temp/Processed_RegistersOfScotlandDataNew_RoS_master_plus_address_filtered_resid_DW_FR.csv")

#Checking it's what I think it is. i.e. single date/title pairs
#After adding yearmon field below...
#Yes, correct number. Err. I wonder how I did this is the original didn't have the yearmon field? Err.
nrow(unique(mrds_plus_address[,c("Title.number","yearmon")]))

#So now: RoS old.
#First, let's drop any that have no regist entry at all. No way to link them.
RoS_old <- RoS_old[RoS_old$regist!="",]

#It needs its own yearmon field. Thought I'd added that...
RoS_old$yearmon <- as.yearmon(RoS_old$selldate_formatted)
#As does the master!
mrds_plus_address$yearmon <- as.yearmon(mrds_plus_address$dateFormatted)

#merge on title number. For the moment I just want to look at what RoS_old fields match
#So just keeping the matches
RoS_combined <- merge(RoS_old, mrds_plus_address, by.x=c("regist","yearmon"), by.y=c("Title.number","yearmon"))

#save! cos I crashed it before somehow
saveRDS(RoS_combined, "JessieExtDrive/Misc_RoS_R_Saves/RoS_combined_test.rds")
RoS_combined <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/RoS_combined_test.rds")

#sort so that duplicate titles are together
#Presume there are some still...? Yup.
length(unique(RoS_combined$regist))

RoS_combined <- RoS_combined[order(RoS_combined$yearmon, RoS_combined$regist),]

#OK, we have matching addresses mostly. Checking that might be a little tricky!
#But, again, let's look at the dups and see what they look like
RoS_combined_dups <- subset(RoS_combined, 
                            duplicated(RoS_combined[,c("regist","yearmon")])
                            |duplicated(RoS_combined[,c("regist","yearmon")],fromLast = T))

RoS_combined_dups <- RoS_combined_dups[order(RoS_combined_dups$yearmon, RoS_combined_dups$regist),]

#Oh, the address was already in the master. Glad we did all that arsing about with the address file.
#Anyway, conclusion:
#A lot of those duplicates are things like HAs building stuff

#Drop dups!
RoS_combined_noDups <- subset(RoS_combined, 
                            !duplicated(RoS_combined[,c("regist","yearmon")])
                            & !duplicated(RoS_combined[,c("regist","yearmon")],fromLast = T))

#Let's check some postcodes across both, in noDups
RoS_combined_noDups[sample(1:nrow(RoS_combined_noDups),50),c("pcode","Postcode")]

#Looking good, though we have some blanks
#338384. Not bad. Ish.
nrow(RoS_combined_noDups[RoS_combined_noDups$pcode == RoS_combined_noDups$Postcode,])

#Are the rest just blank? Or mis-matched?
#Oh no - mostly just slightly mismatched format. Still correct postcode.
RoS_combined_noDups[RoS_combined_noDups$pcode != RoS_combined_noDups$Postcode,c("pcode","Postcode")]

#Prices also match
RoS_combined_noDups[sample(1:nrow(RoS_combined_noDups),50),c("price","Consideration")]

#Let's reformat the postcodes to match. Remove spaces from both, should do the job.
RoS_combined_noDups$pcodeNoSpaces <- gsub(" ", "", RoS_combined_noDups$pcode)
RoS_combined_noDups$PostcodeNoSpaces <- gsub(" ", "", RoS_combined_noDups$Postcode)

#Now? Much better. 435433/454949.
#19516 not matching.
nrow(RoS_combined_noDups[RoS_combined_noDups$pcodeNoSpaces == RoS_combined_noDups$PostcodeNoSpaces,])

#What's left?
#Mostly missing vals but also slightly different postcodes, so poss data entry errors
#Checking against actual addresses -
#Vast majority match on street, most on street number
#But it'd need some matching regex work
postcodeNoMatch <- RoS_combined_noDups[RoS_combined_noDups$pcodeNoSpaces 
                                       != RoS_combined_noDups$PostcodeNoSpaces,
                                       c("strno","street","Property.number",
                                         "Thoroughfare","pcodeNoSpaces","PostcodeNoSpaces")]


###################
# SOME FINAL CHECKS
# Before combining old and new

#Having already loaded RoS_old...
#Drop obs with nothing in the regist field
RoS_old <- RoS_old[RoS_old$regist!="",]

#Add yearmon field
RoS_old$yearmon <- as.yearmon(RoS_old$selldate_formatted)

#Leaves us with ~2M obs down from ~2.7m
#How many duplicates now?
#2068130 uniques leaves 11659 dups
#I need a term for dups that don't include their single instances
nrow(unique(RoS_old[,c("regist","yearmon")]))

#Sigh. Let's count em properly. Single-instance uniques plz!
#20520 clones
RoS_old_singleInstances <- subset(RoS_old, 
                        !duplicated(RoS_old[,c("regist","yearmon")])
                        & !duplicated(RoS_old[,c("regist","yearmon")],fromLast = T))

#Save this for merging later
saveRDS(RoS_old_singleInstances, "JessieExtDrive/Misc_RoS_R_Saves/RoS_old_SingleInstances_title_yearmon_pairs.rds")

#Just to check %s
RoS_old_clones <- subset(RoS_old, 
  duplicated(RoS_old[,c("regist","yearmon")])
  | duplicated(RoS_old[,c("regist","yearmon")],fromLast = T))

#8861 unique title/date pairs in the dups
#Is 0.42% of total unique title/date pairs
nrow(unique(RoS_old_clones[,c("regist","yearmon")]))

#hist(RoS_singleInstances$selldate_formatted, breaks='year')

############################
# FINAL MERGE OF OLD AND NEW
# Working with single-instance versions of both 
# i.e. only single-instance pairs of titles/dates (yearmon)

#Just realised my addition of a flag field needs altering...
mrds_plus_address$RoSversion <- NULL
mrds_plus_address$isInRoSNeW <- 1
RoS_old_singleInstances$isInRoSOld <- 1
#Rest will be NA initially but that's OK

#Keeping all records.
RoS_combined_singleInstances <- merge(RoS_old_singleInstances, 
                                      mrds_plus_address, by.x=c("regist","yearmon"), 
                                      by.y=c("Title.number","yearmon"),
                                      all=T)

#Order so that repeat sales are in groups
RoS_combined_singleInstances <- RoS_combined_singleInstances[order(RoS_combined_singleInstances$regist,
                                                                   RoS_combined_singleInstances$yearmon),]

#Which from either/both dataset
table(RoS_combined_singleInstances$isInRoSOld,
      RoS_combined_singleInstances$isInRoSNeW,
      useNA = 'always')
write.csv(table(RoS_combined_singleInstances$isInRoSOld,
      RoS_combined_singleInstances$isInRoSNeW,
      useNA = 'always'),"C:/Data/temp/tableOfSource.csv")

#OK, let's write that. Two versions, CSV again. Though may compress afterwards.
saveRDS(RoS_combined_singleInstances, 
        "JessieExtDrive/Misc_RoS_R_Saves/RoS_combined_SingleInstances.rds")
RoS_combined_singleInstances <- 
        readRDS("JessieExtDrive/Misc_RoS_R_Saves/RoS_combined_SingleInstances.rds")


#Save in temp for compressing and putting on shared. Wonder if it'll compress as small?
#(1.5gb vs RDS = 192Mb). Hmm - zip compress is a bit smaller.
write.csv(RoS_combined_singleInstances, "C:/Data/temp/RoS_combined_SingleInstances.csv")

hist(RoS_combined_singleInstances[RoS_combined_singleInstances$isInRoSNeW==1,c('dateFormatted')], 
     breaks='year',
     freq=T)
hist(RoS_combined_singleInstances[RoS_combined_singleInstances$isInRoSOld==1,c('selldate_formatted')], 
     breaks='year',
     freq=T)














