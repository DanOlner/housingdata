library(readstata13)
library(plyr)
library(pryr)
library(zoo)
library(stringr)
library(qdap)
library(ggplot2)
library(ineq)
library(stringdist)
library(pryr)
library(dplyr)
library(tidyr)
library(scales)
library(data.table)

#INTRO FILE LOADING----
#Now having a look at the combined new-RoS RDSs compiled in combineNewRoSdata.R
master <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/master.rds")
address <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/address.rds")
applicant <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/applicant.rds")
granter <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/granter.rds")

#save CSV versions for Sue
# write.csv(master, "JessieExtDrive/Data/RoS/CSVversions_RoSNew_combinedTypes/master.csv")
# write.csv(address, "JessieExtDrive/Data/RoS/CSVversions_RoSNew_combinedTypes/address.csv")
# write.csv(applicant, "JessieExtDrive/Data/RoS/CSVversions_RoSNew_combinedTypes/applicant.csv")
# write.csv(granter, "JessieExtDrive/Data/RoS/CSVversions_RoSNew_combinedTypes/granter.csv")

#RoS_old <- read.dta13("JessieExtDrive/STATA_analysis/RoS/RoS19902010.dta")
#nuvva <- read.dta13("JessieExtDrive/STATA_analysis/RoS/dataset_RoS_version1.dta")

#use date-formatted-properly version
RoS_old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted.rds")

#How many matching applicants and granters do we have?
#(In terms of application ID)
app_uniques <- data.frame(unique(applicant$Application.number))
granter_uniques <- data.frame(unique(granter$Application.number))
master_uniques <- data.frame(unique(master$Application.number))
address_uniques <- data.frame(unique(address$Application.number))


#Saving backup of master then removing empty columns X.1-X.13
#Checked, they're NAs. But backing up just in case...
#saveRDS(master, "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/master_backup.rds")

#Remove empty columns, save as master
# master <- master[,c(1:19)]
# saveRDS(master, "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/master.rds")

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

#CHECKING UNIQUENESS----
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




#JUDITA'S LINK FILE WORKING? ----
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




#MISCCHECKINZ
#How much of the new-RoS data is geocoded? -----
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




#How do applicants/granters match up? ------

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





# GEOCODING STUFF

#Checking geocoding of unique address data ------
#Trying this to get unique rows...
#http://stackoverflow.com/questions/9944816/unique-on-a-dataframe-with-only-selected-columns
#address_uniquewhole <- address[row.names(unique(address[,"Application.number"])),]

#What's geocoding like in each?
address_uniquewhole <- address[unique(address$Application.number),]

#Plenty missing still. Huh.
table(is.na(address_uniquewhole$Northings))


  
# Check matching records in old and new RoS ------

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


# LINK OLD / NEW RoS ON TITLE ----------
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


# CHECKING RoS_old BEFORE MERGE -----

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



#IMPORTANT BIT! FILTERED MASTER AND UNIQUE APP/TITLE PAIR ADDRESSES MERGED! -----
#Merge in unique application/title pair addresses (272 will be dropped from master)
mrds_plus_address <- merge(mrds[,c(2:21)], address_uniques_twofields[,c(1:12,14:15,17:18)], by=c('Title.number','Application.number'))

#And let's save that because I just managed to crash it before completion. Impressive!


#IMPORTANT BIT! FILTERED MASTER AND UNIQUE APP/TITLE PAIR ADDRESSES SAVED! -----
saveRDS(mrds_plus_address, "JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mrds_plus_address.rds")
mrds_plus_address <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mrds_plus_address.rds")
#Save as CSV for shared use.
#Write locally first cos shared is SLOOOW
# write.csv(mrds_plus_address,
#           "M:/SMI_Share/IT Support/Research Data/LDOES_Jessie/Processed_RegistersOfScotlandDataNew_RoS_master_plus_address_filtered_resid_DW_FR.csv")
# write.csv(mrds_plus_address,
#           "C:/Data/temp/Processed_RegistersOfScotlandDataNew_RoS_master_plus_address_filtered_resid_DW_FR.csv")



#Checking it's what I think it is. i.e. single date/title pairs -----

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



# Dropping RoS-old empty regist fields -----
#Drop obs with nothing in the regist field
#2774599 to 2079789
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


# FINAL MERGE OF OLD AND NEW -------
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


# Testing first-two-month residential flag -----

#The raw data contains two months at the start with different fields. 
#It *doesn't* have a land.classification field, so can't tell which are residential.
#RoS data is still that way, it's not fixable from their end. 
#But they've supplied a sheet containing only residential properties
#Can use this to mark applications as residential before the following filter
earlyResidFlags <- read.csv("JessieExtDrive/Data/RoS/RoS_newMisc/CopySheffieldApr-May2003-1.csv")

#mark master land.classification for those application numbers
mastertest <- master

#how many? 20735. Seems like a lot.
nrow(mastertest[mastertest$Application.number %in% earlyResidFlags$Application_ID,])

#Let's just look at those, date-wise n ting
matches <- mastertest[mastertest$Application.number %in% earlyResidFlags$Application_ID,]

#Up to July. We would have data for that, in theory, so no need to overwrite.
#So dates for same applications don't quite match.
range(matches$applicationDateFormatted)

#What the date range for master apps with no land classification field?
#Yup, those two months! As we thought.
range(mastertest$applicationDateFormatted[is.na(mastertest$Land.Classification)])

#Only question - is there any damage done by applying "R" to that field
#For dates beyond then?
#No - just checked by just looking at the end of matches.
#It's only 5 obs and they're all residential.

#Let's just check the NA numbers drop as we think they should.
#20735 matches, 5 of which already have "R"
#Should mean 20730 less NAs
#Copy field to keep original just in case
mastertest$checkLandClass <- mastertest$Land.Classification

#28319 now...
nrow(mastertest[is.na(mastertest$checkLandClass),])

mastertest[mastertest$Application.number %in% earlyResidFlags$Application_ID,
           c('checkLandClass')] <- "R"

#7589. 70730 were de-NA'd. Perfect!
nrow(mastertest[is.na(mastertest$checkLandClass),])


# MORE DATE OF ENTRY TESTING: GET IMPUTED DATE FOR MISSING FROM APPLICATION DATE ------
# This is based on having run through filter_newRoS_masterfile.R
# Including date of entry processing
# But removing date of entries that are NA or empty
# I now want to know the spread between application date and date of entry
# To pick a number to use application as proxy for date of entry
# I'll then go back and replace NAs and empties and add a flag to say they're imputed

#TEMP2 is currently holding the processed dates of entry...
#And, yes, they're all the right length.
table(nchar(as.character(master_filtered$TEMP2)))

#Now, distance from application date
#that's the number of days from application to (in theory) actual sale
master_filtered$dateDistance <- master_filtered$TEMP2 - master_filtered$applicationDateFormatted

#Mostly looking pretty close
#http://stackoverflow.com/questions/7393001/histogram-of-date-differences
plot(table(master_filtered$dateDistance))

#let's just look at the subset near to the correct date
plot(table(master_filtered$dateDistance[master_filtered$dateDistance < 365
                                       & master_filtered$dateDistance > -365]))

#And excluding the sensible chunk to make the unsensible ones have more y-axis
plot(table(master_filtered$dateDistance[master_filtered$dateDistance > 30
                                       | master_filtered$dateDistance < -(365)]))

#Assuming dates of entry *after* application dates don't make any sense...
#Look, weekly pattern!
plot(table(master_filtered$dateDistance[master_filtered$dateDistance < 0
                                        & master_filtered$dateDistance > -(365/4)]))

mean(master_filtered$dateDistance[master_filtered$dateDistance < 0
                             & master_filtered$dateDistance > -(365/1)])

#Mean: 20 days with 90 day window
#22 with 120; 27 for a year but that's a long tail
#OK - three weeks prior to application day seems reasonable.
#For most things we need to do with it, that should work.



# DATE OF ENTRY DONE, NOW AT master_does in filter_newRoS_masterfile.R-------
# Some checks on where to cut off the date, what the numbers are

# range(master_does$DateOfEntryFormatted)
# hist(master_does$DateOfEntryFormatted, breaks='year')
#Cutting out obviously silly after-dates
hist(master_does$DateOfEntryFormatted[master_does$DateOfEntryFormatted < '2015-12-31'], breaks='year')

#And a sensible earlier range
hist(master_does$DateOfEntryFormatted[
  master_does$DateOfEntryFormatted < '2015-12-31'
  & master_does$DateOfEntryFormatted > '1990-01-01'
  ], breaks='year')

hist(master_does$DateOfEntryFormatted[
  master_does$DateOfEntryFormatted < '2003-01-01'
  & master_does$DateOfEntryFormatted > '1990-01-01'
  ], breaks='year', freq=T)

#So how many obs is that? 465
length(master_does$DateOfEntryFormatted[
  !(master_does$DateOfEntryFormatted < '2015-12-31'
  & master_does$DateOfEntryFormatted > '1990-01-01')
  ])

#180
length(master_does$DateOfEntryFormatted[master_does$DateOfEntryFormatted > '2015-12-31'])
#285
length(master_does$DateOfEntryFormatted[master_does$DateOfEntryFormatted < '1990-01-01'])
#3670
length(master_does$DateOfEntryFormatted[master_does$DateOfEntryFormatted < '2003-01-01'])

#quick ros-old geocoding check. Oh, no field at all! 


# AFTER CREATING master_filter_datesOfEntryProcessed.rds SOME MORE CHECKS -------
# In filter_newRoS_masterfile.R
# This is the point where dates of entry are sorted (or should be!)
# But haven't yet linked to addresses or checked on duplicates

#CHECK ON PRICES
#Probably not keeping any consideration fields with text in.
#RoS say they're not likely actual values, whatever the other fields contain
master_does <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/master_filter_datesOfEntryProcessed.rds")

master_does[sample(1:nrow(master_does),100),c('Consideration')]

#May come back to this to filter before checking on dups but let's press on

#CHECK ON DUPLICATES
#Still got duplicate titles, presumably?
#In fact, should have the same number as before - there hasn't been
#any extra filtering

#Would also like to check on difference between date and yearmon matching
#For now, sticking to application date, but need to check that too.
master_does$Application_yearmon <- as.yearmon(master_does$applicationDateFormatted)

#Checking on uniques generally
#863294 title numbers.
length(unique(master_does[,c("Title.number")]))
#1235603 title/app-yearmon combos - 7349 dups
nrow(unique(master_does[,c("Title.number","Application_yearmon")]))

#Look at the dups
dups <- subset(master_does, 
                duplicated(master_does[,c("Title.number","Application_yearmon")])
                |duplicated(master_does[,c("Title.number","Application_yearmon")],fromLast = T))

dups <- dups[order(dups$applicationDateFormatted, dups$Title.number),]

#6801 titles, ~14K in total.
length(unique(dups$Title.number))

#Check on dups where the yearmon match has picked up *different* application date matches
#So SD of application dates is 0 if they're the same.
dupsChk <- aggregate(dups$applicationDateFormatted, by = list(dups$Title.number, dups$Application_yearmon), sd)
#Some are very far apart.
#Oh, no they're not - I initially just aggregated on title number. Repeats!
#Max is now 20 days, which is within the month
summary(dupsChk$x)

#Cos I can't get ddply to work, some kinda memory problem - too many groups?
dupsM <- merge(dups,dupsChk,by.x="Title.number",by.y="Group.1", all.x=T)

#Now sort the SD field to see what those weird huge date diffs are
dupsM <- dupsM[order(-dupsM$x),]

#So we're good! Phew.
#Which was obvious from the start, wasn't it? Given we'd use yearmon to group?

#
#REPEAT FOR DATE OF ENTRY
#Just to look...
master_does$DOE_yearmon <- as.yearmon(master_does$DateOfEntryFormatted)

#Checking on uniques generally
#863294 title numbers.
length(unique(master_does[,c("Title.number")]))
#1234430 title/DOE-yearmon combos - 8522 dups
nrow(unique(master_does[,c("Title.number","DOE_yearmon")]))

#Look at the title/doe dups
doedups <- subset(master_does, 
               duplicated(master_does[,c("Title.number","DOE_yearmon")])
               |duplicated(master_does[,c("Title.number","DOE_yearmon")],fromLast = T))

doedups <- doedups[order(doedups$DateOfEntryFormatted, doedups$Title.number),]

#7961 titles, ~17K in total.
length(unique(doedups$Title.number))

#OK, so nothing too shocking there. Same as before - probably best just to drop
#Duplicate title/yearmon pairs because it's impossible to tell what they are
#And they're mostly shared ownership type stuff.

# RERUN MERGE WITH ADDRESSES After decreasing to single instances -------

master_does_singleInstances <- 
  readRDS("JessieExtDrive/Misc_RoS_R_Saves/master_filter_datesOfEntryProcessed_singleInstances.rds")

#Actual title/date single instances? Yup!
nrow(unique(master_does_singleInstances[,c('Title.number','applicationDateFormatted')]))
nrow(unique(master_does_singleInstances[,c('Title.number','Application_yearmon')]))

#112 don't match
nrow(master_does_singleInstances[master_does_singleInstances[,2] %in% address[,2],])



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#RoS old: what do the 700K with no regist look like? -----

#~700K obs in here have no title num in the regist field.
#But we can probably identify most of them as unique properties
#Some also don't have postcode fields either

#Load RoS_old...
RoS_old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted.rds")

noreg <- RoS_old[RoS_old$regist=="",]

#Some are flats. 
#Are there any matching flat entries? i.e. repeat sales on flats? 
#I should be able to check that in ones with regist + "flatpos has value".
#That is: the regist number should be unique to the flat, not the building.
oldwflats <- RoS_old[RoS_old$flatpos!="" & RoS_old$regist!="",]

#Look at multiple-regists...
oldwflatsdup_regists <- subset(oldwflats, 
                               duplicated(oldwflats[,"regist"])
                               | duplicated(oldwflats[,"regist"],fromLast = T))

#http://stackoverflow.com/questions/7854433/finding-all-duplicate-rows-including-elements-with-smaller-subscripts
#See last answer. Get unique duplicates. Intuitive!

#So yes: repeat titles are the same flat.
#So what we should expect:
#Street number, flat and postcode go together for unique properties.
oldwflatsdup_regists <- oldwflatsdup_regists[order(oldwflatsdup_regists$regist),]


#What do no-regist entries with no postcode look like? Probably nothing special, huh?
#30514: a relatively small number
noregnopc <- noreg[noreg$pcode=="",]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Giving PROPERTY IDs to all RoS_old----
#Including those with no title number

#Having loaded RoS_old...
#Break into those with and without titles
wTitle <- RoS_old[RoS_old$regist!="",]
noTitle <- RoS_old[RoS_old$regist=="",]

#Which is all of them, right? TRUE
assertthat::are_equal((nrow(wTitle)+nrow(noTitle)),nrow(RoS_old))

#Give property titles that we DO have their own ID
#http://stackoverflow.com/questions/24119599/how-to-assign-a-unique-id-number-to-each-group-of-identical-values-in-a-column
#Well that didn't work! 
#wTitle$propertyID <- cumsum(!duplicated(wTitle$regist)) 

#This does though
wTitle <- transform(wTitle,propertyID=as.numeric(factor(wTitle$regist)))

#Did that work? Look at dups.
#http://stackoverflow.com/questions/7854433/finding-all-duplicate-rows-including-elements-with-smaller-subscripts
dups <- wTitle[wTitle$regist %in% unique(wTitle$regist[ duplicated(wTitle$regist)]),] 
dups <- dups[order(dups$regist),]

#Yes, all looking good. Slightly more faff than stata tagging, huh?
#Other answers say to use !duplicated - but that doesn't actually work.

#Just to check: all those duplicate property IDs here are repeat sales, yes?
chk <- dups[,c(2,4,5,6,7,8,9,13,19,20)]

#No. No, of course not. That would be too easy wouldn't it?
#Not many but...
#nrow(chk[unique(chk$propertyID, chk$selldate_formatted),])
#1421718
#So 11569 have same ID/date.
nrow(count(chk, vars=c('propertyID','selldate_formatted')))

#So let's look at the duplicate title/dates
duptitledatepairs <- subset(chk, 
                duplicated(chk[,c("propertyID","selldate_formatted")])
                |duplicated(chk[,c("propertyID","selldate_formatted")],fromLast = T))

#A lot of those include sales for a pound that are of no use. 
#What's left if we remove them?
duptitledatepairsMinusPoundSales <- duptitledatepairs[duptitledatepairs$price!=1,]

#How many are just entirely duplicated records?
#Over a thousand
nrow(unique(duptitledatepairsMinusPoundSales))

#So get rid of those
duptitledatepairsMinusPoundSales_uniques <- unique(duptitledatepairsMinusPoundSales)

#These: I can't see a very obvious way of sorting them. Very variable reasons.
#Some different plots with same number, some obvious errors.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OldRoS: Getting unique title/date pairs, minus 700K----

#Just for now - dropping those pesky 700K from old
#So I can link on title number.
#Loaded old from top...

#Keep only old with regist
old <- RoS_old[RoS_old$regist!="",]

#Add a full observation index so I can drop unwanted later
old$id <- seq(1:nrow(old))

#Work on unique old regist/date pairs.
#Again, I need to work out what to do with duplicate titles
#That are NOT repeat sales.
#Some things I can do straight away:

#Drop price of £1
#old <- old[old$price!=1,]
old <- old[old$price>250,]

#Check remaining duplicates, decide...
dups <- old[old$regist %in% unique(old$regist[ duplicated(old$regist)]),] 
dups <- dups[order(dups$regist),]

#Yes, all looking good. Slightly more faff than stata tagging, huh?
#Other answers say to use !duplicated - but that doesn't actually work.

#Just to check: all those duplicate property IDs here are repeat sales, yes?
chk <- dups[,c(2,4,5,6,7,8,9,13,19,20)]

#Just look at ones with matching regist/date pairs
duptitledatepairs <- subset(chk, 
                            duplicated(chk[,c("regist","selldate_formatted")])
                            |duplicated(chk[,c("regist","selldate_formatted")],fromLast = T))

#16491 of those. Keep any?
#Some are straight duplicates. Keep only uniques.
#Except now this doesn't work because I added an ID column.
duptitledatepairs <- unique(duptitledatepairs)

#So instead... all columns but ID column
duptitledatepairsunq <- subset(duptitledatepairs, 
                            !duplicated(duptitledatepairs[,names(duptitledatepairs)[1:9]]) )
                            # &!duplicated(duptitledatepairs[,names(duptitledatepairs)[1:9]],fromLast = T))


#15116 left. A mix of different things, difficult to choose which to keep


#15116 from dup method too. 
#Dropping those IDS.

#So: having added an ob ID to the original old copy
#I can now remove those IDs from it to give the final old regist/date pairs
finalOld <- old[!old$id %in% duptitledatepairsunq$id,]

#These should now all be unique title/date pairs
nrow(unique(finalOld[,c('regist','selldate_formatted')]))

#90 still not unique pairs somehow!
leftovers <- subset(finalOld, 
                    duplicated(finalOld[,c("regist","selldate_formatted")])
                    |duplicated(finalOld[,c("regist","selldate_formatted")],fromLast = T))

#various separate flats again - can't separate them out as repeat sales
#Drop. finalFINALOLD! Probably.
finalFinalOld <- finalOld[!finalOld$id %in% leftovers$id,]

#And....? YES!
nrow(unique(finalFinalOld[,c('regist','selldate_formatted')]))

#SAVE
#write.csv(finalFinalOld, "C:\Data\Housing\JessieExtDrive\Misc_RoS_R_Saves/RoS_old_uniqueTitleDatePairs_noregistdropped.csv")
saveRDS(finalFinalOld, "JessieExtDrive/Misc_RoS_R_Saves/RoS_old_uniqueTitleDatePairs_noregistdropped.rds")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combining price fields in New / generally sorting price out----

#Load new application/date uniques
new <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mdsi_plus_address.rds")

#A lot of these are actually going to be the same sale. 
#We need to filter those out first (i.e. keep only one. Not sure which yet.)

#Let's match on yearmon not specific date
#http://www.inside-r.org/packages/cran/zoo/docs/as.yearmon
#This is working with "old" that's just the original
#minus those with no regist 
#and minus any with price below 250 (see above)
#but no date/regist link made
old$yearmon <- as.yearmon(old$selldate_formatted)
new$yearmon <- as.yearmon(new$DateOfEntryFormatted)

#make single price field in new from Price and Consideration

#First: consideration field is messy. Pounds, commas
#And most annoyingly, a few with decimal point and pennies.

#Points and pennies first. Just keep the first part
#stringr
new$ConsiderationProcessed <- str_split_fixed(new$Consideration, fixed("."), 2)[,1]

#Now we can just drop commas and pound signs
new$ConsiderationProcessed <- gsub("£","",new$ConsiderationProcessed)
new$ConsiderationProcessed <- gsub(",","",new$ConsiderationProcessed)

#How many of these contain text strings?
# 0        1  
# 1070329  158308 
table(0 + grepl("[^0-9]",new$ConsiderationProcessed))
#Really??
new$test <- 0 + grepl("[^0-9]",new$ConsiderationProcessed)
#Yup!
new$test <- NULL

#Remind me what those price columns look like together
prices <- new[,c(5,12,41)]

#where considerationProcessed is empty
#replace with price
#
#First, price needs to be numeric
#Get rid of Price pound sign
new$PriceProcessed <- gsub("£","",new$Price)
new$ConsiderationProcessed[new$ConsiderationProcessed==""] <- new$PriceProcessed[new$ConsiderationProcessed==""]


#OK so: once all text consideration fields are removed
#Price and remaining consideration should provide ALL prices
#"Value" is only for those considerations with text and shouldn't be used at all
#Get only processed consideration fields with numerical values
newc <- new
#drop all obs with *any* non-numeric.
#Didn't do this before and got e.g. "rent 250pm plus 200000" being 250200000 etc.
#Remove spaces first
newc$ConsiderationProcessed <- gsub(" ","", newc$ConsiderationProcessed)
#newc <- newc[grepl("[^a-z]",newc$ConsiderationProcessed,ignore.case=T),]
#newc <- new[grepl("[0-9][!a-z]",new$ConsiderationProcessed, ignore.case = T)|new$ConsiderationProcessed=="",]

#conversion replaces any that contain text chars as NA
newc$numPrice <- as.integer(newc$ConsiderationProcessed)

# 0         1 
# 1063497   12351
table(0 + is.na(newc$numPrice))

#drop NAs
newc <- newc[!is.na(newc$numPrice),]

#Drop silly prices
newc <- newc[newc$numPrice > 500,]

newc <- newc[order(-newc$numPrice),]

plot(Lc(newc$numPrice))

#Which nearly looks OK, but some of those high prices....?
prices <- newc[,c(5,11,12,41,43)]

#Don't like the look of those! Drop anything above 14 million. We're not getting repeat sales on much of that.
#How many is that?
#128. No biggie.
nrow(newc[newc$numPrice > 14000000,])
#1015
nrow(newc[newc$numPrice > 2000000,])

#A lot more sensible-looking!
plot(Lc(newc$numPrice[newc$numPrice < 2000000]))

#Incidentally, the top price in old is clearly WRONG DIDDLY WRONG
#But let's do some sanity checks when comparing repeats
plot(Lc(old$price))

#OK: dropping anything valued over 4 milzzz
newc <- newc[newc$numPrice < 4000000,]

#A number of those are still multiple addresses when they really shouldn't be.
#min/max geocoding doesn't pick them out.


#Let's save that and move on. Special folder plz!
saveRDS(newc,"JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne.rds")


#~~~~~~~~~
#And what about the price field in old?
#zero
nrow(old[is.na(old$price),])
#zero
nrow(old[old$price=="",])
#180! Well, suppose we can leave them and see how things look on repeat sales
nrow(old[old$price<100,])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CUTTINZ: working out problem with non-matching title numbers----
# from next section

#Something odd with title number
#What do new in Aberdeen look like 
combo$aberdeen <- 0 + (grepl("aberdeen", combo$Subjects.in.brief., ignore.case = T))

table(combo$aberdeen)

#Collect all
#Note: none are two. There should be some summing to 2. Something is amiss.
combo$aberdeen <- combo$aberdeen + (grepl("aberdeen", combo$town, ignore.case = T))

aberdeenlook <- combo[combo$aberdeen > 0,]

#Hah. Check this out: ABE in old and ABN in new.
#Before getting on to any other towns and changes
#Do properties match if we remove the text?
#(Given these are all Aberdeen)
aberdeenlook$numberOnlyTitle <- substring(aberdeenlook$regist,4)

#So yes: address does match on number only
aberdeenlook <- aberdeenlook[order(aberdeenlook$numberOnlyTitle),]

#Question: the number isn't unique, presumably? Minus *all* three letter codes?
combo$numberOnlyTitle <- substring(combo$regist,4)

length(unique(combo$regist))
length(unique(combo$numberOnlyTitle))

#So yeah: regist number not unique.
#Before writing to them - this won't be too hard to check.
#I do need town field from new, though...

#Working with new/old from above, having whittled columns down
#Let's get the unique three letter town codes from each
#33 of them
new3letterCodes <- unique(substring(new$Title.number,1,3))
#Same number
old3letterCodes <- unique(substring(old$regist,1,3))

#how many towns vs counties in new?
#1892
length(unique(new$Post.town))
#65!
length(unique(new$County))

#3 letter code plus town should be same number
new$threelettercode <- substring(new$Title.number,1,3)
newCodes <- unique(new[,c('threelettercode','Post.town')])

#Let's look at both together
oldnewcodes <- data.frame(old=old3letterCodes,new=new3letterCodes)

#OK, let's save those and ask. I could work it out but it's a lot less obvious than at first appears.
write.csv(oldnewcodes, "JessieExtDrive/OldNewCombined/oldnew3lettertitlecodes.csv")

#Quick check that it's the same codes in other versions of old
#Yup!
Nuvva3lettercodes <- sort(unique(substring(nuvva$regist,1,3)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Looking again at the master merge

#Issue: geocodes with diff min/max may be more than one property
#mdsi_plus_address, this
#Which have variable geocodes?
md <- mdsi_plus_address

#Subsetting this way just seems to have stopped working...
#vg <- md[md$Minimum.Easting != md$Maximum.Easting,]
vg <- subset(md, Minimum.Easting != Maximum.Easting)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TESTING: COMBINING OLD AND NEW----
#update: turned out to be more faff. So this is a processing/testing section again.

#new: numPrice field has all prices
old <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/RoS_old_uniqueTitleDatePairs_noregistdropped.rds")
new <- readRDS("JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne.rds")

#WOOP! WOOP!
#Old regist has different 3-letter codes at start
#RoS/Deborah has kindly supplied me with a mapping of one t'other
#So let's do that mapping.
map <- read.csv("JessieExtDrive/OldNewCombined/oldnew3lettertitlecodes_conversionTable.csv", stringsAsFactors = F)

#http://stackoverflow.com/questions/19424709/r-gsub-pattern-vector-and-replacement-vector
old$registUpdated <- mgsub(map$old,map$Land.Register.County.Abbreviations,old$regist)
  
#check
regCheck <- old[,c(9,21)]
regCheck[sample(1:nrow(regCheck),50),]

#Seems good. Now to repeat the merge and see if we have better address matching

#While I'm here, let's drop the silly dates from new
#There should be nothing before, say, Jan 1990
#And nothing after 31.3.14
earliest <- as.Date("01/01/1990", format="%d/%m/%Y")
latest <- as.Date('01/04/2014', format='%d/%m/%Y')

#523
#nrow(new[(new$DateOfEntryFormatted < earliest | new$DateOfEntryFormatted > latest),])

new <- new[(new$DateOfEntryFormatted > earliest & new$DateOfEntryFormatted < latest),]
hist(new$DateOfEntryFormatted, breaks='year')

#Then:
old$yearmon <- as.yearmon(old$selldate_formatted)
#new already has it
#new$yearmon <- as.yearmon(new$DateOfEntryFormatted)

#let's pick some initial fields to keep from both just to see what's what
names(new)
names(old)

#save copies, start new section
saveRDS(old,"JessieExtDrive/Misc_RoS_R_Saves/RoS_old_uniqueTitleDatePairs_noregistdropped_registUpdateMapped.rds")
saveRDS(new,"JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne_DateWindowApplied.rds")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#OLD/NEW MATCHING RECORDS ANALYSIS----
old <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/RoS_old_uniqueTitleDatePairs_noregistdropped_registUpdateMapped.rds")
new <- readRDS("JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne_DateWindowApplied.rds")

#Reduce for easy examininz
new <- new[,c(1,8,13:16,18,28:30,35:38)]
old <- old[,c(4:9,13,19,21)]

#New: same situation of a mix of eastings/northings. I'll come back to that.
#Let's merge and have a look
# combo <- merge(old,new, by.x=c('yearmon','regist'), by.y=c('yearmon','Title.number'), all=T)
combo <- merge(old,new, by.x=c('yearmon','registUpdated'), by.y=c('yearmon','Title.number'), all=T)

combo <- combo[order(combo$registUpdated,combo$yearmon),]

saveRDS(combo, "JessieExtDrive/OldNewCombined/firstCombo.rds")
combo <- readRDS("JessieExtDrive/OldNewCombined/firstCombo.rds")

#So these are ones that appeared in both. Much smaller number than I'd have thought.
#The overlap period's about seven years.
#137K? Thought I got more than this first time round.
#Or it was before fixing old regist wrong codes. And now it's...?
#254480 now. Which is a bit more like it
#But still not what I'd expect from 7 yr overlap.
comboOnlyMerges <- merge(old,new, by.x=c('yearmon','registUpdated'), by.y=c('yearmon','Title.number'))

#then combo should give me my repeat sale figures
#Let's look at a reduced version (already sorted)
red <- combo[,c(1,10,16,2,3,5,11,9,22)]

#OK, so: month match isn't enough of a time band.
#What I want to do now:
#See what the spread of time difference is for titles with the same sale price
#If there's more than two, that's going to be tricky
#But merging on title and price should be a start
tpc <- merge(old,new, by.x=c('price','registUpdated'), 
                         by.y=c('numPrice','Title.number'), all=T)

# tpcOnlyMerges <- merge(old,new, by.x=c('price','registUpdated'), 
#                          by.y=c('numPrice','Title.number'))

tpc <- tpc[order(tpc$registUpdated, tpc$yearmon.x),]

#So for ones where there's a data present for both (and so they merged)
#What's the time difference?
tpc$timeDiff <- tpc$selldate_formatted - tpc$DateOfEntryFormatted

hist(as.numeric(tpc$timeDiff), breaks = 100)
hist(as.numeric(tpc$timeDiff[tpc$timeDiff < 730
                                         & tpc$timeDiff > -730 ]), breaks = 100)
#As years. And yup - year peak.
hist((as.numeric(tpc$timeDiff[tpc$timeDiff < 730
                                         & tpc$timeDiff > -730 ])/(365)), breaks = 100)

#So clear enough - and then that weird peak a year later.
#Which I'm just gonna assume is incorrect data entry.

#How many do we lose outside that four year window?
nrow(tpc[tpc$timeDiff > 730 | tpc$timeDiff < -730,])

#Hmm... let's have a look at those.
tpc$pfoury <- 0 + (tpc$timeDiff > 730 | tpc$timeDiff < -730)

table(tpc$pfoury, useNA = "always")
# 0        1     <NA> 
# 801712   11690 1570165 

tpc <- tpc[order(tpc$timeDiff),]

#So 1.57 million where there was no price/title match.
#And only 11K where the dates are further apart than 4 years.

#Let's look at those 1s
onez <- tpc[tpc$price == 10000,]
#OK, this is utterly bizarre. The above works, this does not.
#Is it to do with NAs not being removed in this first one?
#Yes, that's exactly it. Another lovely example of arbitrary differences.
onez <- tpc[tpc$pfoury == 1,]
#SEE, HOW HARD WAS THAT!!
onez <- subset(tpc, pfoury == 1)

#Check dropoff rate of excluded cases for a deadband.
#How many outside a range?
count <- NULL

#Will be 2000 days either side
for(n in 1:200) {
  
  print(n)
  
  count[n] <- nrow( subset(tpc, (tpc$timeDiff > (n * 10) | tpc$timeDiff < - (n*10) )) )
  
}

#Some more counting
#So tpc is old/new merged on price/title.
#How many had a match?
#We can tell because time diff will be null for those
#where there wasn't both an old sell-date and a new date-of-entry
tpc$hasTimeDiff <- 0 + (!is.na(tpc$timeDiff))
#table(0 + (!is.na(tpc$timeDiff)))

table(tpc$hasTimeDiff)
# 0       1 
# 1570165  813402

#So 813K where there was an Old/new match on price/property.
#Which sounds much closer to the number I would expect.
#But I could do with looking at the dates for those.
#http://www.r-bloggers.com/overlapping-histogram-in-r/
# hist(old$selldate_formatted, col=rgb(1,0,0,0.5),
#      main="Overlapping Histogram", xlab="Variable", breaks="quarters")
# hist(new$DateOfEntryFormatted, col=rgb(0,0,1,0.5), add=T, breaks="quarters")
# box()

#Much better!
olddates <- data.frame(type="old", date=old$selldate_formatted)
newdates <- data.frame(type="new", date=new$DateOfEntryFormatted)

alldates <- rbind(olddates,newdates)

# output <- ggplot(alldates, aes(date, fill=type)) + geom_density(alpha = 0.2)
# output
output <- ggplot(alldates, aes(date, fill=type)) + 
  geom_area(alpha = 0.3, stat="bin", position="identity", colour="black",binwidth=100)
# output <- ggplot(alldates, aes(date, fill=type)) + geom_density(alpha = 0.2)
output



#Let's look at the spread of those that do merge on price.
#Just those, exclude others
mop <- merge(old,new, by.x=c('price','registUpdated'), 
                    by.y=c('numPrice','Title.number'))

#Those are comfortingly the same has "hasTimeDiff" NAs above.
oldmopdates <- data.frame(type="old mop", date=mop$selldate_formatted)
newmopdates <- data.frame(type="new mop", date=mop$DateOfEntryFormatted)

moredates <- rbind(alldates,oldmopdates)
moredates <- rbind(moredates,newmopdates)

output <- ggplot(moredates, aes(date, fill=type)) + 
  geom_area(alpha = 0.3, stat="bin", position="identity", colour="black", binwidth=100)
# output <- ggplot(alldates, aes(date, fill=type)) + geom_density(alpha = 0.2)
output

#Next: for each time period, what's the count of "old only", "new only" and "both merged on title/price"?
#We already have "both" in the "hasTimeDiff" flag.
tpc$oldneworboth <- "both"
#Shouldn't need both na tests in one, but just to sanity-check
tpc$oldneworboth[!is.na(tpc$regist) & is.na(tpc$DateOfEntryFormatted)] <- "oldonly"
tpc$oldneworboth[is.na(tpc$regist) & !is.na(tpc$DateOfEntryFormatted)] <- "newonly"

table(tpc$oldneworboth)

#In order to graph, we'll need to grab dates from both columns.
#For illustration porpoises, doesn't matter which
#onb <- tpc[c(9,16,27)]
onb <- tpc

onb$date <- onb$selldate_formatted
onb$date[is.na(onb$selldate_formatted)] <- onb$DateOfEntryFormatted[is.na(onb$selldate_formatted)]

output <- ggplot(onb, aes(x=date, fill=oldneworboth)) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat = "bin", colour="black", binwidth=365)
output

#Actually, I'd like to see both types of date, old and new, see how different they are
#Different column numbers to above, I'm jumping around with the processing, just to 
#mess with my head some more.
#Here I've only added oldneworboth to the original
bothd <- tpc[,c(9,15,22)]

both_datefromold <- data.frame(type="both_datefromold", date=bothd[bothd$oldneworboth=="both",1])
both_datefromnew <- data.frame(type="both_datefromnew", date=bothd[bothd$oldneworboth=="both",2])
justold <- data.frame(type="oldonly", date=bothd[bothd$oldneworboth=="oldonly",1])
justnew <- data.frame(type="newonly", date=bothd[bothd$oldneworboth=="newonly",2])

allz <- do.call("rbind", list(both_datefromold,both_datefromnew, justold, justnew))

output <- ggplot(allz, aes(x=date, fill=type)) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat="bin", position="identity", colour="black", binwidth=100)
  #geom_area(alpha = 0.3, stat = "bin", binwidth=200)
output


#Overlay all as well
output <- ggplot(onb) + 
  geom_area(data = onb, aes(date, fill=oldneworboth),alpha = 0.3, stat="bin", position="identity", colour="black", binwidth=100)
  geom_area(data = onb, aes(date),alpha = 0.3, stat="bin", position="identity", colour="black", binwidth=100)
  #   geom_area(data = onb, aes(date, fill=oldneworboth), alpha = 0.2) +
  #   geom_area(data = onb, aes(date), alpha = 0.2) 
  
output

#May actually want to stack this one - that'll give total sales
output <- ggplot(onb) + 
  geom_area(data = onb, aes(date, fill=oldneworboth),alpha = 0.3, stat="bin", colour="black", binwidth=200) 
  #geom_area(data = onb, aes(date),alpha = 0.3, stat="bin", colour="black", binwidth=200)
  #   geom_area(data = onb, aes(date, fill=oldneworboth), alpha = 0.2) +
  #   geom_area(data = onb, aes(date), alpha = 0.2) 
  
output

#Possibly lastly: because price match might not be perfect (prices may not be entered correctly)
#Let's just look at other matching title numbers in "old only" and "new only"
onlies <- tpc[tpc$oldneworboth %in% c('oldonly','newonly'), ]

#Needs a dual date column again to get date from each
onlies$date <- onlies$selldate_formatted
onlies$date[is.na(onlies$selldate_formatted)] <- onlies$DateOfEntryFormatted[is.na(onlies$selldate_formatted)]

earliest <- as.Date("01/01/2005", format="%d/%m/%Y")
latest <- as.Date('01/01/2010', format='%d/%m/%Y')

#Let's just look between 05 to 10 to see if there's anything we need to know
orr <- subset(onlies, (date > earliest & date < latest))

orr <- orr[order(orr$registUpdated,orr$date),]

#Look at only cloned title numbers
clz <- orr[orr$registUpdated %in% unique(orr$registUpdated[duplicated(orr$registUpdated)]),]

#Right, so there are huge numbers that are clearly meant to be the same price.
#Some are very close, others (pretty much always in the new stuff) have got digits the wrong way round.

#So checking the tolerance
# smood <- ddply(clz, .(registUpdated), mutate, 
#                roughlyEqualz = (abs(max(price)-min(price)) < 100))
# smood <- ddply(clz[1:2000,], .(registUpdated), mutate, 
#                roughlyEqualz = mean(price))

#Next: try rounding prices, see how many more matches jump out

#To nearest thousand
old$priceRounded <- round(old$price,-3)
new$priceRounded <- round(new$priceCombined,-3)

rndz <- merge(old,new, by.x=c('priceRounded','registUpdated'), 
             by.y=c('priceRounded','Title.number'))
             #by.y=c('priceRounded','Title.number'), all=T)



#Oh, that made a tiny difference! Let's try...
#To nearest ten thousand!
old$priceRounded <- round(old$price,-4)
new$priceRounded <- round(new$priceCombined,-4)

rndz2 <- merge(old,new, by.x=c('priceRounded','registUpdated'), 
              by.y=c('priceRounded','Title.number'))
              #by.y=c('priceRounded','Title.number'), all=T)

#That's less merges. How is that possible??
#Ones that were the same price must end up the same rounded!

#No it's not, it's more. Removing all=T:
#820889 and 904011
#I'd have expected the all=T  merge
##to shrink if that's the case: two records to one should reduce
#The size, right?

#I suspect this is taking out too many
rndz3 <- merge(old,new, by.x=c('priceRounded','registUpdated'), 
               by.y=c('priceRounded','Title.number'), all=T)

rndz3$oldneworboth <- "both"
rndz3$oldneworboth[!is.na(rndz3$regist) & is.na(rndz3$DateOfEntryFormatted)] <- "oldonly"
rndz3$oldneworboth[is.na(rndz3$regist) & !is.na(rndz3$DateOfEntryFormatted)] <- "newonly"

#Needs a dual date column again to get date from each
rndz3$date <- rndz3$selldate_formatted
rndz3$date[is.na(rndz3$selldate_formatted)] <- rndz3$DateOfEntryFormatted[is.na(rndz3$selldate_formatted)]

output <- ggplot(rndz3) + 
  geom_area(data = rndz3, aes(date, fill=oldneworboth),alpha = 0.3, stat="bin", colour="black", binwidth=200) 
#geom_area(data = onb, aes(date),alpha = 0.3, stat="bin", colour="black", binwidth=200)
#   geom_area(data = onb, aes(date, fill=oldneworboth), alpha = 0.2) +
#   geom_area(data = onb, aes(date), alpha = 0.2) 

output


#~~~~~~~~~
#Next check: just for old, how often do repeat sales have *exactly* the the same price?
#Given I'm proposing to match on price to identify the same sale in old/new
#This'll help validate that or show it's dumb
old <- old[order(old$registUpdated),]
oldchk <- old[,c(21,19,6:8,13)]

#If any regist/price pairs are the same, they're duplicates...
oonq <- subset(oldchk, !duplicated(oldchk[c('registUpdated','price')]))

#some...
dups <- subset(oldchk, (duplicated(oldchk[c('registUpdated','price')])
                        |duplicated(oldchk[c('registUpdated','price')], fromLast = T)))



#~~~~~~~~~~~~~~~~~~
# And! What does matching on new/application date look like?
new$appyearmon <- as.yearmon(new$applicationDateFormatted)

names(new)[names(new)=='yearmon'] <- 'doe_yearmon'

#Huh - 729733. A goodly number!
appmerges <- merge(old,new, by.x=c('yearmon','registUpdated'), by.y=c('appyearmon','Title.number'))

appmerges2 <- appmerges[,c('yearmon','registUpdated','strno','street','town',
                          'price','numPrice','selldate_formatted','Subjects.in.brief.',
                          'DateOfEntryFormatted','applicationDateFormatted')]

#Yeah, those are all looking like the correct matches. Selldate ain't selldate.


appcombo <- merge(old,new, by.x=c('yearmon','registUpdated'), by.y=c('appyearmon','Title.number'), all = T)

appcombo$oldneworboth <- "both"
#Shouldn't need both na tests in one, but just to sanity-check
appcombo$oldneworboth[!is.na(appcombo$regist) & is.na(appcombo$DateOfEntryFormatted)] <- "oldonly"
appcombo$oldneworboth[is.na(appcombo$regist) & !is.na(appcombo$DateOfEntryFormatted)] <- "newonly"

table(appcombo$oldneworboth)

#In order to graph, we'll need to grab dates from both columns.
#For illustration porpoises, doesn't matter which
#appcombo <- tpc[c(9,16,27)]

appcombo$date <- appcombo$selldate_formatted
appcombo$date[is.na(appcombo$selldate_formatted)] <- appcombo$applicationDateFormatted[is.na(appcombo$selldate_formatted)]

output <- ggplot(appcombo, aes(x=date, fill=oldneworboth)) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat = "bin", colour="black", binwidth=365)
output




#So next: what do the old-only/new-onlies look like this time?
onlies <- appcombo[appcombo$oldneworboth %in% c('oldonly','newonly'), ]
onlies <- onlies[,c('yearmon','registUpdated','strno','street','town',
    'price','numPrice','selldate_formatted','Subjects.in.brief.',
    'DateOfEntryFormatted','applicationDateFormatted','date')]


#Needs a dual date column again to get date from each
#onlies$date <- onlies$selldate_formatted
#onlies$date[is.na(onlies$selldate_formatted)] <- onlies$DateOfEntryFormatted[is.na(onlies$selldate_formatted)]

earliest <- as.Date("01/01/2005", format="%d/%m/%Y")
latest <- as.Date('01/01/2010', format='%d/%m/%Y')

#Let's just look between 05 to 10 to see if there's anything we need to know
orr <- subset(onlies, (date > earliest & date < latest))

orr <- orr[order(orr$registUpdated,orr$date),]

#Look at only cloned title numbers
clz <- orr[orr$registUpdated %in% unique(orr$registUpdated[duplicated(orr$registUpdated)]),]

#Fascinating: the *vast* majority of these are the same in old/new- but *exactly* a year apart
#And the ones that are a year apart are all December. This seems to be confirmed in the original data
#Although it turns out the year is *correct* in the separate 'month' and 'year' fields in old.
#So...


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OLD/NEW LINK: THIS ONE APPEARS TO BE IT, FINALLY----
#RE-RUN WITH UPDATED OLD-ROS DATA, REGIST SORTED AND LINKED TO NEW WHERE POSS
old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2_mergedNewAddressTitles_finalRegistSet.rds")

#Redo date field in old data from month/year fields.

old$newDateFormattedTemp <- paste0("01/",old$month,"/", old$year) 
old$newDateFormatted <- as.Date(old$newDateFormattedTemp, format='%d/%m/%y')
old$newDateFormattedTemp <- NULL

#Now let's repeat all that for our new field
old$yearmon2 <- as.yearmon(old$newDateFormatted)

#Save and reload these versions. They're hopefully final
#Old updated from no-regist link work below
saveRDS(old,"JessieExtDrive/Misc_RoS_R_Saves/UPDATED_RoS_old_uniqueTitleDatePairs_noregistdropped_registUpdateMapped_dateFixed.rds")
#saveRDS(old,"JessieExtDrive/Misc_RoS_R_Saves/RoS_old_uniqueTitleDatePairs_noregistdropped_registUpdateMapped_dateFixed.rds")
saveRDS(new,"JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne_DateWindowApplied_appyearmonadded.rds")

#~~~~~~~~~~~~

old <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/UPDATED_RoS_old_uniqueTitleDatePairs_noregistdropped_registUpdateMapped_dateFixed.rds")
#old <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/RoS_old_uniqueTitleDatePairs_noregistdropped_registUpdateMapped_dateFixed.rds")
new <- readRDS("JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne_DateWindowApplied_appyearmonadded.rds")

names(old)[names(old)=="newRegist"] <- "registUpdated"

appmerges <- merge(old,new, by.x=c('yearmon2','registUpdated'), by.y=c('appyearmon','Title.number'))

appmerges2 <- appmerges[,c('yearmon2','registUpdated','strno','street','town',
                           'price','numPrice','selldate_formatted','Subjects.in.brief.',
                           'DateOfEntryFormatted','applicationDateFormatted')]


appmerges2 <- appmerges2[order(appmerges2$registUpdated,appmerges2$yearmon2),]

#Check any where old/new match does *not* have the same price
appmerges2$pmcheck <- abs(appmerges2$price-appmerges2$numPrice)
table(0 + (appmerges2$pmcheck > 0))

#4050 don't have same price
#look at extremes
appmerges2 <- appmerges2[order(-appmerges2$pmcheck),]

#So the vast majority are just one digit out
#Either one too many or the wrong way round
#Mistakes are all in new. Are there any that aren't these simple errors?
#Use stringdist to find out - small digit differences should have low score
appmerges2$pricedist <- stringdist(as.character(appmerges2$price),as.character(appmerges2$numPrice))

appmerges2 <- appmerges2[order(-appmerges2$pricedist),]

#OK, conclusion: all are actually matching prices
#Apart from a very small number of candidates
#So keep the merges but use the price from old for the ones we can.
#Note these price errors are likely to appear in the rest of new as well.


#appcombo <- merge(old,new, by.x=c('yearmon2','registUpdated'), by.y=c('appyearmon','Title.number'), all = T)
appcombo <- merge(old,new, by.x=c('yearmon2','registUpdated'), by.y=c('appyearmon','Title.number'), all = T)

appcombo$oldneworboth <- "both"
#Shouldn't need both na tests in one, but just to sanity-check
appcombo$oldneworboth[!is.na(appcombo$regist) & is.na(appcombo$DateOfEntryFormatted)] <- "oldonly"
appcombo$oldneworboth[is.na(appcombo$regist) & !is.na(appcombo$DateOfEntryFormatted)] <- "newonly"

table(appcombo$oldneworboth)

#In order to graph, we'll need to grab dates from both columns.
#For illustration porpoises, doesn't matter which
#appcombo <- tpc[c(9,16,27)]

appcombo$date <- appcombo$newDateFormatted
appcombo$date[is.na(appcombo$newDateFormatted)] <- appcombo$applicationDateFormatted[is.na(appcombo$newDateFormatted)]

output <- ggplot(appcombo, aes(x=date, fill=oldneworboth)) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat = "bin", colour="black", binwidth=365)
output



#And what remains now?
#what do the old-only/new-onlies look like this time?
onlies <- appcombo[appcombo$oldneworboth %in% c('oldonly','newonly'), ]
onlies <- onlies[,c('yearmon','registUpdated','strno','street','town',
                    'price','numPrice','selldate_formatted','Subjects.in.brief.',
                    'DateOfEntryFormatted','applicationDateFormatted','date')]


#Needs a dual date column again to get date from each
#onlies$date <- onlies$selldate_formatted
#onlies$date[is.na(onlies$selldate_formatted)] <- onlies$DateOfEntryFormatted[is.na(onlies$selldate_formatted)]

earliest <- as.Date("01/01/2005", format="%d/%m/%Y")
latest <- as.Date('01/01/2010', format='%d/%m/%Y')

#Let's just look between 05 to 10 to see if there's anything we need to know
orr <- subset(onlies, (date > earliest & date < latest))

orr <- orr[order(orr$registUpdated,orr$date),]

#Look at only cloned title numbers
clz <- orr[orr$registUpdated %in% unique(orr$registUpdated[duplicated(orr$registUpdated)]),]

#They look like repeat sales! WOOOOHOOOOOO!
#saved merged
saveRDS(appcombo,"JessieExtDrive/OldNewCombined/UPDATED_rawOldNewMerged_on_appdate_n_correctedRegist.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TIDY FINAL MERGED OLD/NEW----
mrg <- readRDS("JessieExtDrive/OldNewCombined/UPDATED_rawOldNewMerged_on_appdate_n_correctedRegist.rds")
#mrg <- readRDS("JessieExtDrive/OldNewCombined/rawOldNewMerged_on_appdate_n_correctedRegist.rds")

#Keep only necessary fields.
# mrg <- mrg[,c('yearmon2','registUpdated','','','','','','','','','','')]
#Are these the same fields after an update?
mrg <- mrg[,c(1,2,6:9,23,10:11,16,24:25,33:35,41:48,51:58,63:65,68:70)]

#Date is already a combined field
#For price: for old and 'both', use old. See above: 'new' gets price wrong, 'old' seems to hardly at all
names(mrg)[names(mrg)=='price'] <- 'oldRoS_price'
names(mrg)[names(mrg)=='numPrice'] <- 'newRoS_price'

mrg$priceFinal <- mrg$oldRoS_price
mrg$priceFinal[mrg$oldneworboth == 'newonly'] <- mrg$newRoS_price[mrg$oldneworboth == 'newonly']

#Check over matching titles. 
mrg <- mrg[order(mrg$registUpdated,mrg$yearmon2),]

#SAVE A VERSION FOR COMBINING WITH NON-REGIST OLD LATER ON
#Where some new repeat sales might/should appear.
saveRDS(mrg,"JessieExtDrive/OldNewCombined/UPDATED_OldNewMergedTidied_on_appdate_n_correctedRegist_finalPrice.rds")

#Keep only multiple regists
#which should now all be repeat sales, right?
rpt <- subset(mrg, (duplicated(mrg[c('registUpdated')])|duplicated(mrg[c('registUpdated')], fromLast = T)))

#723394 unique properties
nrow(distinct(rpt,registUpdated))

#Some ridiculous prices will need removing. 
# output <- ggplot(rpt[rpt$priceFinal < 4000000,], aes(x=date,y=priceFinal, colour=oldneworboth)) +
#   geom_point()
# output

#And straight into...


#~~~~~~~~~~~~~~~~~~~~~~~
# GEOCODING REPEAT SALES----
mrg <- readRDS("JessieExtDrive/OldNewCombined/UPDATED_OldNewMergedTidied_on_appdate_n_correctedRegist_finalPrice.rds")

#So only one geocode per property needed!
#Starting with 'new only' and 'both' that will have geocodes (for those as has em) from new

#Actually, I said I'd defer to the previous geocodes. So I should probably try and merge those in first
oldgeo <- read.dta13("C:/Users/SMI2/Dropbox/WindFarms/Address Matching/data_geocodes_matched.dta")

#Avoid clash with new address e/ns
names(oldgeo)[names(oldgeo)=='Eastings'] <- 'oldRoS_eastings'
names(oldgeo)[names(oldgeo)=='Northings'] <- 'oldRoS_northings'

table(0 + is.na(oldgeo$oldRoS_eastings))

#Drop those with no geocoding
oldgeo <- oldgeo[!is.na(oldgeo$oldRoS_eastings),]

#Update: why on Earth did I think I needed the date??

#Think I need a matching yearmon field to merge properly
#Seeing as I dropped the month/year fields, gotta do this...
# oldgeo$newDateFormattedTemp <- paste0("01/",oldgeo$month,"/", oldgeo$year_txt) 
# oldgeo$newDateFormatted <- as.Date(oldgeo$newDateFormattedTemp, format='%d/%m/%Y')
# oldgeo$newDateFormattedTemp <- NULL
# 
# oldgeo$yearmon2 <- as.yearmon(oldgeo$newDateFormatted)


#get unique oldgeos too. We don't need repeats, just single addresses
# unqoldgeo <- subset(oldgeo,!duplicated(oldgeo[,c('strno','street','flatpos','town')]))
#730K unique geocodes
unqoldgeo <- subset(oldgeo,!duplicated(oldgeo[,c('oldRoS_eastings','oldRoS_northings')]))

#I think this might be easier, actually, if I:
#Add the title number, where I can, to the geocoded data
#Then use the title number to merge *back* into all the repeat sales
#I'll also then be able to compare old/new geocoding

#Thusly. One: reload old data, with updated title field
old <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/UPDATED_RoS_old_uniqueTitleDatePairs_noregistdropped_registUpdateMapped_dateFixed.rds")
#old <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/RoS_old_uniqueTitleDatePairs_noregistdropped_registUpdateMapped_dateFixed.rds")



#Get unique title numbers from old
oldunq <- subset(old, !duplicated(old[,'newRegist']))

oldgeo_plus_title <- merge(unqoldgeo[,c(1:2,7:10, 14,26)],oldunq[,c(4:7,21,9,14,24)],
                           # by = c('strno','flatpos','street','town','yearmon2'))
                           # by = c('strno','flatpos','street','town'))
                           by.x = c('strno','flatpos','street','frozen_pcode_04'),
                           by.y = c('strno','flatpos','orig_street','pcode')
#                            by.x = c('strno','flatpos','street','town','frozen_pcode_04','yearmon2'),
#                            by.y = c('strno','flatpos','street','town','pcode','yearmon2')
                           )

length(unique(oldgeo_plus_title$registUpdated))
#210698. Not good! It'll have to do for now.
#Update: 615K now. Better!

#So all I need to keep for merging into the repeats is the geocodes and the title
old_geoc_title <- oldgeo_plus_title[,c(5:6,12)]
#*Only* uniques, none with more than one title number
#(Because we don't know which is correct)
#Going from 615169 to... 587836
old_geoc_title <- subset(old_geoc_title, (!duplicated(old_geoc_title$newRegist) 
                                          & !duplicated(old_geoc_title$newRegist,fromLast = T)) )

#I'm just going to save that old-RoS title-geoc pairing for later...
saveRDS(old_geoc_title,"JessieExtDrive/OldNewCombined/old_ros_geocodeTitleMap.rds")

#~~~~~~~~

#merge on a subset of address fields.
#Later can apply to same title, if appropriate
rpt_plusoldgeo <- merge(rpt,old_geoc_title, 
                by = 'newRegist',
                all.x = T)

#So how many did we get a location for?
#222197. Hmm.
table(0 + is.na(rpt_plusoldgeo$oldRoS_northings[rpt_plusoldgeo$oldneworboth %in% c('oldonly','both')]))

#save repeats so far, press on
saveRDS(rpt_plusoldgeo,"JessieExtDrive/OldNewCombined/oldnew_repeats_oldgeocodesadded.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Checking geocodes from new----

rpt <-  readRDS("JessieExtDrive/OldNewCombined/oldnew_repeats_oldgeocodesadded.rds")

#Reminder: we have two versions of geocodes, all variably populated:
#four min/max fields and a single pair from 'address'.

#First thing to check: how many of the min/max are different?
table(0 + ((rpt$Minimum.Easting - rpt$Maximum.Easting)!=0))
table(0 + ((rpt$Minimum.Northing - rpt$Maximum.Northing)!=0))
# 0         1 
# 740961    833

#Let's just look at those: going to be multiple addresses probably
spr <- rpt[((rpt$Minimum.Northing - rpt$Maximum.Northing)!=0)&!is.na(rpt$Maximum.Northing),]

#Yup, multiple addresses. Drop em.
#This includes NAs, which we want!
rpt2 <- rpt[((rpt$Minimum.Northing - rpt$Maximum.Northing)==0)|is.na(rpt$Maximum.Northing),]

#Yup, right number. Drop previous
rpt <- NULL

#Set any new-geocode empty cells to NA (there are a few)
#It's only in 'Eastings'/'Northings'
table(0 + (rpt2$Eastings==""))
table(0 + (rpt2$Northings==""))

rpt2$Eastings[rpt2$Eastings==""] <- NA
rpt2$Northings[rpt2$Northings==""] <- NA

#So, remaining min/maxs are all the same and we can just keep one.
#We can then also keep the geocodes from address.
#Then see what remains.
#Let's just count how many in both / how many overlap
table(0 + is.na(rpt2$Minimum.Easting))
#740961 mix/max
table(0 + is.na(rpt2$Eastings))
#526042
table(0 + (is.na(rpt2$Eastings) & is.na(rpt2$Minimum.Easting)))
#753782 coded. Well that's not very many!
#How many of these were from new?
table(0 + (is.na(rpt2$Application_yearmon)))
#763076
#Oh OK, it's about right then!
#And... how many are geocoded at all from all?
table(0 + (is.na(rpt2$Eastings) & is.na(rpt2$Minimum.Easting) & is.na(rpt2$oldRoS_eastings)))
# 0      1 
# 902442 660451
#Though note, this is before we've applied new geocoded titles to any old with the same title.

#So first: just make a new geocode column that contains what we have from both
rpt2$newRoS_eastings <- rpt2$Minimum.Easting
rpt2$newRoS_eastings[is.na(rpt2$Minimum.Easting)] <- rpt2$Eastings[is.na(rpt2$Minimum.Easting)]
rpt2$newRoS_northings <- rpt2$Minimum.Northing
rpt2$newRoS_northings[is.na(rpt2$Minimum.Northing)] <- rpt2$Northings[is.na(rpt2$Minimum.Northing)]
#So that should now equal... Yup! the is.na sums for both above
table(0 + is.na(rpt2$newRoS_eastings))

#Divide ones that are there by 10
#To convert to metres (currently they're 10cm though most only have 0 in last digit
rpt2$newRoS_eastings <- as.numeric(rpt2$newRoS_eastings)/10
rpt2$newRoS_northings <- as.numeric(rpt2$newRoS_northings)/10

#Before combining the old ones, let's now use these
#to see if they can be applied to any from old
#So first, make a little unique title/new-geocode list
unoo <- subset(rpt2[,c(1,37,38)], !duplicated(rpt2[,c(1,37,38)]))
unoo <- unoo[!is.na(unoo$newRoS_northings),]
#So only actually 503K addresses geocoded there!

#Uh oh... 
#So some regists don't have unique locations. In fact, quite a lot: 24464
length(unique(unoo$registUpdated))

#look at em
dups <- subset(rpt2[,c(1,10,37,38)], !duplicated(rpt2[,c(1,37,38)]))
dups <- dups[!is.na(dups$newRoS_northings),]

#Then keep only duplicated regists
dups <- subset(dups,
               (duplicated(dups[,c('registUpdated')])
                |duplicated(dups[,c('registUpdated')],fromLast = T)))

#Most a small distance apart
#How small? Recall the extra digit - units are 10cm, though mostly 0
#Distance to first regist in list
dups$firstRegist <- dups$registUpdated[which(!duplicated(dups$registUpdated))]

meanRegist <- as.data.frame(tapply(dups$newRoS_eastings,dups$registUpdated,mean))

#summarise works, mutate doesn't
dups2 <- ddply(dups, .(registUpdated), summarise, dist = min(as.integer(newRoS_eastings)))
#so merge back in!
dups <- merge(dups,dups2,by='registUpdated')
#How much does min differ?
dups$diff <- as.integer(dups$newRoS_eastings) - dups$dist
hist(log10(dups$diff))
#OK: some of those are VERY far apart. KMs. Even though same postcode.
#These aren't ones I've merged back in yet, they're from the original data
#But they may be from address
#Probably best to drop for now (though 20K properties)
#And geocode again later

#So keep only title number/geocodes with *no* duplicates
#i.e. don't keep one unique from sets containing duplicates
#Cos we don't know which is correct
unoo2 <- subset(unoo,(!duplicated(unoo$registUpdated)&!duplicated(unoo$registUpdated, fromLast = T)))
#456606 left
#Correct
length(unique(unoo2$registUpdated))

#Now: apply back in. Replacements should all be in the same place.
#If it grows, there's an issue!
#Didn't grow - after rejecting duplicates above
rpt3 <- merge(rpt2[,c(1:36)],unoo2, by = 'registUpdated', all.x = T)

#difference in new-geocoding between the two?
table(0 + is.na(rpt2$newRoS_eastings))
table(0 + is.na(rpt3$newRoS_eastings))
#1,225,963 vs 748,129 before. Not bad!

#And with old geocodes?
table(0 + (is.na(rpt3$newRoS_eastings) & is.na(rpt3$oldRoS_eastings)))
#1,290,270 leaving 272,623 without

#Which is how many unique addresses?
length(unique(rpt3$registUpdated))
#591614. Less than previous 682842 by some way? Hmm.

#Well let's finish up and add in the final old-geocodes.
#New have been converted to metres (above) from 10cm so these are now in the same units
rpt3$newRoS_eastings[is.na(rpt3$newRoS_eastings)] <- rpt3$oldRoS_eastings[is.na(rpt3$newRoS_eastings)]
rpt3$newRoS_northings[is.na(rpt3$newRoS_northings)] <- rpt3$oldRoS_northings[is.na(rpt3$newRoS_northings)]

#Correct
table(0 + is.na(rpt3$newRoS_eastings))

#Save!
saveRDS(rpt3,"JessieExtDrive/OldNewCombined/oldnew_repeats_oldnewgeocodescombined.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXAMINE THE GEOCODING THUS FAR----
rpt <- readRDS("JessieExtDrive/OldNewCombined/oldnew_repeats_oldnewgeocodescombined.rds")

#Still looking the same shape. Which it should, obv, but thought I'd check.
output <- ggplot(rpt, aes(x=date, fill=oldneworboth)) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat = "bin", colour="black", binwidth=365)
output

rpt <- rpt[order(rpt$registUpdated,rpt$yearmon2),]

#how many unique properties?
#591614
length(unique(rpt$registUpdated))

#how many unique properties with geocode?
#482945
length(unique(rpt[!is.na(rpt$newRoS_northings),c('registUpdated')]))

#How many repeat sales of each?
#rptcount <- as.data.frame(tapply(rpt$newRoS_northings,rpt$registUpdated,FUN=length))
rptcount <- aggregate(rpt$newRoS_northings, list(rpt$registUpdated), length)

#Why are there some ones? The larger values are obviously going to be wrong too!
table(rptcount$x)

rptcount <- rptcount[order(-rptcount$x),]

#Looking at some...
#43 repeats. Many different addresses
rpt[rpt$registUpdated=='LAN100000',]

#Need to merge back in to look seeing as ddply doesn't work on this size of data
rpt2 <- merge(rpt,rptcount,by.x='registUpdated',by.y='Group.1')

rpt2 <- rpt2[order(-rpt2$x),]

#Let's just have a look at the 9x sales
ninet <- rpt2[rpt2$x == 9 & rpt2$priceFinal < 2000000,c(1,33,34)]

output <- ggplot(ninet,aes(x = date, y = priceFinal, colour=registUpdated)) +
  geom_line() +
  theme(legend.position = "none") 
output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SAVE CURRENT GEOCODED PROPERTIES FOR VIEWSHED PROCESSING----

#So keep unique properties that have geocodes. Save regist and location.
#Where to? 
#C:\Data\WindFarmViewShed\ViewshedPython\Data
#Drop any with more than 9 sales. They look dodgy (and there are very few)
sv <- rpt2[rpt2$x < 10,]

#keep only unique properties with locations
sv <- subset(sv,(!duplicated(sv$registUpdated) & !is.na(sv$newRoS_northings)))

#Oops - still wrong order of magnitude! This works ->
sv$newRoS_eastings <- sv$newRoS_eastings/10
sv$newRoS_northings <- sv$newRoS_northings/10

#save this version of repeat sales as single properties
saveRDS(sv, "JessieExtDrive/OldNewCombined/oldnew_repeats_singlePropertiesForGeocoding.rds")

#add new index column, don't use row names as index
#This will index an array of house objects directly in Java
#This doesn't work
#sv$id <- seq(0:(nrow(sv)-1))
#This does
sv$id <- seq(from = 0, to = (nrow(sv)-1))

write.csv(sv[,c(40,1,37:38)], "C:/Data/WindFarmViewShed/ViewshedPython/Data/geocodedOldNewRoS.csv",
          row.names = F)

#check on uniqueness of saved CSV id column
#savez <- read.csv("C:/Data/WindFarmViewShed/ViewshedPython/Data/geocodedOldNewRoS.csv")

#~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~
# FIRST LOOK: IDENTIFYING OLD-ROS WITH NO REGIST AS THE SAME PROPERTY----

#So the job here: looking ONLY at those old-RoS with no regist
#How do we go about finding matches for the same properties?
#I'm just going to compare to the old/new linked file - there weren't many missing new.

#Reload linked file - 
#Note this is from "#SAVE A VERSION FOR COMBINING WITH NON-REGIST OLD LATER ON"
mrg <- readRDS("JessieExtDrive/OldNewCombined/OldNewMergedTidied_on_appdate_n_correctedRegist_finalPrice.rds")

#Load RoS_old...
RoS_old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted.rds")

#How many have stupid low price?
nrow(filter(RoS_old,price<100))

#Drop em
RoS_old <- filter(RoS_old,price>100)

#OK, I'm going to look at this from scratch. Reading the code above is just confusing matters.
#Let's look again at those with and without regist
noreg <- filter(RoS_old, regist=="")

noreg <- arrange(noreg,street,strno)

#Ah, so: plenty with no actual address. Those aren't going to be much use, are they? How many?
#~12,516
nrow(filter(noreg, street=="",strno==""))

#Remove obs with neither street nor number
#We'll look separately at no-number, to see what can be seen
noreg <- filter(noreg,street!="",strno!="")

#Bear in mind this is all prior to updating the regist codes so that'll need doing later.

RoS_old <- arrange(RoS_old,street,strno,year)

#How many with really low prices?
nrow(filter(RoS_old,price < 8000))

RoS_old$priceInThousands <- RoS_old$price/1000

#To avoid non-friendly sci nums:
#http://stackoverflow.com/questions/11987308/number-formatting-axis-labels-in-ggplot2
ggplot(RoS_old, aes(priceInThousands)) +
  geom_histogram() +
  scale_x_log10(labels=comma) 

ggplot(filter(RoS_old, price < 20000), aes(price)) +
  geom_histogram(binwidth = 200) 
  #geom_freqpoly()
  #scale_x_log10(labels=comma) 

# RoS_old2 <- RoS_old %>% 
#   group_by(strno,flatpos,street, propty) %>% 
#   mutate(propertyID = group_indices())
#seq(1:nrow(RoS_old %>% distinct(strno,flatpos,street, propty)))

RoS_old2 <- RoS_old %>% 
  #mutate(propertyID = group_indices(RoS_old, strno,flatpos,street, propty))
  mutate(propertyID = 1)

#Is this not working simply cos it's too big? This would just be tag in Stata...
RoS_old_small <- slice(RoS_old, 1:10000)

#Yup, this is fine. OK then.
# RoS_old2 <- RoS_old_small %>% 
#   mutate(propertyID = group_indices(RoS_old_small, strno,flatpos,street, propty))

RoSold_DT <- data.table(RoS_old)

#Jesus. I think that might have worked
#I've lost the damn link now - .GRP is the group index for using with data tables
RoSold_DT[, propertyID:=.GRP, by = list(strno,flatpos,street, propty)]

RoSold2 <- data.frame(RoSold_DT)

RoSold2 <- arrange(RoSold2,propertyID)

#Remove ones with no street or number
RoSold2 <- filter(RoSold2,street!="",strno!="")

#Look at relevant columns to get a feel for what's going on
subz <- select(RoSold2,street,strno,flatpos,town,pcode,regist,month,year,price,selldate_formatted,propertyID)

#~~~~~~~~~~~~~~~~~~~~~~
# SECOND LOOK: PROCESS OLD-ROS TO LINK REGIST AND NON-REGIST----

#Reload, run from scratch
#Load RoS_old...
RoS_old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted.rds")

#First job: re-run the title re-coding so it matches with new-RoS
#Do that here so I don't have to re-apply to updated regist
map <- read.csv("JessieExtDrive/OldNewCombined/oldnew3lettertitlecodes_conversionTable.csv", stringsAsFactors = F)

#http://stackoverflow.com/questions/19424709/r-gsub-pattern-vector-and-replacement-vector
#I have checked this, so it's OK to directly replace!
RoS_old$regist <- mgsub(map$old,map$Land.Register.County.Abbreviations,RoS_old$regist)


#2774599 obs to start.
#Drop money amounts that make little sense
#Drop obs with no useable address / no address - 
#but only those with no regist, since we can still potentially match others on regist

#This is what we're aiming to remove (12516)
nrow(filter(RoS_old, street=="",strno=="", regist==""))

#2762083
RoS_old <- subset(RoS_old, !(street=="" & strno=="" & regist==""))

#Drop obs below certain price - see histogram in above section
#Or graph in windfarm planner doc section "Housing: getting the last old-RoS"

#2665689. 96394 dropped.
RoS_old <- filter(RoS_old,price>4999)


#~~~~~~~~~~~~~~~~~~~~~~~~~~
#POSTCODE LOOKINZ

#Look at how much postcode / number might help match
# RoS_old <- arrange(RoS_old,pcode,as.numeric(strno))
# 
# #How many don't have postcode at all?
# #26720
# nrow(filter(RoS_old,pcode==""))
# 
# #Look at those *with* postcodes
# old_pc <- RoS_old %>% filter(pcode!="")
# 
# #A huge number are only one-part postcodes. The rest consistently contain a space so...
# #85648
# nrow(RoS_old %>% filter(pcode=="" | !grepl(" ",pcode)))
# #Look at only those with postcodes and full postcodes
# old_pc <- RoS_old %>% filter(pcode!="" & grepl(" ",pcode))
# 
# #Oh, and we can't do this check no those with no street number
# #How many is that in the ones we now have?
# #152145. Quite a lot! 
# nrow(filter(old_pc, strno==""))
# 
# #Well, let's remove those with no street number and see
# old_pc <- RoS_old %>% filter(pcode!="" & grepl(" ",pcode) & strno!="")

#NEWP, IGNORE POSTCODE!
#~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~
#UPDATE STREETS TO CONVERT STREET TYPE TO CONSISTENT LONG-FORM
#(Where possible)
#Work on new copy of street for now

#First, remove text in street after commas
#This may or may not improve things.
RoS_old$streetnew <- gsub(',.*','', RoS_old$street)

#Now apply long-street-type map to this new
#First we need to separate it out for a merge...
RoS_old$street_end <- word(RoS_old$streetnew,-1)

#Load short-long map (put together below)
str_map <- read.csv("C:/Data/Housing/JessieExtDrive/OldNewCombined/streetendabbreviation_map.csv", as.is = T)

#Merge in the short-long map
RoS_old <- merge(RoS_old,str_map,by.x="street_end", by.y = "short", all.x=T)

#Combine all-but-last-word street with the new long-end
RoS_old$finalStreet <- ""

#only replace those that aren't empty - avoids having to tidy later
RoS_old$finalStreet[RoS_old$street!=""] <- 
  paste0(word(RoS_old$streetnew[RoS_old$street!=""],start=1,end=-2)," ", RoS_old$long[RoS_old$street!=""])

#Any NAs in the long indicates where we should just use the original street name
RoS_old$finalStreet[is.na(RoS_old$long)] <- RoS_old$streetnew[is.na(RoS_old$long)]

#It's looking about right...
#Final column processing
names(RoS_old)[names(RoS_old)=="street"] <- "orig_street"
#This might be wrong later - for some reason, "street_end" got put at the start
RoS_old <- RoS_old[,c(2:6,23,7:20)]

#now everything else should work... (Except any index refs!)
names(RoS_old)[names(RoS_old)=="finalStreet"] <- "street"

#And think I might save that before carrying on!
saveRDS(RoS_old, "JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registFromNew_streetNamesLongd.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MORE NON-REGIST ADDRESS-MATCHING after street-name processing and new-regist fixing----
RoS_old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registFromNew_streetNamesLongd.rds")


# RoS_old2 <- RoS_old %>% 
#    group_by(chars,strno,flatpos,street,propty,town) %>% 
#    mutate(propertyID = group_indices())

#Trying all that again with reduced town name - only the word before any commas
names(RoS_old)[names(RoS_old)=="town"] <- "oldtown"
RoS_old$town <- gsub(',.*','', RoS_old$oldtown)

#Then: give all properties with same address a group index using data.table
RoSold_DT <- data.table(RoS_old)

#Jesus. I think that might have worked
#I've lost the damn link now - .GRP is the group index for using with data tables
RoSold_DT[, propertyID:=.GRP, by = list(chars,strno,flatpos,street,propty,town)]


RoS_old <- data.frame(RoSold_DT)

RoS_old <- arrange(RoS_old,propertyID)

rm(RoSold_DT)

#Currently false. If I make factor, should then be able to pull out factor number
#For checking for equality in groups
is.factor(RoS_old$regist)

RoS_old$registFactor <- factor(RoS_old$regist)
#Empty cells are the first factor, so indexed as 1
RoS_old$registFactorIndex <- as.numeric(RoS_old$registFactor)

#What we want to do now:
#Before attempting to propagate all regists to any blank fields
#(Those with factor index 1)
#Need to check for propertyIDs with more than one regist.
#If the 1s are all removed, these will have a factor index SD > 0

chkReg <- filter(RoS_old, registFactorIndex != 1)

chkReg <- arrange(chkReg, registFactor)

chkReg <- chkReg %>% 
  group_by(propertyID) %>% 
  mutate(factorSD = sd(registFactorIndex))

#Look at propertyIDs with differing regists
nonz <- filter(chkReg,factorSD > 0)

nonz <- arrange(nonz,street)

#An example
filter(nonz, propertyID == 1067961) %>% 
  arrange(regist,year) %>% 
  select(propty,strno,flatpos,street,town,pcode,regist,month,year,price) %>% 
  #as.character(tbl_df(.)) %>% 
  #writeClipboard()
  tbl_df()

#How many unique titles is that?
#38771
nrow(distinct(nonz,regist))
#How many unique property IDs?
#18218
nrow(distinct(nonz,propertyID))

#Right, decision at the moment is:
#Remove all of these property IDs that have more than one title number
#As it's likely they're split properties

#I can do this by merging the SD value back into RoS_old
#on property ID (unique propertyIDs have unique SDs)

#So it's 18218 distinct properties we're going to drop
#test <- distinct(nonz,propertyID)[,c(20,23)]
RoS_old <- left_join(RoS_old,distinct(nonz,propertyID)[,c('propertyID','factorSD')], by="propertyID")

#Any non-NA columns in factorSD now mark propertyIDs with more than one title
#And they'll also remove non-titled with the same address

#Just test again that we're about to remove ~18K addresses
#Yup!
filter(RoS_old,!is.na(factorSD)) %>% 
  distinct(propertyID) %>% 
  nrow()

#OK, remove:
RoS_old <- filter(RoS_old,is.na(factorSD))

#drop factorSD
RoS_old <- select(RoS_old,-factorSD)

#So we now have RoS_old minus any addresses with multiple titles
#But still containing addresses with single titles and blanks
#NOW: we're going to presume the remaining titles-with-blanks can take the title from that property
#Later we'll test this with a sanity check on how prices change.

#Propagate regists within same properties
#That is: for each property ID (same address) if there are blank regists
#overwrite with existing regist
RoS_old <- RoS_old %>% 
  group_by(propertyID) %>% 
  mutate(newRegistIndex = max(registFactorIndex))

#That gives us the factor index. Use that to get the regist
#RoS_old$newRegist <- RoS_old$registFactor[as.numeric(RoS_old$registFactor) == RoS_old$newRegistIndex]

#I can't think of an easier way to do this than a merge (the above of course has different vector lengths)
#first we need the regists and their equivalent factors
regists <- RoS_old[,c('registFactor','newRegistIndex')] %>% 
  distinct() %>% 
  filter(registFactor!="") %>% 
  arrange(newRegistIndex)

#This will be merged back to replace the old one so give a different name
names(regists)[names(regists)=='registFactor'] <- 'newRegistFactor'

#Is the correct number, looking good.
RoS_old <- left_join(RoS_old, regists, by="newRegistIndex")

#A quick count:
#Of the ~700K that had no regist, how many have been given a regist because of a matching address
#how many remain as single unique addresses?
#673074 no-regists, after removing price floor / no-addresses-w-no-regist
nrow(filter(RoS_old,regist==""))

#So how do those break down by 'given a regist from matching address'
#vs 'single-address no-regist?'
#Look at the table to see why this makes sense ->

#476179 are just single-address no-regists.
#Update: 469565 with processed street names
#Update: 440769 with processed street names AND towns reduced to first word. We have a winner!
nrow(filter(RoS_old, newRegistIndex==1))

#196895 got a regist from a matching address
#Update: 199866 with processed street names. So not a huge number more!
#Update: 228226 with processed street names AND towns reduced to first word. We DEFINITELY have a winner!
#And those do add up to 673074
nrow(filter(RoS_old, registFactorIndex==1 & !is.na(newRegistFactor)))

#Tidy columns before saving and moving on to new-RoS linking
#First, make a "registType" column to mark which came from where
RoS_old$registType <- "regist in original data"
RoS_old$registType[RoS_old$newRegistIndex==1] <- "no regist, single address"
RoS_old$registType[RoS_old$registFactorIndex==1 & !is.na(RoS_old$newRegistFactor)] <- "regist from matching address"

#Change again! Not changing code above cos dependencies below
names(RoS_old)[names(RoS_old)=='newRegistFactor'] <- 'newRegist'

#Drop not-needed cols now
#RoS_old <- RoS_old[,-c(21:23)]
RoS_old <- RoS_old %>% select(-registFactor,-registFactorIndex,-newRegistIndex)

#SAVE! Before moving on to new-RoS linking attempts
saveRDS(RoS_old, "JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TESTING STREET NAME ABBREVIATION CHANGE----
# (That is used in the previous section then re-loaded in the following section)
# PLAN: Old-ros addresses consistently have abbreviated street types
# E.g. st. , gdns, etc. They need re-coding.
# As ros-new uses the full words.

#Let's start by getting a list of the abbreviated street types from old
strtypesabbr <- single_olduniquestreets %>% 
  select(street) %>% 
  # filter(grepl("\\s(\\w+)$",street))
  #filter(grepl('\\s(\\w+)$',street))
  mutate(last = word(street,-1))

#Unique street types...
uniq_strtypes <- unique(strtypesabbr[,3]) %>% 
  arrange(last)

#15K of those. Assume ones over a certain size aren't going to be abbreviations!
uniq_strtypes <- uniq_strtypes %>% 
  filter(nchar(last) < 6)

#1.5K. More manageable.
#Right - gone through and done some manual coding. Result:
str_map <- read.csv("C:/Data/Housing/JessieExtDrive/OldNewCombined/streetendabbreviation_map.csv", as.is = T)

#Test. First, add map to list of streets.
#strtypesabbr$longlast <- mgsub(str_map$V1,str_map$V2,strtypesabbr$last, fixed=T)

#Oh wait - we should be merging. Duh.
# strtypesmrg <- left_join(strtypesabbr,str_map, by = ("last" = "short"))
strtypesmrg <- merge(strtypesabbr,str_map, by.x = "last", by.y = "short", all.x = T)
strtypesmrg <- strtypesmrg[,-c(2,4)]

#Try this:
#insert a leading space into the replacement long ends
str_map$long_lspace <- paste0(" ",str_map$long)
str_map$short_lspace <- paste0(" ",str_map$short)
#Huh - the spaces don't appear in "view". They seem to be there though.
nchar(str_map$long[10])
nchar(str_map$long_lspace[10])
nchar(str_map$short[10])
nchar(str_map$short_lspace[10])

#write after spaces added
write.csv(str_map, "C:/Data/Housing/JessieExtDrive/OldNewCombined/streetendabbreviation_map.csv")

strtypesabbr <- strtypesabbr[,-4]

#So with the space in, we should get a reasonable replacement rate
#Any errors won't affect later old/new matching.
strtypesabbr$newstreet <- mgsub(str_map$short_lspace,str_map$long_lspace,strtypesabbr$street)

#Oh right - I see why that won't work. E.g. RD and ROA are both going to find replacements. It'll get messy.
#Have to stick to the merge method I think. 

#Which involves: getting everything *but* the last word, then adding.
#Yup, this gets all but last
strtypesmrg$newstreet <- word(strtypesmrg$street,start=1,end=-2)

strtypesmrg$finalnewstreet <- paste0(strtypesmrg$newstreet, " ", strtypesmrg$long)

#Well, that worked - once we delete all the NANAs! 
#Question is, where to actually apply it?

#Before that: let's also look at any streets in old that contain commas
#Actually, I've just looked: only in old. And should probably keep only text before commas
#test: tick!
allstreets$nocommas <- gsub(',.*','', allstreets$street)

#Actually, before doing that - just check if there's much doubling up of addresses with commas in
#I've reloaded RoS_old so...
str_w_commaz <- data.frame(RoS_old[grepl(",",RoS_old$street),])

#Hmm...
RoS_old <- RoS_old[order(RoS_old$street),]

#I need to stick all those addresses into one and order them to look.
#And label where they came from. Err. How to do that? Just for street...
# newstreet <- select(new_addresses, street = Thoroughfare) %>% 
#   distinct(street) %>% 
#   mutate(type = "new")
# 
# oldstreet <- select(old_addresses_noRegist, street) %>% 
#   distinct(street) %>% 
#   mutate(type = "old")
# 
# allstreets <- rbind(newstreet,oldstreet) %>% 
#   filter(street!="") %>% 
#   arrange(street)
# 
# #I'm presuming the string match across both works OK?
# inbothunique <- inBoth %>% 
#   select(Thoroughfare) %>% 
#   distinct() %>% 
#   mutate(street = Thoroughfare)
# 
# allstreets <- left_join(allstreets,inbothunique,by="street") %>% 
#   arrange(street)
# 
# #Having arranged... yeah, we've got some messiness in streets there.
# #Let's see about postcodes. How many missing postcodes do we have in each?
# 
# #Just checking something...
# #olduniquestreets <- old[,'street'] %>% 
# olduniquestreets <- old[order(old$street),] %>% 
#   #select(street) %>% #this keeps on picking up propertyID, which I DON'T ASK FOR!!!
#   distinct(street)
#   #arrange(street)
# 
# #Oh good! Messy!
# #Actually, it might not be as bad as I thought.
# 
# #And what about addresses for the old no-regist single remainingzz?
# single_olduniquestreets <- old[order(old$street),] %>% 
#   filter(registType=="no regist, single address") %>% 
#   distinct(street)


#~~~~~~~~~~~~~~~~~
# OLD/NEW LINK CHECKS USING ROS-OLD REGIST-UPDATED----

old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2.rds")
new <- readRDS("JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne_DateWindowApplied_appyearmonadded.rds")

#Quick check: how many unique property IDs in the "no regist" category
#370K out of 2.6 million obs.
old %>% filter(registType=="no regist, single address") %>% 
  distinct(propertyID) %>% nrow()
  

new <- arrange(new, Title.number)
#So we still have 476K's worth of individual properties not linked to any others.
#Update: 440K. Not quite as bad!
#I've linked all the non-regists I can *within* old. Now to look at new -
#Which we'll have to do on address again. Which is likely to be trickier.

#Another thought on old:
#I think no-regists contains repeat sales already. 
#This was obvious, dunno why I didn't think about it.

#If that's the case, the no-regists will have fewer unique propertyIDs than observations...
old_singlez <- filter(old, registType=="no regist, single address")

#In fact, I can see some. So yes - more repeat sales here.
#But we could do with seeing how they link to new

old_NOTsinglez <- old_NOTsinglez %>% 
  filter(registType!="no regist, single address") 

#Why doesn't this work?
#old_NOTsinglez <- arrange(old_NOTsinglez, street)
old_NOTsinglez <- old_NOTsinglez[order(old_NOTsinglez$street),]

#Anyway, was just checking - the singlez looked like a lot on the same streets. It's not.

#Might also be worth seeing if there's a date pattern in the differing reg types
#Oh yes, look at that! Worth knowing.
ggplot(old, aes(x=selldate_formatted, fill=as.factor(registType))) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat = "bin", colour="black", binwidth=365)


#Hum. Well the most I'm likely to be able to do here: where I can, match against street and number.
#How do the codings for flat position differ?
oldflats <- select(old,flatpos) %>% filter(flatpos!="")
newflats <- select(new,Sub.building) %>% filter(Sub.building!="")

#For slightly mentally fiddly logical reasons, it's possible to exclude any 
#news that have an existing title match from old
#And just see about old/new address matches on the remainder.
#See "Linking old non-regists and new" in windfarms planner doc
#Err - is it that fiddly? Might be worth explaining again:

#I've *already* done address-matching within old
#If there's already a found title-number link between old/new
#it means there wouldn't be any point trying to match against a "new" address
# - I've already shown it can't be matched against that address in "old".
#See? No, me neither.

#Long and short of it: remove any title numbers from new
#That we previously found a match for
#Look at match addresses only on the remainder

#For which we will need the old/new match file.
mrg <- readRDS("JessieExtDrive/OldNewCombined/OldNewMergedTidied_on_appdate_n_correctedRegist_finalPrice.rds")

#unique title numbers from the merged
#We also already have a "new only" label, don't we?
#Only 245539 of these - not going to be getting many matches out of 500K old!
mrg_uniqtitlz <- mrg %>% 
  select(registUpdated,oldneworboth) %>% 
  filter(oldneworboth=="newonly") %>% 
  distinct()
  
#And we don't want to look at these in new!
#We also only need a single new title (address *should* be the same per title...)
new_uniqtitlz <- new %>% 
  distinct(Title.number)

#So now: keep only new titles/addresses where there was no match in old
#new_keep <- new_uniqtitlz[!(mrg_uniqtitlz$registUpdated %in% new_uniqtitlz$)]

#Well that crashed!
#Just merge in then keep "new only"s
new_keep <- left_join(new_uniqtitlz,mrg_uniqtitlz,by = c("Title.number"="registUpdated"))

#Well, that seemed to work. Now just keep "new only"
new_keep <- filter(new_keep, oldneworboth=="newonly")

#Might save that...
saveRDS(new_keep, "C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/newonly_for_oldchecking.rds")
new_keep <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/newonly_for_oldchecking.rds")

#NOW! Are there any address matches?
#Some tricky things - note the difference in flat coding above
#We can only get so far with that. But a little flat processing might do the job

#If it's not for the flats, how do things look on street number, name and town?
#Sooo. Looking for matching addresses - best start by looking at addresses.
#I just added postcode to this without running the rest of the code below... 
new_addresses <- new_keep[,c(1,10,26:31,33,20)]
# old_addresses_noRegist <- old[,c(2:9,22)] %>% filter(registType=="no regist, single address")

#One title number per ob
nrow(distinct(new_addresses, Title.number))


#Oh for god's sake - why does select always keep propertyID?
#Well...

#Unique old addresses
old_addresses_noRegist <- old %>% 
  #old[,c(2:9,22)] 
  select(propty:street,town,pcode,registType,selldate_formatted) %>% 
  filter(registType=="no regist, single address") %>% 
  distinct(propertyID)

#How many just from this?
inBoth <- inner_join(new_addresses,old_addresses_noRegist,
                     by=c("Property.number" = "strno",
                          "Thoroughfare" = "street",
                          "Post.town" = "town"))

#uh-oh: only ~6K. Is that because of differing strings?
#Update: 42K unique now, after address processing. Better! 
#More to do though: I think Post.town / town is not an ideal link. That needs looking at.

#Compare town/locality types
new_townlocality <- new_keep %>%
  select(Post.town:Locality) %>% 
  gather(type,place,Post.town:Locality) %>% 
  filter(place!="")

old_town <- select(old, place = town)[,'place']#because ID gets added in regardless...
old_town$type <- "old"

alltowns <- rbind(new_townlocality, old_town) %>% 
  distinct(place) %>% 
  arrange(place)

#How many are there of each type?
#http://rpackages.ianhowson.com/cran/dplyr/man/tally.html
alltowns %>% group_by(type) %>% tally()

# 1 Post.town  1240
# 2  Locality  3208
# 3       old  1805

#So oddly inconsistent: probably from the submitted forms
#So depends on who filled the form in

#How many of the towns repeat? Oh, I kept only the uniques, try again!
townSummary <- rbind(new_townlocality, old_town) %>% 
  group_by(place) %>% summarise(type = max(as.character(type)), count = n()) %>% 
  arrange(desc(count))

#Check an alphabetical too - how many appearing in each?
townSummary <- townSummary %>% arrange(place)
townSummary <- townSummary %>% arrange(desc(count))

#so old has no aberdeen for towns at all?
#Just loaded RoS_old to check original...
#Yup, zero!
RoS_old[RoS_old$town=="Aberdeen",] %>% nrow()

#What's the biggest numbers we have just for the old towns then?
oldTownSummary <- townSummary %>% filter(type=="old") %>% 
  arrange(desc(count))

#Then: thinking about using postcodes as place indicator
#How many have postcodes? What do they look like? Can we just use the first part?

#New: just to make it fun, the subjects in brief appear to contain more postcodes
#Than the postcode field. Which may or may not be down to my poor matching.
#Ah, no: there IS a match: no postcode in 'subjects', none in its own field.
#Makes that easier! 

#Look-see
#How many blank?
#43K without, 1 million with.
table(0 + (new$Postcode != ""))

newpc <- new %>% select(Postcode) %>% 
  filter(Postcode != "") %>% 
  arrange()

#Handily, these are also fixed character-length
#So the first four chars are always the first part (perhaps with space, which we can remove)

#now, a look old ros-old pcodes:
#how many blank?
table(0 + (old$pcode != ""))
#26K without, 2.25 million with.

#Looks like?
oldpc <- old[,c('pcode')] %>% 
  filter(pcode != "") %>% 
  arrange()

#Not fixed-width but always a space if there are two parts
#56K with only first part
table(0 + (grepl(" ",oldpc$pcode)))

#But we only need the first part so that's OK!


#~~~~~~~~~~~~~~~
#Let's add that to each of the new/old datasets - into unique addresses first to check match numbers
#Update: add to the new/old unique addresses to check numbers
new_addresses$pcodepart <- substr(new_addresses$Postcode, 1, 4) %>% 
  gsub(" ", "", .)

#OK
table((nchar(new_addresses$pcodepart)==3) + (nchar(new_addresses$pcodepart)==4))

#Now old:
old_addresses_noRegist$pcodepart <- word(old_addresses_noRegist$pcode,1)

#OK! 
table((nchar(old_addresses_noRegist$pcodepart)==3) + (nchar(old_addresses_noRegist$pcodepart)==4))
# new$pcodepart <- substr(new$Postcode, 1, 4) %>% 
#   gsub(" ", "", .)
# 
# #OK
# table((nchar(new$pcodepart)==3) + (nchar(new$pcodepart)==4))
# 
# #Now old:
# old$pcodepart <- word(old$pcode,1)
# #OK! 
# table((nchar(old$pcodepart)==3) + (nchar(old$pcodepart)==4))

#~~~~~~~~~
#Now see how many matches we have using this.
inBoth_w_pcodes <- inner_join(new_addresses,old_addresses_noRegist,
                     by=c("Property.number" = "strno",
                          "Thoroughfare" = "street",
                          "pcodepart"))

#Just short of 48K. So actually we're getting close to the total number we can get.
#The next thing to do:
#Check if town link gets different ones to postcode link.
#Then add new-ros title number back into old.

#Err. What have I actually got with those new/old addresses?
#Not unique title numbers it would appear.
inBoth <- arrange(inBoth, Title.number)

#how many unique titles in both of those linked? It's not many, I don't think
#22K out of 42K
nrow(distinct(inBoth, Title.number))
#26K out of 47K
nrow(distinct(inBoth_w_pcodes, Title.number))

#Let's just see how many are the same title number on both those matches
#Before sorting this title number problem
both_titles <- inBoth[,c('Title.number','selldate_formatted')] %>% 
  distinct(Title.number)

both_w_pcode_titles <- inBoth_w_pcodes[,c('Title.number','selldate_formatted')] %>% 
  distinct(Title.number)

#So! Out of that 22k/26K unique titles, 20K are common to both link methods.
huh <- inner_join(both_titles,both_w_pcode_titles,by="Title.number")

#example. So subjects doesn't tell us which flat anyway
inBoth %>% filter(Title.number == "ABN41792") %>% select(Subjects...FULL)

#Just to clarify:
#Linking old/new by address, this is happening a lot:
#Flatpos from old tells us these a different properties (separate flats) 
#but they're not distinguished as such in new. There's a textual description but... 
#no, just checked, there's no extra info in there.

#So how many uniques are duplicates? :)
#Or, actually, how many are single-children?
#~15.5K, which is the max address match I think we can manage
oon <- subset(inBoth, 
       !duplicated(inBoth[,"Title.number"])
       &!duplicated(inBoth[,"Title.number"],fromLast = T))

#Check that worked... Yes, there's only one of these
#(i.e. with the "subjects within the land edged red" types)
inBoth %>% filter(Title.number == "ABN64738") %>% select(Subjects...FULL)

#19.5K in the postcode matches.
oon2 <- subset(inBoth_w_pcodes, 
              !duplicated(inBoth_w_pcodes[,"Title.number"])
              &!duplicated(inBoth_w_pcodes[,"Title.number"],fromLast = T))

#14K in common. So we should get a little over 20K in total linked.
huh <- inner_join(oon,oon2,by="Title.number")

#OK - keep ALL unique title numbers from both
#For then adding to matching propertyIDs in old
#Include fully populated fields from each to check on the overlap
oon$oon <- 1
oon2$oon2 <- 1

#20646 in total
allTitles <- full_join(oon[,c("Title.number","propertyID","oon")],
                       oon2[,c("Title.number","propertyID","oon2")],
                       by = c("Title.number","propertyID"))

#allTitles$oonz <- 0 + allTitles$oon + allTitles$oon2
#group_by(allTitles$oonz) %>% tally()

table(allTitles$oon, useNA = "always")
table(allTitles$oon2, useNA = "always")
#14384 overlap
table((allTitles$oon == 1 & allTitles$oon2 == 1), useNA = 'always')
#allTitles$cnt <- (0 + (allTitles$oon == 1 & allTitles$oon2 == 1))

#Back here again. Summat went wrong - too many propertyIDs.
#Did it go wrong here?
#Yup!
length(unique(allTitles$propertyID))

#Or...? Oh, here too. Right.
length(unique(oon$propertyID))
length(unique(oon2$propertyID))

#Quick look at new and old addresses
oldJustStreets <- old_addresses_noRegist[,c('street')] %>% 
  #select(street) %>% #ID! Don't want!
  filter(street!="") %>% 
  distinct(street) %>% 
  arrange(street)

newJustStreets <- new_addresses %>% 
  select(street = Thoroughfare) %>% 
  filter(street!="") %>% 
  distinct(street) %>% 
  arrange(street)

oldJustStreets$type <- "old"
newJustStreets$type <- "new"

all <- rbind(oldJustStreets,newJustStreets) %>% 
  arrange(street)

#Maybe look again keeping full addresses
allz <- merge(new_addresses,old_addresses_noRegist,
              by.x=c("Property.number","Thoroughfare"), by.y = c("strno","street"), all=T)

allz <- allz %>% distinct(Property.number,Thoroughfare) %>% 
  arrange(Property.number,Thoroughfare)

#Look at ones where both towns have *something* in that field
allz2 <- allz %>% 
  filter((!is.na(Locality)|!is.na(Post.town)) & (!is.na(town) ) )

#Mess of post-towns there. More work could be done but not now, I don't think!

#save the merge file so I can clear everything out
saveRDS(allTitles, "C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/oldnewAddress_mergelink.rds")
allTitles <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/oldnewAddress_mergelink.rds")

#CLEAR! *THWONK*

#~~~~~~~~~~~~~~~~~~
# Merge new titles from old address match back into old----
allTitles <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/oldnewAddress_mergelink.rds")
old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2.rds")

#Duplicate property IDs snuck in. Don't really care from where right now - 
#Let's just get unique pairs and move on!
#They'll be ones with matches in new so it should be fine. Probably.
allTitles <- allTitles %>% distinct(propertyID)

#It should only be replacing (some of "no regist, single address")
#So let's just check that before merge
chk <- inner_join(allTitles %>% select(Title.number, propertyID),
                  old %>% select(newRegist,propertyID,registType),
                  by='propertyID')

#Yup, just the one type. All good.
unique(chk$registType)
#And yup, all NA. So can write over.
table(as.character(chk$newRegist),useNA = 'always')

#http://stackoverflow.com/questions/16042380/merge-data-frames-and-overwrite-values
#Nice: transmute is "keep only new columns" version of mutate
#Except actually here, we might want mutate then drop the other column
#Need to look at intermediate stages though
old2 <- left_join(old, allTitles %>% select(Title.number,propertyID),by='propertyID')

old2$newNewRegist <- ifelse(is.na(as.character(old2$newRegist)) ,old2$Title.number, as.character(old2$newRegist))

#That worked. So replace newRegist
old2$newRegist <- NULL
old2$Title.number <- NULL
names(old2)[names(old2)=='newNewRegist'] <- 'newRegist'

#Check in both old/old2 that we have indeed added some extra regists...
#Yup!
table(0 + (is.na(old$newRegist)), useNA = 'always')
table(0 + (is.na(old2$newRegist)), useNA = 'always')


#save with one 'old' job left: attach pretend title number to remaining olds without.
#That can then be linked as repeat sales.
#(Checking that there are no date duplicates)
saveRDS(old2, "JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2_mergedNewAddressTitles.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DEAL WITH THE LAST NO-REGISTS 
old2 <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2_mergedNewAddressTitles.rds")

#So how many left?
#416776. Big fat chunk, huh?
table(0 + (is.na(old2$newRegist)), useNA = 'always')

#How many unique duplicates in those? i.e. how many should be repeat sales just by address?
old2noRegists <- old2 %>% filter(is.na(newRegist))
old2noRegists <- old2noRegists %>% arrange(propertyID)

#Unique duplicates: 52754 properties appear more than once
ooneeqdups <- subset(old2noRegists, duplicated(old2noRegists[,"propertyID"])
  |duplicated(old2noRegists[,"propertyID"],fromLast = T)) %>% 
  distinct(propertyID)

#Check for any dups in each propertyID group using unique propertyID/date pairs
#Well, this was obviously never going to work!
old2$checkDups <- ifelse(
    (duplicated(old2[,c('propertyID','selldate_formatted')])
     |duplicated(old2[,c('propertyID','selldate_formatted')], fromLast=T)),0,1)

#Well this doesn't work!
# old2 <- old2 %>% group_by(propertyID) %>% 
#   mutate(checkDups2 = ifelse
#            (duplicated(.(propertyID,selldate_formatted))
#             |duplicated(duplicated(.(propertyID,selldate_formatted), fromLast=T)),0,1)
#            )

#Uh oh: 12136 ID/date dups. Let's look...
table(old2$checkDups)

dups <- old2 %>% filter(checkDups == 0)

#Many of those look like re-sales after marriage or divorce.
#Those have the same price.

#So ones with the same price / propertyID / date:
#We only want one each of those. 

#here's the filtering that needs doing:
#Remove any where there are propertyID-date duplicates but their price differs. Because who knows what they are? (And there aren't many.)
#Keep only one of each propertyID/date/price duplicates.
#Last first!
#2330 less
old3 <- old2 %>% distinct(price,propertyID,selldate_formatted)

#Now keep only one of each ID/date pair. Names don't matter for me
#though might need to come back here for other stuff
#2565669 in previous step, minus 2561885 = 3784 less
old3 <- old3 %>% distinct(propertyID,selldate_formatted)

#checking again
markedNoRegist <- old3 %>% filter(registType=="no regist, single address")
#Ah good - we still have some added in from previously! Just wanted to check.
table(0 + (is.na(markedNoRegist$newRegist)), useNA='always')

#I should probably update the regist type there so we know it came from new
old3$registType2 <- old3$registType

old3$registType2[old3$registType=="no regist, single address" 
                 & !is.na(old3$newRegist)] <- "regist from new matching address"

old3$registType2[old3$registType=="regist from matching address"] <- "regist from old matching address"

#Right ee ho
table(old3$registType2)

#AND! For remainder, make a new "title" that indicates it didn't have one in old
#Repeat sales can then be bundled the same as everything else.
#Random three letter indicator: ZZZ plus propertyID
old3$newRegist2 <- old3$newRegist
old3$newRegist2[is.na(old3$newRegist)] <- paste0("ZZZ",as.character(old3$propertyID[is.na(old3$newRegist)]))

#OK, looking good. Let's rename those mofos.
old3$registType <- NULL
old3$newRegist <- NULL
old3$checkDups <- NULL

names(old3)[names(old3)=='newRegist2'] <- 'newRegist'
names(old3)[names(old3)=='registType2'] <- 'registType'

#Let's just graph that too
ggplot(old3, aes(x=selldate_formatted, fill=as.factor(registType))) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat = "bin", colour="black", binwidth=365)

#Right ee ho again
table(old3$registType)

#SAVE!
saveRDS(old3, "JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2_mergedNewAddressTitles_finalRegistSet.rds")
old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registUpdated2_mergedNewAddressTitles_finalRegistSet.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#RE-DOING SOME OF THE GEOCODING WORK-----
#Cos previously I've only applied geocodes to repeats. 
#Might as well apply to all properties

#I already have a regist-geocodes lookup for old-RoS, just re-done and got a lot more:
old_geoc_title <- readRDS("JessieExtDrive/OldNewCombined/old_ros_geocodeTitleMap.rds")
new <- readRDS("JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne_DateWindowApplied_appyearmonadded.rds")

#Now to repeat a little of the code above for getting a similar lookup
#from the available geocodes in new
#Hum - I did a LOT of work on those geocodes above! Did it need to be that complex?
#Some of the checks are necessary, but...

#We just need to work with unique title/geocode pairs. 
#Can then merge back in once combined with old's title numbers.
#new_uniqueTitlesGeocodes <- distinct(new, )

#First thing to check: how many of the min/max are different?
table(0 + ((new$Minimum.Easting - new$Maximum.Easting)!=0))
table(0 + ((new$Minimum.Northing - new$Maximum.Northing)!=0))
# 0         1 
# 740961    833

#Let's just look at those: going to be multiple addresses probably
spr <- new[((new$Minimum.Northing - new$Maximum.Northing)!=0)&!is.na(new$Maximum.Northing),]

#Yup, multiple addresses. Drop em.
#This includes NAs, which we want!
new2 <- new[((new$Minimum.Northing - new$Maximum.Northing)==0)|is.na(new$Maximum.Northing),]

#Yup, right number. Drop previous
new <- NULL



#Set any new-geocode empty cells to NA (there are a few)
#It's only in 'Eastings'/'Northings'
table(0 + (new2$Eastings==""))
table(0 + (new2$Northings==""))

new2$Eastings[new2$Eastings==""] <- NA
new2$Northings[new2$Northings==""] <- NA

#So, remaining min/maxs are all the same and we can just keep one.
#We can then also keep the geocodes from address.
#Then see what remains.
#Let's just count how many in both / how many overlap
table(0 + is.na(new2$Minimum.Easting))
#740961 mix/max
table(0 + is.na(new2$Eastings))
#526042
table(0 + (is.na(new2$Eastings) & is.na(new2$Minimum.Easting)))
#753782 coded. Well that's not very many!
#How many of these were from new?
table(0 + (is.na(new2$Application_yearmon)))
#763076
#Oh OK, it's about right then!
#And... how many are geocoded at all from all?
table(0 + (is.na(new2$Eastings) & is.na(new2$Minimum.Easting) & is.na(new2$oldRoS_eastings)))
# 0      1 
# 902442 660451
#Though note, this is before we've applied new geocoded titles to any old with the same title.

#So first: just make a new geocode column that contains what we have from both
new2$newRoS_eastings <- new2$Minimum.Easting
new2$newRoS_eastings[is.na(new2$Minimum.Easting)] <- new2$Eastings[is.na(new2$Minimum.Easting)]
new2$newRoS_northings <- new2$Minimum.Northing
new2$newRoS_northings[is.na(new2$Minimum.Northing)] <- new2$Northings[is.na(new2$Minimum.Northing)]
#So that should now equal... Yup! the is.na sums for both above
table(0 + is.na(new2$newRoS_eastings))

#Divide ones that are there by 10
#To convert to metres (currently they're 10cm though most only have 0 in last digit
new2$newRoS_eastings <- as.numeric(new2$newRoS_eastings)/10
new2$newRoS_northings <- as.numeric(new2$newRoS_northings)/10


#Checking again on the uniqueness of title/geocode-ness
unique_newTitleGeocodes <- distinct(new2, Title.number, newRoS_eastings, newRoS_northings)

#778881 out of 806631
unique_newTitleGeocodes %>% 
  distinct(Title.number) %>% 
  nrow()

#The conclusion above was: no way to know which is right
#So lose all titles with multiple geocodes
#Then attempt to geocode them later

#So we don't want to drop the records - we just want those geocode fields left blank.
#Sooo. 
#1. Keep only single-child title/geocode pairs from unique
#2. Create as two-column, with one as flag
#3. Merge into new2
#4. Over-write geocodes without flag
singlz <- subset(unique_newTitleGeocodes,
                 (!duplicated(unique_newTitleGeocodes$Title.number)
                  &!duplicated(unique_newTitleGeocodes$Title.number, fromLast = T)))

#751572 of those
#Actually... this is our list of new-geocodes, isn't it?
#I need to combine with the list from old
#And then apply to the old/new combo
#So keep only the relevant columns
new_geoc_map <- singlz %>% 
  select(Title.number,newRoS_eastings,newRoS_northings)

#save!
saveRDS(new_geoc_map,"JessieExtDrive/OldNewCombined/new_ros_geocodeTitleMap.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# APPLY OLD/NEW GEOCODES TO OLD/NEW MERGE----

#1. Reload both sets of unique title/geocode pairs
#2. Combine 'em ready for merge
oldg <- readRDS("JessieExtDrive/OldNewCombined/old_ros_geocodeTitleMap.rds")
newg <- readRDS("JessieExtDrive/OldNewCombined/new_ros_geocodeTitleMap.rds")

#Load old/new obs
mrg <- readRDS("JessieExtDrive/OldNewCombined/UPDATED_OldNewMergedTidied_on_appdate_n_correctedRegist_finalPrice.rds")

#Combining old/new geocode map
#We're going to find repeats - as well as, probably, multiple geocodes per title for some
names(oldg)
names(newg)

oldg <- oldg %>% select(eastings = oldRoS_eastings,northings = oldRoS_northings, Title = newRegist)
newg <- newg %>% select(eastings = newRoS_eastings,northings = newRoS_northings, Title = Title.number)

allg <- rbind(oldg,newg)

#duplicateness?
#1,111,216 unique title numbers
allg %>% distinct(Title) %>% nrow()

#1,151,363 unique title/geocode pairs
allg %>% distinct(Title,eastings,northings) %>% nrow()

#And of course less if we stick only to single-children...
#963318. OK then.
# singlz <- subset(allg,
#                  !duplicated(allg[,c("Title","eastings","northings")])
#                   &!duplicated(allg[,c("Title","eastings","northings")], fromLast = T))
#Oh, it's even less - 883024. Uh huh.
singlz <- subset(allg,
                 !duplicated(allg[,c("Title")])
                  &!duplicated(allg[,c("Title")], fromLast = T))


#Again - I don't think we can keep ANY of the geocodes
#Where there's ambiguity on location
#Which is a shame, as we'll be losing a lot

#Err - how many unique titles in old/new merge?
#1,601,710. A fair few huh?
mrg %>% select(registUpdated) %>% distinct(registUpdated) %>% nrow()

#So that should mean something like 638K still need geocoding
#So drop existing geocode fields
#and replace with lookup from singlz
mrg <- mrg %>% select(-(Minimum.Easting:Maximum.Northing),-(Eastings:Northings))

#The correct number now we've got single-child titles from the lookup
mrg2 <- merge(mrg, singlz, by.x="registUpdated", by.y = "Title", all.x = T)

#SAVE! 
saveRDS(mrg2, "JessieExtDrive/OldNewCombined/GeocodesFromRoS_UPDATED_OldNewMergedTidied_on_appdate_n_correctedRegist_finalPrice.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GEOCODING FROM OTHER SOURCES

#Soooo: this has what geocodes we could get from RoS files
#Only keeping those where title had a definite, unique geocode
mrg <- readRDS("JessieExtDrive/OldNewCombined/GeocodesFromRoS_UPDATED_OldNewMergedTidied_on_appdate_n_correctedRegist_finalPrice.rds")

#Remind me how many unique addresses that is?
#1.6 million. That's most of em!
mrg %>% distinct(registUpdated) %>% nrow()

#How many between them have postcodes now?
#If both are NA, there's no postcode at all, so...
#4461 without anything in that field. Not bad.
#Oh: 44618! Still.
numb <- mrg[(is.na(mrg$Postcode)|mrg$Postcode=="") & (is.na(mrg$pcode)|mrg$pcode==""), ] %>% nrow()
#How many of those without postcodes are not geocoded?
#26623. Not so bad.
mrg[(is.na(mrg$Postcode)|mrg$Postcode=="") 
    & (is.na(mrg$pcode)|mrg$pcode=="") 
    & is.na(mrg$northings), ] %>% nrow()

pcodes <- mrg %>% select(pcode, Postcode)

#So let's make a shared postcode field from those.
#Reminder of format:
#pcode from old: single space between postcode parts
#Postcode from new: fixed width, so first part is always 4 chars, sometimes space at end
#Note, last part ("inward code") is always three chars.
#So might as well just get rid of all spaces?
#And choose one to default to if there are two
mrg$postcode_fromBoth <- mrg$pcode
mrg$postcode_fromBoth[mrg$pcode=="" | is.na(mrg$pcode)] <- as.character(mrg$Postcode[mrg$pcode=="" | is.na(mrg$pcode)])
#Make consistent, get rid of space
mrg$postcode_fromBoth <- gsub(" ", "", mrg$postcode_fromBoth)

#Trying something else: let's get postcode first part *now* from both rather than processing below
#from old first - first word, so avoiding any wrongly entered second words
mrg$outward_from_both <- word(mrg$pcode,1)

#Then just first four chars from new
mrg$outward_from_both[is.na(mrg$pcode)] <- substr(mrg$Postcode[is.na(mrg$pcode)],1,4)

#remove any spaces picked up from new
mrg$outward_from_both <- gsub(" ", "", mrg$outward_from_both)

#Picks up a small number from old that didn't have spaces in (~600)
#Assume these *do* have the correct number of last digits and remove the last three
mrg$outward_from_both[nchar(mrg$outward_from_both) > 4] <- gsub('.{3}$',
                                                                '', mrg$postcode_fromBoth)[nchar(mrg$outward_from_both) > 4]

#So...? One. Fine!!
mrg %>% filter(nchar(outward_from_both) > 4) %>% nrow()

#Now should be same missing count... err, nearly!
mrg %>% filter(outward_from_both=="") %>% nrow()

#Addressbase street data
#On loading here, many of the address fields are in one position to the right.
#Oh well, at least it's there!
ab <- read.csv("C:/Data/AddressBase/data/unzipped/ID28_DPA_Records.csv")

abTowns <- ab %>% 
  select(POST_TOWN) %>% 
  distinct(POST_TOWN) %>% 
  arrange(POST_TOWN)

#What's this in postcode? Not the postcode?

#do addressbase post-towns cover all of new-RoS'? I suspect not...
noo <- data.frame(unique(mrg$Post.town))

#Addressbase BLPU to merge in geocodes
blpu <- read.csv("C:/Data/AddressBase/data/unzipped/ID21_BLPU_Records.csv")

#Need only link and geocodes
blpu <- blpu %>% select(UPRN,X_COORDINATE,Y_COORDINATE)

#Merge those into the addresses
ab2 <- left_join(ab,blpu,by = "UPRN")

#Did that populate all address geocodes?
#Yup, got everything
table(0 + (is.na(ab2$Y_COORDINATE)|ab2$Y_COORDINATE==""))

#Save for later
saveRDS(ab2,"C:/Data/AddressBase/data/unzipped/ID28_DPA_Records_withGeocodes.csv")
ab2 <- readRDS("C:/Data/AddressBase/data/unzipped/ID28_DPA_Records_withGeocodes.csv")

#Let's see what we can match using postcode/street
#For which I'm going to need a street to try.
#Or could try both in turn? Might have more luck.

#Just testing what has punctuation, what doesn't
#Header wrong, so...!
smpl <- data.frame(ab2[grepl("minister",ab2$DOUBLE_DEPENDENT_LOCALITY,ignore.case = T),'DOUBLE_DEPENDENT_LOCALITY'])
#Yup, like others, that contains apostrophes.

smpl2 <- data.frame(mrg[grepl("minister",mrg$street,ignore.case = T),'street'])
smpl3 <- data.frame(mrg[grepl("minister",mrg$Thoroughfare,ignore.case = T),'Thoroughfare'])

#Aaaand one of them doesn't contain apostrophes!
#So need to strip out punctuation from all
mrg$old_street <- gsub("[[:punct:]]", "", mrg$street) 
mrg$new_street <- gsub("[[:punct:]]", "", mrg$Thoroughfare) 

#Yup, that works.
smpl2 <- data.frame(mrg[grepl("minister",mrg$old_street,ignore.case = T),'old_street'])
smpl3 <- data.frame(mrg[grepl("minister",mrg$new_street,ignore.case = T),'new_street'])

#Same for addressbase
ab2$street_no_punc <- gsub("[[:punct:]]", "", ab2$DOUBLE_DEPENDENT_LOCALITY) 

#Tick.
smpl <- data.frame(ab2[grepl("minister",ab2$street_no_punc,ignore.case = T),'street_no_punc'])

#addressbase postcode: we're only going to match on first part again.
ab2$outward <- word(ab2$POSTCODE_TYPE,1)

#UPDATE: did the first-part postcode above now, don't need this...
#But let's run again, since I just got LESS MATCHES!! GRR.

#same for the housing data: in this case, drop last three characters
#This will lose part postcodes... err. 
#How many of those are there?
#100K. Hmm. They need filtering differently.
mrg %>% filter(nchar(postcode_fromBoth) < 5) %>% nrow()

#Let's look at them
shortpcs <- mrg %>% filter(nchar(postcode_fromBoth) < 5) %>% 
  select(postcode_fromBoth)

#Oh well, that's easy. Just don't process those ones.
#Looks good.
mrg$outward <- mrg$postcode_fromBoth
mrg$outward[nchar(mrg$postcode_fromBoth) > 4] <- gsub('.{3}$',
                                                      '', mrg$postcode_fromBoth)[nchar(mrg$postcode_fromBoth) > 4]

#So how do "outward" and "outward from both differ?"
#Why did I get less matches from the latter?
pcs <- mrg %>% select(outward,outward_from_both) %>% 
  filter(outward!=outward_from_both)

#Ah right: mostly postcodes with no space, which must be in old, right?
#607. Not a big number!
mrg %>% filter(!grepl(" ",pcode) & nchar(pcode) > 4) %>% nrow()


#So the theory: address geocodes should be unique on number, street and outward code
#Hmm. Not quite!
ab2 %>% distinct(outward,DOUBLE_DEPENDENT_LOCALITY,DEPENDENT_THOROUGHFARE,X_COORDINATE) %>% 
  nrow()

#let's just check on using building name and street name combined
#(They're mutually exclusive fields)
#Constant reminder: fields in ab2 in wrong order!
ab2$number_or_name <- as.character(ab2$DEPENDENT_THOROUGHFARE)
ab2$number_or_name[is.na(ab2$DEPENDENT_THOROUGHFARE)] <- as.character(
  ab2$BUILDING_NUMBER[is.na(ab2$DEPENDENT_THOROUGHFARE)])

#check uniqueness again
#Better. Excellent: 2,375,008 out of 2.61 mill
ab2 %>% distinct(outward,DOUBLE_DEPENDENT_LOCALITY,number_or_name,X_COORDINATE) %>% 
  nrow()


#For uncertain geocodes - again - I should really only work with those that have certainty
#So only single-child ones based on the fields I'm merging on
#2.3 million...
#Wrong number - those should have been unique(ish) - oops, need to go back, remove "X_COORDINATE" from singlez
ab_singlz <- subset(ab2,
                    !duplicated(ab2[,c('outward','street_no_punc','number_or_name')])
                    &!duplicated(ab2[,c('outward','street_no_punc','number_or_name')], 
                                 fromLast = T))
# ab_singlz <- subset(ab2,
#                     !duplicated(ab2[,c('outward','DOUBLE_DEPENDENT_LOCALITY','number_or_name','X_COORDINATE')])
#                     &!duplicated(ab2[,c('outward','DOUBLE_DEPENDENT_LOCALITY','number_or_name','X_COORDINATE')], 
#                                  fromLast = T))

mrg$geocode_source <- NA
mrg$geocode_source[!is.na(mrg$northings)] <- "ROS"

#Actually, I do need just one street and number field in mrg don't I?
#it'll be a palaver otherwise
mrg$street_fromBoth <- mrg$old_street
mrg$street_fromBoth[is.na(mrg$street)] <- mrg$new_street[is.na(mrg$street)]

#RoS_old doesn't have anything like building name, not in the same way
#So get what we can from both for street position
mrg$streetNumberName_fromboth <- ifelse(mrg$strno==""|is.na(mrg$strno),
                                         ifelse(mrg$Building.name==""|is.na(mrg$Building.name),
                                                mrg$Property.number,as.character(mrg$Building.name)),mrg$strno)

#Save both of those before attempted merges
#saveRDS(mrg, "JessieExtDrive/Misc_RoS_R_Saves/mrgfile_before_addressBaseGeocoding.rds")
saveRDS(mrg, "JessieExtDrive/Misc_RoS_R_Saves/mrgfile_before_addressBaseGeocoding_postcodeChange.rds")
saveRDS(ab_singlz, "JessieExtDrive/Misc_RoS_R_Saves/addressBase_single_geocodes.rds")

mrg <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/mrgfile_before_addressBaseGeocoding.rds")
ab_singlz <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/addressBase_single_geocodes.rds")


#Everything in place for an attempted merge, right?
#Ob number shouldn't increase
mrg2 <- merge(mrg,ab_singlz[,c('outward','street_no_punc','number_or_name','X_COORDINATE','Y_COORDINATE')],
              by.x=c('outward_from_both','street_fromBoth','streetNumberName_fromboth'),
              by.y=c('outward','street_no_punc','number_or_name'),
              all.x = T)
              
#OK: so how many geocodes from AB?
#2.03 million out of 2.2 million. Alarmingly good.
mrg2 %>% filter(!is.na(X_COORDINATE)) %>% nrow()

#How many now geocoded in total?
#(Bearing in mind there are going to be some differences...)
#2,475,721, leaving ~400K not geocoded
#Pushed up to 2,475,824 with a little more pcode fiddling. So that made a huge difference.
mrg2 %>% filter(!is.na(X_COORDINATE)|!is.na(eastings)) %>% nrow()

#How many unique addresses is that?
#Bear in mind address matching in titles might not be perfect, but rough ballpark
uniquez <- mrg2 %>% distinct(registUpdated)
#1.33 million out of 1.6 million unique addresses geocoded
#271007 addresses still not given location
uniquez %>% filter(!is.na(X_COORDINATE)|!is.na(eastings)) %>% nrow()

#Let's just look at those not geocoded
notg <- uniquez %>% filter(is.na(X_COORDINATE)&is.na(eastings))

#Well, various messy things going on here!
#1. A lot of the non-matching postcodes *don't have 3 digits for their inward code*
#I think mainly for old-postcodes
#Which was an assumption to get that postcode.
#So I'm going to need a slightly more step-wise postcode filtering.

#Let's just check that
#This doesn't work for some reason... oh, there isn't always a second word
#notg %>% filter(!is.na(pcode) & nchar(word(pcode,2))==2) %>% nrow()
notg %>% filter(!is.na(pcode) & nchar(word(pcode,-1))==2) %>% nrow()
pcs <- notg %>% filter(!is.na(pcode))

pcs$ct <- word(pcs$pcode,-1) %>% nchar()
table(pcs$ct)
#0        1      2      3      4        5      7      8 
#20066    651    736 228627  15355      2    178      1 
#So really not that many accounted for by postcode mistakes then

#Also... 26K without postcodes, so no match there, obv!
mrg2 %>% filter(is.na(postcode_fromBoth)) %>% nrow()

#Err.
ab2 %>% filter(outward=="FK1") %>% nrow()

#Some: the actual street is in the second part of the street field, not the first

#Save so far
saveRDS(mrg2, "JessieExtDrive/Misc_RoS_R_Saves/oldnew_addressBaseGeocodeMerge1.rds")

#huh?? It looks like RoS and AB are still not of the same magnitude.
#Did I not re-apply the /10? That would only be for ros-new anyway...
write.csv(mrg2 %>% 
            distinct(registUpdated) %>% 
            select(registUpdated,eastings,northings) %>% 
            filter(!is.na(eastings)),
          "C:/Data/temp/RoS_geocodeCheck.csv")

#Yeah, ROS're all consistently 10x too big. Phew, easy to fix.

#And next: how many titles do we have that only have a single geocode?
#So def one address?
#Let's use SD method...
#Oh, we need one geocode column first.
mrg2$finalEastings <- ifelse(is.na(mrg2$X_COORDINATE),mrg2$eastings/10,mrg2$X_COORDINATE)
mrg2$finalNorthings <- ifelse(is.na(mrg2$Y_COORDINATE),mrg2$northings/10,mrg2$Y_COORDINATE)
mrg2 %>% filter(is.na(finalEastings)) %>% nrow()

#check again
#Just need to check something about the geocodes...
#Yup, all good - no strays at all.
write.csv(mrg2 %>% 
            distinct(registUpdated) %>% 
            select(registUpdated,finalEastings,finalNorthings) %>% 
            filter(!is.na(finalEastings)),
          "C:/Data/temp/RoS_geocodeCheck2.csv")


#Keep only those geocoded... 
#oh, except I wonder if we're going to have some geocodes that haven't been applied to all regists?
#Perhaps we shouldn't keep those anyway at this point?
mrg_geocs <- mrg2 %>% filter(!is.na(finalEastings))

mrg_geocs <- mrg_geocs %>% group_by(registUpdated) %>% 
  mutate(sd_eastings = sd(finalEastings))

mrg_geocs <- mrg_geocs %>% group_by(registUpdated) %>% 
  mutate(sd_northings = sd(finalNorthings))

#Set any NAs to zero (that's single-sale places)
mrg_geocs$sd_eastings[is.na(mrg_geocs$sd_eastings)] <- 0
mrg_geocs$sd_northings[is.na(mrg_geocs$sd_northings)] <- 0
#  0       1 
#2387625   88199 
table(0 + (mrg_geocs$sd_eastings!=0 | mrg_geocs$sd_northings!=0))

#Some of those might be because from different sources so slightly diff numbers?
#There seem to be few small differences. this is in metres, so.
#Ah, this is much closer now I've corrected RoS
#Probably worth working with a little.
#20K down from 88K (for eastings, at least. Need to check northings too.)
#Very very few of these are going to be beyond 10 metres out
table(0 + (mrg_geocs$sd_eastings > 5 | mrg_geocs$sd_northings > 5))

#For those that are different but not too much, just use an average
#We want the group whose geocode SD is NOT zero but is less than 5
geo_avs <- mrg_geocs %>% filter((sd_eastings!=0 | sd_northings!=0)
                            & (sd_eastings < 5 & sd_northings < 5))

geo_avs <- geo_avs %>% arrange(registUpdated)

#now we can just get an av'd summary of northings and eastings per title
#to merge back in
avs_summary <- geo_avs %>% select(registUpdated,finalEastings,finalNorthings) %>% 
  group_by(registUpdated) %>% 
  summarise(eastingsAV = mean(finalEastings),northingsAv = mean(finalNorthings))

#Then I need a list of ones we're *excluding* because the SD is too high
#Just need the unique regists as we don't need to carry out any summary
#And all per-group SDs are the same
excludes <- mrg_geocs %>% 
  distinct(registUpdated) %>% 
  filter(sd_eastings >= 5 | sd_northings >= 5) %>% 
  select(registUpdated,finalEastings,finalNorthings)

#We'll need a flag field.
#Initially, let's mark these two - geocode from SD < 5 av; SD > 5

#Update names for ones to exclude and wipe values to avoid confusion
#(note annoying cameltype error, oh well!)
excludes <- excludes %>% 
  select(registUpdated,eastingsAV=finalEastings,northingsAv=finalNorthings)

excludes$eastingsAV = NA
excludes$northingsAv = NA

#And flag columns
excludes$geocode_processing_note <- "Excluded: per-title-group geocodes SD > 5m"
avs_summary$geocode_processing_note <- "per-title mean: applied to 0 < SD < 5m per title group"

#And link those two before merging back
sd_link <- rbind(avs_summary,excludes)

#Merge this back in on title
mrg3 <- left_join(mrg2, sd_link, by = "registUpdated")

#Some name-changing
names(mrg3)[names(mrg3)=="finalEastings"] <- "eastings_FromRoS_andAddressBase"
names(mrg3)[names(mrg3)=="finalNorthings"] <- "northings_FromRoS_andAddressBase"

mrg3$eastingsFinal <- mrg3$eastings_FromRoS_andAddressBase
mrg3$northingsFinal <- mrg3$northings_FromRoS_andAddressBase

#Add in avs
mrg3$eastingsFinal[!is.na(mrg3$eastingsAV)] <- mrg3$eastingsAV[!is.na(mrg3$eastingsAV)]
mrg3$northingsFinal[!is.na(mrg3$northingsAv)] <- mrg3$northingsAv[!is.na(mrg3$northingsAv)]

#But exclude excluded!
mrg3$eastingsFinal[mrg3$geocode_processing_note=="Excluded: per-title-group geocodes SD > 5m"] <- NA
mrg3$northingsFinal[mrg3$geocode_processing_note=="Excluded: per-title-group geocodes SD > 5m"] <- NA

#mrg3$geocode_source[is.na(mrg3$geocode_source)&!is.na(mrg3$eastings_FromRoS_andAddressBase)] <- "AddressBase"
#Actually, it defaults to addressbase, doesn't it?
mrg3$geocode_source[!is.na(mrg3$X_COORDINATE)] <- "AddressBase"
mrg3 <- mrg3 %>% arrange(registUpdated)

#Save before it breaks!
saveRDS(mrg3, "JessieExtDrive/Misc_RoS_R_Saves/oldnew_addressBaseGeocodeMerge3.rds")
mrg3 <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/oldnew_addressBaseGeocodeMerge3.rds")

#So that's everything in its raw form. Just choose some columns then save.
#Then I can look at repeat sales. How many of those are there...?
#1.26 million apparently. Hmm. I suspect a lot are going to be in the old
rpts <- subset(mrg3, duplicated(mrg3[,c('registUpdated')]))

#Oh - how many are geocoded? 1.13 million.
#So from lack of geocoding, we lose 129K. Harrumph!
rpts %>% filter(!is.na(eastingsFinal)) %>% nrow()

#Let's graph those differences. 
#Err. I can't remember what I was going to graph.
#Oh - I wanted to see if there's a geocode-success bias towards particular time periods

#Err. This is probbaly just showing an accumulation of repeat sales, isn't it?
ggplot(mrg3, aes(x=date, fill=as.factor(0 + (!is.na(eastingsFinal))))) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat = "bin", colour="black", binwidth=365)

table(0 + (!is.na(rpts$eastingsFinal)),rpts$oldneworboth)
#Where zero = "not geocoded"
#Which is not hugely informative...
#    both newonly oldonly
#0  26694    9215   93405
#1 368191  115161  649684

#~ 7% for new vs 10.5% for old not geocoded. More but not insanely different
#((26694+9215)/(368191+115161))*100
#((26694+93405)/(368191+649684))*100
((26694+9215)/(368191+115161+26694+9215))*100
((26694+93405)/(368191+649684+26694+93405))*100

#OK - cut down columns to something sensible for saving
mrg4 <- mrg3 %>% select(-c(outward:northingsAv))
mrg4 <- mrg4 %>% select(-c(eastings,northings))
mrg4 <- mrg4 %>% select(-c(propertyID))
mrg4 <- mrg4 %>% select(-c(old_street,new_street))

names(mrg4)[names(mrg4)=="outward_from_both"] <- "outwardPostcode_RoS_AB_merge"
names(mrg4)[names(mrg4)=="street_fromBoth"] <- "street_RoS_AB_merge"
names(mrg4)[names(mrg4)=="streetNumberName_fromboth"] <- "streetNo_or_BuildingName_RoS_AB_merge"
names(mrg4)[names(mrg4)=="streetNumberName_fromboth"] <- "streetNo_or_BuildingName_RoS_AB_merge"
names(mrg4)[names(mrg4)=="registUpdated"] <- "Title"
names(mrg4)[names(mrg4)=="yearmon2"] <- "yearmon"

#save both versions, one as "raw", other as "tidied" (a bit)
saveRDS(mrg3,"JessieExtDrive/Misc_RoS_R_Saves/RAW_oldnew_addressBaseGeocodeMerge.rds")
saveRDS(mrg4,"JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge.rds")

#Save as CSVs
write.csv(mrg3,"JessieExtDrive/Misc_RoS_R_Saves/RAW_oldnew_addressBaseGeocodeMerge.csv")
write.csv(mrg4,"JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge.csv")

#~~~~~~~
#So a little looking at this.
#I was worried that repeat sales might have got stuck in old/new
#And little link was made across the two. 
#There was plenty of title-old/new linkage, so perhaps not. But.

#But what exactly do I want to look at?
#ZZZ titles: we *know* these are just in old and there was no link to new.
table(mrg4$registType, useNA = 'always')

#Oh, boo. Probably just making the same graph as before is all that matters. Let's move on to...


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PROCESSING LINK FILE TO JUST REPEAT SALES----

#Reload!
mrg <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge.rds")

#I should probably check if there are any...
#Oh yeah, there are, right there at the start.
#So! I need to (again) apply any geocodes back to any titles missing the values in title groups
#That is: in a group of titles, some/most have geocodes, one or more with the same title do not

#Should be relatively easy, right? Just max per title, merge back in?
maxs <- mrg %>% select(Title,eastingsFinal,northingsFinal) %>% 
  group_by(Title) %>% summarise(maxEastings = max(eastingsFinal,na.rm=T),maxNorthings=max(northingsFinal,na.rm=T))

mrg2 <- left_join(mrg,maxs,by="Title")

#408410 empty
table(0 + (is.na(mrg2$eastingsFinal)))
#390583 empty - better!
table(0 + (is.na(mrg2$maxEastings)))

#Keep only the new columns
mrg2 <- mrg2 %>% select(-c(eastingsFinal,northingsFinal))
mrg2 <- mrg2 %>% rename(eastingsFinal = maxEastings,northingsFinal = maxNorthings)

#Save again!
saveRDS(mrg2,"JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge2.rds")
mrg2 <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge2.rds")
#Save as CSV
write.csv(mrg2,"JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge2.csv")

#Aaand where were we? Wanting to graph where repeat sales were, old/new/both
#Keep only repeat sales
rpts <- subset(mrg2,duplicated(mrg2[,'Title']))

#Hang on, I haven't even removed non-geocodes yet, have I?
rpts <- rpts %>% filter(!is.na(eastingsFinal))

#Which is how many unique properties?
#652,795. Woo hoo...? Is that less than we had in the previous version? Yes, appears to be. Oh good!
length(unique(rpts$Title))

#Well. That's enormously depressing. I'm just going to look at their version. I am curious.
#They're not identified as unique properties anywhere here...
length(unique(oldgeo$id))

#Let's try this instead from the duke's folder
oldgeo <- read.dta13("C:/Users/SMI2/Desktop/ROSCleanData_Matched/RoS19902010_not_geocoded_match.dta")

#yeah, think that's it: rosGrp looks like the property grouping
rptold <- subset(oldgeo,duplicated(oldgeo[,'rosGrp']))
#Good, that's not a different number!
rptold_dups <- subset(oldgeo,duplicated(oldgeo[,'rosGrp'])|duplicated(oldgeo[,'rosGrp'],fromLast = T))

rptold_dups <- rptold_dups %>% arrange(rosGrp)

#So only 611148. Huh. OK, maaybe feeling better now?
#And I'm also very very unsure how they could consider some of those to be matches, given what they have
#address-wise
length(unique(rptold$rosGrp))

#~~~~~~~~~~~~~~~
# OK! Less depressing! Looks like those numbers aren't quite accurate. I am ahead, of this at least.

#so what do the repeat sales look like...
#Old/new/or both is not very interesting.
#Let's try checking on min/max dates in each group

#Oops - wasn't looking at the full set of repeats, try again...
rpts_dups <- subset(mrg2,duplicated(mrg2[,'Title'])|duplicated(mrg2[,'Title'], fromLast = T))

rpts_dups <- rpts_dups %>% group_by(Title) %>% 
  mutate(maxDate = max(date))

#No reason in keeping more than the one property...
#Oh yeah, there is, cos they'll come from either or both
ggplot(rpts_dups, aes(x=maxDate, fill=oldneworboth)) + 
  # geom_density(alpha = 0.2)
  geom_area(alpha = 0.3, stat = "bin", colour="black", binwidth=365)

# Oh right: so the other graph I want:
# •	If repeat sales are all "old only", they're "old-exclusive repeats"
# •	If they're a mix, they're "old/new mixed repeats"
# •	If they're exclusively new-only, they're "new-exclusive repeats"
# So coding that is fiddly. Probably have to use factors again.
#In fact, with factors, it's probably easy, innit?

rpts_dups$oldnewbothFactor = as.numeric(as.factor(rpts_dups$oldneworboth))

#Now, if group SD is zero, can use mean for factor code
#If it's not zero, we know it's a mix anyway

rpts_dups <- rpts_dups %>% group_by(Title) %>% 
  mutate(oldnewbothFactorSD = sd(oldnewbothFactor), oldnewbothFactor_mean = mean(oldnewbothFactor))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UNIQUE STREET NAMES??-----

# just thought of something else really obvious I should have done:
# I've been assuming street names were not a unique identifier. But in a lot of cases, they probably are. For those that are, you don't even need a town name for matching against.
# And it's relatively easy to check what the numbers on that are...

#So just checking on RoS_old:
#1. Get all unique street/town combos
#Actually, I could do with looking at the processed street/town names...
RoS_old <- readRDS("JessieExtDrive/STATA_analysis/RoS/RoS19902010_dateFormatted_registFromNew_streetNamesLongd.rds")

#keep only first town name again
names(RoS_old)[names(RoS_old)=="town"] <- "oldtown"
RoS_old$town <- gsub(',.*','', RoS_old$oldtown)

ooneq <- RoS_old %>% distinct(street,town) %>% 
  filter(street!="") %>% 
  arrange(street) %>% 
  select(street,town)

#So now we have distinct street/town pairs -
#any repeated street is not unique in the dataset

#count duplicates and count singles
ooneq %>% subset(duplicated(street)) %>% nrow() 
ooneq %>% subset(!duplicated(street)) %>% nrow()
#which is total. Good!
ooneq %>% subset(duplicated(street)) %>% nrow() +
  ooneq %>% subset(!duplicated(street)) %>% nrow()

#So: pretty much precisely two-thirds are unique street names in the entire dataset.
#For old at least - we'd have to get unique street name sets from each dataset.

















