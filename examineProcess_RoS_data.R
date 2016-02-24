library(readstata13)
library(plyr)
library(pryr)
library(zoo)
library(stringr)

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

#Having already loaded RoS_old...

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
old <- old[old$price!=1,]

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

#Now the ones that *can* be converting to int, can be
#new$priceCombined <- as.numeric(as.character(new$ConsiderationProcessed))

#Get price from Value and (processed) Consideration at same time
#Value usually has price if consideration has a text description
#And it's already numeric. Crazy!
#If Value has NA, look in (processed) consideration
new$priceCombined <- ifelse(is.na(new$Value), 
                            new$ConsiderationProcessed, 
                            new$Value)

#Get rid of Price pound sign
new$PriceProcessed <- gsub("£","",new$Price)

#And the remaining replaced from Price, where Price has a value
new$priceCombined <- ifelse(new$priceCombined=="", 
                            as.numeric(as.character(new$PriceProcessed)), 
                            new$priceCombined)

#Re-convert to numeric to weed out remaining text fields
new$priceCombined <- as.numeric(new$priceCombined)


head(new[,c('Price', 'PriceProcessed','Consideration','Value','ConsiderationProcessed','priceCombined')],50)

#That's all the prices we can get.
#How many NAs left over?
nrow(new[!is.na(new$priceCombined),])

#1.2million. 30K NA. Not bad but I'm a little suspicious we may have lost some in the whittling.
#Press on! Drop NAs, keep only pairs with prices.
newFinal <- new[!is.na(new$priceCombined),]

#Drop unnecessary price columns
# newFinal <- subset(newFinal, select=-c('Value','Consideration','Price','ConsiderationProcessed','PriceProcessed'))
newFinal <- subset(newFinal, select=-c(Value,Consideration,Price,ConsiderationProcessed,PriceProcessed))

#Stupid price huh?
nrow(newFinal[newFinal$priceCombined<100,])

#Don't think they're much use to us.
#Actually, various silly prices. Let's drop a reasonable range
newFinal <- newFinal[newFinal$priceCombined>100,]

#Let's save that and move on. Special folder plz!
saveRDS(newFinal,"JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne.rds")


#~~~~~~~~~
#And what about the price field in old?
#zero
nrow(old[is.na(old$price),])
#zero
nrow(old[old$price=="",])
#180! Well, suppose we can leave them and see how things look on repeat sales
nrow(old[old$price<100,])



#~~~~~~~~~~~~~~~~~~~~~~~~~
#COMBINING OLD AND NEW----

#Possibly. Maybe. Let's see.
#new: priceCombined field has all prices
old <- readRDS("JessieExtDrive/Misc_RoS_R_Saves/RoS_old_uniqueTitleDatePairs_noregistdropped.rds")
new <- readRDS("JessieExtDrive/OldNewCombined/new_priceFieldsCombinedToOne.rds")











