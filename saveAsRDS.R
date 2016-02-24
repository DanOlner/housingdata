#Save RDS versions from the various files

#For loading stata sheets
#https://github.com/sjewo/readstata13
library(readstata13)

#Let's make RDS copies of the various files so I can easily re-load them for comparison.
#Already done for Zoopla data.

#Registers of Scotland
#Old
#JessieExtDrive\Data\RoS\LPD2 = new RoS data ('03 to March 2014)
#JessieExtDrive\STATA_analysis\RoS  = old
#A few Stata sheets. Jessie says this is the main one, but there are others there: RoS19902010.dta

RoS_old <- read.dta13("JessieExtDrive/STATA_analysis/RoS/RoS19902010.dta")
#Save as RDS
saveRDS(RoS_old, "RDS_Versions/RoS19902010.rds")
rm(RoS_old)

#New RoS data is in CSVs, in matching groups of four files: master, address, applicant and granter.
#Let's look at an example to figure out what's what
master <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2014-03-31_master_new.csv")
address <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2014-03-31_address_dat.csv")
applicant <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2014-03-31_applicant_dat.csv")
granter <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2014-03-31_granter_dat.csv")

#Different batch...
master <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2014-03-31_master_new.csv")
address <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2014-03-31_address_dat.csv")
applicant <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2014-03-31_applicant_dat.csv")
granter <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2014-03-31_granter_dat.csv")

colnames(master)
colnames(address)
colnames(applicant)
colnames(granter)

#are there the same number of IDs? (Before checking if they actually match...)
length(unique(applicant$Application.number))
length(unique(granter$Application.number))
length(unique(master$Application.number))
length(unique(address$Application.number))

length(unique(applicant$Title.number))
length(unique(granter$Title.number))

length(applicant$Title.number)
length(granter$Title.number)

length(unique(master$Application.number))
length(unique(master$Title.number))













