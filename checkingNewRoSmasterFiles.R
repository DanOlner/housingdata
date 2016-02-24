#just checking colnames for the different new-RoS master files.
#Import each type, take a look.
old <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2003-04-30_master_dat.csv")
new <- read.csv("JessieExtDrive/Data/RoS/LPD2/COMPLETE.2010-07-31_master_new.csv")

colnames(old)
colnames(new)
