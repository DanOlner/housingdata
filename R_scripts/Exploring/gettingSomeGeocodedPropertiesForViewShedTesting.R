#Just to get a rough picture of the unique properties with geocodes
#so I can see how many / where they're likely to be
#For working out timings of the viewshed algorithm

library(plyr)
library(pryr)
library(zoo)

mdsi_plus_address <- readRDS("JessieExtDrive/Data/RoS/RoSNew_combinedTypes/mdsi_plus_address.rds")

#Keep only those with eastings/northings
#Some are NA, I think, others just blank
#372636 are NA. Blech.
nrow(mdsi_plus_address[is.na(mdsi_plus_address$Eastings),])

geocoded <- mdsi_plus_address[!is.na(mdsi_plus_address$Eastings),]

#And some are empty
geocoded <- geocoded[geocoded$Eastings!="",]

#Then unique title numbers. How many?
#600,000. Enough to be going on with
# nrow(data.frame(unique(geocoded$Title.number)))
length(unique(geocoded$Title.number))

#597891 obs
uniqueTitles <- geocoded[!duplicated(geocoded$Title.number),]

#geocode might be two digits too precise. Let's check... Yup!
uniqueTitles$Eastings <- as.numeric(uniqueTitles$Eastings)/100
uniqueTitles$Northings <- as.numeric(uniqueTitles$Northings)/100

uniqueTitles <- uniqueTitles[order(uniqueTitles$Title.number),]

#Save as CSV for sticking into QGIS. Don't need much data for test...
write.csv(uniqueTitles[,c(1,38,39)], 
          "C:/Data/WindFarmViewShed/Tests/PythonTests/testData/newRoSuniqueTitles_w_geocode.csv")
