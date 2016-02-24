#Check vizzihome and zoopla IDs in Judita's alignment file
library(plyr)
library(dplyr)
library(pryr)#for memory testing

#Looking at these two separately for now so I can see if Judita only matched against the dated ones
#(The original dtas for 2013/14 have no date field. Rest of fields match.)
dtascombined <- readRDS("Vizzihome/temp_combined_dtas.rds")
xlsxcombined <- readRDS("Vizzihome/temp_combinedxlsx.rds")

#Note: most of the vizzihome obs' status is 'for sale'. I may want to check just against 'sold'
#Judita -> "Vizzihome: only Sold STC[M]? and SSTC entries are being merged"

#The alignment file
alignment <- read.csv("JuditasLinkingFiles/smi_alignment_v2.txt",sep=" ", header=F)

#http://r.789695.n4.nabble.com/How-to-get-a-proportion-of-a-Vector-Member-td2720060.html
#How does this apply the *100? Is the table an object? What?
#5% sold
round(table(xlsxcombined$Status)/length(xlsxcombined$Status),2)*100
#13% sold
round(table(dtascombined$Status)/length(dtascombined$Status),2)*100

#Which is a very odd, large difference.

#Which files did Judita use from Zoopla?
#Judita -> "I am aligning all MV "Exchanged" and "Sold/SSTC" entries, wherever that is possible." 
#(from email thread "All the Zoopla data")
zoopexch <- readRDS("MarketView/Exchanged_data/marketView_Exchanged_combined.rds")
zoopsold <- readRDS("MarketView/Sold_data/marketViewSold_combined.rds")

#Note those two appear to have massive overlap:
#"I've just been doing some duplicate checks on the "sold" and "exchanged (SSTC)" data
#which I've combined to look for unique sales. There are a lot of duplicate properties 
#(identified by Zoopla ID) and I think the vast majority are for a single sale - 
#there are only 24 unique cases where the Price field is different for the same ID. 
#The rest of the duplicate ID'd properties all share exactly the same price. 
#Some of them (~1%) have sale/SSTC dates over a year apart, 
#but I presume if those were re-sales, the price would be different...?"
#(from email thread "All the Zoopla data")

#So there's an issue there about what the date means.
#Lets just check against each in turn.

#Pretty much exactly 50/50 split of MV / vizzihome IDs in Judita's match file
(table(alignment$V1)/length(alignment$V1))*100

#83087 MV IDs in all, 79928 Vizzi ones
#But they're not all unique.
#But they wouldn't be - this is the long-format file.
table(alignment$V1)

#mv uniques: 66247. Vizzi uniques: 55617
align_mv_uniqueIDs <- data.frame(unique(alignment$V2[alignment$V1=="MarketView"]))
align_vizzi_uniqueIDs <- data.frame(unique(alignment$V2[alignment$V1=="Vizzihome"]))

#Just checking - are those the same number as in the wide format alignment file?
#Oh, it doesn't straghtforwardly load due to the long lines...
#alignment_wide <- read.csv("JuditasLinkingFiles/smi_alignment.txt",sep=" ", header=F)


#How many match Zoopla sold data?
#Checking against only unique IDs
uniqueZoopSoldIDs <- data.frame(unique(zoopsold$Zoopla.listing.ID))
uniqueZoopexchIDs <- data.frame(unique(zoopexch$Zoopla.listing.ID))

#66247, which is 100% of uniques.
length(align_mv_uniqueIDs[align_mv_uniqueIDs[,1] %in% uniqueZoopSoldIDs[,1],])
#66202 - 45 off 100%
length(align_mv_uniqueIDs[align_mv_uniqueIDs[,1] %in% uniqueZoopexchIDs[,1],])


#How many vizzihome matches?
#Get unique IDs
# unique_vizzi_xls <- data.frame(unique(xlsxcombined$Property.ID))

#Blimey, that's a suspiciously smaller number!
#5%?? What's going on there? There cannot be that many repeat sales.
# (length(unique_vizzi_xls[,1])/length(xlsxcombined$Property.ID))*100

# xlsxcombined_idorder <- xlsxcombined[order(xlsxcombined$Property.ID),]
#Oh - ignore that until I've stripped out only the sold properties. For sales appear very near each other repeatedly...

table(xlsxcombined$Status)
table(dtascombined$Status)

#Try again - get only sold properties first
#257523 property IDs
vizzixls_sold <- xlsxcombined[xlsxcombined$Status %in% c("Sold STC","Sold STCM","SSTC"),]
#335817
vizzidta_sold <- dtascombined[dtascombined$Status %in% c("Sold STC","Sold STCM"),]

#Get unique sold IDs...
unique_vizzixls_sold <- data.frame(unique(vizzixls_sold$Property.ID))
#Different ID label!
unique_vizzidta_sold <- data.frame(unique(vizzidta_sold$PropertyID))

#In xlsx, 29420, a bit over 10%. A pretty small number again. So what are we looking at...?
#(In dta, 39732)
vizzixls_sold_idorder <- vizzixls_sold[order(vizzixls_sold$Property.ID),]

#Right - a load of duplicates again. I have a feeling Judita may have said something about this...
#No, not that specifically.

#OK, let's check how many of the duplicate sales have the same sale price
#Hang on, I can just do a unique check, can't I?
#29764. So only 344 properties whose price changes.
nrow(unique(vizzixls_sold[c("Property.ID","Price")]))
#40268. 532 whose price changes.
nrow(unique(vizzidta_sold[c("PropertyID","Price")]))

#So that's 70032 out of 

#Let's mark those so we can compare to date difference
#sd == 0 for any who's price doesn't change
vizzixls_sold_idorder <- ddply(vizzixls_sold_idorder, .(Property.ID), mutate, soldDiff = sd(Price))

table(vizzixls_sold_idorder$soldDiff)

#Date!
vizzixls_sold_idorder$codedDates <- as.Date(vizzixls_sold_idorder$date,"%Y-%m-%d")

#gap between dates for each property ID
vizzixls_sold_idorder <- ddply(vizzixls_sold_idorder, .(Property.ID), mutate, 
                               dateDiff = as.numeric(max(codedDates)-min(codedDates)))

#So now: for all property IDs with more than zero price diff across them all
#Are those dates further apart or not?
#Doing this way round so NAs get flagged as 0
vizzixls_sold_idorder$zeroPriceDiffFlag <- 0
vizzixls_sold_idorder$zeroPriceDiffFlag[vizzixls_sold_idorder$soldDiff!=0] <- 1 

#find date difference for each property ID
#broken down by whether it had a price change
diffz <- aggregate(vizzixls_sold_idorder$dateDiff, 
                   by = list(vizzixls_sold_idorder$Property.ID,vizzixls_sold_idorder$zeroPriceDiffFlag),
                   max)

length(diffz$x[diffz$Group.2==0])
#29118
length(diffz$x[diffz$Group.2==1])
#302

summary(diffz$x[diffz$Group.2==0])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    14.0    28.0    61.6    63.0  1050.0 
summary(diffz$x[diffz$Group.2==1])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.0    91.0   161.0   226.9   336.0  1050.0 

#Bit suspicious of that shared max, but the basic point is there:
#Properties who have a price change generally have a much wider date range.
hist(diffz$x[diffz$Group.2==0])
hist(diffz$x[diffz$Group.2==1])

#######
# Now, ID match vizzihome from xlsx against the alignment file

#15602. Out of 55617? Hmm.
length(align_vizzi_uniqueIDs[align_vizzi_uniqueIDs[,1] %in% unique_vizzixls_sold[,1],])

#And how about from the dtas?
#30503
#46105. Still some short.
length(align_vizzi_uniqueIDs[align_vizzi_uniqueIDs[,1] %in% unique_vizzidta_sold[,1],])

#Is there anything about the remaining IDs that might be nixing the match?
unique_vizzidta_sold_copy <- unique_vizzidta_sold
colnames(unique_vizzidta_sold_copy)[1] <- colnames(unique_vizzixls_sold)[1]

all_vizzi_soldIDs <- rbind(unique_vizzixls_sold,unique_vizzidta_sold_copy)

#This should be 46105...
#Oh, except there are some duplicates between the two.
#46056 when combined
length(align_vizzi_uniqueIDs[align_vizzi_uniqueIDs[,1] %in% all_vizzi_soldIDs[,1],])

#Mark vizzi IDs that have match
align_vizzi_uniqueIDs_flagMatch <- align_vizzi_uniqueIDs
align_vizzi_uniqueIDs_flagMatch$flag <- 0
align_vizzi_uniqueIDs_flagMatch$flag[align_vizzi_uniqueIDs_flagMatch[,1] %in% all_vizzi_soldIDs[,1]] <- 1 

#9561 with no match
table(align_vizzi_uniqueIDs_flagMatch$flag)

match <- align_vizzi_uniqueIDs_flagMatch[align_vizzi_uniqueIDs_flagMatch$flag == 1,]
noMatch <- align_vizzi_uniqueIDs_flagMatch[align_vizzi_uniqueIDs_flagMatch$flag == 0,]

#OK, that's the best I can do. Note down the numbers, check with Gwilym - 
#I might not be working with the correct Vizzihome data.

#Let's just check how many unique zoopla IDs there are in Scotland
soldexgeo <- readRDS("C:/Data/SecureData/soldex_unique_latestDate_withlatlon_and_gor.rds")

#S9999999 is scotland.
#There are 178844. They unique?
table(soldexgeo$gor)

soldexgeo_scots <- soldexgeo[soldexgeo$gor=="S99999999",]

#178844 again. OK.
length(unique(soldexgeo_scots$Zoopla.listing.ID))




