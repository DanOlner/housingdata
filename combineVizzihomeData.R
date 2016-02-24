#combine vizzihome data into one sheet.
#Vizzihome is already Scots-only.
library(plyr)
library(pryr)
# library(gdata)#needs Perl
# library(xlsx)#Newp!
library(readxl)
library(readstata13)

#The Vizzihome folder has five excel sheets and two datasheets.
#Answer 15 here, a non-dependency xlsx reader...
#http://stackoverflow.com/questions/6099243/read-an-excel-file-directly-from-a-r-script/6099497#6099497

#excel sheets first
files <- Sys.glob("Vizzihome/*.xlsx")

#No idea why but that's attaching an extra zip ref. Remove
files <- files[1:5]

inList <- lapply(files, read_excel)

xlsxcombined <- ldply(inList,data.frame)

#Let's just save that before doing dtas
saveRDS(xlsxcombined,"Vizzihome/temp_combinedxlsx.rds")

#Load dtas
files <- Sys.glob("Vizzihome/*.dta")

dtas <- lapply(files, read.dta13)
dtascombined <- ldply(dtas,data.frame)

#Just checking those two dtas are the right number of obs
#...Yup!
# dta1 <- read.dta13(files[1])
# dta2 <- read.dta13(files[2])

#oh good - different columns! What have we got?
colnames(dtascombined)
colnames(xlsxcombined)

#date column in the excel sheets not in the dta
#temp save of combined dta, though think I might need to add date in there.
saveRDS(dtascombined,"Vizzihome/temp_combined_dtas.rds")

####################
# Post-RDS save. 

#Load back in. Still working out what's happening with the dates
#Note: Judita talks about some 'with date' data...
#A part of the Vizzihome data does not contain any kind of date; I am
#assuming that the entries are ordered in order of increasing time
#(supported by the 'with date' data).
dtascombined <- readRDS("Vizzihome/temp_combined_dtas.rds")
xlsxcombined <- readRDS("Vizzihome/temp_combinedxlsx.rds")

#Not going to worry about the date for now. Just want to check
#ID matching in Judita's alignment file

#let's keep those separate for now so I can see if Judita only matched against the dated ones

#clear memory
rm(list = ls(all = TRUE))
