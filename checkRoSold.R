#just looking at various old RoS files to sanity-check the regist field
library(readstata13)

RoS_oldcheckz <- read.dta13("JessieExtDrive/STATA_analysis/RoS/dataset_RoS_version1_inclseller.dta")

#again, missing regists. About the same number.
length(RoS_oldcheckz$regist[RoS_oldcheckz$regist==""])

