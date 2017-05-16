#
# STEP 1: subsetting the mor2 data:
#
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
# define / rename variables to simplify later:

# this is the data on just the 130 HH That have paired ecological data,
# along with their associated Organizational info
mor2<- read.spss("./data/ALL_EcolHHOrgMerged_byUvuljaa_03_15_17_Simplified.sav" , 
                 to.data.frame=TRUE,
                 use.value.labels=FALSE) # should this be true?
# 
# tidy format
tbl_df(mor2)

###################
# Ecological Zone #
# 1 = "DS", # desert steppeRef nos.
ez <- select(mor2, ez = EcologicalZone_4Code, RefNum = SocialSurveyReferenceNumber) 
 
ez$ez <- ordered(ez$ez)



#################
##  PRACTICES : #
#
# herd.size
# p1 <- "Total2010SFU" # [605]
# distance travelled
# p2 <- "AvgDist2010"  # [125]
# p3<- "TotDist2010" [126]
# p4 <- "d_FallOtor" # [132]
# p5 <- "e_WtrOtor"  # [134]
# reserving pastures
# p6 <- "a_ResWint"  # [129:131]
# p7 <- "b_ResSpr" #
# p8 <- "c_ResDzud"
# out of season use:
# p9 <- "f_GrzWtr_SumFall"  # [136]
# p10 <- "h_GrzDzud_NonEmrg" # [138]
p.cols <- c(605, 125,126,129:132, 134, 136, 138)
practices<- mor2[,p.cols]
names(practices)<- c("p1","p2","p3", "p4", "p5","p6", "p7", "p8", "p9", "p10")
# 
practices$p8[practices$p8<0]=NA   # change -99 to NA
practices$p5[practices$p5<0]=NA

as.factor(practices$p4)
as.factor(practices$p5)
as.factor(practices$p6)
as.factor(practices$p7)
as.factor(practices$p8)
as.factor(practices$p9)
as.factor(practices$p10)
#############################
##  ECOLOGICAL INDICATORS:  #
# rename columns to 
# e1 <-  "PerGrassCover_percent" # [1528]
# e2 <-  "PerForbCover_percent"  # [1529]
# e3 <-  "BareSoil_percent"      # [1536]
# e4 <-  "LitterCover_percent"   # [1538]

e.cols <- c(1528, 1529, 1536, 1538)
ecol.ind<- mor2[, e.cols]

# rename columns
names(ecol.ind) <- c("e1","e2", "e3", "e4")



##############
##  RULES  ###
# Questions re: Are there Rules regarding:
# r1 <- "timing_Gz"   # Answers are No/Informal/Formal, 0/1/2
# r2 <- "num_LSK"     # same
# RPE = Rule/Practices/Ecology
r.cols <- c(826, 828)   # specifying the columns in the mor2 df
rules<- mor2[, r.cols]
names(rules) <- c("r1", "r2")  # timing, sfu
factor(rules$r1)
factor(rules$r2)
#####################
#  final dataframe  #
#
rpe <- cbind(ez, rules, practices,  ecol.ind)
#


#####################
##   NORMALIZE    ###
# practices
rpe$pl <- as.numeric(log(rpe$p1+1))
rpe$p3s<- as.numeric(log(rpe$p3+1))
rpe$p2s <- as.numeric(log(rpe$p2+1))


# see standardizing.R for normalizing standardized vars


