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
# [27] "EcologicalZone_4Name"       [28] "EcologicalZone_4Code"
# [1339] "Ecologicalzone_4Code100"
ez <- mor2[, c(6:8,1339)] # this pulls the Ecol Zone and the Survey Ref nos.
names(ez[,3:4])<- c("RefNum", "ez")
ez.codes<- c(1:4)
zone.names<- c("DS", # desert steppe
               "ST", # steppe
               "ES", # eastern steppe
               "FMS") # forest/mtn steppe
ezones<- cbind(ez.codes, zone.names)  # for future reference

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

#############################
##  ECOLOGICAL INDICATORS:  #
# rename columns to 
# e1 <-  "PerGrassCover_percent" # [1528]
# e2 <-  "PerForbCover_percent"  # [1529]
# e3 <-  "BareSoil_percent"      # [1536]
# e4 <-  "LitterCover_percent"   # [1538]

e.cols <- c(1528, 1529, 1536, 1538)
ecol.ind<- mor2[, e.cols]
prac.ecol<- cbind(practices, ecol.ind)
# rename columns
names(prac.ecol) <- c("p1","p2","p3", "p4", "p5","p6", "p7", "p8", "p9", "p10", "e1","e2", "e3", "e4")
# 
prac.ecol$p8[prac.ecol$p8<0]=NA   # change -99 to NA
prac.ecol$p5[prac.ecol$p5<0]=NA
# 1. specify factors
# 2. switch levels direction:  <<<NO! DON'T DO THIS!>>>
# prac.ecol$p4<- as.factor(prac.ecol$p4, levels = c(1,0))  
# prac.ecol$p5<- as.factor(prac.ecol$p5, levels = c(1,0))
# prac.ecol$p6<- as.factor(prac.ecol$p6, levels = c(1,0))
# prac.ecol$p7<- as.factor(prac.ecol$p7, levels = c(1,0))
# prac.ecol$p8<- as.factor(prac.ecol$p8, levels = c(1,0))
# prac.ecol$p9<- as.factor(prac.ecol$p9, levels = c(1,0))
# prac.ecol$p10<- as.factor(prac.ecol$p10, levels = c(1,0))
# SCALE THE DATA for p1:p3??  # see note below on cfa 3 re: log of p1 (sfu)

##############
##  RULES  ###
# Questions re: Are there Rules regarding:
# r1 <- "timing_Gz"   # Answers are No/Informal/Formal, 0/1/2
# r2 <- "num_LSK"     # same
# RPE = Rule/Practices/Ecology
r.cols <- c(826, 828)   # specifying the columns in the mor2 df
rules<- mor2[, r.cols]
names(rules) <- c("r1", "r2")  # timing, sfu

#####################
#  final dataframe  #
#
rpe <- cbind(ez, rules, prac.ecol)
#
#rpe <- rpe %>% select(-p1)  
# pl or p1

#rpe$r1<- ordered(rpe$r1, levels = c(0, 1, 2))
#rpe$r2<- ordered(rpe$r2, levels = c(0, 1, 2))

rpe$r1<- factor(rpe$r1)
rpe$r2<- factor(rpe$r2)

#####################
##   NORMALIZE    ###
# practices
rpe$pl <- as.numeric(log(rpe$p1+1))
rpe$p3s<- as.numeric(log(rpe$p3+1))
rpe$p2s <- as.numeric(log(rpe$p2+1))
# ecol indicators
rpe$e1s <- sqrt(rpe$e1)
rpe$e2s <- sqrt(rpe$e2)
# don't know what to do w e3 bc it's kind of bimodal
# and e4 is also a hot mess...

# prob should reorder the vars in rpe eventually too.

rpe <- rename(rpe, RefNum = SocialSurveyReferenceNumber)
rpe <-rename(rpe, ez = Ecologicalzone_4Code100)
rpe$ez <- factor(rpe$ez)
# see standardizing.R for normalizing standardized vars



# if needed : 
##############################
##   SUBSET BY ECOLOG ZONE  ##
# !!!!!!!!!!! these might be out of date.......!!!!!!!!!!!!
# DS <- slice(rpe, 89: 121) # desert steppe
# ST <- slice(rpe, 1:39) # typical steppe
# ES <- slice(rpe, 122:130) # eastern steppe   -- n = 9
# FMS <- slice(rpe, 40:88) # forest/mtn steppe


#####################
#
# REORDERING THE R2 and Practices Vars...
