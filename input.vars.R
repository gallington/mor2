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

# lat long for jay:
mor2loc <- dplyr::select(mor2, 
                  lat = Latitude500,
                  long = Longitude500,
                  RefNum = SocialSurveyReferenceNumber,
                  AimagName,
                  SoumName)
mor2loc$AimagName<- trimws(mor2loc$AimagName)
mor2loc$SoumName<- trimws(mor2loc$SoumName)
#write.table(mor2loc, file= "./data/mor2locations.csv")
 


###################
# Ecological Zone #
# 1 = "DS", # desert steppeRef nos.
ez <- select(mor2, ez = EcologicalZone_4Code, 
             RefNum = SocialSurveyReferenceNumber, 
             CBRM= CBRM, 
             CBRM_type = CBRM_type,
             PlotID = UniquePlotID500)  #this one incorps the Uvuulja and site
#rpe%<>%mutate_at(4:6, funs(ordered(.))) 
#ez$ez <- ordered(ez$ez, ez$CBRM_type)



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


#############################
##  ECOLOGICAL INDICATORS:  #
# rename columns to 
# e1 <-  "PerGrassCover_percent" # [1528]
# e2 <-  "PerForbCover_percent"  # [1529]
# e3 <-  "BareSoil_percent"      # [1536]
# e4 <-  "LitterCover_percent"   # [1538]
# sr <- "SpeciesRichnessMean500_1000
TotalSedgeCover_percent_Mean500_1000
TotalShrubCover_percent_Mean500_1000
AnGrassCover_percent_Mean500_1000
Acnath_gm2_Mean500_1000
BasalCover_percent

e.cols <- c(1528, 1529, 1536, 1538)
ecol.ind<- mor2[, e.cols]

# rename columns
names(ecol.ind) <- c("e1","e2", "e3", "e4", "sr")



##############
##  RULES  ###
# Questions re: Are there Rules regarding:
# r1 <- "timing_Gz"   # Answers are No/Informal/Formal, 0/1/2
# r2 <- "num_LSK"     # same
# RPE = Rule/Practices/Ecology
r.cols <- c(826, 828)   # specifying the columns in the mor2 df
rules<- mor2[, r.cols]
names(rules) <- c("r1", "r2")  # timing, sfu
#factor(rules$r1)
#factor(rules$r2)

################################
# all cols pulled together
#r.cols <- c(826, 828)
#p.cols <- c(605, 125,126,129:132, 134, 136, 138)
#e.cols <- c(1528, 1529, 1536, 1538)

#####################
#  final dataframe  #
#
rpe <- cbind(ez, rules, practices,  ecol.ind)
rpe%<>%mutate_at(c(1,3:4,11:17), funs(factor(.)))  
rpe%<>%mutate_at(6:7, funs(ordered(.)))
#




#####################
##   NORMALIZE    ###
# practices
rpe$pl <- as.numeric(log(rpe$p1+1))
rpe$p3s<- as.numeric(log(rpe$p3+1))
rpe$p2s <- as.numeric(log(rpe$p2+1))
#transformed but not standardized to ecol.zone
rpe$grass<- as.numeric(log(rpe$e1+1))
rpe$forb<- as.numeric(log(rpe$e2+1))
rpe$bare.inv<- as.numeric(sqrt(100-rpe$e3))
rpe$litter<- as.numeric(log(rpe$e4+1))

# see standardizing.R for normalizing standardized vars



##########################
#  TENURE --- org level
# tenure rights:

# USE: 
hhrts <- mor2 %>%
  select(A_UsWtrCamp,
         A_UsWtrPast,
         A_UsSepSprCS,
         A_UsSepSprPas,
         A_SepDzud,
         A_HayCutFld,
         RefNum = SocialSurveyReferenceNumber)%>%
  rename(Wcmp = A_UsWtrCamp,
         Wpast = A_UsWtrPast,
         Sprcmp = A_UsSepSprCS,
         SprPast= A_UsSepSprPas,
         DzPast= A_SepDzud,
         HayFld = A_HayCutFld)
hhrts%<>%mutate_at(1:6, funs(factor(.)))
# contracts
hhcont<- mor2 %>%
  select(B_ContractWtrCamp,
         B_ContractWtrPast,
         B_ContractSprCamp,
         B_ContractSprPast,
         B_ContractDzud,
         B_ContractHayCut,
         RefNum = SocialSurveyReferenceNumber)%>%
  rename(ContractWtrCamp = B_ContractWtrCamp,
         ContractWtrPast = B_ContractWtrPast,
         ContractSprCamp = B_ContractSprCamp,
         ContractSprPast = B_ContractSprPast,
         ContractDzud = B_ContractDzud,
         ContractHayCut = B_ContractHayCut)
hhcont%<>%mutate_at(1:6, funs(ordered(.)))

# trespassing 
trsp   
olu<- mor2 %>% dplyr::select(otherusers = as.factor(mor2$q24_OtherLandUsers),  RefNum = SocialSurveyReferenceNumber)   # from org level survey
# this does something weird and pulls in other data from the meta or annot if you call it as factor 
# oail<-mor2 %>% dplyr::select(anotherail = as.factor(mor2$AnotherAilLSOnPast),  RefNum = SocialSurveyReferenceNumber)  # from HH survey
oail<-mor2 %>% dplyr::select(anotherail = AnotherAilLSOnPast,  RefNum = SocialSurveyReferenceNumber)  # from HH survey

rpetr<- cbind(rpe.new, olu, oail, hhcont)  # add to rpe.new

# getting errors bc of "empty categories"
contord<- c("ContractWtrCamp","ContractWtrPast","ContractSprCamp","ContractSprPast", "ContractDzud", "ContractHayCut")
#tenure->pract
tp.mod<- 'tenure=~ 1*ContractWtrCamp+ContractWtrPast+ContractSprCamp+ContractSprPast+ ContractDzud+ContractHayCut
          practice =~ p4 + p5 + p6+ p7 + p9 + p10'

tp.fit<- sem(tp.mod, data= rpetr, ordered = contord)


######
# 
