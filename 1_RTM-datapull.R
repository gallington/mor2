# This is the first script to run for Rules & Tenure project analysis. 
# This script pulls in the db, queries and wrangles the data &
# saves as rdata to pull in to next script: 

#---------------2020/10/26: updated to correct rescaling of forage
# -----  but that really wacked out the odds ratios in the models.
#---------2021/04/01: going back to unscaled forage params bc they 
# ---------make more sense to interpret 
  

library(haven)  # this is the new package to use instead of (foreign)
#library(corrplot)
library(magrittr)
library(tidyr)
library(dplyr)
library(readxl) # to open excel files 
library(reshape2)
library(scales) # used to rescale() a few params

# THIS IS THE FULL DATASET WITH ALL 700 hh
# the org level data are merged in to this as well. 
mor2FULL<- read_sav("./data/Org_HHS_May2016_5_28_16.sav") #
#mor2FULL <- mor2 

# tidy format
tbl_df(mor2FULL)

# Info on fixing the names in Jay's forage data to match MOR2 ---------------------------------------------
#   need to match these on Soum ID, Aimag + Soum_name
#   
soums <- read.csv("./data/mor2data/from_Jay/soum_names.csv", stringsAsFactors = FALSE)
str(soums)
names(soums) <- c("Soum_name", "MOR2match")
soums$MOR2match<- trimws(soums$MOR2match)  #trim trailing white space

# aimag level :
aimags <- read.csv("./data/mor2data/from_Jay/aimag_names.csv", stringsAsFactors = FALSE)

# subsetting the FULL (combined) dataset to a smaller df:

##Data Import--------  

td<- mor2FULL%>% dplyr::select(
      #** Response Variables (Practices) ***
        ResWint = a_ResWint,  # Reserve Winter Pastures
        ResSpr = b_ResSpr,    # Reserve Spring Pastures
        Wotor = e_WtrOtor,
        Fotor = d_FallOtor,  
      #--- *** Predictor Variables *** 
      # Org-level Tenure q:
        #TenureWPast = q02a_RightNatWintPast,   # Tenure Rights on Winter Pasture
        #(Inf/ use /possession contract) #But only 2 in Answ: Inf & Use 
        #TenureSpPast =q02a_RightNatSpringPast, # Tenure rights on Spring Pasture
        # HH-level Tenure q:
        #UseWPast = A_UsWtrPast,                # Have or use Winter Pasture 
        #  2/3 yes, 1/3 no
        #UseWCamp    = A_UsWtrCamp,             # Have or use a Winter Campsite
        #UseSpPast = A_UsSepSprPas,             # Have or use a sep Spring Pasture
        ContractSpPast = B_ContractSprPast,    # Use/Poss contract for Sp Pasture
        ContractSpCamp = B_ContractSprCamp, 
        ContractWPast= B_ContractWtrPast,      # Use/poss contract for Wtr Pasture 
        #  most are no
        ContractWCamp = B_ContractWtrCamp,     # Use/Poss contract for Wtr Camp
      #ORG-level Rules q:
        Rule = q03_TimingRules3.3,       # Rule formality 
      #** Cognitive Social Capital :          
        # see Ulambayar et al. (201?) for info on which Qs were used, how combined
        cogSC1 = CognSC,                 # Mean of scores for items included
        # scaled 0-2 
        cogSC2 = CognSC2,                # Sum of scores, 0-12
        #cogSCAgg = CognSC_Agg    # NOT SURE HOW THIS WAS CALC'D, range is 0.41 - 2
      #** Bridging & Bonding SC:           # sum of answers re: bridging and bonding SC
        #strSC2 = StrucSC2,              # Sum of answers, range = 0-13
        #just the bonding qs:  includes family and friends outside of the soum (and within)
        bondSC = BondSCsum,        # sum of five bonding items 7.6a-e
      #Access to other pastures:
        accPast = CanUseOtherPast,
      #** confounding vars / fixed & random effects :
        ez = Ecologicalzone_4,           # ez 
        Trsp = AnotherAilLSOnPast,       # trespassing
        cbrmType = CBRM_type,            # CBRM Type
        cbrmYN = CBRM_Y_N,               # CBRM Yes/No
        
        RefNum = SurveyRefNumber,
        Org = ORG_CODE,
        Aimag = AIMAG_NAME,
        Soum = SOUM_NAME
      )
td %<>% mutate_at(c(1:8,13:17), funs(factor(.)))
#td %<>% mutate_at(c(8:10,14:17), funs(factor(.)))
td %<>% mutate_at(c(9), funs(ordered(.)))
# should we make TenurePast ordered also????
#td %<>% mutate_at(c(9:10), funs(scale(.)))


#Recode household-level tenure vars:  --------
  # Per discussion on Jan 7 2018, have condensed use & possession responses in to one category. See previous versions in version control.
# Winter Pasture Tenure:
td<- mutate(td, hhTenureWPast = case_when(ContractWPast == 0 ~ 0,   # No contract
                                          ContractWPast == 1 ~ 1,   # Yes use contract
                                          ContractWPast == 2 ~ 1,   # Yes possession contract
                                          TRUE ~ NA_real_)) 
# Winter Camp Tenure:
td<- mutate(td, hhTenureWCamp = case_when(ContractWCamp == 0 ~ 0,   # No contract
                                          ContractWCamp == 1 ~ 1,   # Yes use contract
                                          ContractWCamp == 2 ~ 1,   # Yes possession contract
                                          TRUE ~ NA_real_))         # else NA

# Spring Pasture Tenure:
td<- mutate(td, hhTenureSpPast = case_when(ContractSpPast == 0 ~ 0,   # No contract
                                           ContractSpPast == 1 ~ 1,   # Yes use contract
                                           ContractSpPast == 2 ~ 1,   # Yes possession contract
                                           TRUE ~ NA_real_))         # else NA
# Spring Camp Tenure:
td<- mutate(td, hhTenureSpCamp = case_when(ContractSpCamp == 0 ~ 0,   # No contract
                                           ContractSpCamp == 1 ~ 1,   # Yes use contract
                                           ContractSpCamp == 2 ~ 1,   # Yes possession contract
                                           TRUE ~ NA_real_))         # else NA

td %<>% mutate_at(22:25, funs(factor(.)))
#Reorder the Access Other Pastures answers:
td<- mutate(td, otherPast = case_when(accPast == 3 ~ 1,   # No set to 1
                                      accPast == 1 ~ 2,   # Yes w/in Soum set to 2
                                      accPast == 2 ~ 3,   # Yes w/in & other soums set to 3
                                      TRUE ~ NA_real_))  # Other set to NA
# order:
td %<>% mutate_at(26, funs(ordered(.)))


#Add dummy Vars for Rules --------------  
  #: [this is only useful for a few of the models]

#td <- mutate(td, RuleNo = case_when(Rule == 0 ~ 1,   # No rules = 1
#                                    Rule == 1 | Rule == 2 ~ 0,   # set others to zero
#                                    TRUE ~ NA_real_))            # else NA

      #### ----->>>>>Flipped the direction of the Rule DV so 0 = no and 1 = yes.
## But changing the name also (1/3/19) to make them easier to interpret.
## By setting them this way, they can approximate incremental impacts akin to ordered
td <- mutate(td, RuleYes = case_when(Rule == 0 ~ 0,   # No rules = 0
                                    Rule == 1 | Rule == 2 ~ 1,   # set others to 1
                                    TRUE ~ NA_real_))            # else NA
      ### Keeping this bc then I don't have to change everything w the numbering but this isn't a good one to use
        # bc it doesn't really show impact of going from 0 to 1 or 0 to 2 so hard to interpret. 
td <- mutate(td, RuleInf = case_when(Rule == 1 ~ 1,   # Informal rules = 1
                                     Rule == 0 | Rule == 2 ~ 0,   # set others to zero
                                     TRUE ~ NA_real_))            # else NA

  # this represents going beyond just any Rules (RuleYes) and increasing the formality
td <- mutate(td, RuleFormal = case_when( Rule == 0 | Rule == 1 ~ 0,   # set others to zero
                                         Rule == 2 ~ 1,   # Formal rules = 1
                                         TRUE ~ NA_real_))            # else NA

td %<>% mutate_at(c(27:29), funs(factor(.)))
 





######## Forage Use: #################
# table that (supposedly) links mor2 names w jay's names
soums <- read.csv("./data/mor2data/from_Jay/soum_names.csv", stringsAsFactors = FALSE)
names(soums) <- c("Soum_name", "MOR2match")
soums$MOR2match<- trimws(soums$MOR2match)  #trim trailing white space
# table of the aimag and soum name pairs from jay--so can 
# distinguish the soums w same name-diff aimag 
jas <- read.csv("./data/mor2data/from_Jay/j_aimag_soum.csv", stringsAsFactors = FALSE)

# now this joins them so that we have the aimag and soums 
aimag.soum<- jas %>% left_join(soums, by = "Soum_name") %>% arrange(Aimag_name, Soum_name)
# but we don't have the mor2 aimag matches...
mor2as<- as.data.frame(td[, 20:21] %>% distinct() %>% arrange(Aimag,Soum))
# yeah the aimag names don't match damn
#df$depth[df$depth<10] <- 0  # one option for reassigning values

aimag.soum<- aimag.soum %>% mutate(Aimag =
                                     case_when(Aimag_name == "Arxangai" ~ "Arkhangai",
                                               Aimag_name == "Bayanxongor"~ "Bayankhongor",
                                               #Aimag== "Dornod", "Dornod",
                                               #Aimag== "Dornogovi"
                                               #Dundgovi
                                               Aimag_name == "O'mnogovi"~ "Umnugovi",
                                               Aimag_name== "O'vorxangai"~"Uvurkhangai",
                                               #Selenge
                                               Aimag_name == "Su'xbaatar"~ "Sukhbaatar",
                                               Aimag_name == "To'v" ~ "Tuv",
                                               TRUE ~ as.character(Aimag_name)))  


# add the CV vals to this:
j.forage.cv <- read.csv("./data/forageCV.csv")
names.cv<- aimag.soum %>% left_join(j.forage.cv, by = c("Aimag_name", "Soum_name"))

# Forage USE = (Forage Demand/Forage Available) 
# see data files for metadata or call sheet = 1 below
j.forage.use <- read_excel("./data/mor2data/from_Jay/Forage_use_paired_soum_modisV6.xlsx", sheet = 2)
tbl_df(j.forage.use)
# select out the cols we want, including the forage use vals for 2010-2011:

j.forage.use <- j.forage.use[2:37,c(1:5, 16:17)]
colnames(j.forage.use)<- c("Aimag_name", "Soum_name", "Soum_ID", "Ecozone", "Poly_ID", "fu10", "fu11")
j.forage.use %<>% mutate_at(6:7, funs(as.numeric(.)))


# get average across the two years
frg.use.av <- j.forage.use %>% mutate(
  pctFrgUse = ((fu10 + fu11) / 2)
)

# get rid of extra colums
frg.use.av<- frg.use.av[,c(1:4,8)]

# save as a df to remove all of the tibble info hanging around
frg.use.av <- as.data.frame(frg.use.av)

# join with the CV data into one df
frg.use.avcv<- names.cv %>% left_join(frg.use.av, by = c("Aimag_name", "Soum_name"))
#rename MOR2Match to match 
names(frg.use.avcv)[3] <- c("Soum")
#df$depth[df$depth<10] <- 0 
frg.use.avcv$Soum[frg.use.avcv$Soum == "Bat-Ulzii"] <- "Bat-Ulziit"

frg.use.avcv <- frg.use.avcv[,c(3,4,5,8)]

# combine with the td dataframe:

td$Aimag <- as.character(td$Aimag)
td$Soum <- as.character(td$Soum)

#x<- td%>% distinct(Aimag, Soum) %>% dplyr::arrange(Aimag, Soum)
#y<- frg.use.avcv %>% distinct(Aimag, Soum) %>% arrange(Aimag, Soum)
#x == y  # to find the ones that aren't lining up.... 
#x[c(27:29,33),]
#y[c(27:29,33),]

td.fg <- td %>% inner_join(frg.use.avcv, by = c("Aimag", "Soum"))
td.fg %<>% mutate(frg.left = (100-pctFrgUse)) %>% dplyr::rename(frgCV = CV)

attr(td.fg$cogSC1, "label") <- NULL
attr(td.fg$cogSC1, "labels") <- NULL
attr(td.fg$cogSC1, "names") <- NULL

attr(td.fg$bondSC, "label") <- NULL
attr(td.fg$bondSC, "labels") <- NULL
attr(td.fg$bondSC, "names") <- NULL

# rescale the STructural scoial capital  (done below now)
library(scales)
td.fg$bondSC<- rescale(td.fg$bondSC, to = c(0,2))
#td.fg$bondSC<- scale(td.fg$bondSC) #rescale it 

# RESCALE Forage Params:
#td.fg%<>% mutate(frg.rs= (frg.left/100))
#td.fg%<>% mutate(frg.rs.CV = (frgCV/100))




#SocCap subset ----
# This creates a subset of the df that removes all records w/ NAs in Social capital, 
# so can compare across:

td.sc <- td.fg %>% drop_na(cogSC1)



# save as Rdata to pull in to next script
# Save multiple objects
save(td.fg, td.sc, file = "./data/td.RData")
# To load the data again
load("./data/td.RData")

write.csv(td.fg, "./data/td-export.csv")




