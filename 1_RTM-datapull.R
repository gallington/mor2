# This is the first script to run for Rules & Tenure project analysis. 
# This script pulls in the db, queries and wrangles the data &
# saves as rdata to pull in to next script: 


library(haven)  # this is the new package to use instead of (foreign)
#library(corrplot)
library(magrittr)
library(tidyr)
library(dplyr)

# THIS IS THE FULL DATASET WITH ALL 700 hh
# the org level data are merged in to this as well. 
mor2FULL<- read_sav("./data/Org_HHS_May2016_5_28_16.sav") #
#mor2FULL <- mor2 

# tidy format
tbl_df(mor2FULL)

# subsetting the FULL (combined) dataset to a smaller df:
## alt data import-------------------

## td<- mor2FULL%>% dplyr::select(
#         #** Response Variables (Practices) ***
#         ResWint = a_ResWint,  # Reserve Winter Pastures
#         ResSpr = b_ResSpr,    # Reserve Spring Pastures
#         #** Predictor Variables *** 
#         # Org-level Tenure q:
#         TenureWPast = q02a_RightNatWintPast,   # Tenure Rights on Winter Pasture
#         #(Inf/ use /possession contract) #But only 2 in Answ: Inf & Use 
#         TenureSpPast =q02a_RightNatSpringPast, # Tenure rights on Spring Pasture
#         # HH-level Tenure q:
#         #UseWPast = A_UsWtrPast,                # Have or use Winter Pasture 
#         #  2/3 yes, 1/3 no
#         #UseWCamp    = A_UsWtrCamp,             # Have or use a Winter Campsite
#         #UseSpPast = A_UsSepSprPas,             # Have or use a sep Spring Pasture
#         ContractSpPast = B_ContractSprPast,    # Use/Poss contract for Sp Pasture
#         ContractWPast= B_ContractWtrPast,      # Use/poss contract for Wtr Pasture 
#         #  most are no
#         ContractWCamp = B_ContractWtrCamp,     # Use/Poss contract for Wtr Camp
#         #ORG-level Rules q:
#         Rule = q03_TimingRules3.3,       # Rule formality 
#         #** Cognitive Social Capital :          
#         # see Ulambayar et al. (201?) for info on which Qs were used, how combined
#         cogSC1 = CognSC,                 # Mean of scores for items included
#         # scaled 0-2 
#         cogSC2 = CognSC2,                # Sum of scores, 0-12
#         #cogSCAgg = CognSC_Agg    # NOT SURE HOW THIS WAS CALC'D, range is 0.41 - 2
#         #** confounding vars / fixed & random effects :
#         ez = Ecologicalzone_4,           # ez 
#         Trsp = AnotherAilLSOnPast,       # trespassing
#         cbrmType = CBRM_type,            # CBRM Type
#         cbrmYN = CBRM_Y_N,               # CBRM Yes/No
#         
#         RefNum = SurveyRefNumber,
#         Org = ORG_CODE,
#         Aimag = AIMAG_NAME,
#         Soum = SOUM_NAME
#       )
# td %<>% mutate_at(c(1:8,11:14), funs(factor(.)))
# #td %<>% mutate_at(c(8:10,14:17), funs(factor(.)))
# td %<>% mutate_at(c(8), funs(ordered(.)))
# # should we make TenurePast ordered also????
# #td %<>% mutate_at(c(9:10), funs(scale(.)))
##Data Import--------  

td<- mor2FULL%>% dplyr::select(
      #** Response Variables (Practices) ***
        ResWint = a_ResWint,  # Reserve Winter Pastures
        ResSpr = b_ResSpr,    # Reserve Spring Pastures
        Wotor = e_WtrOtor,
        Fotor = d_FallOtor,  
      #** Predictor Variables *** 
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
        #just the bonding qs:
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

td %<>% mutate_at(21:24, funs(factor(.)))
#Reorder the Access Other Pastures answers:
td<- mutate(td, otherPast = case_when(accPast == 3 ~ 1,   # No set to 1
                                      accPast == 1 ~ 2,   # Yes w/in Soum set to 2
                                      accPast == 2 ~ 3,   # Yes w/in & other soums set to 3
                                      TRUE ~ NA_real_))  # Other set to NA
# order:
td %<>% mutate_at(25, funs(ordered(.)))


#Add dummy Vars for Rules --------------  
  #: [this is only useful for a few of the models]

#td <- mutate(td, RuleNo = case_when(Rule == 0 ~ 1,   # No rules = 1
#                                    Rule == 1 | Rule == 2 ~ 0,   # set others to zero
#                                    TRUE ~ NA_real_))            # else NA
      #### Flipped the direction of the Rule DV so 0 = no and 1 = yes.
td <- mutate(td, RuleNo = case_when(Rule == 0 ~ 0,   # No rules = 1
                                    Rule == 1 | Rule == 2 ~ 1,   # set others to zero
                                    TRUE ~ NA_real_))            # else NA
td <- mutate(td, RuleInf = case_when(Rule == 1 ~ 1,   # Informal rules = 1
                                     Rule == 0 | Rule == 2 ~ 0,   # set others to zero
                                     TRUE ~ NA_real_))            # else NA


td <- mutate(td, RuleFormal = case_when(Rule == 2 ~ 1,   # Formal rules = 1
                                        Rule == 0 | Rule == 1 ~ 0,   # set others to zero
                                        TRUE ~ NA_real_))            # else NA

td %<>% mutate_at(c(26:28), funs(factor(.)))
 
# rescale the STructural scoial capital
td$strSC2<- rescale(td$strSC2, to = c(0,1))




#SocCap subset ----
  # This creates a subset of the df that removes all records w/ NAs in Social capital, so can compare across:

td.sc <- td %>% drop_na(cogSC1)


# save as Rdata to pull in to next script
# Save multiple objects
save(td, td.sc, file = "./data/td.RData")
# To load the data again
load("./data/td.RData")

write.csv(td, "./data/td-export.csv")
