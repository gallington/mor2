library(foreign)
library(ggplot2)
library(polycor)
library(corrplot)
library(car)
library(ltm)
library(psych)  
#requires
library(GPArotation)
library(lavaan)
library(semTools)
library(dplyr)
library(tidyr)
mor2.hhs<- read.spss("./data/Org_HHS_May2016_5_28_16.sav",
                     to.data.frame = TRUE,
                     use.value.labels = FALSE)
tbl_df(mor2.hhs)
#mor2.hhs <- filter(rpe.new, !is.na(r1)) %>%  # removed row that have NA in r1
#mor2.hhs %<>% mutate(mor2.hhs = replace(x, x<0, NA))

# ORGANIZATION VARS:----------------------------------
cb<- c(2, 8:10) # to pull survey ref no., cbrm status and ecol zone
cbrm.ez<- mor2.hhs[,cb] # these are from hhs data not org interview....
# add survey ref number too?


territory <- mor2.hhs %>%
  dplyr::select(contains("q01_GrazeTerr")) %>%
  dplyr::rename(GrazeTerr = q01_GrazeTerriSize, 
         Terrpercap = q01_GrazeTerriSize_percap, 
         Terrperhh =  q01_GrazeTerriSize_perhh)

# tenure vars  ---------------
# pull these from HH level --> not ORG level. [maria says more reliable]
rights <- mor2.hhs %>%
  select(contains("q02a_Right")) %>%
  rename(WintPastRights = q02a_RightNatWintPast,
         SpringPastRights = q02a_RightNatSpringPast,
         DzudPastRights = q02a_RightNatDzudPast,
         FallPastRights = q02a_RightNatFallPast,
         CutHayRights = q02a_RightNatHayCut,
         HandWellRights = q02a_RightNatHandWell,
         MechWellRights = q02a_RightNatMechWell,
         SpringsRights = q02a_RightNatSprings)

## org level 
# rules -------------------
 
        # GroupJoinifY = q07_GroupJoinRules_ifyes)
# this puts all five form/inf rules questions in to one df:
fullrules<- mor2.hhs %>% select(q07_GroupJoinRules,
                            q03_TimingRules3.3, 
                            q05_StockNumRules3.5,
                            q07_StockTypeRules3.7, 
                            q09_HayCutRules3.9, 
                            q11_WellUseRules3.11) %>%
  rename(GroupJoinRuleyn = q07_GroupJoinRules,
         timing = q03_TimingRules3.3, 
         stocknum = q05_StockNumRules3.5,
         stocktype = q07_StockTypeRules3.7, 
         hay = q09_HayCutRules3.9, 
         wells = q11_WellUseRules3.11)
# but maybe just use timing still? or not? 
# use more complicance vars?
timing <- mor2.hhs %>%
   dplyr :: select(contains("q03_Timing")) %>%
   dplyr :: rename(TimingRules = q03_TimingRules3.3)
timing %<>% mutate_at(1, funs(factor(.)))
timing$TimingRules<- ordered(timing$TimingRules,  labels= c("None", "Informal", "Formal"))

# compliance ----
# timingcompliance <- mor2.hhs %>%
#   select(contains("q04_Timing")) %>%
#   rename(TimingComlply = q04_TimingCompli3.4)
compliance<- mor2.hhs %>% select(q04_TimingCompli3.4, 
                            q06_StockNumCompli3.6,
                            q08_StockTypeCompli3.8, 
                            q10_HayCutCompli3.10, 
                            q12_WellUseCompli3.12) %>%
  rename(timCompl = q04_TimingCompli3.4, 
         stocknumCompl = q06_StockNumCompli3.6,
         stocktypeCompl = q08_StockTypeCompli3.8, 
         hayCompl = q10_HayCutCompli3.10, 
         wellsCompl = q12_WellUseCompli3.12)

# see vars re: lsk number compliance ... 

aware <- bind_cols(
  (mor2.hhs %>%
     select(contains("q15_RuleAwareness")) %>%
     rename(TimingRules = q15_RuleAwareness)),
  (mor2.hhs %>%
     select(contains("q16_RuleComplexity")) %>%
     rename(RuleComplex = q16_RuleComplexity))
    )

# monitoring <- mor2.hhs %>%
#   select(contains("q21_o")) %>%
#   select(contains ("Monitor")) %>%
#   rename(Informal = q21_o1_MonitorInformal , 
#          FormalUsers = q21_o2_MonitorFormal, 
#          FormalNonUsers = q21_o3_MonitorFormalNonUsers ) %>%
#   select(Informal : FormalNonUsers)  

# ALL OF THESE ARE NOs:
# consequence <- mor2.hhs %>%
#   select(contains("q22_o")) %>%
#   select(contains ("Cons")) %>%
#   rename(Herders = q22_o1_ConsHerdersScold , 
#          Leaders = q22_o2_ConsLeadersScold, 
#          Govt = q22_o3_ConsGovtScold,
#          Gossip = q22_o4_ConsGossip,
#          Fine = q22_o5_ConsFine,
#          LoseRight = q22_o6_ConsLoseRight) %>%
#   select(Herders : LoseRight)  

# Practices -----
# pulling the same vars that were used in subset analysis w eco

practice.hhs <- mor2.hhs %>% dplyr::select(AvgDist2010,
                                   TotDist2010,
                                   #TotMoves2010,  # how to handle this data? Numeric? Not normal...
                                   a_ResWint,
                                   b_ResSpr,
                                   c_ResDzud,
                                   d_FallOtor,
                                   e_WtrOtor,
                                   f_GrzWtr_SumFall,
                                   h_GrzDzud_NonEmrg)  %>%
                    rename(avdist = AvgDist2010,
                           totdist = TotDist2010,
                         #  moves =  TotMoves2010,
                           rsvW  = a_ResWint,
                           rsvSp = b_ResSpr,
                           rsvDz = c_ResDzud,
                           fOtor = d_FallOtor,
                           wOtor = e_WtrOtor,
                           gzW_SF= f_GrzWtr_SumFall,
                           gzDzd = h_GrzDzud_NonEmrg) 
                  
practice.hhs %<>% mutate_at(c(3:9), funs(factor(.)))  
practice.hhs %<>% mutate(logavdist = log(avdist + 1))
practice.hhs %<>% mutate(logtotdist = log(totdist + 1))
practice.hhs<- select(practice.hhs, -avdist, -totdist)

# COMPILE ------------
# to one dataframe:   # not including consequence or monitoring ebc no yeses????
orr <- data.frame(cbrm.ez, territory, rights, fullrules, compliance, aware) #, monitoring, compliance)
# how to asign NAs?
# set as factors:
#cols <- c("CBRM_Y_N", "CBRM_type", "Ecologicalzone_4", "GroupJoinyn", "WintPast", "SpringPast" , "DzudPast" , "FallPast" ,"CutHay","HandWell", "MechWell", "Springs","TimingRules", "TimingRules.1", "TimingRules.2" ,   "RuleComplex" ,"Informal", "FormalUsers", "FormalNonUsers", "Herders" , "Leaders", "Govt" , "Gossip",  "Fine", "LoseRight")
fcols<- c(2:4, 8:28)
orr[, fcols] <- lapply(orr[, fcols], as.factor)

orr %<>% mutate_at(cols, funs(factor(.)))
varTable()
#orr %<>% mutate(GroupJoinRule =  "is.na<-"(GroupJoinifY, GroupJoinifY == -99))

# orr predictor vars only
orr.pred <- orr[2:28]
# only full cases--- can't do this here bc there are so many NAs for some.....
# orr.full <- na.omit(orr.pred)

corrplot(hetcor(orr.pred)$cor, 
         type = "lower", method = "circle", order = "hclust", 
         number.cex = 0.7, diag = FALSE)




# general correlations among all the practices:
corrplot(hetcor(practice.hhs)$cor, 
         type = "lower", method = "number", 
         number.cex = 0.7, diag = FALSE)
title(main = "All Rules Groups")
# slightly stronger correllations w the larger dataset
# distance msrs corr'd
# reserving pastures corr'd
# migration and out-of season gzng not as corr'd, but some w/in grp
# 
# add rules to practices to be able to subset
pracwrul<- data.frame(timing, practice.hhs) 
# subset practices dataset by rules group:
# subsets for each rule category
# no rules (r1 == 0)
pr0 <- pracwrul[which(pracwrul$TimingRules=="None"),2:10]
# informal rules (r1 == 1)
pr1 <- pracwrul[which(pracwrul$TimingRules=="Informal"),2:10]
# formal rules (r1 == 2)
pr2 <- pracwrul[which(pracwrul$TimingRules=="Formal"),2:10]


# what about in subgroups?
corrplot(hetcor(pr0)$cor, 
         type = "lower", method = "number", 
         number.cex = 1, diag = FALSE)
title(main="No Rules on Timing of Gzg")
#  

corrplot(hetcor(pr1)$cor, 
         type = "lower", method = "number", 
         number.cex = 1, diag = FALSE)
title(main="Informal Rules")
#  

corrplot(hetcor(pr2)$cor, 
         type = "lower", method = "number", 
         number.cex = 1, diag = FALSE)
title(main="Formal Rules")
#  

# some assumption checking for point-biserial correlations
# check to see if the continuous data are normally distributed within each of the categories
hist(practice.hhs$logavdist)
shapiro.test(practice.hhs$logavdist)
# stil reject null -- not technically normal but it is way better

# test for equality of variance using levene test in the car package (looks good)
# haven't done this for new (bigger) dataset:
leveneTest(pl~r1, data = rpe)   
leveneTest(p3s~r1, data = rpe)

# also haven't looked at point biserial correlations yet either


# exploratory factor analysis
##############################
# so create correlation matrix using hetcor function for moving on...
d <- hetcor(practice.hhs)$cor
# convert to covariance matrix using standard deviations
d1 <- cor2cov(d, sds = sapply(practice.hhs, sd))
sds = sapply(practice.hhs, sd)

# 
#fa.parallel(practice.hhs, fm = "ml")$fa.values   # if have cor mat use n.obs = nrow(rpe.pred), 
fa.parallel(d, n.obs = nrow(practice.hhs), fm = "ml")$fa.values
# suggests 4 factors ... 

# 
fa4 <- fa(d, nfactors = 4,  fm = "ml", n.obs = nrow(practice.hhs)) #rotation = 
summary(fa1)
fa1$loadings
fa1$Structure
fa1$residual
# BIC w 1 factor = 2959, df = 27 
# BIC w 4 factors = 659.19, df=6 
# BIC w 5 factors = 304.8, df=1

# check out Dzud correlations and 
# both loading to same factor.. maybe we need a Dzud prep factor?

# next up: check these out w the separate rules groups