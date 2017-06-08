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
# org level
orgrights <- mor2.hhs %>%
  select(contains("q02a_Right")) %>%
  rename(WintPastRights = q02a_RightNatWintPast,
         SpringPastRights = q02a_RightNatSpringPast,
         DzudPastRights = q02a_RightNatDzudPast,
         FallPastRights = q02a_RightNatFallPast,
         CutHayRights = q02a_RightNatHayCut,
         HandWellRights = q02a_RightNatHandWell,
         MechWellRights = q02a_RightNatMechWell,
         SpringsRights = q02a_RightNatSprings)

## hh level
tbl_df(mor2.hhs)
hhrights <- mor2.hhs %>%
    select(A_UsWtrCamp,
           A_UsWtrPast,
           A_UsSepSprCS,
           A_UsSepSprPas,
           A_SepDzud,
           A_HayCutFld)%>%
    rename(Wcmp = A_UsWtrCamp,
           Wpast = A_UsWtrPast,
           Sprcmp = A_UsSepSprCS,
           SprPast= A_UsSepSprPas,
           DzPast= A_SepDzud,
           HayFld = A_HayCutFld)
hhrights%<>%mutate_at(1:6, funs(factor(.)))

hhcontract<- mor2.hhs %>%
    select(B_ContractWtrCamp,
           B_ContractWtrPast,
           B_ContractSprCamp,
           B_ContractSprPast,
           B_ContractDzud,
           B_ContractHayCut)%>%
    rename(ContractWtrCamp = B_ContractWtrCamp,
           ContractWtrPast = B_ContractWtrPast,
           ContractSprCamp = B_ContractSprCamp,
           ContractSprPast = B_ContractSprPast,
           ContractDzud = B_ContractDzud,
           ContractHayCut = B_ContractHayCut)
hhcontract%<>%mutate_at(1:6, funs(factor(.)))



pr0 <- mor2.hhs[which(mor2.hhs$A_UsWtrCamp=="None"),2:10]

           
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

orr %<>% mutate_at(2, funs(factor(.)))
orr %<>% mutate_at(3:4, funs(ordered(.)))
varTable(orr)
#orr %<>% mutate(GroupJoinRule =  "is.na<-"(GroupJoinifY, GroupJoinifY == -99))

orr$TimingRules<- ordered(orr$TimingRules,  labels= c("None", "Informal", "Formal"))
orr$Ecologicalzone_4<- ordered(orr$Ecologicalzone_4, labels = c("Desert Steppe", "Steppe","Eastern Steppe", "FstMtn Steppe"))





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
#
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
d1 <- cor2cov(d, sds = sapply(prac.num, sd))  # doesn't work
sds = sapply(prac.num, sd) 

# 
#fa.parallel(practice.hhs, fm = "ml")$fa.values   # if have cor mat use n.obs = nrow(rpe.pred), 
fa.parallel(d, n.obs = nrow(practice.hhs), fm = "ml")$fa.values
# suggests 4 factors ... 

# 
fa4 <- fa(d, nfactors = 4,  fm = "ml", n.obs = nrow(practice.hhs)) #rotation = 
summary(fa1)
fa1$loadings #changing name of object based on number of factors specified
fa1$Structure
fa1$residual
# BIC w 1 factor = 2959, df = 27 
# BIC w 4 factors = 659.19, df=6 
# BIC w 5 factors = 304.8, df=1

# check out Dzud correlations and 
# both loading to same factor.. maybe we need a Dzud prep factor?

# have to use numeric values not factors here:
prac.num<- practice.hhs %>% mutate_if(is.factor,as.numeric) 
# be careful here bc it changed them to 1 and 2, not back to 0 and 1 
factanal(~ rsvW+rsvSp+rsvDz+fOtor+wOtor+gzW_SF+gzDzd+logavdist+logtotdist, 
         data = prac.num,
         #covmat = d1, n.obs = nrow(rp),
         factors = 4, rotation = "varimax", fm = "ml")$loadings


# next up: check these out w the separate rules groups
r0d<- hetcor(pr0)$cor
r1d<- hetcor(pr1)$cor
r2d<- hetcor(pr2)$cor


fa.parallel(r0d, n.obs = nrow(pr0), fm = "ml")$fa.values
# suggests 4 factors for No Rules Group
fa.parallel(r1d, n.obs = nrow(pr1), fm = "ml")$fa.values
# suggests 4 factors for Informal Rules Group (but 3 components ???)
# what is difference between factors and components?
fa.parallel(r2d, n.obs = nrow(pr2), fm = "ml")$fa.values
# suggests 4 factors for Formal Rules Group
# see scree plots for all three that are generated by running the above code

# no rules:
far04 <- fa(r0d, nfactors = 4,  fm = "ml", n.obs = nrow(pr0))
# HEywood case detected... BIC =  3108.2
# looks like it is coming from the rsvW var
# and also from av dist
# but if do it w the raw data, not the cov matrix, 
fa(r0.num, nfactors =4 , fm = "ml")
# get Heywood case warning again, but
# looks like it is just on the avdist var now
summary(far04)
# informal rules: 
# four factors
far14 <- fa(r1d, nfactors = 4,  fm = "ml", n.obs = nrow(pr1))
# BIC =  195.87
# three factors
far13 <- fa(r1d, nfactors = 3,  fm = "ml", n.obs = nrow(pr1))
# BIC =  355.86
# formal rules:
far24 <- fa(r2d, nfactors = 4, fm = "ml", n.obs = nrow(pr2))
# BIC w 4 factors =  2269.04, df = 6
# BIC w 3 factors =  2305.03, df = 12
far24$loadings
far24$Structure # what is the difference btwn these outputs? 
# both are labelled Loadings.... 

# check out loadings from stats :: factanal:
# need numeric vals:
r0.num<- pr0 %>% mutate_if(is.factor,as.numeric) 
r1.num<- pr1 %>% mutate_if(is.factor,as.numeric) 
r2.num<- pr2 %>% mutate_if(is.factor,as.numeric) 
factanal(~ rsvW+rsvSp+rsvDz+fOtor+wOtor+gzW_SF+gzDzd+logavdist+logtotdist, 
         data = r0.num,
         #covmat = d1, n.obs = nrow(rp),
         factors = 4, rotation = "varimax", fm = "ml")$loadings
factanal(~ rsvW+rsvSp+rsvDz+fOtor+wOtor+gzW_SF+gzDzd+logavdist+logtotdist, 
         data = r1.num,
         #covmat = d1, n.obs = nrow(rp),
         factors = 4, rotation = "varimax", fm = "ml")$loadings
factanal(~ rsvW+rsvSp+rsvDz+fOtor+wOtor+gzW_SF+gzDzd+logavdist+logtotdist, 
         data = r2.num,
         #covmat = d1, n.obs = nrow(rp),
         factors = 4, rotation = "varimax", fm = "ml")$loadings
# why are the cumulative variance totals always lower for factanal compared to fa?

# Concl : when looking at separate groups: see sem outputs file in google slides
# Still support for four factors, but much more spread between diff factors for individual
# variables -- not as tightly clustered. Thought it might be the opposite...

fitmsrs <- c("df","chisq", "cfi", "rmsea", "rmsea.pvalue", "srmr")

# kelly fit cfa using covariance matrix -- i can't get cov matrix to work... 
# I was just using orig data before, so just gonig to use that for now...
# exploring diff measurement model specifications based on exploratory factor analysis suggestions
# do we really need to fix 1* here? doesn't it do it automatically?
mod1 <- ' f1 =~ 1*logavdist + logtotdist # +fOtor 
          f2 =~ 1*rsvW +rsvSp # + rsvDz
          f3 =~ 1*rsvDz +gzDzd +fOtor + wOtor # 
          f4 =~ 1*gzW_SF # +rsvDz
        ' 
# fit <- cfa(mod1, sample.cov = d1, sample.nobs = nrow(rp))  # this uses cov mat
fit1 <- cfa(mod1, data = practice.hhs, std.all = TRUE) # this uses raw data
summary(fit1, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)

modificationindices(fit1, sort. = TRUE)
# suggests that two vars need to be added to f1 and f4, which corresponds to what EFA said

# so let's try that:
# shoudl prob just add one at a time, though, and check fit each time.... 
# so, let's look at this then maybe step it back
mod2 <- ' f1 =~ 1*logavdist + logtotdist   
          f2 =~ 1*rsvW +rsvSp 
          f3 =~ 1*rsvDz +gzDzd +fOtor + wOtor  
          f4 =~ 1*gzW_SF + gzDzd 
        ' 
fit2 <- cfa(mod2, data = practice.hhs, std.all = TRUE) # this doesn't converge!!!
# doesn't converge even if only add one....

# test:
modtest<- ' f1 =~ 1*logavdist + logtotdist  +fOtor + wOtor  
          f2 =~ 1*rsvW +rsvSp 
          f3 =~ 1*rsvDz +gzDzd  
          f4 =~ 1*gzW_SF + gzDzd 
        ' 
fittest <- cfa(modtest, data = practice.hhs, std.all = TRUE) # this doesn't converge!!!
# nope
# so, thus far mod1 is best

# this one has them grouped by theme as we have been doing it:
modtheme <- 'mobility =~ 1*logtotdist + logavdist  
             rsv pasture =~ 1*rsvW +rsvSp +rsvDz
             otor =~ 1*fOtor + wOtor
             outseas gz =~ 1*gzW_SF + gzDzd
            '
fittheme <- cfa(modtheme, data = practice.hhs, std.all = TRUE) #  

summary(fittheme, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
# this has the best fit yet
semPaths(fittheme, whatLabels = "std")

# BUT getting negative variance/Heywood case on the totaldist variable. 

moddzlink2 <- 'mobility =~ 1*logtotdist + logavdist  
             rsv pasture =~ 1*rsvW +rsvSp +rsvDz
             otor =~ 1*fOtor + wOtor
             outseas gz =~ 1*gzW_SF + gzDzd
             rsvDz ~~ gzDzd       # modindicies suggests these are linked
             rsvW ~~  gzW_SF      # as does the earlier EFA
            '
fitdzlink2 <- cfa(moddzlink2, data = practice.hhs, std.all = TRUE) # 

summary(fitdzlink2, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
semPaths(fitdzlink2,  nCharNodes= 0, "std",
         whatLabels = "std", layout= "spring", 
         residuals = FALSE, intercepts = FALSE, edge.label.cex=0.9)
# Let's look at these by separate Rules groups:
fitthemer0 <- cfa(modtheme, data = pr0, std.all = TRUE)  
fitthemer1 <- cfa(modtheme, data = pr1, std.all = TRUE)  
fitthemer2 <- cfa(modtheme, data = pr2, std.all = TRUE)  

summary(#fitthemer0,
        #fitthemer1,
       fitthemer2,
        rsquare = T, standardized = T, fit.measures=T)

# Now look at 4 factor practices by ecological zone:

orr.ezs<- filter(orr, Ecologicalzone_4 != "Eastern Steppe")
fitdz.ez <- cfa(modtheme, data = orr.ezs, 
                  std.all = TRUE, group = "Ecologicalzone_4") #
summary(fitdz.ez, standardized = TRUE, fit.measures = TRUE)
