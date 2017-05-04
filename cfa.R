library(lavaan)
library(semPlot)
# See input.vars.R script first to format and tidy the data: 
# See standardizing.R script for code to standardize ecological variables
# df w standardized vars is rpe.st

# GROUPING"
# ORDER EXOG?
# need a third indicator for rules?
# INCORP 

# USE THIS BELOW FOR QUICKLY COMPARING FIT MEASURES BTWN RUNS
fitmsrs <- c("cfi", "rmsea", "rmsea.pvalue", "srmr")

# 4 Apr: created new rpe.st dataframe. editing below to use this.
# info on new col names in rpe.st:
#
# > names(rpe.st)
# [1] "ORG_CODE"  "CBRM"      "RefNum"    "ez"        "r1"        "r2"        "p1"        "p2"        "p3"        "p4"        "p5"       
# [12] "p6"        "p7"        "p8"        "p9"        "p10"       "e1"        "e2"        "e3"        "e4"        "pl"        "p3s"      
# [23] "p2s"       "e1s"       "e2s"       "ec.zn"     "grass100"  "grass1k"   "gmean"     "g.stand"   "forb100"   "forb1k"    "fmean"    
# [34] "f.stand"   "bare100"   "bare500"   "bare1k"    "bmean"     "b.stand"   "litter100" "litter500" "litter1k"  "lmean"     "l.stand"  
# [45] "gs"        "fs"        "ls"   
# CFA test setup

# CFA: 
# The baseline is a null model, typically in which all of your 
# observed variables are constrained to covary with no other 
# variables (put another way, the covariances are fixed to 0)
# --just individual variances are estimated. This is what is 
# often taken as a reasonable worst-possible fitting model,
# against which your fitted model is compared in order to 
# calculate relative indexes of model fit (e.g., CFI/TLI). 

# specify the model 1   ORG:RULES:PRACTICES:
# Not actually doing this stage yet, scroll down....
# cfa.model1 <- ' org  =~ x1 + x2 + x3      
#                rules =~ x4 + x5 + x6
#                practice =~ x7 + x8 + x9 '


# start with measurement models

# Practices:  trying diff cmbinations, and checking model fit
prac.mod.orig<- 'practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10'  # v poor. nope
# better? :
prac.mod.0<- 'practice =~ pl + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10'   # nope
prac.mod<- 'practice =~ pl + p2s + p4 + p5 + p6 +p7 +p8 +p9 + p10'   # nope
prac.mod<- 'practice =~ p2s + p4 + p5 + p6 +p7 +p8 +p9 + p10'   # v bad
prac.mod<- 'practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 + p10'  # nope
prac.mod<- 'practice =~  p4 + p5 + p6 +p7 + p10'  # NOW we're getting somewhere. This is good.
# -------> but see negative variances in summary output.....
prac.mod2<- 'practice =~ p4 + p5 + p6 +p7 +p8 + p10'  # not as good if add back in p8
prac.mod3<- 'practice =~ p4 + p5 + p6 +p7 +p9 + p10'  # also not as good.

fit.prac.orig<- cfa(prac.mod.0, data = rpe.st, std.all = TRUE, ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))
fit.prac<- cfa(prac.mod, data = rpe.new, std.all = TRUE , ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))
fit.prac2<- cfa(prac.mod2, data = rpe.st, std.all = TRUE, ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))
fit.prac3<- cfa(prac.mod3, data = rpe.st, std.all = TRUE, ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))

summary(fit.prac, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)

summary(fit.prac3, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
semPaths(fit.prac, "std", edge.label.cex = 0.75, layout = "tree")
semPaths(fit.prac2, "std", edge.label.cex = 0.75, layout = "tree")

fitmeasures(fit.prac, fit.measures = fitmsrs)

modificationindices(fit.prac, sort. = TRUE)
# trying to specify error covariances instead of removing some indicators. 
prac.orig.mod<- ' # latent definition
                  practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10  
                 # error covariances
                  p2s ~~ p3s
                  p4 ~~ p5
                  p3s ~~ p7 
                  p2s ~~ p7 '  

# changed num each time as added another error covariance specification above...
fit.prac.orig5<- cfa(prac.orig.mod, data = rpe.st, std.all = TRUE, ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))
summary(fit.prac.orig5, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
modificationindices(fit.prac.orig5, sort. = TRUE)

# fit measures of models, adding one more error covariance each step. 
fm.prac.orig <-fitmeasures(fit.prac.orig, fit.measures = fitmsrs)
fm.prac.orig2 <-fitmeasures(fit.prac.orig2, fit.measures = fitmsrs)
fm.prac.orig3 <-fitmeasures(fit.prac.orig3, fit.measures = fitmsrs)
fm.prac.orig4 <-fitmeasures(fit.prac.orig4, fit.measures = fitmsrs)
fm.prac.orig5 <-fitmeasures(fit.prac.orig5, fit.measures = fitmsrs) # fit on this final one is ok. not as great as w/o the distance msrs...

# let's compare it to latent spec without distance msrs
fitmeasures(fit.prac, fit.measures = fitmsrs)

# fit.prac is better fit... so let's go with that.


# ecological indicator mesurement model:
ecol.mod <- 'eco =~ g.stand + f.stand + b.stand + l.stand'   #nope
ecol.modf <- 'eco =~ gs + fs + b.stand + ls'    # not good
ecol.mod <- 'eco =~ gs +  b.stand + ls'    # GREAT, maybe too good???
ecol.mod.b <- 'eco =~ gs +  fs+ bare.inv + ls'    # w new bare var     fs+

fit.ecofb <- cfa(ecol.mod.b, data= rpe.new, std.lv = TRUE)
summary(fit.ecofb, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)

fitmeasures(fit.eco, fit.measures = fitmsrs)
modificationindices(fit.ecofb, sort. = TRUE)

semPaths(fit.ecofb, "std", edge.label.cex = 0.75, layout = "tree")

ecol.mod2 <- 'eco =~ gs + fs + b.stand + ls
            # error 
              fs ~~ b.stand
              gs ~~ ls'
fit.eco2 <- cfa(ecol.mod2, data= rpe.st, std.lv = TRUE)
fit.eco2 <- cfa(ecol.mod2, data= rpe.st)
summary(fit.eco2, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
fitmeasures(fit.eco2, fit.measures = fitmsrs)
semPaths(fit.eco, "std")


# ***** this one better than above? Or same thing?

# specify the model 2  PRACTICES:ECOL.INDICATORS
# Note: calling normalized vars:
cfa.model2 <- ' # measurement model
                practice =~ p4 + p5 + p6 +p7 + p10
                #ecol.ind =~ gs + b.stand + ls
                ecol.ind =~ gs + bare.inv + ls
              '
# need to specify correlated errors here? Or only in sem? 
# fit the model 2
fit.cfa2 <- cfa(cfa.model2, data= rpe.new, std.lv = TRUE)
semPaths(fit.cfa2, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")
parameterEstimates(fit.cfa2)
summary(fit.cfa2, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)


##################
# first structural model:
pe.mod<- '
          practice =~ p4 + p5 + p6 +p7 + p10
          ecol.ind =~ gs +  bare.inv + ls
          #p2s ~~ p3s
          #p4 ~~ p5   # got this from modificationindices() FIt improved w it.
          ecol.ind ~ practice  
          '
pe.fit<- sem(pe.mod, data = rpe.new, std.lv= TRUE)

summary(pe.fit,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)  

modificationindices(pe.fit, sort. = TRUE) # did this first w/o p4 ~~ p5
# now it suggests p4 ~~ p6 errors corr'd
pe.mod3<- '
          practice =~ p4 + p5 + p6 +p7 + p10
          #ecol.ind =~ g.stand + f.stand + b.stand + l.stand
          ecol.ind =~ gs +  bare.inv + ls
          p4 ~~ p5   # added in last step first ver didnt have FIt improved w it
          ecol.ind ~ practice  
          '
pe.fit3<- sem(pe.mod3, data = rpe.new, std.lv= TRUE)

# possibly over-fit?
summary(pe.fit3,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE) 
semPaths(pe.fit, "std", layout= "circle")
semPaths(pe.fit3, "std", layout= "circle") 
         # whatLabels = "std", 
         # layout = "circle")
semPaths(pe.fit3, 
         whatLabels = "std", 
         layout = "tree")

fitmeasures(pe.fit, fit.measures = fitmsrs)

# As of 4/27/17, haven't updated this BY ECOL ZONE section yet w new model formations
# BY ECOLOGICAL ZONE:

# change DS/ST/FMS accodingly:
# call e.zones from input.vars.R to recall zone codes by name
FMS <- filter(rpe.st, ez == 4)
pe.fit.FMS <- sem(pe.mod, data = FMS,      
              std.lv = TRUE, 
              ordered = c("r1","p4", "p5", "p6","p7","p8","p9","p10"))
semPaths(pe.fit.FMS, "std", residuals = FALSE)
title("FMS", line=1)

# GET ERROR HERE:
# check that the gs var isn't wonky. 
# error might be bc of issues w gs
DS <- filter(rpe.new, ez == 1)
pe.fit.DS <- sem(pe.mod, data = DS,     
                  std.lv = TRUE, 
                  ordered = c("r1","p4", "p5", "p6","p7","p8","p9","p10"))
# exploring a bit....
rpe.st %>% group_by(ez) %>% summarise(min = min(gs))


# but this works....
ST <- filter(rpe.st, ez == 2)
pe.fit.ST <- sem(pe.mod, data = ST,     
                 std.lv = TRUE, 
                 ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))

semPaths(pe.fit.ST, "std", residuals = FALSE)
title("Typical Steppe", line = 2)

# grouping by ecol zone:
# not enough samples from group 4 (actual value is 3 = ES, but comes last in the df, so called group 4) 
# so isn't working to run groups together....
# ()
# subset out ez = 3 ( which is read as group #4) 
# 
rpe.sub <- slice(rpe, 1:121)

pe.ez<- '
          practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 #+ p10
          ecol.ind =~ e1s + e2s + e3 + e4
          p2s ~~ p3s
          practice ~ ez
          ecol.ind ~ practice 
          e1s ~ ez
          e2s ~ ez
          '
pe.fit.ez <- sem(pe.ez, data = rpe,  
              std.lv = TRUE)  
             # ordered = "ez"  )    #c("p4", "p5", "p6","p7","p8","p9","p10") # these actually aren't really ordered. just binary.
             # , group = "ez"

# ??
# Get an error when try to use group.
# ??????????????????????? #







#4/27/17, hopping down here to try adding in rules...
########################################################
# incorporating the RULES vars:
rule.mod <- 'rules =~ r1 + r2'  # this totally doesn't work
fit.rules <- cfa(rule.mod, data=rpe.st,std.lv = TRUE) 

# new data frame with dummy vars for rules....
rule.mod <- ' #composite definiton w dummy vars
              timing <- 1*timing.inf + timing.form
              herdsize <- 1*lsk.num.inf + lsk.num.form
              # latent
              rules =~ timing + herdsize
            '
fit.rule.comp <- sem(rule.mod, data = rpe.new) 

# old version:
# cfa.model3 <- ' # measurement model
#                 rules =~ r1 + r2
#                 practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10
#                 ecol.ind =~ e1s + e2s + e3 + e4'

cfa.model3 <- ' # measurement model
                rules =~ r1 + r2
                practice =~ p4 + p5 + p6 +p7 + p10
                ecol.ind =~ gs + bare.inv + ls'

# don't do this step if using dummy vars....
# Note: have to specify which vars are ordered      # Is this what messed it up before? Thinks it's linear?
ord<- c("r1", "r2", "p4", "p5", "p6", "p7", "p8", "p9", "p10")
rpe$r1<- ordered(rpe$r1, levels= c(0,1,2))
rpe$r2<- ordered(rpe$r2, levels= c(0,1,2))

#fit.cfa3 <- cfa(cfa.model3, data = rpe.st, std.lv = TRUE, ordered = ord, group = "ez") # this is grouped
fit.cfa3 <- cfa(cfa.model3, data = rpe.new, std.lv = TRUE, ordered = ord)
summary(fit.cfa3,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

semPaths(fit.cfa3,"std", 
                  edge.label.cex = 0.75,
                  curvePivot = TRUE,
                  #layout = "circle",
                  layout = "tree",
                  residuals = FALSE)


# parameterEstimates(fit.cfa3)
modificationindices(fit.cfa3, sort. = TRUE)
fitmeasures(fit.cfa3, fit.measures = fitmsrs)
# indicates that there are correlated errors between a lot of params, but biggest are:
# ecol.ind to 97 and rules to p6, which can't connect? right?
# also: p4-p5 
# but this is measurement model, not structural, so fit seems ok?

# trying w dummy vars instead
cfa.model4 <- ' # measurement model
                  rules =~ timing.inf +  timing.form + lsk.num.inf + lsk.num.form
                  practice =~  p4 + p5 + p6 +p7 + p10     #  practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10
                  ecol.ind =~ gs + bare.inv + ls'
ord2<- c("timing.inf", "timing.form", "lsk.num.inf", "lsk.num.form", "p4", "p5", "p6", "p7", "p8", "p9", "p10")

fit.cfa4 <- cfa(cfa.model4, data = rpe.new, std.lv = TRUE, ordered = ord2)
summary(fit.cfa4,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)
semPaths(fit.cfa4, "std", layout = "circle", residuals = FALSE)


# move on to:
#
# trying an sem w the rules vars added in:

# this is with original rules vars:
# no ecological zone (ez) yet: 
rpe.mod <- '   
                rules =~ r1 + r2
                practice =~ p4 + p5 + p6 + p7 + p10  #subset the practices to those that were loading more
                ecol.ind =~ gs + b.stand + ls
              # regressions
                practice ~ rules
                ecol.ind ~ practice'
 
fit.rpe <- sem(rpe.mod, data= rpe.st,
               std.lv = TRUE,
               ordered = ord)

summary(fit.rpe,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

modificationindices(fit.rpe, sort. = TRUE)
semPaths(fit.rpe, "std", layout = "circle", residuals = FALSE)

# based on this output, a few suggested changed that migth make theoretical sense:
rpe.mod2 <- '   
              rules =~ r1 + r2
              practice =~ p4 + p5 + p6 + p7 + p10  #subset the practices to those that were loading more
              ecol.ind =~ gs + b.stand + ls
            # regressions
              practice ~ rules
              ecol.ind ~ practice
            # corr errors:
              r2 ~~       p6          # is this allowed ???        
              p4 ~~       p5  
              '
fit.rpe2 <- sem(rpe.mod2, data= rpe.st,
               std.lv = TRUE,
               ordered = ord)

summary(fit.rpe2,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

fitmeasures(fit.rpe, fitmsrs)
fitmeasures(fit.rpe2, fitmsrs)  # this looks great, but rules params are not great. 


semPaths(fit.rpe2,"std",
        edge.label.cex = 0.75,
        curvePivot = TRUE,
        layout = "tree",
        residuals = T)

modificationindices(fit.rpe, sort. = TRUE) # fit.rpe2 looks great, but rules params are not great.
# so, let's check it out with the dummy vars.... 
ord2<- c("timing.inf", "timing.form", "lsk.num", "ez", "p4", "p5", "p6", "p7", "p8", "p9", "p10")

rpe.mod <- '  # latent definitions: 
                rules =~ timing.inf +  timing.form + lsk.num.inf + lsk.num.form
                practice =~  p4 + p5 + p6 + p7 + p10  # start w all practices again? pl + p2s + p3s+
                ecol.ind =~ gs  + bare.inv + ls   #add fs?
              # regressions:
                practice ~ rules
                ecol.ind ~ practice'

fit.rpe <- sem(rpe.mod, data= rpe.new,
               std.lv = TRUE,
               ordered = ord2)

summary(fit.rpe,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

semPaths(fit.rpe, "std", residuals = FALSE)

modificationindices(fit.rpe, sort. = TRUE)
#  cfi rmsea  srmr 
# 0.684 0.142 0.196 



# based on results from that make the following changes:
rpe.mod2 <- '  # latent definitions: 
                rules =~ timing.inf +  timing.form + lsk.num
                practice =~ pl + p2s + p3s + p4 + p5 + p6 + p7 + p10  
                ecol.ind =~ gs  + b.stand + ls   #add fs?
              # regressions:
              #  rules ~ timing.inf + timing.form
                practice ~ rules
                ecol.ind ~ practice
              # error covs
                p2s ~~ p3s
                p4 ~~ p5
                timing.inf ~~ timing.form
          '
fit.rpe2.2 <- sem(rpe.mod2, data= rpe.new,
               #std.lv = TRUE,
               ordered = ord2)

summary(fit.rpe2.2,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

semPaths(fit.rpe2.2, "std", residuals = FALSE)
# no rel btwn rules and practices now ...... 

rpe.mod3 <- '  # latent definitions: 
                rules =~ timing.inf +  timing.form + lsk.num.inf + lsk.num.form
                practice =~ p4 + p5 + p6 + p7 + p10  
                ecol.ind =~ gs  + b.stand + ls   #add fs?
              # regressions:
                practice ~ rules
                ecol.ind ~ practice
                ecol.ind ~ rules
              # error covs
                p4 ~~ p5
          '
fit.rpe2.3 <- sem(rpe.mod3, data= rpe.new,
                  std.lv = TRUE,
                  ordered = ord)
summary(fit.rpe2.3,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)
semPaths(fit.rpe2.3, "std")



rpe.ez<- filter(rpe.st, ez != 3)

# # # # # # #  tried to group by ecol zone but :
fit.rpe.ez <- sem(rpe.mod2, data= rpe.ez,
                std.lv = TRUE,
                ordered = ord,
                group= "ez")       # not working bc ez1 doesn't have any formal rules for r2

# comparing different model specifications:
# 
#
sem.test <- '   rules =~ r1 + r2
                practice =~ p4 + p5 + p6 + p7 + p10   #subset the practices to those that were loading more
                ecol.ind =~ gs + b.stand + ls
              # regressions
                ecol.ind ~ rules + practice 
                '
              
# not working  GETTING AN ERROR ASSOC W TYPES: FACTOR?????????
# SOLUTION : HAVE TO CALL ENDOG CATAGORICAL VARs ORDERED.

fit.sem <- sem(sem.test, data= rpe.st,
               std.lv = TRUE,
               ordered = ord)
summary(fit.sem,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)
semPaths(fit.sem,"std",edge.label.cex=0.5, curvePivot = TRUE, residuals=FALSE)


######################   
# sem v2
### 
# have to specify which of the vars are ordered:

sem2 <- ' rules =~ r1 + r2
          practice =~ pl + p2s + p3s + p7 + p10 #p4 + p5 + p6 +p7 +p8 +p9 + p10 #note this is PL not P1
          ecol =~ gstand + fstand + bstand + lstand
          # regressions
          practice ~ rules
          ecol ~  practice
          # residual correlations
          pl ~~ p2s
          pl ~~ p3s
          p2s ~~ p3s
          # p6 ~~ p9
          # p8 ~~ p10
          e1s ~~ e3
          e3 ~~ e4
          '
fit2 <- sem(sem2, data= rpe.st,
               std.lv = TRUE,
               ordered = ord)

sem3 <- ' rules =~ r1 + r2
          #practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10
          practice =~ pl+ p2s + p3s + p7 + p8 + p10  #subset the practices to those that were loading more
          ecol.ind =~  gs + fs + bstand + ls
          # regressions
          ecol.ind ~ rules + practice
          # practice ~ rules
          # ecol.ind ~ practice
          practice ~ ez
          '
      
fit3 <- sem(sem3, data= rpe.st,
               std.lv = TRUE,
               ordered = ord)

### Command type 3: extract results from estimated model object
summary(model.est, rsq=T) # summary of results
summary(model.est, standardized=TRUE, rsq=TRUE)
semPaths(model.est)
semPaths(model.est,"std",edge.label.cex=0.5, curvePivot = TRUE)
semPaths(model.est, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")
parameterEstimates(model.est)
standardizedSolution(model.est)






# What if we just take rules out as a latent var and just use r1?
pe.mod <- '   practice =~ p4 + p5 + p6 + p7 + p10  #subset the practices to those that were loading more
              ecol.ind =~ gs + b.stand + ls
            # regressions
              practice ~ timing.inf +  timing.form + lsk.num.inf + lsk.num.form
              ecol.ind ~ practice
            # corr errors:  # pulled from mification indices...
            p4 ~~ p5  
            p4 ~~ p7
          '
            # p7 ~~ b.stand  # also noted but not sure that makes sense to add...

fit.pe <- sem(pe.mod, data= rpe.new,
                #std.lv = TRUE,
                ordered = ord2)

summary(fit.pe,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

fitmeasures(fit.pe, fitmsrs)
fitmeasures(fit.rpe2, fitmsrs)  # this looks great, but rules params are not great. 

semPaths(fit.pe,"std",
         edge.label.cex = 0.75,
         curvePivot = TRUE,
         layout = "tree",
         residuals = FALSE)

modificationindices(fit.pe, sort. = TRUE)

# THIS SPECIFICATION BELOW  IS NOT AN IMPROVEMENT
pe.mod2 <- '   practice =~ p4 + p5 + p6 + p7 + p10  #subset the practices to those that were loading more
              ecol.ind =~ gs + b.stand + ls
            # regressions
              practice ~ r1 + r2
              ecol.ind ~ practice
              ecol.ind ~ r1 + r2
            # corr errors:  # pulled from mification indices...
            p4 ~~ p5  
            p4 ~~ p7
          '
fit.pe2 <- sem(pe.mod2, data= rpe.st,
              std.lv = TRUE,
              ordered = ord)

summary(fit.pe2,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

