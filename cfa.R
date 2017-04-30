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
prac.mod<- 'practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10'  # v poor. nope
# better? :
prac.mod<- 'practice =~ pl + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10'   # nope
prac.mod<- 'practice =~ pl + p2s + p4 + p5 + p6 +p7 +p8 +p9 + p10'   # nope
prac.mod<- 'practice =~ p2s + p4 + p5 + p6 +p7 +p8 +p9 + p10'   # v bad
prac.mod<- 'practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 + p10'  # nope
prac.mod<- 'practice =~  p4 + p5 + p6 +p7 + p10'  # NOW we're getting somewhere. This is good.
# -------> but see negative variances in summary output.....
prac.mod2<- 'practice =~ p4 + p5 + p6 +p7 +p8 + p10'  # not as good if add back in p8
prac.mod3<- 'practice =~ p4 + p5 + p6 +p7 +p9 + p10'  # also not as good.

fit.prac<- cfa(prac.mod, data = rpe.st, std.all = TRUE, ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))
fit.prac2<- cfa(prac.mod2, data = rpe.st, std.all = TRUE, ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))
fit.prac3<- cfa(prac.mod3, data = rpe.st, std.all = TRUE, ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))

summary(fit.prac, 
        rsquare = T, 
        #standardized = T, 
        fit.measures=T)

summary(fit.prac3, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
semPaths(fit.prac, "std", edge.label.cex = 0.75, layout = "tree")
semPaths(fit.prac2, "std", edge.label.cex = 0.75, layout = "tree")

AIC(fit.prac, fit.prac2, fit.prac3)  # Why won't this work? Why isn't it ML estim??



# ecological indicator mesurement model:
ecol.mod <- 'eco =~ g.stand + f.stand + b.stand + l.stand'   #nope
ecol.mod <- 'eco =~ gs + fs + b.stand + ls'    # not good
ecol.mod <- 'eco =~ gs +  b.stand + ls'    # GREAT, maybe too good???

fit.eco <- cfa(ecol.mod, data= rpe.st, std.lv = TRUE)
summary(fit.eco, 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
semPaths(fit.eco, "std", edge.label.cex = 0.75, layout = "tree")


# ***** this one better than above? Or same thing?

# specify the model 2  PRACTICES:ECOL.INDICATORS
# Note: calling normalized vars:
cfa.model2 <- ' # measurement model
                practice =~ p4 + p5 + p6 +p7 + p10
                ecol.ind =~ gs + b.stand + ls
              '
# need to specify correlated errors here? Or only in sem? 
# fit the model 2
fit.cfa2 <- cfa(cfa.model2, data= rpe.st, std.lv = TRUE)
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
          #ecol.ind =~ g.stand + f.stand + b.stand + l.stand
          ecol.ind =~ gs +  b.stand + ls
          #p2s ~~ p3s
          p4 ~~ p5   # got this from modificationindices() FIt improved w it.
          ecol.ind ~ practice  
          '
pe.fit<- sem(pe.mod, data = rpe.st, std.lv= TRUE)

summary(pe.fit,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)  

modificationindices(pe.fit) # did this first w/o p4 ~~ p5
# now it suggests p4 ~~ p6 errors corr'd
pe.mod3<- '
          practice =~ p4 + p5 + p6 +p7 + p10
          #ecol.ind =~ g.stand + f.stand + b.stand + l.stand
          ecol.ind =~ gs +  b.stand + ls
          p4 ~~ p5   # added in last step first ver didnt have FIt improved w it
          p4 ~~ p6   # improved fit again maybe by too much?  
          ecol.ind ~ practice  
          '
pe.fit3<- sem(pe.mod3, data = rpe.st, std.lv= TRUE)

# possibly over-fit?
summary(pe.fit3,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE) 
semPaths(pe.fit)
semPaths(pe.fit3, 
         whatLabels = "std", 
         layout = "circle")
semPaths(pe.fit3, 
         whatLabels = "std", 
         layout = "tree")

# As of 4/27/17, haven't updated this BY ECOL ZONE section yet w new model formations
# BY ECOLOGICAL ZONE:

# change DS/ST/FMS accodingly:
# call e.zones from input.vars.R to recall zone codes by name
FMS <- filter(rpe.st, ez == 4)
pe.fit.FMS <- sem(pe.mod, data = FMS,      
              std.lv = TRUE, 
              ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))
semPaths(pe.fit.FMS, "std", residuals = FALSE)
title("FMS", line=1)

# GET ERROR HERE:
# check that the gs var isn't wonky. 
# error might be bc of issues w gs
DS <- filter(rpe.st, ez == 1)
pe.fit.DS <- sem(pe.mod, data = DS,     
                  std.lv = TRUE, 
                  ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))
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
rule.mod <- 'rules =~ r1 + r2'
fit.rules <- cfa(rule.mod, data=rpe.st,std.lv = TRUE)

# old version:
# cfa.model3 <- ' # measurement model
#                 rules =~ r1 + r2
#                 practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10
#                 ecol.ind =~ e1s + e2s + e3 + e4'

cfa.model3 <- ' # measurement model
                rules =~ r1 + r2
                practice =~ p4 + p5 + p6 +p7 + p10
                ecol.ind =~ gs + b.stand + ls'

# Note: have to specify which vars are ordered      
ord<- c("r1", "r2", "p4", "p5", "p6", "p7", "p8", "p9", "p10")
rpe$r1<- ordered(rpe$r1, levels= c(0,1,2))
rpe$r2<- ordered(rpe$r2, levels= c(0,1,2))

#fit.cfa3 <- cfa(cfa.model3, data = rpe.st, std.lv = TRUE, ordered = ord, group = "ez") # this is grouped
fit.cfa3 <- cfa(cfa.model3, data = rpe.st, std.lv = TRUE, ordered = ord)
summary(fit.cfa3,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

semPaths(fit.cfa3,"std", 
                  edge.label.cex = 0.75,
                  curvePivot = TRUE,
                  layout = "tree")
                  # residuals = FALSE
# parameterEstimates(fit.cfa3)
modificationindices(fit.cfa3)
# indicates that there are correlated errors between a lot of params, but biggest are:
# ecol.ind to 97 and rules to p6, which can't connect? right?
# also: p4-p5 
# but this is measurement model, but structural, so fit seems ok?

# move on to:
#
# trying an sem w the rules vars added in:
# no ecological zone (ez) yet: 
rpe.mod <- '   
                rules =~ r1 + r2
                practice =~ p4 + p5 + p6 + p7 + p10  #subset the practices to those that were loading more
                ecol.ind =~ gs + b.stand + ls
              # regressions
                practice ~ rules
                ecol.ind ~ practice'

# ecol.ind ~ rules + practice 
# e1s ~ ez
# e2s ~ ez
# # residual correlations
# r1 ~~ r2
# pl ~~ p2s
# pl ~~ p3s
# p2s ~~ p3s
# p6 ~~ p9
# p8 ~~ p10
# e1s ~~ e3
# e3 ~~ e4
 
fit.rpe <- sem(rpe.mod, data= rpe.st,
               std.lv = TRUE,
               ordered = ord)

summary(fit.rpe,
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

modificationindices(fit.rpe, sort. = TRUE)
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
        residuals = FALSE)

modificationindices(fit.rpe, sort. = TRUE)

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





# sem command is used here
model.est.2 <- sem(mod2, data=rpe,
                   ordered = c("r1", "r2", 
                               "p7", "p10")) # not converging....
### Command type 3: extract results from estimated model object
summary(model.est, rsq=T) # summary of results
summary(model.est.2, standardized=TRUE, rsq=TRUE)
semPaths(model.est)
semPaths(model.est,"std",edge.label.cex=0.5, curvePivot = TRUE)
parameterEstimates(model.est)
standardizedSolution(model.est)