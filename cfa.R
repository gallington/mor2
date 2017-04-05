library(lavaan)
library(semPlot)
# See input.vars.R script first to format and tidy the data: 
# See standardizing.R script for code to standardize ecological variables
# df w standardized vars is rpe.st

# GROUPING"
# ORDER EXOG?
# need a third indicator for rules?
# INCORP 


# 4 Apr: created new rpe.st dataframe. editing below to use this.

# CFA test setup

# specify the model 1   ORG:RULES:PRACTICES:
# Not actually doing this stage yet, scroll down....
# cfa.model1 <- ' org  =~ x1 + x2 + x3      
#                rules =~ x4 + x5 + x6
#                practice =~ x7 + x8 + x9 '
# 
# # fit the model
# fit.cfa <- cfa(cfa.model, data=mor2)  # or specify the subset data matrix
# # display summary output
# summary(fit.cfa, fit.measures=TRUE)
# this doesn't work if you take out p2s
prac.mod<- 'practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10'
fit.prac<- cfa(prac.mod, data = rpe, std.all = TRUE, ordered = c("p4", "p5", "p6","p7","p8","p9","p10"))

ecol.mod <- 'eco =~ gstand + fstand + bstand + lstand'
fit.eco <- cfa(ecol.mod, data= rpe.st, std.lv = TRUE)
# specify the model 2  PRACTICES:ECOL.INDICATORS
# Note: calling normalized vars:
cfa.model2 <- ' # measurement model
                practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10
                ecol.ind =~ gstand + fstand + bstand + lstand
              '
cfa.model2.1 <- 'practice =~ pl + p2s + p3s + p7 +p8 + p10
                 ecol.ind =~ gstand + bstand + lstand
                '
# need to specify correlated errors here? Or only in sem? 
# fit the model 2
fit.cfa2 <- cfa(cfa.model2, data= rpe.st, std.lv = TRUE)
semPaths(fit.cfa2, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")
parameterEstimates(fit.cfa2)

# CFA: 
# The baseline is a null model, typically in which all of your 
# observed variables are constrained to covary with no other 
# variables (put another way, the covariances are fixed to 0)
# --just individual variances are estimated. This is what is 
# often taken as a reasonable worst-possible fitting model,
# against which your fitted model is compared in order to 
# calculate relative indexes of model fit (e.g., CFI/TLI). 

pe.mod<- '
          practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10
          ecol.ind =~ gs + fs + b.stand + ls
          p2s ~~ p3s
          ecol.ind ~ practice 
          '
pe.fit<- sem(pe.mod, data = rpe.st, std.lv= TRUE)
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








########################################################
# incorporating the RULES vars:
cfa.model3 <- ' # measurement model
                rules =~ r1 + r2
                practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10
                ecol.ind =~ e1s + e2s + e3 + e4'
# Note: have to specify which vars are ordered      
ord<- c("r1", "r2", "p4", "p5", "p6", "p7", "p8", "p9", "p10")
rpe$r1<- ordered(rpe$r1, levels= c(0,1,2))
rpe$r2<- ordered(rpe$r2, levels= c(0,1,2))

fit.cfa3 <- cfa(cfa.model3, data = rpe, std.lv = TRUE, ordered = ord, group = "ez")
semPaths(fit.cfa3,"std", 
                  edge.label.cex = 0.5,
                  curvePivot = TRUE,
                  layout = "tree")
                  # residuals = FALSE
parameterEstimates(fit.cfa3)


#
# trying an sem w the rules vars added in:

full.set <- '   
                rules =~ r1 + r2
                #practice =~ pl + p2s + p3s + p4 + p5 + p6 +p7 +p8 +p9 + p10
                practice =~ p2s + p4 + p5 + p6 + p7   #subset the practices to those that were loading more
                ecol.ind =~ e1s + e2s + e3 + e4
              # regressions
                practice ~ rules
                 ecol.ind ~ practice'
                 practice ~ ez
                ecol.ind ~ rules + practice 
                e1s ~ ez
                e2s ~ ez
                # residual correlations
                r1 ~~ r2
                pl ~~ p2s
                pl ~~ p3s
                p2s ~~ p3s
                p6 ~~ p9
                p8 ~~ p10
                e1s ~~ e3
                e3 ~~ e4
                  '

# comparing different model specifications:
# sem v1
#
sem.test <- '   rules =~ r1 + r2
                practice =~ pl + p2s + p3s + p7 + p8 + p9+ p10   #subset the practices to those that were loading more
                ecol.ind =~ gs + fs + bstand + ls
              # regressions
                ecol.ind ~ rules + practice 
                #practice ~ rules
                #ecol.ind ~ practice
                '
              
# not working  GETTING AN ERROR ASSOC W TYPES: FACTOR?????????
# SOLUTION : HAVE TO CALL ENDOG CATAGORICAL VARs ORDERED.

fit.sem <- cfa(sem.test, data= rpe.st,
               std.lv = TRUE,
               ordered = ord)
summary(fit.sem)
semPaths(fit.sem,"std",edge.label.cex=0.5, curvePivot = TRUE, residuals=FALSE)
semPaths(fit.sem)

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