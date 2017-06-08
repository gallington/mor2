# need to load polycor & semPaths
library(semPaths)
library(polycor)

modDz<- 'f1=~ 1*logavdist + logtotdist  # can we just set tot dist to something?
         f2=~ 1*rsvW + rsvSp +rsvDz
         f3=~ 1*fOtor + wOtor
         f4=~ 1*gzW_SF + gzDzd
         gzDzd ~~ rsvDz
         # what was the other one??
        '

fitDzmod<- cfa(modDz, data= practice.hhs, std.all = TRUE)
summary(fitDzmod, standardized=TRUE)

modnewprac<-' f1=~1*p2s + p3s+ p10 
              f2=~1*p6 + p7 + p8
              f3=~1*p4 + p5
              f4=~1*p9 + p10
              p8 ~~ p10
              # also add the other cov pair?
            ' 
fitnewprac<- cfa(modnewprac, data = rpe, std.all=TRUE)
fitnewprac<- cfa(modnewprac, data = rpe.new, std.all=TRUE)
# doesn't converge.
# what were fa results from this dataset? 3 factors or 1?

# CONFIGURAL INVARIANCE TESTS FOR PRACTICES latent for THE FULL DATASET:
# can tell you if groups have the same structure
config.prac.fit<- cfa(modDz, data = pracwrul, 
                      #std.all = TRUE, 
                      group = "TimingRules",
                      meanstructure = TRUE)
summary(config.prac.fit, rsquare = T,standardized = T)
fitmeasures(config.prac.fit, fit.measures = fitmsrs)
# f4 not signif for Formal group or None group, is for inf.
# Dzd msrs cov not signif for Formal group

# need to load semPaths
#semPaths(config.prac.fit, whatLabels = "std", layout= "tree")
#title(main= "Config Inv Test", add=TRUE, line = -3)


# MEASUREMENT INVARIANCE TESTS FOR PRACTICES latent for THE FULL DATASET:
metric.prac.fit<- cfa(modDz, data = pracwrul, 
                      #std.all = TRUE, 
                      group = "TimingRules",
                      meanstructure = TRUE,
                      group.equal = c("loadings"))
summary(metric.prac.fit, rsquare = T,standardized = T)
# how best to interpret this, when loadings on f4 held equal across
# groups then f4 is signif for Formal and none... 
# so this means that we *can* assume equal loadings across groups
# and move forward w one model??
fitmeasures(metric.prac.fit, fit.measures = fitmsrs)
# fits not much diff from config test, not enough to be diff?
#semPaths(metric.prac.fit, whatLabels = "std", layout= "tree")
#title("Metric Inv fit", line = -3)

# scalar inv: 
# this forces intercepts to be equal across groups
# intercepts are basically the mean score/answer/value for each question
scalar.prac.fit<- cfa(modDz, data = pracwrul, 
                      #std.all = TRUE, 
                      group = "TimingRules",
                      meanstructure = TRUE,
                      group.equal = c("loadings", "intercepts"))
summary(scalar.prac.fit, rsquare = T,standardized = T)
# get a warning message bc
# f1 and f4 have negative covariance
fitmeasures(scalar.prac.fit, fit.measures = fitmsrs)
# fit drops here, but still not by a ton...

