# group CFA

# this yields an error, model will not converge when add grouping var. 
# also run w data subset by group?
rpe.r1 <- filter(rpe.new, !is.na(r1))   # removed row that have NA in r1
rpe.r <-  filter(rpe.r1, !is.na(r2))
cfa.mod <- ' #measurement
                practice =~ p4 + p5 + p6 + p7 + p10  #subset the practices to those that were loading more
                ecol.ind =~ gs + bare.inv + ls
            '
timing.fit <- sem(cfa.mod, data=rpe.r1, group = "r1", ordered = ord)
                

timing.fit <- measurementInvariance(cfa.mod, data=rpe.r1, group = "r1", ordered = ord)

summary(timing.fit, 
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)

semPaths(timing.fit, "std", residuals = FALSE)

sem.mod <- ' #measurement
                practices =~ p4 + p5 + p6 + p7 + p10  #subset the practices to those that were loading more
                ecol.ind =~ gs + bare.inv + ls
             #reg
                ecol.ind ~ practices'

timing.sem <- sem(sem.mod, data=rpe.r1, group = "r1")
semPaths(timing.sem, "std", residuals = FALSE)


# timing rules as dummy vars:
r1.dummy.mod<- ' timingrule=~ timing.form + timing.inf
                  practice =~  p4 + p5 + p6 + p7 + p10 
                  ecol.ind =~  gs + bare.inv + ls
                #
                  practice ~ timingrule
                  ecol.ind ~ timingrule
                  ecol.ind ~ practice 
                '
r1.dummy.fit<- sem(r1.dummy.mod, data= rpe.r1)
summary(r1.dummy.fit, 
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)
# nope

# composites w/ dummy vars...

rules.comp.mod<- ' rules <~ 1*timing + herdsize
                   timing =~ timing.form + timing.inf 
                   herdsize =~ lsk.num.form + lsk.num.inf
                   practice =~  p4 + p5 + p6 + p7 + p10 
                   ecol.ind =~  gs + bare.inv + ls
                   practice ~ rules
                   ecol.ind ~ practice
                   ecol.ind ~ rules 
                 ' 
rules.comp.fit <- sem(rules.comp.mod, data = rpe.r)
summary(rules.comp.fit, 
        rsquare = TRUE,
        standardized = TRUE,
        fit.measures = TRUE)
#  nope

# rules as groups run as separate models w dummy vars....
rules.comp.mod<- ' rules <~ 1*timing + herdsize
                   timing =~ timing.form + timing.inf 
                   herdsize =~ lsk.num.form + lsk.num.inf
                   practice =~  p4 + p5 + p6 + p7 + p10 
                   ecol.ind =~  gs + bare.inv + ls
                   practice ~ rules
                   ecol.ind ~ practice
                   ecol.ind ~ rules 
                 ' 

# trying w Inf and Form as two diff models which we can then compare....

rules.inf.mod<- '  rules <~ 1*timing.inf + lsk.num.inf
                   practice =~  p4 + p5 + p6 + p7 + p10 
                   ecol.ind =~  gs + bare.inv + ls
                   practice ~ rules
                   ecol.ind ~ practice
                   ecol.ind ~ rules 
                 ' 
rules.inf.fit <- sem(rules.inf.mod, data = rpe.r)

###############################




########
grp.mod<- '
      migration =~ p2s + p3s +p4 +p5   # this mixes numeric and binary
      reserve.past =~ p6 + p7 + p8
      out.season.gz =~ p9 + p10
      ecol.ind =~ gs + bare.inv + ls
      gz.practice <~  1*migration + reserve.past + out.season.gz
      # regressions
      ecol.ind ~ gz.practice'

fit.grp <- sem(grp.mod, data= rpe.r1, group= "r1")

