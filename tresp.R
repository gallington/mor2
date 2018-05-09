# first stab at trying to estimate the 
# effect of trespassing
# within the sem

# add two Q re: others grazing
olu<- as.factor(mor2$q24_OtherLandUsers)   # from org level survey
oail<- as.factor(mor2$AnotherAilLSOnPast)  # from HH survey

trsp<- select(mor2, RefNum= SocialSurveyReferenceNumber, olu = q24_OtherLandUsers, oail = AnotherAilLSOnPast)
#rpetr<- cbind(rpejoin, olu, oail) # add to rpe.new
rpetrsp<- left_join(rpejoin, trsp, by= "RefNum")



# two things to do: 
# 1. check if changing directionality will fit
# 2. run pract-> eco w tres as the grouping var

pe.mod<- ' # note no distance measures here:
          practice =~ p4 + p5 + p6 +p7 + p10
          ecol.ind =~ gs +  bare.inv + ls
          p4 ~~ p5   # got this from modificationindices() FIt improved w it.
          ecol.ind ~ practice  
          '
pe.fit<- sem(pe.mod, data = rpetr, std.lv= TRUE)
fitmeasures(pe.fit, fit.measures = fitmsrs)

# now what if we flip it?
# get pretty much the same thing. 
# bc this is measuring covariance, which is symetric

# looking at trespassing
# can we just do grouping var w sem model?

petr.mod<- ' # note no distance measures here:
          practice =~ p4 + p5 + p6 +p7 + p10
          ecol.ind =~ gs +  bare.inv + ls
          #p4 ~~ p5   # overfit if use this here.
          ecol.ind ~ practice  
          '

pe.trsp<- sem(petr.mod, data=rpetrsp, std.lv=TRUE, group = "olu")
pe.trsph<- sem(petr.mod, data=rpetrsp, std.lv=TRUE, group= "oail")
semPaths(pe.trsph, whatLabels = "std", layout = "tree")
# model overfit if specify p4~~p5.....
# SHOULD PROB EVENTUALLY ALSO CHECK PRACTICES MSRMT MODEL BTWN GROUPS
# prac.mod<- 'practice =~   p5 + p4 +p6 +p7 + p10'  
# fit.prac<- cfa(prac.mod, data = rpe.new, std.all = TRUE , 
#                meanstructure = TRUE, group = "oail")  # NOTE


# TENURE RIGHTS :

rights$CutHayRights<- ordered(rights$CutHayRights)
rights$MechWellRights<- ordered(rights$MechWellRights)
# exploratory factor analysis:

# rnum<- as.numeric(rights)
rights%<>%mutate_at(1:8, funs(factor(.)))
rights$CutHayRights<- ordered(rights$CutHayRights)
rights$MechWellRights<- ordered(rights$MechWellRights)
rord<- c("CutHayRights", "MechWellRights")

r.full<- na.omit(rights)
rcor<- hetcor(r.full)
corrplot(hetcor(rights)$cor, 
         type = "lower", method = "number", 
         number.cex = 0.7, diag = FALSE)

# EFA
# scree plot
fa.parallel(rcor, n.obs = 265, fm = "ml")$fa.values
# get an error??


rfa<- fa(rcor2$correlations,nfactors =2, n.obs = 265 )
# BIC = 6543.31 df = 13
rfa3<-fa(rcor2$correlations,nfactors =3, n.obs = 265 )
# bad
rfa1<- fa(rcor2$correlations, nfactors = 1, n.obs = 265)
# BIC = 6940.86 overall fit poorer
rfa$loadings


# have to use numeric values not factors here:
# rights.num<- rights %>% mutate_if(is.factor,as.numeric) 
# # be careful here bc it changed them to 1 and 2, not back to 0 and 1 
# factanal(~ WintPastRights + SpringPastRights + DzudPastRights + 
#          FallPastRights+ CutHayRights+ HandWellRights+ 
#          MechWellRights+ SpringsRights, 
#          data = rcor2$correlations,
#          n.obs = 265,
#          factors = 2, fm = "ml")$loadings



rights2fmod<- 'f1 =~ WintPastRights + SpringPastRights + DzudPastRights+ FallPastRights+ MechWellRights+CutHayRights
             f2 =~ HandWellRights+SpringsRights'
fit.rights<- cfa(rights2fmod, data=rights, ordered = rord)
# why is this giving an error???

rightsmod<- 'pasture =~ WintPastRights + SpringPastRights + DzudPastRights+ 
FallPastRights + CutHayRights
water =~ HandWellRights+ MechWellRights + SpringsRights'

#