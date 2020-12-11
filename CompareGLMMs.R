# MODEL COMPARISONS AGAIN, JUST TO DOUBLE CHECK:
# The ratio of the between-cluster variance to the total variance is called the 
# Intraclass Correlation.  It tells you 
# the proportion of the total variance in Y that is accounted for by the clustering.
# So, therefore higher is better, yes?

library(lme4)
library(sjPlot)
library(sjmisc)
library(lme4)
library(sjPlot)
library(magrittr)
library(dplyr)
load("./data/td.RData")
load("./data/tdfg.RData")

# STARTING AFRESH DEC 3:
td.fg$Soum <- as.factor(td.fg$Soum)
td.fg$Aimag <- as.factor(td.fg$Aimag)
td.fg%<>% mutate(frg.left = (100-frgUse)) %>% rename(frgCV = CV)

wp <- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 + bondSC + otherPast + frg.left + frgCV + (1|Org) , family = binomial, data = td.fg) 
sp <- glmer(ResSpr ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 + bondSC + otherPast + frg.left + frgCV + (1|Org) , family = binomial, data = td.fg) 
wo <- glmer(Wotor ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 + bondSC + otherPast + frg.left + frgCV + (1|Org) , family = binomial, data = td.fg) 
fo <- glmer(Fotor ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 + bondSC + otherPast + frg.left + frgCV + (1|Org) , family = binomial, data = td.fg) 

tab_model(wp, sp, wo, fo, show.aic = TRUE, show.icc = TRUE)


# OCT 7:
rm<- glmer(ResWint ~  Rule  +   (1|Org) , family = binomial, data = td) 
tm<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp +    (1|Org) , family = binomial, data = td) 
rtm<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +   (1|Org) , family = binomial, data = td) 
scm<- glmer(ResWint ~  cogSC1 +   (1|Org) , family = binomial, data = td) 
rtscm<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 +   (1|Org) , family = binomial, data = td) 

tab_model(rm, tm,  
          show.aic = TRUE,
          show.re.var = TRUE,
          show.icc = TRUE,
          title = "Title Here"
          #pred.labels = c("x", "x", "y", "test", "ex")
          )


rms<- glmer(ResSpr ~  Rule  +   (1|Org) , family = binomial, data = td) 
tms<- glmer(ResSpr ~ hhTenureSpPast + hhTenureSpCamp +    (1|Org) , family = binomial, data = td) 
rts<- glmer(ResSpr ~ hhTenureSpPast + hhTenureSpCamp + Rule +   (1|Org) , family = binomial, data = td) 
scms<- glmer(ResSpr ~  cogSC1 +   (1|Org) , family = binomial, data = td) 
rtscms<- glmer(ResSpr ~ hhTenureSpPast + hhTenureSpCamp + Rule +  cogSC1 +   (1|Org) , family = binomial, data = td) 

tab_model(rms, tms, rts, scms, rtscms, 
          show.aic = TRUE,
          show.re.var = TRUE,
          show.icc = FALSE,
          title = "Title Here")


htmO <- glm(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 +   factor(Org) , family = binomial, data = td)  # AIC = 648
summary(htmO)
ranef(htmO)


# OCT 5:
htmO <- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 +   (1|Org) , family = binomial, data = td)  # AIC = 648
# htmS <- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 +   (1|Soum) , family = binomial, data = td.sc) #worse AIC = 656
anova(htmO, htmS)

#htm0<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 + cbrmYN+ ez +  (1|Org) , family = binomial, data = td.sc) #won't converge
htm1<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 +  cbrmYN + (1|Org) , family = binomial, data = td.sc)
htm2<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 +  (1|Org) , family = binomial, data = td.sc)  # best of 0-4
htm3<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + cogSC1  + (1|Org) , family = binomial, data = td.sc)
htm4<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule + (1|Org) , family = binomial, data = td.sc)
#htm5<- glmer(ResWint ~ hhTenureWPast + Rule +   (1|Org) , family = binomial, data = td)   # won't converge
htm6<- glmer(ResWint ~ hhTenureWCamp + Rule +  cogSC1 +  (1|Org) , family = binomial, data = td.sc)  # better
htm7<- glmer(ResWint ~ hhTenureWCamp + cogSC1 + cbrmYN + (1|Org) , family = binomial, data = td.sc)

anova(htm1, htm2, htm3, htm4)
anova(htm6, htm7)

htm2.2<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + Rule +  cogSC1 +  (1|Soum) , family = binomial, data = td.sc)  # best of 0-4

htm2.2<- glmer(ResWint ~ hhTenureWCamp + Rule +  cogSC1 +  (1|Org) , family = binomial, data = td.sc)  # not better
anova(htm2, htm2.2)
summary(htm2)
summary(htm2.2)


# Need to set the specifications on this to get the outputs I actually want, inc AIC:
tab_model(htm1, htm3, htm4, htm2, show.aic = TRUE, show.icc= FALSE)

plot_model(htm2, sort.est = TRUE)
tab_model(htm2)

      # AS OF OCT. 29, here is the best model table, can rm quadratic term if want...
tab_model(htm2, show.aic = TRUE, show.icc= TRUE, rm.terms = "Rule.Q", 
          title = "Reserving Winter Pastures",
          pred.labels = c("Intercept", 
                          "Tenure: Wtr Pasture", 
                          "Tenure: Wtr Camp", 
                          "Rule formality", 
                          #"Rule-quadratic", 
                          "Cognitive SC"))



# SPRING
bestSp <- glmer(formula = ResSpr ~ hhTenureSpPast + hhTenureSpCamp+ cbrmYN + RuleNo+ cogSC1 + (1|Org), family = binomial, data = td)
bestSp2 <- glmer(formula = ResSpr ~ hhTenureSpPast + hhTenureSpCamp+ cbrmYN + Rule+ cogSC1 + (1|Org), family = binomial, data = td)

# sp1 <- glmer(formula = ResSpr ~ hhTenureSpPast + hhTenureSpCamp+ cbrmYN + RuleNo+ cogSC1 + ez + (1|Org), family = binomial, data = td)
sp2 <- glmer(formula = ResSpr ~ hhTenureSpPast + hhTenureSpCamp+ cbrmYN + Rule+ cogSC1 + (1|Org), family = binomial, data = td)
sp3 <- glmer(formula = ResSpr ~ hhTenureSpPast + hhTenureSpCamp+ cbrmYN  + (1|Org), family = binomial, data = td)
sp4 <- glmer(formula = ResSpr ~ hhTenureSpPast + hhTenureSpCamp+  cogSC1 + (1|Org), family = binomial, data = td)
sp5 <- glmer(formula = ResSpr ~ hhTenureSpPast + hhTenureSpCamp+ Rule + (1|Org), family = binomial, data = td)

anova(bestSp, bestSp2)

summary(bestSp)
tab_model(sp5, sp4, sp3, sp2, bestSp2, show.aic = TRUE)

summary(sp4)



###########

#### INCORPORATIN
