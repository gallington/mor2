#  NOTES:
# This script contains some original code from Kelly on 
# creating correlation matrices with heterogenous data types.


library(corrplot)
library(car)
library(ltm)
library(lavaan)
library(psych)
library(semTools)
# correlations for herder practices data

# load in data
load(file = "rpenew.RData")
head(rpe.new)
# p1, p2, and p3 are all continuous variables
# p4 through p10 are binary viariables
# r1 is indicator for the rules groupings
# 1 is informal, 0 is none, 2 is formal rules

# subset to remove ecol zone and rule names
rpe <- rpe.new[,c(1,4:18)]
# only full cases (lose 9)
rpe.n <- na.omit(rpe)

# how many in each rule category
table(rpe.new$r1)


##################
# CORRELATIONS #
#################

# use hetcor for heterogeneous correlations 
# from polycor package
corrplot(hetcor(rpe)$cor, 
         type = "lower", method = "number", 
         number.cex = 0.7, diag = FALSE)
corrplot(hetcor(rpe)$cor, 
         type = "lower", method = "pie", 
         number.cex = 0.7, diag = FALSE)

# just the predictor vars
rp <- rpe.n[,2:11]

# subsets for each rule category
# no rules (r1 == 0)
r0 <- rp[which(rpe$r1=="None"),2:11]
# informal rules (r1 == 1)
r1 <- rp[which(rpe$r1=="Informal"),2:11]
# formal rules (r1 == 2)
r2 <- rp[which(rpe$r1=="Formal"),2:11]

# KMO test and bartlett sphericity test for factoring
KMO(rp) # not good for factoring (MSA = 0.5)

# test if correlation matrix diff from 0s with 1 on diagonal
cortest.bartlett(hetcor(rp)$cor, n = nrow(rp)) 
# this one isn't as bad


# plots of predictor vars
#################################
corrplot(hetcor(rp)$cor, 
         type = "lower", method = "pie", 
         number.cex = 0.7, diag = FALSE)

# only 2 correlations over 0.5
# p2 and p3 are v highly correlated
# p4 and p5

# what about in subgroups?
corrplot(hetcor(r0)$cor, 
         type = "lower", method = "pie", 
         number.cex = 0.7, diag = FALSE)
# p6 and p7, p8 and p7 close to 0.5

corrplot(hetcor(r1)$cor, 
         type = "lower", method = "pie", 
         number.cex = 0.7, diag = FALSE)
# p7 and p2s,p3s

corrplot(hetcor(r2)$cor, 
         type = "lower", method = "pie", 
         number.cex = 0.7, diag = FALSE)
# pl and p9 negatively correlated

# doesn't seem like they are different

# some assumption checking for point-biserial correlations
# check to see if the continuous data are normally distributed within each of the categories
hist(rpe$pl)
shapiro.test(rpe$pl)# not normal
hist(rpe$p3s)
shapiro.test(rpe$p3s)

shapiro.test(r0$pl) #
shapiro.test(r0$p3s)
shapiro.test(r1$pl)
shapiro.test(r1$p3s)
shapiro.test(r2$pl) #
shapiro.test(r2$p3s)

# test for equality of variance using levene test in the car package (looks good)
leveneTest(pl~r1, data = rpe)
leveneTest(p3s~r1, data = rpe)

# note that there are several outliers
boxplot(pl~r1, data = rpe)
# less for p3s
boxplot(p3s~r1, data = rpe)

# point-biserial correlation or biserial (Pearson) correlation
biserial(rpe$p1, rpe$p4)
cor(rpe$p1, rpe$p4)
cov(rpe$p1, rpe$p4)
phi(table(rpe$p4, rpe$p5)) # very similar to pearson
phi(table(rpe$p4, rpe$p6))
cor(rpe$p4, rpe$p5) # pearson by default
cor(rpe$p4, rpe$p6)


# compute point-biserial correlations using ltm package

biserial.cor(rpe$pl, rpe$p4)
biserial.cor(rpe$pl, rpe$p4, level = 2)
biserial.cor(rpe$pl, rpe$p5)
biserial.cor(rpe$pl, rpe$p6)
biserial.cor(rpe$pl, rpe$p7)
biserial.cor(rpe$pl, rpe$p8)
biserial.cor(rpe$pl, rpe$p9)
biserial.cor(rpe$pl, rpe$p10)

biserial.cor(rpe$p3s, rpe$p4)
biserial.cor(rpe$p3s, rpe$p4, level = 2)
biserial.cor(rpe$p3s, rpe$p5)
biserial.cor(rpe$p3s, rpe$p6)
biserial.cor(rpe$p3s, rpe$p7)
biserial.cor(rpe$p3s, rpe$p8)
biserial.cor(rpe$p3s, rpe$p9)
biserial.cor(rpe$p3s, rpe$p10)

biserial.cor(r2$pl, r2$p4)
biserial.cor(r2$pl, r2$p4, level = 2)
biserial.cor(r2$pl, r2$p5)
biserial.cor(r2$pl, r2$p6)
biserial.cor(r2$pl, r2$p7)
biserial.cor(r2$pl, r2$p8)
biserial.cor(r2$pl, r2$p9)
biserial.cor(r2$pl, r2$p10)

biserial.cor(r2$p3s, r2$p4)
biserial.cor(r2$p3s, r2$p4, level = 2)
biserial.cor(r2$p3s, r2$p5)
biserial.cor(r2$p3s, r2$p6)
biserial.cor(r2$p3s, r2$p7)
biserial.cor(r2$p3s, r2$p8)
biserial.cor(r2$p3s, r2$p9)
biserial.cor(r2$p3s, r2$p10)

# the biserial correlations computed by hetcor are a tiny bit bigger than point biserial
corrplot(hetcor(rp)$cor, method = "number", type = "lower", diag = FALSE, bg = "gray")
corrplot(hetcor(rp)$cor, method = "circle", type = "lower", outline = TRUE, diag = FALSE)

# so create correlation matrix using hetcor function for moving on...
d <- hetcor(rp)$cor
# convert to covariance matrix using standard deviations
d1 <- cor2cov(d, sds = sapply(rp, sd))

# exploratory factor analysis
##############################

# suggests 3 factors
fa.parallel(d, n.obs = nrow(rp), fm = "ml")
fa.parallel(d, n.obs = nrow(rp), fm = "ml")$fa.values

fa1 <- fa(d1, factors = 3, rotation = "varimax", fm = "ml")
summary(fa1)
fa1$loadings
fa1$Structure
fa1$residual

factanal(~pl+p2s+p3s+p4+p5+p6+p7+p8+p9+p10, 
         covmat = d1, n.obs = nrow(rp),
         factors = 3, rotation = "varimax", fm = "ml")$loadings


# fit cfa using covariance matrix
# JUST based on exploratory factor analysis suggestions
mod1 <- ' seas.migrtn =~ 1*p3s + p2s + p10
          otor =~ 1*p5 + p4 + p9
          resv.past =~ 1*p7 + p6 + p8 + pl
          p4 ~~ p6'
fit <- cfa(mod1, sample.cov = d1, sample.nobs = nrow(rp))
fit2 <- cfa(mod1, rpe.n) # some negative covariances
fit3<- cfa(mod1, rpe.ezs, group="ez", meanstructure = TRUE) #doesn't converge

modificationindices(fit) # look for something greater than 3.84 (1 sd)
# added in p6 to factor 2
# mod2 <- ' f1 =~ 1*p3s + p2s + p10
#           f2 =~ 1*p5 + p4 + p9 + p6
#           f3 =~ 1*p7 + p6 + p8 + pl'
# fit <- cfa(mod2, sample.cov = d1, sample.nobs = nrow(rp))
# 
summary(fit2, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
# made RMSEA worse
# go back to model 1
# fit <- cfa(mod1, sample.cov = d1, sample.nobs = nrow(rp))

semPaths(fit2, whatLabels ='std', 
         node.label.cex=1,
         edge.label.cex=0.9, curvePivot = TRUE, 
         nCharNodes = 0, residuals = FALSE
         )
semPaths(fit2, what='std', 
         edge.label.cex=0.8, curvePivot = TRUE, 
         residuals = FALSE)
semPaths(fit3, nCharNodes = 0, whatLabels = "std",
         layout = "tree", residuals = FALSE,
         edge.label.cex= 0.9, curvePivot= TRUE)

fit.3fp.DS <- cfa(mod1, data= rpe.DS)
fit.3fp.St <- cfa(mod1, data= rpe.St)
fit.3fp.FMS <-cfa(mod1, data= rpe.FMS)
summary(fit.3fp.DS, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
summary(fit.3fp.St, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
summary(fit.3fp.FMS, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


# add in a composite latent var
# model not converging
mod3f.comp <- ' f1 =~ p3s + p2s + p10
                f2 =~ p4 + p5 + p9
                f3 =~ p7 + p6 + p8 + pl
                prac <~ f1 + f2 + f3
              '
comp.fit <- cfa(mod3f.comp, rpe.n)

# semTools groups

measurementInvariance(mod1, data = rpe.n, group = "r1")
