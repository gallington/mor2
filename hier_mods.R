#practice ~ ez + CBRM + ez*CBRM + (1|CBRM/rules)
require(car)
require(MASS)
require(lme4)

gmm<- glmer(p4 ~ ez + CBRM + ez*CBRM + (1|CBRM/r1) + (1|RefNum), data = raw.data, 
            family = binomial(link = "logit"))


# Only random effects:
library(MCMCglmm)
# fix this:
prior = list(R = list(V = 1, n = 0, fix = 1), 
             G = list(G1 = list(V = 1, n = 1),
             G2 = list(V = 1, n = 1), 
             G3 = list(V = 1, n = 1)))
set.seed(45)
MCMC <- MCMCglmm(p4 ~ 1, random = ~CBRM + ez + RefNum, #+ gen + district,
                 data = raw.data, family = "categorical", prior = prior, verbose = FALSE,
                 nitt = 5e+05, burnin = 5000, thin = 100)
summary(MCMC)

MCMCplot(MCMC)twit