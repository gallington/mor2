# THIS WAS ALL MOVED TO THE MARKDOWN VERSION.
#--------------------------------------------
# generating latent mean values for pos/neg ecol indicators
# 1. EFA w new vars: combined, then a pos and a neg
# 2. CFAs for pos/neg latents
# 3. 
library(corrplot)
library(car)
library(ltm)
library(lavaan)
library(psych)

# uses the rpesoil df
# 
# will need some cor matrix to feed to efa
# subset to just get the ecol related vars:
tbl_df(rpesoil)
ecols<- dplyr :: select(rpesoil, g.stand, f.stand, b.stand, l.stand, RRC, SRD, ndvi, slope)


# so create correlation matrix using hetcor function for moving on...
ecor <- hetcor(ecols)$cor

# convert to covariance matrix using standard deviations
# ecov <- cor2cov(ecor, sds = sapply(ecols, sd))
# DOESN'T WORK BC THIS HAS FACTORS 

# exploratory factor analysis
##############################

# all ecol data combined:
fa.parallel(ecor, n.obs = nrow(ecols), fm = "ml")$fa.values
fa.parallel(d, n.obs = nrow(rp), fm = "ml")$fa.values
# Parallel analysis suggests that the number of factors =  2  and the number of components =  1 

fa1 <- fa(ecor, factors = 4, rotation = "varimax", fm = "ml")
summary(fa1)
fa1$loadings
fa1$Structure
fa1$residual

factanal(~pl+p2s+p3s+p4+p5+p6+p7+p8+p9+p10, 
         covmat = d1, n.obs = nrow(rp),
         factors = 3, rotation = "varimax", fm = "ml")$loadings


