# standardizing ecological variables by zone.
#
#

# standardization function ----------------------------------------------------------------

# create a new col w mean of the ecol var standardized relative to the min and max from the 100-1km plots. 
stndx<- function( df, mean.var , zone.number){
  sub.df <- df %>% filter(ec.zn == zone.number)
  mean.var.sd <- substitute((mean.var - min(sub.df[,2:3]))/(max(sub.df[,2:3])-min(sub.df[,2:3])))
  sdf <- mutate_(sub.df, mean.var.sd = mean.var.sd)
  return(sdf)
}
# the new column is always just called mean.var, 
# so will have to change it for each indicator...

# grass -----------------------------------------------------------------------------------

# ###############################
# 1. GRASS, ALL ZONES: 
#################################
# prep the df:
grass100<- mor2$PerGrassCover100
grass1k<- mor2$PerGrassCover1000
gmean <- mor2$PerGrassCover_percent_Mean500_1000
ec.zn<- mor2$Ecologicalzone_4Code100
grass<- as.data.frame(cbind(ec.zn, grass100, grass1k, gmean))
tbl_df(grass)

# still need to change the name of the column ...

g1<- stndx(grass, gmean, 1)
g2<- stndx(grass, gmean, 2)
g3<- stndx(grass, gmean, 3)
g4<- stndx(grass, gmean, 4)
# overwrite the grass df above w this new one or just add the new column?
gstan<- bind_rows(g1,g2,g3,g4) %>%
        rename(g.stand = mean.var.sd)

#####################################################
# 2. Forb Cover all zones:
forb100<- mor2$PerForbCover100
forb1k<- mor2$PerForbCover1000
fmean <- mor2$PerForbCover_percent_Mean500_1000
ec.zn<- mor2$Ecologicalzone_4Code100
forb<- as.data.frame(cbind(ec.zn, forb100, forb1k, fmean))
tbl_df(forb)

# call the function to standardize the mean val by ecol zone:
f1<- stndx(forb, fmean, 1)
f2<- stndx(forb, fmean, 2)
f3<- stndx(forb, fmean, 3)
f4<- stndx(forb, fmean, 4)
# overwrite the forbdf above w this new one or just add the new column?
fstan<- bind_rows(f1,f2,f3,f4) %>%
  rename(f.stand = mean.var.sd) %>%
  select(-ec.zn)   # remove ecol zone so don't end up with duplicates when combine


###################################
# 3. Bare Soil:  
bare100<- mor2$BareSoilCover100
bare500<- mor2$BareSoilCover500 #included 500 here bc there were lower vals at 500 which threw off diff btwn mean and min/max
bare1k<- mor2$BareSoilCover1000
bmean <- mor2$BareSoil_percent_Mean500_1000
ec.zn<- mor2$Ecologicalzone_4Code100
bare<- as.data.frame(cbind(ec.zn, bare100, bare500, bare1k, bmean))
tbl_df(bare)

# call the function to standardize the mean val by ecol zone:
b1<- stndx(bare, bmean, 1)
b2<- stndx(bare, bmean, 2)
b3<- stndx(bare, bmean, 3)
b4<- stndx(bare, bmean, 4)
# overwrite the bare df above w this new one or just add the new column?
bstan<- bind_rows(b1,b2,b3,b4) %>%
  rename(b.stand = mean.var.sd) %>%
  select(-ec.zn) #remove ecol zone so don't end up with duplicates when combine

#################################
# 4. Litter
litter100<- mor2$LitterCover100
litter500<- mor2$LitterCover500 #same here as with bare, had to include 500 msrmnt bc lower
litter1k<- mor2$LitterCover1000
lmean <- mor2$LitterCover_percent_Mean500_1000
ec.zn<- mor2$Ecologicalzone_4Code100
litter<- as.data.frame(cbind(ec.zn, litter100, litter500, litter1k, lmean))
tbl_df(litter)

# call the function to standardize the mean val by ecol zone:
l1<- stndx(litter, lmean, 1)
l2<- stndx(litter, lmean, 2)
l3<- stndx(litter, lmean, 3)
l4<- stndx(litter, lmean, 4)
# overwrite the litter df above w this new one or just add the new column?
lstan<- bind_rows(l1,l2,l3,l4) %>%
  rename(l.stand = mean.var.sd) %>%
  select(-ec.zn) #remove ecol zone so don't end up with duplicates when combine
#################################
#  5.  add these back to rpe 
######  .
rpe<- arrange(rpe, ez) # to get it in the same order as the others
rpe.st <- cbind(rpe, gstan, fstan, bstan, lstan)


# BUT WAIT! THESE AREN'T NORMALIZED!!!!

# normalizing....
rpe.st$gs <- sqrt(rpe.st$g.stand)    # new version of e1s
rpe.st$fs <- sqrt(rpe.st$f.stand+1)  # new version of e2s
rpe.st$ls <- sqrt(rpe.st$l.stand)    # new version of e4

# 
# need to create dummy vars for rules:
rpe.sub<- select(rpe.st, one_of(c("r1", "r2", "pl", "p2s", "p3s", "p4","p5", "p6", "p7", "p10", "gs", "b.stand", "ls")))

# Rules regarding the timing of grazing (r1)
# timing.NO # leave this one out as what we're then testing against.
rpe.new <-  rpe.sub %>% 
  mutate(timing.inf = ifelse(r1 == 1, 1, 0)) %>%            # new var informal rules re: timing of grazing y/n
  mutate(timing.form = ifelse(r1 == 2, 1, 0)) %>%           # new var formal rules re: timing of grazing y/n
  # Rules regarding number of livestock (r2)
  mutate( lsk.num.inf = ifelse(r2 == 1, 1, 0)) %>%  # new var informal rules re: size of herd y/n
  mutate(lsk.num.form = ifelse(r2 == 2, 1, 0)) %>%     # new var formal rules re: size of herd y/n
  # Remove the original rules vars now
  select(-r1) %>%
  select(-r2)

# asign as factors
rpe.new$lsk.num.inf<- factor(rpe.new$lsk.num.inf)
rpe.new$lsk.num.form<- factor(rpe.new$lsk.num.form)
rpe.new$timing.form<- factor(rpe.new$timing.form)
rpe.new$timing.inf<- factor(rpe.new$timing.inf)
# ordered too, or not? 
rpe.new$lsk.num.inf<- ordered(rpe.new$lsk.num.inf)
rpe.new$lsk.num.form<- ordered(rpe.new$lsk.num.form)
rpe.new$timing.form<- ordered(rpe.new$timing.form)
rpe.new$timing.inf<- ordered(rpe.new$timing.inf)


