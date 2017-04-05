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
f3<- stndx(forb, bmean, 3)
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


# the old way, without the function:
# Now don't have to do these steps: 
# the subset by ecol zone: DS, FMS, ST, ES

# 1a: Desert Steppe
# DSx<- grass %>% 
#   filter(ec.zn == 1) 
# Dstnd <- (DSx$gmean-min(sub.df[,2:3]))/(max(sub.df[,2:3])-min(sub.df[,2:3]))
# ddfg<- as.data.frame(cbind(sub.df, Dstnd))
# ddfg <- rename(ddfg, gstand= Dstnd)
# 
# # 1b:Typical Steppe:
# STx<- grass %>% 
#   filter(ec.zn == 2) 
# Sstnd <- (STx$gmean-min(STx[,2:3]))/(max(STx[,2:3])-min(STx[,2:3]))
# sdfg<- as.data.frame(cbind(STx, Sstnd))
# sdfg<- rename(sdfg, gstand = Sstnd)
# 
# # 1c: Forest/Mountain Steppe
# FSx<- grass %>% 
#   filter(ec.zn == 4) 
# Fstnd <- (FSx$gmean-min(FSx[,2:3]))/(max(FSx[,2:3])-min(FSx[,2:3]))
# fdfg<- as.data.frame(cbind(FSx, Fstnd))
# fdfg<- rename(fdfg, gstand=Fstnd)
# 
# # 1d: Eastern Steppe
# ESx<- grass %>% 
#   filter(ec.zn == 3) 
# Estnd <- (ESx$gmean-min(ESx[,2:3]))/(max(ESx[,2:3])-min(ESx[,2:3]))
# edfg<- as.data.frame(cbind(ESx, Estnd))
# edfg <- rename(edfg, gstand = Estnd)
# 
# #COMBINE
# # these are added in order of ecol zone code: 1/2/3/4
# # later, have to arrange rpe in order by ez to match.
# grass.stand <- bind_rows(ddfg, sdfg, edfg, fdfg)




# 
# # 2a: Desert Steppe
# DSf<- forb %>% 
#   filter(ec.zn == 1) 
# Dstndf <- (DSf$fmean-min(DSf[,2:3]))/(max(DSf[,2:3])-min(DSf[,2:3]))
# ddff<- as.data.frame(cbind(DSf, Dstndf))
# ddff<- rename(ddff, fstand = Dstndf)
# 
# # 2b:Typical Steppe:
# STf<- forb %>% 
#   filter(ec.zn == 2) 
# Sstndf <- (STf$fmean-min(STf[,2:3]))/(max(STf[,2:3])-min(STf[,2:3]))
# sdff<- as.data.frame(cbind(STf, Sstndf))
# sdff <- rename(sdff, fstand =Sstndf)
# 
# 
# # 2c: Forest/Mountain Steppe
# FSf<- forb %>% 
#   filter(ec.zn == 4) 
# Fstndf <- (FSf$fmean-min(FSf[,2:3]))/(max(FSf[,2:3])-min(FSf[,2:3]))
# fdff<- as.data.frame(cbind(FSf, Fstndf))
# fdff <- rename( fdff, fstand = Fstndf)
# 
# # 2d: Eastern Steppe
# ESf<- forb %>% 
#   filter(ec.zn == 3) 
# Estndf <- (ESf$fmean-min(ESf[,2:3]))/(max(ESf[,2:3])-min(ESf[,2:3]))
# edff<- as.data.frame(cbind(ESf, Estndf))
# edff <- rename( edff, fstand = Estndf)
# 
# #COMBINE
# forb.stand <- bind_rows(ddff, sdff, edff, fdff)




# # 3a: Desert Steppe -bare:
# DSb<- bare %>% 
#   filter(ec.zn == 1) 
# Dstndb <- (DSb$bmean-min(DSb[,2:4]))/(max(DSb[,2:4])-min(DSb[,2:4]))
# ddfb<- as.data.frame(cbind(DSb, Dstndb))
# ddfb <- rename(ddfb, bstand = Dstndb)
# 
# # 3b:Typical Steppe -Bare:
# STb<- bare %>% 
#   filter(ec.zn == 2) 
# Sstndb <- (STb$bmean-min(STb[,2:4]))/(max(STb[,2:4])-min(STb[,2:4]))
# sdfb<- as.data.frame(cbind(STb, Sstndb))
# sdfb<- rename(sdfb, bstand = Sstndb)
# 
# # 3c: Forest/Mountain Steppe
# FSb<- bare %>% 
#   filter(ec.zn == 4) 
# Fstndb <- (FSb$bmean-min(FSb[,2:4]))/(max(FSb[,2:4])-min(FSb[,2:4]))
# fdfb<- as.data.frame(cbind(FSb, Fstndb))
# fdfb<- rename(fdfb, bstand = Fstndb)
# 
# # 3d: Eastern Steppe
# ESb<- bare %>% 
#   filter(ec.zn == 3) 
# Estndb <- (ESb$bmean-min(ESb[,2:4]))/(max(ESb[,2:4])-min(ESb[,2:4]))
# edfb<- as.data.frame(cbind(ESb, Estndb))
# edfb<- rename(edfb, bstand = Estndb)
# 
# #COMBINE
# bare.stand <- bind_rows(ddfb, sdfb, edfb, fdfb)


# # 4a: Desert Steppe -litter:
# DSl<- litter %>% 
#   filter(ec.zn == 1) 
# Dstndl <- (DSl$lmean-min(DSl[,2:4]))/(max(DSl[,2:4])-min(DSl[,2:4]))
# ddfl<- as.data.frame(cbind(DSl, Dstndl))
# ddfl <- rename(ddfl, lstand = Dstndl)
# 
# # 4b:Typical Steppe -Bare:
# STl<- litter %>% 
#   filter(ec.zn == 2) 
# Sstndl <- (STl$lmean-min(STl[,2:4]))/(max(STl[,2:4])-min(STl[,2:4]))
# sdfl<- as.data.frame(cbind(STl, Sstndl))
# sdfl<- rename(sdfl, lstand = Sstndl)
# 
# # 4c: Forest/Mountain Steppe
# FSl<- litter %>% 
#   filter(ec.zn == 4) 
# Fstndl <- (FSl$lmean-min(FSl[,2:4]))/(max(FSl[,2:4])-min(FSl[,2:4]))
# fdfl<- as.data.frame(cbind(FSl, Fstndl))
# fdfl<- rename(fdfl, lstand = Fstndl)
# 
# # 4d: Eastern Steppe
# ESl<- litter %>% 
#   filter(ec.zn == 3) 
# Estndl <- (ESl$lmean-min(ESl[,2:4]))/(max(ESl[,2:4])-min(ESl[,2:4]))
# edfl<- as.data.frame(cbind(ESl, Estndl))
# edfl<- rename(edfl, lstand = Estndl)
# 
# #COMBINE
# litter.stand <- bind_rows(ddfl, sdfl, edfl, fdfl)




