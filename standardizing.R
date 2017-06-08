# standardizing ecological variables by zone.
#
# need to change functin so that it uses the

# standardization function ----------------------------------------------------------------

# create a new col w mean of the ecol var standardized relative to the min and max from the 100-1km plots. 
stndx<- function( df, mean.var , zone.number){
  sub.df <- df %>% filter(ec.zn == zone.number)
  mean.var.sd <- substitute((mean.var - min(sub.df[,2:4]))/(max(sub.df[,2:4])-min(sub.df[,2:4])))
  sdf <- mutate_(sub.df, mean.var.sd = mean.var.sd)
  return(sdf)
}
# the new column is always just called mean.var, 
# so will have to change it for each indicator...


# sub.df<- grass %>% filter(ec.zn == 4)
# a<- (sub.df$gmean - min(sub.df[,2:4]))
# b<- ((max(sub.df[,2:4])-min(sub.df[,2:4])))
# c<- a/b
# summary(c)





# grass -----------------------------------------------------------------------------------
# 1. GRASS, ALL ZONES: 
#
# prep the df:
grass100<- mor2$PerGrassCover100
grass500<- mor2$PerGrassCover500 
grass1k<- mor2$PerGrassCover1000
gmean <- mor2$PerGrassCover_percent_Mean500_1000
ec.zn<- mor2$Ecologicalzone_4Code100
RefNum<- mor2$SocialSurveyReferenceNumber
grass<- as.data.frame(cbind(ec.zn, grass100, grass500, grass1k, gmean, RefNum))
tbl_df(grass)



# still need to change the name of the column ...

g1<- stndx(grass, gmean, 1)
g2<- stndx(grass, gmean, 2)
g3<- stndx(grass, gmean, 3)
g4<- stndx(grass, gmean, 4)
# overwrite the grass df above w this new one or just add the new column?
gstan<- bind_rows(g1,g2,g3,g4) %>%
        rename(g.stand = mean.var.sd)

# forb####################################################
# 2. Forb Cover all zones:
forb100<- mor2$PerForbCover100
forb500<- mor2$PerForbCover500 
forb1k<- mor2$PerForbCover1000
fmean <- mor2$PerForbCover_percent_Mean500_1000
ec.zn<- mor2$Ecologicalzone_4Code100
RefNum<- mor2$SocialSurveyReferenceNumber
forb<- as.data.frame(cbind(ec.zn, forb100, forb500, forb1k, fmean, RefNum))
tbl_df(forb)

# call the function to standardize the mean val by ecol zone:
f1<- stndx(forb, fmean, 1)
f2<- stndx(forb, fmean, 2)
f3<- stndx(forb, fmean, 3)
f4<- stndx(forb, fmean, 4)
# overwrite the forbdf above w this new one or just add the new column?
fstan<- bind_rows(f1,f2,f3,f4) %>%
  rename(f.stand = mean.var.sd) 
#%>%
 # select(-ec.zn)   # remove ecol zone so don't end up with duplicates when combine


#bare##################################
# 3. Bare Soil:  
bare100<- mor2$BareSoilCover100
#included 500 here bc there were lower vals at 500 which threw off diff btwn mean and min/max
bare500<- mor2$BareSoilCover500 
bare1k<- mor2$BareSoilCover1000
bmean <- mor2$BareSoil_percent_Mean500_1000
ec.zn<- mor2$Ecologicalzone_4Code100
RefNum<- mor2$SocialSurveyReferenceNumber
bare<- as.data.frame(cbind(ec.zn, bare100, bare500, bare1k, bmean, RefNum))
tbl_df(bare)


# call the function to standardize the mean val by ecol zone:
b1<- stndx(bare, bmean, 1)
b2<- stndx(bare, bmean, 2)
b3<- stndx(bare, bmean, 3)
b4<- stndx(bare, bmean, 4)
# overwrite the bare df above w this new one or just add the new column?
bstan<- bind_rows(b1,b2,b3,b4) %>%
  rename(b.stand = mean.var.sd)
#%>%
 # select(-ec.zn) #remove ecol zone so don't end up with duplicates when combine

#litter################################
# 4. Litter
litter100<- mor2$LitterCover100
#same here as with bare, had to include 500 msrmnt bc lower
litter500<- mor2$LitterCover500 
litter1k<- mor2$LitterCover1000
lmean <- mor2$LitterCover_percent_Mean500_1000
ec.zn<- mor2$Ecologicalzone_4Code100
RefNum<- mor2$SocialSurveyReferenceNumber
litter<- as.data.frame(cbind(ec.zn, litter100, litter500, litter1k, lmean, RefNum))
tbl_df(litter)

# call the function to standardize the mean val by ecol zone:
l1<- stndx(litter, lmean, 1)
l2<- stndx(litter, lmean, 2)
l3<- stndx(litter, lmean, 3)
l4<- stndx(litter, lmean, 4)
# overwrite the litter df above w this new one or just add the new column?
lstan<- bind_rows(l1,l2,l3,l4) %>%
  rename(l.stand = mean.var.sd) 
#%>%
 # select(-ec.zn) #remove ecol zone so don't end up with duplicates when combine
#compile################################
#  5.  add these back to rpe 
######  .

#join back to rpe, aligning by RefNum
rpe.ar<- arrange(rpe, ez)

rpejoin <- left_join(rpe, fstan[,6:7], by= "RefNum") %>% 
  left_join(gstan[,6:7], by= "RefNum") %>%
  left_join(bstan[,6:7], by= "RefNum") %>%
  left_join(lstan[,6:7], by= "RefNum")




# BUT WAIT! THESE AREN'T NORMALIZED!!!!

# normalizing....
rpejoin$gs <- sqrt(rpejoin$g.stand)    # new version of e1s
rpejoin$fs <- sqrt(rpejoin$f.stand)  # new version of e2s
rpejoin$ls <- sqrt(rpejoin$l.stand)    # new version of e4
rpejoin %<>% mutate(bare.inv = (1 - b.stand))

# 
# need to create dummy vars for rules:
#rpe.sub<- select(rpejoin, one_of(c("r1", "r2", "ez", "pl", "p2s", "p3s", "p4","p5", "p6", "p7", "p8", "p9", "p10", "gs","fs", "b.stand","bare.inv", "ls")))
rpe.new<- select(rpejoin, one_of(c("r1", "r2", "ez", "pl", "p2s", "p3s", "p4","p5", "p6", "p7", "p8", "p9", "p10", "gs","fs", "b.stand","bare.inv", "ls")))
#rpe.new<- select(rpejoin, one_of(c("r1", "r2", "ez", "pl", "p2s", "p3s", "p4","p5", "p6", "p7", "p10", "gs","fs", "b.stand", "ls")))

# Rules regarding the timing of grazing (r1)
# timing.NO # leave this one out as what we're then testing against.
# rpe.new <-  rpe.sub %>% 
#   mutate(timing.inf = ifelse(r1 == 1, 1, 0)) %>%            # new var informal rules re: timing of grazing y/n
#   mutate(timing.form = ifelse(r1 == 2, 1, 0)) %>%           # new var formal rules re: timing of grazing y/n
#   # Rules regarding number of livestock (r2)
#   mutate( lsk.num.inf = ifelse(r2 == 1, 1, 0)) %>%  # new var informal rules re: size of herd y/n
#   mutate(lsk.num.form = ifelse(r2 == 2, 1, 0)) %>%     # new var formal rules re: size of herd y/n
#   mutate(lsk.num = ifelse(r2 == 0, 0, 1)) %>%          # put informal and formal rules together in one
#     # Remove the original rules vars now 
#   select(-r1) %>%
#   select(-r2)

# asign as factors
# rpe.new$lsk.num.inf<- factor(rpe.new$lsk.num.inf)
# rpe.new$lsk.num.form<- factor(rpe.new$lsk.num.form)
# rpe.new$timing.form<- factor(rpe.new$timing.form)
# rpe.new$timing.inf<- factor(rpe.new$timing.inf)
#  rpe.new$lsk.num<- factor(rpe.new$lsk.num)

# ordered too, or not? 
 # rpe.new$lsk.num.inf<- ordered(rpe.new$lsk.num.inf)
 # rpe.new$lsk.num.form<- ordered(rpe.new$lsk.num.form)
 # rpe.new$timing.form<- ordered(rpe.new$timing.form)
 # rpe.new$timing.inf<- ordered(rpe.new$timing.inf)
 # 




# 
df<-as.data.frame(lapply(c("p4","p5", "p6", "p7","p8", "p9", "p10"), function(x) factor(rpe.new[[x]])))
names(df)<- c("p4","p5", "p6", "p7","p8", "p9", "p10")

for (i in 1:length(names(df))) {
  rpe.new[[names(df)[i]]] <- df[[i]]
}

rpe.new$r1<- ordered(rpe.new$r1,  labels= c("None", "Informal", "Formal"))
rpe.new$ez<- ordered(rpe.new$ez, labels = c("Desert Steppe", "Steppe","Eastern Steppe", "FstMtn Steppe"))


olu<- as.factor(mor2$q24_OtherLandUsers)
oail<- as.factor(mor2$AnotherAilLSOnPast)
rpetr<- cbind(rpe.new, olu, oail)



# go through them systematically..... to explore ... 

scatterplotMatrix(
  #formula = ~ p4 + p5 + p6 +p7 + gs + fs + bare.inv + ls |r1,
  #formula = ~ p4  + gs + fs + bare.inv + ls |r1,
  #formula = ~   p5  + gs + fs + bare.inv + ls |r1,
  formula = ~  p6   + gs + fs + bare.inv + ls |r1,
  #formula = ~  p7 + gs + fs + bare.inv + ls |r1,
  data = rpe.new,
  diagonal = "density",
  by.groups = TRUE)


ggplot(rpe.r1, aes(x= pl, y= gs, color = r1))+
  #geom_smooth(model=lm)+
  #geom_density2d()+
  geom_point() +
  labs(x= "Total 2010 SFU", y="Perr grass cover")

# plot 2: Density plot with transparency (using the alpha argument):
ggplot(data=rpe.r1,aes(x=r1, group=p6, fill=p6)) + 
  geom_density(adjust=1.5 , alpha=0.2)
# plot 3: Stacked density plot:
ggplot(data=rpe.r1,aes(x=r1, group=p6, fill=p6)) + 
  geom_density(adjust=1.5, position="fill")


ggplot(rpe.r1, aes(x= p2s, y= gs, color = r1))+
  geom_smooth(model=lm)+
  labs(x= " Avg Distance 2010", y="Perr grass cover")


ggplot(rpe.r1, aes(x= p3s, y= gs, color = r1))+
  geom_smooth(model=lm)+
  labs(x= " Total Distance 2010", y="Perr grass cover")


ggplot(rpe.r1, aes(x= r1, y= gs, color = p4))+
  geom_boxplot()+
  labs(x= "Rules on Timing of Grazing? (No/Informal/Formal)", 
       y="Perr grass cover",
       title = "Fall Otor")
ggplot(rpe.r1, aes(x= r1, y= gs, color = p5))+
  geom_boxplot()+
  labs(x= "Rules on Timing of Grazing? (No/Informal/Formal)", 
       y="Perr grass cover",
       title = "Winter Otor")
ggplot(rpe.r1, aes(x= r1, y= gs, color = p6))+
  geom_boxplot()+  
  labs(x= "Rules on Timing of Grazing?  (No/Informal/Formal)", 
       y="Perr grass cover",
       title = "Reserving Winter Pastures")


ggplot(rpe.r1, aes(x= r1, y= gs, color = p7))+
  geom_boxplot()+ 
  labs(x= "Rules on Timing of Grazing?  (No/Informal/Formal)", 
       y="Perr grass cover",
       title = "Reserving Spring Pastures")
ggplot(rpe.r1, aes(x= r1, y= gs, color = p8))+
  geom_boxplot()+
  labs(x= "Rules on Timing of Grazing?  (No/Informal/Formal)", 
       y="Perr grass cover",
       title = "Reserve Dzud Pasture")

ggplot(rpe.r1, aes(x= r1, y= gs, color = p9))+
  geom_boxplot()+
  labs(x= "Rules on Timing of Grazing?  (No/Informal/Formal)", 
       y="Perr grass cover",
       title = "Graze Winter Pastures in Summer/Fall")

ggplot(rpe.sub, aes(x= r1, y= gs, color = p10))+
  geom_boxplot()+
  labs(x= "Rules on Timing of Grazing?  (No/Informal/Formal)", 
       y="Perr grass cover",
       title = "Grazing Dzud Reserves in Non-Emergency")



# plot 2: Density plot with transparency (using the alpha argument):
ggplot(data=rpe.r1,aes(x=pl, group=ez, fill=ez)) + 
  geom_density(adjust=1.5 , alpha=0.2)+
  labs(x="Total 2010 SFU", 
       title = "Distribution of herd size across ecological zones")+
  scale_fill_discrete(name = "Ecol Zone",
                      labels = c("Desert Steppe", "Steppe", "Eastern Steppe", "Forest/Mtn Steppe"))

ecol.sum <- rpe.r1 %>% group_by(ez) %>% summarise(sum = sum(pl))

ggplot(data=rpe.r1,aes(x=r1, group=ez, fill=ez)) + 
  geom_density(adjust=1.5 , alpha=0.2)+
  labs(x="Total 2010 SFU", 
       title = "Distribution of rules about timing of grazing\n across ecological zones (None/Informal/Formal")+
  scale_fill_discrete(name = "Ecol Zone",
                      labels = c("Desert Steppe", "Steppe", "Eastern Steppe", "Forest/Mtn Steppe"))


# also run w data subset by group?
rpe.r1 <- filter(rpe.new, !is.na(r1))  # removed row that have NA in r1