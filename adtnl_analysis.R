library(readxl)


c.test<- test[,5:19]

orig.params<- eco.params[,c(5,8,11,12,14,16,19)]
eco.means<- summarise_all(orig.params, funs(mean, sd, max, min))


# GOATS :: 
goats<- mor2%>% dplyr::select(
  RefNum = SocialSurveyReferenceNumber,
  CBRM,
  Goat2010_SFU,
  Total2010SFU) %>%
  mutate(goat_prop = Goat2010_SFU/Total2010SFU)

eco.vars.pred <- readRDS("./data/eco.2latent.RDS")

goats_veg<- left_join(goats, eco.vars.pred, by= "RefNum")

goats_veg%<>% mutate(eco1s=scales::rescale(goats_veg$eco1, to= c(0,1)))%>%
  mutate(eco2s=scales::rescale(goats_veg$eco2, to= c(0,1)))

ggplot(data= goats_veg)+
  geom_point(aes(x=goat_prop, y = eco1s, color= as.factor(CBRM)))+
  geom_smooth(aes(x=goat_prop, y=eco1s))


ggplot(data= goats_veg)+
  geom_point(aes(y=eco2s, x = goat_prop, color= as.factor(CBRM)))+
  geom_smooth(aes(y=eco2s, x=goat_prop))


## CCA


species<- read_excel("./data/Species richness data_5001000Ginger.xlsx", sheet = 1)
sp_meta<- read_excel("./data/Species richness data_5001000Ginger.xlsx", sheet = 2)


spx<- species[,c(10,12, 31:377)]
spx<- dplyr::rename(spx, PlotID= UniquePlotID)
#comb<- left_join(spx, eco.vars.pred, by = "PlotID")%>% na.omit()
comb<- left_join(spx, goats_veg, by = "PlotID")%>% na.omit()

spx500<- comb%>% filter(DistanceClass_m == 500) %>% dplyr::select(ACAS:UKPF)
#spx500<- comb%>% filter(DistanceClass_m == 500) %>% dplyr::select(ARCH,ARCO,ARDR,ARFRE,ARFRI,ARGL,ARINT,ARLA,ARMAC,ARPAL,ARPC,CARBU,CARKO,CARLE,CARMI,CARPY,CARST,CHAC,CHAL,CHARI,CXDH,CXDU,CXEN,CXKO,CXOR,CXPE,CXPED)
spx1k <- comb%>% filter(DistanceClass_m == 1000) %>% dplyr::select(ACAS:UKPF)
spx500<-as.matrix(spx500)
spx500[spx500>0]<-1
spx1k<- as.matrix(spx1k)
spx1k[spx1k>0]<- 1

convars<- comb%>% arrange(PlotID) %>% 
  dplyr::select(eco1s, eco2s, goat_prop)


ca<- cca(spx500 ~ eco1 + eco2, data = latents)
plot(ca, xlim= c(-3,3), ylim= c(-3,3))
fig <- ordiplot(ca$CCA$v, display = "species", xlim = c(-3,3), ylim = c(-3,3)) #
#points(fig, "sites",  pch=21, col="red", bg="yellow")
text(fig, "species", col="blue", cex=0.5)
identify(fig, "spec")
# test it:
anova(ca, by = "term", permu = 200)
# both terms significant.
cca.sp<-as.data.frame(ca$CCA$v)
cca.sp <- data.frame(SpCode = row.names(cca.sp), cca.sp)
cca.sp<- mutate(cca.sp, EF = ifelse(CCA2 > 0 & CCA1 > 0, 2,
                           ifelse(CCA2 <0 & CCA1 >0, 1,
                           0)))
sp_meta$SpCode<- sp_meta$`Species Code`
# link to the species name info:
cca.species<- inner_join(cca.sp, sp_meta, by = "SpCode")
# export
write.csv(cca.species, file = ".exportedCCA.csv")

# w the goats info and the rescale latent values so they are set 0-1, don't have negatives.
# tried this with an interaction and it is not signif
ca2<- cca(spx500 ~ eco1s + eco2s + goat_prop, data= convars)
anova(ca2, by = "term", permu = 200)
# model is signif although only latents signif if examine individually
cca2.sp<- as.data.frame(ca2$CCA$v)
cca2.sp <- data.frame(SpCode = row.names(cca2.sp), cca2.sp)
cca2.sp<- mutate(cca2.sp, EF = ifelse(CCA2 >0.75 & CCA1 > -0.25, 3,
                                ifelse(CCA2 <-1 & CCA1 <0, 1,
                                 ifelse(CCA2 > -1 & CCA2 <2 & CCA1 < -0.5, 2,
                                           0))))
# link to the species name info:
cca2.species<- inner_join(cca2.sp, sp_meta, by = "SpCode")
# export
write.csv(cca2.species, file = "exportedCCA2.csv")

# SHRUBS ::
#mutate_at(1:2, funs(as.character(.)))
sp_tidy<- tbl_df(species) %>% 
  gather("SpCode", "Cover", 31:377)

# EXTRACTING ARTEMESIA FRIGIDA:
artem<- species %>% 
  dplyr::select(PlotID = UniquePlotID,
                AimagName,
                SoumName,
                DistanceClass_m,
                ARAD
                #ARFRI
                )%>%
  filter(ARAD > 0)  # final total is 101 obs for ARTEM FRI, 50 for ARAD.
# just 500 m for now. Need to get avg between the two dis classes eventually
#arfri500<- filter(arfri, DistanceClass_m == 500)
artem500<- filter(artem, DistanceClass_m == 500)
#artem1000<- filter(artem, DistanceClass_m == 1000)
# join to predicted vars:
artemjoin<- left_join(artem500, eco.vars.pred, by = "PlotID")
#afrijoin1k<- left_join(arfri1000, eco.vars.pred, by = "PlotID")
# rescale the eco vars
artemjoin %<>% 
  na.omit() %>%
  mutate(eco1.rs = scales::rescale(eco1, to=c(0, 1))) %>%
  mutate(eco2.rs = scales::rescale(eco2, to=c(0, 1)))

# afrijoin1k<- afrijoin1k %>% 
#   # na.omit() %>%
#   mutate(eco1.rs = scales::rescale(eco1, to=c(0, 1))) %>%
#   mutate(eco2.rs = scales::rescale(eco2, to=c(0, 1)))

artmat<-artemjoin[,c(5,9,10)]
cor(artmat, use= "pairwise.complete.obs")

#         ARFRI   eco1.rs   eco2.rs
# ARFRI   1.0000000 0.3508496 0.1201922
# eco1.rs 0.3508496 1.0000000 0.6779810
# eco2.rs 0.1201922 0.6779810 1.0000000



# EXTRACTING CARAGANA sp.:
carag<- species %>% 
  dplyr::select(PlotID = UniquePlotID,
                AimagName,
                SoumName,
                DistanceClass_m,
                CARBU:CARST)%>%
        filter(DistanceClass_m == 1000,
        #filter(DistanceClass_m == 500,
               CARPY>0) #switch out to check sample size when removed...
str(carag)
# CARMI in 17
# CARBU in 6
# CARKO in 1
# CARLE in 18
# CARPY in 11,10
# CARST in 19,18

carag500<- filter(carag, DistanceClass_m == 500)
caragjoin<- left_join(carag500, eco.vars.pred, by = "PlotID")
caragjoin<- caragjoin %>% 
  na.omit() %>%
  mutate(eco1.rs = scales::rescale(eco1, to=c(0, 1))) %>%
  mutate(eco2.rs = scales::rescale(eco2, to=c(0, 1)))

caragmat<-caragjoin[,c(5:10, 14:15)]
cor(caragmat, use= "pairwise.complete.obs")
# 
# A quick and dirty look at these associations doesn't reveal much.
# What is a better way to assess probability that a species will be associated 
# with one factor more than the other?'
