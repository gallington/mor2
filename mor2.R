library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
mor2<- read.spss("./data/ALL_EcolHHOrgMerged_byUvuljaa_03_15_17_Simplified.sav" , 
                 to.data.frame=TRUE,
                 use.value.labels=FALSE) # should this be true?
                  # 
# tidy format
tbl_df(mor2)

# tip!  use grep() to query index info based on column name:
grep("B", colnames(df))


#
# ECOLOGICAL VARS for CORR MATRIX:
#subset out the ecological vars of interest
ecol<- mor2[,1515:1538]
ecol <- select(ecol, -10)
ecol <- select(ecol, -9)
#ecol <- select(ecol, -CrudeProtein_percent_Mean500_1000)
#shorten the variable names
names(ecol) = sub(pattern = "_Mean500_1000", replacement = "", x = names(ecol))
# create cor matrix
cor.ecol<- cor(ecol)
# plot it
corrplot(cor.ecol, method="ellipse", tl.cex = 0.5, type = "upper") #tl.pos = "td",
corrplot.mixed(cor.ecol, tl.cex = 0.5)
corrplot(cor.ecol, order="hclust", addrect=2, tl.cex = 0.5)

# Do similar to above, but w data subdivided by ecological group:
# EcologicalZone_4Name or Code
#DS<- mor2 %>% group_by("Ecologicalzone_4Code100")   # 1339

# DESERT STEPPE    n= 33
DS <- mor2 %>% filter(Ecologicalzone_4Code100 == 1)
DS.ecol<- DS[,1515:1538] %>% 
  select(-AcidDetergentFiber_percent_Mean500_1000) %>%
  select(-CrudeProtein_percent_Mean500_1000)
#shorten the variable names
names(DS.ecol) = sub(pattern = "_Mean500_1000", replacement = "", x = names(DS.ecol))
# create cor matrix
cor.DSecol<- cor(DS.ecol)
# plot it
corrplot(cor.DSecol, method="ellipse", tl.cex = 0.75, type = "upper") #tl.pos = "td",
corrplot.mixed(cor.DSecol, tl.cex = 0.5)
corrplot(cor.DSecol, order="hclust", addrect=2, tl.cex = 0.5)

# STEPPE   n= 39
ST <- mor2 %>% filter(Ecologicalzone_4Code100 == 2)
ST.ecol<- ST[,1515:1538] %>% 
  select(-AcidDetergentFiber_percent_Mean500_1000) %>%
  select(-CrudeProtein_percent_Mean500_1000)
#shorten the variable names
names(ST.ecol) = sub(pattern = "_Mean500_1000", replacement = "", x = names(ST.ecol))
# create cor matrix
cor.STecol<- cor(ST.ecol)
# plot it
corrplot(cor.STecol, method="ellipse", tl.cex = 0.75, type = "upper") #tl.pos = "td",
corrplot.mixed(cor.STecol, tl.cex = 0.5)
corrplot(cor.STecol, order="hclust", addrect=2, tl.cex = 0.5)

# EASTERN STEPPE    n= 9
ES <- mor2 %>% filter(Ecologicalzone_4Code100 == 3)
ES.ecol<- ES[,1515:1538] %>% 
  select(-AcidDetergentFiber_percent_Mean500_1000) %>%
  select(-CrudeProtein_percent_Mean500_1000) %>%
  select(-Acnath_gm2_Mean500_1000) %>%
  select(-AnGrassCover_percent_Mean500_1000)
#shorten the variable names
names(ES.ecol) = sub(pattern = "_Mean500_1000", replacement = "", x = names(ES.ecol))
# create cor matrix
cor.ESecol<- cor(ES.ecol)
# plot it
corrplot(cor.ESecol, method="ellipse", tl.cex = 0.75, type = "upper") #tl.pos = "td",
#corrplot.mixed(cor.ESecol, tl.cex = 0.5)
corrplot(cor.ESecol, order="hclust", addrect=2, tl.cex = 0.5)


# FOREST MTN STEPPE:    n= 49
FMS <- mor2 %>% filter(Ecologicalzone_4Code100 == 4)
FMS.ecol<- FMS[,1515:1538] %>% 
  select(-AcidDetergentFiber_percent_Mean500_1000) %>%
  select(-CrudeProtein_percent_Mean500_1000) %>%
  select(-Acnath_gm2_Mean500_1000)
#shorten the variable names
names(FMS.ecol) = sub(pattern = "_Mean500_1000", replacement = "", x = names(FMS.ecol))
# create cor matrix
cor.FMSecol<- cor(FMS.ecol)
# plot it
corrplot(cor.FMSecol, method="ellipse", tl.cex = 0.75, type = "upper") #tl.pos = "td",
#corrplot.mixed(cor.FMSecol, tl.cex = 0.5)
corrplot(cor.FMSecol, order="hclust", addrect=2, tl.cex = 0.5)

# PRACTICES + ECOL DATA FOR CFA:
#













# 
hist(mor2$CBRM_type)

hist(mor2$SpeciesRichnessMean500_1000)
hist(mor2$PerGrassCover_percent_Mean500_1000)


sr<- select(mor2, SocialSurveyReferenceNumber,
                  SpeciesRichnessMean500_1000,
                  PerGrassCover_percent_Mean500_1000)
 
# sr x cover x cbrm
x<- mor2 %>% 
  select(q04_CBRM_Y_N, SpeciesRichnessMean500_1000 , PerGrassCover_percent_Mean500_1000)


y<- ggplot(x, aes(x=SpeciesRichnessMean500_1000, 
                  y=PerGrassCover_percent_Mean500_1000, 
                  shape=as.factor(q04_CBRM_Y_N)))+
  geom_point(aes(color=as.factor(q04_CBRM_Y_N)))

boxplot(SpeciesRichnessMean500_1000 ~ q04_CBRM_Y_N, data= x)
