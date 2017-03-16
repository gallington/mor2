library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
mor2<- read.spss("./data/ALL_EcolHHOrgMerged_byUvuljaa_03_15_17_Simplified.sav" , 
                 to.data.frame=TRUE,
                 use.value.labels=FALSE) # should this be true?
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
  select(CBRM_type, SpeciesRichnessMean500_1000 , PerGrassCover_percent_Mean500_1000)

y<- ggplot(x, aes(x=SpeciesRichnessMean500_1000, 
                  y=PerGrassCover_percent_Mean500_1000, 
                  shape=as.factor(CBRM_type)))+
  geom_point(aes(color=as.factor(CBRM_type)))

boxplot(SpeciesRichnessMean500_1000 ~ CBRM_type, data= x)
