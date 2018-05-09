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

# bare X 