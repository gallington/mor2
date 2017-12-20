
# Packages: ------------
library(foreign)
library(corrplot)
library(car)
library(ltm)
library(lavaan)
library(psych)
library(semTools)
library(semPlot)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(sp)
library(readxl)
library(knitr)

# DATA ---------------
mor2<- foreign::read.spss("./data/ALL_EcolHHOrgMerged_byUvuljaa_03_15_17_Simplified.sav" , 
                          to.data.frame=TRUE,
                          use.value.labels=FALSE) #
# tidy format
tbl_df(mor2)


eco.vars500 <- dplyr :: select(mor2, 
                              # identifiers for linking w other datasets:
                              RefNum = SocialSurveyReferenceNumber, 
                              PlotID = UniquePlotID500,  #this one incorps the Uvuulja and site
                              Lat = Latitude500, # bc need to match later at 500
                              Long= Longitude500,
                              # grasses:
                              PerrGrass = PerGrassCover500,
                              #Grass_bm = Grass_gm2_500, # eliminated in earlier analysis
                              # AnuGrass = AnGrassCover_percent_Mean500_1000,  #  not enough of this to use it. Mostly zeros....
                              #TotGrassCover = TotalGrassCover500,
                              # Acnath = Acnath_gm2_Mean500_1000,  # almost allll zeros
                              # forbs:
                              PerrForb = PerForbCover500,
                              # AnuForb = AnForbCover500, # rm in earlier analsis
                              #Forb_bm = Forb_gm2_500, # rm in earlier analysis
                              # sedges:
                              Sedges = TotalSedgeCover500,
                              # shrubs:
                              Shrubs = TotalShrubCover500,
                              #TotFoliar = TotalFoliarCover500, #rm in earlier analysis
                              # BasalCover = BasalCover_percent_Mean500_1000,   # something is wrong w this variable. values are huge and tiny.
                              # abiotic params:
                              BareSoil = BareSoilCover500,
                              LitterCover = LitterCover500,
                              #Protein = CrudeProtein_percent_500, # rm in earlier analysis
                              # species richness:
                              SpRich = SpeciesRichness500
)

eco.vars1k <- dplyr :: select(mor2, 
                               # identifiers for linking w other datasets:
                               RefNum = SocialSurveyReferenceNumber, 
                               PlotID = UniquePlotID1000,  #this one incorps the Uvuulja and site
                               Lat = Latitude1000, # bc need to match later at 1000
                               Long= Longitude1000,
                               # grasses:
                               PerrGrass = PerGrassCover1000,
                               #Grass_bm = Grass_gm2_1000, # eliminated in earlier analysis
                               # AnuGrass = AnGrassCover_percent_Mean1000_1000,  #  not enough of this to use it. Mostly zeros....
                               # TotGrassCover = TotalGrassCover1000,
                               # Acnath = Acnath_gm2_Mean1000_1000,  # almost allll zeros
                               # forbs:
                               PerrForb = PerForbCover1000,
                               # AnuForb = AnForbCover1000, # rm in earlier analsis
                               #Forb_bm = Forb_gm2_1000, # rm in earlier analysis
                               # sedges:
                               Sedges = TotalSedgeCover1000,
                               # shrubs:
                               Shrubs = TotalShrubCover1000,
                               #TotFoliar = TotalFoliarCover1000, #rm in earlier analysis
                               # BasalCover = BasalCover_percent_Mean1000_1000,   # something is wrong w this variable. values are huge and tiny.
                               # abiotic params:
                               BareSoil = BareSoilCover1000,
                               LitterCover = LitterCover1000,
                               #Protein = CrudeProtein_percent_1000, # rm in earlier analysis
                               # species richness:
                               SpRich = SpeciesRichness1000
)
# add in the soils parameters

#library(readxl)
soils <- read_excel("/nfs/gallington-data/Herder_sem/data/Soil surface data_Ginger.xlsx", 
                    sheet = "Sheet1")
#View(soils)
soils %<>% mutate_at(9:18, funs(factor(.)))

# do these need to be flipped? so best condition is highest?
erosion.levels<- c("high erosion", "medium erosion", "little erosion")
retention.levels <- c("isolated", "fragmented", "connected")
soils$`SRD_3classes_with name` <- ordered(soils$`SRD_3classes_with name`, levels = erosion.levels)
soils$`RRC_3classes_with name` <- ordered(soils$`RRC_3classes_with name`, levels = retention.levels)
#  
soils500<- soils %>% filter(DistanceClass_m == 500) %>%
  dplyr::select(PlotID ='UniquePlotID', RRC=`RRC_3classes_with name`, SRD=`SRD_3classes_with name`)
#
soils1000<- soils %>% filter(DistanceClass_m == 1000) %>%
  dplyr::select(PlotID ='UniquePlotID', RRC=`RRC_3classes_with name`, SRD=`SRD_3classes_with name`)
# now add the soils  

eco.vars500 %<>% left_join(soils500, by = "PlotID")
eco.vars500 <- mutate(eco.vars500, Bare.inv = (sqrt(100 - BareSoil))) %>% dplyr::select(-BareSoil)


eco.vars1k %<>% left_join(soils1000, by = "PlotID")
eco.vars1k <- mutate(eco.vars1k, Bare.inv = (sqrt(100 - BareSoil))) %>% dplyr::select(-BareSoil)

# not using protein anymore...
#eco.params$Protein[eco.params$Protein == "NA"] <- mean(eco.params$Protein)

# TRANSFORMED DATA ***********************************************************
# NEED TO TRY THIS WITH ARCSINE AND LOG TRANSFORMED DATA?
asinTransform <- function(p) { asin(sqrt(p/100)) }
eco500as<- eco.vars500 %>% mutate_at(c(5:9, 13), funs(asinTransform(.))) # this step not working right bc 0s
eco500lt<- eco.vars500 %>% mutate_at(c(5:9, 13), funs(log(1+.)))

eco1kas<- eco.vars1k %>% mutate_at(c(5:9, 13), funs(asinTransform(.))) # this step not working right bc 0s
eco1klt<- eco.vars1k %>% mutate_at(c(5:9, 13), funs(log(1+.)))

# ******************************************************************************

# ASSES FACTORABILITY-------------------
# Checking the correlation matrix and sampling adequacy
#specify which cols (usually all):
cols<- c(5:13) 
# function to get cor matrix
corall <- function(df){
  ecol<- dplyr::select(df, c(cols))
  ecor <- hetcor(ecol)$cor
  corrplot(ecor)
}

# create a list of the dataframes:
dflist<- list(eco500as, eco500lt, eco1kas, eco1klt)

# for(i in 1:length(dflist)){
#   cplots<- corall[i]
#   return(cplots)
# }

# get a list of cor matrices
corlist<- lapply(dflist, corall)

# check sampling adequacy
# k<- KMO(ecor)
kmolist<- lapply(corlist, KMO)

# factor analysis:
fa2factor <- function(cormat){
              factanal(~PerrGrass+ PerrForb + Sedges + Shrubs +Bare.inv + LitterCover + SpRich + RRC+ SRD,
              covmat = cormat, n.obs = nrow(eco.vars),
              factors = 2, rotation = "varimax", fm = "ml")#$loadings
              }

factorlist<- lapply(corlist, fa2factor) 
# things to consider:
# looks like shurbs and perr forbs might be performing differently when divided out this way
# at 100k shrubs maybe on ly on eco1?
# at 500m forbs unclear?



# Measurement models (CFA)
twofact.mod <- 'eco1 =~ PerrGrass + LitterCover +RRC + SRD + Bare.inv + Shrubs
                eco2 =~ PerrForb +Sedges + SpRich + Shrubs
                '

#   fit the model
#fit.ecol.mod <- cfa(twofact.mod, data= eco.vars, std.lv = TRUE)
fit.list<- function(dfs){
            cfa(twofact.mod, data= dfs, std.lv = TRUE)
            }
# list of fitted models
fit.mod.list <- lapply(dflist, fit.list)

# SUMMARY OF 500m, arcsine transformed:
summary(fit.mod.list[[1]], 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
# SUMMARY of 500m log transformed
summary(fit.mod.list[[2]], 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
# Summary of 1000m arcsine transformed:
summary(fit.mod.list[[3]], 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)
# Summary of 1000m arcsine transformed:
summary(fit.mod.list[[4]], 
        rsquare = T, 
        standardized = T, 
        fit.measures=T)

lapply(fit.mod.list, modificationindices)
#modificationindices(fit.ecol.mod, sort. = TRUE)[1:6,]

# predicting latent values:
eco2.pred <- function(fitmods){
                lavPredict(fitmods, type = "lv", label = TRUE)
}

# creates a list of dfs of predicted vals for the two latents:
pred.list<- lapply(fit.mod.list, eco2.pred)
# filenum<- seq(1,4)
# for(i in 1:length(pred.list)){
#     for(j in 1:length(filenum)){
#   saveRDS(pred.list[i], file= "./data/eco.pred.TwoLatents[j].RDS")
# }}

#saveRDS(pred.list[2], file= "./data/eco.latent.500.RDS")

# subset out the df from the lists
# log transformed: 
ecolatent500<- as.data.frame(pred.list[2])
ecolatent1k<- as.data.frame(pred.list[4])


# rescale from 0-1:
eco500.rs<- ecolatent500 %>% 
  na.omit() %>%
  mutate(eco1.rs = scales::rescale(eco1, to=c(0, 1))) %>%  
  mutate(eco2.rs = scales ::rescale(eco2, to=c(0,1)))


eco1k.rs<- ecolatent1k %>% 
 # na.omit() %>%
  mutate(eco1.rs = scales::rescale(eco1, to=c(0, 1))) %>%  
  mutate(eco2.rs = scales ::rescale(eco2, to=c(0,1)))



# add to the identifying info
  # remove the NAs from eco.vas1k:
eco.vars1k<- filter(eco.vars1k, RRC!= "NA")
# log transformed:
finaleco500<- cbind(eco500.rs, eco.vars500[,1:2])
finaleco1k<- cbind(eco1k.rs, eco.vars1k[,1:2])

write.csv(finaleco500, file= "./data/eco.latent.500.csv")
write.csv(finaleco1k, file= "./data/eco.latent.1000.csv")

# ******************************************
# Plots -------------------------

plot.cfas<- function(fit.models){
  semPaths(fit.models, 
         what = "path",
         whatLabels ="std", 
         edge.label.cex = 0.75, 
         layout = "tree", 
         residuals = TRUE,
         esize = 2
         )}
lapply(fit.mod.list, plot.cfas)
#title("Two Factor Model Fit", line=1)


eco1<- "The Resource Retention Factor (F1ResRet)" 
eco2<- "The Functional Diversity Factor (F2FunctDiv)" 


#************************************
# Matching w the 2 distances: --------------
library(MatchIt)
finaleco500%<>%mutate_at(5, funs(factor(.)))
#finaleco1k%<>%mutate_at(5, funs((.))) # this has 125 not 130
raw.data$RefNum<-as.numeric(raw.data$RefNum)

#***********
# 500m data: -------------------------
#***********
chnt.vars<- dplyr::select(raw.data, 1:33)%>%left_join(finaleco500, by= "RefNum")
chnt1kvars<- dplyr::select(raw.data, 1:33)%>% left_join(finaleco1k, by = "RefNum")
chnt1kvars%<>%mutate_at(6, funs(factor(.))) 

# 500 m
m.chnt <- matchit(CBRM ~ 
                    ez1_DS +
                    ez2_St +
                    ez3_ES +
                    ez4_FMS +
                    #AimagName +
                    #SoumName+
                    ppt99 +  # mean precip up to 1999
                    #ppt99sd+
                    sfu99,  # mean sfu up to 1999
                  data = chnt.vars, 
                  #method = "full", distance = "logit",
                  method = "full", distance = "probit",
                  discard = "both"
)
mchnt.summary<- summary(m.chnt, 
                         standardize = TRUE  
) 
mchnt.summary              

# 1000m
m.chnt1k <- matchit(CBRM ~ 
                    ez1_DS +
                    ez2_St +
                    ez3_ES +
                    ez4_FMS +
                    #AimagName +
                    #SoumName+
                    ppt99 +  # mean precip up to 1999
                    #ppt99sd+
                    sfu99,  # mean sfu up to 1999
                  data = chnt1kvars, 
                  #method = "full", distance = "logit",
                  method = "full", distance = "probit",
                  discard = "both"
)
mchnt1k.summary<- summary(m.chnt1k, 
                        standardize = TRUE  
) 
mchnt1k.summary

# extract the matched data
matched1k.chnt <- match.data(m.chnt1k)

# 
love.plot(bal.tab(m.chnt1k), 
              stat = "mean.diffs",  # c("mean.diffs", "variance.ratios", "ks.statistics"), 
              # cant' get var ratios for binary vars... can't get this to work unless select one stat msr at a time.
              threshold = .2, 
              no.missing = TRUE,
              var.order = NULL/0) #,
              # colors = c("#998ec3","#f1a340"),
              #var.names = v)

# factor 1:
cbrm_0<-(matched.chnt$eco1.rs*matched.chnt$weights)[which(matched.chnt$CBRM==0)]
cbrm_0pre<-(matched.chnt$eco1.rs)[which(matched.chnt$CBRM==0)]
cbrm_1<-(matched.chnt$eco1.rs*matched.chnt$weights)[which(matched.chnt$CBRM==1)]
cbrm_1pre<-(matched.chnt$eco1.rs)[which(matched.chnt$CBRM==1)]
plot(ecdf(cbrm_0))
lines(ecdf(cbrm_1), col = 'blue')

ks.test(cbrm_0, cbrm_1)
t.test(cbrm_0, cbrm_1)
ks.test(cbrm_0pre, cbrm_1pre)
t.test(cbrm_0pre, cbrm_1pre)

median(cbrm_0)

# factor 2
cbrm_02<-(matched.chnt$eco2.rs*matched.chnt$weights)[which(matched.chnt$CBRM==0)]
cbrm_0pre2<-(matched.chnt$eco2.rs)[which(matched.chnt$CBRM==0)]
cbrm_12<-(matched.chnt$eco2.rs*matched.chnt$weights)[which(matched.chnt$CBRM==1)]
cbrm_1pre2<-(matched.chnt$eco2.rs)[which(matched.chnt$CBRM==1)]

# factor 2
ks.test(cbrm_02, cbrm_12)  # distributions not signif diff.


#***********************************************************

# Density plots

#**********************************************************

# Set plotting params:
height = 3
width =3
pal1 = "Dark2" # palette for density plots
line1 = "grey35"  # color for Ctrl abline
line2 = "grey25"  # color for Trt abline

#*************************************
# Latent factor 1 : Resource Retention
#*************************************
ggplot(matched.chnt, aes(x=(eco1.rs), colour=CBRM)) + 
  geom_density()+ 
  scale_color_brewer(type = 'div', palette = pal1)+
 # theme(legend.position="none") +
  geom_vline(aes(xintercept = median(cbrm_0pre)), # control
             color = line1, linetype = 3)+
  geom_vline(aes(xintercept = median(cbrm_1pre)), 
             color = line1, linetype = 2)+      # treatment = smaller dashed
  labs(title= "CBRM Treatment Effect\npre-matching data", 
       x= "Resource Retention (Factor 1)")

#ggsave("./plots/cb1pre.png", height = height, width = width)

ggplot(matched.chnt, aes(x=(eco1.rs *weights), colour=CBRM)) + 
  geom_density()+ 
  xlim(0, 3)+
  scale_color_brewer(type = 'div', palette = pal1)+
 #theme(legend.position="none") +
  geom_vline(aes(xintercept = median(cbrm_0)), 
             color = line1, linetype = 3)+
  geom_vline(aes(xintercept = median(cbrm_1)), 
             color = line2, linetype = 2)+
  labs(title="CBRM Treatment Effect\npost-matched data", x= "Resource Retention (Factor 1)")
#ggsave("./plots/cb1post.png", height = height, width = width)

#Trying to combine:
ggplot(matched.chnt) + 
  geom_density(aes(x=(eco1.rs), colour=CBRM), linetype = 3)+ 
  geom_density(aes(x=(eco1.rs *weights), colour=CBRM))+
  xlim(0, 3)+
  scale_color_brewer(type = 'div', palette = pal1)+
  geom_vline(aes(xintercept = 0.63), color = line1, linetype = 2)+
  geom_vline(aes(xintercept = 0.69), color = line2, linetype = 3)+
  labs(title= "CBRM Treatment Effect\npre-matching data", x= "Resource Retention (Factor 1)")

#*************************************
# Latent factor 2 : Functional Diversity
#*************************************
ggplot(matched.chnt, aes(x=(eco2.rs), colour=CBRM)) + 
  geom_density()+ 
  scale_color_brewer(type = 'div', palette = pal1)+
  # theme(legend.position="none") +
  geom_vline(aes(xintercept = median(cbrm_0pre2)), # control
             color = line1, linetype = 3)+
  geom_vline(aes(xintercept = median(cbrm_1pre2)), 
             color = line1, linetype = 2)+      # treatment = smaller dashed
  labs(title= "CBRM Treatment Effect\npre-matching data", 
       x= "Functional Diversity (Factor 2)")

#ggsave("./plots/cb1pre.png", height = height, width = width)

ggplot(matched.chnt, aes(x=(eco2.rs *weights), colour=CBRM)) + 
  geom_density()+ 
  xlim(0, 3)+
  scale_color_brewer(type = 'div', palette = pal1)+
  #theme(legend.position="none") +
  geom_vline(aes(xintercept = median(cbrm_02)), 
             color = line1, linetype = 3)+
  geom_vline(aes(xintercept = median(cbrm_12)), 
             color = line2, linetype = 2)+
  labs(title="CBRM Treatment Effect\npost-matched data", 
       x= "Functional Diversity (Factor 2)")
#ggsave("./plots/cb1post.png", height = height, width = width)

#####################################
#### 1000m ---------------------------
#####################################

# factor 1:
cbrm1k_0<-(matched.chnt$eco1.rs*matched.chnt$weights)[which(matched.chnt$CBRM==0)]
cbrm1k_0pre<-(matched.chnt$eco1.rs)[which(matched.chnt$CBRM==0)]
cbrm1k_1<-(matched.chnt$eco1.rs*matched.chnt$weights)[which(matched.chnt$CBRM==1)]
cbrm1k_1pre<-(matched.chnt$eco1.rs)[which(matched.chnt$CBRM==1)]
plot(ecdf(cbrm1k_0))
lines(ecdf(cbrm1k_1), col = 'blue')

ks.test(cbrm_0, cbrm_1)
t.test(cbrm_0, cbrm_1)
ks.test(cbrm_0pre, cbrm_1pre)
t.test(cbrm_0pre, cbrm_1pre)

median(cbrm_0)

# factor 2
cbrm_02<-(matched.chnt$eco2.rs*matched.chnt$weights)[which(matched.chnt$CBRM==0)]
cbrm_0pre2<-(matched.chnt$eco2.rs)[which(matched.chnt$CBRM==0)]
cbrm_12<-(matched.chnt$eco2.rs*matched.chnt$weights)[which(matched.chnt$CBRM==1)]
cbrm_1pre2<-(matched.chnt$eco2.rs)[which(matched.chnt$CBRM==1)]

# factor 2
ks.test(cbrm_02, cbrm_12)  # distributions not signif diff.


#***********************************************************

# Density plots