# CFA test setup

# define / rename variables to simplify later:


# specify the model 1
cfa.model1 <- ' org  =~ x1 + x2 + x3      
               rules =~ x4 + x5 + x6
               practice =~ x7 + x8 + x9 '

# fit the model
fit.cfa <- cfa(cfa.model, data=mor2)  # or specify the subset data matrix
 
# display summary output
summary(fit.cfa, fit.measures=TRUE)


# specify the model 2 
# Practice -> Pasture Conditions
# > names(ES.ecol)
# [1] "SpeciesRichnessMean500_1000" "Grass_gm2"                  
# [3] "Forb_gm2"                    "Shrub_gm2"                  
# [5] "Sedge_gm2"                   "Litter_gm2"                 
# [7] "StDead_gm2"                  "AnForbCover_percent"        
# [9] "AnPlantCover_percent"        "PerGrassCover_percent"      
# [11] "PerForbCover_percent"        "PerPlantCover_percent"      
# [13] "TotalGrassCover_percent"     "TotalForbCover_percent"     
# [15] "TotalSedgeCover_percent"     "TotalShrubCover_percent"    
# [17] "TotalFoliarCover_percent"    "BareSoil_percent"           
# [19] "BasalCover_percent"          "LitterCover_percent"   
# from mor2:
# [125 "AvgDist2010"                             "TotDist2010"                            
# [127] "MovedOutside"                            "TakeLivestock"                          
# [129] "a_ResWint"                               "b_ResSpr"                               
# [131] "c_ResDzud"                               "d_FallOtor"                             
# [133] "d_FallOtor_ifyes"                        "e_WtrOtor"                              
# [135] "e_WtrOtor_ifyes"                         "f_GrzWtr_SumFall"                       
# [137] "g_GrzSpr_SumFall"                        "h_GrzDzud_NonEmrg"      

# PRACTICES :
# herd.size
# p1 <- "Total2010SFU" # [605]
#distance travelled
# p2 <- "AvgDist2010"  # [125]           
# p3<- "TotDist2010" [126]                 
# p4 <- "d_FallOtor" # [132]
# p5 <- "e_WtrOtor"  # [134]
# reserving pastures
# p6 <- "a_ResWint"  # [129:131]
# p7 <- "b_ResSpr" #
# p8 <- "c_ResDzud"
# out of season use:
# p9 <- "f_GrzWtr_SumFall"  # [136]
# p10 <- "h_GrzDzud_NonEmrg" # [138]
p.cols <- c(605, 125,126,129:132, 134, 136, 138)
practices<- mor2[,p.cols]
  # ECOLOGICAL INDICATORS  
  # rename columns to 
# e1 <-  "PerGrassCover_percent" # [1528]
# e2 <-  "PerForbCover_percent" # [1529]
# e3 <-  "BareSoil_percent"  # [1536]
# e4 <-  "LitterCover_percent" # [1538]

e.cols <- c(1528, 1529, 1536, 1538)
ecol.ind<- mor2[, e.cols]
prac.ecol<- cbind(practices, ecol.ind)
# rename columns
names(prac.ecol) <- c("p1","p2","p3", "p4", "p5","p6", "p7", "p8", "p9", "p10", "e1","e2", "e3", "e4")
# 

# SCALE THE DATA?? 


cfa.model2 <- ' practice =~ p1 + p2 + p3 + p4 # + p5 + p6 +p7 +p8 +p9 + p10
                ecol.ind =~ e1 + e2 + e3 + e4'

# fit the model 2
fit.cfa2 <- cfa(cfa.model2, data= prac.ecol)


# 
# 

