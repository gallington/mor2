
# Code to pull in & tidy Data from Jay Angerer on 
#   soum level ndvi, forage use, spei and precip
#   most of these are yearly average per soum.

library(readxl)
library(dplyr)
library(magrittr)
library(reshape2)
library(tidyr)
# Import raw files--------------------------------------------------------------------
# THE DATA THAT NEEDS TO BE IMPORTED AND JOINED:
# 
#   these are soum-level data:
# Precip mean & sd
j.precip<- read.csv("./data/mor2data/from_Jay/cpc_rain_soum_means_stdev.csv")
tbl_df(j.precip)
j.precip %<>% mutate_at(1:2, funs(as.character(.)))

# Forage Availability from modis kg/ha
#   this just pulls in the first sheet, which has averages
#   the third sheet has stdev
j.forage.avail<- read_excel("./data/mor2data/from_Jay/Forage_available_modisv6_average_stdev_kg_ha.xlsx", sheet = 2)
tbl_df(j.forage.avail) %>% mutate_at(1:2, funs(as.character(.)))

# Sheep Forage Units 
j.sfu <- read_excel("./data/mor2data/from_Jay/sheep_units_per_ha_soum.xlsx", sheet = 1)
tbl_df(j.sfu) %>% mutate_at(1:2, funs(as.character(.)))




# Info on fixing the names to match MOR2 ---------------------------------------------
#   need to match these on Soum ID, Aimag + Soum_name
# 
# soum level :
#   NOTE: MANUALLY CORRECTED # TYPOS IN THIS .csv FILE on 9/10/17
#   bc REALIZED THEY WERE NOT MATCHING THE MOR2 DATABASE
soums <- read.csv("./data/mor2data/from_Jay/soum_names.csv", stringsAsFactors = FALSE)
soums %<>% rename(Soum_name = jay)
soums$MOR2match<- trimws(soums$MOR2match)  #trim trailing white space

# aimag level :
aimags <- read.csv("./data/from_Jay/aimag_names.csv", stringsAsFactors = FALSE)


# Tidying jay's data and matching in correct names:------------------------------
# 
# ********* STILL NEED TO CORRECT THE YEAR RANGE ON THESE TO CUT AT 2010 or 2011...
#   years = 1979 - 2016
ppt.mean <- j.precip[,1:43] %>% 
  gather("Year", "ppt_mean", 6:43) %>%
  separate (Year, c("x1", "x2", "x3", "year"), sep="_", remove = TRUE) %>%
  dplyr::select(-x1, -x2, -x3) %>%
  left_join(soums, by = "Soum_name") #%>%
  # left_join(aimags, by = "Aimag_name")
ppt.mean$year<- as.integer(ppt.mean$year)    # this still has data for all years


ppt.sd<- j.precip[,c(1:3, 44:81)]%>%
  gather("Year", "ppt_sd", 4:41)%>%
  separate(Year, c("y1", "y2", "y3", "year"), sep="_", remove = TRUE) %>%
  dplyr::select(-y1, -y2, -y3) # %>%
# left_join(soums, by = "Soum_name") %>%
# left_join(aimags, by = "Aimag_name")
ppt.sd$year<- as.integer(ppt.sd$year)        # this still has data for all years

# put mean and sd ppt together in to one df and fix col headers
soum.stats<- left_join(ppt.mean, ppt.sd, by = c("Aimag_name", "Soum_name", "Soum_ID", "year"))%>%
  left_join(soums, by = "Soum_name") %>% # this matches by the common field "Soum_name" but only retains the correct field from soums --- doesn' tsuplicate Soum_name field
  left_join(aimags, by = "Aimag_name")
soum.stats<- soum.stats[,c(3,5:10)]
soum.stats %<>% rename(SoumName = MOR2match, AimagName = aimag_match) 

# now need to calc a summary, mean across all years by SoumID <- this is why we've kept SoumID around
lt.ppt <- soum.stats %>% tbl_df() %>%
  group_by(year) %>% 
  group_by(AimagName, SoumName, Soum_ID) %>% # if put all these together they will appear in the output
  summarise(ltmeanppt = mean(ppt_mean), ltsdppt = mean(ppt_sd))

# precip through 1999, to use for matching for CBRM
ppt.99 <- soum.stats %>% tbl_df() %>%
  filter(year <= 1999) %>%
  group_by(year) %>% 
  group_by(AimagName, SoumName, Soum_ID) %>% # if put all these together they will appear in the output
  summarise(ppt99 = mean(ppt_mean), ppt99sd = mean(ppt_sd))

# STILL NEED TO FIGURE OUT HOW TO EXTRACT THE COEFFICIENTS 
# s<-soum.stats %>% 
#   filter(year <= 1999) %>%
#   group_by(AimagName, SoumName, Soum_ID) %>%
#   do(model = lm(ppt_mean ~ year, data = .)) #%>%
#   #mutate(coef=coef(model)["wl"])  
# 
# s$model[1]
#ppt.99slope<- lm(data = filter(soum.stats, year <= 1999), formula = ppt_mean ~ year)




ks.test(lt.ppt$ltmeanppt, ppt.99$ppt99)
ks.test(lt.ppt$ltsdppt, ppt.11$ppt11sd)
# distributions not diff
t.test(lt.ppt$ltmeanppt, ppt.11$ppt11)
t.test(lt.ppt$ltsdppt, ppt.11$ppt11sd)
# also not diff

#   change this back to a df to eliminate all of the stored info on grouping that messes things up later.
#lt.ppt <- as.data.frame(lt.ppt)

# Stocking rate: ----------------
#   sfu/ha
#   sfu has different year range than the ppt data so for now am treating it separately in case want entire range of this one later..
#   but still need to figure out correct place to cut off for this one too
#   years = 1981 - 2016
sfu <- j.sfu %>% 
  gather("Year", "sfu", 6: 41) %>%    # per hectare
  separate(Year, c("x1", "x2"), sep = "_", remove = TRUE) %>%
  separate(x1, c("x3", "year"), sep = "SU", remove = TRUE) %>%
  dplyr::select(Aimag_name, Soum_name, Soum_ID, year, sfu) %>%
  left_join(soums, by = "Soum_name") %>% # this matches by the common field "Soum_name" but only retains the correct field from soums --- doesn' tsuplicate Soum_name field
  left_join(aimags, by = "Aimag_name")
sfu %<>% rename(SoumName = MOR2match, AimagName = aimag_match) 

sfu$year <- trimws(sfu$year)
sfu$year <- as.integer(sfu$year)            # this still has data for all years
sfu$Soum_ID <- as.integer(sfu$Soum_ID)  

# Calc a summary across all years for each SoumID
#   this is why we've kept SoumID around
#   ******** and add stdev as well   *********
lt.sfu <- sfu %>% 
  group_by(year) %>%
  group_by(AimagName, SoumName, Soum_ID) %>% # if put all these together they will appear in the output)
  summarise(ltmeansfu = mean(sfu))
#lt.sfu<- as.data.frame(lt.sfu)
# mean thru 1999:
sfu.99 <- sfu %>% 
  filter(year <= 1999) %>%
  group_by(year) %>%
  group_by(AimagName, SoumName, Soum_ID) %>% # if put all these together they will appear in the output)
  summarise(sfu99 = mean(sfu))

# quick check of the distributions and means shows they are not actually different...
ks.test(sfu.99$sfu99, lt.sfu$ltmeansfu)
t.test(sfu.99$sfu99, lt.sfu$ltmeansfu)


# Available Forage: 
#   2000-2011
frg.avail <- j.forage.avail %>% 
  gather("Year", "biomass", 6:17) %>%
  separate(Year, c("x1", "x2", "year"), sep = "_", remove = TRUE) %>%
  dplyr::select(Aimag_name, Soum_name, Soum_ID, year, biomass) %>%
  left_join(soums, by = "Soum_name") %>%
  left_join(aimags, by = "Aimag_name") %>%
  rename(SoumName = MOR2match, AimagName = aimag_match) 
frg.avail$year<- as.integer(frg.avail$year)
frg.avail<- frg.avail[,3:7]

# now calc mean across all years by SoumID
frg.avg <- frg.avail %>%
  group_by(year) %>%
  group_by(AimagName, SoumName, Soum_ID) %>% # if put all these together they will appear in the output)
  summarise(avail.forage = mean(biomass))
# frg.avg<- as.data.frame(frg.avg)



######## Forage Use: #################
  # table that (supposedly) links mor2 names w jay's names
soums <- read.csv("./data/mor2data/from_Jay/soum_names.csv", stringsAsFactors = FALSE)
soums %<>% rename(Soum_name = jay)
soums$MOR2match<- trimws(soums$MOR2match)  #trim trailing white space
  # table of the aimag and soum name pairs from jay--so can 
  # distinguish the soums w same name-diff aimag 
jas <- read.csv("./data/mor2data/from_Jay/j_aimag_soum.csv", stringsAsFactors = FALSE)

  # now this joins them so that we have the aimag and soums 
aimag.soum<- jas %>% left_join(soums, by = "Soum_name") %>% arrange(Aimag_name, Soum_name)
  # but we don't have the mor2 aimag matches...
mor2as<- as.data.frame(td[, 20:21] %>% distinct() %>% arrange(Aimag,Soum))
 # yeah the aimag names don't match damn
#df$depth[df$depth<10] <- 0  # one option for reassigning values

aimag.soum<- aimag.soum %>% mutate(Aimag =
                                     case_when(Aimag_name == "Arxangai" ~ "Arkhangai",
                                               Aimag_name == "Bayanxongor"~ "Bayankhongor",
                                               #Aimag== "Dornod", "Dornod",
                                               #Aimag== "Dornogovi"
                                               #Dundgovi
                                               Aimag_name == "O'mnogovi"~ "Umnugovi",
                                               Aimag_name== "O'vorxangai"~"Uvurkhangai",
                                               #Selenge
                                               Aimag_name == "Su'xbaatar"~ "Sukhbaatar",
                                               Aimag_name == "To'v" ~ "Tuv",
                                               TRUE ~ as.character(Aimag_name)))  
                                        

# add the CV vals to this:
j.forage.cv <- read.csv("./data/forageCV.csv")
names.cv<- aimag.soum %>% left_join(j.forage.cv, by = c("Aimag_name", "Soum_name"))

# Forage USE = (Forage Demand/Forage Available) 
  # see data files for metadata or call sheet = 1 below
j.forage.use <- read_excel("./data/mor2data/from_Jay/Forage_use_paired_soum_modisV6.xlsx", sheet = 2)
tbl_df(j.forage.use)
# select out the cols we want, including the forage use vals for 2010-2011:

j.forage.use <- j.forage.use[2:37,c(1:5, 16:17)]
colnames(j.forage.use)<- c("Aimag_name", "Soum_name", "Soum_ID", "Ecozone", "Poly_ID", "fu10", "fu11")
j.forage.use %<>% mutate_at(6:7, funs(as.numeric(.)))

# rename the colum from the soums name table imported above
#soums$Soum<- soums$Soum_name

# add the other cols now? 


#   2010-2011 avg
frg.use <- j.forage.use %>% 
  gather("Year", "pctFrgUse", 6:7) 
frg.use$Year<- as.factor(frg.use$Year)
frg.use$pctFrgUse<- as.numeric(frg.use$pctFrgUse)

frg.use<- frg.use[,c(1,2,4,6,7)]



# function to cal CV
#cv <- function(x) 100*( sd(x)/mean(x))
#j.forage.cv <- read.csv("./data/forageCV.csv")
#j.forage.cv$Aimag<- j.forage.cv$Aimag_name
#j.forage.cv$Soum<- j.forage.cv$Soum_name

#frg.use.a<- frg.use%>% left_join(j.forage.cv, by = c("Aimag", "Soum"))

# Need to summarize first bc now have three years of values for each soum
# now calc mean across 2010-2011 SoumID
frg.use.av <- frg.use %>%
  group_by(Aimag_name, Soum_name) %>% # if put all these together they will appear in the output
  summarise(frgUse = mean(pctFrgUse)) 
# save as a df to remove all of the tibble info hanging around
frg.use.av <- as.data.frame(frg.use.av)
#frg.use.av <- frg.use.av [,2:3]  # might need to keep Soum bc of duplicates...

frg.use.avcv<- names.cv %>% left_join(frg.use.av, by = c("Aimag_name", "Soum_name"))
#rename MOR2Match to match 
frg.use.avcv<- frg.use.avcv %>% rename(Soum = MOR2match)
#df$depth[df$depth<10] <- 0 
frg.use.avcv$Soum[frg.use.avcv$Soum == "Bat-Ulzii"] <- "Bat-Ulziit"


# combine with the td dataframe:
load("./data/td.RData")
td$Aimag <- as.character(td$Aimag)
td$Soum <- as.character(td$Soum)

x<- td%>% distinct(Aimag, Soum) %>% arrange(Aimag, Soum)
y<- frg.use.avcv %>% distinct(Aimag, Soum) %>% arrange(Aimag, Soum)
x == y  # to find the ones that aren't lining up.... 
#x[c(27:29,33),]
#y[c(27:29,33),]

td.fg <- td %>% inner_join(frg.use.avcv, by = c("Aimag", "Soum"))
td.fg %<>% mutate(frg.left = (100-frgUse))
#------
#Don't need this anymore, foudn a better way to do it above:
# Had to do it this way bc the line above was generating extra lines
# and I can't figure out why : 
td.fg<- td%>% mutate(frg.avg = 
                       case_when(Soum == "Ikh Tamir" ~ "48.09660",
                     Soum == "Jargalant" ~  "43.11589",
                     Soum == "Tariat" ~  "46.72256",
                     Soum == "Undur-Ulaan" ~  "47.62593",
                     Soum == "Bayan-Ovoo" ~  "80.30450",
                     Soum == "Bayan-Undur" ~  "25.66259",       
                     Soum == "Bayangovi" ~  "56.74503",
                     Soum == "Bayantsagaan" ~  "48.66149" , 
                     Soum == "Erdenetsogt" ~  "44.34469" ,
                     Soum == "Gurvanbulag" ~  "22.92072",
                     Soum == "Jargalant" ~  "29.64674",
                     Soum == "Jinst"  ~ "72.33708",
                     Soum == "Bayan-Uul" ~  "12.67167",
                     Soum == "Bayandun" ~  "16.97653", 
                     Soum == "Chuluunkhoroot" ~  "10.52656",
                     Soum ==  "Dashbalbar" ~  "22.03663",
                     Soum == "Sergelen" ~  "25.93054",
                     Soum == "Tsagaan Ovoo" ~  "20.79864",
                     Soum == "Altanshiree" ~  "52.88769",
                     Soum == "Saikhandulaan" ~  "53.40083", 
                     Soum == "Ulziit" ~  "36.72490",
                     Soum == "Undurshil" ~  "70.94102",
                     Soum == "Khankhongor" ~  "31.08567",
                     Soum == "Tsogt-Ovoo" ~  "32.09243",
                     Soum == "Bat-Ulziit" ~  "41.52044",
                     Soum == "Bayangol" ~  "51.88795",
                     Soum == "Sant" ~  "61.16841", 
                     Soum == "Uyanga" ~  "49.89615",
                     Soum == "Bayangol" ~  "67.27556",
                     Soum ==  "Saikhan" ~ "64.15439",
                     Soum == "Bayandelger" ~ "97.07804",
                     Soum == "Ongon" ~  "61.07740" ,
                     Soum == "Bayan" ~  "29.36778", 
                     Soum == "Bayantsagaan" ~  "23.19291", 
                     Soum == "Erdenesant" ~ "88.04511", 
                     Soum == "Undurshireet" ~  "78.79142",
                     TRUE ~ as.character(Soum)))
td.fg$frg.avg<- as.numeric(td.fg$frg.avg)

td.fg<- td.fg%>% mutate(frg.cv = 
                       case_when(Soum == "Ikh Tamir" ~ "15.786169",
                                 Soum == "Jargalant" ~  "17.13132", #DUP
                                 Soum == "Tariat" ~  "21.64224",
                                 Soum == "Undur-Ulaan" ~  "18.07620",
                                 Soum == "Bayan-Ovoo" ~  "42.67757",
                                 Soum == "Bayan-Undur" ~  "32.13516",       
                                 Soum == "Bayangovi" ~  "48.53851",
                                 Soum == "Bayantsagaan" ~  "47.03617" , 
                                 Soum == "Erdenetsogt" ~  "28.61885" ,
                                 Soum == "Gurvanbulag" ~  "34.39416",
                                 Soum == "Jargalant" ~  "21.82340", #DUP
                                 Soum == "Jinst"  ~ "47.57293",
                                 Soum == "Bayan-Uul" ~  "24.78928",
                                 Soum == "Bayandun" ~  "30.91730", 
                                 Soum == "Chuluunkhoroot" ~  "44.07814",
                                 Soum ==  "Dashbalbar" ~  "35.59093",
                                 Soum == "Sergelen" ~  "31.15398",
                                 Soum == "Tsagaan Ovoo" ~  "30.43305",
                                 Soum == "Altanshiree" ~  "43.22403",
                                 Soum == "Saikhandulaan" ~  "35.77483", 
                                 Soum == "Ulziit" ~  "25.48745",
                                 Soum == "Undurshil" ~  "21.90852",
                                 Soum == "Khankhongor" ~  "26.84686", 
                                 Soum == "Tsogt-Ovoo" ~  "25.34527", 
                                 Soum == "Bat-Ulziit" ~  "21.77934",
                                 Soum == "Bayangol" ~  "48.95972", #DUP
                                 Soum == "Sant" ~  "45.43440", 
                                 Soum == "Uyanga" ~  "28.42987",
                                 Soum == "Bayangol" ~  "36.16526",
                                 Soum ==  "Saikhan" ~ "39.71289", 
                                 Soum == "Bayandelger" ~ "43.08943",
                                 Soum == "Ongon" ~  "30.23980" ,
                                 Soum == "Bayan" ~  "38.14300", 
                                 Soum == "Bayantsagaan" ~  "35.72160", 
                                 Soum == "Erdenesant" ~ "47.71352", 
                                 Soum == "Undurshireet" ~  "43.54259",
                                 TRUE ~ as.character(Soum)))
td.fg$frg.cv<- as.numeric(td.fg$frg.cv)
#td.fg<- td.fg[,c(1:4, 9:16, 21:27,31)]
#----

# save this to pull it in elsewhere.
save(td.fg, file = "./data/tdfg.RData")

# let's make sure the values transferred correctly:
  # mean frg.avg across each sum
td %>% group_by(Soum) %>% summarise(mean(frg.avg)) # seems to correspond to values above.





# COMBINE THE DATAFRAMES ----------------------------------------------------
#   for merging with the matching df:
#   the dataframes to be merged are:
#     lt.ppt = long-term average precip 79-16 <- change to 11
#     frg.avg = available forage mean 00-12
#     lt.sfu = long-term average sfu 81-16
#soum_stats <- left_join(lt.ppt, frg.avg, by = c("AimagName", "SoumName"))

soum_stats <- bind_cols((arrange(lt.ppt, AimagName,SoumName)), 
                        (arrange(frg.avg, AimagName, SoumName)),
                        (arrange(lt.sfu, AimagName, SoumName)),
                        (arrange(ppt.99, AimagName, SoumName)),
                        (arrange(sfu.99, AimagName, SoumName))
                        )
soum_stats<- as.data.frame(soum_stats)

soum_stats<- soum_stats[,c(1:5,9, 13, 17, 18, 22)]

soum_stats$AimagName <- trimws(soum_stats$AimagName)
soum_stats$SoumName <- trimws(soum_stats$SoumName) 

# EXPORT R DATA FILE ----------------
#   this is so we can call it in the Rmd without having to run all this code in it

saveRDS(soum_stats, file= "soum_stats.rds")

# ----------------------------------------------------------------------------
# now add this to the household data for matching....
# maybe move this bit over to matching.Rmd 

# x<- hhmatch %>% left_join(lt.ppt, by = c("AimagName", "SoumName"))


# Generating the slope values...
pslope<- soum_stats %>% 
  filter(year <= 2010) %>%
  group_by(AimagName, SoumName) %>% 
  do(mod = lm(ppt_mean ~ year, data = .)) %>%
  summarise(pptslope = summary(mod)$coeff[2]) 
ggplot(pslope)+ geom_density(aes(x= pptslope))


##### ARE THESE REALLY ALL NEGATIVE??

  
  




