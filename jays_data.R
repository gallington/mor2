
# Code to pull in & tidy Data from Jay Angerer on 
#   soum level ndvi, forage use, spei and precip
#   most of these are yearly average per soum.

library(readxl)

# Import raw files--------------------------------------------------------------------
# THE DATA THAT NEEDS TO BE IMPORTED AND JOINED:
# 
#   these are soum-level data:
# Precip mean & sd
j.precip<- read.csv("./data/from_Jay/cpc_rain_soum_means_stdev.csv")
tbl_df(j.precip)
j.precip %<>% mutate_at(1:2, funs(as.character(.)))

# Forage Availability from modis kg/ha
#   this just pulls in the first sheet, which has averages
#   the third sheet has stdev
j.forage.avail<- read_excel("./data/from_Jay/Forage_available_modisv6_average_stdev_kg_ha.xlsx", sheet = 2)
tbl_df(j.forage.avail) %>% mutate_at(1:2, funs(as.character(.)))

# Sheep Forage Units 
j.sfu <- read_excel("./data/from_Jay/sheep_units_per_ha_soum.xlsx", sheet = 1)
tbl_df(j.sfu) %>% mutate_at(1:2, funs(as.character(.)))



# Info on fixing the names to match MOR2 ---------------------------------------------
#   need to match these on Soum ID, Aimag + Soum_name
# 
# soum level :
#   NOTE: MANUALLY CORRECTED # TYPOS IN THIS .csv FILE on 9/10/17
#   bc REALIZED THEY WERE NOT MATCHING THE MOR2 DATABASE
soums <- read.csv("./data/from_Jay/soum_names.csv", stringsAsFactors = FALSE)
soums %<>% rename(Soum_name = jay)
soums$MOR2match<- trimws(soums$MOR2match)

# aimag level :
aimags <- read.csv("./data/from_Jay/aimag_names.csv", stringsAsFactors = FALSE)


# Tidying jay's data and matching in correct names:------------------------------
# 
# ********* STILL NEED TO CORRECT THE YEAR RANGE ON THESE TO CUT AT 2010 or 2011...
#   years = 1979 - 2016
ppt.mean <- j.precip[,1:43] %>% 
  gather("Year", "ppt_mean", 6:43) %>%
  separate (Year, c("x1", "x2", "x3", "year"), sep="_", remove = TRUE) %>%
  dplyr::select(-x1, -x2, -x3) # %>%
  # left_join(soums, by = "Soum_name") %>%
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

  
  




