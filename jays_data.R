# code to pull in & tidy Data from Jay Angerer on 
# soum level ndvi, forage use, spei and precip
# most of these are yearly average per soum.

library(readxl)

# Import raw files--------------------------------------------------------------------
# THE DATA THAT NEEDS TO BE IMPORTED AND JOINED:
# 
# soum level precip data, mean and stdev:
# 
j.precip<- read.csv("./data/from_Jay/cpc_rain_soum_means_stdev.csv")
tbl_df(j.precip)
j.precip %<>% mutate_at(1:2, funs(as.character(.)))

# soum level forage availability from modis kg/ha
# this just pulls in the first sheet, which has averages
# the third sheet has stdev
j.forage.avail<- read_excel("./data/from_Jay/Forage_available_modisv6_average_stdev_kg_ha.xlsx", sheet = 2)
tbl_df(j.forage.avail) %>% mutate_at(1:2, funs(as.character(.)))

#
j.sfu <- read_excel("./data/from_Jay/sheep_units_per_ha_soum.xlsx", sheet = 1)
tbl_df(j.sfu) %>% mutate_at(1:2, funs(as.character(.)))



# Info on fixing the names to match MOR2 ---------------------------------------------
# 
# need to match these on Soum ID, Aimag + Soum_name
# 
# soum level :
#   NOTE: MANUALLY CORRECTED # TYPOS IN THIS .csv FILE on 9/10/17
#   bc REALIZED THEY WERE NOT MATCHING THE MOR2 DATABASE
soums <- read.csv("./data/from_Jay/soum_names.csv", stringsAsFactors = FALSE)
soums %<>% rename(Soum_name = jay)
soums$MOR2match<- trimws(soums$MOR2match)

# aimag level :
aimags <- read.csv("./data/from_Jay/aimag_names.csv", stringsAsFactors = FALSE)

#
# Tidying jay's data and matching in correct names:------------------------------
# 
ppt.mean <- j.precip[,1:43] %>% 
  gather("Year", "ppt_mean", 6:43) %>%
  separate (Year, c("x1", "x2", "x3", "year"), sep="_", remove = TRUE) %>%
  dplyr::select(-x1, -x2, -x3) %>%
  left_join(soums, by = "Soum_name") %>%
  left_join(aimags, by = "Aimag_name")
ppt.mean$year<- as.integer(ppt.mean$year) 
  
ppt.sd<- j.precip[,c(1:3, 44:81)]%>%
  gather("Year", "ppt_sd", 4:41)%>%
  separate(Year, c("y1", "y2", "y3", "year"), sep="_", remove = TRUE) %>%
  dplyr::select(-y1, -y2, -y3) %>%
  left_join(soums, by = "Soum_name") %>%
  left_join(aimags, by = "Aimag_name")
ppt.sd$year<- as.integer(ppt.sd$year) 


frg.avail <- j.forage.avail %>% 
  gather("Year", "biomass", 6:17) %>%
  separate(Year, c("x1", "x2", "year"), sep = "_", remove = TRUE) %>%
  dplyr::select(Aimag_name, Soum_name, Soum_ID, year, biomass) %>%




# put mean and sd together in to one df and fix col headers
soum.ppt <- left_join(ppt.mean, ppt.sd, by = c("Aimag_name", "Soum_name", "year"))  #"Soum_ID", "year")) 
soum.ppt<- soum.ppt[,c(3,4,6,7,8,9,11)]
soum.ppt$SoumName <- soum.ppt$MOR2match.x
soum.ppt$AimagName <- soum.ppt$aimag_match.x
                     

# --------------------------------------------------------------------------
# now need to calc a summary, mean across all years by SoumID
lt.ppt <- soum.ppt %>% 
  group_by(year) %>% 
  group_by(AimagName, SoumName, Soum_ID.x) %>% # if put all these together they will appear in the output
  summarise(ltmeanppt = mean(ppt_mean), ltmeansd = mean(ppt_sd))
lt.ppt <- as.data.frame(lt.ppt)
lt.ppt$AimagName<- trimws(lt.ppt$AimagName)
lt.ppt$SoumName<- trimws(lt.ppt$SoumName)

save(lt.ppt, file= "lt.ppt.R")
# ----------------------------------------------------------------------------
# now add this to the household data for matching....
# maybe move this bit over to matching.Rmd 

x<- hhmatch %>% left_join(lt.ppt, by = c("AimagName", "SoumName"))

# why am I getting NAs?
# 

# Let's try meging in the correct SoumCode
x<- mor2 %>% group_by(AimagName, SoumName, SoumCode) %>% summarise()
y<- soum.ppt %>% group_by(AimagName, SoumName, Soum_ID.x) %>% summarise()


x$AimagName <- trimws(as.character(x$AimagName))
x$SoumName <- trimws(as.character(x$SoumName))

y$AimagName <- trimws(as.character(y$AimagName))
y$SoumName <- trimws(as.character(y$SoumName))
z<- left_join(x, y, by = c("AimagName", "SoumName"))







