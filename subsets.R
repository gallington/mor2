sumfac<- mutate(rpe, pracsum = p4+p5+p6+p7+p8+p9+p10, 
                     ecolsum = e1+(100-e3) +e4) #+e2


quantile(sumfac$ecolsum)
# w/o forbs
0%    25%    50%    75%   100% 
29.00  75.25 108.80 163.45 272.20 
equant<- sumfac$ecolsum

equant<- if_else(equant > 272, 5, equant) 
equant<- if_else(equant>163, 4, equant)
equant<-  if_else(equant>108, 3, equant)
equant<-  if_else(equant>75, 2, equant)
equant<-  if_else(equant>29, 1, equant)

# 
refcols = c(51,107,115,132,207, 211,224, 345,357,376,590,597, 598,605,609)

cases<- filter(sumfac, RefNum == c()) 


# pulling location info:
mor2loc<- select(mor2, RefNum = SocialSurveyReferenceNumber, 
                 Lat100 = Latitude100, 
                 Lat500 = Latitude500, 
                 Lat1k=Latitude1000, 
                 Long100 = Longitude100, 
                 Long500 =  Longitude500, 
                 Long1k = Longitude1000)
write.csv(mor2loc, "./data/mor2loc.csv")


rename(mor2loc, RefNum = SocialSurveyReferenceNumber, Lat100)