# 
# subset ecological vars = ecol
# but, need Ecological Zone info too?
# 
# [1463]  Ecologicalzone_4Code1000
#subset out the ecological vars of interest for Mean_500_1000
e.cols <- c(1463, 1515:1538)
ecolmeans<- mor2[,e.cols]
tbl_df(ecolmeans)
ecolmeans <- select(ecolmeans, -AcidDetergentFiber_percent_Mean500_1000)
ecolmeans <- select(ecolmeans, -CrudeProtein_percent_Mean500_1000)
#shorten the variable names
names(ecolmeans) = sub(pattern = "_Mean500_1000", replacement = "", x = names(ecolmeans))

e.pca<- (ecolmeans)
