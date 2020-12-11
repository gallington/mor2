library(raster)
library(rgdal)
library(sp)
library(rgdal)
library(maptools)
library(spatial)
library(maps)
library(sf)  # use this once it's avail -need new version of gdal first
library(prettymapr)

#USE readOGR bc it brings in the projection info. readShapePoly does not.
im_mn<-readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data", layer='IM_MN_state')
china_provs<- readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data/china/CHN_adm_shp", layer='CHN_adm1')
china <- readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data/china/CHN_adm_shp", layer='CHN_adm0')
china_leagues<- readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data/china/CHN_adm_shp", layer='CHN_adm2')
Xht<-readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data", layer='xilinhot')
mg_provs<- readOGR(dsn="./nfs/gallington-data/Timeseries/GIS_data/boundary_mg", layer = 'province')
mg_soums<- readOGR(dsn="/nfs/gallington-data/Timeseries/GIS_data/boundary_mg", layer = 'soums')
crsproj <- CRS('+init=epsg:4326')

mg_provs <- readOGR(dsn= "/Users/gallington/Repositories/mor2/data/mor2data", layer = "MNG_adm2" )


#check out the dataframe to get the index no for Xilinhot
data.frame(im_mn)
# Rename levels
levels(im_mn$NAME) <- c("Mongolia","Inner Mongolia")

#subset out IMAR, remember that indexing stars w 1 at column name, so add one to get the right row
imar<- im_mn[2,]
mg<- im_mn[1,]
imar2 <- china_provs[19 ,5]

#subset out Xilingol
xgl<- spTransform(china_leagues[219,], crsproj)

#subset out sukhbaatar
sukh<- mg_provs[1,]

# make all same projection:
mg_proj <- spTransform(mg, crsproj)
mg_prov_proj <-spTransform(mg_provs, crsproj)
soum_proj <- spTransform(mg_soums, crsproj)
china_proj<- spTransform(china, crsproj)
imar_proj<- spTransform(imar, crsproj)
china_provs_proj<- spTransform(china_provs, crsproj)
sukh<- spTransform(sukh, crsproj)
xht<- spTransform(Xht, crsproj)

# THE RASTER PART:-------------------
# require the package
require(MODIS)
# land cover raster:
landcover<-raster('/nfs/gallington-data/Timeseries/GLOBCOVER/GLOB_IM_MN1.tif')
modis[modis>16]<- NA
landcover <- setMinMax(landcover)
projection(landcover)<- CRS('+init=epsg:4326')
# info for the palette:
library(plotKML)
library(rasterVis)
data("worldgrids_pal")
globpal<- worldgrids_pal$globcov
myTheme=rasterTheme(region=globpal)

# labels:
coords<- data.frame(x=c(99.01805, 100.67798, 112.0, 112.0), #98.34661), 
                    y = c(47.87726, 40.97735, 46.5, 44.5), #34.00082), 
                    labels= c("MONGOLIA", "Inner Mongolia", "Sukhbaatar", "Xilingol")) # "CHINA"))
coordinates(coords)<- ~x+y
labels <- SpatialPoints(coords)

coordinates(mor2loc)<- ~long+lat   
mor2_pts<- SpatialPoints(mor2loc)




# plot the maps
globpal2<- (globpal[c(1:5,7:16,18:22)])
png("/nfs/gallington-data/ScenPlanMS/mygraph.png",
    width = 6, height = 5)

plot(landcover, bty = "n", col = globpal2)
#levelplot(landcover, par.settings=myTheme, margin=F)
#plot(landcover, bty = "n", col = globpal)
#plot(china_proj, border = "grey30", col= NA, add= TRUE)
#plot(china_provs_proj, border = "grey70", add = TRUE)
plot(imar_proj, col= NA, border = "grey30", add= TRUE) 
plot(mg_proj, col= NA, border = "grey30", lty= 1, add = TRUE)
plot(sukh, col= NA, border = "grey15", lty= 2, lwd=2, add = TRUE)
plot(xgl, col= NA, border = "grey15", lty= 2, lwd=2,add = TRUE)

text(coords, labels=as.character(coords$labels), col="grey20",
     cex=0.8, font=2, adj=c(0,1))
title("Mongolian Plateau", cex = 0.9, line = -1)
addnortharrow(pos= 'bottomleft', padin = c(0.45, 0.45),scale= 0.6)
addscalebar(plotepsg = 4326, unitcategory = 'metric', htin = 0.1, style = 'bar', padin = c(0.25, 0.25))

plot(mg_proj, col= NA, border = "grey30", lty= 1)


############################

# FOR THE ERL PAPER:-----------
mg_provs <- readOGR(dsn= "/Users/gallington/Repositories/mor2/data/mor2data", layer = "MNG_adm2" )

mg<- readOGR(dsn="/Users/gallington/Repositories/mor2/data/mor2data/MNG_adm0.shp")
mg_aimag<- readOGR(dsn="/Users/gallington/Repositories/mor2/data/mor2data/MNG_adm1.shp")
mg_soums<- readOGR(dsn="/Users/gallington/Repositories/mor2/data/mor2data/MNG_adm2.shp")
mg_ez<- readOGR(dsn="/Users/gallington/Repositories/mor2/data/mor2data/MOR2_view/NaturalZones/Natural_Zones_aggregate_utm48n.shp")

#mor2_soums<- readOGR(dsn="/nfs/gallington-data/Herder_sem/data/MOR2_view/MOR2/MOR2_STUDY_SITES_all.shp")

cbsoums<- mor2_soums[mor2_soums$CBRM=='y',]
trueCentroids = gCentroid(cbsoums,byid=TRUE)


# subset data w coords
mor2Map<- dplyr::select(mor2, aimag = AimagName, 
                        soum = SoumName, 
                        cbrm = CBRM, 
                        lat = Latitude500, 
                        long = Longitude500, 
                        ez = Ecologicalzone_4Code500)

subMOR2<- dplyr::distinct(mor2Map, soum, .keep_all = TRUE)

# just the points
mor2pts<- mor2 %>% dplyr::select( 
#mor2pts<- subMOR2 %>% dplyr::select(
                          lat = Latitude500,
                          long = Longitude500) 


# just the points for cbrm=0
mor2ptscb0<- mor2 %>% dplyr::filter(CBRM ==0)%>%
                      dplyr::select( 
                        lat = Latitude500,
                        long = Longitude500,
                        soum= SoumName) 
# just the points for cbrm = 1
mor2ptscb1<- mor2 %>% dplyr::filter(CBRM ==1)%>%
                      dplyr::select(
                        lat = Latitude500,
                        long = Longitude500) 
# assign coordinates
coordinates(mor2pts)<- ~long+lat 
coordinates(mor2ptscb1)<- ~long+lat 
coordinates(mor2ptscb0)<- ~long+lat 
coordinates(subMOR2)<- ~long+lat
# 

# set projection
projection(mor2ptscb0)<- CRS('+init=epsg:4326')
projection(mor2ptscb1)<- CRS('+init=epsg:4326')
mor2_pts<- SpatialPointsDataFrame(mor2pts, mor2Map)

projection(mor2_pts)<- CRS('+init=epsg:4326')
projection(subMOR2)<- CRS('+init=epsg:4326')

mor2_ptscb0<- SpatialPointsDataFrame(mor2ptscb0, (dplyr::filter(mor2Map, cbrm == 0)))
mor2_ptscb1<- SpatialPointsDataFrame(mor2ptscb1, (dplyr::filter(mor2Map, cbrm == 1)))
crsproj <- CRS('+init=epsg:4326')

mg_soums<- spTransform(mg_soums, crsproj)
mor2_pts<- spTransform(mor2_pts, crsproj)
subMOR2<- spTransform(subMOR2, crsproj)

mor2_ptscb0<- spTransform(mor2_ptscb0, crsproj)
mor2_ptscb1<- spTransform(mor2_ptscb1, crsproj)
mg_ez<- spTransform(mg_ez, crsproj)
mor2_soums<- spTransform(mor2_soums, crsproj)
trueCentroids<- spTransform(trueCentroids, crsproj)
#o = over(mor2_pts, mg_soums)

polys.sub<- mg_soums[!is.na(sp::over(mg_soums, sp::geometry(mor2_pts))), ] 
polys.sub<- mg_soums[!is.na(sp::over(mg_soums, sp::geometry(subMOR2))), ] 

polys.subcb1 <- mg_soums[!is.na(sp::over(mg_soums, sp::geometry(mor2_ptscb1))), ] 
polys.subcb0 <- mg_soums[!is.na(sp::over(mg_soums, sp::geometry(mor2_ptscb0))), ] 

#subsoums<- subMOR2$soum
#soum.cbrm <- subset(mor2_soums, CBRM==y)
#nope
#mor2_sm <- intersect(trueCentroids, subMOR2)
#nope

#mor2_sm <- over(trueCentroids, subMOR2)
#nope
mor2_sm<- mg_soums[!is.na(sp::over(mg_soums, sp::geometry()))]



# MG LABEL
coords<- data.frame(x=c(97.5, 100, 110.0, 112.0), #98.34661), 
                    y = c(49, 42.23, 50.5, 44.5), #34.00082), 
                    labels= c("MONGOLIA", "P.R. China", "Russia", "")) # "CHINA"))
coordinates(coords)<- ~x+y
labels <- SpatialPoints(coords)
projection(coords)<- CRS('+init=epsg:4326')
coords<- spTransform(coords, crsproj)

coordinates(coords)<- ~x+y
labels <- SpatialPoints(coords)




#--------- Map with Ecozones----
#ezlabels<- levels(mg_ez@data$Z_NAME_ENG)
ezlabels<- c("Desert","Eastern Steppe" , "Forest Steppe","Gobi desert","High Mountain","Lake","Mountain Taiga","Steppe")
ezpal<- c("#f6e8c3", "#f6e8c3", "#7fbf7b", "#1b7837", "#1b7837", "#f5f5f5", "#d1e5f0")

ezpal<- c("#f6E8C3",  "#d4f0a3", "#189827", "#f6E8C3", "#189827",  "#4285f4", "#beceb9", "#c9eebe" )     #Steppe  
legend.ezpal<- c("#f6E8C3",  "#d4f0a3", "#c9eebe","#189827", "#beceb9", "#4285f4" )
leglabs<- c("Desert Steppe","Eastern Steppe", "Steppe", "Forest & Mntn Steppe","Alpine & Taiga","Lake")

Fig.1MOR2map<- tm_shape(mg_ez)+
  tm_fill(col = "Z_NAME_ENG", #)
 # tm_polygons(col = 'Z_NAME_ENG', 
              palette = ezpal, 
              border.col = "grey75",
              legend.show= FALSE)    +
 tm_add_legend(type="fill", 
               col=legend.ezpal, 
               labels = leglabs, 
               title="Ecozones") +
#tm_shape(MNG_adm0) +
  tm_shape(mg) +
  tm_polygons(alpha = 0) +
#tm_shape(MNG_adm1) +
 tm_shape(mg_aimag) +
  tm_polygons(alpha = 0) +
tm_shape(MNG_adm2) + tm_polygons(alpha = 0)+
#tm_shape(mor2_soums) +
 # tm_fill(col = "grey", alpha = .5) +
  tm_borders(col = "black", lwd = 2) +
  tm_add_legend(type="line", lwd = 2,
                col= "black",
                labels = "Study Soums") +
#tm_shape(mor2_ptscb1) +
# tm_shape(trueCentroids)+
#   tm_dots(col = "grey15",
#          size = .15,
#          shape = 17)+
#  tm_add_legend(type="symbol",shape = 17,
#                col= "black",
#                labels = "soums w/ CBRM")+
  tm_compass(position = c(.15, .12), color.light = "grey90") +
#tm_shape(coords)+
 tm_text("labels", size = 1)
Fig.1MOR2map

save_tmap(Fig.1MOR2map, "Fig.1MOR2map.png", width = 9.5, height = 8)

# FOR BOOK CHAPTER: -------
# SLIGHTLY ADJUSTED FIGURE FOR MOR2 Book Chapter
# Sept 4, 2018

# labels:
  # place name labels
coords<- data.frame(x=c(96.5, 100, 110.00), #98.34661), 
                    y = c(48, 42.23, 50.5), #34.00082), 
                    labels= c("MONGOLIA", "P.R. China", "Russia")) # "CHINA"))
coordinates(coords)<- ~x+y
labels <- SpatialPoints(coords)

  # ecol zone labels
ezlabels<- c("Desert","Eastern Steppe" , "Forest Steppe","Gobi desert","High Mountain","Lake","Mountain Taiga","Steppe")
ezpal<- c("#f6e8c3", "#f6e8c3", "#7fbf7b", "#1b7837", "#1b7837", "#f5f5f5", "#d1e5f0")

ezpal<- c("#f6E8C3",  "#d4f0a3", "#189827", "#f6E8C3", "#189827",  "#4285f4", "#beceb9", "#c9eebe" )     #Steppe  
legend.ezpal<- c("#f6E8C3",  "#d4f0a3", "#c9eebe","#189827", "#beceb9", "#4285f4" )
leglabs<- c("Desert Steppe","Eastern Steppe", "Steppe", "Forest & Mntn Steppe","Alpine & Taiga","Lake")

  # plotting: 
MOR2map<- tm_shape(mg_ez)+
  tm_fill(col = "Z_NAME_ENG", #)
          palette = ezpal, 
          border.col = "grey75",
          legend.show= FALSE)    +
  tm_add_legend(type="fill", 
                col=legend.ezpal, 
                labels = leglabs, 
                title="Ecozones") +
  tm_shape(mg_soums) +
  #tm_shape(mor2_soums) +
  tm_borders(col = "grey65", lwd = 0.7) +
  tm_add_legend(type="line", lwd = 2,
                col= "grey65",
                labels = "Soums") +
  tm_add_legend(type="line", lwd = 1,
                col= "black",
                labels = "Aimag Boundary") +
  tm_shape(mg) +
  tm_polygons(alpha = 0) +
  tm_shape(mg_aimag) +
  tm_polygons(alpha = 0) +
  tm_borders(col = "black", lwd = 1) +
  #tm_shape(trueCentroids)+
  #tm_dots(col = "grey25",
   #       size = .15,
    #      shape = 16)+
  #tm_add_legend(type="symbol",shape = 16,
   #             col= "black",
    #            labels = "soums w/ CBRM")+
  tm_compass(position = c(.15, .12), color.light = "grey90") +
  tm_shape(coords)+
  tm_text("labels", size = 1)
MOR2map
# png version
tmap_save(MOR2map, "./MongoliaEZmap.png", width = 10, height = 8.5)
# tif version
tmap_save(MOR2map, "./MongoliaEZmap.tiff", width = 10, height = 8.5)

















#NOT USED-----

##### NOT USED:
# textcoords<- data.frame(x=99.01805,y = 47.87726, labels= "MONGOLIA")
# coordinates(textcoords)<- ~x+y
# mglabel<- SpatialPoints(textcoords)


#out <- st_intersection(points, poly)
#out <- st_intersection(mor2_pts, mg_soums)
#----------Map with cbrms------ 
prettymap(plot(MNG_adm0), oma = c(0, 0, 0, 0), mai = c(0, 0, 0, 0),
          drawbox = TRUE, box.lwd = 1, drawscale = TRUE,
          scale.pos = "bottomleft", scale.htin = 0.1, scale.widthhint = 0.25,
          scale.unitcategory = "metric", scale.style = "bar",
          scale.bar.cols = c("black", "white"), scale.lwd = 1,
          scale.linecol = "black", scale.padin = c(0.15, 0.15),
          scale.labelpadin = 0.08, scale.label.cex = 0.8,
          scale.label.col = "black", scale.plotunit = NULL, scale.plotepsg = NULL,
          scale.tick.cex = 0.8, drawarrow = FALSE, arrow.pos = "topright",
          arrow.scale = 1, arrow.padin = c(0.15, 0.15), arrow.lwd = 1,
          arrow.cols = c("white", "black"), arrow.border = "black",
          arrow.text.col = "black", title = NULL)
plot(mg_ez, col = ezpal, border = "grey75", add= TRUE)
#plot(MNG_adm2, col= NA, border = "grey85", lty= 1, lwd=1, add = TRUE)
plot(MNG_adm1, col= NA, border = "grey15", lty= 1, lwd=1, add = TRUE)
plot(MNG_adm0, col= NA, border = "grey25", lty= 1, lwd=2, add = TRUE)
plot(mor2_soums, col=NA, border = "black", lwd = 2, add= TRUE)
plot(trueCentroids, pch = 16, add= TRUE)
text(coords, labels=as.character(coords$labels), col="grey20",
     cex=0.8, font=2, adj=c(0,1))
#text(polys.subcb0, labels=as.character(polys.subcb0$NAME_2), col="grey10", cex=0.8, font=2, adj=c(1,1))
#title("Lo", cex = 0.9, line = -1)
#mgbb<- sp::bbox(MNG_adm0)
#makebbox(53, 120, 40, 86)
# SF VERSION OF ABOVE:::: plus merging two dataframes

soums.sf <- st_read(dsn= "/nfs/gallington-data/Herder_sem/data/MNG_adm2.shp")
st_crs(soums.sf)$proj4string



prj <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

sf.pts <- st_as_sf(mor2Map, coords = c("long", "lat"), crs = EPSG:4326)
sf.t<- st_sfc(mor2Map, crs= 4326)
st_crs(sf.pts)

sf.pts<- st_transform(sf.pts, crs = 4326)
soums.sf<- st_transform(soums.sf, crs = 4326)

plot(soums.sf$geometry)
plot(sf.pts, add = TRUE)

cbrm.soums<- st_within(soums.sf, sf.pts)

# DEPRECATED:
# 
# map<- ggplot() + 
#   geom_polygon(data = china_proj, aes(x = long, y= lat), fill = "white", line = 0.5, color = "black")+
#   geom_polygon(data=imar_proj, aes(x=long, y=lat), fill = "grey70" )+
#   geom_polygon(data= mg_proj, aes(x = long, y= lat), fill = "grey70")+
#   labs(x="", y = "", title="Mongolian Plateau")+ #labels
#   theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
#         axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
#         plot.title = element_text(lineheight=.8, face="bold", vjust=1))+ # make title bold and add space
#   coord_equal(ratio=1)+  # square plot to avoid the distortion
#   annotate("text", x = c(99.01805, 113.67798,  98.34661), y = c(47.87726, 43.97735, 34.00082), 
#            label =c("MONGOLIA", "Inner Mongolia", "CHINA"))
# 
# ggsave(map, filename ="plateau_map.png" )
# 
# map(database= "world", regions = c("China", "Mongolia"), fill = TRUE, col = c("grey70", "grey30"))
# map.text("world", regions = c("China", "Mongolia"),  
#          #labels , 
#          add= TRUE)
# china<- map("world", "China")
# 

    
