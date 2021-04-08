



# 4/1/2021: I don't think we want the radar plots.
# It obscures the inherent predicted prob at control level.
# while it's nice that they are all combined, it doesn't show the diffs as easily 
# as the dot plots.





#### SOLVED: THESE VALUES WERE ORIGINALLY COPIED AND PASTEDFROM THE
# output that is pasted to console when you plot with
# plotLMER.fnc()  which is in the predProb_each_by_ez.R script
#   keeping those plotting scripts for now --what I used to generate the dor plots 
# but I think what I really want to use
# is the marginal effects predicted from 
# get_model_data()  (no walso in that same script)
# 




# Look into this more.
# Check get_model_data Help page
# Think about what I really want to represent in the radar plots.
# Is it the coefficients? bc that's not what I got, right?'
# check for NAs
map(mtcars, ~sum(is.na(.)))

#extract marginal effects 
  # if you use type = "eff" it holds the discrete predictors constant at their proportions
wp.me <- get_model_data(hypw, type = "pred")

  # this is the full model, though. not the divided out by ecological zone info...



# pulled the predictions, but don't need them here really
wp_pred<- as.data.frame(predict(hypw))
names(wp_pred) <- c("wp_pred")

# subset dataframe by ez ? 
ds<- subset(td.fg, td.fg$ez==1)
#st<- subset(to, to$ez==c(2,4)) # WHY ARE THESE COMBINED??
st<- subset(td.fg, td.fg$ez ==2)
es<- subset(td.fg, td.fg$ez==3)
fms<- subset(td.fg, td.fg$ez==4)

# played with how to use the original model to predict for a subset of the data. 
# dwpred <- (predict(hypw, ds, allow.new.levels = TRUE)) 
#names(dwpred) <- c("ds_wp")
# dwpred$ds_wp <- as.numeric(dwpred$ds_wp)

# March 27
# Ok new plan, let's see if we calc the marg effects by indiv ez model?

hypw1<- glmer(ResWint ~ hhTenureWPast + hhTenureWCamp + RuleYes + RuleFormal +  cogSC1 + frgCV + frg.left + (1 | Org), data = ds, family = binomial)


# the predictions
w1pr <- predict(hypw1)
# the data used (obsv w/o NAs)
df <- hypw1@frame
dsw_me <- get_model_data(hypw1, type = "pred")

# COMPARE THESE TO GOOGLE SHEETS? 


Maybe I just plotted the betas in the radar plots? 
Oh! Wait, I think maybe I used the z values.

getME(hypw,"theta")
fixef(hypw)

# checking model convergence issues via 
# suggestions from:
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
numcols <- grep("^c\\.",names(td.fg))
dfs <- ds  # update dataset
dfs[,numcols] <- scale(dfs[,numcols])
hypw_sc <- update(hypw1,data=dfs)
# ok

# check singularity
  # check that are not 0 or close to it
tt <- getME(hypw_sc,"theta")
ll <- getME(hypw_sc,"lower")
min(tt[ll==0])
# ok

# scaled gradients
derivs1 <- hypw_sc@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))

dd <- update(hypw_sc,devFunOnly=TRUE)
pars <- unlist(getME(hypw_sc,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))


ss <- getME(hypw_sc,c("theta","fixef"))
m2 <- update(hypw_sc,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

  #MODEL CONVERGES IF CHANGE THE OPTIMIZER AND UP THE ITERATIONS
m3 <- update(hypw_sc,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))




# Load libraries ---------------- 
library(fmsb) # for functiosn to plot radar charts
library(cowplot)
library(sjPlot)



# labels for groups
pract = data.frame(
  practice = c("Reserve Wtr", "Reserve Spr", "W otor", "F otor"),
  code = c(1:4)
)

# max-min info necessary for the chart function
maxmin = data.frame(
  PastureTenure = c(3, 1),
  CampTenure = c(2, 0),
  Rules = c(4,0),
  FormalRules = c(4,0),
  CogSC = c(3,1),
  CVForage = c(1.5, 0.5),
  RemainingForage = c(1.5, 0.5),
  StructSC = c(1, 0.5),
  AccessOtherPastures = c(2.5, 1)
)

# Load data --------- 
# data compiled in excel bc I couldn't figure out how to code it.
rad <- read.csv( "./data/marginal_effects/radar.csv", stringsAsFactors = FALSE, na.strings = c("NA"))
rad <- rad[1:4,]

# bind w the max-min info
dat <- rbind(maxmin,rad)

# base plot just to check
radarchart(dat, axistype = 2)

# Color vector

colors_in=c( "aquamarine3", "darkorchid3", "antiquewhite4", "chocolate4")
colors_border=c(  "aquamarine3", "darkorchid3","antiquewhite4", "chocolate4" )
#colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4), rgb(0.3,0.5,0.5,0.4) )
#colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.3,0.5,0.5,0.9) )
dkb<- "#a6611a"
tan<- "#dfc27d"
ltgr<- "#80cdc1"
dkgr<- "#018571"
ez_cols = c(dkb, tan, ltgr)

# Plot 1 ALL EZ --------------
# with default options:
radarchart( dat  , axistype=1 , 
            #custom polygon
            pcol=colors_border , # pfcol=colors_in, 
            plwd=2 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,4,1), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# Add a legend
legend(x=1.2, y=1, 
       legend = pract$practice, 
       bty = "n", pch=20 , 
       col=colors_border , text.col = "darkgrey", 
       cex=1, pt.cex=3)

# Now create separate plots for each ecological zone 
# Plot Each EZ -----------------

# Load data --------- 

ds.me <- read.csv( "./data/marginal_effects/desertsteppe.csv", stringsAsFactors = FALSE, na.strings = c("NA"))
#ds.me <- ds.me[, 2:11]

st.me <- read.csv( "./data/marginal_effects/steppe.csv", stringsAsFactors = FALSE, na.strings = c("NA"))
# st.me <- st.me[, 2:11]
# st.me

fms.me <- read.csv( "./data/marginal_effects/fms.csv", stringsAsFactors = FALSE, na.strings = c("NA"))
# fms.me <- fms.me[, 2:11]
# fms.me

# Tidy dataframe-----------
me.ds <- ds.me %>% 
  gather("Variable", "Marg_effect", 2:11) %>%
  mutate(EZ = 1)

me.st <- st.me %>%
  gather("Variable", "Marg_effect", 2:11) %>%
  mutate(EZ = 2)

me.fms<- fms.me%>%
  gather("Variable", "Marg_effect", 2:11) %>%
  mutate(EZ = 3)

me<- bind_rows(me.ds, me.st, me.fms)



# check that max-min still set correctly?

# max-min info necessary for the chart function
maxmin = data.frame(
  PastureTenure = c(0.75, 0),
  CampTenure = c(0.75, 0),
  Rules = c(0.75, 0),
  FormalRules = c(0.75, 0),
  CogSC = c(0.75, 0),
  StructSC = c(0.75, 0),
  RemainingForage = c(0.75, 0),
  CVForage = c(0.75, 0),
  AccessOtherPastures = c(0.75, 0),
  AccessDiffSoum = c(0.75, 0)
)

# combine
ds.dat <- rbind(maxmin, ds.me)
st.dat <- rbind(maxmin, st.me)
fms.dat <- rbind(maxmin, fms.me)


# Plot Desert Steppe -----------

radarchart( ds.dat  , 
                   title = c("Desert Steppe"),
            axistype=1 , 
            #custom polygon
            pcol=colors_border , # pfcol=colors_in, 
            plwd=2 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels= c(0, 0.2,0.4, 0.6, 0.8), 
            cglwd=0.8,
            #custom labels
            vlcex=0.8,
           # centerzero = TRUE
            )

# Plot Steppe -----------

s.rc<- radarchart( st.dat  , 
                   title = c("Steppe"),
                   axistype=1 , 
                   #custom polygon
                   pcol=colors_border , # pfcol=colors_in, 
                   plwd=2 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", 
                   caxislabels= c(0, 0.2,0.4, 0.6, 0.8), 
                   cglwd=0.8,
                   #custom labels
                   vlcex=0.8,
                   # centerzero = TRUE
)

# Plot Forest-Mtn Steppe -----------

f.rc<- radarchart( fms.dat  , 
                   title = c("Forest-Mountain \n Steppe"),
                   axistype=1 , 
                   #custom polygon
                   pcol=colors_border , # pfcol=colors_in, 
                   plwd=2 , plty=1,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", 
                   caxislabels= c(0, 0.2,0.4, 0.6, 0.8), 
                   cglwd=0.8,
                   #custom labels
                   vlcex=0.8,
                   # centerzero = TRUE
)

# Add a legend
legend(x=1.6, y=1.3, 
       legend = pract$practice, 
       title = c("PRACTICES"),
       bty = "n", pch=20 , 
       col=colors_border , text.col = "black", 
       cex=1, pt.cex=3)



# Plot by Practice-----------
  # reserve winter 
rw.me <- read.csv( "./data/marginal_effects/rsvW.csv", stringsAsFactors = FALSE, na.strings = c("NA"))
rw.me <- rw.me[1:3, 2:8]
  # reserve spring
rs.me <- read.csv( "./data/marginal_effects/rsvS.csv", stringsAsFactors = FALSE, na.strings = c("NA"))
rs.me <- rs.me[1:3, 2:8]
  # winter otor  (rememebr: most did NOT go on otor though)
wo.me <- read.csv( "./data/marginal_effects/wotor.csv", stringsAsFactors = FALSE, na.strings = c("NA"))
wo.me <- wo.me[1:3, 2:11]
  # fall otor
fo.me <- read.csv( "./data/marginal_effects/fotor.csv", stringsAsFactors = FALSE, na.strings = c("NA"))
fo.me <- fo.me[1:3, 2:11]

# new maxmin w fewer cols: ------
maxmin2 = data.frame(
  PastureTenure = c(0.75, 0),
  CampTenure = c(0.75, 0),
  Rules = c(0.75, 0),
  FormalRules = c(0.75, 0),
  CogSC = c(0.75, 0),
# StructSC = c(0.75, 0),
  RemainingForage = c(0.75, 0),
  CVForage = c(0.75, 0)
# AccessOtherPastures = c(0.75, 0),
# AccessDiffSoum = c(0.75, 0)
)

  # use the smaller df for reserving pastures
rw.dat <- rbind(maxmin2, rw.me)
rs.dat <- rbind(maxmin2, rs.me)
  # use the full maxmin df for otor
wo.dat <- rbind(maxmin, wo.me)
fo.dat <- rbind(maxmin, fo.me)


# Plot Rserve Winter Pastures -------
radarchart( rw.dat  , 
            title = c("Reserve Winter Pastures"),
            axistype=1 , 
            #custom polygon
            pcol= ez_cols , # pfcol=colors_in, 
            plwd=2 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", 
            caxislabels= c(0, 0.2,0.4, 0.6, 0.8), 
            cglwd=0.8,
            #custom labels
            vlcex=0.8
            # centerzero = TRUE
)


# Plot Reserve Spring Pastures -------
radarchart( rs.dat  , 
            title = c("Reserve Spring Pastures"),
            axistype=1 , 
            #custom polygon
            pcol= ez_cols , # pfcol=colors_in, 
            plwd=2 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", 
            caxislabels= c(0, 0.2,0.4, 0.6, 0.8), 
            cglwd=0.8,
            #custom labels
            vlcex=0.8
            # centerzero = TRUE
)

# Plot Winter otor -------
radarchart( wo.dat  , 
            title = c("Winter otor"),
            axistype=1 , 
            #custom polygon
            pcol= ez_cols , # pfcol=colors_in, 
            plwd=2 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", 
            caxislabels= c(0, 0.2,0.4, 0.6, 0.8), 
            cglwd=0.8,
            #custom labels
            vlcex=0.8
            # centerzero = TRUE
)

  # remove NA columns to simplify the plot:
radarchart( wo.dat[, c(1,3,4,6:10)]  , 
            title = c("Winter otor"),
            axistype=1 , 
            #custom polygon
            pcol= ez_cols , # pfcol=colors_in, 
            plwd=2 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", 
            caxislabels= c(0, 0.2,0.4, 0.6, 0.8), 
            cglwd=0.8,
            #custom labels
            vlcex=0.8
            # centerzero = TRUE
)

# Plot Fall otor -------
radarchart( fo.dat  , 
            title = c("Fall otor"),
            axistype=1 , 
            #custom polygon
            pcol= ez_cols , # pfcol=colors_in, 
            plwd=2 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", 
            caxislabels= c(0, 0.2,0.4, 0.6, 0.8), 
            cglwd=0.8,
            #custom labels
            vlcex=0.8
            # centerzero = TRUE
)

ez_leg <- c("Desert Steppe", "Steppe", "Forest-Mtn St")

# Add a legend
legend(x=1.6, y=1.3, 
       legend = ez_leg, 
       title = c("ECOZONE"),
       bty = "n", pch=20 , 
       col=ez_cols , text.col = "black", 
       cex=1, pt.cex=3)