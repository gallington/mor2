### NOTE: See bottom of this script for the original code that I used to plot the dot plots of marginal effects by EZ.
# The issue w these is that they don't show confidence intervals. Also MFG found them too difficult to parse
# all together, bc there are so many, so that's what led me to try to combine info into the radar plots
# however, in those, you don't get the baseline (0) 

## NOTE: 2021/4/8:
# Ran separate models for each ez
# predicted ME and creating plots for each predictor, with each ez in it
# kind of like the original dotplots, but now with the error bars included.
# also creates new table of model fits for each ez, each practice


###NOTE: 2021/4/1
####    This is what I used to pull the effect sizes for the radar plots!!!
####    No wonder I couldn't figure out how to get them out in a clean way!!!
####    The plotLMER function reports a effect size for the parameter in the
####    console and I just copied that..... 

# 4/1/2021 End of Day notes: I think I've decided it would be better to
# scrap the radar plots and just remake the dot plots with CIs
# so that it's easier to see the base probability for each.
# so, come back and set those up. USE GGEFFECTS---> plot_grid!


##########   THE OTHER ISSUE:
    # running the original models with the ez-subset data isn't really valid?
    # the data aren't sufficient in some cases and/or parameters are no longer 
    # significant. The lack of signif i think is ok (bc we can still show lack of effect)
    # but we do need them to run....

# NEED TO MAKE A NEW (Supplement) Table that specifies each ez-specific model.

load("./data/td.RData")


# PLOT EFFECTS by ecological zone: 
#This subsets the data into ecological zones then reruns the relevant models for each ez separately... 
#Then plots the predicted probabilities of reserving based on each param separately, for each ez

#library(languageR)
library("dplyr")
library("tidyr")
library("sjPlot")
library("magrittr")
library("lme4")
library(ggeffects)
library(cowplot)

dkb<- "#a6611a"   # ds
tan<- "#dfc27d"   #es
ltgr<- "#80cdc1"  #st
dkgr<- "#018571"  # fms
pal3 <- c("#a6611a" , "#018571", "#80cdc1")
pal4
# change Access paramto a dummy variable so can examine for otor??
to <- mutate(td.fg, othersYes = case_when(otherPast == 1 ~ 0,   # no access = 0
                                          otherPast == 2 | otherPast == 3 ~ 1, # access to past in&out of soum 
                                          TRUE ~ NA_real_))  
to <- mutate(to, othersOut = case_when(otherPast == 1 | otherPast == 2 ~ 0 , 
                                       otherPast == 3 ~ 1,   # access to pastures in other soums
                                       TRUE ~ NA_real_))  
to$othersYes<- as.factor(to$othersYes)
to$othersOut<- as.factor(to$othersOut)
ds<- subset(to, to$ez==1)
#st<- subset(to, to$ez==c(2,4)) # WHY ARE THESE COMBINED??
st<- subset(to, to$ez ==2)
es<- subset(to, to$ez==3)  # not enough to use.
fms<- subset(to, to$ez==4)

# rename the variables so they make sense:
pv <- c(
  `(Intercept)` = "Intercept",
  cogSC1 = "Cognitive Social Capital",
  bondSC = "Structural Social Capital", 
  hhTenureWPast1 = "Winter Pasture Tenure", 
  hhTenureWCamp1 = "Winter Camp Tenure",
  hhTenureSpPast1 = "Spring Pasture Tenure",
  hhTenureSpCamp1 = "Spring Camp Tenure", 
  otherPast.L = "Access to Other Pastures",
  RuleYes1 = "Rules on Timing of Grazing",
  RuleInf1 = "Informal Rules on Grazing",
  RuleFormal1 = "Formal Rules on Timing",
  frg.left = "Remaining Forage Available",
  frgCV= "CV of Forage Availability",
  Rule.L = "Informal Rules",
  Rule.Q = "Formal Rules"
)
### Winter Pastures:-----------------

w1<- glmer(ResWint ~ hhTenureWPast + 
             hhTenureWCamp + 
           # RuleYes + 
             RuleInf +
          #   RuleFormal+  # only five hh said Y to this. 
           #  Rule+
             cogSC1 + 
             frgCV + 
             frg.left+   
             (1|Org) , family = binomial, data = ds)
pw1<- plot_model(w1,
          title = "Reserve Winter Pastures in the Desert Steppe", 
          sort.est = TRUE,
          auto.label = FALSE, 
          axis.labels = pv,
          colors = "bw",
         # rm.terms = "otherPast.Q",  # remove the quadratic term
          show.values = TRUE, value.offset = .3, value.size = 3
          )


w2<-glmer(ResWint ~ #hhTenureWPast + # almost all No
            hhTenureWCamp + # almost all YES
          #  RuleYes + 
          #   RuleFormal+  
            Rule+   # even
            cogSC1 + 
            #frgCV + 
            frg.left+   
            (1|Org) , family = binomial, data = st)

pw2<- plot_model(w2,
                 title = "Reserve Winter Pastures in the Steppe", 
                 sort.est = TRUE,
                 auto.label = FALSE, 
                 axis.labels = pv,
                 colors = "bw",
                 # rm.terms = "otherPast.Q",  # remove the quadratic term
                 show.values = TRUE, value.offset = .3, value.size = 3
)

w3<- glmer(ResWint ~ hhTenureWPast + 
             hhTenureWCamp + 
           #  RuleYes + 
           #   RuleFormal+  
             Rule + 
             cogSC1 + 
             frgCV + 
             frg.left+   
             (1|Org) , family = binomial, data = fms)

pw3<- plot_model(w3,
                 title = "Reserve Winter Pastures in the Forest-Mtn Steppe", 
                 sort.est = TRUE,
                 auto.label = FALSE, 
                 axis.labels = pv,
                 colors = "bw",
                 # rm.terms = "otherPast.Q",  # remove the quadratic term
                 show.values = TRUE, value.offset = .3, value.size = 3
)

########## Combined Plot of odds ratios of winter pasture predictors
orWP<- cowplot::plot_grid(pw1, pw2, pw3, nrow= 3)

#### Combined Table of WP Predictors:
tab_model(w1, w2, w3,    
          show.icc = TRUE,
          show.aic = TRUE,
          dv.labels = c("Desert Steppe", "Steppe", "Forest-Mtn Steppe"),
          pred.labels = pv,
          rm.terms = "otherPast.Q",  # remove the quadratic term
          p.style = "a",
          title = "Reserving Winter Pastures")  

# get marginal effects for each predictor, each as single plot

    # DESERT STEPPE
wp1.ds <- ggpredict(w1, "hhTenureWPast") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp2.ds <- ggpredict(w1, "hhTenureWCamp") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp3.ds <- ggpredict(w1, "RuleInf") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp4.ds <- ggpredict(w1, "cogSC1") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp5.ds <- ggpredict(w1, "frgCV") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp6.ds <- ggpredict(w1, "frg.left") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
 
# combined pltos of merginal effects of each predictor of prob of resv w past:
#meWP<- sjPlot::plot_grid(list(p1, p2, p3, p4, p5, p6), tags = TRUE)
#meWPds <- cowplot::plot_grid(p1, p2, p3, p4 ,p5, p6, nrow = 3, 
                         #  labels = "auto",
                        #   label_size = 10)
# # now add the title---------
# title <- ggdraw() + 
#   draw_label(
#     "Marginal effects of predictors on Reserving Wtr Pasture",
#     fontface = 'bold',
#     x = 0,
#     hjust = 0
#   ) +
#   theme(
#     # add margin on the left of the drawing canvas,
#     # so title is aligned with left edge of first plot
#     plot.margin = margin(0, 0, 0, 7)
#   )
# plot_grid(
#   title, meWP,
#   ncol = 1, nrow = 3
#   # rel_heights values control vertical title margins
#   #rel_heights = c(0.1, 1)
# )
# w4<- glmer(ResWint ~ #hhTenureWPast + 
#              #hhTenureWCamp + 
#              #RuleYes + 
#              #RuleInf +
#              #RuleFormal+  
#              cogSC1 + 
#              frgCV + 
#              #frg.left+   
#              (1|Org) , family = binomial, data = es)


   # STEPPE-----
wp1.st <- ggpredict(w2, "hhTenureWCamp") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp2.st <- ggpredict(w2, "Rule") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp3.st <- ggpredict(w2, "cogSC1") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp4.st <- ggpredict(w2, "frg.left") %>%
  plot(show.y.title = FALSE, show.title = FALSE)

# meWPst <- cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, 
#                              labels = "auto",
#                              label_size = 10)

       #FMS
wp1.fms <- ggpredict(w3, "hhTenureWPast") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp2.fms <- ggpredict(w3, "hhTenureWCamp") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp3.fms <- ggpredict(w3, "Rule") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp4.fms <- ggpredict(w3, "cogSC1") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp5.fms <- ggpredict(w3, "frgCV") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
wp6.fms <- ggpredict(w3, "frg.left") %>%
  plot(show.y.title = FALSE, show.title = FALSE)

# meWPfms <- cowplot::plot_grid(p1, p2, p3, p4 ,p5, p6, nrow = 3, 
#                              labels = "auto",
#                              label_size = 10)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.2) # move them .05 to the left and right



# W Pasture Tenure:
a <- as.data.frame(wp1.ds[1:2, 1:6]) %>% mutate(ez = "DesertSteppe")
b <- as.data.frame(wp1.fms[1:2, 1:6]) %>% mutate(ez = "F-M Steppe")
wpPast<- rbind(a,b)
wpPast$ez <- as.factor(wpPast$ez)

ggplot(wpPast, aes(x=x, y=predicted, colour=ez, group=ez)) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color = ez),
                width=.1, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd, size=3)+
  scale_color_manual(values = pal3)+
  labs(x= "Household Tenure on Winter Pasture", 
       y = "Predicted Prob of Resv Winter Pasture")

# W Camp:
a <- as.data.frame(wp2.ds[1:2, 1:6]) %>% mutate(ez = "DesertSteppe")
b <- as.data.frame(wp1.st[1:2, 1:6]) %>% mutate(ez = "Steppe")
c <- as.data.frame(wp2.fms[1:2, 1:6]) %>% mutate(ez = "F-M Steppe")
wpCamp<- rbind(a,b,c)
wpCamp$ez <- as.factor(wpCamp$ez)

ggplot(wpCamp, aes(x=x, y=predicted, colour=ez, group=ez)) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color = ez),
                width=.1, position=pd) +
  #  geom_line(position=pd) +
  geom_point(position=pd, size=3)+
  scale_color_manual(values = pal3)+
  labs(x= "Household Tenure on Winter Camp", 
       y = "Predicted Prob of Resv Winter Pasture")
















### Spring Pastures:-----------------

s1<- glmer(ResWint ~ hhTenureSpPast + 
             hhTenureSpCamp + 
             # RuleYes + 
             RuleInf +
             #   RuleFormal+  # only five hh said Y to this. 
             #  Rule+
             cogSC1 + 
             frgCV + 
             frg.left+   
             (1|Org) , family = binomial, data = ds)
ps1<- plot_model(s1,
                 title = "Reserve Spring Pastures in the Desert Steppe", 
                 sort.est = TRUE,
                 auto.label = FALSE, 
                 axis.labels = pv,
                 colors = "bw",
                 # rm.terms = "otherPast.Q",  # remove the quadratic term
                 show.values = TRUE, value.offset = .3, value.size = 3
)


s2<-glmer(ResWint ~ #hhTenureSpPast + # almost all No
            hhTenureSpCamp + # mix of yes and no
            #  RuleYes + 
            #   RuleFormal+  
            Rule+   # even
            cogSC1 +    # don't get much from including cogSC1 in st.
            frgCV + 
            frg.left+   
            (1|Org) , family = binomial, data = st)
ss <- getME(s2,c("theta","fixef"))
s2.1 <- update(s2,start=ss,control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e5)))

ps2<- plot_model(s2.1,
                 title = "Reserve Spring Pastures in the Steppe", 
                 sort.est = TRUE,
                 auto.label = FALSE, 
                 axis.labels = pv,
                 colors = "bw",
                 # rm.terms = "otherPast.Q",  # remove the quadratic term
                 show.values = TRUE, value.offset = .3, value.size = 3
)

s3<- glmer(ResWint ~ hhTenureSpPast + 
             hhTenureSpCamp + 
             #  RuleYes + 
             #   RuleFormal+  
             Rule + 
             cogSC1 + 
             frgCV + 
             frg.left+   
             (1|Org) , family = binomial, data = fms)
ss <- getME(s3,c("theta","fixef"))
s3.1 <- update(s3,start=ss,control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e5)))

ps3<- plot_model(s3.1,
                 title = "Reserve Spring Pastures in the Forest-Mtn Steppe", 
                 sort.est = TRUE,
                 auto.label = FALSE, 
                 axis.labels = pv,
                 colors = "bw",
                 # rm.terms = "otherPast.Q",  # remove the quadratic term
                 show.values = TRUE, value.offset = .3, value.size = 3
)

########## Combined Plot of odds ratios of spring pasture predictors
orSP<- cowplot::plot_grid(ps1, ps2, ps3, nrow= 3)


# plot marginal effects for each predictor, each as single plot

# DESERT STEPPE
sp1.ds <- ggpredict(s1, "hhTenureSpPast") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
sp2.ds <- ggpredict(s1, "hhTenureSpCamp") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
sp3.ds <- ggpredict(s1, "RuleInf") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p4 <- ggpredict(s1, "cogSC1") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p5 <- ggpredict(s1, "frgCV") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p6 <- ggpredict(s1, "frg.left") %>%
  plot(show.y.title = FALSE, show.title = FALSE)

# combined pltos of merginal effects of each predictor of prob of resv w past:
meSP<- sjPlot::plot_grid(list(p1, p2, p3, p4, p5, p6), tags = TRUE)

meSPds <- cowplot::plot_grid(p1, p2, p3, p4 ,p5, p6, nrow = 3, 
                             labels = "auto",
                             label_size = 10)

# STEPPE
p1 <- ggpredict(s2, "hhTenureSpPast") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p2 <- ggpredict(s2, "Rule") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p3 <- ggpredict(s2, "cogSC1") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p4 <- ggpredict(s2, "frg.left") %>%
  plot(show.y.title = FALSE, show.title = FALSE)

meSPst <- cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, 
                             labels = "auto",
                             label_size = 10)

#FMS
p1 <- ggpredict(s3, "hhTenureSpPast") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p2 <- ggpredict(s3, "hhTenureSpCamp") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p3 <- ggpredict(s3, "Rule") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p4 <- ggpredict(s3, "cogSC1") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p5 <- ggpredict(s3, "frgCV") %>%
  plot(show.y.title = FALSE, show.title = FALSE)
p6 <- ggpredict(s3, "frg.left") %>%
  plot(show.y.title = FALSE, show.title = FALSE)

meSPfms <- cowplot::plot_grid(p1, p2, p3, p4 ,p5, p6, nrow = 3, 
                              labels = "auto",
                              label_size = 10)













### From help for  plotLMER.fnc()--------
#   "Plot partial effects of a (generalized) linear mixed-effects model fit with 
#    lmer. For gaussian models, 95% highest posterior density credible intervals 
#   can be added."

# update on 4.1.21:
# if you just call plotLMER.fnc(w1) without specifying the predictor
# it yields all of the effect sizes at once.
# log odds are back transformed to proabilities:
# but you can't save those values to a vector or anything. Just plots them.
# so let's try using margins instead?
#library(margins)
#m <- margins(w1)   # nope that doesn't work

w1ef <-get_model_data(w1, type = "pred") # how does this compare to what is used for plots below?

#Plots of odds ratios for each model
w1p<- plot_model(w1,
               title = "DS: Reserve Winter Pastures", 
               sort.est = TRUE,
               auto.label = FALSE, 
             #  axis.labels = pv,
               colors = "bw",
            #   rm.terms = "otherPast.Q",  # remove the quadratic term
               show.values = TRUE, value.offset = .3, value.size = 3) 

...............


# Plots of marginal effects by EZ 
# I think I maybe like this best?
mewds<- plot_model(w1, type = "pred")
  ###### RESERVING WINTER PASTURES:-----
mewds<- get_model_data(w1, type = "pred") 
mewst<- get_model_data(w2, type = "pred") 
mewfms<- get_model_data(w3, type= "pred")

# Extract ME pasture tenure on Wtr Pasture
mewds.hhT<- as.data.frame(mewds$hhTenureWPast)%>% mutate(ez = c("Desert Steppe"))
mewst.hhT<- as.data.frame(mewst$hhTenureWPast)%>% mutate(ez = c("Steppe"))
mewfms.hht <- as.data.frame(mewfms$hhTenureWPast) %>% mutate(ez = c("Forest-Mtn Steppe"))

# dataframe w marginal effects of Pasture Tenure on Reserving Winter Pasture by EZ:
mew.hht<- rbind(mewds.hhT, mewst.hhT, mewfms.hht) 
mew.hht$group <- c("No", "Yes","No", "Yes","No", "Yes")
mew.hht$ez <- as.factor(mew.hht$ez)

 

# Extract ME Camp Tenure on Wtr Pasture
mewds.hhTC<- as.data.frame(mewds$hhTenureWCamp)%>% mutate(ez = c("Desert Steppe"))
mewst.hhTC<- as.data.frame(mewst$hhTenureWCamp)%>% mutate(ez = c("Steppe"))
mewfms.hhTC <- as.data.frame(mewfms$hhTenureWCamp) %>% mutate(ez = c("Forest-Mtn Steppe"))

# dataframe w marginal effects of Pasture Tenure on Reserving Winter Pasture by EZ:
mew.hhtc<- rbind(mewds.hhTC, mewst.hhTC, mewfms.hhTC)

# Extract ME RULES on Wtr Pasture
mewds.hhR<- as.data.frame(mewds$RuleInf)%>% mutate(ez = c("Desert Steppe"))
mewst.hhR<- as.data.frame(mewst$hhTenureWCamp)%>% mutate(ez = c("Steppe"))
mewfms.hhR <- as.data.frame(mewfms$hhTenureWCamp) %>% mutate(ez = c("Forest-Mtn Steppe"))

# dataframe w marginal effects of Pasture Tenure on Reserving Winter Pasture by EZ:
mew.hhtc<- rbind(mewds.hhTC, mewst.hhTC, mewfms.hhTC)








#DOTPLOTS---- 

# ORIGINAL CODE THAT I USED TO PLOT THE DOT PLOTS
# Pasture Tenure  
plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor= dkb, lwd=4,xlabel="Pasture Tenure (No/Yes)", ylabel="Probability of Reserving WPast", pred = c("hhTenureWPast")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureWPast"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureWPast"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureWPast"), addToExistingPlot = TRUE) 

# Camp Tenure  
plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Camp Tenure (No/Yes)", ylabel="Probability of Reserving WPast", pred = c("hhTenureWCamp")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureWCamp"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureWCamp"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureWCamp"), addToExistingPlot = TRUE) 

# Any Rules

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Any Rules Re: Timing of Grazing (No/Yes)", ylabel="Probability of Reserving", pred = c("RuleYes")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("RuleYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("RuleYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("RuleYes"), addToExistingPlot = TRUE) 

# Formal Rules

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="FORMAL Rules (No/Yes)", ylabel="Probability of Reserving", pred = c("RuleFormal")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("RuleFormal"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("RuleFormal"), addToExistingPlot = TRUE) 

#plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = #c("RuleFormal"), addToExistingPlot = TRUE) 


# cogSC1

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Cognitive Social Captial", ylabel="Probability of Reserving", pred = c("cogSC1")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("cogSC1"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("cogSC1"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("cogSC1"), addToExistingPlot = TRUE) 


# remaining forage

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Forage Remaining (%)", ylabel="Probability of Reserving", pred = c("frg.left")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("frg.left"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("frg.left"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("frg.left"), addToExistingPlot = TRUE) 

# CV forage

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="CV of Forage (%)", ylabel="Probability of Reserving", pred = c("frgCV")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("frgCV"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("frgCV"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("frgCV"), addToExistingPlot = TRUE) 

```


### Spring Pastures
```{r, eval = FALSE}

s1<- glmer(ResSpr ~ hhTenureSpPast + 
             hhTenureSpCamp + 
             RuleYes + 
             RuleFormal+  
             cogSC1 + 
             frgCV + 
             frg.left+   (1|Org) , family = binomial, data = ds)
s2<-glmer(ResSpr ~ hhTenureSpPast + 
            hhTenureSpCamp  + 
            RuleYes + 
            RuleFormal+  
            cogSC1 + 
            frgCV + 
            frg.left+   (1|Org) , family = binomial, data = st)
s3<- glmer(ResSpr ~ hhTenureSpPast + 
             hhTenureSpCamp  + 
             RuleYes + 
             RuleFormal+  
             cogSC1 + 
             frgCV + 
             frg.left+   (1|Org) , family = binomial, data = fms)

###### GET A SINGULARITY ERROR FOR THIS::::
s4<- glmer(ResSpr ~ #hhTenureSpPast + 
             hhTenureSpCamp + 
             RuleYes + 
             #RuleFormal+  
             cogSC1 + 
             #frgCV + 
             frg.left+   
             (1|Org) , family = binomial, data = es)

# Pasture Tenure  
plotLMER.fnc(s1,ylimit=0:1,lockYlim=TRUE,linecolor= dkb, lwd=4,xlabel="Pasture Tenure (No/Yes)", ylabel="Probability of Reserving Spr", pred = c("hhTenureSpPast")) 

plotLMER.fnc(s2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureSpPast"), addToExistingPlot = TRUE) 

plotLMER.fnc(s3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureSpPast"), addToExistingPlot = TRUE) 

plotLMER.fnc(s4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureSpPast"), addToExistingPlot = TRUE) 

# Camp Tenure  
plotLMER.fnc(s1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Camp Tenure (No/Yes)", ylabel="Probability of Reserving", pred = c("hhTenureSpCamp")) 

plotLMER.fnc(s2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureSpCamp"), addToExistingPlot = TRUE) 

plotLMER.fnc(s3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureSpCamp"), addToExistingPlot = TRUE) 

plotLMER.fnc(s4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("hhTenureSpCamp"), addToExistingPlot = TRUE) 

# Any Rules

plotLMER.fnc(s1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Rules Re:Timing of Grazing (No/Yes)", ylabel="Probability of Reserving", pred = c("RuleYes")) 

plotLMER.fnc(s2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("RuleYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(s3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("RuleYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(s4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("RuleYes"), addToExistingPlot = TRUE) 

# Formal Rules

plotLMER.fnc(s1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="FORMAL Rules (No/Yes)", ylabel="Probability of Reserving", pred = c("RuleFormal")) 

plotLMER.fnc(s2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("RuleFormal"), addToExistingPlot = TRUE) 

plotLMER.fnc(s3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("RuleFormal"), addToExistingPlot = TRUE) 

plotLMER.fnc(s4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("RuleFormal"), addToExistingPlot = TRUE) 


# cogSC1

plotLMER.fnc(s1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Cognitive Social Captial", ylabel="Probability of Reserving", pred = c("cogSC1")) 

plotLMER.fnc(s2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("cogSC1"), addToExistingPlot = TRUE) 

plotLMER.fnc(s3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("cogSC1"), addToExistingPlot = TRUE) 

plotLMER.fnc(s4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("cogSC1"), addToExistingPlot = TRUE) 


# remaining forage

plotLMER.fnc(s1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Forage Remaining (%)", ylabel="Probability of Reserving", pred = c("frg.left")) 

plotLMER.fnc(s2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("frg.left"), addToExistingPlot = TRUE) 

plotLMER.fnc(s3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("frg.left"), addToExistingPlot = TRUE) 

plotLMER.fnc(s4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("frg.left"), addToExistingPlot = TRUE) 

# CV forage

plotLMER.fnc(s1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="CV of Forage (%)", ylabel="Probability of Reserving", pred = c("frgCV")) 

plotLMER.fnc(s2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Reserving", pred = c("frgCV"), addToExistingPlot = TRUE) 

plotLMER.fnc(s3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Reserving", pred = c("frgCV"), addToExistingPlot = TRUE) 

plotLMER.fnc(s4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Reserving", pred = c("frgCV"), addToExistingPlot = TRUE) 

```


### Winter Otor
```{r}
#summary(hypwort)
w1<- glmer(Wotor  ~ hhTenureWPast + 
             hhTenureWCamp + 
             RuleYes + 
             RuleFormal+  
             bondSC +
             othersYes+
             othersOut+
             frgCV + 
             frg.left+   (1|Org) , family = binomial, data = ds)
w2<-glmer(Wotor  ~ hhTenureWPast + 
            hhTenureWCamp + 
            RuleYes + 
            RuleFormal+  
            bondSC +
            othersYes+
            othersOut+
            frgCV + 
            frg.left+   (1|Org) , family = binomial, data = st)
w3<- glmer(Wotor  ~ hhTenureWPast + 
             hhTenureWCamp + 
             RuleYes + 
             RuleFormal+  
             bondSC +
             othersYes+
             othersOut+
             frgCV + 
             frg.left+   (1|Org) , family = binomial, data = fms)
w4<- glmer(Wotor  ~ hhTenureWPast + 
             #hhTenureWCamp + 
             RuleYes + 
             #RuleFormal+  
             #bondSC +
             #othersYes+
             othersOut+
             #frgCV + 
             #frg.left+   
             (1|Org) , family = binomial, data = es)
# Pasture Tenure  
plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor= dkb, lwd=4,xlabel="Pasture Tenure (No/Yes)", ylabel="Probability of Wtr Otor", pred = c("hhTenureWPast")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("hhTenureWPast"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("hhTenureWPast"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("hhTenureWPast"), addToExistingPlot = TRUE) 


# Any Rules

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Rules Re: Timing of Grazing", ylabel="Probability of Wtr Otor", pred = c("RuleYes")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleYes"), addToExistingPlot = TRUE) 

# Formal Rules

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="FORMAL Rules (No/Yes)", ylabel="Probability of Wtr Otor", pred = c("RuleFormal")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleFormal"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleFormal"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleFormal"), addToExistingPlot = TRUE) 

# Access to other pastures

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Access to other pastures (No/Yes)", ylabel="Probability of Wtr Otor", pred = c("othersYes")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersYes"), addToExistingPlot = TRUE) 

# Access to other pastures in another soum

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Access to other soum pastures", ylabel="Probability of Wtr Otor", pred = c("othersOut")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersOut"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersOut"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersOut"), addToExistingPlot = TRUE) 


# bondSC

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="bonding SC", ylabel="Probability of Wtr Otor", pred = c("bondSC")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("bondSC"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("bondSC"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("bondSC"), addToExistingPlot = TRUE) 

# frg left

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Remaining forage", ylabel="Probability of Wtr Otor", pred = c("frg.left")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frg.left"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frg.left"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frg.left"), addToExistingPlot = TRUE) 

# frg CV

plotLMER.fnc(w1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Forage CV", ylabel="Probability of Wtr Otor", pred = c("frgCV")) 

plotLMER.fnc(w2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frgCV"), addToExistingPlot = TRUE) 

plotLMER.fnc(w3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frgCV"), addToExistingPlot = TRUE) 

plotLMER.fnc(w4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frgCV"), addToExistingPlot = TRUE) 
```
Fall Otor
```{r}
#summary(hypwort)
f1<- glmer(Fotor ~ cogSC1 + 
             bondSC + 
             othersYes + 
             othersOut +
             RuleFormal + 
             RuleYes +  
             frgCV + 
             frg.left + (1 | Org) , family = binomial, data = ds)

f2<- glmer(Fotor ~ cogSC1 + 
             bondSC + 
             othersYes + 
             othersOut +
             RuleFormal + 
             RuleYes +  
             frgCV + 
             frg.left + (1 | Org) , family = binomial, data = st)
f3<- glmer(Fotor ~ cogSC1 + 
             bondSC + 
             othersYes + 
             othersOut +
             RuleFormal + 
             RuleYes +  
             frgCV + 
             frg.left + (1 | Org) , family = binomial, data = fms)
f4<- glmer(Fotor ~ cogSC1 + 
             bondSC + 
             othersYes + 
             othersOut +
             RuleFormal + 
             RuleYes +  
             frgCV + 
             frg.left + (1 | Org) , family = binomial, data = es)


# Formal Rules

plotLMER.fnc(f1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="FORMAL Rules (No/Yes)", ylabel="Probability of Fall Otor", pred = c("RuleFormal")) 

plotLMER.fnc(f2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleFormal"), addToExistingPlot = TRUE) 

plotLMER.fnc(f3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleFormal"), addToExistingPlot = TRUE) 

plotLMER.fnc(f4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleFormal"), addToExistingPlot = TRUE) 


# Any Rules

plotLMER.fnc(f1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Rules Re: Timing of Grazing", ylabel="Probability of Fall Otor", pred = c("RuleYes")) 

plotLMER.fnc(f2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(f3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(f4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("RuleYes"), addToExistingPlot = TRUE) 

# Access to other pastures

plotLMER.fnc(f1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Access to other pastures (No/Yes)", ylabel="Probability of Fall Otor", pred = c("othersYes")) 

plotLMER.fnc(f2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(f3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersYes"), addToExistingPlot = TRUE) 

plotLMER.fnc(f4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersYes"), addToExistingPlot = TRUE) 

# Access to other pastures in another soum

plotLMER.fnc(f1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Access to other soum pastures", ylabel="Probability of Fall Otor", pred = c("othersOut")) 

plotLMER.fnc(f2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersOut"), addToExistingPlot = TRUE) 

plotLMER.fnc(f3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersOut"), addToExistingPlot = TRUE) 

plotLMER.fnc(f4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("othersOut"), addToExistingPlot = TRUE) 

# Remaining forage

plotLMER.fnc(f1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Remaining Forage (%)", ylabel="Probability of Fall Otor", pred = c("frg.left")) 

plotLMER.fnc(f2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frg.left"), addToExistingPlot = TRUE) 

plotLMER.fnc(f3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frg.left"), addToExistingPlot = TRUE) 

plotLMER.fnc(f4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frg.left"), addToExistingPlot = TRUE) 

# CV of forage

plotLMER.fnc(f1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="CV of Forage (%)", ylabel="Probability of Fall Otor", pred = c("frgCV")) 

plotLMER.fnc(f2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frgCV"), addToExistingPlot = TRUE) 

plotLMER.fnc(f3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frgCV"), addToExistingPlot = TRUE) 

plotLMER.fnc(f4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("frgCV"), addToExistingPlot = TRUE) 


# cogSC

plotLMER.fnc(f1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Cognitive SC", ylabel="Probability of Fall Otor", pred = c("cogSC1")) 

plotLMER.fnc(f2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("cogSC1"), addToExistingPlot = TRUE) 

plotLMER.fnc(f3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("cogSC1"), addToExistingPlot = TRUE) 

plotLMER.fnc(f4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("cogSC1"), addToExistingPlot = TRUE) 


# bondSC

plotLMER.fnc(f1,ylimit=0:1,lockYlim=TRUE,linecolor=dkb, lwd=4,xlabel="Bonding SC", ylabel="Probability of Fall Otor", pred = c("bondSC")) 

plotLMER.fnc(f2,ylimit=0:1,lockYlim=TRUE,linecolor= tan, lwd=4, ylabel="Probability of Wtr Otor", pred = c("bondSC"), addToExistingPlot = TRUE) 

plotLMER.fnc(f3,ylimit=0:1,lockYlim=TRUE,linecolor=ltgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("bondSC"), addToExistingPlot = TRUE) 

plotLMER.fnc(f4,ylimit=0:1,lockYlim=TRUE,linecolor=dkgr, lwd=4, ylabel="Probability of Wtr Otor", pred = c("bondSC"), addToExistingPlot = TRUE)

```
