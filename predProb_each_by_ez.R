# DEPRECATED

###NOTE: 2021/4/1
####    This is what I used to pull the effect sizes for the radar plots!!!
####    No wonder I couldn't figure out how to get them out in a clean way!!!
####    The plotLMER function reports a effect size for the parameter in the
####    console and I just copied that..... 


# 4/1/2021 End of Day notes: I think I've decided it would be better to
# scrap the radar plots and just remake the dot plots with CIs
# so that it's easier to see the base probability for each.
# so, come bback and set those up.
# Or, if Maria really wants the radar plots, can recreate with predicted below?

# NEED TO MAKE A NEW (Supplement) Table that specifies each ez-specific model.




# PLOT EFFECTS by ecological zone: 
#This subsets the data into ecological zones then reruns the relevant models for each ez separately... 
#Then plots the predicted probabilities of reserving based on each param separately, for each ez

library(languageR)
dkb<- "#a6611a"
tan<- "#dfc27d"
ltgr<- "#80cdc1"
dkgr<- "#018571"
#based on EZs:
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
es<- subset(to, to$ez==3)
fms<- subset(to, to$ez==4)

### Winter Pastures:

w1<- glmer(ResWint ~ hhTenureWPast + 
             hhTenureWCamp + 
           # RuleYes + 
            RuleInf +
          #   RuleFormal+  # only five hh said Y to this. 
             cogSC1 + 
             frgCV + 
             frg.left+   
             (1|Org) , family = binomial, data = ds)
w2<-glmer(ResWint ~ hhTenureWPast + 
            hhTenureWCamp + 
            RuleYes + 
            RuleFormal+  
            cogSC1 + 
            frgCV + 
            frg.left+   
            (1|Org) , family = binomial, data = st)
w3<- glmer(ResWint ~ hhTenureWPast + 
             hhTenureWCamp + 
             RuleYes + 
             RuleFormal+  
             cogSC1 + 
             frgCV + 
             frg.left+   
             (1|Org) , family = binomial, data = fms)
# w4<- glmer(ResWint ~ #hhTenureWPast + 
#              #hhTenureWCamp + 
#              #RuleYes + 
#              #RuleInf +
#              #RuleFormal+  
#              cogSC1 + 
#              frgCV + 
#              #frg.left+   
#              (1|Org) , family = binomial, data = es)

### From help for  plotLMER.fnc()
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
               show.values = TRUE, value.offset = .3, value.size = 3
) 

...............


# Plots of marginal effects by EZ 
# I think I maybe like this best?
mewds<- plot_model(w1, type = "pred")
mewds<- get_model_data(w1, type = "pred") 
mewst<- get_model_data(w2, type = "pred") 
mewfms<- get_model_data(w3, type= "pred")
mewds.hhT<- as.data.frame(mewds$hhTenureWPast)%>% mutate(ez = c("Desert Steppe"))
mewst.hhT<- as.data.frame(mewst$hhTenureWPast)%>% mutate(ez = c("Steppe"))
mewfms.hht <- as.data.frame(mewfms$hhTenureWPast) %>% mutate(ez = c("Forest-Mtn Steppe"))

# dataframe w marginal effects of Pasture Tenure on Reserving Winter Pasture by EZ:
mew.hht<- rbind(mewds.hhT, mewst.hhT, mewfms.hht)
 






#ORIGINAL CODE TO PLOT THE DOT PLOTS--- 
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
