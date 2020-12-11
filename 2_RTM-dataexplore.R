load("./data/tdfg.RData")
# NOV 2020 test models:

library(modelr)


# OLDER CROSSTABS STUFF

# Can't figure out how to get it to create a nice looking output.
# need to add more to table specifications and also set labels

library(qwraps2)
options(qwraps2_markup = "markdown")
sumdf<- dfSummary(td)
sumdf

view(dfSummary(td, plain.ascii = FALSE))
sumdf<- td[c(1:2, 8:9, 11, 14, 16, 19:21)]
summary_table(sumdf, )

td %>%            # this needs to be adjusted, it's pulling the wrong vars:
  dplyr::select(c(1:2, 8:9, 11, 16, 19:21)) %>%
  summary_table(.)

dfSummary(td, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")


######### SOMEHOW LOST ALL OF THE REST OF THE CODE? NEED TO PULL IT FROM A PREVIOUS VERSION OF RMReport

# more crosstabs:
tpcw<- table(td.fg$hhTenureWPast, td.fg$hhTenureWCamp) 
chisq.test(tpcw)



tpcs<- table(td$hhTenureSpPast, td$hhTenureSpCamp)
chisq.test(tpcs)

# PRACTICES BY EZ:
wrez<- table(td.fg$ResWint, td.fg$ez) 
srez<- table(td.fg$ResSpr, td.fg$ez) 
woez<- table(td.fg$Wotor, td.fg$ez) 
foez<- table(td.fg$Fotor, td.fg$ez) 

# until then, here's some new plots:
  # TO DO: update color palette

wsc<- ggplot(td, aes(cogSC1, as.numeric(ResWint)-1, color=ez)) +
    stat_smooth(method="glm", family=binomial, formula=y~x,
    alpha=0.2, size=2, aes(fill=ez)) +
    geom_point(position=position_jitter(height=0.03, width=0)) +
    xlab("cog soc cap") + ylab("Pr (reserve)")

ssc<- ggplot(td, aes(cogSC1, as.numeric(ResSpr)-1, color=ez)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=ez)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("cog soc cap") + ylab("Pr (reserve S pasture)")


wp<- ggplot(data = subset(td, !is.na(hhTenureWPast)), aes(as.numeric(Rule), as.numeric(ResWint)-1, color=hhTenureWPast)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=hhTenureWPast)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Rule formality") + ylab("Pr (reserve)")

wp + facet_grid(. ~ ez)

wc<- ggplot(data = subset(td, !is.na(hhTenureWCamp)), aes(as.numeric(Rule), as.numeric(ResWint)-1, color=hhTenureWCamp)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=hhTenureWCamp)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Rule formality") + ylab("Pr (reserve W camp)")

wc + facet_grid(. ~ ez)

sp<- ggplot(data = subset(td, !is.na(hhTenureSpPast)), aes(as.numeric(Rule), as.numeric(ResSpr)-1, color=hhTenureSpPast)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=hhTenureSpPast)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Rule formality") + ylab("Pr (reserve Sp pasture)")
sp + facet_grid(.~ez)

sc<- ggplot(data = subset(td, !is.na(hhTenureSpCamp)), aes(as.numeric(Rule), as.numeric(ResSpr)-1, color=hhTenureSpCamp)) +
  stat_smooth(method="glm", family=binomial, formula=y~x,
              alpha=0.2, size=2, aes(fill=hhTenureSpCamp)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Rule formality") + ylab("Pr (reserve Sp pasture)")
sc + facet_grid(. ~ez)


# simple grid
plot_grid(wp, wc, sp, sc)

# simple grid with labels and aligned plots
plot_grid(
  wp, wc, sp, sc,
  labels = c('A', 'B', 'C', 'D'),
  align="hv"
)


# histograms of cogSC facet wrapped by Rule & ez
ggplot(td, aes(x=cogSC1))+
  geom_histogram()+
  facet_wrap(~ez+Rule, labeller = label_both)

ggplot(wDat, aes(x=cogSC1))+
  geom_histogram()+
  facet_wrap(~hhTenureWPast + Rule, labeller = label_both)

df.bestSp2<- bestSp2@frame
spDat <- data.frame(bestSp2@frame, resid=residuals(bestSp2,type="pearson"),fitted=fitted(bestSp2))


ggplot(spDat, aes(x=cogSC1))+
  geom_histogram()+
  facet_wrap(~hhTenureSpPast + Rule, labeller = label_both)

# Figure to compare cogSCXRule formality by Tenure status: