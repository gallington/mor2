library(ggplot2)

# making the figs for the table:

load("./data/tdfg.RData")


# remove the grid lines:



# Formal Tenure x Access to other's pastures:
td.fg %>%
  filter(!is.na(hhTenureWPast)) %>%
  filter(!is.na(otherPast)) %>%
ggplot(aes(x= hhTenureWPast, y=otherPast))+
  geom_count()+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("Tenure on winter pasture")+
  ylab("Access to other pastures") +
  facet_grid(.~ez, labeller = label_both)
#add lbels to the axes

# both tenures:
a <-td.fg %>%
  filter(!is.na(hhTenureWPast)) %>%
  filter(!is.na(otherPast)) %>%
  filter(!is.na(hhTenureWCamp))%>%
  ggplot(aes(x= otherPast))+
  geom_bar(aes(fill=ez))+
  scale_fill_brewer(palette= "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("Access to other pastures")+
  #ylab("Tenure on winter pasture") +
  facet_grid(hhTenureWPast~hhTenureWCamp, labeller = label_both)

# Tenure and Rules formality:
b<- td.fg %>%
  filter(!is.na(hhTenureWPast)) %>%
  filter(!is.na(Rule)) %>%
  filter(!is.na(hhTenureWCamp))%>%
  ggplot(aes(x= Rule))+
  geom_bar(aes(fill=ez))+
  scale_fill_brewer(palette= "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("Rule Formality")+
  #ylab("Tenure on winter pasture") +
  facet_grid(hhTenureWPast~hhTenureWCamp, labeller = label_both)

# Acess to Pastures and Rules formality:
c<- td.fg %>%
  filter(!is.na(otherPast)) %>%
  filter(!is.na(Rule)) %>%
  ggplot(aes(x= Rule))+
  geom_bar()+
  #geom_bar(aes(fill=ez))+
  #scale_fill_brewer(palette= "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("Rule Formality")+
  #ylab("Tenure on winter pasture") +
  facet_grid(.~otherPast, labeller = label_both)

# Rule and social capital
  #cognitive
d<- td.fg %>%
  filter(!is.na(Rule)) %>%
  filter(!is.na(cogSC1)) %>%
  ggplot(aes(x= cogSC1))+
  geom_density(aes(color = ez), size = 1)+
  scale_color_brewer(palette = "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("cognitive social capital")+
  #ylab("Tenure on winter pasture") +
  facet_grid(. ~Rule, labeller = label_both)
  # struct
e<- td.fg %>%
  filter(!is.na(Rule)) %>%
  filter(!is.na(bondSC)) %>%
  ggplot(aes(x= bondSC))+
  geom_density(aes(color = ez), size = 1)+
  scale_color_brewer(palette = "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("structural (bonding) social capital")+
  facet_grid(. ~Rule, labeller = label_both)
# Tenure and cogSC
f<- td.fg %>%
  filter(!is.na(hhTenureWPast)) %>%
  filter(!is.na(cogSC1)) %>%
  filter(!is.na(hhTenureWCamp))%>%
  ggplot(aes(x= cogSC1))+
  geom_density(aes(color = ez), size = 1)+
  scale_color_brewer(palette= "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("cognitive social capital")+
  #ylab("Tenure on winter pasture") +
  facet_grid(hhTenureWPast ~hhTenureWCamp, labeller = label_both)

# Tenure and structSC
g<- td.fg %>%
  filter(!is.na(hhTenureWPast)) %>%
  filter(!is.na(bondSC)) %>%
  filter(!is.na(hhTenureWCamp))%>%
  ggplot(aes(x= bondSC))+
 # geom_density(aes(color = ez), size = 1)+
  geom_density(aes(color = ez), size = 1, stat = "count")+
  scale_color_brewer(palette= "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("bonding social capital")+
  #ylab("Tenure on winter pasture") +
  facet_grid(hhTenureWPast ~hhTenureWCamp, labeller = label_both)

# Access and social capital
h<- td.fg %>%
  filter(!is.na(otherPast)) %>%
  filter(!is.na(cogSC1)) %>%
  ggplot(aes(x= cogSC1))+
  geom_density(aes(color = ez), size = 1)+
  scale_color_brewer(palette= "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("cognitive social capital")+
  #ylab("Tenure on winter pasture") +
  facet_grid( ~otherPast, labeller = label_both)

# Access and sstructural (bonding) capital
i<- td.fg %>%
  filter(!is.na(otherPast)) %>%
  filter(!is.na(bondSC)) %>%
  ggplot(aes(x= bondSC))+
  #geom_density(aes(color = ez), size = 1)+
  geom_histogram(binwidth = 0.5)+
  scale_color_brewer(palette= "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,2), ylim = c(0,1))+
  xlab("bonding social capital")+
  #ylab("Tenure on winter pasture") +
  facet_grid( ~otherPast, labeller = label_both)

# Struct x Cognitive SC

j<- td.fg %>%
  filter(!is.na(cogSC1)) %>%
  filter(!is.na(bondSC)) %>%
  ggplot(aes(x= bondSC, y= cogSC1, color = ez))+
  geom_smooth( size = 1)+
  scale_color_brewer(palette= "BrBG")+
  theme_classic() +
  #coord_cartesian(xlim = c(0,5), ylim = c(0,2))+
  xlab("bonding social capital")+
  ylab("cognitive social capital") +
  facet_grid(ez~., labeller = label_both)

plot_grid(
  a, NULL, NULL, NULL,
  b, c, NULL, NULL,
  d, e, f, NULL ,
  g, h, i, j, 
  labels = "AUTO",
  ncol = 4)

