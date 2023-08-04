rm(list = ls())   

library(dplyr)
library(colorspace)
library(viridis)
library(zoib)
library(ggplot2)
library(lemon)
library(knitr)
library(tidyr)

chinMODEL <- read.csv("code outputs/1-modeled_Chinook_samples.csv")%>% rename(winter = winter_samples, spring = spring_samples, summer = summer_samples)

chinMODELdf <- pivot_longer(chinMODEL, cols = 1:3, names_to = "season", values_to = "PRCNT")
chinMODELdf$group <- "all"
chinMODELdf$orgin <- "model"

winterchin <- chinMODELdf %>% filter(season == "winter")
springchin <- chinMODELdf %>% filter(season == "spring")
summerchin <- chinMODELdf %>% filter(season == "summer")


modelchin <- quantile(chinMODELdf$PRCNT,probs = c(0.5, 0.025, 0.975, 0.25,0.75))
modelchin

win.modelchin <- quantile(winterchin$PRCNT,probs = c(0.5, 0.025, 0.975, 0.25,0.75))
win.modelchin

spr.modelchin <-  quantile(springchin$PRCNT,probs = c(0.5, 0.025, 0.975, 0.25,0.75))
spr.modelchin

sum.modelchin <-  quantile(summerchin$PRCNT,probs = c(0.5, 0.025, 0.975, 0.25,0.75))
sum.modelchin

allchinprop <- cbind(modelchin, win.modelchin, spr.modelchin, sum.modelchin)

allchinprop


chinMODELdf$PRCNT

juvMODEL <- read.csv("code outputs/2- modeled_age0_Chinook_samples.csv", header=TRUE, stringsAsFactors = FALSE)%>% rename(winter = winter_samples, spring = spring_samples, summer = summer_samples)

juvMODELdf <- pivot_longer(juvMODEL, cols = 1:3, names_to = "season", values_to = "PRCNT")
juvMODELdf$group <- "juv"
juvMODELdf$orgin <- "model"


winterchin0 <- juvMODELdf %>% filter(season == "winter")
springchin0 <- juvMODELdf %>% filter(season == "spring")
summerchin0 <- juvMODELdf %>% filter(season == "summer")

modelchin0 <- quantile(juvMODELdf$PRCNT,probs = c(0.5, 0.025, 0.975, 0.25,0.75))
modelchin0

win.modelchin0 <- quantile(winterchin0$PRCNT,probs = c(0.5, 0.025, 0.975, 0.25,0.75))
win.modelchin0

spr.modelchin0 <-  quantile(springchin0$PRCNT,probs = c(0.5, 0.025, 0.975, 0.25,0.75))
spr.modelchin0

sum.modelchin0 <-  quantile(summerchin0$PRCNT,probs = c(0.5, 0.025, 0.975, 0.25,0.75))
sum.modelchin0


chin0prop <- cbind(modelchin0, win.modelchin0, spr.modelchin0, sum.modelchin0)
chin0prop

modelonly <- rbind(chinMODELdf, juvMODELdf)

modelonly$group <- factor(modelonly$group, levels =  c('all', 'juv'), labels = c('All ages', 'Age-0 only'))

modelonly$season <- factor(modelonly$season, levels =  c('winter', 'spring', 'summer'), labels = c('Winter', 'Spring', 'Summer'))

model_plot  <- ggplot(modelonly, aes(x=season, y=PRCNT)) + 
  geom_jitter(aes(color = 2, alpha = 0.1)) +
  geom_boxplot(fill = 'lightgrey') +
  labs(x="Data Source", y="Chinook Diet proportion (RRA)") + 
  facet_grid(rows = "group") + 
  scale_fill_manual(values = grey) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        strip.text.x = element_text(size=12, color="black", face="bold"), 
        strip.text.y = element_text(size=12, color="black", face="bold"), 
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold", size=14, hjust = 0.5, vjust = 2.50),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.spacing = unit(1.15, "lines")) 


ggsave("Figures/Figure 5.pdf", model_plot, units = c("in"), width = 8, 
       height = 5, dpi = 500)


