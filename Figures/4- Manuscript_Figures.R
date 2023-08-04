##### FIGURES #####

rm(list = ls())   

library('scales')
library(dplyr)
library(colorspace)
library(viridis)
library(zoib)
library(ggplot2)
library(lemon)
library(knitr)
library(tidyr)

#### FIGURE 3
salmon <- read.csv("Figures/figure3.csv", header = T)
longdat <- pivot_longer(salmon, cols =2:4, names_to = "sample", values_to = "PRCNT")
longdat$sample <- as.factor(longdat$sample)

longdat$season <- factor(longdat$sample, levels =  c('winter', 'spring', 'summer'), labels = c('Winter', 'Spring', 'Summer'))

longdat$common <- factor(longdat$common, levels =  c( 'Chum_Salmon', 'Pink_Salmon', 'Sockeye_Salmon', 'Steelhead','Coho_Salmon','Chinook_Salmon'), labels = c( 'Chum (Oncorhynchus keta)', 'Pink (Oncorhynchus gorbuscha)', 'Sockeye (Oncorhynchus nerka)', 'Steelhead (Oncorhynchus mykiss)', 'Coho (Oncorhynchus kisutch)','Chinook'))

hybrid <- longdat %>% select(common, PRCNT, season)

seasonorder <- c('Winter', 'Spring', 'Summer')

fig3plot <- ggplot(data=hybrid, aes(x=factor(season), y=PRCNT, fill=factor(common)))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#88CCEE","#A27885","#E2A43B", "#33454e","#DDCC77", "#44AA99"))+  
  labs(x= "Season", y = "Diet Proportion", fill = "Salmon species", size = 12) + scale_y_continuous(breaks=c(0.0, 0.05,0.1, 0.15, 0.2))+ 
  theme_light() + 
  theme(text=element_text(size=30), legend.text = element_text(size = 20), legend.title = element_text(size=26), legend.spacing.y = unit(0.25, 'in')) + 
  guides(fill = guide_legend(byrow = TRUE))

ggsave("Figures/Figure3.pdf", fig3plot,  units = c("in"), width = 16, height = 10, dpi = 500)

########## FIGURE 4 
fig4dat <- read.csv("Figures/3-figure4dat.csv")

fig4plot <- ggplot(data=fig4dat, aes(x=factor(season, level= seasonorder), y=avg))+
  geom_bar(stat="identity", colour = "#74898C", size = 0)+
  geom_errorbar(data=fig4dat, aes(ymin = ci_min, ymax = ci_max), width =0.1)+ 
  geom_errorbar(data=fig4dat, aes(ymin = ci_low, ymax = ci_high), width =0.0, size = 6, col = "#E2A43B")+ 
  theme_light()  + 
  theme(text=element_text(size=12)) + 
  xlab('Season') + 
  ylab('Total Biomass of Prey Consumed (metric tons)')

fig4plot

ggsave("Figures/Figure4.pdf", fig4plot, units = c("in"), width = 6, 
       height = 4, dpi = 500)

########## FIGURE 6
allChin <- read.csv(file = "code outputs/biomass_allChinook.csv")
allChin$season <- factor(allChin$season, levels =  c('Winter_tot_chin_sum', 'Spring_tot_chin_sum', 'Summer_tot_chin_sum'), labels = c('Winter', 'Spring', 'Summer'))

age0chin <- read.csv(file = "code outputs/biomass_age0.csv")
age0chin$season <- factor(age0chin$season, levels =  c('Winter_tot_biomass_Chinook_age0_sum', 'Spring_tot_biomass_Chinook_age0_sum', 'Summer_tot_biomass_Chinook_age0_sum'), labels = c('Winter', 'Spring', 'Summer'))

allChin$group <- "all"
age0chin$group <- "juv"

fig6dat <- rbind(allChin, age0chin) %>% select(season, avg, ci_min, ci_max, ci_low, ci_high, group)

fig6dat$group <- factor(fig6dat$group, levels =  c('all', 'juv'), 
                        labels = c('All', 'Age-0 only'))

fig6plot <- ggplot(data=fig6dat, aes(x=factor(season, level= seasonorder), y=avg, fill = group)) +
  geom_bar(stat="identity", size = 0, position = "dodge", aes(fill = group)) +
  scale_fill_manual(values = c("#3b3c36","lightgray"), name = "Age-class") + 
  geom_errorbar(data=fig6dat, aes(ymin = ci_min, ymax = ci_max), width =0.1,  position = position_dodge(width = 0.9))+ 
  geom_errorbar(data=fig6dat, aes(ymin = ci_low, ymax = ci_high), width =0.0, size = 6, col = '#55AFBD', position = position_dodge(width = 0.9)) + 
  theme_light()  +  
  theme(text=element_text(size=12)) + 
  xlab('Season') + 
  ylab('Biomass of Chinook Consumed (metric tons)')

fig6plot

ggsave("Figures/Figure6.pdf", fig6plot, units = c("in"), width = 8, 
       height = 5, dpi = 500)



########## FIGURE 7
fig7dat <- read.csv("code outputs/3-countAge0Chinook_summary.csv")

fig7dat$season <- factor(fig7dat$season, levels =  c('count_WinterTot', 'count_SpringTot', 'count_SummerTot'), labels = c('Winter', 'Spring', 'Summer'))

fig7plot <- ggplot(data=fig7dat, aes(x=factor(season, level= seasonorder), y=avg))+
  geom_bar(stat="identity", colour = "#74898C", size = 0)+
  geom_errorbar(data=fig7dat, aes(ymin = ci_min, ymax = ci_max), width =0.1)+ 
  geom_errorbar(data=fig7dat, aes(ymin = ci_low, ymax = ci_high), width =0.0, size = 6, col = "#55AFBD")+ 
  theme_light()  +  
  theme(text=element_text(size=12)) + 
  scale_y_continuous(labels = scales::comma) + 
  xlab('Season') + 
  ylab('Count of Age-0 Chinook  Consumed')


fig7plot

ggsave("Figures/Figure7.pdf", fig7plot, units = c("in"), width = 6, 
       height = 5, dpi = 500)


