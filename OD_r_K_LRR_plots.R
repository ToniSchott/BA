#clear environment
rm(list=ls(all=TRUE))

#load library
library(readxl)
library(tidyverse)
library(here)

#load table
kr_comparison <- read.csv(file = here("Tables/rK_comparison.csv"))

# plot r over temp

ggplot(kr_comparison, aes( y = r, fill= as.factor(temperature)))+
  geom_boxplot()+
  scale_fill_manual("Temperature [°C]",
                  values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Temperature dependence of growth rate", y= "Growth rate")+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))
  #facet_wrap(~temperature, nrow = 1)

#save in folder figures
ggsave("Figures/OD_r_temperature.png", plot = last_plot(), width = 15, height = 8)

#plot r over nutrients and temperature

ggplot(kr_comparison, aes(x = as.factor(temperature), y = r, fill= nut ))+
  geom_boxplot()+
  scale_fill_manual( "Nutrient \n concentration",
                     values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Temperature dependence of r", x = "Temperature [°C]", y= "Growth rate")+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))

#save in folder figures
ggsave("Figures/OD_r_nut_temperature.png", plot = last_plot(), width = 15, height = 8)

# plot K over temp

#omit extreme K value 
kr_comparison[56,"K" ] <- NA 

ggplot(kr_comparison, aes( y = K, fill= as.factor(temperature)))+
  geom_boxplot()+
  scale_fill_manual("Temperature [°C]",
                    values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Temperature dependence of K", y= "capacity")+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))
  #ylim(0.07,0.2) #loescht extremen K wert (pulse9_H2)
#facet_wrap(~temperature, nrow = 1)

#save in folder figures
ggsave("Figures/OD_K_temperature.png", plot = last_plot(), width = 15, height = 8)


#plot K over nutrients and temperature

ggplot(kr_comparison, aes(x = as.factor(temperature), y = K, fill= nut ))+
  geom_boxplot()+
  scale_fill_manual( "Nutrient \n concentration",
                    values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Temperature dependence of K", x = "Temperature [°C]", y= "capacity")+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))

#save in folder figures
ggsave("Figures/OD_K_nut_temperature.png", plot = last_plot(), width = 15, height = 8)


#plot r over K

ggplot(kr_comparison, aes(x = K, y = r ))+
  geom_point()+
  #scale_fill_manual( "Nutrient \n concentration",
   #                  values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Temperature dependence of K", x = "K", y= "r")+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))
