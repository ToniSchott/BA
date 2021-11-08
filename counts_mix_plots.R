#Environment leeren

rm(list=ls(all=TRUE))
#Libraries:
#Load packages
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)


#import dataset

data <- read_delim("Tables/counts_mix_structured.csv", 
                                    ";", escape_double = FALSE, trim_ws = TRUE)

data <- select( data, -...1)

# calculate relative abundance (in %) per sampling and sample

data <- data  %>% group_by( sample, sampling, nutrients,temperature,morp ) %>% 
  mutate(relAbundBysampling = (cells_ml / sum(cells_ml))*100)

#rename mix into control
data$morp[data$morp == "mix"] <- "control"


### plots all species and comparing pulse and mix

panel_labels <- c( "asterio" = "Asterionellopsis glaciales","dity" = "Ditylum brightwellii" ,
                   "rhizo"="Rhizosolenia setigera" ,"odontella" = "Odontella sinensis","guido"="Guinardia striata" )

ggplot(data, aes(x = as.factor(morp), y = relAbundBysampling, 
                 fill= as.factor(species)))+
  geom_boxplot()+
  scale_fill_manual("Species", labels = panel_labels,
                    values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Pulse disturbance effect on community composition", 
       x="Treatment", y= "Relative species abundance \n [%]")+
  theme_bw()+
  theme(legend.position = 'bottom')+
  theme(
    axis.text=element_text(size=13), 
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title=element_text(size=16),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=15),
    legend.text = element_text(size=12))+
  guides(fill= guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~sampling)



#plot over temperature



ggplot(data, aes(x = as.factor(temperature), y = relAbundBysampling, 
                 fill= as.factor(species)))+
  geom_boxplot()+
  scale_fill_manual("Species", labels = panel_labels,
                    values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Temperature effect on community composition", 
       x="Temperature", y= "Relative species abundance \n [%]")+
  theme_bw()+
  theme(legend.position = 'bottom')+
  theme(
    axis.text=element_text(size=13), 
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title=element_text(size=16),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=15),
    legend.text = element_text(size=12))+
  guides(fill= guide_legend(nrow = 2, byrow = TRUE))



