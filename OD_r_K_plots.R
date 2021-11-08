#clear environment
rm(list=ls(all=TRUE))

#load library
library(readxl)
library(tidyverse)
library(here)

# OD over time
All_sampling <- read_csv("~/Uni/ICBM_Bachelorarbeit/DataForAnalysis/Toni/MixPulse25.csv")

#omit replica 3 high mix 21°C!!!!
All_sampling <- filter(All_sampling, treatment != "MIX"| nut != "HI" | temperature != 21| rep != 3)


# Data wrangling
DataPlot <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% #filter wavelength
  group_by(sampling, treatment,temperature, nut) %>% #group data
  mutate(value = as.numeric(value))%>% #change variable format
  mutate(sampling = as.numeric(sampling))%>%
  summarise(meanValue = mean(value), #Mittelwerte berechnen
            sdValue = sd(value)) #creates new colum  with mean and sd Value


panel_labels <- c("pulse"="pulse", "MIX"="control")

ggplot(DataPlot, aes(x = sampling, y = meanValue, color = treatment))+
  geom_line(linetype = 'dashed')+
  geom_errorbar(aes(ymin = meanValue- sdValue, ymax = meanValue + sdValue), width = .5, position = position_dodge(width = .5))+
  geom_point(size = 2.5, position = position_dodge(width = .5))+
  labs(title  = "\n\nOptical Density over time", x = 'Sampling', y = 'Optical Density')+
  #scale_x_continuous(limits = c(0, 25))+
  scale_color_manual(values = c('#d95f02','#7570b3'), labels= panel_labels, name = "Treatment")+
  facet_grid(~temperature~nut)+
  annotate("rect", xmin = 7, xmax = 8, ymin = 0, ymax = 0.15, 
           alpha = .5)  + #set pulse disturbance as grey horizontal bar
  theme_bw()+
  theme(
    axis.text=element_text(size=9), 
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    #axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=11))


# r and K plots

#load table
kr_comparison <- read.csv(file = here("Tables/rK_comparison.csv"))


#rename Hi into High
kr_comparison$nut[kr_comparison$nut == "HI"] <- "High"

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


#plot r over nutrients and temperature

ggplot(kr_comparison, aes(x = as.factor(temperature), y = r, fill= nut ))+
  geom_boxplot()+
  scale_fill_manual( "Nutrient \nconcentration",
                     values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Temperature and nutrient dependence of growth rate", x = "Temperature [°C]", y= "r")+
  theme_bw()+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))

# plot K over temp

#omit extreme K value 
kr_comparison[55,"K" ] <- NA 

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



#plot K over nutrients and temperature

#plot K over nutrients and temperature

ggplot(kr_comparison, aes(x = as.factor(temperature), y = K, fill= nut ))+
  geom_boxplot()+
  scale_fill_manual( "Nutrient \nconcentration",
                     values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Temperature dependence of K", x = "Temperature [°C]", y= "K")+
  theme_bw()+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))



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
