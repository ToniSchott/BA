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


#show mean relabund over all samlpings bigger than 7
datamean <- filter(data, sampling >=7)
mean(datamean$relAbundBysampling[datamean$species== "asterio"])
mean(datamean$relAbundBysampling[datamean$species== "dity"])
mean(datamean$relAbundBysampling[datamean$species== "rhizo"])
mean(datamean$relAbundBysampling[datamean$species== "guido"])
mean(datamean$relAbundBysampling[datamean$species== "odontella"])


# for loop for relative abundance for every species and sampling
c <- c(1,7,8,11,18,24,25)
speciesvector <- c("asterio","dity","odontella","guido","rhizo")

sink(file = "Tables/relabundpercentages.txt")
for (i in c) { 
 
  datameans1 <- filter(data, sampling == i)
  print("relative abundance for sampling" )
  print(i)
  for (s in speciesvector) {
    print(s)
   print(mean(datameans1$relAbundBysampling[datameans1$species== s]))
  }
}
#revert output back to console
sink(type = "message")
sink()

#increase of Dity rel abundance from sampling 1 to maximal rel abundance (sampling 25)

x <-mean(data$relAbundBysampling[data$species== "dity" & data$sampling == "25"] )/mean(data$relAbundBysampling[data$species== "dity" & data$sampling == "1"] )  
x

#increase of Asterio rel abundance from sampling 1 to maximal rel abundance (sampling 8)

y<-mean(data$relAbundBysampling[data$species== "asterio" & data$sampling == "8"] )/mean(data$relAbundBysampling[data$species== "asterio" & data$sampling == "1"] )  
y

#decrease of ASterio rel abundance from sampling 8 to sampling 25
z<-mean(data$relAbundBysampling[data$species== "asterio" & data$sampling == "25"] )/mean(data$relAbundBysampling[data$species== "asterio" & data$sampling == "1"] )  
z


### plots all species in abhängigkeit von pulse or mix

ggplot(data, aes(x = as.factor(morp), y = relAbundBysampling, 
                 fill= as.factor(species)))+
  geom_boxplot()+
  scale_fill_manual("Species",
                    values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Pulse disturbance effect on community composition and size", 
       x="Treatment", y= "Relative species abundance \n [%]")+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))+
    facet_wrap(~sampling, nrow = 1)

#save in folder figures
ggsave("Figures/relAbund_morp_sampling.png", plot = last_plot(), width = 15, height = 8)

#plot over temperature

ggplot(data, aes(x = as.factor(morp), y = relAbundBysampling, fill = as.factor(species)))+
  geom_boxplot()+
  scale_fill_manual("Species",
                    values= c('#d95f02','#7570b3','#e7298a','#1b9e77','#66a61e'))+
  labs(title = "\n\n Pulse disturbance effect on community composition over time", 
       x="Treatment", y= "Relative species abundance \n [%]")+
  theme(
    axis.text=element_text(size=8), plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title=element_text(size=12),
    #axis.text.x = element_blank(), #tickmarks entfernen
    axis.ticks.x = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8))+
  facet_wrap(~temperature, nrow = 1)

#save in folder figures
ggsave("Figures/relAbund_morp_temperature.png", plot = last_plot(), width = 15, height = 8)

