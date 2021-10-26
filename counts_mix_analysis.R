#Environment leeren
rm(list=ls(all=TRUE))

#Libraries:
#Load packages
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(effsize)
library(scales)
library(vegan)
library(dplyr)

#import dataset

data <- read_delim("Tables/counts_mix_structured.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
data <- select( data, -...1,-info)
data$temperature <- as.factor(data$temperature)
data$morp <- as.factor(data$morp)
data$nutrients <- as.factor(data$nutrients)
data$species <- as.factor(data$species)
data$sampling <- as.factor(data$sampling)

#divide into samplings and
#calculate relative abundance for each sampling and sample
c <- c(1,7,8,11,18,24,25)

for (i in c) {
 name <-  paste("data_s",i, sep = "")
  data_s <- data %>% filter(sampling == i)  %>% group_by( sample) %>% 
    mutate(relAbundBysampling = (cells_ml / sum(cells_ml)))
}


data_s1 <- data %>% filter(sampling == 1)
data_s7 <- data %>% filter(sampling == 7)
data_s8 <- data %>% filter(sampling == 8)
data_s11 <- data %>% filter(sampling == 11)
data_s18 <- data %>% filter(sampling == 18)
data_s24<- data%>% filter(sampling == 24)
data_s25 <- data%>% filter(sampling == 25)

data_s1 <- data_s1  %>% group_by( sample) %>% 
  mutate(relAbundBysampling = (cells_ml / sum(cells_ml)))
data_s7 <- data_s7  %>% group_by( sample) %>% 
  mutate(relAbundBysampling = (cells_ml / sum(cells_ml)))
data_s8 <- data_s8  %>% group_by( sample) %>% 
  mutate(relAbundBysampling = (cells_ml / sum(cells_ml)))
data_s11 <- data_s11  %>% group_by( sample) %>% 
  mutate(relAbundBysampling = (cells_ml / sum(cells_ml)))
data_s18 <- data_s18  %>% group_by( sample) %>% 
  mutate(relAbundBysampling = (cells_ml / sum(cells_ml)))
data_s24 <- data_s24  %>% group_by( sample) %>% 
  mutate(relAbundBysampling = (cells_ml / sum(cells_ml)))
data_s25 <- data_s25  %>% group_by( sample) %>% 
  mutate(relAbundBysampling = (cells_ml / sum(cells_ml)))


#### compare community after pulse for pulse and mix####

##### Sampling 8 ####
ungroup(data_s8)
#1) hist(data) & qqnorm(data)
hist(data_s8$relAbundBysampling)
qqnorm(data_s8$relAbundBysampling)
qqline(data_s8$relAbundBysampling)

#"not really" equally distributed


#2) 
aov_s8<-aov((data_s8$relAbundBysampling)~nutrients*morp*species*temperature, data = data_s8)
summary(aov_s8)




#### Sampling 11 #####
ungroup(data_s11)
#1) hist(data) & qqnorm(data)
hist(data_s11$relAbundBysampling)
qqnorm(data_s11$relAbundBysampling)
qqline(data_s11$relAbundBysampling)

#"not really" equally distributed


#2) 
aov_s11<-aov((data_s11$relAbundBysampling)~nutrients*morp*species*temperature, data = data_s11)
summary(aov_s11)

#### Sampling 18 ####
ungroup(data_s18)
#1) hist(data) & qqnorm(data)
hist(data_s18$relAbundBysampling)
qqnorm(data_s18$relAbundBysampling)
qqline(data_s18$relAbundBysampling)

#"not really" equally distributed


#2) 
aov_s18<-aov((data_s18$relAbundBysampling)~nutrients*morp*species*temperature, data = data_s18)
summary(aov_s18)

#### Sampling 24 ####
ungroup(data_s24)
#1) hist(data) & qqnorm(data)
hist(data_s24$relAbundBysampling)
qqnorm(data_s24$relAbundBysampling)
qqline(data_s24$relAbundBysampling)

#"not really" equally distributed


#2) 
aov_s24<-aov((data_s24$relAbundBysampling)~nutrients*morp*species*temperature, data = data_s24)
summary(aov_s24)

# Sampling 25
ungroup(data_s25)
#1) hist(data) & qqnorm(data)
hist(data_s25$relAbundBysampling)
qqnorm(data_s25$relAbundBysampling)
qqline(data_s25$relAbundBysampling)

#"not really" equally distributed



#2) 
aov_s25<-aov((data_s25$relAbundBysampling)~nutrients*species*temperature, data = data_s25)
summary(aov_s25)
TukeyHSD(aov_s25)


#save anova summaries in a .txt file
#sink(file = "Tables/anova_results.txt", append = F)
print("Sampling 8")
summary(aov_s8)
print("Sampling 11")
summary(aov_s11)
print("Sampling 18")
summary(aov_s18)
print("Sampling 24")
summary(aov_s24)
print("Sampling 25")
summary(aov_s25)
#sink()
#sink TukeyHSD summaries in a new .txt file
#sink(file = "Tables/tukeyhsd_results.xlsx",  append = F)
print("Sampling 8")
TukeyHSD(aov_s8)
print("Sampling 11")
TukeyHSD(aov_s11)
print("Sampling 18")
TukeyHSD(aov_s18)
print("Sampling 24")
TukeyHSD(aov_s24)
print("Sampling 25")
TukeyHSD(aov_s25)
#sink()


