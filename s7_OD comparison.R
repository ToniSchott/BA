# OD comparison of m and p on s7

#clear environment
rm(list=ls(all=TRUE))

#Libraries:
#Load packages
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(equivalence)

#import dataset OD of mix and pulse on sampling 7
data <- read_csv("~/Uni/ICBM_Bachelorarbeit/DataForAnalysis/Toni/MixPulse25.csv")
str(data)

#filter s7 and OD = 440

data_s7 <- filter(data, sampling == 7, method == "OD",wavelength1 == 440)


#import blank plates dataset ANMERKUNG!! keine blanks für mix und pulse plates gemacht, 
#da bei jedem zweiten sampling dafür neue plates verwendet wurden
#daher hier mean values der kalibrierung der anderen plates verwendet
AllBlanks <- read_csv("~/Uni/ICBM_Bachelorarbeit/DataForAnalysis/data_files_tpc/AllBlanks.csv")
#View(AllBlanks)

# mean of values grouped
AllBlanks <- AllBlanks %>% group_by(wavelength1,plate,plateRow,plateColumn,method) %>%
  mutate(meanbygroup = mean(value))

AllBlanks <- ungroup(AllBlanks)  

#filter one temperature, since they all have the same mean values
AllBlanks <- filter(AllBlanks, temperature == 9)

#filter unnötige wells und spalten
AllBlanks <- AllBlanks %>% filter( treatment_ID != "MQ", method == "OD", wavelength1 == 440) %>%
  dplyr::select(wavelength1, meanbygroup, plate, method ) %>%
  rename(valueBlank = meanbygroup, plateBlank = plate)

# join df by row names 

data_joined <- left_join(data_s7 %>% mutate(Symbol = rownames(data_s7)),
                         AllBlanks %>% mutate(Symbol = rownames(data_s7)),by = c("Symbol","wavelength1","method"))

#omit irelevant column
data_joined <- dplyr::select(data_joined, -...1)

# subtract blank OD value from s7 OD values
data_real_OD <- mutate(data_joined, value = value - valueBlank)

#### compare OD values of mix with OD values of pulse ####

#check pulse for normality
hist(data_real_OD$value[data_real_OD$treatment == "pulse"])
qqnorm(data_real_OD$value[data_real_OD$treatment == "pulse"])
qqline(data_real_OD$value[data_real_OD$treatment == "pulse"])

#check MIX for normality
hist(data_real_OD$value[data_real_OD$treatment == "MIX"])
qqnorm(data_real_OD$value[data_real_OD$treatment == "MIX"])
qqline(data_real_OD$value[data_real_OD$treatment == "MIX"])

#TOSTER test for equality of two populations (mix and pulse)
pulse <- filter(data_real_OD, treatment == "pulse")
mix <- filter(data_real_OD, treatment == "MIX")

tost <- rtost(x = pulse$value, y = mix$value, epsilon = 1, var.equal = F)

tost
# significant similarity between the two treatments before the heatwave

