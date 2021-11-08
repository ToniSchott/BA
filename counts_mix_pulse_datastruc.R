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

data <- read_excel("counts_mix_rawdata.xlsx", 
                               col_types = c("numeric", "numeric", "text", 
                                             "numeric", "numeric", "text", "text", "numeric", "text"))

str(data)

#### datastrucutre ####

##Zellzahl auf 1 ml berechnen und in neue Spalte

data <- mutate(data, cells_ml = if_else(data$magnification == 10,(data$counts*63.9)/(1*1*data$gf*0.5),
                                        if_else(data$magnification == 20,(data$counts*63.9)/(0.5*0.5*data$gf*0.5),
                                        if_else(data$magnification == 40,(data$counts*63.9)/(0.25*0.25*data$gf*0.5), NULL))))

#Zusaetzlich Temp und nutrient conc. in neuen spalten hinzufuegen


data <- mutate(data, temperature = if_else(data$sample <=108 , 9,
                                           if_else(data$sample >=157 & data$sample <=168, 12,
                                                   if_else(data$sample  >=217 & data$sample <=228, 15,
                                                           if_else(data$sample  >=277 & data$sample <=288, 18,
                                                                   if_else(data$sample  >=337 & data$sample <=348, 21, NULL))))))

# for nut neue tabelle mit nutspalte einladen

data_nut <- read_excel("TPC_Masters_ID.xlsx")

data_nut <- select(data_nut, Sample,Nut)

#sampling ID identisch mit dem von data machen

data_nut$Sample <- data_nut$Sample - 1000

#namen der Spalten identisch machen
data_nut <- rename(data_nut, sample= Sample, nutrients = Nut)

#mit left joint nutrientspalte and data anf?gen abh?ngig von samlpingID

data <- left_join(data, data_nut, by = "sample")

#als neue Tabelle speichern

write.csv2(data, file = "counts_mix_structured.csv")




