# Skript to analyse Pulse and Mix treatments

#clear environment
rm(list=ls(all=TRUE))

library(readxl)
library(tidyverse)
library(here)

######## starte von hier und lese erstellten DF ein ######
All_sampling <- read_csv("~/Uni/ICBM_Bachelorarbeit/DataForAnalysis/Toni/MixPulse16.csv")

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



#plotten der Daten
ggplot(DataPlot, aes(x = sampling, y = meanValue, color = treatment))+
  geom_line(linetype = 'dashed')+
  geom_errorbar(aes(ymin = meanValue- sdValue, ymax = meanValue + sdValue), width = .5, position = position_dodge(width = .5))+
  geom_point(size = 2.5, position = position_dodge(width = .5))+
  labs(x = 'Sampling', y = 'OD')+
  scale_x_continuous(limits = c(0, 16), breaks = c(1,3,5,7,9,11,13,15))+
 # scale_color_manual(values = c('#ffdb58', '#15A3C7'))+
  facet_grid(~temperature~nut)+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(plot = last_plot(), file = 'Figures/Growth_MIXpulse_sampling16OD.png', width = 6, height = 4)



# curve fitting for MIX and pulse data #####

#Mix 21°C Low rep 1
Mix21_L1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_21_Low_1')
N0s<- median(Mix21_L1$value[Mix21_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix21_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix21_L1$value[Mix21_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix21_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                          start=list(N0=N0s,K=Ks,r=rs),
                          data = Mix21_L1)
summary(Mix21_L1_model)

sum_modeL <-summary(Mix21_L1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix21_L1 $N0 <- N0
Mix21_L1 $K <- K
Mix21_L1 $r <- r
Mix21_L1 $pK <- pK
Mix21_L1 $pr <- pr
ggplot(Mix21_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix21_L1, aes(x = sampling, y = predict(Mix21_L1_model)))

#Mix 21°C Low rep 2
Mix21_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_21_Low_2')
N0s<- median(Mix21_L2$value[Mix21_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix21_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix21_L2$value[Mix21_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix21_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix21_L2)
summary(Mix21_L2_model)

sum_modeL <-summary(Mix21_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix21_L2 $N0 <- N0
Mix21_L2 $K <- K
Mix21_L2 $r <- r
Mix21_L2 $pK <- pK
Mix21_L2 $pr <- pr
ggplot(Mix21_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix21_L2, aes(x = sampling, y = predict(Mix21_L2_model)))

#Mix 21°C Low rep 3
Mix21_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_21_Low_3')
N0s<- median(Mix_21_L3$value[Mix_21_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix_21_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix_21_L3$value[Mix_21_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix_21_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix_21_L3)
summary(Mix_21_L3_model)

sum_modeL <-summary(Mix_21_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix_21_L3 $N0 <- N0
Mix_21_L3 $K <- K
Mix_21_L3 $r <- r
Mix_21_L3 $pK <- pK
Mix_21_L3 $pr <- pr
ggplot(Mix_21_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix_21_L3, aes(x = sampling, y = predict(Mix_21_L3_model)))

#Mix 21°C High rep 1
Mix21_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_21_HI_1')
N0s<- median(Mix21_H1$value[Mix21_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix21_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix21_H1$value[Mix21_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix21_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix21_H1)
summary(Mix21_H1_model)

sum_modeL <-summary(Mix21_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix21_H1 $N0 <- N0
Mix21_H1 $K <- K
Mix21_H1 $r <- r
Mix21_H1 $pK <- pK
Mix21_H1 $pr <- pr
ggplot(Mix21_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix21_H1, aes(x = sampling, y = predict(Mix21_H1_model)))

#Mix 21°C Hi rep 2
Mix21_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_21_HI_2')
N0s<- median(Mix21_H2$value[Mix21_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix21_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix21_H2$value[Mix21_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix21_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix21_H2)
summary(Mix21_H2_model)

sum_modeL <-summary(Mix21_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix21_H2 $N0 <- N0
Mix21_H2 $K <- K
Mix21_H2 $r <- r
Mix21_H2 $pK <- pK
Mix21_H2 $pr <- pr
ggplot(Mix21_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix21_H2, aes(x = sampling, y = predict(Mix21_H2_model)))

#Mix 21°C Hi rep 3 geht noch nicht!!!!!!!
#Mix21_H3 <- All_sampling %>%
#  filter(wavelength1 %in% c('440')) %>% 
#  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
#         log = log(value),
#         sampling = as.numeric(sampling)) %>%
#  filter(treatmentID == 'MIX_21_HI_3')
#N0s<- median(Mix21_H3$value[Mix21_H3$sampling==1] )#Set the start OD-value (Day=1)
#Daymax<-max(Mix21_H3$sampling) #Select the maximum number of days (time of incubation)
#Ks<-Mix21_H3$value[Mix21_H3$sampling==Daymax]
##Estimated value for K (Carrying Capacity) = Time of incubation
#rs<-0.2
#Estimated growth rate (r)
#Mix21_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
#                      start=list(N0=N0s,K=Ks,r=rs),
#                      data = Mix21_H3)
#summary(Mix21_H3_model)
#
#sum_modeL <-summary(Mix21_H3_model)
#N0 <- sum_modeL$coefficients[1,1] #N0
#K  <- sum_modeL$coefficients[2,1] #K
#r  <- sum_modeL$coefficients[3,1] #r
#pK  <- sum_modeL$coefficients[2,4] #pK
#pr  <- sum_modeL$coefficients[3,4] #pr
#Mix21_H3 $N0 <- N0
#Mix21_H3 $K <- K
#Mix21_H3 $r <- r
#Mix21_H3 $pK <- pK
#Mix21_H3 $pr <- pr
#ggplot(Mix21_H3, aes(x = sampling, y = value))+
#  geom_point()+
#  geom_line(data = Mix21_H3, aes(x = sampling, y = predict(Mix21_H3_model)))

# curve fitting for MIX 18 °C Low rep 1 
Mix18_L1<- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_18_Low_1') %>%
  filter(!sampling %in% c(8.5, 16))
N0s<- median(Mix18_L1$value[Mix18_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix18_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix18_L1$value[Mix18_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.6
#Estimated growth rate (r)
Mix18_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                   start=list(N0=N0s,K=Ks,r=rs),
                   data = Mix18_L1)
summary(Mix18_L1_model)
#plot model output
ggplot(Mix18_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix18_L1, aes(x = sampling, y = predict(Mix18_L1_model)))
# replicate 3 is really bad


# curve fitting for MIX 18 °C Low rep 2
Mix18_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_18_Low_2')
N0s<- median(Mix18_L2$value[Mix18_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix18_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix18_L2$value[Mix18_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix18_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix18_L2)
summary(Mix18_L2_model)

sum_modeL <-summary(Mix18_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix18_L2 $N0 <- N0
Mix18_L2 $K <- K
Mix18_L2 $r <- r
Mix18_L2 $pK <- pK
Mix18_L2 $pr <- pr
ggplot(Mix18_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix18_L2, aes(x = sampling, y = predict(Mix18_L2_model)))

# curve fitting for MIX 18 °C Low rep 3
Mix18_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_18_Low_3')
N0s<- median(Mix18_L3$value[Mix18_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix18_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix18_L3$value[Mix18_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix18_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix18_L3)
summary(Mix18_L3_model)

sum_modeL <-summary(Mix18_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix18_L3 $N0 <- N0
Mix18_L3 $K <- K
Mix18_L3 $r <- r
Mix18_L3 $pK <- pK
Mix18_L3 $pr <- pr
ggplot(Mix18_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix18_L3, aes(x = sampling, y = predict(Mix18_L3_model)))

# curve fitting for MIX 18 °C High rep 1
Mix18_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_18_HI_1')
N0s<- median(Mix18_H1$value[Mix18_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix18_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix18_H1$value[Mix18_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix18_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix18_H1)
summary(Mix18_H1_model)

sum_modeL <-summary(Mix18_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix18_H1 $N0 <- N0
Mix18_H1 $K <- K
Mix18_H1 $r <- r
Mix18_H1 $pK <- pK
Mix18_H1 $pr <- pr
ggplot(Mix18_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix18_H1, aes(x = sampling, y = predict(Mix18_H1_model)))

# curve fitting for MIX 18 °C High rep 2
Mix18_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_18_HI_2')
N0s<- median(Mix18_H2$value[Mix18_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix18_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix18_H2$value[Mix18_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix18_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix18_H2)
summary(Mix18_H2_model)

sum_modeL <-summary(Mix18_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix18_H2 $N0 <- N0
Mix18_H2 $K <- K
Mix18_H2 $r <- r
Mix18_H2 $pK <- pK
Mix18_H2 $pr <- pr
ggplot(Mix18_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix18_H2, aes(x = sampling, y = predict(Mix18_H2_model)))

# curve fitting for MIX 18 °C High rep 3
Mix18_H3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_18_HI_3')
N0s<- median(Mix18_H3$value[Mix18_H3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix18_H3$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix18_H3$value[Mix18_H3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix18_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix18_H3)
summary(Mix18_H3_model)

sum_modeL <-summary(Mix18_H3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix18_H3 $N0 <- N0
Mix18_H3 $K <- K
Mix18_H3 $r <- r
Mix18_H3 $pK <- pK
Mix18_H3 $pr <- pr
ggplot(Mix18_H3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix18_H3, aes(x = sampling, y = predict(Mix18_H3_model)))

# curve fitting for MIX 15 °C Low rep 1
Mix15_L1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_15_Low_1')
N0s<- median(Mix15_L1$value[Mix15_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix15_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix15_L1$value[Mix15_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix15_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix15_L1)
summary(Mix15_L1_model)

sum_modeL <-summary(Mix15_L1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix15_L1 $N0 <- N0
Mix15_L1 $K <- K
Mix15_L1 $r <- r
Mix15_L1 $pK <- pK
Mix15_L1 $pr <- pr
ggplot(Mix15_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix15_L1, aes(x = sampling, y = predict(Mix15_L1_model)))

# curve fitting for MIX 15 °C Low rep 2
Mix15_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_15_Low_2')
N0s<- median(Mix15_L2$value[Mix15_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix15_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix15_L2$value[Mix15_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix15_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix15_L2)
summary(Mix15_L2_model)

sum_modeL <-summary(Mix15_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix15_L2 $N0 <- N0
Mix15_L2 $K <- K
Mix15_L2 $r <- r
Mix15_L2 $pK <- pK
Mix15_L2 $pr <- pr
ggplot(Mix15_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix15_L2, aes(x = sampling, y = predict(Mix15_L2_model)))


# curve fitting for MIX 15 °C Low rep 3
Mix15_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_15_Low_3')
N0s<- median(Mix15_L3$value[Mix15_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix15_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix15_L3$value[Mix15_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix15_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix15_L3)
summary(Mix15_L3_model)

sum_modeL <-summary(Mix15_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix15_L3 $N0 <- N0
Mix15_L3 $K <- K
Mix15_L3 $r <- r
Mix15_L3 $pK <- pK
Mix15_L3 $pr <- pr
ggplot(Mix15_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix15_L3, aes(x = sampling, y = predict(Mix15_L3_model)))

# curve fitting for MIX 15 °C High rep 1
Mix15_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_15_HI_1')
N0s<- median(Mix15_H1$value[Mix15_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix15_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix15_H1$value[Mix15_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix15_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix15_H1)
summary(Mix15_H1_model)

sum_modeL <-summary(Mix15_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix15_H1 $N0 <- N0
Mix15_H1 $K <- K
Mix15_H1 $r <- r
Mix15_H1 $pK <- pK
Mix15_H1 $pr <- pr
ggplot(Mix15_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix15_H1, aes(x = sampling, y = predict(Mix15_H1_model)))

# curve fitting for MIX 15 °C High rep 2
Mix15_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_15_HI_2')
N0s<- median(Mix15_H2$value[Mix15_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix15_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix15_H2$value[Mix15_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix15_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix15_H2)
summary(Mix15_H2_model)

sum_modeL <-summary(Mix15_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix15_H2 $N0 <- N0
Mix15_H2 $K <- K
Mix15_H2 $r <- r
Mix15_H2 $pK <- pK
Mix15_H2 $pr <- pr
ggplot(Mix15_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix15_H2, aes(x = sampling, y = predict(Mix15_H2_model)))

# curve fitting for MIX 15 °C High rep 3
Mix15_H3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_15_HI_3')
N0s<- median(Mix15_H3$value[Mix15_H3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix15_H3$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix15_H3$value[Mix15_H3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix15_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix15_H3)
summary(Mix15_H3_model)

sum_modeL <-summary(Mix15_H3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix15_H3 $N0 <- N0
Mix15_H3 $K <- K
Mix15_H3 $r <- r
Mix15_H3 $pK <- pK
Mix15_H3 $pr <- pr
ggplot(Mix15_H3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix15_H3, aes(x = sampling, y = predict(Mix15_H3_model)))

# curve fitting for MIX 12 °C Low rep 1 ######
Mix12_L1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_12_Low_1')
N0s<- median(Mix12_L1$value[Mix12_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix12_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix12_L1$value[Mix12_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix12_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix12_L1)
summary(Mix12_L1_model)

sum_modeL <-summary(Mix12_L1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix12_L1 $N0 <- N0
Mix12_L1 $K <- K
Mix12_L1 $r <- r
Mix12_L1 $pK <- pK
Mix12_L1 $pr <- pr
ggplot(Mix12_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix12_L1, aes(x = sampling, y = predict(Mix12_L1_model)))

# curve fitting for MIX 12 °C Low rep 2
Mix12_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_12_Low_2')
N0s<- median(Mix12_L2$value[Mix12_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix12_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix12_L2$value[Mix12_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix12_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix12_L2)
summary(Mix12_L2_model)

sum_modeL <-summary(Mix12_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix12_L2 $N0 <- N0
Mix12_L2 $K <- K
Mix12_L2 $r <- r
Mix12_L2 $pK <- pK
Mix12_L2 $pr <- pr
ggplot(Mix12_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix12_L2, aes(x = sampling, y = predict(Mix12_L2_model)))

# curve fitting for MIX 12 °C Low rep 3
Mix12_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_12_Low_3')
N0s<- median(Mix12_L3$value[Mix12_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix12_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix12_L3$value[Mix12_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix12_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix12_L3)
summary(Mix12_L3_model)

sum_modeL <-summary(Mix12_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix12_L3 $N0 <- N0
Mix12_L3 $K <- K
Mix12_L3 $r <- r
Mix12_L3 $pK <- pK
Mix12_L3 $pr <- pr
ggplot(Mix12_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix12_L3, aes(x = sampling, y = predict(Mix12_L3_model)))

# curve fitting for MIX 12 °C High rep 1
Mix12_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_12_HI_1')
N0s<- median(Mix12_H1$value[Mix12_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix12_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix12_H1$value[Mix12_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix12_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix12_H1)
summary(Mix12_H1_model)

sum_modeL <-summary(Mix12_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix12_H1 $N0 <- N0
Mix12_H1 $K <- K
Mix12_H1 $r <- r
Mix12_H1 $pK <- pK
Mix12_H1 $pr <- pr
ggplot(Mix12_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix12_H1, aes(x = sampling, y = predict(Mix12_H1_model)))

# curve fitting for MIX 12 °C High rep 2
Mix12_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_12_HI_2')
N0s<- median(Mix12_H2$value[Mix12_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix12_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix12_H2$value[Mix12_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix12_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix12_H2)
summary(Mix12_H2_model)

sum_modeL <-summary(Mix12_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix12_H2 $N0 <- N0
Mix12_H2 $K <- K
Mix12_H2 $r <- r
Mix12_H2 $pK <- pK
Mix12_H2 $pr <- pr
ggplot(Mix12_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix12_H2, aes(x = sampling, y = predict(Mix12_H2_model)))

# curve fitting for MIX 12 °C High rep 3
Mix12_H3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_12_HI_3')
N0s<- median(Mix12_H3$value[Mix12_H3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix12_H3$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix12_H3$value[Mix12_H3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix12_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix12_H3)
summary(Mix12_H3_model)

sum_modeL <-summary(Mix12_H3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix12_H3 $N0 <- N0
Mix12_H3 $K <- K
Mix12_H3 $r <- r
Mix12_H3 $pK <- pK
Mix12_H3 $pr <- pr
ggplot(Mix12_H3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix12_H3, aes(x = sampling, y = predict(Mix12_H3_model)))


# curve fitting for MIX 9 °C Low rep 1 ######
Mix9_L1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_9_Low_1')
N0s<- median(Mix9_L1$value[Mix9_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix9_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix9_L1$value[Mix9_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix9_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = Mix9_L1)
summary(Mix9_L1_model)

sum_modeL <-summary(Mix9_L1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix9_L1 $N0 <- N0
Mix9_L1 $K <- K
Mix9_L1 $r <- r
Mix9_L1 $pK <- pK
Mix9_L1 $pr <- pr
ggplot(Mix9_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix9_L1, aes(x = sampling, y = predict(Mix9_L1_model)))

# curve fitting for MIX 9 °C Low rep 2
Mix9_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_9_Low_2')
N0s<- median(Mix9_L2$value[Mix9_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix9_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix9_L2$value[Mix9_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix9_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = Mix9_L2)
summary(Mix9_L2_model)

sum_modeL <-summary(Mix9_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix9_L2 $N0 <- N0
Mix9_L2 $K <- K
Mix9_L2 $r <- r
Mix9_L2 $pK <- pK
Mix9_L2 $pr <- pr
ggplot(Mix9_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix9_L2, aes(x = sampling, y = predict(Mix9_L2_model)))

# curve fitting for MIX 9 °C Low rep 3
Mix9_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_9_Low_3')
N0s<- median(Mix9_L3$value[Mix9_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix9_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix9_L3$value[Mix9_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix9_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = Mix9_L3)
summary(Mix9_L3_model)

sum_modeL <-summary(Mix9_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix9_L3 $N0 <- N0
Mix9_L3 $K <- K
Mix9_L3 $r <- r
Mix9_L3 $pK <- pK
Mix9_L3 $pr <- pr
ggplot(Mix9_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix9_L3, aes(x = sampling, y = predict(Mix9_L3_model)))

# curve fitting for MIX 9 °C High rep 1 
Mix9_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_9_HI_1')
N0s<- median(Mix9_H1$value[Mix9_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix9_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix9_H1$value[Mix9_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix9_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = Mix9_H1)
summary(Mix9_H1_model)

sum_modeL <-summary(Mix9_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix9_H1 $N0 <- N0
Mix9_H1 $K <- K
Mix9_H1 $r <- r
Mix9_H1 $pK <- pK
Mix9_H1 $pr <- pr
ggplot(Mix9_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix9_H1, aes(x = sampling, y = predict(Mix9_H1_model)))

# curve fitting for MIX 9 °C High rep 2
Mix9_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_9_HI_2')
N0s<- median(Mix9_H2$value[Mix9_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix9_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix9_H2$value[Mix9_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix9_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = Mix9_H2)
summary(Mix9_H2_model)

sum_modeL <-summary(Mix9_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix9_H2 $N0 <- N0
Mix9_H2 $K <- K
Mix9_H2 $r <- r
Mix9_H2 $pK <- pK
Mix9_H2 $pr <- pr
ggplot(Mix9_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix9_H2, aes(x = sampling, y = predict(Mix9_H2_model)))

# curve fitting for MIX 9 °C High rep 3
Mix9_H3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'MIX_9_HI_3')
N0s<- median(Mix9_H3$value[Mix9_H3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(Mix9_H3$sampling) #Select the maximum number of days (time of incubation)
Ks<-Mix9_H3$value[Mix9_H3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
Mix9_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = Mix9_H3)
summary(Mix9_H3_model)

sum_modeL <-summary(Mix9_H3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
Mix9_H3 $N0 <- N0
Mix9_H3 $K <- K
Mix9_H3 $r <- r
Mix9_H3 $pK <- pK
Mix9_H3 $pr <- pr
ggplot(Mix9_H3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = Mix9_H3, aes(x = sampling, y = predict(Mix9_H3_model)))

#### Pulse curve fitting ####

#pulse 21°C Low rep 1 geht noch nicht
pulse21_L1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_21_Low_1')
N0s<- median(pulse21_L1$value[pulse21_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse21_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse21_L1$value[pulse21_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse21_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse21_L1)
summary(pulse21_L1_model)

sum_modeL <-summary(pulse21_L1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse21_L1 $N0 <- N0
pulse21_L1 $K <- K
pulse21_L1 $r <- r
pulse21_L1 $pK <- pK
pulse21_L1 $pr <- pr
ggplot(pulse21_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse21_L1, aes(x = sampling, y = predict(pulse21_L1_model)))

#pulse 21°C Low rep 2
pulse21_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_21_Low_2')
N0s<- median(pulse21_L2$value[pulse21_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse21_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse21_L2$value[pulse21_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse21_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse21_L2)
summary(pulse21_L2_model)

sum_modeL <-summary(pulse21_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse21_L2 $N0 <- N0
pulse21_L2 $K <- K
pulse21_L2 $r <- r
pulse21_L2 $pK <- pK
pulse21_L2 $pr <- pr
ggplot(pulse21_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse21_L2, aes(x = sampling, y = predict(pulse21_L2_model)))

#pulse 21°C Low rep 3
pulse21_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_21_Low_3')
N0s<- median(pulse21_L3$value[pulse21_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse21_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse21_L3$value[pulse21_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse21_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                       start=list(N0=N0s,K=Ks,r=rs),
                       data = pulse21_L3)
summary(pulse21_L3_model)

sum_modeL <-summary(pulse21_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse21_L3 $N0 <- N0
pulse21_L3 $K <- K
pulse21_L3 $r <- r
pulse21_L3 $pK <- pK
pulse21_L3 $pr <- pr
ggplot(pulse21_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse21_L3, aes(x = sampling, y = predict(pulse21_L3_model)))

#pulse 21°C High rep 1
pulse21_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_21_HI_1')
N0s<- median(pulse21_H1$value[pulse21_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse21_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse21_H1$value[pulse21_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse21_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse21_H1)
summary(pulse21_H1_model)

sum_modeL <-summary(pulse21_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse21_H1 $N0 <- N0
pulse21_H1 $K <- K
pulse21_H1 $r <- r
pulse21_H1 $pK <- pK
pulse21_H1 $pr <- pr
ggplot(pulse21_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse21_H1, aes(x = sampling, y = predict(pulse21_H1_model)))

#pulse 21°C Hi rep 2
pulse21_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_21_HI_2')
N0s<- median(pulse21_H2$value[pulse21_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse21_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse21_H2$value[pulse21_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse21_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse21_H2)
summary(pulse21_H2_model)

sum_modeL <-summary(pulse21_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse21_H2 $N0 <- N0
pulse21_H2 $K <- K
pulse21_H2 $r <- r
pulse21_H2 $pK <- pK
pulse21_H2 $pr <- pr
ggplot(pulse21_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse21_H2, aes(x = sampling, y = predict(pulse21_H2_model)))

#pulse 21°C Hi rep 3 geht noch nicht
pulse21_H3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_21_HI_3')
N0s<- median(pulse21_H3$value[pulse21_H3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse21_H3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse21_H3$value[pulse21_H3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse21_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse21_H3)
summary(pulse21_H3_model)

sum_modeL <-summary(pulse21_H3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse21_H3 $N0 <- N0
pulse21_H3 $K <- K
pulse21_H3 $r <- r
pulse21_H3 $pK <- pK
pulse21_H3 $pr <- pr
ggplot(pulse21_H3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse21_H3, aes(x = sampling, y = predict(pulse21_H3_model)))

# curve fitting for pulse 18 °C Low rep 1 
pulse18_L1<- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_18_Low_1') %>%
  filter(!sampling %in% c(8.5, 16))
N0s<- median(pulse18_L1$value[pulse18_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse18_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse18_L1$value[pulse18_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.6
#Estimated growth rate (r)
pulse18_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse18_L1)
summary(pulse18_L1_model)
#plot model output
ggplot(pulse18_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse18_L1, aes(x = sampling, y = predict(pulse18_L1_model)))
# replicate 3 is really bad


# curve fitting for pulse 18 °C Low rep 2 geht nocht nicht
pulse18_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_18_Low_2')
N0s<- median(pulse18_L2$value[pulse18_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse18_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse18_L2$value[pulse18_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse18_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse18_L2)
summary(pulse18_L2_model)

sum_modeL <-summary(pulse18_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse18_L2 $N0 <- N0
pulse18_L2 $K <- K
pulse18_L2 $r <- r
pulse18_L2 $pK <- pK
pulse18_L2 $pr <- pr
ggplot(pulse18_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse18_L2, aes(x = sampling, y = predict(pulse18_L2_model)))

# curve fitting for pulse 18 °C Low rep 3
pulse18_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_18_Low_3')
N0s<- median(pulse18_L3$value[pulse18_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse18_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse18_L3$value[pulse18_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse18_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse18_L3)
summary(pulse18_L3_model)

sum_modeL <-summary(pulse18_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse18_L3 $N0 <- N0
pulse18_L3 $K <- K
pulse18_L3 $r <- r
pulse18_L3 $pK <- pK
pulse18_L3 $pr <- pr
ggplot(pulse18_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse18_L3, aes(x = sampling, y = predict(pulse18_L3_model)))

# curve fitting for pulse 18 °C High rep 1
pulse18_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_18_HI_1')
N0s<- median(pulse18_H1$value[pulse18_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse18_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse18_H1$value[pulse18_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse18_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse18_H1)
summary(pulse18_H1_model)

sum_modeL <-summary(pulse18_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse18_H1 $N0 <- N0
pulse18_H1 $K <- K
pulse18_H1 $r <- r
pulse18_H1 $pK <- pK
pulse18_H1 $pr <- pr
ggplot(pulse18_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse18_H1, aes(x = sampling, y = predict(pulse18_H1_model)))

# curve fitting for pulse 18 °C High rep 2
pulse18_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_18_HI_2')
N0s<- median(pulse18_H2$value[pulse18_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse18_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse18_H2$value[pulse18_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse18_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse18_H2)
summary(pulse18_H2_model)

sum_modeL <-summary(pulse18_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse18_H2 $N0 <- N0
pulse18_H2 $K <- K
pulse18_H2 $r <- r
pulse18_H2 $pK <- pK
pulse18_H2 $pr <- pr
ggplot(pulse18_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse18_H2, aes(x = sampling, y = predict(pulse18_H2_model)))

# curve fitting for pulse 18 °C High rep 3
pulse18_H3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_18_HI_3')
N0s<- median(pulse18_H3$value[pulse18_H3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse18_H3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse18_H3$value[pulse18_H3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse18_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse18_H3)
summary(pulse18_H3_model)

sum_modeL <-summary(pulse18_H3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse18_H3 $N0 <- N0
pulse18_H3 $K <- K
pulse18_H3 $r <- r
pulse18_H3 $pK <- pK
pulse18_H3 $pr <- pr
ggplot(pulse18_H3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse18_H3, aes(x = sampling, y = predict(pulse18_H3_model)))

# curve fitting for pulse 15 °C Low rep 1
pulse15_L1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_15_Low_1')
N0s<- median(pulse15_L1$value[pulse15_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse15_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse15_L1$value[pulse15_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse15_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse15_L1)
summary(pulse15_L1_model)

sum_modeL <-summary(pulse15_L1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse15_L1 $N0 <- N0
pulse15_L1 $K <- K
pulse15_L1 $r <- r
pulse15_L1 $pK <- pK
pulse15_L1 $pr <- pr
ggplot(pulse15_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse15_L1, aes(x = sampling, y = predict(pulse15_L1_model)))

# curve fitting for pulse 15 °C Low rep 2
pulse15_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_15_Low_2')
N0s<- median(pulse15_L2$value[pulse15_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse15_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse15_L2$value[pulse15_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse15_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse15_L2)
summary(pulse15_L2_model)

sum_modeL <-summary(pulse15_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse15_L2 $N0 <- N0
pulse15_L2 $K <- K
pulse15_L2 $r <- r
pulse15_L2 $pK <- pK
pulse15_L2 $pr <- pr
ggplot(pulse15_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse15_L2, aes(x = sampling, y = predict(pulse15_L2_model)))


# curve fitting for pulse 15 °C Low rep 3
pulse15_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_15_Low_3')
N0s<- median(pulse15_L3$value[pulse15_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse15_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse15_L3$value[pulse15_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse15_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse15_L3)
summary(pulse15_L3_model)

sum_modeL <-summary(pulse15_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse15_L3 $N0 <- N0
pulse15_L3 $K <- K
pulse15_L3 $r <- r
pulse15_L3 $pK <- pK
pulse15_L3 $pr <- pr
ggplot(pulse15_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse15_L3, aes(x = sampling, y = predict(pulse15_L3_model)))

# curve fitting for pulse 15 °C High rep 1
pulse15_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_15_HI_1')
N0s<- median(pulse15_H1$value[pulse15_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse15_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse15_H1$value[pulse15_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse15_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse15_H1)
summary(pulse15_H1_model)

sum_modeL <-summary(pulse15_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse15_H1 $N0 <- N0
pulse15_H1 $K <- K
pulse15_H1 $r <- r
pulse15_H1 $pK <- pK
pulse15_H1 $pr <- pr
ggplot(pulse15_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse15_H1, aes(x = sampling, y = predict(pulse15_H1_model)))

# curve fitting for pulse 15 °C High rep 2
pulse15_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_15_HI_2')
N0s<- median(pulse15_H2$value[pulse15_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse15_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse15_H2$value[pulse15_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse15_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse15_H2)
summary(pulse15_H2_model)

sum_modeL <-summary(pulse15_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse15_H2 $N0 <- N0
pulse15_H2 $K <- K
pulse15_H2 $r <- r
pulse15_H2 $pK <- pK
pulse15_H2 $pr <- pr
ggplot(pulse15_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse15_H2, aes(x = sampling, y = predict(pulse15_H2_model)))

# curve fitting for pulse 15 °C High rep 3
pulse15_H3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_15_HI_3')
N0s<- median(pulse15_H3$value[pulse15_H3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse15_H3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse15_H3$value[pulse15_H3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse15_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse15_H3)
summary(pulse15_H3_model)

sum_modeL <-summary(pulse15_H3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse15_H3 $N0 <- N0
pulse15_H3 $K <- K
pulse15_H3 $r <- r
pulse15_H3 $pK <- pK
pulse15_H3 $pr <- pr
ggplot(pulse15_H3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse15_H3, aes(x = sampling, y = predict(pulse15_H3_model)))

# curve fitting for pulse 12 °C Low rep 1 ######
pulse12_L1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_12_Low_1')
N0s<- median(pulse12_L1$value[pulse12_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse12_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse12_L1$value[pulse12_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse12_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse12_L1)
summary(pulse12_L1_model)

sum_modeL <-summary(pulse12_L1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse12_L1 $N0 <- N0
pulse12_L1 $K <- K
pulse12_L1 $r <- r
pulse12_L1 $pK <- pK
pulse12_L1 $pr <- pr
ggplot(pulse12_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse12_L1, aes(x = sampling, y = predict(pulse12_L1_model)))

# curve fitting for pulse 12 °C Low rep 2
pulse12_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_12_Low_2')
N0s<- median(pulse12_L2$value[pulse12_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse12_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse12_L2$value[pulse12_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse12_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse12_L2)
summary(pulse12_L2_model)

sum_modeL <-summary(pulse12_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse12_L2 $N0 <- N0
pulse12_L2 $K <- K
pulse12_L2 $r <- r
pulse12_L2 $pK <- pK
pulse12_L2 $pr <- pr
ggplot(pulse12_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse12_L2, aes(x = sampling, y = predict(pulse12_L2_model)))

# curve fitting for pulse 12 °C Low rep 3
pulse12_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_12_Low_3')
N0s<- median(pulse12_L3$value[pulse12_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse12_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse12_L3$value[pulse12_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse12_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse12_L3)
summary(pulse12_L3_model)

sum_modeL <-summary(pulse12_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse12_L3 $N0 <- N0
pulse12_L3 $K <- K
pulse12_L3 $r <- r
pulse12_L3 $pK <- pK
pulse12_L3 $pr <- pr
ggplot(pulse12_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse12_L3, aes(x = sampling, y = predict(pulse12_L3_model)))

# curve fitting for pulse 12 °C High rep 1
pulse12_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_12_HI_1')
N0s<- median(pulse12_H1$value[pulse12_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse12_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse12_H1$value[pulse12_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse12_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse12_H1)
summary(pulse12_H1_model)

sum_modeL <-summary(pulse12_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse12_H1 $N0 <- N0
pulse12_H1 $K <- K
pulse12_H1 $r <- r
pulse12_H1 $pK <- pK
pulse12_H1 $pr <- pr
ggplot(pulse12_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse12_H1, aes(x = sampling, y = predict(pulse12_H1_model)))

# curve fitting for pulse 12 °C High rep 2
pulse12_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_12_HI_2')
N0s<- median(pulse12_H2$value[pulse12_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse12_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse12_H2$value[pulse12_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse12_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse12_H2)
summary(pulse12_H2_model)

sum_modeL <-summary(pulse12_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse12_H2 $N0 <- N0
pulse12_H2 $K <- K
pulse12_H2 $r <- r
pulse12_H2 $pK <- pK
pulse12_H2 $pr <- pr
ggplot(pulse12_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse12_H2, aes(x = sampling, y = predict(pulse12_H2_model)))

# curve fitting for pulse 12 °C High rep 3
pulse12_H3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_12_HI_3')
N0s<- median(pulse12_H3$value[pulse12_H3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse12_H3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse12_H3$value[pulse12_H3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse12_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                      start=list(N0=N0s,K=Ks,r=rs),
                      data = pulse12_H3)
summary(pulse12_H3_model)

sum_modeL <-summary(pulse12_H3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse12_H3 $N0 <- N0
pulse12_H3 $K <- K
pulse12_H3 $r <- r
pulse12_H3 $pK <- pK
pulse12_H3 $pr <- pr
ggplot(pulse12_H3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse12_H3, aes(x = sampling, y = predict(pulse12_H3_model)))


# curve fitting for pulse 9 °C Low rep 1 ######
pulse9_L1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_9_Low_1')
N0s<- median(pulse9_L1$value[pulse9_L1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse9_L1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse9_L1$value[pulse9_L1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse9_L1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = pulse9_L1)
summary(pulse9_L1_model)

sum_modeL <-summary(pulse9_L1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse9_L1 $N0 <- N0
pulse9_L1 $K <- K
pulse9_L1 $r <- r
pulse9_L1 $pK <- pK
pulse9_L1 $pr <- pr
ggplot(pulse9_L1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse9_L1, aes(x = sampling, y = predict(pulse9_L1_model)))

# curve fitting for pulse 9 °C Low rep 2
pulse9_L2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_9_Low_2')
N0s<- median(pulse9_L2$value[pulse9_L2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse9_L2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse9_L2$value[pulse9_L2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse9_L2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = pulse9_L2)
summary(pulse9_L2_model)

sum_modeL <-summary(pulse9_L2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse9_L2 $N0 <- N0
pulse9_L2 $K <- K
pulse9_L2 $r <- r
pulse9_L2 $pK <- pK
pulse9_L2 $pr <- pr
ggplot(pulse9_L2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse9_L2, aes(x = sampling, y = predict(pulse9_L2_model)))

# curve fitting for pulse 9 °C Low rep 3
pulse9_L3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_9_Low_3')
N0s<- median(pulse9_L3$value[pulse9_L3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse9_L3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse9_L3$value[pulse9_L3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse9_L3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = pulse9_L3)
summary(pulse9_L3_model)

sum_modeL <-summary(pulse9_L3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse9_L3 $N0 <- N0
pulse9_L3 $K <- K
pulse9_L3 $r <- r
pulse9_L3 $pK <- pK
pulse9_L3 $pr <- pr
ggplot(pulse9_L3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse9_L3, aes(x = sampling, y = predict(pulse9_L3_model)))

# curve fitting for pulse 9 °C High rep 1 
pulse9_H1 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_9_HI_1')
N0s<- median(pulse9_H1$value[pulse9_H1$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse9_H1$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse9_H1$value[pulse9_H1$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse9_H1_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = pulse9_H1)
summary(pulse9_H1_model)

sum_modeL <-summary(pulse9_H1_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse9_H1 $N0 <- N0
pulse9_H1 $K <- K
pulse9_H1 $r <- r
pulse9_H1 $pK <- pK
pulse9_H1 $pr <- pr
ggplot(pulse9_H1, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse9_H1, aes(x = sampling, y = predict(pulse9_H1_model)))

# curve fitting for pulse 9 °C High rep 2
pulse9_H2 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_9_HI_2')
N0s<- median(pulse9_H2$value[pulse9_H2$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse9_H2$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse9_H2$value[pulse9_H2$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse9_H2_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = pulse9_H2)
summary(pulse9_H2_model)

sum_modeL <-summary(pulse9_H2_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse9_H2 $N0 <- N0
pulse9_H2 $K <- K
pulse9_H2 $r <- r
pulse9_H2 $pK <- pK
pulse9_H2 $pr <- pr
ggplot(pulse9_H2, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse9_H2, aes(x = sampling, y = predict(pulse9_H2_model)))

# curve fitting for pulse 9 °C High rep 3
pulse9_H3 <- All_sampling %>%
  filter(wavelength1 %in% c('440')) %>% 
  mutate(treatmentID = paste(treatment, temperature, nut, rep, sep = '_'),
         log = log(value),
         sampling = as.numeric(sampling)) %>%
  filter(treatmentID == 'pulse_9_HI_3')
N0s<- median(pulse9_H3$value[pulse9_H3$sampling==1] )#Set the start OD-value (Day=1)
Daymax<-max(pulse9_H3$sampling) #Select the maximum number of days (time of incubation)
Ks<-pulse9_H3$value[pulse9_H3$sampling==Daymax]
#Estimated value for K (Carrying Capacity) = Time of incubation
rs<-0.2
#Estimated growth rate (r)
pulse9_H3_model <- nls(value ~ K*N0*exp(r*sampling)/(K+N0*(exp(r*sampling)-1)),
                     start=list(N0=N0s,K=Ks,r=rs),
                     data = pulse9_H3)
summary(pulse9_H3_model)

sum_modeL <-summary(pulse9_H3_model)
N0 <- sum_modeL$coefficients[1,1] #N0
K  <- sum_modeL$coefficients[2,1] #K
r  <- sum_modeL$coefficients[3,1] #r
pK  <- sum_modeL$coefficients[2,4] #pK
pr  <- sum_modeL$coefficients[3,4] #pr
pulse9_H3 $N0 <- N0
pulse9_H3 $K <- K
pulse9_H3 $r <- r
pulse9_H3 $pK <- pK
pulse9_H3 $pr <- pr
ggplot(pulse9_H3, aes(x = sampling, y = value))+
  geom_point()+
  geom_line(data = pulse9_H3, aes(x = sampling, y = predict(pulse9_H3_model)))


#data struc -> bind all dfs with K and r into one df

#list all relevant dataframes
ls(pattern = "1$|2$|3$")

# bind all dfs together if Mix21_H3 works, include it here!!!

kr_comparison <- bind_rows(Mix12_H1  ,  Mix12_H2  ,  Mix12_H3 ,   Mix12_L1  ,  Mix12_L2 ,  Mix12_L3 ,   Mix15_H1  , 
                            Mix15_H2 ,   Mix15_H3,    Mix15_L1,    Mix15_L2,    Mix15_L3,    Mix18_H1,    Mix18_H2 ,  
                            Mix18_H3  ,  Mix18_L1 ,   Mix18_L2 ,   Mix18_L3 ,   Mix21_H1 ,   Mix21_H2   , 
                            Mix21_L1   , Mix21_L2  ,  Mix21_L3  ,  Mix9_H1   ,  Mix9_H2   ,  Mix9_H3   ,  Mix9_L1    ,
                            Mix9_L2     ,Mix9_L3    , pulse21_L3, pulse12_H1 , pulse12_H2 , pulse12_H3 , pulse12_L1 ,
                            pulse12_L2,  pulse12_L3  ,pulse15_H1  ,pulse15_H2  ,pulse15_H3  ,pulse15_L1  ,pulse15_L2 ,
                            pulse15_L3 , pulse18_H1,  pulse18_H2,  pulse18_H3  ,pulse18_L1,  pulse18_L2,  pulse18_L3 ,
                            pulse21_H1  ,pulse21_H2 , pulse21_H3 , pulse21_L1  ,pulse21_L2 , pulse9_H1  , pulse9_H2  ,
                            pulse9_H3   ,pulse9_L1   ,pulse9_L2   ,pulse9_L3  )

#remove duplicate rows 
kr_comparison <- distinct(kr_comparison, treatmentID, .keep_all = T)

#save df

write.csv(kr_comparison, file = here("Tables/rK_comparison.csv"))
