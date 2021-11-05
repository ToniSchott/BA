#clear environment
rm(list=ls(all=TRUE))

#load library
library(readxl)
library(tidyverse)
library(here)

#load table
kr_comparison <- read.csv(file = here("rK_comparison.csv"))




#calculate Topt for each temperature and nutrient level

kr_comparison <- group_by(kr_comparison, temperature, nut)
Topt <-summarise_at(kr_comparison,vars(r), list(name= mean),na.rm = TRUE)
Topt

#as factor

kr_comparison$temperature <- as.factor(kr_comparison$temperature)
kr_comparison$nut <- as.factor(kr_comparison$nut)
kr_comparison$treatment <- as.factor(kr_comparison$treatment)

#check distribution

hist(log(kr_comparison$r))
qqnorm(log(kr_comparison$r))
qqline(log(kr_comparison$r))


# ANOVA comparison between K and r of pulse,nutrients and temperature
#### Attention!!!! all variables are used as factors

#r comparison of temperature, nutrients and treatment (pulse or control) 

aov_tem_r <- aov(r~temperature*nut*treatment, data = kr_comparison)

summary(aov_tem_r)

# form post- hoc test in one df and .txt file
res <- TukeyHSD(aov_tem_r)
rtemp <- as.data.frame(res$temperature)
rnut <- as.data.frame(res$nut)
rnuttemp <- as.data.frame(res$`temperature:nut`)

ODTukey_r <- rbind(rtemp,rnut,rnuttemp)

ODTukey_r <- as.data.frame(ODTukey_r)


write.table(ODTukey_r, file = here("OD_r_tukeyhsd.txt"), sep = ";")


#rename column

ODTukey_r <- rename(ODTukey_r, p = "p adj")

#filter significant p values
ODTukey_r <- filter(ODTukey_r, p <= 0.05)

# K comparison of temperature, nutrients and treatment (pulse or control)
#### Attention!!!! all variables are used as factors

#check distribution

hist(log(kr_comparison$K))
qqnorm(log(kr_comparison$K))
qqline(log(kr_comparison$K))



#omit extreme K value 
kr_comparison[56,"K" ] <- NA 

aov_tem_K <- aov(K~temperature*nut*treatment, data = kr_comparison)

summary(aov_tem_K)

# sink post- hoc test in .txt file
res <- TukeyHSD(aov_tem_K)
Ktemp <- as.data.frame(res$temperature)
Knut <- as.data.frame(res$nut)
Knuttemp <- as.data.frame(res$`temperature:nut`)

ODTukey_K <- rbind(Ktemp,Knut,Knuttemp)

write.table(ODTukey_K, file = here("Tables/OD_K_tukeyhsd.txt"), sep = ";")

#rename column

ODTukey_K <- rename(ODTukey_K, p = "p adj")

#filter significant p values
ODTukey_K <- filter(ODTukey_K, p <= 0.05)

