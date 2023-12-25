#Data Analysis
#Only used Table 1 Results from this code
#Table 2 is from Stata 
#Updated: March 7 2021
#Author: Annabel Tan

library(tidyverse)
library(dplyr)
library (haven)
library(tableone)
library(lubridate)
library(rlang)
library(tidyr)
library(readxl)

library(psych)

#read in files
covid_cases_all <- read_csv("C:/Users/annabelx/Box/Annabel Tan's Files/COVID-19/Created Datasets/covid_cases_all_03_01_21.csv")
covid_deaths_all <- read_csv("C:/Users/annabelx/Box/Annabel Tan's Files/COVID-19/Created Datasets/covid_deaths_all_03_01_21.csv")

#this edited file is the original covid_cases_all/covid_deaths_all turned wide to long in a script named covid_reshape_03_01_21

covid_cases_edit <- read_csv("C:/Users/annabelx/Box/Annabel Tan's Files/COVID-19/Created Datasets/covid_cases_edit_03_01_21.csv")
covid_deaths_edit <- read_csv("C:/Users/annabelx/Box/Annabel Tan's Files/COVID-19/Created Datasets/covid_deaths_edit_03_01_21.csv")


#make all the negative cases zero (there are very few)
covid_cases_edit$cases[covid_cases_edit$cases<0] <- 0
covid_deaths_edit$deaths[covid_deaths_edit$deaths<0] <- 0

#numbers for abstract
#median cases per 100,000 - total
covid_cases_all$prop_case = (covid_cases_all$case_total/covid_cases_all$Population)*100000
median(covid_cases_all$prop_case)
IQR(covid_cases_all$prop_case)
summary(covid_cases_all$prop_case)

#median deaths per 100,000 - total
covid_deaths_all$prop_death = (covid_deaths_all$death_total/covid_deaths_all$Population)*100000
median(covid_deaths_all$prop_death)
IQR(covid_deaths_all$prop_death)
summary(covid_deaths_all$prop_death)

#median GINI
median(covid_cases_all$gini_coeff)
IQR(covid_cases_all$gini_coeff)
summary(covid_cases_all$gini_coeff)

#Table 1 - Descriptives
#median cases per 100,000 by time 
covid_cases_edit$prop_case = (covid_cases_edit$cases/covid_cases_edit$Population)*100000 
covid_cases_median = covid_cases_edit %>%
 group_by(time) %>%
  summarize(med_size = median(prop_case, na.rm = TRUE), quants = quantile(prop_case, probs=c(0.25,0.75)), n = n()) %>%
  mutate(med_size = round(med_size)) %>%
  mutate(quants = round(quants))

#median deaths per 100,000 by time
covid_deaths_edit$prop_deaths = (covid_deaths_edit$deaths/covid_deaths_edit$Population)*100000 

covid_deaths_median = covid_deaths_edit %>%
  group_by(time) %>%
  summarize(med_size = median(prop_deaths, na.rm = TRUE), quants = quantile(prop_deaths, probs=c(0.25,0.75)), n = n()) %>%
  mutate(med_size = round(med_size)) %>%
  mutate(quants = round(quants))

covid_deaths_edit$logdeaths = log(covid_deaths_edit$deaths)

hist(covid_cases_edit$cases)
hist(covid_deaths_edit$logdeaths)

#3/1/2021
#plot gini histogram 
giniplot = covid_cases_all %>%
  mutate(gini_round = signif(gini_coeff, digits=3)) %>%
  select(state_no, county_no, gini_round) %>%
  ungroup

#histogram
ggplot(giniplot, aes(x=gini_round))+
  geom_histogram(color="darkblue", fill="lightblue") +
  xlab("Gini Coefficient") +  ylab("Count")



#cases = 28,306,349
#deaths = 505,620

#Calculating proportions -- cases/pop x 100,000
covid_cases_prop = covid_cases_all %>%
  mutate(prop_marchapr = (march_april/Population)*100000) %>%
  mutate(prop_mayjune = (may_june/Population)*100000) %>%
  mutate(prop_julaug = (july_august/Population)*100000) %>%
  mutate(prop_sepoct = (sep_oct/Population)*100000) %>%
  mutate(prop_novdec = (nov_dec/Population)*100000) %>%
  mutate(prop_janfeb = (jan_feb/Population)*100000) %>%
  mutate(prop_case = (case_total/Population)*100000) %>%
  select(state_no, county_no, Admin2, Province_State, gini_coeff, prop_marchapr, prop_mayjune, prop_julaug, prop_sepoct, prop_novdec, prop_janfeb, prop_case) %>%
  mutate(sum_marchapr = round(sum(prop_marchapr))) %>%
  mutate(sum_mayjune = round(sum(prop_mayjune))) %>%
  mutate(sum_julaug = round(sum(prop_julaug))) %>%
  mutate(sum_sepoct = round(sum(prop_sepoct))) %>%
  mutate(sum_novdec = round(sum(prop_novdec))) %>%
  mutate(sum_janfeb = round(sum(prop_janfeb))) %>%
  ungroup()


#Calculating proportions -- deaths/pop x 100,000
covid_deaths_prop = covid_deaths_all %>%
  mutate(prop_marchapr = (march_april/Population)*100000) %>%
  mutate(prop_mayjune = (may_june/Population)*100000) %>%
  mutate(prop_julaug = (july_august/Population)*100000) %>%
  mutate(prop_sepoct = (sep_oct/Population)*100000) %>%
  mutate(prop_novdec = (nov_dec/population)*100000) %>%
  mutate(prop_janfeb = (jan_feb/population)*100000) %>%
  mutate(prop_death = (death_total/Population)*100000) %>%
  select(state_no, county_no, Admin2, Province_State, gini_coeff, prop_marchapr, prop_mayjune, prop_julaug, prop_sepoct, prop_novdec, prop_janfeb, prop_death) %>%
  mutate(sum_marchapr = round(sum(prop_marchapr))) %>%
  mutate(sum_mayjune = round(sum(prop_mayjune))) %>%
  mutate(sum_julaug = round(sum(prop_julaug))) %>%
  mutate(sum_sepoct = round(sum(prop_sepoct))) %>%
  ungroup()


#describe is from the psych pkg
describe(covid_cases_all$case_total)
describe(covid_deaths_all$death_total)

describe(covid_cases_all$gini_coeff)

###

median(covid_cases_all$percbelowpoverty_noperc)

median(covid_cases_all$age_under25_prop)
median(covid_cases_all$age_25to39_prop) 
median(covid_cases_all$age_40to65_prop) 
median(covid_cases_all$age_65to79_prop) 

median(covid_cases_all$race_white_prop) 
median(covid_cases_all$race_black_prop) 
median(covid_cases_all$race_asian_prop) 
median(covid_cases_all$race_native_prop)
median(covid_cases_all$race_hawaiian_prop) 

median(covid_cases_all$crowd_owner_pointfive_prop) 
median(covid_cases_all$crowd_owner_lessone_prop) 
median(covid_cases_all$crowd_owner_lessoneptfive_prop) 
median(covid_cases_all$crowd_owner_lesstwo_prop) 
median(covid_cases_all$crowd_owner_morethantwo_prop)

median(covid_cases_all$crowd_renter_pointfive_prop) 
median(covid_cases_all$crowd_renter_lessone_prop) 
median(covid_cases_all$crowd_renter_lessoneptfive_prop)
median(covid_cases_all$crowd_renter_lesstwo_prop)
median(covid_cases_all$crowd_renter_morethantwo_prop)

median(covid_cases_all$urbanpercentage) 
median(covid_cases_all$ruralpercentage) 

median(covid_cases_all$educ_lessthanhighschool)
median(covid_cases_all$educ_highschool) 
median(covid_cases_all$educ_somecollege)
median(covid_cases_all$educ_college)


# #Table xx - Corr Coefficients -- just used in text
# 
#cases
cor_total_prop <- cor.test(covid_cases_prop$gini_coeff, covid_cases_prop$prop_case, use="complete.obs", method="spearman", exact=FALSE) #corr = 0.052 
cor_total_prop

#death
cor_total_prop_death <- cor.test(covid_deaths_prop$gini_coeff, covid_deaths_prop$prop_death, use="complete.obs", method="spearman", exact=FALSE) 
#corr = 0.133898
cor_total_prop_death

cor_total <- cor.test(covid_cases_all$gini_coeff, covid_cases_all$case_total, use="complete.obs", method="spearman", exact=FALSE) #corr = 0.185
cor_total

cor_marchapr <- cor.test(covid_cases_all$gini_coeff, covid_cases_all$march_april, use="complete.obs", method="spearman", exact=FALSE) #corr = 0.1003233 
cor_marchapr

cor_mayjune <- cor.test(covid_cases_all$gini_coeff, covid_cases_all$may_june, use="complete.obs", method="spearman",exact=FALSE) #corr = 0.1958059 
cor_mayjune

cor_julyaug <- cor.test(covid_cases_all$gini_coeff, covid_cases_all$july_august, use="complete.obs", method="spearman", exact=FALSE) #corr = 0.2445
cor_julyaug


cor_sepoct <- cor.test(covid_cases_all$gini_coeff, covid_cases_all$sep_oct, use="complete.obs", method="spearman", exact=FALSE) #corr = 0.1140512
cor_sepoct

