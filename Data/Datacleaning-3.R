##################################################
# Recoding inconsistent variables and Merge ESS round 1 to 11
# Include legislation information variable
# Input: ess_Datecleaned_Votecoded.rds
#        ess11_Datecleaned.rds
# Output: ESS_1to10_cleaned.rds 
#         ESS_11_cleaned.rds
#         ESS_1to11_cleaned.rds
#setwd("/Users/hyeriseo/Documents/GitHub/Public-Opinion-Fall-24/Data")
##################################################

rm(list=ls())
library(dplyr)
library(tidyverse)
library(rio)
library(tidyr)
library(lubridate)
library(SGmisc)
library(stats)

data1 <- readRDS("Data/ess_Datecleaned_Votecoded.rds") #round 1 to 10
data2 <- readRDS("Data/ess11_Datecleaned.rds") #round 11

####### Recode consistent variables: age, gender, ideology, religiosity, freehms (attitude homosexuality)
####### Checked the ESS codebook to see whether variables are coded consistently or not

## age: agea
table(data1$agea)  #min 13, max 123, missing coded as 999, missing obs 3292
sum(is.na(data1$agea))  

data1 <- data1 %>% mutate(agea = if_else(agea==999, NA_integer_, agea))
table(data1$agea, useNA = "always")
data1 <- data1 %>% mutate(agea = case_when(
  agea<=15 ~ 15,
  agea>= 95 ~ 95,
  TRUE ~ agea
))

table(data1$agea, useNA = "always") 

table(data2$agea)
sum(is.na(data2$agea))

data2 <- data2 %>% mutate(agea = if_else(agea==999, NA_integer_, agea)) # missing 151 obs
table(data2$agea, useNA = "always") 

## gender: gndr -> male binary
table(data1$gndr, useNA = "always")
data1 <- data1 %>% mutate(male = ifelse(gndr==1, 1, 0))
table(data1$male, useNA = "always")

table(data2$gndr, useNA = "always")
data2 <- data2 %>% mutate(male = ifelse(gndr==1, 1, 0))
table(data2$male, useNA = "always")

## ideology: lrscale, ideology_right, ideology_right_NAfill
table(data1$lrscale, useNA = "always")

data1 <- data1 %>%
  mutate(ideology_right10 = case_when(
    lrscale <=10 ~ lrscale,
    lrscale %in% c(77,88, 99) ~ NA_integer_
  ))

table(data1$ideology_right10, useNA = "always") # missing obs: 69,163

data1 <- data1 %>%
  mutate(ideology_right10_NAfill = case_when(
    lrscale <=10 ~ lrscale,
    lrscale %in% c(77,99) ~ NA_integer_,
    lrscale == 88 ~ 5
  )) # don't know to the middle

table(data1$ideology_right10_NAfill, useNA = "always") # missing obs: 11,809 
hist(data1$ideology_right10)
hist(data1$ideology_right10_NAfill)

table(data2$lrscale, useNA = "always")
data2 <- data2 %>%
  mutate(ideology_right10 = case_when(
    lrscale <=10 ~ lrscale,
    lrscale %in% c(77,88, 99) ~ NA_integer_
  ))

table(data2$ideology_right10, useNA = "always") # missing obs: 2,319

data2 <- data2 %>%
  mutate(ideology_right10_NAfill = case_when(
    lrscale <=10 ~ lrscale,
    lrscale %in% c(77,99) ~ NA_integer_,
    lrscale == 88 ~ 5
  ))

table(data2$ideology_right10_NAfill, useNA = "always") # missing obs: 579
hist(data2$ideology_right10)
hist(data2$ideology_right10_NAfill)


## religiosity: rlgatnd, religiosity, religiosity_NAfill
table(data1$rlgatnd, useNA = "always")

data1 <- data1 %>% mutate(religiosity = case_when(
  rlgatnd == 7 ~ 1,
  rlgatnd == 6 ~ 2,
  rlgatnd == 5 ~ 3,
  rlgatnd == 4 ~ 4,
  rlgatnd == 3 ~ 5,
  rlgatnd == 2 ~ 6,
  rlgatnd == 1 ~ 7,
  rlgatnd %in% c(77,88, 99) ~ NA_integer_)) 

table(data1$religiosity, useNA = "always") # missing obs: 3,613

data1 <- data1 %>% mutate(religiosity_NAfill = case_when(
  rlgatnd == 7 ~ 1, # never
  rlgatnd == 6 ~ 2, # less often
  rlgatnd == 5 ~ 3, # only on special holidays
  rlgatnd == 4 ~ 4, # at least once a month
  rlgatnd == 3 ~ 5, # once a week
  rlgatnd == 2 ~ 6, # more than once a week
  rlgatnd == 1 ~ 7, # everyday
  rlgatnd == 88 ~ 1, # don't know to never
  rlgatnd %in% c(77, 99) ~ NA_integer_)) 

table(data1$religiosity_NAfill, useNA = "always") # missing obs: 1,610

hist(data1$religiosity)
hist(data1$religiosity_NAfill)

table(data2$rlgatnd, useNA = "always")

data2 <- data2 %>% mutate(religiosity = case_when(
  rlgatnd == 7 ~ 1,
  rlgatnd == 6 ~ 2,
  rlgatnd == 5 ~ 3,
  rlgatnd == 4 ~ 4,
  rlgatnd == 3 ~ 5,
  rlgatnd == 2 ~ 6,
  rlgatnd == 1 ~ 7,
  rlgatnd %in% c(77,88, 99) ~ NA_integer_)) 

table(data2$religiosity, useNA = "always") # missing obs: 116

data2 <- data2 %>% mutate(religiosity_NAfill = case_when(
  rlgatnd == 7 ~ 1,
  rlgatnd == 6 ~ 2,
  rlgatnd == 5 ~ 3,
  rlgatnd == 4 ~ 4,
  rlgatnd == 3 ~ 5,
  rlgatnd == 2 ~ 6,
  rlgatnd == 1 ~ 7,
  rlgatnd == 88 ~ 1,
  rlgatnd %in% c(77, 99) ~ NA_integer_)) 

table(data2$religiosity_NAfill, useNA = "always") # missing obs: 66

hist(data2$religiosity)
hist(data2$religiosity_NAfill)


####### Recode inconsistent variables 

### education (edulvla, edulvlb)
table(data1$edulvla, data1$essround, useNA = "always") # 1 to 4
table(data1$edulvlb, data1$essround, useNA = "always") # 5 to 10
table(data2$edulvlb, useNA = "always")

data1 <- data1 %>% mutate(education = case_when(
  edulvla <= 1 | edulvlb < 200 ~ 1, # Low
  edulvla == 2 | (edulvlb >= 200 & edulvlb < 300) ~  2, # Lower secondary
  edulvla == 3 | (edulvlb >= 300 & edulvlb < 400) ~  3, # Upper secondary
  edulvla == 4 | (edulvlb >= 400 & edulvlb < 500) ~  4, # Post-sec/non-teritiary
  edulvla == 5 | (edulvlb >= 500 & edulvlb < 1000) ~ 5, # Teritiary
  TRUE ~ NA_integer_
))

table(data1$education, useNA = "always") # missing obs: 3,731
hist(data1$education) 

data2 <- data2 %>% mutate(education = case_when(
  edulvlb < 200 ~ 1, # Low
  (edulvlb >= 200 & edulvlb < 300) ~  2, # Lower secondary
  (edulvlb >= 300 & edulvlb < 400) ~  3, # Upper secondary
  (edulvlb >= 400 & edulvlb < 500) ~  4, # Post-sec/non-teritiary
  (edulvlb >= 500 & edulvlb < 1000) ~ 5, # Teritiary
  TRUE ~ NA_integer_
))

table(data2$education, useNA = "always") # missing obs: 140
hist(data2$education)


### married (marital, maritala, maritalb)

table(data1$marital, data1$essround, useNA = "always") # 1 to 2
table(data1$maritala, data1$essround, useNA = "always") # 3 to 4
table(data1$maritalb, data1$essround, useNA = "always") # 5 to 10
table(data2$maritalb, useNA = "always")

data1 <- data1 %>% mutate(married = case_when(
  marital == 1 | maritala <= 2 | maritalb <= 2 ~ 1, # legal relationship (married or civil partnership)
  (marital >= 2 & marital <= 5) | (maritala >=3 & maritala <= 9) | (maritalb >= 3 & maritalb <= 6) ~ 0, # others 
  marital %in% c(7, 8, 9) ~ NA_integer_, 
  maritala %in% c(77, 88, 99) ~ NA_integer_, 
  maritalb %in% c(7, 8, 9) ~ NA_integer_)) 

data1 <- data1 %>% mutate(married_NAfill = case_when(
  marital == 1 | maritala <= 2 | maritalb <= 2 ~ 1, # legal relationship (married or civil partnership)
  (marital >= 2 | maritala >=3 | maritalb >= 3) ~ 0, # others 
  TRUE ~ NA_integer_))

table(data1$married, useNA = "always") # missing obs: 33,683
table(data1$married_NAfill, useNA = "always") # missing obs: 28,922


data2 <- data2 %>% mutate(married = case_when(
  maritalb <= 2 ~ 1, # legal relationship (married or civil partnership)
  (maritalb >= 3 & maritalb <= 6) ~ 0, # others
  maritalb %in% c(77, 88, 99) ~ NA_integer_)) 

data2 <- data2 %>% mutate(married_NAfill = case_when(
  maritalb <= 2 ~ 1, # legal relationship (married or civil partnership)
  maritalb >= 3 ~ 0, # others
  TRUE ~ NA_integer_)) 

table(data2$married, useNA = "always") # missing obs: 160
table(data2$married_NAfill, useNA = "always") # missing obs: 0

missing_all_married <- data1 %>%
  group_by(cntry, essround) %>%
  summarize(
    all_missing_married = all(is.na(married))
  ) %>%
  filter(all_missing_married == TRUE) # 9 countries missing marital status data in ESS round 10 (AT, CY, DE, ES, IL, LV, PL, RS)
                                      # EE-4, FI-5, FR-1 & 2

missing_all_married2 <- data2 %>%
  group_by(cntry, essround) %>%
  summarize(
    all_missing_married2 = all(is.na(married))
  ) %>%
  filter(all_missing_married2 == TRUE) # no missing country in ESS round 11


## income

table(data1$hinctnt, data1$essround, useNA = "always")  # 1 to 3
table(data1$hinctnta, data1$essround, useNA = "always") # 4 to 10

missing_check <- data1 %>%
  group_by(cntry, essround) %>%
  summarise(missing_check = if_else(all(hinctnt >= 77 | is.na(hinctnt)), 1, 0))
missing_check1<- missing_check %>% filter(essround <=3 & missing_check == 1) 
missing_check1     # EE-2&3, FR-1, HU-1&3, IE-1, UA-2&3

missing_check <- data1 %>%
  group_by(cntry, essround) %>%
  summarise(missing_check = if_else(all(hinctnta >= 77 | is.na(hinctnta)), 1, 0))
missing_check2<- missing_check %>% filter(essround >=4 & missing_check == 1) 
missing_check2    # BG-4, CY-4, EE-7, PT-5, SK-4

missing_check <- data2 %>%
  group_by(cntry,essround) %>%
  summarise(missing_check = if_else(all(hinctnta >= 77 | is.na(hinctnta)), 1, 0))
missing_check3<- missing_check %>% filter(essround == 11 & missing_check == 1) 
missing_check3 # no missing country-round

data1 <- data1 %>%
  mutate(hinctnt_10 = hinctnt * 10 / 12)

data1 <- data1 %>% mutate(income = case_when(
  hinctnt == 1 ~ 1*10/12,
  hinctnt == 2 ~ 2*10/12,
  hinctnt == 3 ~ 3*10/12,
  hinctnt == 4 ~ 4*10/12,
  hinctnt == 5 ~ 5*10/12,
  hinctnt == 6 ~ 6*10/12,
  hinctnt == 7 ~ 7*10/12,
  hinctnt == 8 ~ 8*10/12,
  hinctnt == 9 ~ 9*10/12,
  hinctnt == 10 ~ 10*10/12,
  hinctnt == 11 ~ 11*10/12,
  hinctnt == 12 ~ 12*10/12,
  hinctnta <= 10 ~ hinctnta,
  hinctnt %in% c(77,88,99) ~ NA_integer_,
  hinctnta %in% c(77,88,99) ~ NA_integer_
))

table(data1$income, useNA = "always") # missing obs: 118,636
hist(data1$income)

table(data2$hinctnta, useNA = "always") 
data2 <- data2 %>% mutate(income = case_when(
  hinctnta <= 10 ~ hinctnta,
  hinctnta %in% c(77,88,99) ~ NA_integer_
))

table(data2$income, useNA = "always") # missing obs: 3,984
hist(data2$income)


####### Major variables
## progay attitude: freehms, agree_gay

table(data1$freehms, data1$essround, useNA = "always")

missing_all_freehms <- data1 %>%
  group_by(cntry, essround) %>%
  summarize(
    all_missing_freehms = all(freehms %in% c(7, 8, 9))
  ) %>%
  filter(all_missing_freehms == TRUE) # no totally missing country-round

missing_all_freehms2 <- data2 %>%
  group_by(cntry, essround) %>%
  summarize(
    all_missing_freehms2 = all(freehms %in% c(7, 8, 9))
  ) %>%
  filter(all_missing_freehms2 == TRUE) # no totally missing country-round

data1 <- data1 %>% mutate(agree_gay = case_when(freehms == 1 ~ 1,
                                                freehms == 2 ~ 1,
                                                freehms == 3 ~ 0,
                                                freehms == 4 ~ 0,
                                                freehms == 5 ~ 0,
                                                TRUE ~ NA_integer_ ))

table(data1$agree_gay, useNA = "always") # missing obs: 20,886

data2 <- data2 %>% mutate(agree_gay = case_when(freehms == 1 ~ 1,
                                                freehms == 2 ~ 1,
                                                freehms == 3 ~ 0,
                                                freehms == 4 ~ 0,
                                                freehms == 5 ~ 0,
                                                TRUE ~ NA_integer_ ))

table(data2$agree_gay, useNA = "always") # missing obs: 422

## surveyyear
data1$surveyyear <- year(data1$interview_date)
summary(data1$surveyyear)
table(data1$surveyyear, useNA = "always")

data2$surveyyear <- year(data2$interview_date)
summary(data2$surveyyear)
table(data2$surveyyear, useNA = "always")

## country full name 
country_full_name <- c(
  AL = "Albania", AT = "Austria", BE = "Belgium", BG = "Bulgaria", CH = "Switzerland",
  CY = "Cyprus", CZ = "Czechia", DE = "Germany", DK = "Denmark", EE = "Estonia",
  ES = "Spain", FI = "Finland", FR = "France", GB = "United Kingdom", GR = "Greece",
  HR = "Croatia", HU = "Hungary", IE = "Ireland", IL = "Israel", IS = "Iceland",
  IT = "Italy", LT = "Lithuania", LU = "Luxembourg", LV = "Latvia", ME = "Montenegro",
  MK = "North Macedonia", NL = "Netherlands", NO = "Norway", PL = "Poland", PT = "Portugal",
  RO = "Romania", RS = "Serbia", RU = "Russia", SE = "Sweden", SI = "Slovenia",
  SK = "Slovakia", TR = "Turkey", UA = "Ukraine", XK = "Kosovo")

data1 <- data1 %>%
  mutate(country_full_name = country_full_name[cntry])

data2 <- data2 %>%
  mutate(country_full_name = country_full_name[cntry])

## legislation - I use the year of first same-sex marriage case (17 countries in the ESS)
data1 <- data1 %>% mutate(legislation = case_when(
  cntry =="AT" & surveyyear > 2019 ~ 1,    # Austria: 1/1/2019
  cntry =="BE" & surveyyear > 2003 ~ 1,    # Belgium: 6/1/2003
  cntry =="CH" & surveyyear > 2022 ~ 2,    # Switzerland: 7/1/2022
  cntry =="DE" & surveyyear > 2017 ~ 1,    # Germany: 10/1/2017
  cntry =="DK" & surveyyear > 2012 ~ 1,    # Denmark: 6/15/2012
  cntry =="ES" & surveyyear > 2005 ~ 1,    # Spain: 7/11/2005
  cntry =="FI" & surveyyear > 2017 ~ 2,    # Finland: 3/1/2017
  cntry =="FR" & surveyyear > 2013 ~ 1,    # France: 5/29/2013
  cntry =="GB" & surveyyear > 2014 ~ 1,    # United Kingdom: 3/29/2014
  cntry =="IE" & surveyyear > 2015 ~ 2,    # Ireland: 11/17/2015
  cntry =="IS" & surveyyear > 2010 ~ 1,    # Iceland: 6/27/2010
  cntry =="LU" & surveyyear > 2015 ~ 1,    # Luxembourg: 1/1/2015
  cntry =="NL" & surveyyear > 2001 ~ 1,    # Netherlands: 4/1/2001
  cntry =="NO" & surveyyear > 2009 ~ 1,    # Norway: 6/17/2009
  cntry =="PT" & surveyyear > 2010 ~ 1,    # Portugal: 6/7/2010
  cntry =="SE" & surveyyear > 2009 ~ 1,    # Sweden: 5/1/2009
  cntry =="SI" & surveyyear > 2022 ~ 1,    # Slovenia: 8/1/2022
  TRUE ~ 0
))

    # Countries without legislation - 22 countries in the ESS
    # Albania (family code defining marriage only between man and woman),
    # Bulgaria (ban constitutionally), Croatia (Life Partnership Act 2014),
    # Cyprus (civil union legal 2015), Czechia (civil union 2006), 
    # Estonia (same-sex marriage legalized in 2023, but no ESS data), 
    # Greece (same-sex marraige legalized in 2024, but no ESS data), 
    # Hungary (marriage ban 2011), Israel (permit same-sex marriage if outside Israel),
    # Italy (civil union 2016), Kosovo (Court ruled that same-sex marriage is legally compatible, but failed to legislate),
    # Latvia (civil union 2023), Lithuania (Constitutional ban), Montenegro (civil union 2020),
    # North Macedonia (no legal recognition of union or marriage), Poland (no legal recognition of union or marriage),
    # Romania (prohibited by Civil Code), Russian Federation (Constitutional ban),
    # Serbia (Constitutional ban), Slovakia (Constitutional ban),
    # Turkey (no legal recognition of union or marriage), Ukraine (no legal recognition of union or marriage)

table(data1$legislation, data1$cntry, useNA = "always")

data2 <- data2 %>% mutate(legislation = case_when(
  cntry =="AT" & surveyyear > 2019 ~ 1,
  cntry =="BE" & surveyyear > 2003 ~ 1,
  cntry =="CH" & surveyyear > 2022 ~ 2,
  cntry =="DE" & surveyyear > 2017 ~ 1,
  cntry =="DK" & surveyyear > 2012 ~ 1,
  cntry =="ES" & surveyyear > 2005 ~ 1,
  cntry =="FI" & surveyyear > 2017 ~ 2,
  cntry =="FR" & surveyyear > 2013 ~ 1,
  cntry =="GB" & surveyyear > 2014 ~ 1,
  cntry =="IE" & surveyyear > 2015 ~ 2,
  cntry =="IS" & surveyyear > 2010 ~ 1,
  cntry =="LU" & surveyyear > 2015 ~ 1,
  cntry =="NL" & surveyyear > 2001 ~ 1,
  cntry =="NO" & surveyyear > 2009 ~ 1,
  cntry =="PT" & surveyyear > 2010 ~ 1,
  cntry =="SE" & surveyyear > 2009 ~ 1,
  cntry =="SI" & surveyyear > 2022 ~ 1,
  TRUE ~ 0
))

table(data2$legislation, data2$cntry, useNA = "always")


saveRDS(data1, "Data/ESS_1to10_cleaned.rds")
saveRDS(data2, "Data/ESS_11_cleaned.rds")

#Merge: key variables (country, round, interview date, demographics, attitudes toward homosexuality, etc.)

ESS1to10 <- data1[, c("name", "essround", "proddate", "idno", "cntry", "country_full_name", "edition",
                      "dweight", "pweight", "interview_date", "surveyyear", "legislation", 
                      "agea", "gndr", "male", "education", "married", "married_NAfill",
                      "hinctnt", "hinctnta",
                      "income", "lrscale", "ideology_right10", "ideology_right10_NAfill", "galtan",
                      "rlgatnd", "religiosity", "religiosity_NAfill", "freehms", "agree_gay",
                      "pdjobev", "uemp12m", "jbspv"
)]   #31 vars

ESS11 <- data2[, c("name", "essround", "proddate", "idno", "cntry", "country_full_name", "edition",
                   "dweight", "pweight", "interview_date", "surveyyear", "legislation", 
                   "agea", "gndr", "male", "education", "married", "married_NAfill",
                   "hinctnta", "income", "lrscale", "ideology_right10", "ideology_right10_NAfill", 
                   "rlgatnd", "religiosity", "religiosity_NAfill", "freehms", "agree_gay",
                   "pdjobev", "uemp12m", "jbspv"
)]   #30 vars

library(plyr)

ESS1to11 <- rbind.fill(ESS1to10, ESS11)  # 39 vars, 512,745 obs

saveRDS(ESS1to11, "Data/ESS_1to11_cleaned.rds")



####### Imputation in the combined dataset

rm(list=ls())
library(dplyr)
library(tidyverse)
library(rio)
library(tidyr)
library(lubridate)
library(SGmisc)
library(stats)
library(mice)

data1 <- readRDS("Data/ESS_1to11_cleaned.rds")

## impute age with mean
table(data1$agea, useNA = "always") # 3,443
mean(data1$agea, na.rm = TRUE) # mean age: 49

data1 <- data1 %>% mutate(agea_NAfill = if_else(is.na(agea), 49, agea))
table(data1$agea_NAfill, useNA = "always")
hist(data1$agea)
hist(data1$agea_NAfill)


## impute income with multiple imputation - mice (https://www.appsilon.com/post/imputation-in-r)
#missing income: EE (2, 3), FR (1), HU (1,3), 
#                IE (1), UA (2, 3), BG (4), CY (4), EE (7), PT (5), SK (4)
# Imputation

data1 <- data1 %>% mutate(had_paidjob = case_when(
  pdjobev == 1 ~ 1, # Ever had paid job?
  TRUE ~ 0))

data1 <- data1 %>% mutate(unemployed12 = case_when(
  uemp12m == 1 ~ 1, # Any period of unemployment and work seeking lasted 12 months or more
  TRUE ~ 0))

data1 <- data1 %>% mutate(supervisor = case_when(
  jbspv == 1 ~ 1, # Any period of unemployment and work seeking lasted 12 months or more
  TRUE ~ 0))


library(mice)
impute_dataset <- data1 %>% dplyr::select(income, education, 
                                          agea, male, married, 
                                          had_paidjob, unemployed12, supervisor)
mice_imputed <- data.frame(
  original = impute_dataset$income,
  imputed_pmm = complete(mice(impute_dataset, method = "pmm"))$income
)

mice_imputed <- mice_imputed %>% dplyr::rename(income_imputed = imputed_pmm)
data1$income_imputed <- mice_imputed$income_imputed
rm(impute_dataset, mice_imputed)


table(data1$income, useNA = "always")
table(data1$income_imputed, useNA = "always")


saveRDS(data1, "Data/ESS_1to11_cleaned_imputed.rds")

