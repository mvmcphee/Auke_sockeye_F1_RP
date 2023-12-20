### Code for "Hatchery supplementation provides a demographic boost but alters age composition of sockeye salmon in Auke Lake, Southeast Alaska"
### Author: Megan McPhee, University of Alaska Fairbanks
### Date: 2021-2023

#Analysis of phenotypes

library (tidyverse)
library (lubridate)
library (lme4)
library (merTools)
library (MuMIn)

#For this analysis we chose a more stringent LOD threshold for accepting parent-offspring assigments (LOD ≥ 9). These assignments are found in the df 'parent9'. The values in parent1 and parent2 include individuals whose parent names have been udpated in 'samesex-pairings.Rmd.'. This shouldn't affect the phenotype analyses since those are focused on the offspring.
#Need to update parent9 to remove those who didn't have either parent assigned at LOD ≥ 9


parent9 <- parent9 %>% filter (!is.na (parent1) | !is.na(parent2)) #6,863 to 6,441 obs.

#Run timing
#Response variable is date of capture at the weir (return date, rd). Test is Kolgorov-Smirnov test for differences in distribution of return date by parental type within a brood year or return year.

#Make new df that joins parent9 with offspring demographic information, then assign parental type (hatchery vs. wild)


demog9 <- left_join (parent9, demog.all, by = c("Offspring" = "franzID")) #6,441 obs

demog9 <- demog9 %>% mutate (ptype = case_when(
  substr(parent1, 3, 3) == "h" | substr (parent2, 3, 3) == "h" ~ "hatchery",
  TRUE ~ "wild"
))

table (demog9$ptype, useNA = "always")


#There are some 'NAs' in the JulianDayReturn column. Let's just create a new variable that gets that info from the DateCollected column.

demog9$capturedate <- mdy (demog9$DateCollected)
demog9$doy <- yday(demog9$capturedate)

slice (demog9 %>% filter (is.na(doy))) #a few obs with missing data

demog9 <- demog9 %>% filter (!is.na(doy)) #down to 6,439 obs.



#Run timing by brood year

rd11.h <- as.numeric (unlist ((demog9 %>% filter (broodyear == 2011 & ptype == "hatchery") %>% dplyr::select (doy))))
rd11.w <- as.numeric (unlist ((demog9 %>% filter (broodyear == 2011 & ptype == "wild") %>% dplyr::select (doy))))
ks.test (rd11.h, rd11.w)
median (rd11.h)
median (rd11.w)

median (rd11.h)- median (rd11.w)


rd12.h <- as.numeric (unlist ((demog9 %>% filter (broodyear == 2012 & ptype == "hatchery") %>% dplyr::select (doy))))
rd12.w <- as.numeric (unlist ((demog9 %>% filter (broodyear == 2012 & ptype == "wild") %>% dplyr::select (doy))))
ks.test (rd12.h, rd12.w)
median (rd12.h)
median (rd12.w)

median (rd12.h)- median (rd12.w)

rd13.h <- as.numeric (unlist ((demog9 %>% filter (broodyear == 2013 & ptype == "hatchery") %>% dplyr::select (doy))))
rd13.w <- as.numeric (unlist ((demog9 %>% filter (broodyear == 2013 & ptype == "wild") %>% dplyr::select (doy))))
ks.test (rd13.h, rd13.w)
median (rd13.h)
median (rd13.w)

median (rd13.h)- median (rd13.w)


#By return year (2015-2019)


rd15.h <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2015 & ptype == "hatchery") %>% dplyr::select (doy))))
rd15.w <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2015 & ptype == "wild") %>% dplyr::select (doy))))
ks.test (rd15.h, rd15.w)
median (rd15.h)
median (rd15.w)

median (rd15.h)- median (rd15.w)

rd16.h <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2016 & ptype == "hatchery") %>% dplyr::select (doy))))
rd16.w <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2016 & ptype == "wild") %>% dplyr::select (doy))))
ks.test (rd16.h, rd16.w)
median (rd16.h)
median (rd16.w)

median (rd16.h)- median (rd16.w)

rd17.h <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2017 & ptype == "hatchery") %>% dplyr::select (doy))))
rd17.w <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2017 & ptype == "wild") %>% dplyr::select (doy))))
ks.test (rd17.h, rd17.w)
median (rd17.h)
median (rd17.w)

median (rd17.h)- median (rd17.w)

rd18.h <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2018 & ptype == "hatchery") %>% dplyr::select (doy))))
rd18.w <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2018 & ptype == "wild") %>% dplyr::select (doy))))
ks.test (rd18.h, rd18.w)
median (rd18.h)
median (rd18.w)

median (rd18.h)- median (rd18.w)

rd19.h <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2019 & ptype == "hatchery") %>% dplyr::select (doy))))
rd19.w <- as.numeric (unlist ((demog9 %>% filter (ReturnYear == 2019 & ptype == "wild") %>% dplyr::select (doy))))
ks.test (rd19.h, rd19.w)
median (rd19.h)
median (rd19.w)

median (rd19.h)- median (rd19.w)

#Sequential (Holm-) Bonferroni alpha values:

0.05/8
0.05/7
0.05/6
0.05/5
0.05/4
0.05/3
0.05/2


#Visual presentation of results: Figure 4 - see "scripts/SAUKE_F1_MainFigures.R"


#Run timing for only parents that returned on the same dates that hatchery broodstock were collected

#We have demog9, which includes all offspring assigned to at least one parent with LOD >= 9, and includes all the phenotypic data for the offspring, but NOT the parents. 
#Next thing to do is get the doy data for the parents.

#First, create a new df that has parents and their doy of capture at the weir.


pd.temp <- demog.all %>% filter (ReturnYear > 2010 & ReturnYear < 2014) %>% dplyr::select (franzID, DateCollected)
names (pd.temp)[1] <- "parentID"
names (pd.temp)[2] <- "parent-capture-date"


#do a join with demog9; want to keep all the columns in demog9 and rename the columns in pd.temp

doy.join.temp <- left_join (demog9, pd.temp, by = c("parent1" = "parentID"))
names (doy.join.temp)[23] <- "par1_capture_date"

demog9.parent.doy <- left_join (doy.join.temp, pd.temp, by = c("parent2" = "parentID"))
names (demog9.parent.doy)[24] <- "par2_capture_date"
rm (doy.join.temp, pd.temp)


#Next, filter to retain only those individuals who have parents that returned the same day that the hatchery fish were sampled.
#2011: 7/25/2011 & 7/26/2011
#2012: 7/17/2012 & 7/18/2012
#2013: 7/10/2013 & 7/11/2013

phenotypes.doy <- demog9.parent.doy %>% mutate (retain = case_when(
  par1_capture_date == "07/25/2011" | par2_capture_date == "07/25/2011" | is.na (par2_capture_date) ~ "retain",
  par1_capture_date == "07/26/2011" | par2_capture_date == "07/26/2011" | is.na (par2_capture_date) ~ "retain",
  par1_capture_date == "07/17/2012" | par2_capture_date == "07/17/2012" | is.na (par2_capture_date) ~ "retain",
  par1_capture_date == "07/18/2012" | par2_capture_date == "07/18/2012" | is.na (par2_capture_date) ~ "retain",
  par1_capture_date == "07/10/2013" | par2_capture_date == "07/10/2013" | is.na (par2_capture_date) ~ "retain",
  par1_capture_date == "07/11/2013" | par2_capture_date == "07/11/2013" | is.na (par2_capture_date) ~ "retain",
  TRUE ~ "reject"
))

table (phenotypes.doy$retain, phenotypes.doy$ptype)



##Return date
#Run timing by brood year - just the wild parents that returned same days as hatchery parents:

rd11.h1 <- as.numeric (unlist ((phenotypes.doy %>% filter (broodyear == 2011 & ptype == "hatchery") %>% dplyr::select (doy))))
rd11.w1 <- as.numeric (unlist ((phenotypes.doy %>% filter (broodyear == 2011 & ptype == "wild" & retain == "retain") %>% dplyr::select (doy))))
ks.test (rd11.h1, rd11.w1)
median (rd11.h1)
median (rd11.w1)

median (rd11.h1)- median (rd11.w1)

rd12.h1 <- as.numeric (unlist ((phenotypes.doy %>% filter (broodyear == 2012 & ptype == "hatchery") %>% dplyr::select (doy))))
rd12.w1 <- as.numeric (unlist ((phenotypes.doy %>% filter (broodyear == 2012 & ptype == "wild" & retain == "retain") %>% dplyr::select (doy))))
ks.test (rd12.h1, rd12.w1)
median (rd12.h1)
median (rd12.w1)

median (rd12.h1)- median (rd12.w1)

rd13.h1 <- as.numeric (unlist ((phenotypes.doy %>% filter (broodyear == 2013 & ptype == "hatchery") %>% dplyr::select (doy))))
rd13.w1 <- as.numeric (unlist ((phenotypes.doy %>% filter (broodyear == 2013 & ptype == "wild" & retain == "retain") %>% dplyr::select (doy))))
ks.test (rd13.h1, rd13.w1)
median (rd13.h1)
median (rd13.w1)

median (rd13.h1)- median (rd13.w1)


#By return year (2015-2019)

rd15.h1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2015 & ptype == "hatchery") %>% dplyr::select (doy))))
rd15.w1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2015 & ptype == "wild" & retain == "retain") %>% dplyr::select (doy))))
ks.test (rd15.h1, rd15.w1)
median (rd15.h1)
median (rd15.w1)

median (rd15.h1)- median (rd15.w1)

rd16.h1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2016 & ptype == "hatchery" ) %>% dplyr::select (doy))))
rd16.w1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2016 & ptype == "wild" & retain == "retain") %>% dplyr::select (doy))))
ks.test (rd16.h1, rd16.w1)
median (rd16.h1)
median (rd16.w1)

median (rd16.h1)- median (rd16.w1)

rd17.h1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2017 & ptype == "hatchery") %>% dplyr::select (doy))))
rd17.w1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2017 & ptype == "wild" & retain == "retain") %>% dplyr::select (doy))))
ks.test (rd17.h1, rd17.w1)
median (rd17.h1)
median (rd17.w1)

median (rd17.h1)- median (rd17.w1)

rd18.h1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2018 & ptype == "hatchery") %>% dplyr::select (doy))))
rd18.w1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2018 & ptype == "wild" & retain == "retain") %>% dplyr::select (doy))))
ks.test (rd18.h1, rd18.w1)
median (rd18.h1)
median (rd18.w1)

median (rd18.h1)- median (rd18.w1)

rd19.h1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2019 & ptype == "hatchery") %>% dplyr::select (doy))))
rd19.w1 <- as.numeric (unlist ((phenotypes.doy %>% filter (ReturnYear == 2019 & ptype == "wild" & retain == "retain") %>% dplyr::select (doy))))
ks.test (rd19.h1, rd19.w1)
median (rd19.h1)
median (rd19.w1)

median (rd19.h1)- median (rd19.w1)


##Age composition: all fish (parentage-based age)
#Make a new age variable, p_age, that is based on parentage.
#Pool the two youngest ages into single cell, then create contingency tables.


demog9$p_age <- demog9$ReturnYear - demog9$broodyear

demog9 <- demog9 %>% mutate (age_pool = case_when(
  p_age == 3 ~ 4,
  TRUE ~ p_age
))


demog9 %>% group_by(broodyear, ptype, p_age) %>% tally()

#Data for Table 4:

demog9 %>% filter (broodyear == 2011) %>% group_by (ptype, p_age) %>% tally()
demog9 %>% filter (broodyear == 2012) %>% group_by (ptype, p_age) %>% tally()
demog9 %>% filter (broodyear == 2013) %>% group_by (ptype, p_age) %>% tally()


#Contingency tests:

at_11 <- matrix (c(69, 322, 163, 264, 1191, 2187), nrow = 2, ncol = 3, byrow = T)
rownames(at_11) <- c('hatchery', 'wild')
colnames(at_11) <- c('Age34', 'Age5', 'Age6')
at_11

chisq.test(at_11)


at_12 <- matrix (c(104, 556, 51, 26, 345, 157), nrow = 2, ncol = 3, byrow = T)
rownames(at_12) <- c('hatchery', 'wild')
colnames(at_12) <- c('Age34', 'Age5', 'Age6')
at_12

chisq.test(at_12)

at_13 <- matrix (c(57, 186, 36, 58, 353, 314), nrow = 2, ncol = 3, byrow = T)
rownames(at_13) <- c('hatchery', 'wild')
colnames(at_13) <- c('Age34', 'Age5', 'Age6')
at_13

chisq.test(at_13)

rm (at_11, at_12, at_13)


# Age-Sex-Length (ASL) sampled fish (parentage- vs. scale-based age)


demog9$fw_age <- as.numeric (substr(demog9$ScaleAge, 1, 1))
demog9$sw_age <- as.numeric (substr(demog9$ScaleAge, 3, 3))

asl <- demog9 %>% filter (AgeYearsScales > 2) #gets rid of individuals with no/bad scale data

asl$age.disc <- asl$p_age - asl$AgeYearsScales #negative value means that scale age older than parentage age
table (asl$age.disc)


#What is the distribution of freshwater ages in hatchery vs. wild-born fish?

asl  %>% filter (age.disc == 0) %>% group_by (ptype, fw_age, broodyear) %>% tally()

#contingency tests

ct2011 <- matrix (c(27, 60, 4, 174), nrow = 2, ncol = 2, byrow = T)
rownames(ct2011) <- c("FW1", "FW2")
colnames(ct2011) <- c("hatchery", "wild")
ct2011

fisher.test (ct2011)


ct2012 <- matrix (c(31, 13, 14, 40), nrow = 2, ncol = 2, byrow = T)
rownames(ct2012) <- c("FW1", "FW2")
colnames(ct2012) <- c("hatchery", "wild")
ct2012

fisher.test (ct2012)

ct2013 <- matrix (c(14, 21, 14, 71), nrow = 2, ncol = 2, byrow = T)
rownames(ct2013) <- c("FW1", "FW2")
colnames(ct2013) <- c("hatchery", "wild")
ct2013

fisher.test (ct2013)

#age composition for only parents who returned on the same dates as broodstock: (see lines 152-190 for data wrangling)
#Statistical comparisons: pooling youngest ages for chi-sq contingency tests
phenotypes.doy$p_age <- phenotypes.doy$ReturnYear - phenotypes.doy$broodyear

phenotypes.doy <- phenotypes.doy %>% mutate (age_pool = case_when(
  p_age == 3 ~ 4,
  TRUE ~ p_age
))


phenotypes.doy %>% group_by(broodyear, ptype, p_age) %>% tally()

#Data for Table 4:
  

phenotypes.doy %>% filter (broodyear == 2011 & retain == "retain") %>% group_by (ptype, p_age) %>% tally()

phenotypes.doy %>% filter (broodyear == 2012 & retain == "retain") %>% group_by (ptype, p_age) %>% tally()

phenotypes.doy %>% filter (broodyear == 2013 & retain == "retain") %>% group_by (ptype, p_age) %>% tally()


#Contingency tests:

rt_11 <- matrix (c(69, 322, 163, 22+129, 645, 1219), nrow = 2, ncol = 3, byrow = T)
rownames(rt_11) <- c('hatchery', 'wild')
colnames(rt_11) <- c('Age34', 'Age5', 'Age6')
rt_11

chisq.test(rt_11)

rt_12 <- matrix (c(104, 556, 51, 5+14, 99, 52), nrow = 2, ncol = 3, byrow = T)
rownames(rt_12) <- c('hatchery', 'wild')
colnames(rt_12) <- c('Age34', 'Age5', 'Age6')
rt_12

chisq.test(rt_12)

rt_13 <- matrix (c(57, 186, 36, 5+18, 111, 105), nrow = 2, ncol = 3, byrow = T)
rownames(rt_13) <- c('hatchery', 'wild')
colnames(rt_13) <- c('Age34', 'Age5', 'Age6')
rt_13

chisq.test(rt_13)

rm (rt_11, rt_12, rt_13)




###Size at age
#making some goofy values so I can get ages to appear on x axis as desired (this was done for making figures)
asl.saa <- asl %>% mutate (age.class = case_when(
  ptype == "wild" & ScaleAge == "1.1" ~ 5.1,
  ptype == "wild" & ScaleAge == "2.1" ~ 6.1,
  ptype == "hatchery" & ScaleAge == "1.2" ~ 6.9,
  ptype == "wild" & ScaleAge == "1.2" ~ 7.1,
  ptype == "hatchery" & ScaleAge == "2.2" ~ 7.9,
  ptype == "wild" & ScaleAge == "2.2" ~ 8.1,
  ptype == "hatchery" & ScaleAge == "1.3" ~ 8.9,
  ptype == "wild" & ScaleAge == "1.3" ~ 9.1,
  ptype == "hatchery" & ScaleAge == "2.3" ~9.9,
  ptype == "wild" & ScaleAge == "2.3" ~ 10.1
))

asl.saa <- asl.saa %>% mutate (age.class2 = case_when(
  broodyear == 2011 ~ age.class -0.05,
  broodyear == 2012 ~ age.class,
  broodyear == 2013 ~ age.class +0.05
))

asl.saa.mod <- asl.saa %>% filter (ScaleAge != "1.1") %>% filter (ScaleAge != "2.1")
asl.saa.mod$ScaleAge <- factor(asl.saa.mod$ScaleAge)
asl.saa.mod$ScaleAge <- droplevels(asl.saa.mod$ScaleAge)
asl.saa.mod$ScaleAgeOrdered <- ordered(asl.saa.mod$ScaleAge, levels = c("1.2", "2.2", "1.3", "2.3"))

asl.saa.mod <- asl.saa.mod %>% filter (age.disc == 0) #remove individuals w/ conflicting scale and parentage-based ages


#create a sex variable that doesn't have jack as a level:
asl.saa.mod$Sex <- tolower (asl.saa.mod$Sex)
asl.saa.mod <- asl.saa.mod %>% mutate (sex = case_when(
  Sex == "m" ~ "male",
  Sex == "j" ~ "male",
  Sex == "f" ~ "female"
))

table (asl.saa.mod$sex)

#Does size-at-age vary between hatchery and wild-origin fish? Nuisance variables: sex, age, brood year, interactions.
#Identify any rows with missing data so can omit prior to model comparisons.
 
table (asl.saa.mod$ptype)
table (asl.saa.mod$sex)
table (asl.saa.mod$ScaleAge)
table (asl.saa.mod$broodyear)

asl.saa.mod %>% filter (is.na (MidEyeForkLength))


saa.interaction.full <- lm(MidEyeForkLength ~ ptype*sex*ScaleAge*broodyear, data = asl.saa.mod, na.action = "na.fail")
dredge (saa.interaction.full)


#This shows evidence for brood year interactions with the other predictor variables, but no other interactions. Analyze each brood year separately, then.


saa.by11 <- lm(MidEyeForkLength ~ ptype+sex+ScaleAge, data = asl.saa.mod %>% filter (broodyear == 2011))
summary (saa.by11)

saa.by12 <- lm(MidEyeForkLength ~ ptype+sex+ScaleAge, data = asl.saa.mod %>% filter (broodyear == 2012))
summary (saa.by12)

saa.by13 <- lm(MidEyeForkLength ~ ptype+sex+ScaleAge, data = asl.saa.mod %>% filter (broodyear == 2013))
summary (saa.by13)


#Examining model assumptions:

plot (saa.by11)

plot (saa.by12)

plot (saa.by13)

#visualization of results (Figure 5) - see "scripts/SAUKE_F1_MainFigures.R"


#Phenotypes of parents
#Phenotypic comparisons of hatchery and wild spawners by brood year: a) body length; b) date of capture at the Auke Creek weir. 
#Summary stats for reporting, as well as for Supplementary Figure S1

parent.demog <- demog.all %>% filter (ReturnYear > 2010 & ReturnYear < 2014)
parent.demog$TypeAsSpawningAdult <- tolower (parent.demog$TypeAsSpawningAdult)
parent.demog$capturedate <- mdy (parent.demog$DateCollected)
parent.demog$doy <- yday (parent.demog$capturedate)

parent.demog <- parent.demog %>% filter (TypeAsSpawningAdult != "hatchery_notspawned")

#TypeAsSpawningAdult distinguishes wild from radio-tagged; for this purpose these should be pooled
parent.demog <- parent.demog %>% mutate (sp_type = case_when(
  TypeAsSpawningAdult == "hatchery" ~ "hatchery",
  TRUE ~ "wild"
))

parent.demog$Sex <- tolower (parent.demog$Sex)

parent.demog <- parent.demog %>% mutate (Sex = case_when(
  Sex == "f" ~ "female",
  Sex == "f\n" ~ "female", 
  Sex == "female" ~ "female",
  Sex == "j" ~ "jack",
  Sex == "j " ~ "jack",
  Sex == "jack" ~ "jack",
  Sex == "m" ~ "male",
  Sex == "male" ~ "male"
))

table (parent.demog$Sex)



#Are the differences in size between hatchery and wild significant?


kw.df.tmp <- parent.demog %>% filter (MidEyeForkLength > 0) %>% filter (Sex != "jack") %>% dplyr::select (c(ReturnYear, Sex, sp_type, MidEyeForkLength)) %>% 
  mutate (grp = interaction (sp_type, Sex))

table (kw.df.tmp$grp)

kruskal.test (MidEyeForkLength ~ grp, data = kw.df.tmp %>% filter (ReturnYear == 2011))

kruskal.test (MidEyeForkLength ~ grp, data = kw.df.tmp %>% filter (ReturnYear == 2012))

kruskal.test (MidEyeForkLength ~ grp, data = kw.df.tmp %>% filter (ReturnYear == 2013))


#And how do they differ?

kw.df.tmp %>% group_by (factor(ReturnYear), Sex, sp_type) %>% summarize (med.ln = median(MidEyeForkLength), iqr.lo = quantile (MidEyeForkLength, probs = 0.25), iqr.hi = quantile (MidEyeForkLength, probs = 0.75))


#Summary stats for return date

parent.demog %>% group_by (ReturnYear, sp_type) %>% summarize (med = median(doy))

#Supplementary Figure S1: TK



