### Code for "Hatchery supplementation provides a demographic boost but alters age composition of sockeye salmon in Auke Lake, Southeast Alaska"
### Author: Megan McPhee, University of Alaska Fairbanks
### Date: 2021-2023

#Parentage data wrangling and calculating individual reproductive success (n_offspring)

library (tidyverse)

demog.all <- read.csv ("data/SAUKE_DemographicInformation_V12.csv", header = TRUE)
parentage.all <- read.csv ("data/parentage.csv", header = TRUE) #comes from parentage analysis in FRANz, following methods described in the manuscript.

#set all fish IDs to lowercase
demog.all$franzID <- tolower (demog.all$franzID)

parentage.all$Offspring <- tolower (parentage.all$Offspring)
parentage.all$Parent.1 <- tolower (parentage.all$Parent.1)
parentage.all$Parent.2 <- tolower (parentage.all$Parent.2)

parentage.all$broodyear <- 2000 + as.numeric (substr (parentage.all$Parent.1, 1, 2))


# Parentage with dyad assignment LOD ≥ 4.5 for RP analysis

#identify accepted assignments
parentage45 <- parentage.all %>% mutate (parent1 = case_when( 
  Pair.LOD.Parent.1 >= 4.5 ~ Parent.1
)) %>% mutate (parent2 = case_when (
  Pair.LOD.Parent.2 >= 4.5 ~ Parent.2 
)) %>% select (c (Offspring, parent1, parent2, broodyear, Pair.LOD.Parent.1, Pair.LOD.Parent.2, Posterior.Parent.1, Posterior.Parent.2))

parentage45 <- parentage45 %>% filter (broodyear == 2011 | broodyear == 2012 | broodyear == 2013)


parentage45 <- parentage45 %>% mutate (samesex = case_when(
  substr (parent1, 4, 4) == "f" & substr (parent2, 4, 4) == "f" ~ "hof",
  substr (parent1, 4, 4) == "m" & substr (parent2, 4, 4) == "m" ~ "hom",
  substr (parent1, 4, 4) == "m" & substr (parent2, 4, 4) == "j" ~ "hom",
  substr (parent1, 4, 4) == "j" & substr (parent2, 4, 4) == "m" ~ "hom",
  TRUE ~ "het"
)) 
#technically "het" also includes offspring w/ only 1 parent identified, but that's ok b/c we just want to identify same-sex pairs to try to correct them.

table (parentage45$samesex)


#Identify same-sex pairings to determine if we can correct sex mis-identification. 
#This process is rather long and is in its own file, "scripts/SAUKE_F1_Rscript_03_Samesex_pairings.Rmd")
#  NB: same-sex pairings were only evaluated for BY 2011-2013


#identify accepted assignments @ LOD ≥ 4.5
parent45 <- parentage.all %>% mutate (parent1 = case_when( 
  Pair.LOD.Parent.1 >= 4.5 ~ Parent1.updated
)) %>% mutate (parent2 = case_when (
  Pair.LOD.Parent.2 >= 4.5 ~ Parent2.updated 
)) %>% select (c (Offspring, parent1, parent2, broodyear, Pair.LOD.Parent.1, Pair.LOD.Parent.2, Posterior.Parent.1, Posterior.Parent.2))

parent45 <- parent45 %>% filter (broodyear == 2011 | broodyear == 2012 | broodyear == 2013)


#check to see if any same-sex pairings remain

parent45 <- parent45 %>% mutate (samesex = case_when(
  substr (parent1, 4, 4) == "f" & substr (parent2, 4, 4) == "f" ~ "yes",
  substr (parent1, 4, 4) == "m" & substr (parent2, 4, 4) == "m" ~ "yes",
  substr (parent1, 4, 4) == "m" & substr (parent2, 4, 4) == "j" ~ "yes",
  substr (parent1, 4, 4) == "j" & substr (parent2, 4, 4) == "m" ~ "yes",
  TRUE ~ "no"
)) 

table (parent45$samesex)



#Parentage with dyad assignment LOD ≥ 9.0 for analysis of phenotypes
#Starting with parent45, convert assignments to NA if LOD < 9


#identify accepted assignments @ LOD ≥ 9.0
parent9 <- parent45 %>% mutate (parent1 = case_when( 
  Pair.LOD.Parent.1 >= 9.0 ~ parent1
)) %>% mutate (parent2 = case_when (
  Pair.LOD.Parent.2 >= 9.0 ~ parent2
)) 



# identify potential parents

g13 <- read.csv ("data/SAUKE_Genotypes_V13.csv", header = TRUE)
g13 <- g13 %>% select (c(-SillyCodeID, -One_GTHa, -One_MHC2_190))
names (g13)[1] <- "franzID"
g13$franzID <- tolower (g13$franzID)


#Make long

gt.long <- g13 %>% 
  pivot_longer(!franzID, names_to = "Locus", values_to = "genotype")
str (gt.long)


#Assign a locus type to each

gt.long <- gt.long %>%
  mutate (LocType = case_when(
    Locus == "One_Oki10" ~ "STR",
    Locus == "One_Oki100" ~ "STR",
    Locus == "One_Oki1a" ~ "STR",
    Locus == "One_Oki1b" ~ "STR",
    Locus == "One_One102" ~ "STR",
    Locus == "One_One109" ~ "STR",
    Locus == "One_One114" ~ "STR",
    Locus == "One_One8" ~ "STR",
    Locus == "One_Ssa419" ~ "STR",
    TRUE ~ "SNP"
  ))
head (gt.long, 57)


#Divide the df into individuals with sufficient or insufficient genotypes (SNPs or STRs). 

sufficient.str <- gt.long %>% 
  filter (LocType == "STR") %>%
  filter (genotype != "?/?") %>%
  group_by(franzID) %>%
  count () #n = 26,512

nrow (g13 %>% filter (str_detect (franzID, "^11"))) #n= 2422
nrow (g13 %>% filter (str_detect (franzID, "^12"))) #n= 1554
nrow (g13 %>% filter (str_detect (franzID, "^13"))) #n= 2055


range (sufficient.str$n) #1-9 good

sufficient.str <- sufficient.str %>%
  filter (n >= 5) #26,417 observations (of 26,512 total, or 99.6%%)


sufficient.snp <- gt.long %>% 
  filter (LocType == "SNP") %>%
  filter (genotype != "?/?") %>%
  group_by(franzID) %>%
  count () #n = 26,340

range (sufficient.snp$n) # 1-45

sufficient.snp <- sufficient.snp %>%
  filter (n >= 22) #26,210 observations (of 26,340 total, or 99.5%)


#identify all 2011-2013 adults that meet the minimum genotyping requirement as "potential parents"

names (sufficient.str)[2] <- "n_str"
names (sufficient.snp)[2] <- "n_snp"


all.potential.parents <- as.data.frame (merge (sufficient.str, sufficient.snp, by = "franzID")) %>% filter (str_detect(franzID, "^11") | str_detect(franzID, "^12") | str_detect(franzID, "^13")) #5,910 observations

range (all.potential.parents$n_str) #should be 5-9.
range (all.potential.parents$n_snp) # should be 22-45.

#finally weed out those with fewer than 40 loci in total genotyped (mintype --40)
all.potential.parents$n_tot <- all.potential.parents$n_snp + all.potential.parents$n_str
all.potential.parents <- all.potential.parents %>% filter (n_tot >= 40) #from 5910 to 5884.

#calculate indivdiual reproductive success
#add parental type and ma/pa information to parent45


parent45 <- parent45 %>% mutate (ma = case_when(
  substr (parent1, 4, 4) == "f" ~ parent1,
  substr (parent2, 4, 4) == "f" ~ parent2,
  TRUE ~ "unknown"
)) %>% mutate (pa = case_when (
  substr (parent1, 4, 4) == "m" ~ parent1,
  substr (parent1, 4, 4) == "j" ~ parent1,
  substr (parent2, 4, 4) == "m" ~ parent2,
  substr (parent2, 4, 4) == "j" ~ parent2,  
))


#Group by and tally number of offspring
#Mas:
ma45_noffspring <- parent45 %>% group_by (ma) %>% tally ()
names(ma45_noffspring)[1] <- "franzID"
names(ma45_noffspring)[2] <- "n_offspring"

ma45_noffspring$Psex <- "female"


#Pas:
pa45_noffspring <- parent45 %>% group_by (pa) %>% tally ()
names(pa45_noffspring)[1] <- "franzID"
names(pa45_noffspring)[2] <- "n_offspring"

pa45_noffspring$Psex <- "male"

#Merge into single df:
lod45_RS <- rbind (ma45_noffspring, pa45_noffspring)
rm (ma45_noffspring, pa45_noffspring)

#check for any duplicates
lod45_RS$dup <- duplicated (lod45_RS$franzID)
table (lod45_RS$dup) #none

#get rid of "NA" and "unknown" parents
lod45_RS <- lod45_RS %>% filter (!is.na (franzID)) %>% filter (franzID != "unknown")


#Before matching this df to the "all.potential.parents", we need to deal with those fish from samesex crosses that had their sex ID updated. 
#Also need to remove the hatchery males in 2013 that were recorded as "not spawned" from the zero-offspring df.

lod45_RS$temp.id <- lod45_RS$franzID 
substr (lod45_RS$temp.id , 4, 4) <- "x"

all.potential.parents$temp.id <- all.potential.parents$franzID
substr (all.potential.parents$temp.id , 4, 4) <- "x"

zero45 <- anti_join (all.potential.parents, lod45_RS, by = c("temp.id" = "temp.id")) #3081+2806

slice (genotypes.all) %>% filter (franzID %in% (c ("13wf263352", "13wj263667", "13wm262019"))) #ok
#genotypes.all created during the datawrangling for CKMRsim; see "scripts/SAUKE_F1_Rscript_01_CKMRsim.R"


#Merge zero45 and lod45_RS

zero45 <- zero45 %>% select (c(franzID))
zero45$n_offspring <- 0
zero45 <- zero45 %>% mutate (Psex = case_when(
  substr(franzID, 4, 4) == "f" ~ "female",
  substr (franzID, 4, 4) == "j" ~ "male",
  substr (franzID, 4, 4) == "m" ~ "male"
)) 

#filter out 2013 hatchery males marked as "not spawned"
zero45 <- zero45  %>% filter (!franzID %in% c("13hm263944", "13hm263946", "13hm263947", "13hm263948", "13hm263949", "13hm263950" , "13hm263951", "13hm263952", "13hm263953"))


lod45_RS <- lod45_RS %>% select (c(franzID, n_offspring, Psex))
lod45_RS <- rbind (lod45_RS, zero45)


#add parental type and brood year
lod45_RS <- lod45_RS %>% mutate (Ptype = case_when(
  substr (franzID, 3, 3) == "h" ~ "hatchery",
  TRUE ~ "wild"
)) 

lod45_RS$by <- 2000 + as.numeric (substr (lod45_RS$franzID, 1, 2))

table (lod45_RS$Psex, lod45_RS$Ptype, lod45_RS$by)

#results from data wrangling and calculations of the number of offspring are used to calculate RRS
#see "scripts/SAUKE_F1_Rscript_04_RRS.R"
