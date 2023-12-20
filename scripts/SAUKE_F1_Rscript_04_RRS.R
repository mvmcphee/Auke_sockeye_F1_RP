### Code for "Hatchery supplementation provides a demographic boost but alters age composition of sockeye salmon in Auke Lake, Southeast Alaska"
### Author: Megan McPhee, University of Alaska Fairbanks
### Date: 2021-2023


#Calculating relative reproductive success and 95% confidence intervals for females

library (tidyverse)

#Function to calculate 95% confidence intervals per Kalinowski & Taper 2005.This function is from Shedd et al. 2022, modified to include the higher RS in hatchery Auke sockeye:
  

rrs_ci_kalinowski_auke <- function(n_h_off, n_w_off, n_h_par, n_w_par, alpha){
  chi_alpha <- qchisq(p = (1 - alpha), df = 1)
  n_off <- sum(c(n_h_off, n_w_off))
  n_par <- sum(c(n_h_par, n_w_par))
  
  rs_h <- n_h_off / n_h_par
  rs_w <- n_w_off / n_w_par
  
  p_h_par <- n_h_par / n_par
  p_w_par <- n_w_par / n_par
  
  rrs_h <- rs_h / rs_w
  rrs_w <- rs_w / rs_w
  rrs_avg <- (rrs_h * p_h_par) + (rrs_w * p_w_par)
  
  rrs_ml <- (n_h_off * log(p_h_par * rrs_h / rrs_avg)) + (n_w_off * log(p_w_par * rrs_w / rrs_avg))
  
  xi_dist <- bind_rows(
    lapply(seq(0.01, 200, by = 0.01), function(rrs_h_xi) { #modification: upper RRS up to 200
      rrs_avg_xi <- (rrs_h_xi * p_h_par) + (rrs_w * p_w_par)
      tibble(rrs_crit = rrs_h_xi,
             logl = (n_h_off * log(p_h_par * rrs_h_xi / rrs_avg_xi)) + (n_w_off * log(p_w_par * rrs_w / rrs_avg_xi)) - (rrs_ml - chi_alpha / 2)
      )
    } )
  )
  
  rrs_min <- xi_dist %>% 
    mutate(abs_logl = abs(logl)) %>% 
    filter(rrs_crit < rrs_h) %>% 
    top_n(-1, abs_logl) %>% 
    pull(rrs_crit)
  
  rrs_max <- xi_dist %>% 
    mutate(abs_logl = abs(logl)) %>% 
    filter(rrs_crit > rrs_h) %>% 
    top_n(-1, abs_logl) %>% 
    pull(rrs_crit)
  
  return(c(rrs_min, rrs_h, rrs_max))
}



lod45_RS %>% filter (Psex == "female") %>% group_by(by, Ptype) %>% tally() # number of spawners

lod45_RS %>% filter (Psex == "female") %>% group_by(by, Ptype) %>% summarise (total_off = sum(n_offspring))


#Calculate RRS and CI

rrs_ci_kalinowski_auke (n_h_off = 504, n_w_off = 3653, n_h_par = 29, n_w_par = 1263, alpha = 0.05) #2011

rrs_ci_kalinowski_auke (n_h_off = 701, n_w_off = 534, n_h_par = 22, n_w_par = 815, alpha = 0.05) #2012

rrs_ci_kalinowski_auke (n_h_off = 286, n_w_off = 770, n_h_par = 27, n_w_par = 1006, alpha = 0.05) #2013


#Percentage of females that had zero offspring

lod45_RS <- lod45_RS %>% mutate (successful = case_when (
  n_offspring == 0 ~ "no",
  n_offspring > 0 ~ "yes"
))

lod45_RS %>% filter (Psex == "female") %>% group_by(Psex, Ptype, by) %>% count (successful)



#females
100*(3/29) #11 h
100*(1/22) #12 h
100*(4/27) #13 h

100*(449/(449+814)) #11 w
100*(527/(527+288)) #12 w
100*(668/(668+338)) #13 w

lod45_RS %>% filter (Psex == "male") %>% group_by(Psex, Ptype, by) %>% count (successful)



#males
#11 h is 0%
# 12 h is 0%
100*(1/14) #13 h

100*(382/(382+706)) #11 w
100*(375/(375+237)) #12 w
100*(662/(662+320)) #13 w


# Supplementary Table S4 - what is the overall productivity (adult offspring/parent) with three different LOD cut-offs? (no threshold, LOD ≥ 4.5, LOD ≥9)

all.potential.parents$broodyear <- as.numeric (substr (all.potential.parents$franzID, 1, 2)) + 2000

all.potential.parents <- all.potential.parents %>% mutate (sp_type = case_when (
  substr(franzID, 3, 3) == "h" ~ "hatchery",
  TRUE ~ "wild"
))
potent.temp <- all.potential.parents %>% filter (!franzID %in% c("13hm263944", "13hm263946", "13hm263947", "13hm263948", "13hm263949", "13hm263950" , "13hm263951", "13hm263952", "13hm263953"))

table (potent.temp$broodyear, potent.temp$sp_type)


#how many assignments when no threshold is used?

lod.none <- parentage.all %>% select (c(Offspring, broodyear, Parent.1, Parent.2)) %>% filter (broodyear > 2010 & broodyear < 2014) %>%
  mutate (ptype = case_when(
    substr (Parent.1, 3,3) == "h" | substr (Parent.2, 3, 3) == "h" ~ "hatchery",
    substr (Parent.1, 3,3) == "w" | substr (Parent.2, 3, 3) == "w" ~ "wild",
    substr (Parent.1, 3,3) == "r" | substr (Parent.2, 3, 3) == "w" ~ "wild",
    substr (Parent.1, 3,3) == "w" | substr (Parent.2, 3, 3) == "r" ~ "wild",
    substr (Parent.1, 3,3) == "r" | substr (Parent.2, 3, 3) == "r" ~ "wild",
  ))

table (lod.none$broodyear, lod.none$ptype)


#productivity for no threshold:

574/40
723/31
292/41

3824/2351
583/1427
867/1985


#LOD ≥ 4.5

lod.45 <- parentage.all %>% select (c(Offspring, broodyear, Parent.1, Parent.2, Pair.LOD.Parent.1, Pair.LOD.Parent.2)) %>% filter (broodyear > 2010 & broodyear < 2014) %>%
  mutate (Parent.1 = case_when(
    Pair.LOD.Parent.1 >= 4.5 ~ Parent.1
  )) %>%
  mutate (Parent.2 = case_when(
    Pair.LOD.Parent.2 >= 4.5 ~ Parent.2
  )) %>%
  mutate (ptype = case_when(
    substr (Parent.1, 3,3) == "h" | substr (Parent.2, 3, 3) == "h" ~ "hatchery",
    substr (Parent.1, 3,3) == "w" | substr (Parent.2, 3, 3) == "w" ~ "wild",
    substr (Parent.1, 3,3) == "r" | substr (Parent.2, 3, 3) == "w" ~ "wild",
    substr (Parent.1, 3,3) == "w" | substr (Parent.2, 3, 3) == "r" ~ "wild",
    substr (Parent.1, 3,3) == "r" | substr (Parent.2, 3, 3) == "r" ~ "wild",
  ))

table (lod.45$broodyear, lod.45$ptype)

nrow (parentage.all %>% filter (Pair.LOD.Parent.1 >= 4.5 | Pair.LOD.Parent.2 >= 4.5)) #15,099 but this includes all years; for 14-19 it's 12,874
nrow (parentage.all %>% filter (Pair.LOD.Parent.1 >= 4.5 | Pair.LOD.Parent.2 >= 4.5) %>% filter (broodyear > 2010 & broodyear < 2014)) #6,800

assign45 <- parentage.all %>% filter (Pair.LOD.Parent.1 >= 4.5 | Pair.LOD.Parent.2 >= 4.5)
assign45$dup <- duplicated(assign45$Offspring)
table (assign45$dup)

assign45$offspringyear <- as.integer(substr(assign45$Offspring, 1, 2))
table (assign45$offspringyear)




#productivity for LOD ≥ 4.5

574/40
722/31
291/41

3806/2351
572/1427
835/1985


#LOD ≥ 9

lod.9 <- parentage.all %>% select (c(Offspring, broodyear, Parent.1, Parent.2, Pair.LOD.Parent.1, Pair.LOD.Parent.2)) %>% filter (broodyear > 2010 & broodyear < 2014) %>%
  mutate (Parent.1 = case_when(
    Pair.LOD.Parent.1 >= 9 ~ Parent.1
  )) %>%
  mutate (Parent.2 = case_when(
    Pair.LOD.Parent.2 >= 9 ~ Parent.2
  )) %>%
  mutate (ptype = case_when(
    substr (Parent.1, 3,3) == "h" | substr (Parent.2, 3, 3) == "h" ~ "hatchery",
    substr (Parent.1, 3,3) == "w" | substr (Parent.2, 3, 3) == "w" ~ "wild",
    substr (Parent.1, 3,3) == "r" | substr (Parent.2, 3, 3) == "w" ~ "wild",
    substr (Parent.1, 3,3) == "w" | substr (Parent.2, 3, 3) == "r" ~ "wild",
    substr (Parent.1, 3,3) == "r" | substr (Parent.2, 3, 3) == "r" ~ "wild",
  ))

table (lod.9$broodyear, lod.9$ptype)

nrow (parentage.all %>% filter (Pair.LOD.Parent.1 >= 9 | Pair.LOD.Parent.2 >= 9) %>% filter (broodyear > 2010 & broodyear < 2014))
#6,441


#productivity for LOD ≥ 9

554/40
712/31
279/41

3643/2351
528/1427
725/1985


#What would have happened in brood years 2012 and 2013 had there been no supplementation?
#What is the distribution of RS values for females in 2012 and 2013?

w.rs.12 <- lod45_RS %>% filter (Ptype == "wild", Psex == "female", by == 2012)
w.rs.13 <- lod45_RS %>% filter (Ptype == "wild", Psex == "female", by == 2013)

mean (w.rs.12$n_offspring)
mean (w.rs.13$n_offspring)

mean (w.rs.12$n_offspring) * 30
mean (w.rs.13$n_offspring) * 30

#Genotyping rates for all potential parents and offspring, regardless of assigned brood year

all.genos <- g13 %>% mutate (broodyear = 2000 + as.numeric (substr (franzID, 1, 2))) #26,776
nrow (all.genos %>% filter (broodyear < 2017)) #21,122
nrow (all.genos %>% filter (broodyear > 2013)) #13,361

sufficient.genotypes <- as.data.frame (merge (sufficient.str, sufficient.snp, by = "franzID"))
sufficient.genotypes$n_tot <- sufficient.genotypes$n_snp + sufficient.genotypes$n_str
sufficient.genotypes <- sufficient.genotypes %>% filter (n_tot >= 40) #25,867
sufficient.genotypes$broodyear <- (2000 + as.numeric (substr (sufficient.genotypes$franzID, 1, 2)))

nrow (sufficient.genotypes %>% filter (broodyear < 2017)) #20,349
nrow (sufficient.genotypes %>% filter (broodyear > 2013)) #13,102


#Next: analysis of phenotypes - see "scripts/SAUKE_F1_05_Phenotypes.R"

