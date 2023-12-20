### Code for "Hatchery supplementation provides a demographic boost but alters age composition of sockeye salmon in Auke Lake, Southeast Alaska"
### Author: Megan McPhee, University of Alaska Fairbanks
### Date: 2021-2023


##Analysis 1: power of marker suite
library (tidyverse)
library (ckmrsim)
library (cowplot)

###Data wrangling for CKMRsim

genotypes.all <- read.csv ("SAUKE_Genotypes_V13.csv", header = TRUE)
genotypes.all <- genotypes.all %>% dplyr::select (-c(SillyCodeID, One_GTHa, One_MHC2_190)) #remove ADFG fish ID and 2 loci linked to other loci
genotypes.all$franzID <- tolower (genotypes.all$franzID)

#two loci have "-" as an allele; replace with "Z"
#One_Ots208_234
#One_U1016_115
genotypes.all$One_Ots208_234 <- gsub('-','Z',genotypes.all$One_Ots208_234)
genotypes.all$One_U1016_115 <- gsub('-','Z',genotypes.all$One_U1016_115)

genotypes.all <- genotypes.all %>% mutate_all (funs(str_replace (., "/", "."))) #replace allele divider with period




#Two rounds of lengthening...

genosL <- genotypes.all %>%
  pivot_longer (cols = starts_with ("One"), names_to = "Locus", values_to = "genotype") %>%
  separate (genotype, into = c("Allele_1", "Allele_2")) %>%
  rename (Indiv = franzID)


genosL.2 <- genosL %>%
  pivot_longer (cols = starts_with ("All"), names_to = "which_allele", values_to = "Allele") %>%
  mutate (gene_copy = 
            if_else (which_allele == "Allele_1", 1, 2)) %>% 
  dplyr::select (-which_allele)

genosL.2$Allele <- as.character (genosL.2$Allele)
genosL.2$Allele [genosL.2$Allele==""] <- NA
genosL.2$Allele [genosL.2$Allele==" "] <- NA #not sure if this was required but in case there were spaces in there...



#Calculate allele frequencies...

alle_freqs <- genosL.2 %>%
  group_by (Locus, Allele) %>%
  tally() %>%
  filter (!is.na(Allele)) %>%
  mutate (Freq = n / sum(n)) %>%
  arrange (Locus, desc(Freq)) %>%
  dplyr::select (-n) %>%
  group_by (Locus) %>%
  filter (n()>1) %>%
  mutate (AlleIdx = 1:n()) %>%
  ungroup() %>%
  mutate (LocIdx = as.integer (factor (Locus, levels = unique(Locus))),
          Chrom = "Unk", 
          Pos = LocIdx
  ) %>%
  dplyr::select (Chrom, Pos, Locus, Allele, Freq, LocIdx, AlleIdx) %>%
  reindex_markers ()

###CKMR simulations

#Create CKMRsim object. We'll assume genotyping error rate of 0.005

ckmr_005 <- create_ckmr (
  D = alle_freqs,
  kappa_matrix = kappas[c("PO", "FS", "HS", "U"), ],
  ge_mod_assumed = ge_model_TGIE,
  ge_mod_true = ge_model_TGIE, 
  ge_mod_assumed_pars_list = list (epsilon = 0.005), 
  ge_mod_true_pars_list = list (epsilon = 0.005)
)

ckmr_005


#And run simulation...


ckmr_005_Qs <- simulate_Qij(ckmr_005,
                       calc_relats = c("PO", "FS", "U"),
                       sim_relats = c("PO", "FS", "HS", "U"), 
                       reps = 10^5)

ckmr_005_Qs


#Compare parent-offspring vs. unrelated:

PO_U_logls <- extract_logls (ckmr_005_Qs,
                             numer = c(PO = 1),
                             denom = c(U = 1))


#False negative and false positive rates vs. LOD

PO_is_005 <- mc_sample_simple(ckmr_005_Qs, nu = "PO", de = "U")
PO_is_005


#Find LOD threshold that optimizes trade-off between false negative and false positive errors (i.e., maximizes their product)

PO_is_005_seq <- mc_sample_simple(ckmr_005_Qs, 
                                        nu = "PO", 
                                        de = "U", 
                                        lambda_stars = seq(-2, 16, by = 0.1))


ggplot (data = PO_is_005_seq) + (aes (x = Lambda_star, y = (FNR*FPR))) + geom_point()



#Extract error rates for LOD threshold of 4.5

PO_005_is_4p5 <- mc_sample_simple(ckmr_005_Qs, 
                                        nu = "PO", 
                                        de = "U", 
                                        lambda_stars = 4.5)
PO_005_is_4p5


#Examine avuncular relationships

avunc_ckmr_005 <- create_ckmr (
  D = alle_freqs,
  kappa_matrix = kappas[c("PO", "AN", "U"), ],
  ge_mod_assumed = ge_model_TGIE,
  ge_mod_true = ge_model_TGIE, 
  ge_mod_assumed_pars_list = list (epsilon = 0.005), 
  ge_mod_true_pars_list = list (epsilon = 0.005)
)

avunc_ckmr_005


#And run simulation...


avunc_ckmr_005_Qs <- simulate_Qij(avunc_ckmr_005,
                       calc_relats = c("PO", "AN", "U"),
                       sim_relats = c("PO", "AN", "U"), 
                       reps = 10^5)

avunc_ckmr_005_Qs

#Compare parent-offspring vs. unrelated:

avunc_PO_U_logls <- extract_logls (avunc_ckmr_005_Qs,
                             numer = c(PO = 1),
                             denom = c(U = 1))





###CKMR simulations for SNPS only

#Remove STR loci from allele frequency file:

snp_allel_freq <- alle_freqs %>%
  filter (Locus != "One_Oki10") %>%
  filter (Locus != "One_Oki100") %>%
  filter (Locus != "One_Oki1a") %>%
  filter (Locus != "One_Oki1b")  %>% 
  filter (Locus != "One_One102") %>%
  filter (Locus != "One_One109") %>%
  filter (Locus != "One_One114") %>%
  filter (Locus != "One_One8") %>%
  filter (Locus != "One_Ssa419")


#Now make a new ckmr object:


snp_ckmr_005 <- create_ckmr (
  D = snp_allel_freq,
  kappa_matrix = kappas[c("PO", "FS", "HS", "U"), ],
  ge_mod_assumed = ge_model_TGIE,
  ge_mod_true = ge_model_TGIE, 
  ge_mod_assumed_pars_list = list (epsilon = 0.005), 
  ge_mod_true_pars_list = list (epsilon = 0.005)
)

snp_ckmr_005


#And run simulations...


snp_ckmr_005_Qs <- simulate_Qij(snp_ckmr_005,
                       calc_relats = c("PO", "FS", "U"),
                       sim_relats = c("PO", "FS", "HS", "U"), 
                       reps = 10^5)


#Comparing parent-offspring vs. unrelated:

snp_005_PO_U_logls <- extract_logls (snp_ckmr_005_Qs,
                             numer = c(PO = 1),
                             denom = c(U = 1))


###Make figures for manuscript - Supplementary Figure S2

#Base plots:

p1 <- ggplot (data = PO_U_logls %>% filter (true_relat %in% c("PO", "U"))) + aes (x = logl_ratio, fill = true_relat) + geom_density(alpha = 0.25) + xlab ("LOD") + ylab ("Density") + theme_cowplot() + theme (legend.title = element_blank()) + theme(legend.position = c(0.1, 0.85)) 

p2 <- ggplot (data = PO_is_005_seq) + (aes (x = Lambda_star, y = (FNR*FPR))) + geom_point(size = 1) + xlab ("LOD threshold") + ylab ("FNR x FPR") + theme_cowplot() 

p3 <- ggplot (data = snp_005_PO_U_logls %>% filter (true_relat %in% c("PO", "U"))) + aes (x = logl_ratio, fill = true_relat) + geom_density(alpha = 0.25) + xlab ("LOD") + ylab ("Density") + theme_cowplot() + theme (legend.title = element_blank()) + theme(legend.position = c(0.1, 0.85)) 

p4 <- ggplot (data = avunc_PO_U_logls) + aes (x = logl_ratio, fill = true_relat) + geom_density(alpha = 0.25) + xlab ("LOD") + ylab ("Density") + theme_cowplot() + scale_fill_manual (values = c ("#F5C710", "#DF536B", "#28E2E5")) + theme (legend.title = element_blank()) + theme(legend.position = c(0.1, 0.85)) 


#Combine into to a single plot

sf2.b1 <- plot_grid (p1, p2)
sf2.b2 <- plot_grid (p4, p3, nrow = 1)

supp.fig2 <- sf2.b1/sf2.b2

ggdraw(supp.fig2) + draw_label ("a)", x = 0.02, y = 0.98) + draw_label ("b)", x = 0.51, y = 0.98) + draw_label ("c)", x = 0.02, y = 0.51) + draw_label ("d)", x = 0.51, y = 0.51)


#save plot
dev.new (width = 180, height = 120, unit = "mm", noRStudioGD = T); last_plot()
ggsave ("Figures/Supplementary_FigS2.pdf", width = dev.size()[1], height = dev.size()[2], dpi = 600); dev.off()

