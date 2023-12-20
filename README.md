### Project: Hatchery supplementation provides a demographic boost but alters age composition of sockeye salmon in 
Auke Lake, Southeast Alaska"
### Author: Megan McPhee, University of Alaska Fairbanks
### Date: 2021-2023

This repo contains datafiles and scripts used in the analyses behind the manuscript "Hatchery supplementation 
provides a demographic boost but alters age composition of sockeye salmon in Auke Lake, Southeast Alaska",
currently in revision at Evolutionary Applications (as of 12/18/2023)

Datasets:
Genotypes: "data/SAUKE_Genotypes_V13.csv" [DOI TK]
Demographic information: "SAUKE_DemographicInformation_V12.csv" [DOI TK]
Parentage:"parentage.csv"
   parent assignment was conducted in FRANz as detailed in manuscript; 
   one would save in a file called "parentage.csv" which retains the same columns as the parentage output from 
FRANz

Analysis workflow

1. Analyze statistical power of marker set and determine LOD threshold that optimizes trade off between false 
positive/false negative assignment rates ("scripts/SAUKE_F1_Rscript_01_CKMRsim.R")

2. Data wrangling of parentage results from FRANz, calculate number of offspring for each parent 
("scripts/SAUKE_F1_Rscript_02_ParentageWrangling-and-ReproductiveSuccess.R"")
      Includes steps to reconcile "same sex" triads (due to errors in field ID of sex): 
"scripts/SAUKE_F1_Rscript_03_Samesex_pairings.R"
      
3. Calculate relative reproductive success (RRS) and 95% confidence intervals  
("scripts/SAUKE_F1_Rscript_04_RRS.R")

4. Comparing phenotypes by parental type, including comparing phenotypes of those offspring who had parents 
returning only on the dates that broodstock 
   were collected ("scripts/SAUKE_F1_Rscript_05_Phenotypes.R)
   
5. Make figures: "scripts/SAUKE_F1_MainFigures.R" and "scripts/SAUKE_F1_SupplementaryFigures.R"



The code contained in these scripts was developed under many different versions of R and R packages but it has 
been tested under the following
conditions:
 
R version 4.3.1 (2023-06-16)
Platform: x86_64-apple-darwin20 (64-bit)
Running under: macOS Ventura 13.6.1

Matrix products: default
BLAS:   
/System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Juneau
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggforce_0.4.1   patchwork_1.1.3 cowplot_1.1.1   MuMIn_1.47.5    merTools_0.6.1  arm_1.13-1      MASS_7.3-60     
lme4_1.1-35.1  
 [9] Matrix_1.5-4.1  lubridate_1.9.3 forcats_1.0.0   stringr_1.5.0   dplyr_1.1.3     purrr_1.0.2     readr_2.1.4     
tidyr_1.3.0    
[17] tibble_3.2.1    ggplot2_3.4.3   tidyverse_2.0.0

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.0    farver_2.1.1        fastmap_1.1.1       tweenr_2.0.2        promises_1.2.1      
digest_0.6.33       timechange_0.2.0   
 [8] mime_0.12           lifecycle_1.0.3     ellipsis_0.3.2      magrittr_2.0.3      compiler_4.3.1      
rlang_1.1.1         tools_4.3.1        
[15] utf8_1.2.3          yaml_2.3.7          knitr_1.44          labeling_0.4.3      abind_1.4-5         
withr_2.5.0         grid_4.3.1         
[22] polyclip_1.10-6     stats4_4.3.1        fansi_1.0.4         xtable_1.8-4        colorspace_2.1-0    
future_1.33.0       globals_0.16.2     
[29] scales_1.2.1        iterators_1.0.14    cli_3.6.1           mvtnorm_1.2-4       rmarkdown_2.25      
ragg_1.2.5          generics_0.1.3     
[36] rstudioapi_0.15.0   tzdb_0.4.0          minqa_1.2.6         splines_4.3.1       parallel_4.3.1      
vctrs_0.6.3         boot_1.3-28.1      
[43] jsonlite_1.8.7      hms_1.1.3           listenv_0.9.0       systemfonts_1.0.4   foreach_1.5.2       
glue_1.6.2          blme_1.0-5         
[50] parallelly_1.36.0   nloptr_2.0.3        codetools_0.2-19    stringi_1.7.12      gtable_0.3.4        
later_1.3.2         broom.mixed_0.2.9.4
[57] munsell_0.5.0       furrr_0.3.1         pillar_1.9.0        htmltools_0.5.6     R6_2.5.1            
textshaping_0.3.6   evaluate_0.21      
[64] shiny_1.8.0         lattice_0.21-8      backports_1.4.1     broom_1.0.5         httpuv_1.6.13       
Rcpp_1.0.11         coda_0.19-4        
[71] nlme_3.1-162        xfun_0.40           pkgconfig_2.0.3    
> 

