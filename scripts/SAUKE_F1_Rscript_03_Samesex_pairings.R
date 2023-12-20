### Code for "Hatchery supplementation provides a demographic boost but alters age composition of sockeye salmon in Auke Lake, Southeast Alaska"
### Author: Megan McPhee, University of Alaska Fairbanks
### Date: 2021-2023

#Parentage data wrangling and calculating individual reproductive success (n_offspring)
#This .rmd examines parentage data to determine if sex identification can be corrected based on parsimony.
#This takes the df 'parentage45' that was created in "scripts/SAUKE_F1_Rscript02_ParentageWrangling.R"
#If can't find a parsimonious solution remove the assignment with the lowest LOD.
#If monogamous saemsex pair, randomize:
#sample 1 or 2. If 1, change sex of parent 1. If 2, change sex of parent 2

#you must run the code in "scripts/SAUKE_F1_Rscript_02_ParentageWrangling-and-ReproductiveSuccess.R" first.
#during original analysis I forgot to set the seed so there might be some conflicts between what is generated with sample (1 or 2) and what changes are actually made

library (tidyverse)

table (parentage45$samesex) 

ss <- parentage45 %>% filter (samesex != "het") %>% select (c(parent1, parent2, samesex))

ss.long <- pivot_longer (ss, !samesex, names_to = "which_parent", values_to = "parentID")

ss.long$dup <- duplicated (ss.long$parentID)
table (ss.long$dup)


problem.parents <- ss.long %>% filter (dup == FALSE)
rm (ss, ss.long)

 #examine these to determine whether we can fix sex ID.
#Find all "problem parents" in parentage.all, then repeat the filtering to LOD ≥ 4.5 & BY = 11-13

#create new parent columns that will be updated when we need to change sex ID:
parentage.all$Parent1.updated <- parentage.all$Parent.1
parentage.all$Parent2.updated <- parentage.all$Parent.2

#determine whether sex ID should be changed using the "good" assignments only 
#with all available brood years

parentage45.allby <- parentage.all %>% mutate (parent1 = case_when(  
  Pair.LOD.Parent.1 >= 4.5 ~ Parent.1
)) %>% mutate (parent2 = case_when (
 Pair.LOD.Parent.2 >= 4.5 ~ Parent.2 
)) %>% select (c (Offspring, parent1, parent2, broodyear, Pair.LOD.Parent.1, Pair.LOD.Parent.2, Posterior.Parent.1, Posterior.Parent.2))

problem.parents[1,]

slice (parentage45.allby %>% filter (parent1 == "11wm010214" | parent2 == "11wm010214")) #row1

#11wm010214 to female
parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010214", "11wf010214")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010214", "11wf010214"))

problem.parents[2,]
slice (parentage45.allby %>% filter (parent1 == "11wj010176" | parent2 == "11wj010176"))

#row 2 #No change

problem.parents[3,]
slice (parentage45.allby %>% filter (parent1 == "11wm008663" | parent2 == "11wm008663"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008663", "11wf008663")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008663", "11wf008663"))

problem.parents[4,]
slice (parentage45.allby %>% filter (parent1 == "11wj009947" | parent2 == "11wj009947"))

##No change.


problem.parents[5,]
slice (parentage45.allby %>% filter (parent1 == "11wm010208" | parent2 == "11wm010208"))

slice (parentage45.allby %>% filter (parent1 == "11wm010214" | parent2 == "11wm010214"))


##No change to 11wm010208


problem.parents[6,]
slice (parentage45.allby %>% filter (parent1 == "11wf009755" | parent2 == "11wf009755"))

slice (parentage45.allby %>% filter (parent1 == "11wf008541" | parent2 == "11wf008541"))
slice (parentage45.allby %>% filter (parent1 == "11wm009483" | parent2 == "11wm009483"))

##No change to 11wf009755 but 11wf008541 should be changed to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf008541", "11wm008541")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf008541", "11wm008541"))

problem.parents[7,]
#we just fixed this one!

problem.parents[8,]
slice (parentage45.allby %>% filter (parent1 == "11wf009378" | parent2 == "11wf009378"))

slice (parentage45.allby %>% filter (parent1 == "11wf009051" | parent2 == "11wf009051"))


#9051 mated with 2 females and 1 male... 9051 should be a male

slice (parentage45.allby %>% filter (parent1 == "11wm010447" | parent2 == "11wm010447"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf009051", "11wm009051")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf009051", "11wm009051"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Offspring == "17wm000637", NA)) #fix conflict

problem.parents[9,]
#just fixed

problem.parents[10,]
slice (parentage45.allby %>% filter (parent1 == "11wf009822" | parent2 == "11wf009822"))

#fem mates: i
#male mates: iii
#No change to 11wf009822


slice (parentage45.allby %>% filter (parent1 == "11wf008741" | parent2 == "11wf008741"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "16wf000191", NA)) #fix conflict

problem.parents[11,]
slice (parentage45.allby %>% filter (parent1 == "11wf009822" | parent2 == "11wf009822"))
#just dealt with - #No change

problem.parents[12,]
slice (parentage45.allby %>% filter (parent1 == "11wm009713" | parent2 == "11wm009713"))
#just fixed - #No change

#2 females, 2 male mates
#11wm008661, 11wm010463
#11wf010276, 11wf009804


slice (parentage45.allby %>% filter (parent1 == "11wm008661" | parent2 == "11wm008661"))
slice (parentage45.allby %>% filter (parent1 == "11wm010463" | parent2 == "11wm010463"))
slice (parentage45.allby %>% filter (parent1 == "11wf010276" | parent2 == "11wf010276"))
slice (parentage45.allby %>% filter (parent1 == "11wf009804" | parent2 == "11wf009804"))

#Focal fish, 11wm009713, stays male
#Mates:
#11wm008661 changes to female
#11wm010463 changes to female
#11wf009804 stays female
#11wm010062 stays male
#11wf010276 stays female


slice (parentage45.allby %>% filter (parent1 == "11wm010062" | parent2 == "11wm010062"))
slice (parentage45.allby %>% filter (parent1 == "11wm010463" | parent2 == "11wm010463"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008661", "11wf008661")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008661", "11wf008661"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010463", "11wf010463")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010463", "11wm010463"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Offspring == "15wm001204", NA)) #fix conflict

problem.parents[13,]
#just fixed - #No change

problem.parents[14,]
slice (parentage45.allby %>% filter (parent1 == "11wf009870" | parent2 == "11wf009870"))
slice (parentage45.allby %>% filter (parent1 == "11wf008513" | parent2 == "11wf008513"))

#Focal fish doesn't change
#11wf008513 <- male


parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf008513", "11wm008513")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf008513", "11wm008513"))

problem.parents[15,]
#just fixed

problem.parents[16,]
slice (parentage45.allby %>% filter (parent1 == "11wm009636" | parent2 == "11wm009636"))
slice (parentage45.allby %>% filter (parent1 == "11wm008816" | parent2 == "11wm008816"))

#11wm008816	11wf009054
#11wm008816	11wf009890


slice (parentage45.allby %>% filter (parent1 == "11wf009054" | parent2 == "11wf009054"))
slice (parentage45.allby %>% filter (parent1 == "11wf009890" | parent2 == "11wf009890"))


#Focal fish 11wm009636 #No change
#11wm008816 <- female - leaves 2 conflicts to resolve


parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008816", "11wf008816")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008816", "11wf008816"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "16wf000972", NA)) #fix conflict

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "16wm002184", NA)) #fix conflict


problem.parents[17,]
#just fixed

problem.parents[18,]
slice (parentage45.allby %>% filter (parent1 == "11wf009375" | parent2 == "11wf009375"))

#11wf009375:
#  male mates: ii 
#female mates: i


slice (parentage45.allby %>% filter (parent1 == "11wf008706" | parent2 == "11wf008706"))
slice (parentage45.allby %>% filter (parent1 == "11wm008551" | parent2 == "11wm008551"))
slice (parentage45.allby %>% filter (parent1 == "11wm008980" | parent2 == "11wm008980"))

#No change to focal fish (11wf009375)
#11wf008706 <- male


parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf008706", "11wm008706")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf008706", "11wm008706"))

problem.parents[19,]
#just fixed

problem.parents[20,]
slice (parentage45.allby %>% filter (parent1 == "11wf009417" | parent2 == "11wf009417"))
slice (parentage45.allby %>% filter (parent1 == "11wf008640" | parent2 == "11wf008640"))

#Focal fish 11wf009417 - #No change
#11wf008640 to male


parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf008640", "11wm008640")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf008640", "11wm008640"))

problem.parents[21,]
#just fixed

problem.parents[22,]
slice (parentage45.allby %>% filter (parent1 == "11wm009245" | parent2 == "11wm009245"))


#11wm009245 stays unchanged.


problem.parents[23,]
slice (parentage45.allby %>% filter (parent1 == "11wf009300" | parent2 == "11wf009300"))
slice (parentage45.allby %>% filter (parent1 == "11wf009043" | parent2 == "11wf009043"))
slice (parentage45.allby %>% filter (parent1 == "11wm010419" | parent2 == "11wm010419"))


#9300 is OK (focal)
#11wf009043 changes to male


parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf009043", "11wm009043")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf009043", "11wm009043"))

problem.parents[24,]
#just fixed

problem.parents[25,]
slice (parentage45.allby %>% filter (parent1 == "11wm010464" | parent2 == "11wm010464"))
slice (parentage45.allby %>% filter (parent1 == "11wm008350" | parent2 == "11wm008350"))



#11wm010464 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010464", "11wf010464")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010464", "11wf010464"))

problem.parents[26,]
##No change

problem.parents[27,]
slice (parentage45.allby %>% filter (parent1 == "11wm010465" | parent2 == "11wm010465"))
slice (parentage45.allby %>% filter (parent1 == "11wm009263" | parent2 == "11wm009263"))
slice (parentage45.allby %>% filter (parent1 == "11wm009200" | parent2 == "11wm009200"))


#11wm010465 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010465", "11wf010465")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010465", "11wf010465"))

problem.parents[28,]
#just fixed

problem.parents[29,]
slice (parentage45.allby %>% filter (parent1 == "11wm009296" | parent2 == "11wm009296"))

#11wm009296 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009296", "11wf009296")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009296", "11wf009296"))

problem.parents[30,]
slice (parentage45.allby %>% filter (parent1 == "11wm009120" | parent2 == "11wm009120"))

#No change.

problem.parents[31,]
slice (parentage45.allby %>% filter (parent1 == "11wm008997" | parent2 == "11wm008997"))
slice (parentage45.allby %>% filter (parent1 == "11wm008450" | parent2 == "11wm008450"))


#8540
#male mates: ii
#female: iii

slice (parentage45.allby %>% filter (parent1 == "11wm008433" | parent2 == "11wm008433"))

#8433 mated w/ one female
#parsimony: 11wm008450 is male, meaning that 11wm008997 changes to female.
#Changing 11wm008997 causes no direct conflicts, 
#but conflicts arise from assuming that 8450 is male.
#11wm008433 is then a female
#11wf009878 should then be a male...? ok


slice (parentage45.allby %>% filter (parent1 == "11wf009878" | parent2 == "11wf009878"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008997", "11wf008997")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008997", "11wf008997"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008433", "11wf008433")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008433", "11wf008433"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf009878", "11wm009878")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf009878", "11wm009878"))

problem.parents[32,]
##No change

problem.parents[33,]
slice (parentage45.allby %>% filter (parent1 == "11wf009303" | parent2 == "11wf009303"))
slice (parentage45.allby %>% filter (parent1 == "11wf008576" | parent2 == "11wf008576"))
slice (parentage45.allby %>% filter (parent1 == "11wj009614" | parent2 == "11wj009614"))

#11wf008576 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf008576", "11wm008576")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf008576", "11wm008576"))

problem.parents[34,]
#just fixed

problem.parents[35,]
slice (parentage45.allby %>% filter (parent1 == "11wm009663" | parent2 == "11wm009663"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009663", "11wf009663")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009663", "11wf009663"))

problem.parents[36,]
slice (parentage45.allby %>% filter (parent1 == "11wm009238" | parent2 == "11wm009238"))

#No change

problem.parents[37,]
slice (parentage45.allby %>% filter (parent1 == "11wm010657" | parent2 == "11wm010657"))
slice (parentage45.allby %>% filter (parent1 == "11wm010143" | parent2 == "11wm010143"))


#11wm010143 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010143", "11wf010143")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010143", "11wf010143"))

problem.parents[38,]

problem.parents[39,]
slice (parentage45.allby %>% filter (parent1 == "11wm010211" | parent2 == "11wm010211"))
slice (parentage45.allby %>% filter (parent1 == "11wm009262" | parent2 == "11wm009262"))
slice (parentage45.allby %>% filter (parent1 == "11wf010115" | parent2 == "11wf010115"))

#11wm009262 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009262", "11wf009262")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009262", "11wf009262"))

problem.parents[40,]
#just fixed

problem.parents[41,]
#ok

problem.parents[42,]
slice (parentage45.allby %>% filter (parent1 == "11wm010462" | parent2 == "11wm010462"))
slice (parentage45.allby %>% filter (parent1 == "11wm009266" | parent2 == "11wm009266"))

#Monogamous pair.


sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010462", "11wf010462")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010462", "11wf010462"))

problem.parents[43,]
##No change

problem.parents[44,]
slice (parentage45.allby %>% filter (parent1 == "11wf009311" | parent2 == "11wf009311"))
slice (parentage45.allby %>% filter (parent1 == "11wm009266" | parent2 == "11wm009266"))

#11wf009311 mated with:
#  females: ii
#males: i

#BUT: 11wf008513 was changed to male


slice (parentage45.allby %>% filter (parent1 == "11wf010477" | parent2 == "11wf010477"))

#change 11wf010477 to male


parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010477", "11wm010477")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010477", "11wm010477"))

problem.parents[45,]
slice (parentage45.allby %>% filter (parent1 == "11wm009729" | parent2 == "11wm009729"))
slice (parentage45.allby %>% filter (parent1 == "11wm008385" | parent2 == "11wm008385"))

#monogamous

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009729", "11wf009729")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009729", "11wf009729"))

problem.parents[46,]
##No change

problem.parents[47,]
slice (parentage45.allby %>% filter (parent1 == "11wf009825" | parent2 == "11wf009825"))
slice (parentage45.allby %>% filter (parent1 == "11wf008489" | parent2 == "11wf008489"))

#no sex change;
#resolve the one conflict

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "16wf001515", NA)) #fix conflict

problem.parents[48,]
#ok

problem.parents[49,]
slice (parentage45.allby %>% filter (parent1 == "11wf010375" | parent2 == "11wf010375"))
slice (parentage45.allby %>% filter (parent1 == "11wf009307" | parent2 == "11wf009307"))

#11wf009307 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf009307", "11wm009307")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf009307", "11wm009307"))

problem.parents[50,]
#just fixed

problem.parents[51,]
slice (parentage45.allby %>% filter (parent1 == "11wf010466" | parent2 == "11wf010466"))
slice (parentage45.allby %>% filter (parent1 == "11wf009307" | parent2 == "11wf009307"))

#11wf010466 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010466", "11wm010466")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010466", "11wf010466"))

problem.parents[52,]
slice (parentage45.allby %>% filter (parent1 == "11wf009350" | parent2 == "11wf009350"))
slice (parentage45.allby %>% filter (parent1 == "11wf010466" | parent2 == "11wf010466"))

#No change.

problem.parents[53,]
slice (parentage45.allby %>% filter (parent1 == "11wm010441" | parent2 == "11wm010441"))
slice (parentage45.allby %>% filter (parent1 == "11wm009967" | parent2 == "11wm009967"))

#11wm010441 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010441", "11wf010441")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010441", "11wf010441"))

problem.parents[54,]
#ok

problem.parents[55,]
slice (parentage45.allby %>% filter (parent1 == "11wf009322" | parent2 == "11wf009322"))
slice (parentage45.allby %>% filter (parent1 == "11wf008603" | parent2 == "11wf008603"))
slice (parentage45.allby %>% filter (parent1 == "11wm008389" | parent2 == "11wm008389"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "16wf002015", NA)) #fix conflict


problem.parents[56,]

problem.parents[57,]
slice (parentage45.allby %>% filter (parent1 == "12wf012005" | parent2 == "12wf012005"))
slice (parentage45.allby %>% filter (parent1 == "12wf011559" | parent2 == "12wf011559"))

#12wf011559 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011559", "12wm011559")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011559", "12wm011559"))

problem.parents[58,]
#just fixed

problem.parents[59,]
slice (parentage45.allby %>% filter (parent1 == "11wm008784" | parent2 == "11wm008784"))
slice (parentage45.allby %>% filter (parent1 == "11wm010214" | parent2 == "11wm010214"))
slice (parentage45.allby %>% filter (parent1 == "11wf009873" | parent2 == "11wf009873"))

#11wm010214 was changed to female. #No change to 11wm008784 necessary.


problem.parents[60,]
#already fixed

problem.parents[61,]
slice (parentage45.allby %>% filter (parent1 == "11wf010478" | parent2 == "11wf010478"))
slice (parentage45.allby %>% filter (parent1 == "11wf009750" | parent2 == "11wf009750"))

#11wf009750 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf009750", "11wm009750")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf009750", "11wm009750"))

problem.parents[62,]
#fixed

problem.parents[63,]
slice (parentage45.allby %>% filter (parent1 == "11wm009243" | parent2 == "11wm009243"))
slice (parentage45.allby %>% filter (parent1 == "11wm008999" | parent2 == "11wm008999"))


#11wm008999 to female


parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008999", "11wf008999")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008999", "11wf008999"))

problem.parents[64,]
# fixed

problem.parents[65,]
slice (parentage45.allby %>% filter (parent1 == "11wm009595" | parent2 == "11wm009595"))
slice (parentage45.allby %>% filter (parent1 == "11wm008999" | parent2 == "11wm008999"))

#11wm010214 was changed to female.


problem.parents[66,]
slice (parentage45.allby %>% filter (parent1 == "11wm008867" | parent2 == "11wm008867"))
slice (parentage45.allby %>% filter (parent1 == "11wm008653" | parent2 == "11wm008653"))
#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008653", "11wf008653")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008653", "11wf008653"))

problem.parents[67,]
#just changed to female

problem.parents[68,]
slice (parentage45.allby %>% filter (parent1 == "12wf012081" | parent2 == "12wf012081"))
slice (parentage45.allby %>% filter (parent1 == "12wf012000" | parent2 == "12wf012000"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf012081", "12wm012081")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf012081", "12wm012081"))


#12wf012081 to male

problem.parents[69,]
#just fixed

problem.parents[70,]
slice (parentage45.allby %>% filter (parent1 == "11wm009016" | parent2 == "11wm009016"))
slice (parentage45.allby %>% filter (parent1 == "11wf008840" | parent2 == "11wf008840"))
slice (parentage45.allby %>% filter (parent1 == "11wm008483" | parent2 == "11wm008483"))

#11wm008483 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008483", "11wf008483")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008483", "11wf008483"))

problem.parents[71,]
#just fixed

problem.parents[72,]
#changed previously

problem.parents[73,]
slice (parentage45.allby %>% filter (parent1 == "11wf010314" | parent2 == "11wf010314"))
slice (parentage45.allby %>% filter (parent1 == "11wf009383" | parent2 == "11wf009383"))
slice (parentage45.allby %>% filter (parent1 == "11wm009704" | parent2 == "11wm009704"))

#11wf010314 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010314", "11wm010314")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010314", "11wm010314"))

problem.parents[74,]
##No change

problem.parents[75,]

#11wm010143 already changed to female


problem.parents[76,]
slice (parentage45.allby %>% filter (parent1 == "11wm010221" | parent2 == "11wm010221"))
slice (parentage45.allby %>% filter (parent1 == "11wm008798" | parent2 == "11wm008798"))

#11wm010221 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010221", "11wf010221")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010221", "11wf010221"))

problem.parents[77,]
##No change

problem.parents[78,]
slice (parentage45.allby %>% filter (parent1 == "11wm008804" | parent2 == "11wm008804"))
slice (parentage45.allby %>% filter (parent1 == "11wm008426" | parent2 == "11wm008426"))

#11wm008804 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008804", "11wf008804")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008804", "11wf008804"))

problem.parents[79,]
#ok

problem.parents[80,]
slice (parentage45.allby %>% filter (parent1 == "11wm008419" | parent2 == "11wm008419"))
slice (parentage45.allby %>% filter (parent1 == "11wm008406" | parent2 == "11wm008406"))
slice (parentage45.allby %>% filter (parent1 == "11wm008445" | parent2 == "11wm008445"))

#11wm008419 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008419", "11wf008419")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008419", "11wf008419"))

problem.parents[81,]
##No change

problem.parents[82,]
slice (parentage45.allby %>% filter (parent1 == "11wm010210" | parent2 == "11wm010210"))
slice (parentage45.allby %>% filter (parent1 == "11wm010214" | parent2 == "11wm010214"))
slice (parentage45.allby %>% filter (parent1 == "11wf009828" | parent2 == "11wf009828"))


#11wm010214 was changed to female already

problem.parents[83,]
slice (parentage45.allby %>% filter (parent1 == "11wm009673" | parent2 == "11wm009673"))
slice (parentage45.allby %>% filter (parent1 == "11wm008782" | parent2 == "11wm008782"))


#11wm008782 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008782", "11wf008782")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008782", "11wf008782"))

problem.parents[84,]
#just fixed

problem.parents[85,]
slice (parentage45.allby %>% filter (parent1 == "11wf010340" | parent2 == "11wf010340"))
slice (parentage45.allby %>% filter (parent1 == "11wm009250" | parent2 == "11wm009250"))
slice (parentage45.allby %>% filter (parent1 == "11wf009063" | parent2 == "11wf009063"))

#11wf009063 to male
parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf009063", "11wm009063")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf009063", "11wm009063"))

problem.parents[86,]
#just fixed

problem.parents[87,]
slice (parentage45.allby %>% filter (parent1 == "11wf010552" | parent2 == "11wf010552"))
slice (parentage45.allby %>% filter (parent1 == "11wf009038" | parent2 == "11wf009038"))
slice (parentage45.allby %>% filter (parent1 == "11wj008382" | parent2 == "11wj008382"))

#11wf010552 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010552", "11wm010552")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010552", "11wm010552"))

problem.parents[88,]
#ok

problem.parents[89,]
slice (parentage45.allby %>% filter (parent1 == "11wm010189" | parent2 == "11wm010189"))
slice (parentage45.allby %>% filter (parent1 == "11wm009998" | parent2 == "11wm009998"))

#11wm009998 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009998", "11wf009998")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009998", "11wf009998"))

problem.parents[90,]
#just fixed

problem.parents[91,]
slice (parentage45.allby %>% filter (parent1 == "11wm010654" | parent2 == "11wm010654"))
slice (parentage45.allby %>% filter (parent1 == "11wm010593" | parent2 == "11wm010593"))
slice (parentage45.allby %>% filter (parent1 == "11wf010699" | parent2 == "11wf010699"))
slice (parentage45.allby %>% filter (parent1 == "11wf010451" | parent2 == "11wf010451"))

#10699 appears to be a legit female, making 10564 legit male,
#so what about 10593?
#  10451 appears to be legit female.
#so, the original samesex cross we just need to choose which to drop.
#drop parent2.

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "16wm002429", NA)) #fix conflict

problem.parents[92,]
slice (parentage45.allby %>% filter (parent1 == "11wm010654" | parent2 == "11wm010654"))
##No change

problem.parents[93,]
slice (parentage45.allby %>% filter (parent1 == "12wf011967" | parent2 == "12wf011967"))
slice (parentage45.allby %>% filter (parent1 == "12wf011360" | parent2 == "12wf011360"))
slice (parentage45.allby %>% filter (parent1 == "12wf011422" | parent2 == "12wf011422"))

#12wf011360 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011360", "12wm011360")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011360", "12wm011360"))

problem.parents[94,]
#just fixed

problem.parents[95,]

#11wm008433 already changed to female

problem.parents[96,]
slice (parentage45.allby %>% filter (parent1 == "11wj009426" | parent2 == "11wj009426"))

#11wm009998 already changed to female, #No change to 11wj009426


problem.parents[97,]
slice (parentage45.allby %>% filter (parent1 == "11wf009075" | parent2 == "11wf009075"))
slice (parentage45.allby %>% filter (parent1 == "11wf008537" | parent2 == "11wf008537"))


#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf008537", "11wm008537")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf008537", "11wm008537"))

#11wf008537 to male

problem.parents[98,]
#just fixed

problem.parents[99,]
slice (parentage45.allby %>% filter (parent1 == "11wm010324" | parent2 == "11wm010324"))
slice (parentage45.allby %>% filter (parent1 == "11wm008978" | parent2 == "11wm008978"))


#11wm010324 legit male
#11wm008978 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008978", "11wf008978")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008978", "11wf008978"))

problem.parents[100,]
#just fixed

problem.parents[101,]
slice (parentage45.allby %>% filter (parent1 == "11wf009835" | parent2 == "11wf009835"))

#11wf009051 already changed to male

problem.parents[102,]
slice (parentage45.allby %>% filter (parent1 == "11wm009478" | parent2 == "11wm009478"))

#11wm009296 already changed to female

problem.parents[103,]
slice (parentage45.allby %>% filter (parent1 == "11wf010300" | parent2 == "11wf010300"))
slice (parentage45.allby %>% filter (parent1 == "11wf009546" | parent2 == "11wf009546"))

#11wf010466 was changed to male, so 11wf009546 is legit female, leaving 11wf010300 to be changed to male
#11wf010300 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010300", "11wm010300")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010300", "11wm010300"))

problem.parents[104,]
##No change

problem.parents[105,]
slice (parentage45.allby %>% filter (parent1 == "11wm009711" | parent2 == "11wm009711"))
slice (parentage45.allby %>% filter (parent1 == "11wm009240" | parent2 == "11wm009240"))

#11wm009240 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009240", "11wf009240")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009240", "11wf009240"))

problem.parents[106,]
#just fixed

problem.parents[107,]
slice (parentage45.allby %>% filter (parent1 == "12wf011371" | parent2 == "12wf011371"))

#12wf011371 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011371", "12wm011371")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011371", "12wm011371"))

problem.parents[108,]
slice (parentage45.allby %>% filter (parent1 == "12wf011106" | parent2 == "12wf011106"))

#No change

problem.parents[109,]
slice (parentage45.allby %>% filter (parent1 == "11wf009447" | parent2 == "11wf009447"))
slice (parentage45.allby %>% filter (parent1 == "11wf009359" | parent2 == "11wf009359"))

#11wf009359 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf009359", "11wm009359")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf009359", "11wm009359"))

problem.parents[110,]
#just fixed

problem.parents[111,]
slice (parentage45.allby %>% filter (parent1 == "11wm008569" | parent2 == "11wm008569"))


#11wm008433 was changed to female already

problem.parents[112,]
slice (parentage45.allby %>% filter (parent1 == "13wf262668" | parent2 == "13wf262668"))
slice (parentage45.allby %>% filter (parent1 == "13wf262637" | parent2 == "13wf262637"))

#13wf262637 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf262637", "13wm262637")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf262637", "13wm262637"))

problem.parents[113,]
#just fixed

problem.parents[114,]
slice (parentage45.allby %>% filter (parent1 == "12wf012530" | parent2 == "12wf012530"))
slice (parentage45.allby %>% filter (parent1 == "12wf011902" | parent2 == "12wf011902"))
slice (parentage45.allby %>% filter (parent1 == "12wm012332" | parent2 == "12wm012332"))
slice (parentage45.allby %>% filter (parent1 == "12wf011892" | parent2 == "12wf011892"))

#getting a bit convoluted. for now will just work on focal individual - drop lowest

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Offspring == "17wf001105", NA)) #fix conflict

problem.parents[115,] #12wf011902
parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Offspring == "18wm000249", NA)) #fix conflict

problem.parents[116,]
slice (parentage45.allby %>% filter (parent1 == "11wf010303" | parent2 == "11wf010303"))
slice (parentage45.allby %>% filter (parent1 == "11wf010286" | parent2 == "11wf010286"))
slice (parentage45.allby %>% filter (parent1 == "11wm009720" | parent2 == "11wm009720"))

#drop one parent

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "17wf001169", NA)) #fix conflict

problem.parents[117,]
#just fixed

problem.parents[118,]
slice (parentage45.allby %>% filter (parent1 == "11wf010701" | parent2 == "11wf010701"))
slice (parentage45.allby %>% filter (parent1 == "11wf009576" | parent2 == "11wf009576"))
slice (parentage45.allby %>% filter (parent1 == "11wm008962" | parent2 == "11wm008962"))

#11wf010701 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010701", "11wm010701")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010701", "11wm010701"))

problem.parents[119,]
##No change

problem.parents[120,]
slice (parentage45.allby %>% filter (parent1 == "11wf010405" | parent2 == "11wf010405"))
slice (parentage45.allby %>% filter (parent1 == "11wf009860" | parent2 == "11wf009860"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "17wf001475", NA)) #fix conflict

problem.parents[121,]
##No change

problem.parents[122,]
slice (parentage45.allby %>% filter (parent1 == "12wf011319" | parent2 == "12wf011319"))

#12wf011371 changed to male already

problem.parents[123,]
slice (parentage45.allby %>% filter (parent1 == "11wm009020" | parent2 == "11wm009020"))
slice (parentage45.allby %>% filter (parent1 == "11wm008637" | parent2 == "11wm008637"))

#11wm009020 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009020", "11wf009020")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009020", "11wf009020"))

problem.parents[124,]
##No change

problem.parents[125,]
slice (parentage45.allby %>% filter (parent1 == "11wf009935" | parent2 == "11wf009935"))

#11wf009043 already changed to male

problem.parents[126,]
slice (parentage45.allby %>% filter (parent1 == "12wf011422" | parent2 == "12wf011422"))

#12wf011422 already changed to male

problem.parents[127,]
slice (parentage45.allby %>% filter (parent1 == "11wf010310" | parent2 == "11wf010310"))
slice (parentage45.allby %>% filter (parent1 == "11wf010110" | parent2 == "11wf010110"))

#11wf010110 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010110", "11wm010110")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010110", "11wm010110"))

problem.parents[128,]
#just fixed

problem.parents[129,]
slice (parentage45.allby %>% filter (parent1 == "11wm008546" | parent2 == "11wm008546"))
slice (parentage45.allby %>% filter (parent1 == "11wj008647" | parent2 == "11wj008647"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "17wf002212", NA)) #fix conflict

problem.parents[130,]
##No change

problem.parents[131,]
slice (parentage45.allby %>% filter (parent1 == "11wf010535" | parent2 == "11wf010535"))
slice (parentage45.allby %>% filter (parent1 == "11wf009809" | parent2 == "11wf009809"))

#11wf010535 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010535", "11wm010535")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010535", "11wm010535"))

problem.parents[132,]
##No change

problem.parents[133,]
slice (parentage45.allby %>% filter (parent1 == "12wf012179" | parent2 == "12wf012179"))
slice (parentage45.allby %>% filter (parent1 == "11wf009809" | parent2 == "11wf009809"))

#12wf011559 already changed to male

problem.parents[134,]
slice (parentage45.allby %>% filter (parent1 == "11wm008880" | parent2 == "11wm008880"))
slice (parentage45.allby %>% filter (parent1 == "11wm009663" | parent2 == "11wm009663"))

#11wm009663 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009663", "11wf009663")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009663", "11wf009663"))

problem.parents[135,]
slice (parentage45.allby %>% filter (parent1 == "12wf012020" | parent2 == "12wf012020"))
slice (parentage45.allby %>% filter (parent1 == "12wf011901" | parent2 == "12wf011901"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf012020", "12wm012020")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf012020", "12wm012020"))

problem.parents[136,]
##No change

problem.parents[137,]
slice (parentage45.allby %>% filter (parent1 == "12wf011444" | parent2 == "12wf011444"))
slice (parentage45.allby %>% filter (parent1 == "12wf011403" | parent2 == "12wf011403"))

#12wf011403 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011403", "12wm011403")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011403", "12wm011403"))

problem.parents[138,]
#just fixed

problem.parents[139,]
slice (parentage45.allby %>% filter (parent1 == "11wf009571" | parent2 == "11wf009571"))
slice (parentage45.allby %>% filter (parent1 == "12wf011403" | parent2 == "12wf011403"))

#11wf008576 already changed to male

problem.parents[140,]
slice (parentage45.allby %>% filter (parent1 == "11wm009652" | parent2 == "11wm009652"))
slice (parentage45.allby %>% filter (parent1 == "11wj010179" | parent2 == "11wj010179"))

#11wm009652 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009652", "11wf009652")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009652", "11wf009652"))

problem.parents[141,]
##No change

problem.parents[142,]
slice (parentage45.allby %>% filter (parent1 == "11wm009200" | parent2 == "11wm009200"))
##No change

problem.parents[143,]
slice (parentage45.allby %>% filter (parent1 == "11wf009605" | parent2 == "11wf009605"))
slice (parentage45.allby %>% filter (parent1 == "11wf009568" | parent2 == "11wf009568"))

#11wf009568 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf009568", "11wm009568")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf009568", "11wm009568"))

problem.parents[144,]
#just fixed

problem.parents[145,]
slice (parentage45.allby %>% filter (parent1 == "11wm009677" | parent2 == "11wm009677"))
slice (parentage45.allby %>% filter (parent1 == "11wm008663" | parent2 == "11wm008663"))
slice (parentage45.allby %>% filter (parent1 == "11wm010650" | parent2 == "11wm010650"))

#11wm008663 - already changed to female
#11wm010650 - change to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010650", "11wf010650")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010650", "11wf010650"))

problem.parents[146,]
slice (parentage45.allby %>% filter (parent1 == "11wf010309" | parent2 == "11wf010309"))
slice (parentage45.allby %>% filter (parent1 == "11wf010018" | parent2 == "11wf010018"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "17wf003207", NA)) #fix conflict

problem.parents[147,]
##No change

problem.parents[148,]
slice (parentage45.allby %>% filter (parent1 == "11wf009900" | parent2 == "11wf009900"))
slice (parentage45.allby %>% filter (parent1 == "11wf009857" | parent2 == "11wf009857"))
slice (parentage45.allby %>% filter (parent1 == "11wm008350" | parent2 == "11wm008350"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "17wf003373", NA)) #fix conflict

problem.parents[149,]
##No change

problem.parents[150,]
slice (parentage45.allby %>% filter (parent1 == "11wm009170" | parent2 == "11wm009170"))

#11wm008816 already changed to female

problem.parents[151,]
slice (parentage45.allby %>% filter (parent1 == "11wm009170" | parent2 == "11wm009170"))

#11wm010650 already changed to female


problem.parents[152,]
slice (parentage45.allby %>% filter (parent1 == "11wf009317" | parent2 == "11wf009317"))
slice (parentage45.allby %>% filter (parent1 == "11wf008580" | parent2 == "11wf008580"))
slice (parentage45.allby %>% filter (parent1 == "11wf008828" | parent2 == "11wf008828"))

slice (parentage45.allby %>% filter (parent1 == "11wm009245" | parent2 == "11wm009245"))
slice (parentage45.allby %>% filter (parent1 == "11wm009735" | parent2 == "11wm009735"))

#9317 with 2 males
#2 females
#Drop lower of parents in the 2 same-sex crosses.

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "17wf003479", NA)) #fix conflict

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Offspring == "15wm002420", NA))

problem.parents[153:154,]
slice (parentage45.allby %>% filter (parent1 == "11wf009141" | parent2 == "11wf009141"))
slice (parentage45.allby %>% filter (parent1 == "11wf008700" | parent2 == "11wf008700"))

#11wf008700 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf008700", "11wm008700")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf008700", "11wm008700"))

problem.parents[155,]
#just fixed

problem.parents[156,]
slice (parentage45.allby %>% filter (parent1 == "13wm261976" | parent2 == "13wm261976"))

#13wm261976 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm261976", "13wf261976")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm261976", "13wf261976"))

problem.parents[157,]
slice (parentage45.allby %>% filter (parent1 == "13wj262699" | parent2 == "13wj262699"))

#13wm261976 already changed to female

problem.parents[158,]
slice (parentage45.allby %>% filter (parent1 == "12wf011893" | parent2 == "12wf011893"))
#12wf011559 already changed to female

problem.parents[159,]
slice (parentage45.allby %>% filter (parent1 == "12wf011905" | parent2 == "12wf011905"))
slice (parentage45.allby %>% filter (parent1 == "12wf011333" | parent2 == "12wf011333"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011905", "12wm011905")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011905", "12wm011905"))

problem.parents[160,]
# #No change

problem.parents[161,]
slice (parentage45.allby %>% filter (parent1 == "11wm009125" | parent2 == "11wm009125"))
slice (parentage45.allby %>% filter (parent1 == "11wm008398" | parent2 == "11wm008398"))
slice (parentage45.allby %>% filter (parent1 == "11wf009032" | parent2 == "11wf009032"))


#11wm008398 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm008398", "11wf008398")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm008398", "11wf008398"))

problem.parents[162,]
#just fixed

problem.parents[163,]
slice (parentage45.allby %>% filter (parent1 == "12wm011854" | parent2 == "12wm011854"))
slice (parentage45.allby %>% filter (parent1 == "12wm011287" | parent2 == "12wm011287"))
slice (parentage45.allby %>% filter (parent1 == "12wf011179" | parent2 == "12wf011179"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "17wm000240", NA)) #fix conflict

problem.parents[164,]
# #No change

#11wf009568 was already changed to male

problem.parents[165,]
slice (parentage45.allby %>% filter (parent1 == "11wf010136" | parent2 == "11wf010136"))
slice (parentage45.allby %>% filter (parent1 == "11wf008836" | parent2 == "11wf008836"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "17wm000515", NA)) #fix conflict

problem.parents[166,]
##No change

problem.parents[167,]
slice (parentage45.allby %>% filter (parent1 == "11wm008445" | parent2 == "11wm008445"))

#11wm008419 already changed to female

problem.parents[168,]
slice (parentage45.allby %>% filter (parent1 == "11wm009995" | parent2 == "11wm009995"))
slice (parentage45.allby %>% filter (parent1 == "11wm008930" | parent2 == "11wm008930"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Offspring == "17wm000919", NA)) #fix conflict

problem.parents[169,]
##No change

problem.parents[170,]
slice (parentage45.allby %>% filter (parent1 == "12wf011330" | parent2 == "12wf011330"))
slice (parentage45.allby %>% filter (parent1 == "12wf011042" | parent2 == "12wf011042"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011330", "12wm011330")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011330", "12wm011330"))

problem.parents[171,]
#No change

problem.parents[172,]
slice (parentage45.allby %>% filter (parent1 == "12wf011436" | parent2 == "12wf011436"))
slice (parentage45.allby %>% filter (parent1 == "12wf011346" | parent2 == "12wf011346"))

#12wf011346 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011346", "12wm011346")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011346", "12wm011346"))

problem.parents[173,]
#already fixed

problem.parents[174,]
slice (parentage45.allby %>% filter (parent1 == "11wf010479" | parent2 == "11wf010479"))
slice (parentage45.allby %>% filter (parent1 == "12wf011346" | parent2 == "12wf011346"))

#11wf010276 stays female - from previous
#11wf010479 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010479", "11wm010479")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010479", "11wm010479"))

problem.parents[175,]
# no change

problem.parents[176,]
slice (parentage45.allby %>% filter (parent1 == "12wf011335" | parent2 == "12wf011335"))
slice (parentage45.allby %>% filter (parent1 == "12wf011346" | parent2 == "12wf011346"))

#12wf011346 already changed to male

problem.parents[177,]
slice (parentage45.allby %>% filter (parent1 == "12wf011970" | parent2 == "12wf011970"))
slice (parentage45.allby %>% filter (parent1 == "12wf011473" | parent2 == "12wf011473"))


#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011970", "12wm011970")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011970", "12wm011970"))

problem.parents[178,]
##No change

problem.parents[179,]
slice (parentage45.allby %>% filter (parent1 == "12wf012364" | parent2 == "12wf012364"))
slice (parentage45.allby %>% filter (parent1 == "12wf011538" | parent2 == "12wf011538"))

#12wf012364 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf012364", "12wm012364")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf012364", "12wm012364"))

problem.parents[180,]
##No change

problem.parents[181,]
slice (parentage45.allby %>% filter (parent1 == "12wf011986" | parent2 == "12wf011986"))

#12wf011371 already changed to male

problem.parents[182,]

#"11wm010062 stays male" - from previous

problem.parents[183,]
slice (parentage45.allby %>% filter (parent1 == "11wf010476" | parent2 == "11wf010476"))
slice (parentage45.allby %>% filter (parent1 == "11wf009168" | parent2 == "11wf009168"))

#11wf010476 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010476", "11wm010476")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010476", "11wm010476"))

problem.parents[184,]
##No change

problem.parents[185,]
slice (parentage45.allby %>% filter (parent1 == "12wf011399" | parent2 == "12wf011399"))
slice (parentage45.allby %>% filter (parent1 == "12wf011389" | parent2 == "12wf011389"))

#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011389", "12wm011389")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011389", "12wm011389"))

problem.parents[186,]
# just fixed

problem.parents[187,]
slice (parentage45.allby %>% filter (parent1 == "11wf009558" | parent2 == "11wf009558"))


#11wf009307 already changed to male


problem.parents[188,]
slice (parentage45.allby %>% filter (parent1 == "11wf009556" | parent2 == "11wf009556"))
slice (parentage45.allby %>% filter (parent1 == "11wf009553" | parent2 == "11wf009553"))

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Offspring == "17wm003317", NA)) #fix conflict

problem.parents[189,]
#No change

problem.parents[190,]
slice (parentage45.allby %>% filter (parent1 == "13wf263642" | parent2 == "13wf263642"))
slice (parentage45.allby %>% filter (parent1 == "13wf262721" | parent2 == "13wf262721"))

#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf262721", "13wm262721")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf262721", "13wm262721"))

problem.parents[191,]
#just fixed

problem.parents[192,]
slice (parentage45.allby %>% filter (parent1 == "12wf011478" | parent2 == "12wf011478"))
slice (parentage45.allby %>% filter (parent1 == "12wf011404" | parent2 == "12wf011404"))

#12wf011404 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011404", "12wm011404")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011404", "12wm011404"))

problem.parents[193,]
slice (parentage45.allby %>% filter (parent1 == "12wf011478" | parent2 == "12wf011478"))
slice (parentage45.allby %>% filter (parent1 == "12wf011404" | parent2 == "12wf011404"))

problem.parents[194,]
slice (parentage45.allby %>% filter (parent1 == "13wm263196" | parent2 == "13wm263196"))
slice (parentage45.allby %>% filter (parent1 == "13wm262845" | parent2 == "13wm262845"))

#Monogamous pair.
sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm262845", "13wf262845")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm262845", "13wf262845"))

problem.parents[195,]
#just fixed

problem.parents[196,]
slice (parentage45.allby %>% filter (parent1 == "13wm262086" | parent2 == "13wm262086"))

#13wm261976 already changed to female

problem.parents[197,]
slice (parentage45.allby %>% filter (parent1 == "12wf012089" | parent2 == "12wf012089"))
slice (parentage45.allby %>% filter (parent1 == "12wf011391" | parent2 == "12wf011391"))

#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011391", "12wm011391")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011391", "12wm011391"))

problem.parents[198,]
#just fixed

problem.parents[199,]
slice (parentage45.allby %>% filter (parent1 == "13wf263217" | parent2 == "13wf263217"))
slice (parentage45.allby %>% filter (parent1 == "13wf262715" | parent2 == "13wf262715"))

#13wf262715 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf262715", "13wm262715")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf262715", "13wm262715"))

problem.parents[200,]
#just fixed

problem.parents[201,]
slice (parentage45.allby %>% filter (parent1 == "13wf263031" | parent2 == "13wf263031"))
slice (parentage45.allby %>% filter (parent1 == "13wf262644" | parent2 == "13wf262644"))

#13wf262644 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf262644", "13wm262644")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf262644", "13wm262644"))

problem.parents[202,]
# just fixed

problem.parents[203,]
slice (parentage45.allby %>% filter (parent1 == "13wm263182" | parent2 == "13wm263182"))

#13wm263182 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm263182", "13wf263182")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm263182", "13wf263182"))

problem.parents[204,]
#No change

problem.parents[205,]
slice (parentage45.allby %>% filter (parent1 == "13wm263173" | parent2 == "13wm263173"))
slice (parentage45.allby %>% filter (parent1 == "13wm262710" | parent2 == "13wm262710"))

#13wm263173 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm263173", "13wf263173")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm263173", "13wf263173"))

problem.parents[206,]
#just fixed

problem.parents[207,]
slice (parentage45.allby %>% filter (parent1 == "13wf263235" | parent2 == "13wf263235"))
slice (parentage45.allby %>% filter (parent1 == "13wm262710" | parent2 == "13wm262710"))

#13wf263235 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf263235", "13wm263235")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf263235", "13wm263235"))

problem.parents[208,]
#No change

problem.parents[209,]
slice (parentage45.allby %>% filter (parent1 == "13wm262321" | parent2 == "13wm262321"))
slice (parentage45.allby %>% filter (parent1 == "13wm261955" | parent2 == "13wm261955"))
slice (parentage45.allby %>% filter (parent1 == "13wm262056" | parent2 == "13wm262056"))

slice (parentage45.allby %>% filter (parent1 == "13wf262732" | parent2 == "13wf262732"))

slice (parentage45.allby %>% filter (parent1 == "13wm262916" | parent2 == "13wm262916"))

#change 13wm261955 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm261955", "13wf261955")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm261955", "13wf261955"))

problem.parents[210,]
##No change

problem.parents[211,]
slice (parentage45.allby %>% filter (parent1 == "13wf262919" | parent2 == "13wf262919"))


#13wf262637 already changed to male

problem.parents[212,]
slice (parentage45.allby %>% filter (parent1 == "12wf011706" | parent2 == "12wf011706"))
slice (parentage45.allby %>% filter (parent1 == "12wf011378" | parent2 == "12wf011378"))

#12wf011378 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011378", "12wm011378")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011378", "12wm011378"))

problem.parents[213,]
#just fixed

problem.parents[214,]
slice (parentage45.allby %>% filter (parent1 == "13wm262445" | parent2 == "13wm262445"))
slice (parentage45.allby %>% filter (parent1 == "13wm262376" | parent2 == "13wm262376"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm262445", "13wf262445")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm262445", "13wf262445"))

problem.parents[215,]
#just fixed

problem.parents[216,]
slice (parentage45.allby %>% filter (parent1 == "13wm261942" | parent2 == "13wm261942"))

#13wm263182 already changed to female

problem.parents[217,]
slice (parentage45.allby %>% filter (parent1 == "12wf012032" | parent2 == "12wf012032"))
slice (parentage45.allby %>% filter (parent1 == "12wf011443" | parent2 == "12wf011443"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf012032", "12wm012032")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf012032", "12wm012032"))

problem.parents[218,]
##No change

problem.parents[219,]
slice (parentage45.allby %>% filter (parent1 == "13wf262650" | parent2 == "13wf262650"))

#13wf262637 already changed to male

problem.parents[220,]
slice (parentage45.allby %>% filter (parent1 == "13wm263320" | parent2 == "13wm263320"))
slice (parentage45.allby %>% filter (parent1 == "13wm262337" | parent2 == "13wm262337"))


#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm263320", "13wf263320")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm263320", "13wf263320"))

problem.parents[221,]
#No change

problem.parents[222,]
slice (parentage45.allby %>% filter (parent1 == "13wf263219" | parent2 == "13wf263219"))
slice (parentage45.allby %>% filter (parent1 == "13wf262279" | parent2 == "13wf262279"))

#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf262279", "13wm262279")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf262279", "13wm262279"))

problem.parents[223,]
#just fixed

problem.parents[224,]
slice (parentage45.allby %>% filter (parent1 == "13wm262916" | parent2 == "13wm262916"))
slice (parentage45.allby %>% filter (parent1 == "13wm262056" | parent2 == "13wm262056"))

#13wm262056 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm262056", "13wf262056")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm262056", "13wf262056"))

problem.parents[225,]

problem.parents[226,]
slice (parentage45.allby %>% filter (parent1 == "13wm262898" | parent2 == "13wm262898"))
slice (parentage45.allby %>% filter (parent1 == "13wm262314" | parent2 == "13wm262314"))

#13wm262314 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm262314", "13wf262314")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm262314", "13wf262314"))

problem.parents[227,]

problem.parents[228,]
slice (parentage45.allby %>% filter (parent1 == "12wf012338" | parent2 == "12wf012338"))
slice (parentage45.allby %>% filter (parent1 == "12wf011317" | parent2 == "12wf011317"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf012338", "12wm012338")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf012338", "12wm012338"))

problem.parents[229,]

problem.parents[230,]
slice (parentage45.allby %>% filter (parent1 == "13wm263330" | parent2 == "13wm263330"))
slice (parentage45.allby %>% filter (parent1 == "13wm262479" | parent2 == "13wm262479"))

#13wm263330 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm263330", "13wf263330")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm263330", "13wf263330"))


problem.parents[231,]
#No change

problem.parents[232,]
slice (parentage45.allby %>% filter (parent1 == "13wf263352" | parent2 == "13wf263352"))
slice (parentage45.allby %>% filter (parent1 == "13wf262539" | parent2 == "13wf262539"))
slice (parentage45.allby %>% filter (parent1 == "13wm262412" | parent2 == "13wm262412"))


#13wf262539 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf262539", "13wm262539")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf262539", "13wm262539"))

problem.parents[233,]

problem.parents[234,]
slice (parentage45.allby %>% filter (parent1 == "13wm263161" | parent2 == "13wm263161"))
slice (parentage45.allby %>% filter (parent1 == "13wm261969" | parent2 == "13wm261969"))
slice (parentage45.allby %>% filter (parent1 == "13wm262313" | parent2 == "13wm262313"))

#13wm263161 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm263161", "13wf263161")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm263161", "13wf263161"))

problem.parents[235,]

problem.parents[236,]
slice (parentage45.allby %>% filter (parent1 == "12wf011892" | parent2 == "12wf011892"))

#from previous code = already addressed this one
parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Offspring == "18wm000249", NA)) #fix conflict

problem.parents[237,]
slice (parentage45.allby %>% filter (parent1 == "13wm263185" | parent2 == "13wm263185"))
slice (parentage45.allby %>% filter (parent1 == "13wm262968" | parent2 == "13wm262968"))

#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm262968", "13wf262968")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm262968", "13wf262968"))

problem.parents[238,]

problem.parents[239,]
slice (parentage45.allby %>% filter (parent1 == "12wf011930" | parent2 == "12wf011930"))
slice (parentage45.allby %>% filter (parent1 == "12wf011401" | parent2 == "12wf011401"))

#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011401", "12wm011401")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011401", "12wm011401"))

problem.parents[240,]

problem.parents[241,]
slice (parentage45.allby %>% filter (parent1 == "12wf011507" | parent2 == "12wm011507"))
slice (parentage45.allby %>% filter (parent1 == "12wf011411" | parent2 == "12wf011411"))


#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011411", "12wm011411")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011411", "12wm011411"))

problem.parents[242,]

problem.parents[243,]
slice (parentage45.allby %>% filter (parent1 == "12wf011889" | parent2 == "12wf011889"))

#12wf011404 already changed to male

problem.parents[244,]
slice (parentage45.allby %>% filter (parent1 == "12wm012255" | parent2 == "12wm012255"))
slice (parentage45.allby %>% filter (parent1 == "12wm011268" | parent2 == "12wm011268"))


#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wm011268", "12wf011268")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wm011268", "12wf011268"))

problem.parents[245,]

problem.parents[246,]
slice (parentage45.allby %>% filter (parent1 == "13wf262593" | parent2 == "13wf262593"))


#13wf262637 already changed to male

problem.parents[247,]
slice (parentage45.allby %>% filter (parent1 == "12wf012521" | parent2 == "12wf012521"))
slice (parentage45.allby %>% filter (parent1 == "12wf012512" | parent2 == "12wf012512"))

#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf012512", "12wm012512")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf012512", "12wm012512"))

problem.parents[248,]

problem.parents[249,]
slice (parentage45.allby %>% filter (parent1 == "13wm262313" | parent2 == "13wm262313"))

#13wm263161 already changed to female


problem.parents[250,]
slice (parentage45.allby %>% filter (parent1 == "13wm262915" | parent2 == "13wm262915"))

#13wm261976 already changed to female

problem.parents[251,]
slice (parentage45.allby %>% filter (parent1 == "13wf262642" | parent2 == "13wf262642"))
slice (parentage45.allby %>% filter (parent1 == "13wf262622" | parent2 == "13wf262622"))

#13wf262642 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf262642", "13wm262642")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf262642", "13wm262642"))

problem.parents[252,]

problem.parents[253,]
slice (parentage45.allby %>% filter (parent1 == "13wf263393" | parent2 == "13wf263393"))
slice (parentage45.allby %>% filter (parent1 == "13wf262785" | parent2 == "13wf262785"))
#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf263393", "13wm263393")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf263393", "13wm263393"))

problem.parents[254,]

problem.parents[255,]
slice (parentage45.allby %>% filter (parent1 == "13wm263291" | parent2 == "13wm263291"))
slice (parentage45.allby %>% filter (parent1 == "13wm262060" | parent2 == "13wm262060"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wm263291", "13wf263291")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wm263291", "13wf263291"))

problem.parents[256,]

problem.parents[257,]
slice (parentage45.allby %>% filter (parent1 == "13wm262517" | parent2 == "13wm262517"))

#13wm261976 already changed to female

problem.parents[258,]
slice (parentage45.allby %>% filter (parent1 == "13wf263718" | parent2 == "13wf263718"))
slice (parentage45.allby %>% filter (parent1 == "13wf263375" | parent2 == "13wf263375"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf263718", "13wm263718")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf263718", "13wm263718"))

problem.parents[259,]

problem.parents[260,]
slice (parentage45.allby %>% filter (parent1 == "13wm263201" | parent2 == "13wm263201"))

#13wm263182 already changed to female

problem.parents[261,]
slice (parentage45.allby %>% filter (parent1 == "13wf263551" | parent2 == "13wf263551"))
slice (parentage45.allby %>% filter (parent1 == "13wf263275" | parent2 == "13wf263275"))

#13wf263551 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf263551", "13wm263551")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf263551", "13wm263551"))

problem.parents[262,]

problem.parents[263,]
slice (parentage45.allby %>% filter (parent1 == "13wf263136" | parent2 == "13wf263136"))
slice (parentage45.allby %>% filter (parent1 == "13wf261904" | parent2 == "13wf261904"))

#13wf261904 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf261904", "13wm261904")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf261904", "13wm261904"))

problem.parents[264,]

problem.parents[265,]
slice (parentage45.allby %>% filter (parent1 == "13wf263391" | parent2 == "13wf263391"))
slice (parentage45.allby %>% filter (parent1 == "13wf262946" | parent2 == "13wf262946"))

#Monogamous pair.
sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf263391", "13wm263391")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf263391", "13wm263391"))

problem.parents[266,]

problem.parents[267,]
slice (parentage45.allby %>% filter (parent1 == "13wf263223" | parent2 == "13wf263223"))
slice (parentage45.allby %>% filter (parent1 == "13wf262121" | parent2 == "13wf262121"))

#13wf262121 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "13wf262121", "13wm262121")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "13wf262121", "13wm262121"))

problem.parents[268,]

problem.parents[269,]
slice (parentage45.allby %>% filter (parent1 == "13wm263159" | parent2 == "13wm263159"))
slice (parentage45.allby %>% filter (parent1 == "13wj262705" | parent2 == "13wj262705"))

parentage.all <- parentage.all %>% 
  mutate (Parent2.updated = replace (Parent2.updated, Offspring == "19wm000030", NA)) #fix conflict

problem.parents[270,]

problem.parents[271,]
slice (parentage45.allby %>% filter (parent1 == "13wf263373" | parent2 == "13wf263373"))

#13wf263235 already changed to male

problem.parents[272,]
slice (parentage45.allby %>% filter (parent1 == "13wf262618" | parent2 == "13wf262618"))

#13wf262539 already changed to male

problem.parents[273,]
slice (parentage45.allby %>% filter (parent1 == "13wf263226" | parent2 == "13wf263226"))

#13wf262121 already changed to male

problem.parents[274,]
slice (parentage45.allby %>% filter (parent1 == "12wf011376" | parent2 == "12wf011376"))
slice (parentage45.allby %>% filter (parent1 == "12rf011584" | parent2 == "12rf011584"))

#12wf011376 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011376", "12wm011376")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011376", "12wm011376"))

problem.parents[275,]

problem.parents[276,]
slice (parentage45.allby %>% filter (parent1 == "12wf011152" | parent2 == "12wf011152"))
slice (parentage45.allby %>% filter (parent1 == "12wf011136" | parent2 == "12wf011136"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011136", "12wm011136")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011136", "12wm011136"))

problem.parents[277,]

problem.parents[278,]
slice (parentage45.allby %>% filter (parent1 == "12wf011140" | parent2 == "12wf011140"))
slice (parentage45.allby %>% filter (parent1 == "12wf011075" | parent2 == "12wf011075"))

#Monogamous pair.

sample (1:2, 1) #1

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf011140", "12wm011140")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf011140", "12wm011140"))

problem.parents[279,]

problem.parents[280,]
slice (parentage45.allby %>% filter (parent1 == "12wf012249" | parent2 == "12wf012249"))
slice (parentage45.allby %>% filter (parent1 == "12wf012546" | parent2 == "12wf012546"))

#Monogamous pair.

sample (1:2, 1) #2

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wf012546", "12wm012546")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wf012546", "12wm012546"))

problem.parents[281,]

problem.parents[282,]
slice (parentage45.allby %>% filter (parent1 == "11wf010159" | parent2 == "11wf010159"))
slice (parentage45.allby %>% filter (parent1 == "11wf008642" | parent2 == "11wf008642"))

#11wf010159 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010159", "11wm010159")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010159", "11wm010159"))

problem.parents[283,]

problem.parents[284,]
slice (parentage45.allby %>% filter (parent1 == "11wm010420" | parent2 == "11wm010420"))
slice (parentage45.allby %>% filter (parent1 == "11wm008869" | parent2 == "11wm008869"))

#11wm010420 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010420", "11wf010420")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010420", "11wf010420"))

problem.parents[285,]

problem.parents[286,]
slice (parentage45.allby %>% filter (parent1 == "11wm009979" | parent2 == "11wm009979"))
slice (parentage45.allby %>% filter (parent1 == "11wj008383" | parent2 == "11wj008383"))

#11wm009979 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm009979", "11wf009979")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm009979", "11wf009979"))

problem.parents[287,]

problem.parents[288,]
slice (parentage45.allby %>% filter (parent1 == "11wf010518" | parent2 == "11wf010518"))
slice (parentage45.allby %>% filter (parent1 == "11wf010042" | parent2 == "11wf010042"))

#11wf010518 to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wf010518", "11wm010518")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wf010518", "11wm010518"))

problem.parents[289,]

problem.parents[290,]
slice (parentage45.allby %>% filter (parent1 == "11wf008609" | parent2 == "11wf008609"))
slice (parentage45.allby %>% filter (parent1 == "11wf010276" | parent2 == "11wf010276"))

#11wf010479 already changed to male

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Offspring == "15wm001204", NA)) #fix conflict

problem.parents[291,]

slice (parentage45.allby %>% filter (parent1 == "11wm010192" | parent2 == "11wm010192"))
slice (parentage45.allby %>% filter (parent1 == "11wm008998" | parent2 == "11wm008998"))

#11wm010192 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "11wm010192", "11wf010192")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "11wm010192", "11wf010192"))

problem.parents[292,]

problem.parents[293,]
slice (parentage45.allby %>% filter (parent1 == "11wf008828" | parent2 == "11wf008828"))
#No change

problem.parents[294,]
slice (parentage45.allby %>% filter (parent1 == "12wm011194" | parent2 == "12wm011194"))
slice (parentage45.allby %>% filter (parent1 == "12wj012279" | parent2 == "12wj012279"))

#12wm011194 to female

parentage.all <- parentage.all %>% 
  mutate (Parent1.updated = replace (Parent1.updated, Parent.1 == "12wm011194", "12wf011194")) %>%
  mutate (Parent2.updated = replace (Parent2.updated, Parent.2 == "12wm011194", "12wf011194"))

problem.parents[295,]

#DONE