### Code for "Hatchery supplementation provides a demographic boost but alters age composition of sockeye salmon in Auke Lake, Southeast Alaska"
### Author: Megan McPhee, University of Alaska Fairbanks
### Date: 2021-2023

#Supplementary figures

library (tidyverse)
library (cowplot)
library (patchwork)
library (ggforce)

################### Supplementary Figure S1 - Parent phenotypes ############################################
##Length-frequency histograms
sf1.a <- ggplot (data = parent.demog %>% filter (MidEyeForkLength > 100)) + aes (x = MidEyeForkLength) + 
  geom_histogram (bins = 26, aes (fill = sp_type)) + facet_grid (rows = vars (sp_type), cols = vars (ReturnYear), scales = "free_y") + 
  xlab ("") + ylab ("") + scale_fill_manual (values = c("salmon", "steelblue")) + 
  theme (legend.position = "none", strip.text.y = element_blank())


##Capture date plot

doy.plot11 <- ggplot (data = parent.demog %>% filter (sp_type == "wild" & doy > 50 & ReturnYear == 2011)) + aes (x = doy) + 
  geom_histogram (bins = 80, fill = "steelblue") + xlab (NULL) + ylab ("") +  scale_x_continuous(limits = c(170, 250), breaks = c(182, 213, 244))+ 
  scale_y_continuous( limits = c(0, 500), expand = c(0.03,0), breaks = c(0, 250, 500))  + 
  theme (axis.text.x = element_blank()) +  geom_point (x = 206, y = 400, shape = 3, size = 1, color = "salmon") +  
  geom_point (x = 207, y = 400, shape = 3, size = 1, color = "salmon") 

doy.plot12 <- ggplot (data = parent.demog %>% filter (sp_type == "wild" & doy > 50 & ReturnYear == 2012)) + aes (x = doy) + 
  geom_histogram (bins = 80, fill = "steelblue") + xlab (NULL) + ylab ("") +  scale_x_continuous(limits = c(170, 250), breaks = c(182, 213, 244))+ 
  scale_y_continuous( limits = c(0, 500), expand = c(0.03,0), breaks = c(0, 250, 500)) + theme (axis.text.x = element_blank()) + 
  geom_point (x = 199, y = 85, shape = 3, size = 1, color = "salmon") +  geom_point (x = 200, y = 85, shape = 3, size = 1, color = "salmon") 

doy.plot13 <- ggplot (data = parent.demog %>% filter (sp_type == "wild" & doy > 50 & ReturnYear == 2013)) + aes (x = doy) + 
  geom_histogram (bins = 80, fill = "steelblue") + xlab (NULL) + ylab ("") + 
  scale_y_continuous( limits = c(0, 500), expand = c(0.03,0), breaks = c(0, 250, 500)) + 
  scale_x_continuous(limits = c(170, 250), breaks = c(182, 213, 244), labels = c ("1-Jul", "1-Aug", "1-Sep")) + 
  geom_point (x = 191, y = 475, shape = 3, size = 1, color = "salmon") +  
  geom_point (x = 192, y = 475, shape = 3, size = 1, color = "salmon") 

rt.plot <- doy.plot11/doy.plot12/doy.plot13


###Assemble, add stat summaries for length comparisons (jacks excluded)

parent.demog %>% filter (MidEyeForkLength > 400) %>% group_by (ReturnYear, sp_type) %>% summarize (mnln = mean (MidEyeForkLength))

sup.f1.b1 <- plot_grid (sf1.a, rt.plot, ncol = 1, rel_heights = c(1,1.2))

ggdraw (sup.f1.b1) + draw_label ("Mid-eye to fork length (mm)", size = 11, x = 0.52, y = 0.58) + 
  draw_label ("Number of fish", angle = 90, size = 11, x = 0.012, y = 0.78 ) + draw_label ("Return date", size = 11, x = 0.52, y = 0.015) + 
  draw_label ("Number of fish", angle = 90, size = 11, x = 0.012, y = 0.30) +
  draw_label ("a)", x = 0.012, y = 0.975) + draw_label ("b)", x = 0.012, y = 0.5) + 
  draw_label ("2011", size = 10, x = 0.92, y = 0.47) + draw_label ("2012", size = 10, x = 0.92, y = 0.31) + 
  draw_label ("2013", size = 10, x = 0.92, y = 0.16)


dev.new (width = 7.09, height = 5.31, unit = "in", noRStudioGD = T); last_plot() #180 mm x 125 mm
ggsave ("Supplementary_FigS1.pdf", width = dev.size()[1], height = dev.size()[2], dpi = 600) 
dev.off()

################### Supplementary Figure S2 - CKMRsim results ############################################
#see "scripts/SAUKE_F1_Rscript01_CKMRsim"


################### Supplementary Figure S3 - LOD distributions ############################################
par.all <- parentage.all %>% 
  dplyr::select (c(Offspring, Parent1.updated, Parent2.updated, Pair.LOD.Parent.1, Pair.LOD.Parent.2, Posterior.Parent.1, Posterior.Parent.2, broodyear)) %>%
  mutate (partype = case_when (
    substr (Parent1.updated, 3, 3) == "h" ~ "hatchery",
    substr (Parent2.updated, 3, 3) == "h" ~ "hatchery",
    TRUE ~ "wild"
  )) %>% filter (broodyear > 2010 & broodyear <2014)

table (par.all$broodyear, par.all$partype)

#make a df for plotting
lod.p1 <- par.all %>% dplyr::select (Pair.LOD.Parent.1, partype, broodyear)
lod.p2 <- par.all %>% dplyr::select (Pair.LOD.Parent.2, partype, broodyear)
names (lod.p1)[1] <- "LOD"
names (lod.p2)[1] <- "LOD"
lod.all <- rbind (lod.p1, lod.p1) %>% filter (!is.na (LOD))

ggplot (data = lod.all) + aes (x = partype, y = LOD) + geom_sina(shape = 1, aes (color = partype)) + facet_wrap (~broodyear) + 
  xlab ("Parental type") +
  stat_summary(fun.y=median, geom="point", fill= "white", shape = 21, size =3) + scale_color_manual (values = c("salmon", "steelblue")) + 
  theme (legend.position = "none")

ggsave("Supplementary_FigS3.pdf", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 180, height = 135, units = "mm",
       dpi = 600, limitsize = TRUE)


################### Supplementary Figure S4 - Run timing; only parents sampled on broodstock days ############################################

supp4b.base <- ggplot (data = phenotypes.doy %>% filter (ReturnYear > 2014 & retain == "retain")) + aes (x = doy, y = 100*(..y..), color = ptype) + stat_ecdf () + 
  scale_color_manual (values = c("salmon", "steelblue")) + facet_wrap (~factor (ReturnYear), nrow = 2) +  xlab (NULL) + ylab (NULL) + 
  scale_x_continuous (breaks = c(183, 214, 245), labels = c ("1-Jul", "1-Aug", "1-Sep")) + 
  theme (legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank())  + 
  theme_cowplot(10) + theme (legend.position = c(0.75, 0.25), legend.title = element_blank())

ggdraw (supp4b.base)  + 
  draw_label ("\u03B4 = 1", x = 0.079, y = 0.90, size = 7, hjust = 0) + draw_label ("p = 0.44", x = 0.079, y = 0.87, size = 7, hjust = 0) + 
  draw_label ("\u03B4 = 0", x = 0.38, y = 0.90, size = 7, hjust = 0) + draw_label ("p = 0.74", x = 0.38, y = 0.87, size = 7, hjust = 0) + 
  draw_label ("\u03B4 = -2", x = 0.7, y = 0.90, size = 7, hjust = 0) + draw_label ("p < 0.001*", x = 0.7, y = 0.87, size = 7, hjust = 0) + 
  draw_label ("\u03B4 = 1", x = 0.079, y = 0.43, size = 7, hjust = 0) + draw_label ("p = 0.07", x = 0.079, y = 0.40, size = 7, hjust = 0) + 
  draw_label ("\u03B4 = 0.5", x = 0.38, y = 0.43, size = 7, hjust = 0) + draw_label ("p = 0.56", x = 0.38, y = 0.40, size = 7, hjust = 0)

ggsave ("FigureS4_color.png", plot = last_plot(), device = NULL, path = NULL, scale = 1, width = 12, height = 8, units = "cm", dpi = 600, limitsize = TRUE)

################### Supplementary Figure S5 - Environmental data ############################################

#data received from Josh Russell, NOAA, via email on Apr 13, 2022, "Auke Creek 2011-2014, Temp, Flow, and Ice-out.xls".
#saved first sheet (temp and gage height) to .csv, "TempGage.csv"

auke.env <- read.csv ("data/TempGage.csv", header = TRUE)
str (auke.env)

auke.env$date <- dmy (auke.env$Date)
auke.env$month <- month (auke.env$date)
auke.env$year <- year (auke.env$date)
auke.env$doy <- yday (auke.env$date)

#divide into two times - return and spawning (1 Jul-30 Sep); incubation (Oct 1-31 Dec)

#return & spawning period
t1 <- ggplot (auke.env %>% filter (doy>181 & doy < 274 & year != 2014)) + aes (x = doy, y = CreekTemperature, color = factor(year)) + geom_line(size = 1) + scale_x_continuous(breaks = c(182, 213, 244, 274), labels = c("1-Jul", "1-Aug", "1-Sep", "1-Oct")) + scale_y_continuous (limits = c(10, 20), expand = c(0,0)) + theme_cowplot() + theme (legend.position = "none") + xlab ("") + ylab ("Temperature (°C)")

g1 <- ggplot (auke.env %>% filter (doy>181 & doy < 274 & year != 2014)) + aes (x = doy, y = GageHeight, color = factor(year)) + geom_line(size = 1) + scale_x_continuous(breaks = c(182, 213, 244, 274), labels = c("1-Jul", "1-Aug", "1-Sep", "1-Oct")) + scale_y_continuous (limits = c(20.5, 23), expand = c(0,0), breaks = c(21, 22, 23)) + theme_cowplot() + theme (legend.position = "none") + xlab ("") + ylab ("Gage height (cm)")

#incubation period
t2 <- ggplot (auke.env %>% filter (doy>273  & year != 2014)) + aes (x = doy, y = CreekTemperature, color = factor(year)) + geom_line(size = 1) + scale_x_continuous(breaks = c(274, 305, 335, 366), labels = c("1-Oct", "1-Nov", "1-Dec", "1-Jan")) + scale_y_continuous (limits = c(0, 12), expand = c(0,0)) + theme_cowplot() + theme (legend.position = "none") + xlab ("") + ylab ("")

g2 <- ggplot (auke.env %>% filter (doy > 273 & year != 2014)) + aes (x = doy, y = GageHeight, color = factor(year)) + geom_line(size = 1) + scale_x_continuous(breaks = c(274, 305, 335, 366), labels = c("1-Oct", "1-Nov", "1-Dec", "1-Jan")) + scale_y_continuous (limits = c(20.5, 23), expand = c(0,0), breaks = c(21, 22, 23)) + theme_cowplot() + theme (legend.position = "none") + xlab ("") + ylab ("")

#Put it all together.
(t1+t2)/(g1+g2)


















