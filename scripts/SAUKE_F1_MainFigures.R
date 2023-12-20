### Code for "Hatchery supplementation provides a demographic boost but alters age composition of sockeye salmon in Auke Lake, Southeast Alaska"
### Author: Megan McPhee, University of Alaska Fairbanks
### Date: 2021-2023

#Main manuscript figures

library (tidyverse)
library (cowplot)
library (patchwork)

############ Figure 3 number of offspring ############################

#Panel A - all RS

##Top row - hatchery females

p1.1 <- ggplot (data = lod45_RS %>% filter (Psex == "female" & Ptype == "hatchery" & by == 2011)) + aes (x = log(n_offspring+1)) + 
  geom_histogram(bins = 15, color = "salmon", fill = "salmon")  + coord_cartesian (xlim = c(0,5.2), ylim = c(0,11)) + xlab (NULL) + 
  ylab (NULL)  +  scale_x_continuous (expand = expansion(mult = c(0.04, 0.015)), breaks = c(log(1), log(3), log(11), log(51), log(151))) +
  scale_y_continuous (expand = expansion(0), breaks = c(5,10)) +  
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text = element_text(size = 11), 
        plot.margin = margin (0,0.1,0,0), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1), panel.background = element_blank())


p1.2 <- ggplot (data = lod45_RS %>% filter (Psex == "female" & Ptype == "hatchery" & by == 2012)) + aes (x = log(n_offspring+1)) + 
  geom_histogram(bins = 15, color = "salmon", fill = "salmon")  + coord_cartesian (xlim = c(0,5.2), ylim = c(0,11)) + xlab (NULL) + 
  ylab (NULL)  +  scale_x_continuous (expand = expansion(mult = c(0.04, 0.015)), breaks = c(log(1), log(3), log(11), log(51), log(151))) +
  scale_y_continuous (expand = expansion(0), breaks = c(5,10)) +  
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text = element_text(size = 11), 
        plot.margin = margin (0,0.1,0,0), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1), panel.background = element_blank())

p1.3 <- ggplot (data = lod45_RS %>% filter (Psex == "female" & Ptype == "hatchery" & by == 2013)) + aes (x = log(n_offspring+1)) + 
  geom_histogram(bins = 15, color = "salmon", fill = "salmon")  + coord_cartesian (xlim = c(0,5.2), ylim = c(0,11)) + xlab (NULL) + 
  ylab (NULL)  +  scale_x_continuous (expand = expansion(mult = c(0.04, 0.015)), breaks = c(log(1), log(3), log(11), log(51), log(151))) +
  scale_y_continuous (expand = expansion(0), breaks = c(5,10)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), text = element_text(size = 11), 
        plot.margin = margin (0,0.1,0,0), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size = 1), panel.background = element_blank())


p.row1 <- p1.1 + p1.2 + p1.3


##Second row - wild females


p2.1 <- ggplot (data = lod45_RS %>% filter (Psex == "female" & Ptype == "wild" & by == 2011)) + aes (x = log(n_offspring+1)) + 
  geom_histogram(bins = 15, color = "steelblue", fill= "steelblue")  + coord_cartesian (xlim = c(0,5.2), ylim = c(0,800)) +
  xlab (NULL) + ylab (NULL)  +  scale_y_continuous (expand = expansion(0)) + 
  scale_x_continuous (expand = expansion (mult= c(0.04,0.015)), breaks = c(log(1), log(3), log(11), log(51), log(151)), labels = c(0, 2, 10, 50, 150)) + 
  theme(text = element_text(size = 11), plot.margin = margin (3,0.01,0,0), panel.border = element_rect(colour = "black", fill=NA, size = 1), 
        panel.background = element_blank())

p2.2 <- ggplot (data = lod45_RS %>% filter (Psex == "female" & Ptype == "wild" & by == 2012)) + aes (x = log(n_offspring+1)) + 
  geom_histogram(bins = 15, color = "steelblue", fill= "steelblue")  + coord_cartesian (xlim = c(0,5.2), ylim = c(0,800)) + xlab (NULL) + 
  ylab (NULL)  +  scale_y_continuous (expand = expansion(0)) + 
  scale_x_continuous (expand = expansion(mult= c(0.04,0.015)), breaks = c(log(1), log(3), log(11), log(51), log(151)), labels = c(0, 2, 10, 50, 150)) + 
  theme(text = element_text(size = 11), plot.margin = margin (3,0.01,0,0), axis.title.y=element_blank(), axis.text.y=element_blank(),
  axis.ticks.y=element_blank(), panel.border = element_rect(colour = "black", fill=NA, size = 1), panel.background = element_blank())

p2.3 <- ggplot (data = lod45_RS %>% filter (Psex == "female" & Ptype == "wild" & by == 2013)) + aes (x = log(n_offspring+1)) + 
  geom_histogram(bins = 15, color = "steelblue", fill= "steelblue")  + coord_cartesian (xlim = c(0,5.2), ylim = c(0,800)) + xlab (NULL) + 
  ylab (NULL)  +  scale_y_continuous (expand = expansion(0)) + 
  scale_x_continuous (expand = expansion(mult= c(0.04,0.015)), breaks = c(log(0), log(3), log(11), log(51), log(151)), labels = c(0, 2, 10, 50, 150)) + 
  theme(text = element_text(size = 11), plot.margin = margin (3,0.01,0,0), axis.title.y=element_blank(), axis.text.y=element_blank(),
    axis.ticks.y=element_blank(), panel.border = element_rect(colour = "black", fill=NA, size = 1), panel.background = element_blank())

p.row2 <- p2.1 + p2.2 + p2.3

#Combine the two all-RS plots into a single plot.

fig2.base <- p.row1/p.row2
fig2.base

#add labels

p.temp <- plot_grid (NULL, fig2.base, ncol = 2, rel_widths = c(0.08, 1))
p.temp2 <- plot_grid (p.temp, NULL, ncol = 1, rel_heights = c(1, 0.08))

p.temp4 <- p.temp2 + draw_label ("Number of offspring", x = 0.55, y = 0.044, size = 12) + 
  draw_label ("Number of females", angle = 90, x = 0.04, y = 0.75, size = 12) + 
  draw_label ("Number of females", angle = 90, x = 0.04, y = 0.27, size = 12) + draw_label ("a)", x = 0.018, y = 0.93, size = 16) + 
  draw_label ("b)", x = 0.018, y = 0.46, size = 16) + draw_label ("Hatchery", x = 0.92, y = 0.25, color = "salmon", fontface = "bold") + 
  draw_label ("Wild", x = 0.94, y = 0.21, color = "steelblue", fontface = "bold")

plot_grid (NULL, p.temp4, ncol = 1, rel_heights = c(0.05, 1)) + draw_label ("2011", x = 0.25, y = 0.92, size = 13) + 
  draw_label ("2012", x = 0.53, y = 0.92, size = 13) + draw_label ("2013", x = 0.84, y = 0.92, size = 13)


ggsave("Figure3.pdf", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 18.2, height = 13.65, units = "cm",
       dpi = 600, limitsize = TRUE)


########## Figure 4: Run timing ##################

supp4.base <- ggplot (data = demog9 %>% filter (ReturnYear > 2014)) + aes (x = doy, y = 100*(..y..), color = ptype) + stat_ecdf () + scale_color_manual (values = c("salmon", "steelblue")) + facet_wrap (~factor (ReturnYear), nrow = 2) +  xlab (NULL) + ylab (NULL) + scale_x_continuous (breaks = c(183, 214, 245), labels = c ("1-Jul", "1-Aug", "1-Sep")) + theme_cowplot(10) + 
  theme (
    legend.position = c(0.75, 0.25), 
    legend.title = element_blank(),
    plot.margin = unit(c(1, 1, 1, 8),"mm")) 

ggdraw (supp4.base)  + 
  draw_label ("\u03B4 = 1", x = 0.09, y = 0.90, size = 9, hjust = 0) + draw_label ("p = 0.44", x = 0.09, y = 0.87, size = 9, hjust = 0) + 
  draw_label ("\u03B4 = 0", x = 0.41, y = 0.90, size = 9, hjust = 0) + draw_label ("p = 0.99", x = 0.41, y = 0.87, size = 9, hjust = 0) + 
  draw_label ("\u03B4 = -1", x = 0.71, y = 0.90, size = 9, hjust = 0) + draw_label ("p < 0.001*", x = 0.71, y = 0.87, size = 9, hjust = 0) + 
  draw_label ("\u03B4 = 1", x = 0.09, y = 0.43, size = 9, hjust = 0) + draw_label ("p = 0.12", x = 0.09, y = 0.40, size = 9, hjust = 0) + 
  draw_label ("\u03B4 = -0.5", x = 0.41, y = 0.43, size = 9, hjust = 0) + draw_label ("p = 0.95", x = 0.41, y = 0.40, size = 9, hjust = 0) + 
  draw_label ("Cumulative percent", x = 0.03, y = 0.5, angle = 90, size = 10)


ggsave ("Figure4.eps", plot = last_plot(), device = cairo_ps, path = NULL, scale = 1, width = 180, height = 120, units = "mm", dpi = 600, limitsize = TRUE)

#Note: use the device = cairo_ps (for eps) or cairo_pdf (for pdf); see
#https://stackoverflow.com/questions/57558661/encoding-greek-symbols-in-eps-and-pdf-of-ggplot-figures

########## Figure 5: Size at age ##################

ggplot (data = asl.saa %>% filter (age.disc == 0)) +  aes (x = age.class2, y = MidEyeForkLength, color = ptype) + geom_point(size = 0.7) + theme_cowplot(8) + scale_x_continuous (labels = c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3")) + scale_y_continuous(limits = c(250,650), breaks = seq (350, 650, 100), expand = c(0,0)) + xlab ("Scale age") + ylab ("Mid-eye to fork length (mm)")  + scale_color_manual (values = c("salmon", "steelblue")) + 
  theme (axis.ticks.x = element_blank(),
         legend.position = c(0.8, 0.2), 
         legend.title = element_blank())

ggsave ("Figure5.pdf", plot = last_plot(), device = NULL, path = NULL, scale = 1, width = 8.84, height = 5.89, units =  "cm", dpi = 600, limitsize = TRUE)


