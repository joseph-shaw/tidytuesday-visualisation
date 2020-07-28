# Tidy Tuesday 2020-07-28: Penguins
# Script by Joseph Shaw

library(extrafont)
extrafont::font_import()
extrafont::loadfonts(device = "win")
library(tidyverse)
library(ggridges)
library(patchwork)

penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins')

#Density Plot 1
p1 <- 
ggplot(penguins)+
  geom_density_ridges(aes(x = body_mass_g/1000, y = species, fill = species), alpha = 0.7, colour = "transparent", scale = 3, rel_min_height = 0.01)+
  scale_fill_manual(values = rev(c("#28324B", "#FBDE44", "#F65158")))+  
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    plot.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    text = element_text(family = "Bahnschrift"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 30, unit = "pt")
    
  )+
  coord_cartesian(xlim = c(3, 6), ylim = c(1.4, 6), expand = T)+
  scale_x_continuous(breaks = seq(2, 7, 0.5))+
  labs(x = "Body Mass (kg)",
       y = NULL)+

# Export -------------------------------------------------------------
ggsave(here::here("2020-07-28_Penguins", "Temp", paste0("astro-plot", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       type = "cairo", height = 150, width = 300, units = "mm")



#Density Plot 2
p2 <- 
ggplot(penguins)+
  geom_density_ridges(aes(x = bill_length_mm, y = species, fill = species), alpha = 0.7, colour = "transparent", scale = 3, rel_min_height = 0.01)+
  scale_fill_manual(values = rev(c("#28324B", "#FBDE44", "#F65158")))+  
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    plot.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    text = element_text(family = "Bahnschrift"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 30, unit = "pt")
    
  )+
  coord_cartesian(xlim = c(36, 54), ylim = c(1.4, 6), expand = T)+
  scale_x_continuous(breaks = seq(0, 200, 2))+
  labs(x = "Bill Length (mm)",
       y = NULL)+
  
  # Export -------------------------------------------------------------
ggsave(here::here("2020-07-28_Penguins", "Temp", paste0("astro-plot", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       type = "cairo", height = 150, width = 300, units = "mm")


#Density Plot 3
p3 <- 
ggplot(penguins)+
  geom_density_ridges(aes(x = bill_depth_mm, y = species, fill = species), alpha = 0.7, colour = "transparent", scale = 3, rel_min_height = 0.01)+
  scale_fill_manual(values = rev(c("#28324B", "#FBDE44", "#F65158")))+  
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    plot.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    text = element_text(family = "Bahnschrift"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 30, unit = "pt")
  )+
  coord_cartesian(xlim = c(14, 21), ylim = c(1.4, 6), expand = T)+
  scale_x_continuous(breaks = seq(0, 200, 1))+
  labs(x = "Bill Depth (mm)",
       y = NULL)+
  # Export -------------------------------------------------------------
ggsave(here::here("2020-07-28_Penguins", "Temp", paste0("astro-plot", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       type = "cairo", height = 150, width = 300, units = "mm")

#Density Plot 4
p4 <- 
ggplot(penguins)+
  geom_density_ridges(aes(x = flipper_length_mm, y = species, fill = species), alpha = 0.7, colour = "transparent", scale = 3, rel_min_height = 0.01)+
  scale_fill_manual(values = rev(c("#28324B", "#FBDE44", "#F65158")))+  
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    plot.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    text = element_text(family = "Bahnschrift"),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 30, unit = "pt")
  )+
  coord_cartesian(xlim = c(180, 230), ylim = c(1.4, 6), expand = T)+
  scale_x_continuous(breaks = seq(0, 300, 10))+
  labs(x = "Flipper Length (mm)",
       y = NULL)+
  # Export -------------------------------------------------------------
ggsave(here::here("2020-07-28_Penguins", "Temp", paste0("astro-plot", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       type = "cairo", height = 150, width = 300, units = "mm")

#Combine plots
(p1|p2|p3|p4)+
ggsave(here::here("2020-07-28_Penguins", "Temp", paste0("astro-plot", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       type = "cairo", height = 50, width = 350, units = "mm")


#Scatter plot
ggplot(penguins)+
  geom_point(aes(x = bill_length_mm, y = flipper_length_mm, size = body_mass_g/1000, colour = species), alpha = 0.6)+
  scale_colour_manual(values = rev(c("#28324B", "#FBDE44", "#F65158")))+  
  labs(x = "Bill Length (mm)",
       y = "Flipper Length (mm)",
       size = "Body Mass (kg)",
       colour = "Species")+
  coord_cartesian(ylim = c(160, 240), xlim = c(30, 60))+
  scale_y_continuous(breaks = seq(160, 240, 20))+
  scale_x_continuous(breaks = seq(20, 60, 10))+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    plot.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    text = element_text(family = "Bahnschrift"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  )+
# Export -------------------------------------------------------------
ggsave(here::here("2020-07-28_Penguins", "Temp", paste0("astro-plot", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       type = "cairo", height = 150, width = 300, units = "mm")

