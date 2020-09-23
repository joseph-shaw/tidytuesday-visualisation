# 2020-09-22 - Mountains
# Script by Joseph Shaw for Tidy Tuesday

library(tidyverse)
library(ggtext)
library(extrafont)
library(extrafontdb)
#extrafont::font_import()
extrafont::loadfonts(device = "win")

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')
members.full <- read.csv("fullmembers.csv")
expeditions.full <- read.csv("data3.csv") %>% 
  rename("expedition_id" = 1)

everest <- expeditions %>% 
  filter(
    peak_name == "Everest",
    year < 1954
  ) %>% 
  arrange(year) %>% 
  mutate(
    order = 1:16
  ) %>% 
  left_join(expeditions.full, by = "expedition_id")

members.full <- members.full %>% 
  rename("member_id" = "membid", "year" = "myear") %>% 
  mutate(
    member_id = as.character(member_id),                                           # convert to character
    member_id = ifelse(member_id %in% 1:9, paste0("0", member_id), member_id),     # add 0
    member_id = paste0(expid, "-", member_id))                                     # combine w/ expid

m.everest <- members %>% 
  filter(
    peak_name == "Everest",
    year < 1954
  ) %>% 
  arrange(year) %>% 
  left_join(members.full, by = c("member_id", "year")) %>% 
  left_join(everest[, c("expedition_id", "order")], by = "expedition_id")

m.labels <- m.everest %>% 
  group_by(expedition_id, mperhighpt, order) %>% 
  mutate(lname = ifelse(lname %in% c("", NA, "Sherpa"), fname, lname)) %>% 
  summarise(
   label =  paste0(lname, ", ", collapse=''),
   label = substr(label, 1, nchar(label)-2)
  ) %>% 
  filter(mperhighpt>0) %>% 
  ungroup() %>% 
  mutate(
    row = 1:62,
    mperhighpt = ifelse(row %in% c(15, 22, 23, 24, 32, 36, 45, 49, 50), mperhighpt+50, mperhighpt),
    mperhighpt = ifelse(row %in% c(23, 24), mperhighpt+90, mperhighpt),
    mperhighpt = ifelse(row %in% c(24), mperhighpt+50, mperhighpt),
    mperhighpt = ifelse(row %in% c(22, 45, 49), mperhighpt+50, mperhighpt),
    mperhighpt = ifelse(row %in% c(62), mperhighpt+300, mperhighpt),
    label = ifelse(row %in% c(62), "Tenzing Norgay<br>& Edmund Hillary", label),
  )

d.m.labels <- m.everest %>% 
  mutate(lname = ifelse(lname %in% c("", NA, "Sherpa"), fname, lname)) %>% 
  filter(death_height_metres>0,
         death == TRUE
         ) %>% 
  mutate(
        mperhighpt = ifelse( mperhighpt == 0, death_height_metres, mperhighpt),
        row = 1:15
  ) %>% 
  group_by(mperhighpt, expedition_id, order) %>% 
  summarise(
    label =  paste0(lname, ", ", collapse=''),
    label = substr(label, 1, nchar(label)-2)
  ) %>% 
  ungroup() %>% 
  mutate(
    mperhighpt = ifelse(order == 2, mperhighpt - 200, mperhighpt),
    mperhighpt = ifelse(label == "Mallory, Irvine", mperhighpt + 50, mperhighpt),
    mperhighpt = ifelse(label == "Mingma Dorje", mperhighpt + 100, mperhighpt),
    mperhighpt = ifelse(label == "Unknown", mperhighpt + 200, mperhighpt),
  )

group.labels<- m.everest %>% 
  group_by(expedition_id, expedition_role) %>% 
  filter(expedition_role!= "Leader") %>% 
  summarise(
    label = paste0(fname, " ", lname, ", ", collapse='') #,
    #label = paste0(expedition_role, "s: ", label),
    #label = gsub("Cameramans", "Cameramen", label)
  ) %>% 
  distinct() %>% 
  mutate(
    label = paste0(substr(label, 1, nchar(label)-2), ". ")
         ) %>% 
  group_by(expedition_id) %>% 
  summarise(
    label = paste0(label, collapse='')
    ) %>% 
  left_join(everest[, c("expedition_id", "order")], by = "expedition_id")
    

everest %>% 
  ggplot()+
# gridlines
  geom_segment(data = everest[1:10,], y = c(seq(0, 8000, 1000), 8850), yend = c(seq(0, 8000, 1000), 8850), x = rep(0.4, 10), xend = rep(17.15, 10), colour = "lightgrey", size = 0.1)+
  geom_segment(data = everest[1,], y = 8850, yend = 8850, x = 0.4, xend = 17.15, colour = "darkgrey", size = 0.2)+
  
  # Y axis labels  
  geom_richtext(data = expeditions[1:20,], x = c(rep(0.4, 10), rep(17, 10)), y = rep(c(seq(0, 8000, 1000), 8850)+70, 2), label = rep(c(seq(0, 8000, 1000), 8850), 2), angle = 0, hjust = 0, 
                fill = "transparent", label.padding = unit(0.1, "lines"), label.colour = "transparent", color = "darkgrey",
                family = "Bahnschrift", size = 2.1) +
  geom_richtext(data = everest[1,], x = 0.4, y = 0, label = "Elevation (m)", angle = 90, hjust = 0, vjust = 0,
                fill = "transparent", label.padding = unit(0.1, "lines"), label.colour = "transparent", color = "darkgrey",
                family = "Bahnschrift", size = 4) +
  
# Plot paths
  geom_segment(aes(x = order, xend = order, y = 0, yend = bcht), colour = "#5e5e5e", linetype = "dotted")+
  geom_segment(aes(x = order, xend = order, y = bcht, yend = highpoint_metres), colour = "#5e5e5e", linetype = "dashed")+
  geom_segment(data = everest[c(1, 9, 10, 15),], aes(x = c(1, 9, 10, 15), xend = c(1, 9, 10, 15), y = rep(0, 4), yend = highpoint_metres), colour = "#5e5e5e", linetype = "dotted")+
  
# Plot basecamp
  geom_point(aes(x = order, y = bcht), shape = 24, colour = "black", fill = "white")+
  
# Mallory/Irvine Mystery Peak
  #geom_point(x = 3, y = 8850, shape = 21, fill = "white", colour = "black", size = 3, stroke = 0.2)+
  geom_point(x = 3, y = 8850, shape = 23, colour = "darkgrey", fill = "white", size = 2)+

# Plot summit
  geom_point(aes(x = order, y = highpoint_metres), shape = 23, size = 2, fill = "white", colour = "black")+

  
# Add year & leader
  geom_richtext(aes(x = order-0.1, y = 0, label = paste(year.x, leaders, sep = "  |  ")), angle = 90, hjust = 0, fill = "transparent", color = "#474747", label.padding = unit(0.1, "lines"), label.colour = "transparent", family = "Bahnschrift", size = 5) +
  #ggimage::geom_image(data = d, aes(image=image, x = x, y = y), width=.001)+
# Add members  
  geom_richtext(data = group.labels, aes(x = order+0.02, y = 0, label = gsub("\n", "<br>", stringr::str_wrap(label, 90))),
                angle = 90, hjust = 0, vjust = 1, fill = "transparent", color = "darkgrey", label.padding = unit(0.1, "lines"),
                label.colour = "transparent", family = "Bahnschrift", size = 1.4) +

# Add member height
  geom_richtext(data = m.labels[1:61,], aes(x = order+0.04, y = mperhighpt , label = gsub("\n", "<br>", stringr::str_wrap(label, 40))), angle = 0, hjust = 0, 
                fill = "transparent", label.padding = unit(0.1, "lines"), label.colour = "transparent", color = "#5e5e5e",
                family = "Bahnschrift", size = 1.2) +
  geom_richtext(data = m.labels[62,], aes(x = order+0.04, y = mperhighpt , label = gsub("\n", "<br>", stringr::str_wrap(label, 40))), angle = 0, hjust = 0, 
                fill = "transparent", label.padding = unit(0.1, "lines"), label.colour = "transparent", color = "black", fontface = "bold",
                family = "Bahnschrift", size = 2.6) +
# Add deceased climbers
  geom_richtext(data = d.m.labels, aes(x = order+0.04, y = mperhighpt, label = gsub("\n", "<br>", stringr::str_wrap(label, 40))), angle = 0, hjust = 0, 
                fill = "transparent", label.padding = unit(0.1, "lines"), label.colour = "transparent", color = "darkred",  
                family = "Bahnschrift", size = 1.2) +
  
# Change theme options  
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
  )+
# Change axis limits  
  coord_cartesian(ylim = c(0.5, 9500), xlim = c(0.4, 17))+
  
  
#Export  
  ggsave(here::here("2020-09-22_Mountains", "temp", paste0("ever-plot", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       type = "cairo", height = 150, width = 650, units = "mm")








