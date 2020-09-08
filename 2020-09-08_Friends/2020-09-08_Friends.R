# Tidy Tuesday - Friends
# Script by Joseph Shaw 2020-09-07

library(tidyverse)
library(ggtext)
library(extrafont)

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

df <- friends %>% 
  left_join(friends_emotions, by = c("season", "episode", "scene", "utterance")) %>% 
  left_join(friends_info, by = c("season", "episode")) %>% 
  #mutate(row = as.numeric(row.names(df))) %>% 
  group_by(season) %>% 
  mutate(
    x = episode / 25
  ) %>% 
  filter(
    grepl("on a break", text, ignore.case = T)
  ) %>% 
  ungroup() %>% 
  add_row(season = c(1,2)) %>% 
  mutate(
    x = ifelse(utterance == 24, x+0.01, x),
    x = ifelse(utterance == 38, x+0.02, x),
    x = ifelse(utterance == 5 & scene == 10, x+0.01, x),
    x = ifelse(utterance == 10 & scene == 12, x+0.01, x),
    speaker = ifelse(speaker == "Passenger", "Hugh Laurie", speaker),
    speaker = factor(speaker, levels = c("Ross Geller", "Rachel Green", "Joey Tribbiani", "Phoebe Buffay", "Ben Geller", "Hugh Laurie"))
  )

eps <- c(24, 24, 25, 24, 24, 25, 24, 24, 24, 18 )

ggplot(df)+
  geom_segment(data = df[1:10,], aes(x = rep(1/100, 10), xend = eps/25, y = -1:-10, yend = -1:-10), col = "grey")+
  geom_richtext(data = df[1:10,], aes(x = rep(-0.05, 10), y = -1:-10, label = 1:10), fill = "transparent", label.colour = "transparent", colour = "#60605e", family = "Bahnschrift")+
  geom_richtext(data = friends[1:25,], aes(x = seq(1, 25, 1)/25, y = rep(-10.7, 25), label = seq(1, 25, 1)), fill = "transparent", label.colour = "transparent", colour = "#60605e", family = "Bahnschrift")+
  geom_richtext(data = friends[1,], aes(x = 0.52, y = -11.2, label = "Episode"), fill = "transparent", label.colour = "transparent", colour = "#60605e", family = "Bahnschrift")+
  geom_richtext(data = friends[1,], aes(x = -0.1, y = -5.5, label = "Season"), fill = "transparent", label.colour = "transparent", colour = "#60605e", family = "Bahnschrift", angle = 90)+
  geom_point(aes(x = x, y = -season, colour = speaker), size = 8, alpha = 0.5)+
  scale_color_manual(values = rev(c("#E47820","#5FACC8",  "#D23428",  "#27213D","#2C6049",  "#DFC41B")))+
  geom_point(data = df[c(1, 2, 3, 6, 10, 12),], aes(x = seq(0.1, 1.1, 1/5)-0.12, y = c(rep(-12.5, 6)), colour = levels(speaker)), size = 8, alpha = 0.5)+
  geom_richtext(data = df[c(1, 2, 3, 6, 10, 12),], aes(x = seq(0.1, 1.1, 1/5)-0.12, y = rep(-13, 6), label = levels(speaker)), fill = "transparent", label.colour = "transparent", colour = "#60605e", family = "Bahnschrift", hjust = 0.5)+
  geom_richtext(data = friends[1,], aes(x = -0.1, y = 3.1, label = 'WE WERE'), fill = "transparent", label.colour = "transparent", colour = "black", family = "Arial", size = 30, hjust = 0)+
  geom_richtext(data = friends[1,], aes(x = -0.1, y = 1.4, label = 'ON A BREAK'), fill = "transparent", label.colour = "transparent", colour = "black", family = "Arial Bold", size = 30, hjust = 0)+
  geom_richtext(data = friends[1,], aes(x = -0.08, y = 0.35, label = 'When was the phrase "on a break" mentioned throughout Friends?'), fill = "transparent", label.colour = "transparent", colour = "#474747", family = "Arial Bold", size = 5, hjust = 0)+
  geom_richtext(data = friends[1,], aes(x = -0.08, y = 0., label = 'Visualisation by Joseph Shaw | Data: EmilHvitfeldt/friends'), fill = "transparent", label.colour = "transparent", colour = "#474747", family = "Arial Bold", size = 4, hjust = 0)+
  coord_cartesian(ylim = c(-14, 3.3))+
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FFFBF5", colour = "transparent"),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(2, 2, 0, 1, "cm")
    
  )+
  ggsave(here::here("2020-09-08_Friends", "Temp", paste0("friends-plot", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
           type = "cairo", height = 300, width = 240, units = "mm")
  


