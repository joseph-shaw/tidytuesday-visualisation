# Tidy Tuesday 2020-07-14
# Astronauts script by Joseph Shaw

# Load packages
library(tidyverse)
library(janitor)
library(knitr)
library(ggtext)
library(extrafont)

# Load Data
astro <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv') %>% 
  clean_names() %>% 
  filter(!is.na(number)) %>%  # remove last row (all values are NA)
  mutate(
    sex = if_else(sex == "M", "male", "female"),
    military_civilian = if_else(military_civilian == "Mil", "military", "civilian")
  ) %>% 
  arrange(year_of_mission, mission_title) %>%
  filter(
    total_eva_hrs > 0
  ) %>% 
  mutate(
    # Create id for plotting
    id = 1:n(),
    log_total_hrs_sum = log(total_hrs_sum),
    # create rotation helper
    rot = seq(0, 359, 359/n())[1:603],
    rot = 0-rot-90,
    label = paste(name, nationality, selection, occupation, sep = "  |  "),
    # Create variable alpha
    alpha = scale(eva_hrs_mission, center = T, scale = T),
    alpha = alpha - min(alpha, na.rm = T) ,
    alpha = alpha / max(alpha)*2,
    alpha = ifelse(alpha>1, 1, alpha),
    # Flip rotation of labels in one half of the plot
    rot = ifelse(id < n()/2, rot-180, rot)
  )

#Create dfs to plot year and mission
first <- astro[match(unique(astro$mission_title), astro$mission_title),]
year.first <- astro[match(unique(astro$year_of_mission), astro$year_of_mission),]

  # Create plot
ggplot(astro)+
  # Add star brightness --------------------------------------------------
  geom_segment(aes(x = id, xend = id, y = -20, yend = runif(nrow(astro), -5, 7.5), fill = "white", alpha = 0.01),col = "white" )+
  # Plot EVA hrs in each mission -----------------------------------------
  geom_point(aes(x = id, y = eva_hrs_mission, col = occupation, size = hours_mission))+
  scale_size_continuous(range = c(0.2, 2))+
  scale_color_manual(values = c("#F8E75B","#F8766D", "#ED7A41","#00BEC3","#619CFE", "#8500F8"))+
  # Add trailing lines --------------------------------------------------
  geom_segment(aes(x = id, xend = id, y = eva_hrs_mission, yend = eva_hrs_mission-total_number_of_missions*2, col = occupation, alpha = alpha/3))+
  geom_segment(aes(x = id, xend = id, y = eva_hrs_mission, yend = eva_hrs_mission-20, col = occupation, alpha = alpha/14))+
  # Plot more star trails -----------------------------------------------
  geom_segment(aes(x = id, xend = id, y = -20, yend = runif(nrow(astro), -10, 0), fill = "white", alpha = 0.01), col = "white", size = 0.3 )+
  geom_segment(aes(x = id, xend = id, y = -20, yend = runif(nrow(astro), -15, -10), fill = "white", alpha = 0.01), col = "white", size = 0.3 )+
  geom_segment(aes(x = id, xend = id, y = -20, yend = runif(nrow(astro), 0, -5), fill = "white", alpha = 0.01), col = "white", size = 0.3 )+
  # Add labels ----------------------------------------------------------
  geom_richtext(data = first, aes(x = id, y = 52, label = toupper(mission_title), angle = rot, hjust = ifelse(id>302, 1, 0)), fill = "transparent", color = "white", label.padding = unit(0.1, "lines"), label.colour = "transparent", family = "Bahnschrift", size = 1.6) +
  geom_richtext(aes(x = id, y = 65, label = toupper(name), angle = rot, hjust = ifelse(id>=302, 1, 0)), fill = "transparent", color = "white", label.padding = unit(0.1, "lines"), label.colour = "transparent", family = "Bahnschrift", size = 1.3) +
  geom_richtext(data = year.first, aes(x = ifelse(year_of_mission == 1974, id-1, id), y = 38, label = toupper(year_of_mission), angle = rot, hjust = ifelse(id>302, 1, 0)), fill = "transparent", color = "white", label.padding = unit(0.1, "lines"), label.colour = "transparent", family = "Bahnschrift", size = 3.5) +
  # Stop overlap of first and last --------------------------------------
  expand_limits(x=0)+
  # Polar coordinates ---------------------------------------------------
  coord_polar(clip = "off")+
  # Change y gridlines  -------------------------------------------------
  scale_y_continuous(breaks = seq(0,55, 2))+
  # Change theme --------------------------------------------------------
  theme_minimal()+
  theme(
  # Gridlines -------------------------
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, linetype = "dotted", colour = "grey"),
  # Remove axis text ------------------
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = margin(t = 200, l = 0, r = 0, b = 50),
    plot.background = element_rect(fill = "black")
      )+
  # Export -------------------------------------------------------------
  ggsave(here::here("2020-07-14_Astronauts", "Temp", paste0("astro-plot", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
        type = "cairo", height = 700, width = 700, units = "mm")



