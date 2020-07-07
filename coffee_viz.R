# Coffee Viz - Tidy Tuesday
# Script by Joseph Shaw - 07/07/2020

library(tidyverse)
library(patchwork)
library(extrafont)

raw_arabica <- read_csv("https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/arabica_data_cleaned.csv") %>% 
  janitor::clean_names()

raw_robusta <- read_csv("https://raw.githubusercontent.com/jldbc/coffee-quality-database/master/data/robusta_data_cleaned.csv",
                        col_types = cols(
                          X1 = col_double(),
                          Species = col_character(),
                          Owner = col_character(),
                          Country.of.Origin = col_character(),
                          Farm.Name = col_character(),
                          Lot.Number = col_character(),
                          Mill = col_character(),
                          ICO.Number = col_character(),
                          Company = col_character(),
                          Altitude = col_character(),
                          Region = col_character(),
                          Producer = col_character(),
                          Number.of.Bags = col_double(),
                          Bag.Weight = col_character(),
                          In.Country.Partner = col_character(),
                          Harvest.Year = col_character(),
                          Grading.Date = col_character(),
                          Owner.1 = col_character(),
                          Variety = col_character(),
                          Processing.Method = col_character(),
                          Fragrance...Aroma = col_double(),
                          Flavor = col_double(),
                          Aftertaste = col_double(),
                          Salt...Acid = col_double(),
                          Balance = col_double(),
                          Uniform.Cup = col_double(),
                          Clean.Cup = col_double(),
                          Bitter...Sweet = col_double(),
                          Cupper.Points = col_double(),
                          Total.Cup.Points = col_double(),
                          Moisture = col_double(),
                          Category.One.Defects = col_double(),
                          Quakers = col_double(),
                          Color = col_character(),
                          Category.Two.Defects = col_double(),
                          Expiration = col_character(),
                          Certification.Body = col_character(),
                          Certification.Address = col_character(),
                          Certification.Contact = col_character(),
                          unit_of_measurement = col_character(),
                          altitude_low_meters = col_double(),
                          altitude_high_meters = col_double(),
                          altitude_mean_meters = col_double()
                        )) %>% 
  janitor::clean_names() %>% 
  rename(acidity = salt_acid, sweetness = bitter_sweet,
         aroma = fragrance_aroma, body = mouthfeel, uniformity = uniform_cup)

all_ratings <- bind_rows(raw_arabica, raw_robusta) %>% 
  select(-x1) %>% 
  select(total_cup_points, species, everything()) %>% 
  group_by(country_of_origin) %>% 
  mutate(order = mean(total_cup_points)) %>% 
  arrange(desc(order)) %>% 
  ungroup() %>% 
  mutate(
    country_of_origin = factor(country_of_origin, levels = unique(country_of_origin))
  )%>% 
  mutate_at(c("aroma", "flavor", "aftertaste", "acidity", "body", "balance", "uniformity", "clean_cup", "sweetness", 
            "cupper_points", "moisture", "total_cup_points"), 
          scale) %>% 
  filter(
    total_cup_points > -20,
    !is.na(country_of_origin)
  ) %>% 
  rename("clean-cup" = clean_cup, "cupper-points" = cupper_points) %>% 
  select(c(1, 4, 21:31)) %>% 
  gather(key = "parameter", value = "value", c(3:11)) %>% 
  group_by(country_of_origin, parameter) %>% 
  summarise_all(mean, na.rm = T)
  

countries <- unique(all_ratings$country_of_origin)

for(i in 1:length(countries)){
  
  plot.data <- all_ratings %>% 
    mutate(group = base::ifelse(country_of_origin == countries[i], "red", "grey"),
           group = factor(group, levels = c("red", "grey"))
    ) %>% 
    arrange(desc(group))
  
  title <-  toString(countries[i])
  
  assign(paste0("p", i),
  ggplot(plot.data, aes(x = parameter, y = value, fill = group, colour = group))+
  ggtitle(bquote(atop(.(title))))+
  geom_hline(yintercept=0, colour = "white")+
  geom_point(aes(alpha = group), shape = 21, size = 2)+
  scale_fill_manual(values = c("#93DA49", "#565656"))+
  scale_colour_manual(values = c("#93DA49", "#565656"))+
  scale_alpha_manual(values = c(1, 0.7))+
  scale_y_continuous(breaks = seq(-3, 3, 1))+
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  coord_cartesian(ylim = c(-3, 3), expand = T)+
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(colour = "white", family = "Bahnschrift", size= 15, vjust = -5, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.x = element_text(colour = "white", size = 10, angle = 45, hjust = 1),
    axis.text = element_text(colour = "white", family = "Bahnschrift", size = 12),
    axis.ticks = element_line(colour = "white"),
    axis.line = element_line(colour = "white", size = 1),
    panel.grid.major.x = element_blank(), #   element_line(size = 0.002, linetype = "8f", colour = "#565656"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y =element_line(size = 0.2, colour = "#565656"),
    #panel.background = element_rect(fill = "#393D50"),
    plot.background = element_rect(fill = "#121730", colour = NA),
    plot.margin = unit(c(0.3,1,0.2,1), "cm"),
    panel.border = element_blank()
  )
  )
}

ggsave("plot4.png", (p1|p2|p3|p4|p5|p6)/(p7|p8|p9|p10|p11|p12)/(p13|p14|p15|p16|p17|p18)/
         (p19|p20|p21|p22|p23|p24)/(p25|p26|p27|p28|p29|p30)/(p31|p32|p33|p34|p35|p36),
       type = "cairo", width = 600, height = 600, units = "mm", bg = "transparent")




