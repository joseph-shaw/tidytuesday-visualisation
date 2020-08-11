# Tidy Tuesday 2020-08-10 - Avatar
# Script by Joseph Shaw

# Load packages
library(tidyverse)
library(ggtext)
library(extrafont)

# Load data
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

df <- df %>%   
  group_by(book, book_num, chapter, chapter_num, writer, director, imdb_rating) %>% 
  summarise(
    mean_line_length = mean(sapply(strsplit(character_words, " "), length)),
    character_count = length(unique(character))-1
  ) %>% 
  arrange(book_num, chapter_num) %>% 
  mutate(chapter_num = ifelse(book_num == 3, chapter_num+42, ifelse(book_num == 2, chapter_num+21, chapter_num)),
         chapter = gsub(", Part 1", "", chapter),
         chapter = gsub(", Part 2", "", chapter),
         chapter = gsub(", Part 3", "", chapter),
         chapter = gsub(", Part 4", "", chapter)
         )

#Make chapter a factor, outside of pipeline to sort levels
df$chapter <- factor(df$chapter, levels = rev(unique(df$chapter)))
#Fix missing rating
df$imdb_rating <- ifelse(is.na(df$imdb_rating), 9.7, df$imdb_rating)

ggplot(df)+
  geom_point(aes(x = imdb_rating, y = chapter_num, size = character_count), fill = "white", colour = "transparent", shape = 21, alpha = 0.6)+
#Scales  
  scale_size_continuous(range = c(1, 18))+
  scale_alpha_continuous(range = c(0.1, 1))+
  geom_richtext(aes(y = chapter_num, x = 10.05, label = chapter), hjust = 0, fill = "transparent", label.colour = "transparent", size = 2.5, colour = "white", family = "Bahnschrift")+
  geom_segment(aes(x = 7, xend = 10, y = chapter_num, yend = chapter_num), col = "white", alpha = 0.5, size = 0.3 )+
# Mean lines  
  geom_segment(aes(x = mean(df[book_num == 1,]$imdb_rating), xend = mean(df[book_num == 1,]$imdb_rating), y = 0.5, yend = 20.5), col = "white", linetype = "dotted", size = 0.5, alpha = 0.8 )+
  geom_segment(aes(x = mean(df[book_num == 2,]$imdb_rating), xend = mean(df[book_num == 2,]$imdb_rating), y = 21.5, yend = 41.5), col = "white", linetype = "dotted", size = 0.5, alpha = 0.8 )+
  geom_segment(aes(x = mean(df[book_num == 3,]$imdb_rating), xend = mean(df[book_num == 3,]$imdb_rating), y = 42.5, yend = 63.5), col = "white", linetype = "dotted", size = 0.5, alpha = 0.8 )+
#Line lengths
  geom_segment(data = df[df$book_num == 1,], aes(x = mean(imdb_rating)-(mean_line_length/40), xend = mean(imdb_rating)+(mean_line_length/40), y = chapter_num, yend = chapter_num), col = "white", size = 1, alpha = 0.6 )+
  geom_segment(data = df[df$book_num == 2,], aes(x = mean(imdb_rating)-(mean_line_length/40), xend = mean(imdb_rating)+(mean_line_length/40), y = chapter_num, yend = chapter_num), col = "white", size = 1, alpha = 0.6 )+
  geom_segment(data = df[df$book_num == 3,], aes(x = mean(imdb_rating)-(mean_line_length/40), xend = mean(imdb_rating)+(mean_line_length/40), y = chapter_num, yend = chapter_num), col = "white", size = 1, alpha = 0.6 )+
#Axis  
  coord_cartesian(xlim = c(7,11.2), ylim = c(64, 0))+
  labs(
    x =  "IMDB Rating"
  )+
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#FF6B6B", colour = "transparent"),
    plot.background = element_rect(fill = "#FF6B6B", colour = "transparent"),
    legend.position = "none"
  )+
  ggsave("Avatar.png", type = "cairo", height = 360, width = 200, unit = "mm")







