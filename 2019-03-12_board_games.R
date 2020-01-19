# Load packages and data
library(tidyverse)
library(patchwork)
library(ggridges)

games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")
View(games)

# Create 'decade' variable
games <- games %>%
  mutate(decade = factor(floor(year_published / 10) * 10)) %>%
  mutate(decade = paste0(decade, "s"))

# Density ridge plots for avg ratings by decade
p1 <- ggplot(games, aes(x = average_rating, y = decade, fill = decade)) + 
  geom_density_ridges(alpha = 0.7) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  labs(x = "Average Rating", y = "", 
       title = "Board Game Ratings (by decade published)") + 
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 9, color = "gray50"))
p1

# Violin and jitter plot for avg ratings in 1980s
set.seed(12) # Set seed for geom_jitter()
p2 <- ggplot(filter(games, decade == "1980s"), aes(x = factor(year_published), y = average_rating)) + 
  geom_violin() + 
  geom_jitter(alpha = 0.3, color = "purple") + 
  labs(x = "", y = "Average Rating", title = "Decade: 1980s") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 11))
p2

# Dot plot for avg ratings in 1989
p3 <- ggplot(filter(games, year_published == 1989), aes(x = average_rating)) + 
  geom_dotplot(fill = "lightblue", dotsize = 1.5, method = 'histodot', alpha = 0.9) + 
  labs(x = "Average Rating", caption = "Data: Board Game Geek", 
       title = "Year: 1989") +
  theme_minimal() + 
  scale_y_continuous(NULL, breaks = NULL) + 
  annotate("text", x = 7.3, y = 0.75, fontface = "italic", 
           color = "gray20", label = glue::glue("Space Hulk")) +
  geom_curve(x = 7.35, xend = 7.46, y = 0.72, yend = 0.1, 
             arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
             color = "gray20", curvature = -0.3) + 
  theme(plot.title = element_text(size = 11), 
        plot.caption = element_text(size = 9, color = "gray50"))
p3

# Patchwork!
p1 | (p2 / p3)
