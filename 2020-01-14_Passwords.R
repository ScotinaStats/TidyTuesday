# Load packages and data
library(tidyverse)
library(ggsci)
library(ggthemes)

passwords = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv")
View(passwords)

# Obtain overall mean password strength
strength_avg <- passwords %>%
  summarize(avg = mean(strength, na.rm = T)) %>%
  pull(avg)

# Set seed for geom_jitter()
set.seed(12)

# Strip plot of password strength vs category
passwords %>%
  na.omit() %>%
  group_by(category) %>%
  mutate(strength_avg_cat = mean(strength)) %>%
  ungroup() %>%
  mutate(category = fct_reorder(category, -strength_avg_cat)) %>%
  ggplot(aes(x = category, y = strength, color = category)) + 
  geom_segment(aes(x = category, xend = category, y = strength_avg, yend = strength_avg_cat),
               size = 0.7) +
  geom_hline(aes(yintercept = strength_avg), color = "gray70", size = 0.6) +
  stat_summary(fun.y = mean, geom = "point", size = 5) +
  geom_jitter(alpha = 0.3, size = 2) + 
  theme_minimal() + 
  coord_flip() + 
  annotate("text", x = 10, y = 12.5, fontface = "italic",
           size = 2.7, color = "gray20",
           label = glue::glue("Overall strength:\n{round(strength_avg, 2)}")) +
  geom_curve(x = 9.5, xend = 8.5, y = 12.5, yend = 7.5,
             arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
             color = "gray20", curvature = -0.3) +
  annotate("text", x = 1.75, y = 43, fontface = "italic", 
           size = 2.7, color = "gray20", 
           label = glue::glue("rush2112")) +
  geom_curve(x = 1.75, xend = 1, y = 45.5, yend = 47.5, 
             arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
             color = "gray20", curvature = -0.3) +
  annotate("text", x = 2.25, y = 17, fontface = "italic", 
           size = 2.7, color = "gray20", 
           label = glue::glue("Average strength\n by category")) +
  geom_curve(x = 1.75, xend = 1, y = 17, yend = 15, 
             arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
             color = "gray20", curvature = -0.3) +
  scale_color_simpsons() +
  labs(x = NULL, y = "Password Strength (relative to others)",
       caption = "Data: Information is Beautiful", 
       title = "Which passwords are better?", subtitle = "Password strength vs. category") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 9, color = "gray50"))


