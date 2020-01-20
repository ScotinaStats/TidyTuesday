# Load packages and data
library(tidyverse)
library(usmap)

cats_dogs = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-09-11/cats_vs_dogs.csv")
View(cats_dogs)

# Create variable that measures difference in dog vs cat ownership by state
cats_dogs <- cats_dogs %>%
  mutate(dog_cat_diff = percent_dog_owners - percent_cat_owners)

# Map plot
plot_usmap(data = cats_dogs, values = "dog_cat_diff", labels = TRUE, label_color = "white") + 
  scale_fill_continuous(name = "More dogs", low = "purple", high = "lightblue", 
                        breaks = c(10, 0, -10), label = c("10%", "0", "-10%")) + 
  labs(title = "Cats vs. Dogs",
       subtitle = "Difference in % dog vs. cat households (by state)",
       caption = "Data: American Veterinary Medical Association") +
  theme(legend.position = "right", 
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 9, color = "gray50"))


