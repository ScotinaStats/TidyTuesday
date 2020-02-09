# Load packages and data
library(tidyverse)
library(teamcolors)

attendance <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

View(attendance)
View(standings)
View(games)

# Extract game data for Patriots Super Bowl seasons
# Create absolute pt diff, pats win logical variable
games_pats <- games %>%
  filter((home_team == "New England Patriots" | away_team == "New England Patriots") & 
           year %in% c(2001, 2003, 2004, 2014, 2016, 2018)) %>%
  mutate(pt_diff_abs = pts_win - pts_loss, 
         pats_win = (winner == "New England Patriots")) %>%
  left_join(teamcolors[,c("name", "primary")], by = c("winner" = "name"))

# Fun with factors
games_pats <- games_pats %>%
  group_by(year) %>%
  mutate(game = factor(c(1:16, "Div", "Conf", "SB"), 
                       levels = c(c(1:16, "Div", "Conf", "SB")))) %>%
  ungroup()

# Point differential (negative values mean the Patriots lost)
games_pats$pt_diff <- ifelse(games_pats$winner == "New England Patriots", 
                             games_pats$pt_diff_abs, 
                             -games_pats$pt_diff_abs)

# Taking care of colors for teams that changed locations
games_pats$primary[games_pats$winner == "St. Louis Rams"] <- teamcolors$secondary[teamcolors$name == "Los Angeles Rams"]
games_pats$primary[games_pats$winner == "San Diego Chargers"] <- teamcolors$primary[teamcolors$name == "Los Angeles Chargers"]

# Changing color for teams with primary palette that looks similar to Patriots 
games_pats$primary[games_pats$winner == "Cincinnati Bengals"] <- teamcolors$secondary[teamcolors$name == "Cincinnati Bengals"]
games_pats$primary[games_pats$winner == "Pittsburgh Steelers"] <- teamcolors$secondary[teamcolors$name == "Pittsburgh Steelers"]
games_pats$primary[games_pats$winner == "Jacksonville Jaguars"] <- teamcolors$secondary[teamcolors$name == "Jacksonville Jaguars"]
games_pats$primary[games_pats$winner == "Tennessee Titans"] <- teamcolors$secondary[teamcolors$name == "Tennessee Titans"]
games_pats$primary[games_pats$winner == "Denver Broncos"] <- teamcolors$secondary[teamcolors$name == "Denver Broncos"]
games_pats$primary[games_pats$winner == "Tennessee Titans"] <- teamcolors$secondary[teamcolors$name == "Tennessee Titans"]
games_pats$primary[games_pats$winner == "Green Bay Packers"] <- teamcolors$secondary[teamcolors$name == "Green Bay Packers"]
games_pats$primary[games_pats$winner == "Seattle Seahawks"] <- teamcolors$secondary[teamcolors$name == "Seattle Seahawks"]

# (Primary) color palette for winning teams
winner_colors <- games_pats$primary
names(winner_colors) <- games_pats$winner

pats_sb_plot = 
  ggplot(games_pats, aes(x = game, y = pt_diff, fill = winner)) + 
  geom_col() + 
  facet_wrap(~ year, nrow = 2) + 
  theme_bw() + 
  theme(legend.position = "none", 
        plot.title = element_text(size = 18, face = "bold"), 
        plot.subtitle = element_text(size = 12, face = "bold"), 
        plot.caption = element_text(size = 14, face = "italic"), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16, face = "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"), 
        strip.text.x = element_text(size = 14, face = "bold")) + 
  labs(x = "Game", y = "Point Differential", 
       title = "New England Patriots Point Differential (by game)", 
       subtitle = "Super Bowl Seasons", 
       caption = "Data: Pro Football Reference") + 
  scale_x_discrete(limits = rev(levels(games_pats$game))) +
  scale_fill_manual(values = winner_colors) +
  coord_flip()
pats_sb_plot

##########

# Option to make the graphic interactive
library(plotly)
ggplotly(pats_sb_plot)
