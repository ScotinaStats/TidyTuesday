# Load packages and data
library(tidyverse)
#library(vapoRwave)
library(tm)
library(wordcloud)

spotify_songs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
View(spotify_songs)


# Speechiness vs Danceability 
ggplot(spotify_songs, aes(x = speechiness, y = danceability, color = playlist_genre)) + 
  geom_point(size = 0.5, alpha = 0.2) + 
  labs(x = "Speechiness", y = "Danceability", color = "Playlist Genre", 
       caption = "Data: spotifyr") +
  theme_minimal() + 
  xlim(0, 1) + 
  guides(colour = guide_legend(override.aes = list(size = 1, alpha = 1))) +
  #scale_color_manual(values = vapoRwave:::hyperBubble_palette[1:6], 
  #                   labels = c("EDM", "Latin", "Pop", "R&B", "Rap", "Rock")) +
  scale_color_discrete(labels = c("EDM", "Latin", "Pop", "R&B", "Rap", "Rock")) +
  theme(plot.title = element_text(size = 11), 
        plot.caption = element_text(size = 9, color = "gray50"))


# Looking at songs with 'top 10' popularity
spotify1 = spotify_songs %>%
  filter(track_popularity %in% 1:10) 

# Work with titles of top songs
# Convert to corpus
titles <- with(spotify1, VCorpus(VectorSource(track_name)))

# Remove a bunch of things, including some words I chose in the last line
titles <- titles %>%
  tm_map(stripWhitespace) %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, c("feat", "version", "remastered", "radio", "single", 
                        "edit", "mix", "remix", "original", "album"))

# Wordcloud!
wordcloud(titles, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
