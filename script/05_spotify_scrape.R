library(tidyverse)
library(httr)
library(rvest)
library(ggplot2)
library(spotifyr)
library(ggridges)
library(ggjoy)

install.packages("ggjoy")

#install.packages to use spotify wrapper
install.packages("spotifyr")

#set system values - insert information provided by spotify for developers
Sys.setenv(SPOTIFY_CLIENT_ID = "")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "")

access_token <- get_spotify_access_token()

#access the information of the artist
bad_bunny <- get_artist_audio_features("bad bunny")

#Create a plot with Bad Bunny's most danceable songs
#I first filter the data to the most danceable songs. Let's just say his top 10 danceable songs. 
top_ten_dancebunny <- bad_bunny %>%
  arrange(desc(danceability)) %>%
  slice(1:10)

# Create the plot using ggplot2
top_ten_dancegraph <- ggplot(data = top_ten_dancebunny, aes(x = track_name, y = danceability)) +
  geom_bar(stat = "identity", fill = "grey") + # Set fill color to grey
  xlab("Track Name") +
  ylab("Danceability Score") +
  ggtitle("Top 10 dance tracks from Bad Bunny")

# Find the track with the highest danceability score
highest_score <- max(top_ten_dancebunny$danceability)

top_ten_dancegraph <- top_ten_dancegraph + geom_bar(data = subset(top_ten_dancebunny, danceability == highest_score),
                  aes(fill = "Highest danceability"), # Set fill color to "Highest"
                  stat = "identity") +
  scale_fill_manual(values = c("red", "grey")) # Set color values for fill, grey for other bars and red for the highest bar

print(top_ten_dancegraph)

# save the plot
ggsave("output/plots/top_ten_dancegraph.png", width = 16, height = 9)

#retrieve information from one of the songs of Bad Bunny's album "Un verano sin ti"
moscow_mule <- get_track("6Xom58OOXk2SoU711L2IXO")

#I want to see how the danceabilty of the songs is througout the different albums
#I use the packages ggjoy and ggridges
ggplot(bad_bunny, aes(danceability, album_name))+
  geom_joy()+
  theme_joy()

#I create a better looking graph using geom_density_ridges_gradient
bad_bunny %>% 
  group_by(album_name) %>% 
  ggplot(aes(danceability, album_name, fill = ..x..))+
  geom_density_ridges_gradient()+ 
  xlab("Danceability") +
  ylab("Album")

#I save the graph
ggsave("output/plots/danceability_albums_bunny.png", width = 16, height = 9)
