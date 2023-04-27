library(tidyverse)
library(httr)
library(rvest)
library(ggplot2)
library(spotifyr)
library(ggridges)
library(ggjoy)

here::here()

##Get access to spotify----
#Read the credentials to get the access token
client_ID <- readLines("credentials/ID.txt", n = 1, warn = FALSE)
api_key <- readLines("credentials/key.txt", n = 1, warn = FALSE)

#set system values - insert information provided by spotify for developers
Sys.setenv(SPOTIFY_CLIENT_ID = client_ID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = api_key)

access_token <- get_spotify_access_token()

#First try to get artist information----
#access the information of the artist
bad_bunny <- get_artist_audio_features("bad bunny")

#retrieve information from one of the songs of Bad Bunny's album "Un verano sin ti"
moscow_mule <- get_track("6Xom58OOXk2SoU711L2IXO")

#Create a plot with Bad Bunny's most danceable songs
#I first filter the data to the most danceable songs. 
#Let's just say his top 10 danceable songs. 
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

#I then create a new graph showing the most danceable song
top_ten_dancegraph <- top_ten_dancegraph + geom_bar(data = subset(top_ten_dancebunny, danceability == highest_score),
                                                    aes(fill = "Highest danceability"), # Set fill color to "Highest"
                                                    stat = "identity") +
  scale_fill_manual(values = c("red", "grey")) # Set color values for fill, grey for other bars and red for the highest bar

print(top_ten_dancegraph)

# save the plot
ggsave("output/plots/top_ten_dancegraph.png", width = 16, height = 9)


## Create graph showing the danceability of the different albums----
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


##Research about the change of the top 100----
#Get different from top 100 playlists in the last 11 years (2022 not available)
top100_2011 <- get_playlist_tracks("37i9dQZF1DXcagnSNtrGuJ")
top100_2012 <- get_playlist_tracks("37i9dQZF1DX0yEZaMOXna3")
top100_2013 <- get_playlist_tracks("37i9dQZF1DX3Sp0P28SIer")
top100_2014 <- get_playlist_tracks("37i9dQZF1DX0h0QnLkMBl4")
top100_2015 <- get_playlist_tracks("37i9dQZF1DX9ukdrXQLJGZ")
top100_2016 <- get_playlist_tracks("37i9dQZF1DX8XZ6AUo9R4R")
top100_2017 <- get_playlist_tracks("37i9dQZF1DWTE7dVUebpUW")
top100_2018 <- get_playlist_tracks("37i9dQZF1DXe2bobNYDtW8")
top100_2019 <- get_playlist_tracks("37i9dQZF1DWVRSukIED0e9")
top100_2020 <- get_playlist_tracks("2fmTTbBkXi8pewbUvG3CeZ")
top100_2021 <- get_playlist_tracks("5GhQiRkGuqzpWZSE7OU4Se")

##OLD Code to create data frame for average time of top 100----------------

#I create one data frame with all the averages over the last 11 years 
average_min_2 <- data.frame(mean(top100_2011$track.duration_ms)) %>% 
  mutate(mean(top100_2012$track.duration_ms)) %>% 
  mutate(mean(top100_2013$track.duration_ms)) %>% 
  mutate(mean(top100_2014$track.duration_ms)) %>% 
  mutate(mean(top100_2015$track.duration_ms)) %>% 
  mutate(mean(top100_2016$track.duration_ms)) %>% 
  mutate(mean(top100_2017$track.duration_ms)) %>% 
  mutate(mean(top100_2018$track.duration_ms)) %>% 
  mutate(mean(top100_2019$track.duration_ms)) %>% 
  mutate(mean(top100_2020$track.duration_ms)) %>% 
  mutate(mean(top100_2021$track.duration_ms))

#I turn the columns into rows
average_min_all <- as.data.frame(t(average_min))

#rename rows
row.names(average_min_all)[row.names(average_min_all) == "mean.top100_2011.track.duration_ms."] <- "1"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2012$track.duration_ms)"] <- "2"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2013$track.duration_ms)"] <- "3"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2014$track.duration_ms)"] <- "4"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2015$track.duration_ms)"] <- "5"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2016$track.duration_ms)"] <- "6"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2017$track.duration_ms)"] <- "7"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2018$track.duration_ms)"] <- "8"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2019$track.duration_ms)"] <- "9"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2020$track.duration_ms)"] <- "10"
row.names(average_min_all)[row.names(average_min_all) == "mean(top100_2021$track.duration_ms)"] <- "11"

#I create an additional column for the years. Makes it easier to create a graph. 
year <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

# Create a data frame with the the two columns
average_min_all <- data.frame(year, average_min_all)

#New Code to create data frame containing the average of the top 100------

#I create one data frame with all the averages over the last 11 years
average_min <- data.frame()

for (i in 2011:2021) {
  df <- data.frame(year = i, mean_duration = mean(get(paste0("top100_", i))$track.duration_ms))
  average_min <- rbind(average_min, df)
}

##Create Graph for average time of top 100------

#rename columns
colnames(average_min)[colnames(average_min) == 'mean_duration'] <- 'average_time'

#I divide now every song by 60'000 to receive the average in Minutes. The average is now in milliseconds.
average_min$average_time <- average_min$average_time / 60000

#Create a graph showing the time averages over the years
ggplot(data = average_min, aes(x = year, y = average_time)) +
  geom_line(colour = "cyan", linetype = 7, linewidth = 3) +
  labs(x = "Year", y = "Average Time", title = "Average time of the top 100 songs over years")

#save the graph
ggsave("output/plots/average_time_top100.png", width = 16, height = 9)


##Code to create data frames for the boxplots of top 100-----
#one data frame with all the track durations
average_min_box <- data.frame(top100_2011$track.duration_ms) %>% 
  mutate(top100_2012$track.duration_ms) %>% 
  mutate(top100_2013$track.duration_ms) %>% 
  mutate(top100_2014$track.duration_ms) %>% 
  mutate(top100_2015$track.duration_ms) %>% 
  mutate(top100_2016$track.duration_ms) %>% 
  mutate(top100_2017$track.duration_ms) %>% 
  mutate(top100_2018$track.duration_ms) %>% 
  mutate(top100_2019$track.duration_ms) %>% 
  mutate(top100_2020$track.duration_ms) %>% 
  mutate(top100_2021$track.duration_ms)

# Old code for renaming columns
#change column names
#colnames(average_min_box)[colnames(average_min_box) == "top100_2011.track.duration_ms"] <- "time_2011"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2012$track.duration_ms"] <- "time_2012"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2013$track.duration_ms"] <- "time_2013"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2014$track.duration_ms"] <- "time_2014"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2015$track.duration_ms"] <- "time_2015"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2016$track.duration_ms"] <- "time_2016"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2017$track.duration_ms"] <- "time_2017"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2018$track.duration_ms"] <- "time_2018"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2019$track.duration_ms"] <- "time_2019"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2020$track.duration_ms"] <- "time_2020"
#colnames(average_min_box)[colnames(average_min_box) == "top100_2021$track.duration_ms"] <- "time_2021"

#New renaming loop 
for (year in 2011:2021) {
  colnames(average_min_box)[colnames(average_min_box) == paste0("top100_", year, "$track.duration_ms")] <- paste0(year)
}

#I need to change one column name manually as it is different
colnames(average_min_box)[colnames(average_min_box) == "top100_2011.track.duration_ms"] <- "2011"

#Divide all the scores to have minutes
average_min_box <- average_min_box / 60000

#Create boxplot to see if there are outliers
boxplot(average_min_box, main="Boxplot Song Duration", xlab="Top 100 Year", ylab="Song duration in minutes")

#I save the graph
ggsave("output/plots/boxplot_time.png")

#Code to see the differences between genres-----
#I scrape spotify to know if there is a differentiation between genres in length
#I only scrape for the most popular genres
rock_classics <- spotifyr::get_playlist_tracks("37i9dQZF1DX4vth7idTQch")
hiphop_classics <- spotifyr::get_playlist_tracks("37i9dQZF1DXawlg2SZawZf")
reggaeton_classics <- spotifyr::get_playlist_tracks("75IFdPYlFXqjpZO4DY2aHK")
dance_classics <- spotifyr::get_playlist_tracks("37i9dQZF1DX8a1tdzq5tbM")
indie_classics <- spotifyr::get_playlist_tracks("37i9dQZF1DX26DKvjp0s9M")
pop_classics <- spotifyr::get_playlist_tracks("13Jd6NS2kvaoVorVFi7luv")

#Create one data frame with all the genres
genre_averages <- data.frame(rock_classics$track.duration_ms) %>% 
  mutate(hiphop_classics$track.duration_ms) %>% 
  mutate(reggaeton_classics$track.duration_ms) %>% 
  mutate(dance_classics$track.duration_ms) %>% 
  mutate(indie_classics$track.duration_ms) %>% 
  mutate(pop_classics$track.duration_ms)

#change column names
colnames(genre_averages)[colnames(genre_averages) == "rock_classics.track.duration_ms"] <- "Rock"
colnames(genre_averages)[colnames(genre_averages) == "hiphop_classics$track.duration_ms"] <- "Hiphop/Rap"
colnames(genre_averages)[colnames(genre_averages) == "reggaeton_classics$track.duration_ms"] <- "Reggaeton"
colnames(genre_averages)[colnames(genre_averages) == "dance_classics$track.duration_ms"] <- "Dance"
colnames(genre_averages)[colnames(genre_averages) == "indie_classics$track.duration_ms"] <- "Indie"
colnames(genre_averages)[colnames(genre_averages) == "pop_classics$track.duration_ms"] <- "Pop"

#Divide all the scores to have minutes
genre_averages <- genre_averages / 60000

#Create boxplot to see how the differentiation between the genres 
boxplot(genre_averages, main="Song duration of classic songs in different genres", xlab="Classic Songs", ylab="Song duration in minutes")

#I save the graph
ggsave("output/plots/boxplot_genre.png")
