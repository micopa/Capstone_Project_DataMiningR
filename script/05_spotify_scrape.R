library(tidyverse)
library(httr)
library(rvest)
library(ggplot2)
library(spotifyr)

#install.packages to use spotify wrapper
install.packages("spotifyr")

#set system values - insert information provided by spotify for developers
Sys.setenv(SPOTIFY_CLIENT_ID = "")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "")

access_token <- get_spotify_access_token()

#access the information of the artist
bad_bunny <- get_artist_audio_features("bad bunny")

#Create a plot with Bad Bunny's most danceable songs
#I first filter the data to the most danceable songs. Songs with a score above 0.8.

danceable_songs <- bad_bunny[bad_bunny$danceability > 0.8, ]


#retrieve information from one of the songs of Bad Bunny's album "Un verano sin ti"
moscow_mule <- get_track("6Xom58OOXk2SoU711L2IXO")
