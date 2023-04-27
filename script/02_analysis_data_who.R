library(tidyverse)
library(httr)
library(rvest)
library(ggplot2)

here::here()

# As stated in the scrape script filtering of the DATA using API was not possible
# so I follow from here on with the data files provided directly from the WHO DATA site

data_who_air_death_total<- load("data/data_who_air_total.Rdata")

data_who_air_death_total_2 <- select(data_who_air_death_total, "Location", "FactValueNumeric")

# For my interest are only Spain, Portugal, Italy, Germany, France, Sweden and Norway

# Create a vector with the countries to include
countries <- c("Spain", "Portugal", "Italy", "Germany", "France", "Sweden", "Norway")

# Use the subset() function to filter the data frame
filter_air <- subset(data_who_air_death_total_2, Location %in% countries)

# I create an overview with the different countries
death_plot <- ggplot(filter_air, aes(x = Location, y = FactValueNumeric)) +
  geom_bar(stat = "identity")+
  ggtitle("Total deaths due to ambient polution")

# print the plot
print(death_plot)

# save the plot
ggsave("output/plots/death_europe.png", width = 16, height = 9)

