library(tidyverse)
library(httr)
library(rvest)
library(here)

# Global Health Observatory - WHO
# https://www.who.int/data/gho/info/gho-odata-api
# Get the list with all the variables that are available (indicators)
#  https://www.who.int/data/gho/data/indicators
response <-  httr::GET(
  url = 'https://ghoapi.azureedge.net/api/Indicator',
  verbose()
)

cnt <- content(response, as = "parsed")
dat <- tibble(
  code = map_chr(cnt$value, 1), 
  name = map_chr(cnt$value, 2), 
  language = map_chr(cnt$value, 3) 
)

# Download data from one indicator (Alcohol consumption per capita): 
dat[dat$code == "SA_0000001400",]
response <-  httr::GET(
  url = 'https://ghoapi.azureedge.net/api/SA_0000001400',
  verbose()
