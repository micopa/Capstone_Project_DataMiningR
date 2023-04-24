# Global Health Observatory - WHO
# https://www.who.int/data/gho/info/gho-odata-api
library(tidyverse)
library(httr)
library(rvest)

# Get the list with all the variables that are available (indicators)
#  https://www.who.int/data/gho/data/indicators
indicators <-  httr::GET(
  url = 'https://ghoapi.azureedge.net/api/Indicator',
  verbose()
)

content_indicators <- content(indicators, as = "parsed")
Indicators_tibble <- tibble(
  code = map_chr(content_indicators$value, 1), 
  name = map_chr(content_indicators$value, 2), 
  language = map_chr(content_indicators$value, 3) 
)

# Download data from one indicator (Ambient air pollution attributable deaths): 
Indicators_tibble[Indicators_tibble$code == "AIR_41",]
response <-  httr::GET(
  url = 'https://ghoapi.azureedge.net/api/AIR_41',
  verbose()
)

cnt_air41 <- content(response, as = "parse")

data_deaths <- tibble(
  id = map_chr(cnt_air41$value, 1), 
  country = map_chr(cnt_air41$value, 4), 
  year = map_dbl(cnt_air41$value, 6), 
  value = map_dbl(cnt_air41$value, ~(.x)$NumericValue)
)
