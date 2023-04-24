library(tidyverse)
library(httr)
library(rvest)
library(here)
library(purr)


here::here()

#WORLDBANK API

#I search the WORLDBANK API for Documents regarding the topic energy and that include the term "water" in the title
response <- httr::GET("https://search.worldbank.org/api/v2/wds?format=json&qterm=energy&display_title=water&fl=display_title")
content_WB <- content(response, as = "parsed")

#I know want to have the necessary information such as document ID, Title and the direct PDF link in a data frame
data_WB <- tibble(
  ID = map_chr(content_WB$documents, 1), 
  TITLE = map_chr(content_WB$documents, 4), 
  PDF = map_chr(content_WB$documents, 5) 
)

#the code doesn't work as it contains a sublist with 0 
#Error in `map_chr()`:
#i In index: 11.
#i With name: facets.
#Caused by error:
  #! Result must be length 1, not 0.
#Run `rlang::last_error()` to see where the error occurred.

#I delete the sublist so I can create a data frame with the tibble function
content_WB <- content_WB$documents[-11]

# I create a data frame with ID, TITLE and PDF URL of the documents
data_WB <- tibble(
  ID = map_chr(content_WB, 1), 
  TITLE = map_chr(content_WB, 4), 
  PDF = map_chr(content_WB, 5) 
)

#I save the data frame as a CVS file
save(data_WB, file = "energy_water_pdf.RData")


#Search the WORLDBANK API for Documents for the country spain and that include the term "water" in the title
response_spain <- httr::GET("https://search.worldbank.org/api/v2/wds?format=json&count_exact=Spain&display_title=water&fl=display_title")
content_spain <- content(response_spain, as = "parsed")

#I tried to first search for the keywords "energy", "fossil" and "wind turbines" but i didn't find any results

#I delete the sublist so I can create a data frame with the tibble function
content_spain <- content_spain$documents[-8]

# I create a data frame with ID, TITLE and PDF URL of the documents
data_spain <- tibble(
  ID = map_chr(content_spain, 1), 
  TITLE = map_chr(content_spain, 4), 
  PDF = map_chr(content_spain, 5))

#I save the data frame as a CVS file
save(data_WB, file = "spain_water_pdf.RData")


