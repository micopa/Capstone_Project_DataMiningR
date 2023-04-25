library(tidyverse)
library(httr)
library(rvest)
library(here)
library(purr)
install.packages()


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

#I save the data frame
save(data_WB, file = "output/tables/energy_water_pdf.RData")


#Search the WORLDBANK API for Documents for the country Spain and that include the term "water" in the title
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

#I save the data frame 
save(data_spain, file = "output/tables/spain_water_pdf.RData")

#Search the WORLDBANK API for Documents for the country Germany and that include the term "water" in the title
response_germany <- httr::GET("https://search.worldbank.org/api/v2/wds?format=json&count_exact=Germany&display_title=water&fl=display_title")
content_germany <- content(response_germany, as = "parsed")

#I delete the sublist again so I can create a data frame with the tibble function
content_germany <- content_germany$documents[-3]

# I create a data frame with ID, TITLE and PDF URL of the documents
data_germany <- tibble(
  ID = map_chr(content_germany, 1), 
  TITLE = map_chr(content_germany, 4),
  PDF = map_chr(content_germany, 5))

#I save the data frame 
save(data_germany, file = "output/tables/germany_water_pdf.RData")

#Download the pdf's

# Set the directory where you want to save the PDF files
pdf_dir <- "output/pdf/"

# Download the PDF files and save them to the specified directory
for (i in seq_along(data_spain$PDF)) {
  # Get the filename from the URL
  filename <- basename(data_spain$PDF[i])
  
  # Build the output path for the file
  output_path <- file.path(pdf_dir, filename)
  
  # Download the file from the URL and save it to the output path
  GET(data_spain$PDF[i], write_disk(output_path))
}


pdf_url <- "http://documents.worldbank.org/curated/en/740231468299931915/pdf/717440WP0Box370nNovember72011CYB0EE.pdf"
download.file(pdf_url, dest_path, mode = "wb")
dest_path <- "output/pdf/spain_water1.pdf"
