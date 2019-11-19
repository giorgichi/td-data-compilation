# Initialize functions 
source('00_project_settings.R')
library(googledrive)



# DOWNLOAD ................................................................
# Download all Soil data

sheets <- drive_find(pattern = 'Soil Data', type = 'spreadsheet')

sheets %>%
  filter(str_detect(name, '. Soil Data$')) %>%
  pull(name) -> sheet_names

for (i in sheet_names) {
  DownloadGoogleSheet(TITLE = i, FOLDER = 'SOIL')
}



