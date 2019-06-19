# Load the main packages
library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)
library(googlesheets)



# download from the google drive and save in the site folder
DownloadGoogleSheet <- 
  function(TITLE, FOLDER = NA){
    if (is.na(FOLDER)) {
      dt = stringr::word(TITLE, 2)
      folder = case_when(dt == 'Crop' ~ 'AGR',
                         dt == 'Soil' ~ 'SOIL',
                         dt == 'Weather' ~ 'WEATHER',
                         dt %in% c('WQ', 'Tile') ~ 'WATER',
                         TRUE ~ 'OTHER')
    } else {
      folder = FOLDER
    }
    gs_title(TITLE) %>%
      gs_download(to = paste0('Input_Data/', folder, '/', TITLE, '.xlsx' ), 
                  overwrite = TRUE)
    gs_ls(TITLE) %>%
      mutate(downloaded = Sys.time()) %>%
      select(sheet_title, sheet_key, updated, downloaded) %>%
      write_csv('Input_Data/log.csv', append = TRUE)
  }


# read
ReadExcelSheets <-
  function(PATH, GUESS = 30000){
    sheets <- excel_sheets(PATH)
    dl <- vector('list', length = length(sheets))
    for (i in seq_along(sheets)){
      column_names <- read_excel(path = PATH, sheet = i, n_max = 2) %>%
        names()
      dl[[i]] <- read_excel(path = PATH,
                            sheet = i, 
                            col_names = column_names, 
                            cell_limits(c(3, 1), c(NA, length(column_names))), 
                            guess_max = GUESS,
                            na = c('NA', 'N/A', 'n/a', 'dnc', 
                                   'did not collect', 'not collected')) %>%
        mutate(sheet = sheets[i])
    }
    return(dl)
  }


# read soil moisture data
SoilMoistureDataReader <- 
  function(PATH, SKIP) {
    header <- read_excel(path = PATH, n_max = 10, col_names = FALSE) %>%
      filter(X__1 %in% c('Location', 'Distance to surface', 'Hydra Probe #', 'Measurement', 'Unit')) %>%
      t() %>% 
      as_tibble() %>%
      slice(-(1:2)) %>%
      mutate(V1 = str_sub(V1, 2, 2)) %>%
      fill(V1, V2, V3) %>%
      select(location = V1, depth = V2, probe = V3, var = V4, unit = V5) %>%
      mutate(unit = ifelse(str_detect(unit, 'm3'), 'm3/m3', unit),
             var = case_when(unit == 'm3/m3' ~ 'SMwfv',
                             unit == 'oC'    ~ 'Temp',
                             TRUE            ~ var),
             key = paste(var, probe, sep = '_')) %>% 
      pull(key)
    
    data <- read_excel(path = PATH, skip = SKIP, na = c('-9999', 'NAN'),
                       col_names = c('TIMESTAMP', 'RECORD', header),
                       col_types = c('date', rep('numeric', length(header)), 'numeric')) %>%
      gather(key, value, -(TIMESTAMP:RECORD)) %>%
      filter(str_detect(key, 'Temp_|SMwfv_|ECc_'))
    return(data)
  }


# Set up a theme for plotting
theme_gio <-
  theme_light() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(colour = "#666666", size = rel(2), hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "#666666", size = rel(1.5), hjust = 0.5, face = "plain", lineheight = rel(1.1)), 
        plot.caption = element_text(colour = "#666666", size = rel(1.2), hjust = 0.5, face = "plain"),
        axis.title = element_text(colour = "#666666", size = rel(1.2)),
        axis.text.x = element_text(colour = "#757575", size = rel(1)), 
        axis.text.y = element_text(colour = "#757575", size = rel(0.9)), 
        legend.title =  element_text(colour = "#757575", size = rel(1.2)),
        legend.text = element_text(colour = "#757575", size = rel(1)),
        strip.text = element_text(colour = "#666666", hjust = 0.5, face = "bold", size = rel(1)),
        strip.background = element_rect(colour = NA, fill = NA),
        panel.grid.minor = element_blank())

theme_gio2 <-
  theme_light() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(colour = "#666666", size = rel(2), hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "#666666", size = rel(1.5), hjust = 0.5, face = "plain", lineheight = rel(1.1)), 
        plot.caption = element_text(colour = "#666666", size = rel(1.2), hjust = 0.5, face = "plain"), 
        axis.title = element_text(colour = "#666666", size = rel(1.2)),
        axis.text = element_text(colour = "#757575", size = rel(1)), 
        legend.title =  element_text(colour = "#757575", size = rel(1.2)),
        legend.text = element_text(colour = "#757575", size = rel(1)),
        strip.text = element_text(colour = "#666666", hjust = 0.5, face = "bold", size = rel(1)),
        panel.grid.minor = element_blank())
