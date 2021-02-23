library(tidyverse)
library(googlesheets4)



# read data dictionary
dd <- read_sheet(as_sheets_id('1QU858NHlkZ4L23dmAZ7zwFsVdEmmu-vhbF5o1EFk8pw')) %>%
  as_tibble(.name_repair = janitor::make_clean_names)


# format and prepare for publication
data_dictionary <-
  dd %>%
  select(file_name:accepts_null_value, -character_length) %>%
  mutate(acceptable_values = ifelse(data_type == 'double', NA, acceptable_values)) %>%
  ungroup()


write_csv(data_dictionary, 'Ag_Commons_Data/data_dictionary.csv',
          na = "n/a")

# drive_upload(media = 'Ag_Commons_Data/data_dictionary.csv',
#              path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
#              overwrite = TRUE)
