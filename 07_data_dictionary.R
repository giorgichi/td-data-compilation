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

# check variables are ordered as in the tbales
col_check <-
  function(FILE) {
    cname <- 
      read_csv(paste0("Ag_Commons_Data/", FILE),
               n_max = 2) %>% names()
    
    res <-
      tibble(file_name = FILE, 
             element_or_value_display_name = cname)
    
    return(res)
  }

files_to_check <-
  data_dictionary %>%
  distinct(file_name) %>%
  pull(file_name)

col_list <- vector("list", length(files_to_check))
for (i in seq_along(files_to_check)) {

  print(files_to_check[i])
  col_list[[i]] <- col_check(files_to_check[i])
  
}

col_list %>%
  bind_rows() %>%
  full_join(data_dictionary) %>%
  write_csv('Ag_Commons_Data/data_dictionary.csv', na = "n/a")

# drive_upload(media = 'Ag_Commons_Data/data_dictionary.csv',
#              path = as_id('1zblZuTiEUdZOq1_IHgO_gEtR018TidRq'), 
#              overwrite = TRUE)
