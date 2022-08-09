## libraries
library(tidyverse)
library(sapfluxnetr)

# 1. Function sfndata to csv ----------------------------------------------------------

sfn2csv <- function(si_code, where, folder) {
  
  sfn_data <- sapfluxnetr::read_sfn_data(si_code, where)
  sapf_data <- sapfluxnetr::get_sapf_data(sfn_data) %>%
    dplyr::mutate(solar_TIMESTAMP = sapfluxnetr::get_solar_timestamp(sfn_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  sapf_flags <- sapfluxnetr::get_sapf_flags(sfn_data) %>%
    dplyr::mutate(solar_TIMESTAMP = sapfluxnetr::get_solar_timestamp(sfn_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  env_data <- sapfluxnetr::get_env_data(sfn_data) %>%
    dplyr::mutate(solar_TIMESTAMP = sapfluxnetr::get_solar_timestamp(sfn_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  env_flags <- sapfluxnetr::get_env_flags(sfn_data) %>%
    dplyr::mutate(solar_TIMESTAMP = sapfluxnetr::get_solar_timestamp(sfn_data)) %>%
    dplyr::select(TIMESTAMP, solar_TIMESTAMP, dplyr::everything())
  site_md <- sapfluxnetr::get_site_md(sfn_data)
  stand_md <- sapfluxnetr::get_stand_md(sfn_data)
  species_md <- sapfluxnetr::get_species_md(sfn_data)
  plant_md <- sapfluxnetr::get_plant_md(sfn_data)
  env_md <- sapfluxnetr::get_env_md(sfn_data)
  
  sapf_data_name <- file.path(folder, paste0(si_code, '_sapf_data.csv'))
  env_data_name <- file.path(folder, paste0(si_code, '_env_data.csv'))
  sapf_flags_name <- file.path(folder, paste0(si_code, '_sapf_flags.csv'))
  env_flags_name <- file.path(folder, paste0(si_code, '_env_flags.csv'))
  site_md_name <- file.path(folder, paste0(si_code, '_site_md.csv'))
  stand_md_name <- file.path(folder, paste0(si_code, '_stand_md.csv'))
  species_md_name <- file.path(folder, paste0(si_code, '_species_md.csv'))
  plant_md_name <- file.path(folder, paste0(si_code, '_plant_md.csv'))
  env_md_name <- file.path(folder, paste0(si_code, '_env_md.csv'))
  
  readr::write_csv(sapf_data, sapf_data_name)
  readr::write_csv(env_data, env_data_name)
  readr::write_csv(sapf_flags, sapf_flags_name)
  readr::write_csv(env_flags, env_flags_name)
  readr::write_csv(site_md, site_md_name)
  readr::write_csv(stand_md, stand_md_name)
  readr::write_csv(species_md, species_md_name)
  readr::write_csv(env_md, env_md_name)
}


# 2. Folders ------------------------------------------------------------------

# Create folder for output
out_db <- file.path('output/0.1.6')
out_plant<- file.path('output/0.1.6/RData/plant')
out_sapwood<- file.path('output/0.1.6/RData/sapwood')
out_leaf<- file.path('output/0.1.6/RData/leaf')

# csv
out_plant_csv<- file.path('output/0.1.6/csv/plant')
out_sapwood_csv<- file.path('output/0.1.6/csv/sapwood')
out_leaf_csv<- file.path('output/0.1.6/csv/leaf')


if(!dir.exists(out_plant_csv)){
  dir.create(out_plant,recursive=TRUE)
} else{
  message('Folder already exists') 
}

if(!dir.exists(out_sapwood_csv)){
  dir.create(out_sapwood_csv,recursive=TRUE)
} else{
  message('Folder already exists') 
}

if(!dir.exists(out_leaf_csv)){
  dir.create(out_leaf_csv,recursive=TRUE)
} else{
  message('Folder already exists') 
}

# 3. Write CSVs --------------------------------------------------------------

# leaf folder

leaf_si_codes <- list.files(out_leaf, '.RData') %>% stringr::str_remove('.RData')
leaf_lapply <- lapply(leaf_si_codes, sfn2csv, where= out_leaf, folder = out_leaf_csv)

sapwood_si_codes <- list.files(out_sapwood, '.RData') %>% stringr::str_remove('.RData')
sapwood_lapply <- lapply(sapwood_si_codes, sfn2csv, where = out_sapwood, folder = out_sapwood_csv)

plant_si_codes <- list.files(plant_folder, '.RData') %>% stringr::str_remove('.RData')
plant_lapply <- lapply(plant_si_codes, sfn2csv, where = out_plant, folder = out_plant_csv)

# 4. Fix timezones in csv's -----------------------------------------------

## This script is based on the 0.1.5 version, to fix the regression error on the
## csv files.
## Sites affected:
##    - ESP_YUN_T3_THI    - COL_MAC_SAF_RAD   - AUS_MAR_HSW_HIG
##    - USA_SMI_SER   - USA_SMI_SCB   - ZAF_SOU_SOU
##    - ZAF_RAD - ZAF_FRA_FRA - RUS_FYO - IDN_JAM_RUB - IDN_JAM_OIL
##    - ESP_LAS- USA_WVF - USA_TNB - USA_TNO - USA_TNP - USA_TNY - USA_PAR_FER
##    - USA_INM - COL_MAC_SAF_RAD



#  4.1. Function to fix individual file -----------------------------------

individual_file_fix <- function(filename) {
  
  # check file presence
  if (file.exists(filename)) {
    # if file exists, load it
    table_bad <- read.csv(filename)
    # if file has bad timestamp name, change it
    if (!is.null(table_bad$TIMESTAMP_solar)) {
      table_bad %>%
        rename(solar_TIMESTAMP = TIMESTAMP_solar) %>%
        readr::write_csv(filename)
      return(glue::glue("{filename} fixed succesfully"))
    } else {
      return(glue::glue("{filename} does not have bad solar timestamp"))
    }
  } else {
    return(glue::glue("{filename} does not exist"))
  }
}

# 4.2. Function to run timezone fixes across folders ----------------------

csv_fixes <- function(db_folder, site) {
  
  # get the folders
  folders <- c(
    file.path(db_folder, 'csv', 'plant'),
    file.path(db_folder, 'csv', 'sapwood'),
    file.path(db_folder, 'csv', 'leaf')
  )
  
  # get the file names
  names <- glue::glue(
    "{site}_{c('sapf_data.csv', 'sapf_flags.csv', 'env_data.csv', 'env_flags.csv')}"
  )
  
  # build the routes
  files_to_check <-
    folders %>%
    purrr::map(~ glue::glue("{.x}/{names}")) %>%
    purrr::flatten_chr()
  
  # execute the fixes for each file
  res <- files_to_check %>%
    purrr::map(
      ~ individual_file_fix(.x)
    )
  
  return(res)
  
}

# 5. Running timezone fixes in csv's --------------------------------------

## execute the fixes, 'ESP_PRA' removed

c(
  "ESP_YUN_T3_THI", "COL_MAC_SAF_RAD", "AUS_MAR_HSW_HIG", "USA_SMI_SER", "USA_SMI_SCB",
  "ZAF_SOU_SOU", "ZAF_RAD", "ZAF_FRA_FRA", "RUS_FYO", "IDN_JAM_RUB", "IDN_JAM_OIL",
  "ESP_LAS", "USA_WVF", "USA_TNB", "USA_TNO", "USA_TNP", "USA_TNY", "USA_PAR_FER",
  "USA_INM", "COL_MAC_SAF_RAD"
) %>%
  purrr::map(~ csv_fixes(out_db, .x)) -> res

## check results
res
