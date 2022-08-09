library(sapfluxnetr)
library(dplyr)
library(purrr)
library(tidyr)


# 1. Read metadata ---------------------------------------------------------------

# Folders
# It assumes that sfn data folder is two levels above working dir

sfn_RData <- file.path('..','..','0.1.5','RData')
sfn_RData_plant <- file.path(sfn_RData,'plant')
sfn_RData_sw <- file.path(sfn_RData,'sapwood')
sfn_RData_leaf <- file.path(sfn_RData,'leaf')

# Create folder for output
out_plant<- file.path('output/0.1.6/RData/plant')
out_sapwood<- file.path('output/0.1.6/RData/sapwood')
out_leaf<- file.path('output/0.1.6/RData/leaf')


# 2. Checks ---------------------------------------------------------------

# 0.1.5
sfn_metadata_plant <- read_sfn_metadata(folder = sfn_RData_plant, 
                                        .write_cache = FALSE)
sfn_metadata_sapwood <- read_sfn_metadata(folder =  sfn_RData_sw, 
                                          .write_cache = FALSE)
sfn_metadata_leaf <- read_sfn_metadata(folder =  sfn_RData_leaf, 
                                       .write_cache = FALSE)

# 0.1.6. Create metadata cache
sfn_metadata_plant_0.1.6 <- read_sfn_metadata(folder = out_plant, 
                                              .write_cache = FALSE)
sfn_metadata_sapwood_0.1.6 <- read_sfn_metadata(folder =  out_sapwood, 
                                                .write_cache = FALSE)
sfn_metadata_leaf_0.1.6 <- read_sfn_metadata(folder =  out_leaf, 
                                                .write_cache = FALSE)

# Check coordinates changes in USA_DUK_HAR

sfn_metadata_plant[['site_md']] %>% 
  filter(si_code=='USA_DUK_HAR') %>% pull(si_lat)
sfn_metadata_plant_0.1.6[['site_md']] %>% 
  filter(si_code=='USA_DUK_HAR') %>% pull(si_lat)


# Check timezones ---------------------------------------------------------

sfn_metadata_plant[['env_md']] %>% 
  select(si_code,env_time_zone) %>% 
  rename(env_time_zone_0_1_5 = env_time_zone) %>% 
  left_join(select(sfn_metadata_plant_0.1.6[['env_md']], si_code,env_time_zone)) %>% View()


get_timezone(read_sfn_data('FRA_PUE',out_plant))
get_timezone(read_sfn_data('ESP_CAN',out_plant))

get_timestamp(read_sfn_data('ESP_CAN',out_plant)) %>% lubridate::tz()
get_timestamp(read_sfn_data('FRA_PUE',out_plant)) %>% lubridate::tz()

# TODO generalize for all data to check timestamps

tictoc::tic()
get_timestamp(read_sfn_data('FRA_PUE',out_plant)) %>% lubridate::tz()
tictoc::toc()


