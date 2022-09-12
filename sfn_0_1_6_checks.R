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
                                              .write_cache = TRUE)
sfn_metadata_sapwood_0.1.6 <- read_sfn_metadata(folder =  out_sapwood, 
                                                .write_cache = TRUE)
sfn_metadata_leaf_0.1.6 <- read_sfn_metadata(folder =  out_leaf, 
                                                .write_cache = TRUE)

# Read data 0.1.6 ---------------------------------------------------------


# plant
sfn_plant_data_0.1.6 <- read_sfn_data(sfn_metadata_plant_0.1.6[['site_md']]$si_code,
                                folder=out_plant)

# plant
sfn_sapwood_data_0.1.6 <- read_sfn_data(sfn_metadata_sapwood_0.1.6[['site_md']]$si_code,
                                  folder=out_plant)

# leaf
sfn_leaf_data_0.1.6 <- read_sfn_data(sfn_metadata_leaf_0.1.6[['site_md']]$si_code,
                               folder=out_plant)


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


# get_timezone(read_sfn_data('FRA_PUE',out_plant))
# get_timezone(read_sfn_data('ESP_CAN',out_plant))


# Get timezones from metadata using get_timezone  -------------------------

# tz_md
sfn_plant_0.1.6_tz1 <- sfn_plant_data_0.1.6 %>% 
imap(~get_timezone(.x)) %>% 
  imap_dfr(~bind_cols(si_code=.y,tz_md=.x)) 

sfn_sapwood_0.1.6_tz1 <- sfn_sapwood_data_0.1.6 %>% 
  imap(~get_timezone(.x)) %>% 
  imap_dfr(~bind_cols(si_code=.y,tz_md=.x)) 

sfn_leaf_0.1.6_tz1 <- sfn_leaf_data_0.1.6 %>% 
  imap(~get_timezone(.x)) %>% 
  imap_dfr(~bind_cols(si_code=.y,tz_md=.x)) 


# Get timezones from time series using get_timestamp ----------------------


# get_timestamp(read_sfn_data('ESP_CAN',out_plant)) %>% lubridate::tz()
# get_timestamp(read_sfn_data('FRA_PUE',out_plant)) %>% lubridate::tz()

# tz_ts
sfn_plant_0.1.6_tz2 <- sfn_plant_data_0.1.6 %>% 
  imap(~lubridate::tz(get_timestamp(.x))) %>% 
  imap_dfr(~bind_cols(tz_ts=.x,si_code=.y)) 

sfn_sapwood_0.1.6_tz2 <- sfn_sapwood_data_0.1.6 %>% 
  imap(~lubridate::tz(get_timestamp(.x))) %>% 
  imap_dfr(~bind_cols(tz_ts=.x,si_code=.y)) 

sfn_leaf_0.1.6_tz2 <- sfn_leaf_data_0.1.6 %>% 
  imap(~get_timezone(.x)) %>% 
  imap_dfr(~bind_cols(tz_ts=.x,si_code=.y)) 


# Combine -----------------------------------------------------------------

sfn_plant_0.1.6_tz1 %>% 
  bind_cols(select(sfn_plant_0.1.6_tz2,-si_code)) %>% View()

# Function to replace metadata tz by ts tz --------------------------------



tzts_to_tzmd<- function(sfn_object)
  {
  if(is.null(get_timezone(sfn_object))){
    get_timezone(sfn_object) <- lubridate::tz(get_timestamp(sfn_object))
  } else {
    message(paste0('Time zone present ',get_timezone(sfn_object)))
  }
  
}

tzts_to_tzmd(sfn_plant_data_0.1.6[['ESP_CAN']])

# TODO generalize for all data to check timestamps

tictoc::tic()
get_timestamp(read_sfn_data('ESP_CAN',out_plant)) %>% lubridate::tz()
tictoc::toc()


