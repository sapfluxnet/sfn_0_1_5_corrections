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
                                  folder=out_sapwood)

# leaf
sfn_leaf_data_0.1.6 <- read_sfn_data(sfn_metadata_leaf_0.1.6[['site_md']]$si_code,
                               folder=out_leaf)


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


#  Fix timezones ----------------------------------------------------------

timezone_fix<- function (tz) 
{
  if (is.na(tz) | is.null(tz)) {
    stop("Timezone not provided in environmental metadata")
  }
  timezones <- list(`1UTC-12:00, Y` = "Etc/GMT+12", `2UTC-11:00, X` = "Etc/GMT+11", 
                    `3UTC-10:00, W` = "Etc/GMT+10", `4UTC-09:30, V\\u2020` = "Pacific/Marquesas", 
                    `5UTC-09:00, V` = "Etc/GMT+9", `6UTC-08:00, U` = "Etc/GMT+8", 
                    `7UTC-07:00, T` = "Etc/GMT+7", `8UTC-06:00, S` = "Etc/GMT+6", 
                    `9UTC-05:00, R` = "Etc/GMT+5", `11UTC-04:00, Q` = "Etc/GMT+4", 
                    `12UTC-03:30, P\\u2020` = "Canada/Newfoundland", `13UTC-03:00, P` = "Etc/GMT+3", 
                    `14UTC-02:00, O` = "Etc/GMT+2", `15UTC-01:00, N` = "Etc/GMT+1", 
                    `16UTC±00:00, Z` = "Etc/GMT+0", `17UTC+01:00, A` = "Etc/GMT-1", 
                    `18UTC+02:00, B` = "Etc/GMT-2", `19UTC+03:00, C` = "Etc/GMT-3", 
                    `20UTC+03:30, C\\u2020` = "Asia/Tehran", `21UTC+04:00, D` = "Etc/GMT-4", 
                    `22UTC+04:30, D\\u2020` = "Asia/Kabul", `23UTC+05:00, E` = "Etc/GMT-5", 
                    `24UTC+05:30, E\\u2020` = "Asia/Kolkata", `25UTC+05:45, E*` = "Asia/Katmandu", 
                    `26UTC+06:00, F` = "Etc/GMT-6", `27UTC+06:30, F\\u2020` = "Indian/Cocos", 
                    `28UTC+07:00, G` = "Etc/GMT-7", `29UTC+08:00, H` = "Etc/GMT-8", 
                    `30UTC+08:30, H\\u2020` = "Asia/Pyongyang", `31UTC+08:45, H*` = "Australia/Eucla", 
                    `32UTC+09:00, I` = "Etc/GMT-9", `33UTC+09:30, I\\u2020` = "Australia/Adelaide", 
                    `34UTC+10:00, K` = "Etc/GMT-10", `35UTC+10:30, K\\u2020` = "Australia/Lord_Howe", 
                    `36UTC+11:00, L` = "Etc/GMT-11", `37UTC+12:00, M` = "Etc/GMT-12", 
                    `38UTC+12:45, M*` = "Pacific/Chatham", `39UTC+13:00, M\\u2020` = "Etc/GMT-13", 
                    `40UTC+14:00, M\\u2020` = "Etc/GMT-14")
  return(timezones[[as.character(tz)]])
}

sfn_plant_0.1.6_tz2$tz_ts[sfn_plant_0.1.6_tz2$si_code=='ARG_MAZ']
sfn_plant_0.1.6_tz1$tz_md[sfn_plant_0.1.6_tz1$si_code=='ARG_MAZ']
sfn_plant_data_0.1.6[['ARG_MAZ']] %>% get_timezone
sfn_plant_data_0.1.6[['ARG_MAZ']] %>% get_env_md %>% pull(env_time_zone)

sfn_plant_0.1.6_tz2$tz_ts[sfn_plant_0.1.6_tz2$si_code=='ESP_CAN']
sfn_plant_0.1.6_tz1$tz_md[sfn_plant_0.1.6_tz1$si_code=='ESP_CAN']
sfn_plant_data_0.1.6[['ESP_CAN']] %>% get_timezone

sfn_plant_data_0.1.6[['CAN_TUR_P39_POS']] %>% get_env_md %>% pull(env_time_zone)

sfn_plant_0.1.6_tz2$tz_ts[sfn_plant_0.1.6_tz2$si_code=='ESP_CAN']


foo <- sfn_plant_data_0.1.6[['ESP_CAN']]



get_env_md(foo)$env_time_zone <- sfn_plant_0.1.6_tz2$tz_ts[sfn_plant_0.1.6_tz2$si_code=='ESP_CAN']

get_timezone(foo)



foo %>% get_timezone

foo$

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


