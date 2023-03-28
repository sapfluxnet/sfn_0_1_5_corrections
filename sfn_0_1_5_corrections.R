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
out_db <- file.path('output/0.1.6')
out_plant<- file.path('output/0.1.6/RData/plant')
out_sapwood<- file.path('output/0.1.6/RData/sapwood')
out_leaf<- file.path('output/0.1.6/RData/leaf')


if(!dir.exists(out_plant)){
    dir.create(out_plant,recursive=TRUE)
} else{
  message('Folder already exists') 
}

if(!dir.exists(out_sapwood)){
  dir.create(out_sapwood,recursive=TRUE)
} else{
  message('Folder already exists') 
}

if(!dir.exists(out_leaf)){
  dir.create(out_leaf,recursive=TRUE)
} else{
  message('Folder already exists') 
}

# Check cache file exists

file.exists(file.path(sfn_RData,'plant','.metadata_cache.RData'))
file.exists(file.path(sfn_RData,'sapwood','.metadata_cache.RData'))

sfn_metadata_plant <- read_sfn_metadata(folder = sfn_RData_plant, 
                                        .write_cache = FALSE)
sfn_metadata_sapwood <- read_sfn_metadata(folder =  sfn_RData_sw, 
                                          .write_cache = FALSE)
sfn_metadata_leaf <- read_sfn_metadata(folder =  sfn_RData_leaf, 
                                          .write_cache = FALSE)

# 2. Metadata wrangling  -----------------------------------------------

# Join all metadata regardless of having sap flow per sapwood or per plant

# Site codes, depending on plant/sapwood levels
sfn_plant_sites<- sfn_metadata_plant[['site_md']] 
sfn_sapwood_sites<- sfn_metadata_sapwood[['site_md']] 
sfn_leaf_sites<- sfn_metadata_leaf[['site_md']] 

# Merging all stand metadata
sfn_allstands<- sfn_metadata_plant[['stand_md']] %>% 
  full_join(sfn_metadata_sapwood[['stand_md']]) %>% arrange(si_code)

# Mergin all plant metadata
sfn_allplants<- sfn_metadata_plant[['plant_md']] %>% 
  full_join(sfn_metadata_sapwood[['plant_md']]) %>% 
  distinct(pl_code,.keep_all = TRUE)

# Species in plant-level datasets
sfn_sitesp_plant <- sfn_metadata_plant[['species_md']] %>% 
  group_by(si_code,sp_name) %>% 
  mutate(n_sp=sum(sp_ntrees))
# Species in sapwod-level datasets
sfn_sitesp_sw <- sfn_metadata_sapwood[['species_md']] %>% 
  group_by(si_code,sp_name) %>% 
  mutate(n_sp=sum(sp_ntrees))

# Codes, sapwood and all
sfn_sw_codes <- sfn_sapwood_sites$si_code %>% 
  setdiff(sfn_plant_sites$si_code)

sfn_all_codes <- sort(c(sfn_plant_sites$si_code, sfn_sw_codes))

# All sites metadata
sfn_all_sites <- sfn_plant_sites %>% 
  bind_rows(sfn_sapwood_sites %>% filter(si_code%in%sfn_sw_codes)) %>% 
  arrange(si_code)


# 3. Read data ---------------------------------------------------------------
# Takes ~ 73 sec in laptop

# plant
sfn_plant_data <- read_sfn_data(sfn_plant_sites$si_code,
                                folder=sfn_RData_plant)

# plant
sfn_sapwood_data <- read_sfn_data(sfn_sapwood_sites$si_code,
                                folder=sfn_RData_sw)

# leaf
sfn_leaf_data <- read_sfn_data(sfn_leaf_sites$si_code,
                                  folder=sfn_RData_sw)

# # sapwood only 
# sfn_sw_data <- read_sfn_data(sfn_sw_codes,
#                              folder=sfn_RData_sw)
# # all
# sfn_all_data<- c(sfn_plant_data,sfn_sw_data)

# Check bad solar timestamps


# 4. Site metadata -----------------------------------------------------------


# USA_DUK_HAR-------------------------------------------------------------

get_site_md(sfn_plant_data[['USA_DUK_HAR']])$si_lat <- 35.9736
get_site_md(sfn_sapwood_data[['USA_DUK_HAR']])$si_lat <- 35.9736

# 4. Stand metadata -------------------------------------------------------


# ESP_RIN -----------------------------------------------------------------

get_stand_md(sfn_plant_data[['ESP_RIN']])$st_basal_area <- 27.46
get_stand_md(sfn_sapwood_data[['ESP_RIN']])$st_basal_area <- 27.46

# CHE_DAV_SEE -------------------------------------------------------------
# Basal area reported by Roman: 45 m2 ha-1

get_stand_md(sfn_plant_data[['CHE_DAV_SEE']])$st_basal_area <- 45
get_stand_md(sfn_sapwood_data[['CHE_DAV_SEE']])$st_basal_area <- 45


# USA_SMI_SER
# stand basal area was 156 m2 ha-1, way too high
# Here it says 40.8 m2 ha-1
# https://serc.si.edu/research/projects/spatial-dimensions-forest-dynamics

get_stand_md(sfn_plant_data[['USA_SMI_SER']])$st_basal_area <- 40.8
get_stand_md(sfn_sapwood_data[['USA_SMI_SER']])$st_basal_area <- 40.8


# USA_MOR_SF  -------------------------------------------------------------

# basal area here: 26.34 m2 ha-1
# https://doi.org/10.1016/S0168-1923(00)00140-4

get_stand_md(sfn_plant_data[['USA_MOR_SF']])$st_basal_area <- 26.34
get_stand_md(sfn_sapwood_data[['USA_MOR_SF']])$st_basal_area <- 26.34

# USA_SYL_HL1 -------------------------------------------------------------
# Use stand composition from USA_SYL_HL2 in HL1

syl_hl1_spbasperc<- sfn_plant_data[['USA_SYL_HL2']]%>% 
  get_species_md() %>% 
  filter(sp_name%in%c('Betula alleghaniensis','Acer saccharum',
                      'Tsuga canadensis')) %>% 
  pull(sp_basal_area_perc)

get_species_md(sfn_sapwood_data[['USA_SYL_HL1']])$sp_basal_area_perc <- syl_hl1_spbasperc


# USA_SYL_HL2  ----------------------------------------------------------
# Use stand basal area from HL1 for HL2 

get_stand_md(sfn_sapwood_data[['USA_SYL_HL2']])$st_basal_area <- 
  get_stand_md(sfn_sapwood_data[['USA_SYL_HL1']])$st_basal_area

get_stand_md(sfn_plant_data[['USA_SYL_HL2']])$st_basal_area <- 
  get_stand_md(sfn_sapwood_data[['USA_SYL_HL2']])$st_basal_area


# USA_CHE_MAP -------------------------------------------------------------
# https://doi.org/10.1029/2010JG001377
# Table 1, tree count and mean dbh per species
# https://doi.org/10.1111/j.1365-2435.2009.01657.x
# Plot size

che_map_abasal <-(454*(pi*(22.13/2)^2)+127*(pi*(38.87/2)^2)+
                    156*(pi*(28.93/2)^2)+10*(pi*(29.06/2)^2))/(132*140)

che_density <- (454+127+156+10)/(132*140)*10000

get_stand_md(sfn_plant_data[['USA_CHE_MAP']])$st_basal_area <- che_map_abasal 
get_stand_md(sfn_plant_data[['USA_CHE_MAP']])$st_density <- che_density


get_stand_md(sfn_sapwood_data[['USA_CHE_MAP']])$st_basal_area <- che_map_abasal 
get_stand_md(sfn_sapwood_data[['USA_CHE_MAP']])$st_density <- che_density



# Species metadata --------------------------------------------------------

# CRI_TAM_TOW -------------------------------------------------------------

# plant
get_species_md(sfn_plant_data[['CRI_TAM_TOW']]) <- 
  get_species_md(sfn_plant_data[['CRI_TAM_TOW']]) %>% 
  mutate(sp_name = case_when(
    sp_name == 'Eschweillera sp.' ~'Eschweilera',
    TRUE ~ sp_name))

# sapwood
get_species_md(sfn_sapwood_data[['CRI_TAM_TOW']]) <- 
  get_species_md(sfn_sapwood_data[['CRI_TAM_TOW']]) %>% 
  mutate(sp_name = case_when(
    sp_name == 'Eschweillera sp.' ~'Eschweilera',
    TRUE ~ sp_name))

# GUF_GUY_GUY -------------------------------------------------------------

# plant
get_species_md(sfn_plant_data[['GUF_GUY_GUY']]) <- 
  get_species_md(sfn_plant_data[['GUF_GUY_GUY']]) %>% 
  mutate(sp_name = case_when(
    sp_name == 'Vacapoua americana' ~ 'Vouacapoua americana',
    TRUE ~ sp_name))

# sapwood
get_species_md(sfn_sapwood_data[['GUF_GUY_GUY']]) <- 
  get_species_md(sfn_sapwood_data[['GUF_GUY_GUY']]) %>% 
  mutate(sp_name = case_when(
    sp_name == 'Vacapoua americana' ~ 'Vouacapoua americana',
    TRUE ~ sp_name))

# IDN_PON_STE -------------------------------------------------------------

# plant
get_species_md(sfn_plant_data[['IDN_PON_STE']]) <- 
  get_species_md(sfn_plant_data[['IDN_PON_STE']]) %>% 
  mutate(sp_name = case_when(
    sp_name == 'Myrtaceae sp.' ~'Myrtaceae',
    TRUE ~ sp_name))

# sapwood
get_species_md(sfn_sapwood_data[['IDN_PON_STE']]) <- 
  get_species_md(sfn_sapwood_data[['IDN_PON_STE']]) %>% 
  mutate(sp_name = case_when(
    sp_name == 'Myrtaceae sp.' ~'Myrtaceae',
    TRUE ~ sp_name))

# MDG_SEM_TAL -------------------------------------------------------------

# plant
get_species_md(sfn_plant_data[['MDG_SEM_TAL']]) <- 
  get_species_md(sfn_plant_data[['MDG_SEM_TAL']]) %>% 
  mutate(sp_name = case_when(
    sp_name == 'Brachulaena ramiflora' ~ 'Brachylaena ramiflora',
    sp_name == 'Cryptocaria spp.' ~ 'Cryptocarya',
    sp_name == 'Eugenia spp.' ~ 'Eugenia',
    TRUE ~ sp_name))

# sapwood
get_species_md(sfn_sapwood_data[['MDG_SEM_TAL']]) <- 
  get_species_md(sfn_sapwood_data[['MDG_SEM_TAL']]) %>% 
  mutate(sp_name = case_when(
    sp_name == 'Brachulaena ramiflora' ~ 'Brachylaena ramiflora',
    sp_name == 'Cryptocaria spp.' ~ 'Cryptocarya',
    sp_name == 'Eugenia spp.' ~ 'Eugenia',
    TRUE ~ sp_name))


# RUS_POG_VAR -------------------------------------------------------------

# plant
get_species_md(sfn_plant_data[['RUS_POG_VAR']]) <- 
  get_species_md(sfn_plant_data[['RUS_POG_VAR']]) %>% 
  mutate(sp_name = case_when(
    sp_name =='Larix sibirica Ledeb.'~'Larix sibirica',
    TRUE ~ sp_name))


# Environmental metadata --------------------------------------------------

# CAN_TUR* ----------------------------------------------------------------

# plant

get_env_md(sfn_plant_data[['CAN_TUR_P39_POS']])$env_time_zone <- '9UTC-05:00, R'
get_env_md(sfn_plant_data[['CAN_TUR_P39_PRE']])$env_time_zone <- '9UTC-05:00, R'
get_env_md(sfn_plant_data[['CAN_TUR_P74']])$env_time_zone <- '9UTC-05:00, R'

# sapwood

get_env_md(sfn_sapwood_data[['CAN_TUR_P39_POS']])$env_time_zone <- '9UTC-05:00, R'
get_env_md(sfn_sapwood_data[['CAN_TUR_P39_PRE']])$env_time_zone <- '9UTC-05:00, R'
get_env_md(sfn_sapwood_data[['CAN_TUR_P74']])$env_time_zone <- '9UTC-05:00, R'



# ESP_TIL_OAK -------------------------------------------------------------

# Time zones

get_env_md(sfn_plant_data[['ESP_TIL_OAK']])$env_time_zone  <- "16UTC±00:00, Z"
get_env_md(sfn_sapwood_data[['ESP_TIL_OAK']])$env_time_zone  <- "16UTC±00:00, Z"
get_env_md(sfn_leaf_data[['ESP_TIL_OAK']])$env_time_zone  <- "16UTC±00:00, Z"


get_timestamp(sfn_plant_data[['ESP_TIL_OAK']]) <- get_timestamp(
  sfn_plant_data[['ESP_TIL_OAK']]) %>% lubridate::force_tz("Etc/GMT+0")
get_timestamp(sfn_sapwood_data[['ESP_TIL_OAK']]) <- get_timestamp(
  sfn_sapwood_data[['ESP_TIL_OAK']]) %>% lubridate::force_tz("Etc/GMT+0")
get_timestamp(sfn_leaf_data[['ESP_TIL_OAK']]) <- get_timestamp(
  sfn_leaf_data[['ESP_TIL_OAK']]) %>% lubridate::force_tz("Etc/GMT+0")


# Solar timestamps --------------------------------------------------------

set_solar_tz<- function(sfn_obj){
  solar_time <- get_solar_timestamp(sfn_obj)
  lubridate::tz(solar_time) <- "UTC"
  get_solar_timestamp(sfn_obj) <- solar_time
  return(sfn_obj)
  
}

# Plant
tz_solar_res <- sapply(
  sfn_plant_data,
  function(x) lubridate::tz(get_solar_timestamp(x)))  

solar_to_fix<- tz_solar_res[which(tz_solar_res=='')] %>% names

# Fix
plant_solar_fixed <- purrr::map(sfn_plant_data[solar_to_fix],
           ~set_solar_tz(.x))

# Sapwood
tz_solar_res_sw <- sapply(
  sfn_sapwood_data,
  function(x) lubridate::tz(get_solar_timestamp(x)))  

solar_to_fix_sw<- tz_solar_res_sw[which(tz_solar_res_sw=='')] %>% names

# Fix
plant_solar_fixed_sw <- purrr::map(sfn_sapwood_data[solar_to_fix_sw],
                                ~set_solar_tz(.x))

# Leaf
tz_solar_res_lf <- sapply(
  sfn_leaf_data,
  function(x) lubridate::tz(get_solar_timestamp(x)))  

solar_to_fix_lf<- tz_solar_res_lf[which(tz_solar_res_lf=='')] %>% names

# Fix
plant_solar_fixed_lf <- purrr::map(sfn_leaf_data[solar_to_fix_lf],
                                   ~set_solar_tz(.x))


# Replace sites with solar timestamps fixed in original lists
sfn_plant_data<- modifyList(sfn_plant_data,plant_solar_fixed)

sfn_sapwood_data<- modifyList(sfn_sapwood_data,plant_solar_fixed_sw)

sfn_leaf_data<- modifyList(sfn_leaf_data,plant_solar_fixed_lf)

# Remove intermediate objects
rm(plant_solar_fixed,plant_solar_fixed_sw,plant_solar_fixed_lf)

# TODO: check IDN_PON_STE when saving sapwood object not found!
# It's present in 0.1.5 sapwood

# Plant metadata ----------------------------------------------------------

#  Species issues ---------------------------------------------------------

# CRI_TAM_TOW -------------------------------------------------------------

# plant
get_plant_md(sfn_plant_data[['CRI_TAM_TOW']]) <- 
  get_plant_md(sfn_plant_data[['CRI_TAM_TOW']]) %>% 
  mutate(pl_species = case_when(
    pl_species == 'Eschweillera sp.' ~'Eschweilera',
    TRUE ~ pl_species))

# sapwood
get_plant_md(sfn_sapwood_data[['CRI_TAM_TOW']]) <- 
  get_plant_md(sfn_sapwood_data[['CRI_TAM_TOW']]) %>% 
  mutate(pl_species = case_when(
    pl_species == 'Eschweillera sp.' ~'Eschweilera',
    TRUE ~ pl_species))

# GUF_GUY_GUY -------------------------------------------------------------

# plant
get_plant_md(sfn_plant_data[['GUF_GUY_GUY']]) <- 
  get_plant_md(sfn_plant_data[['GUF_GUY_GUY']]) %>% 
  mutate(pl_species = case_when(
    pl_species == 'Vacapoua americana' ~ 'Vouacapoua americana',
    TRUE ~ pl_species))

# sapwood
get_plant_md(sfn_sapwood_data[['GUF_GUY_GUY']]) <- 
  get_plant_md(sfn_sapwood_data[['GUF_GUY_GUY']]) %>% 
  mutate(pl_species = case_when(
    pl_species == 'Vacapoua americana' ~ 'Vouacapoua americana',
    TRUE ~ pl_species))

# IDN_PON_STE -------------------------------------------------------------

# plant
get_plant_md(sfn_plant_data[['IDN_PON_STE']]) <- 
  get_plant_md(sfn_plant_data[['IDN_PON_STE']]) %>% 
  mutate(pl_species = case_when(
    pl_species == 'Myrtaceae sp.' ~'Myrtaceae',
    TRUE ~ pl_species))

# sapwood
get_plant_md(sfn_sapwood_data[['IDN_PON_STE']]) <- 
  get_plant_md(sfn_sapwood_data[['IDN_PON_STE']]) %>% 
  mutate(pl_species = case_when(
    pl_species == 'Myrtaceae sp.' ~'Myrtaceae',
    TRUE ~ pl_species))

# MDG_SEM_TAL -------------------------------------------------------------

# plant
get_plant_md(sfn_plant_data[['MDG_SEM_TAL']]) <- 
  get_plant_md(sfn_plant_data[['MDG_SEM_TAL']]) %>% 
  mutate(pl_species = case_when(
    pl_species == 'Brachulaena ramiflora' ~ 'Brachylaena ramiflora',
    pl_species == 'Cryptocaria spp.' ~ 'Cryptocarya',
    pl_species == 'Eugenia spp.' ~ 'Eugenia',
    TRUE ~ pl_species))

# sapwood
get_plant_md(sfn_sapwood_data[['MDG_SEM_TAL']]) <- 
  get_plant_md(sfn_sapwood_data[['MDG_SEM_TAL']]) %>% 
  mutate(pl_species = case_when(
    pl_species == 'Brachulaena ramiflora' ~ 'Brachylaena ramiflora',
    pl_species == 'Cryptocaria spp.' ~ 'Cryptocarya',
    pl_species == 'Eugenia spp.' ~ 'Eugenia',
    TRUE ~ pl_species))

# RUS_POG_VAR -------------------------------------------------------------

# plant
get_plant_md(sfn_plant_data[['RUS_POG_VAR']]) <- 
  get_plant_md(sfn_plant_data[['RUS_POG_VAR']]) %>% 
  mutate(pl_species = case_when(
    pl_species =='Larix sibirica Ledeb.'~'Larix sibirica',
    TRUE ~ pl_species))



# Environmental data ------------------------------------------------------

# ESP_LAS -----------------------------------------------------------------


# Recalculate VPDs from ta and RH because there was a jump in the 
# time series with values too high

get_env_data(sfn_plant_data[['ESP_LAS']])<- 
  get_env_data(sfn_plant_data[['ESP_LAS']]) %>% 
  select(-TIMESTAMP) %>% 
  mutate(vpd=sapfluxnetQC1::qc_vpd(
    data=tibble(ta=get_env_data(sfn_plant_data[['ESP_LAS']])$ta,
                rh=get_env_data(sfn_plant_data[['ESP_LAS']])$rh))$vpd) 

get_env_data(sfn_plant_data[['ESP_LAS']]) %>% pull(vpd) %>% summary


#  Other issues -----------------------------------------------------------


# COL_MAC_SAF_RAD ---------------------------------------------------------

# plant
get_plant_md(
  sfn_plant_data[['COL_MAC_SAF_RAD']])$pl_code <- get_plant_md(
    sfn_plant_data[['COL_MAC_SAF_RAD']])$pl_code %>% 
  stringr::str_replace('COL_MAC_SAF','COL_MAC_SAF_RAD')

# sapwood
get_plant_md(
  sfn_sapwood_data[['COL_MAC_SAF_RAD']])$pl_code <- get_plant_md(
    sfn_sapwood_data[['COL_MAC_SAF_RAD']])$pl_code %>% 
  stringr::str_replace('COL_MAC_SAF','COL_MAC_SAF_RAD')

# leaf
get_plant_md(
  sfn_leaf_data[['COL_MAC_SAF_RAD']])$pl_code <- get_plant_md(
    sfn_leaf_data[['COL_MAC_SAF_RAD']])$pl_code %>% 
  stringr::str_replace('COL_MAC_SAF','COL_MAC_SAF_RAD')


# AUS_WOM -----------------------------------------------------------------

# Sapwood areas need to be recalculated based on sapwood depths

# Retain old sapwood areas in a vector
aus_wom_oldsw<- get_plant_md(sfn_plant_data[['AUS_WOM']])$pl_sapw_area

# Plant
get_plant_md(sfn_plant_data[['AUS_WOM']]) <-  
  get_plant_md(sfn_plant_data[['AUS_WOM']]) %>% 
  mutate(
    pl_sapw_area= pi*(((pl_dbh/2)-(pl_bark_thick*0.1))^2-((pl_dbh/2)-(pl_bark_thick*0.1)-pl_sapw_depth)^2)
  )

# Sapwood
get_plant_md(sfn_sapwood_data[['AUS_WOM']]) <-  
  get_plant_md(sfn_sapwood_data[['AUS_WOM']]) %>% 
  mutate(
    pl_sapw_area= pi*(((pl_dbh/2)-(pl_bark_thick*0.1))^2-((pl_dbh/2)-(pl_bark_thick*0.1)-pl_sapw_depth)^2)
  )

# Changes in sapwood used to recalculate fluxes

# Sap flow per plant
get_sapf_data(sfn_plant_data[['AUS_WOM']])<- get_sapf_data(sfn_plant_data[['AUS_WOM']]) %>% 
  select(-TIMESTAMP) %>% 
  # first divide by old sapwood area and multiply by corrected values
  mutate(across(everything(), ~.x/aus_wom_oldsw*get_plant_md(sfn_plant_data[['AUS_WOM']])$pl_sapw_area))

# Sap flow per sapwood area
get_sapf_data(sfn_sapwood_data[['AUS_WOM']])<- get_sapf_data(sfn_sapwood_data[['AUS_WOM']]) %>% 
  select(-TIMESTAMP) %>% 
  # first multiply by old sapwood area and divide by corrected values
  mutate(across(everything(), ~.x*aus_wom_oldsw/get_plant_md(sfn_sapwood_data[['AUS_WOM']])$pl_sapw_area))


# THA_KHU -----------------------------------------------------------------
# Sapwood areas need to be recalculated based on sapwood depths

# Retain old sapwood areas in a vector
tha_khu_oldsw<- get_plant_md(sfn_plant_data[['THA_KHU']])$pl_sapw_area

# Plant
get_plant_md(sfn_plant_data[['THA_KHU']]) <-  
  get_plant_md(sfn_plant_data[['THA_KHU']]) %>% 
  mutate(
    pl_sapw_area= pi*(((pl_dbh/2)-(pl_bark_thick*0.1))^2-((pl_dbh/2)-(pl_bark_thick*0.1)-pl_sapw_depth)^2)
  )

# Sapwood
get_plant_md(sfn_sapwood_data[['THA_KHU']]) <-  
  get_plant_md(sfn_sapwood_data[['THA_KHU']]) %>% 
  mutate(
    pl_sapw_area= pi*(((pl_dbh/2)-(pl_bark_thick*0.1))^2-((pl_dbh/2)-(pl_bark_thick*0.1)-pl_sapw_depth)^2)
  )


# Changes in sapwood used to recalculate fluxes

# Sap flow per plant
get_sapf_data(sfn_plant_data[['THA_KHU']])<- get_sapf_data(sfn_plant_data[['THA_KHU']]) %>% 
  select(-TIMESTAMP) %>% 
  # first divide by old sapwood area and multiply by corrected values
  mutate(across(everything(), ~.x/tha_khu_oldsw*get_plant_md(sfn_plant_data[['THA_KHU']])$pl_sapw_area))

# Sap flow per sapwood area
get_sapf_data(sfn_sapwood_data[['THA_KHU']])<- get_sapf_data(sfn_sapwood_data[['THA_KHU']]) %>% 
  select(-TIMESTAMP) %>% 
  # first multiply by old sapwood area and divide by corrected values
  mutate(across(everything(), ~.x*tha_khu_oldsw/get_plant_md(sfn_sapwood_data[['THA_KHU']])$pl_sapw_area))


# NLD_SPE_DOU -----------------------------------------------------------------
# Sapwood areas need to be recalculated based on sapwood depths

# Retain old sapwood areas in a vector
nld_spe_dou_oldsw<- get_plant_md(sfn_plant_data[['NLD_SPE_DOU']])$pl_sapw_area

# Plant
get_plant_md(sfn_plant_data[['NLD_SPE_DOU']]) <-  
  get_plant_md(sfn_plant_data[['NLD_SPE_DOU']]) %>% 
  mutate(
    pl_sapw_area= pi*(((pl_dbh/2)-(pl_bark_thick*0.1))^2-((pl_dbh/2)-(pl_bark_thick*0.1)-pl_sapw_depth)^2)
  )

# Sapwood
get_plant_md(sfn_sapwood_data[['NLD_SPE_DOU']]) <-  
  get_plant_md(sfn_sapwood_data[['NLD_SPE_DOU']]) %>% 
  mutate(
    pl_sapw_area= pi*(((pl_dbh/2)-(pl_bark_thick*0.1))^2-((pl_dbh/2)-(pl_bark_thick*0.1)-pl_sapw_depth)^2)
  )

# Changes in sapwood used to recalculate fluxes

# Sap flow per plant
get_sapf_data(
  sfn_plant_data[['NLD_SPE_DOU']])<- get_sapf_data(sfn_plant_data[['NLD_SPE_DOU']]) %>% 
  select(-TIMESTAMP) %>% 
  # first divide by old sapwood area and multiply by corrected values
  mutate(across(everything(), 
                ~.x/nld_spe_dou_oldsw*get_plant_md(sfn_plant_data[['NLD_SPE_DOU']])$pl_sapw_area))

# Sap flow per sapwood area
get_sapf_data(
  sfn_sapwood_data[['NLD_SPE_DOU']])<- get_sapf_data(sfn_sapwood_data[['NLD_SPE_DOU']]) %>% 
  select(-TIMESTAMP) %>% 
  # first multiply by old sapwood area and divide by corrected values
  mutate(across(everything(), 
                ~.x*nld_spe_dou_oldsw/get_plant_md(sfn_sapwood_data[['NLD_SPE_DOU']])$pl_sapw_area))


# Summary: changed sites --------------------------------------------------

si_change <- c('CHE_DAV_SEE','ESP_RIN','ESP_LAS','USA_MOR_SF','USA_SYL_HL1',
               'USA_SYL_HL2', 'USA_CHE_MAP','CRI_TAM_TOW','GUF_GUY_GUY','IDN_PON_STE',
               'MDG_SEM_TAL','RUS_POG_VAR','AUS_WOM','THA_KU','NLD_SPE_DOU',
               'USA_DUK_HAR','COL_MAC_SAF_RAD')

#  Write to folder --------------------------------------------------------

# Highly inefficient as it writes the entire database not only those
# datasets that change. 

# plant
invisible(lapply(names(sfn_plant_data), function(u) {
  assign(u, sfn_plant_data[[u]])
  save(list = u, file=paste0(file.path(out_plant,u),'.RData'))
}))

rm(sfn_plant_data)
gc()

# sapwood
invisible(lapply(names(sfn_sapwood_data), function(u) {
  assign(u, sfn_sapwood_data[[u]])
  save(list = u, file=paste0(file.path(out_sapwood,u),'.RData'))
}))

rm(sfn_sapwood_data)
gc()

# leaf
invisible(lapply(names(sfn_leaf_data), function(u) {
  assign(u, sfn_leaf_data[[u]])
  save(list = u, file=paste0(file.path(out_leaf,u),'.RData'))
}))

rm(sfn_leaf_data)
gc()

# Doesn't work for Rdata objects, it does for RDS

# sfn_plant_data %>% 
#    iwalk(~save(.x,file= paste0(file.path(out_plant,.y),'.RData')))
# 
# # 
# sfn_sapwood_data %>% 
#   iwalk(~save(.x,file= paste0(file.path(out_sapwood,.y),'.RData')))


