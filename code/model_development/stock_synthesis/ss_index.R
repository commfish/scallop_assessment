# notes ----
# stock synthesis 3.30 cpue index
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# last updated: 2022/1/3

# load ----
library(tidyverse)

### functions for survey data
source("./code/misc/statewide_scallop_survey_functions.R")

# dredge survey ----

## logbook data
logbook <- read.csv("./data/statewide_scallop_survey/logbook/survey_log_ts_temporary.csv") 

## catch data
catch_raw <- read.csv("./data/statewide_scallop_survey/catch/survey_catch_ts_temporary.csv") 

## tows
tows <- f_clean_log(logbook) 
catch <- f_catch_by_tow(catch_raw, tows)

## remove tow 54 from 2019 (towed twice) and change name of bed (YAKB -> EK1)
filter(tows, tow != 19010054) %>% 
  mutate(bed_code = ifelse((bed_code == "EK1" & lon > -144), "YAKB", bed_code)) -> tows
filter(catch, tow != 19010054) %>%
  mutate(bed_code = ifelse((bed_code == "EK1" & lon > -144), "YAKB", bed_code)) -> catch 

## edit strata so that YAKB gets the code EK1 for consistency
strata %>% # strata loaded in custom functions
  # filter for only active stations
  filter(status == "active") %>%
  ## compute bed area
  group_by(district) %>%
  summarise(area_nm2 = sum(area_nmi2_alb)) -> bed_area


## point estimates (cpue in kg / nmi2)
catch %>%
  filter(rcode == 74120,
         # remove ancillary beds from sheikof estimate
         !(bed_code %in% c("KSH2", "KSH3"))) %>%
  group_by(year, samp_grp, district, bed_code) %>%
  summarise(cpue_abund = mean(cpue_cnt, na.rm = T),
            var_abund = var(cpue_cnt, na.rm = T),
            cpue_biomass = mean(cpue_wt, na.rm = T),
            var_biomass = var(cpue_wt, na.rm = T),
            n = n()) %>%
  mutate(district = ifelse(grepl("KNE", bed_code), "KNE", district)) %>%
  group_by(year, district) %>%
  summarise(cpue_abund = sum(cpue_abund),
            var_abund = sum(var_abund),
            cpue_biomass = sum(cpue_biomass),
            var_biomass = sum(cpue_biomass),
            tot_n = sum(n)) %>%
  left_join(bed_area, by = "district") %>%
  ungroup() %>%
  ## compute abundance and biomass (t)
  mutate(abundance = cpue_abund * area_nm2,
         se_abund = sqrt((var_abund * area_nm2^2) / tot_n),
         biomass = cpue_biomass * area_nm2 * 0.001,
         se_biomass =  sqrt((var_biomass * area_nm2^2 * 0.001^2) / tot_n)) %>%
  dplyr::select(district, year, abundance, se_abund, biomass, se_biomass) -> dredge_index
  
 
# trawl survey ----

## load and clean survey data
source("./code/fishery_independent/trawl_surveys/load_clean_westward_standard_largemesh_data.R")

## standard stations
std_stations <- read_csv("./data/westward_standard_lm_survey/tanner_standard_stations.csv")

wslm_catch %>%
  rename_all(tolower) %>%
  left_join(std_stations, by = "station") -> trawl_catch

## all districts
trawl_catch %>%
  filter(!is.na(scallop_index_dist)) %>%
  ## compute average cpue (wt) per season
  group_by(year, scallop_index_dist) %>%
  summarise(cpue = mean(final_wt_kg / distance_km),
            se = sqrt(var(final_wt_kg / distance_km) /n()),
            cv = se / cpue) %>%
  ungroup -> trawl_index

# save ss input matrix ----

## kamishak
dredge_index %>%
  filter(district == "KAM") %>%
  mutate(seas = 1,
         fleet = 2,
         se_log = log(se_biomass)) %>%
  dplyr::select(year, seas, fleet, biomass, se_log) %>%
  arrange(fleet, year) %>%
  write_csv("./data/model_development/stock_synthesis/ss_kam_dredge_index.csv")

## kodiak shelikof
### fishery cpue
read_csv("./output/observer/2021/standardized_cpue_season_KSH.csv") %>%
  mutate(year = as.numeric(substring(Season, 1, 4))) %>%
  mutate(seas = 1,
         fleet = 1,
         # convert to t per dredge hr
         std_cpue = std_cpue * 0.000453592,
         se_log = log(se * 0.000453592)) %>%
  dplyr::select(year, seas, fleet, std_cpue, se_log) %>%
  arrange(fleet, year) %>%
  write_csv("./data/model_development/stock_synthesis/ss_ksh_fishery_index.csv")
### dredge survey
dredge_index %>%
  filter(district == "KSH") %>%
  mutate(seas = 1,
         fleet = 2,
         se_log = log(se_biomass)) %>%
  dplyr::select(year, seas, fleet, biomass, se_log) %>%
  arrange(fleet, year) %>%
  write_csv("./data/model_development/stock_synthesis/ss_ksh_dredge_index.csv")
### trawl survey
trawl_index %>%
  filter(scallop_index_dist == "KSH") %>%
  mutate(seas = 1,
         fleet = 3,
         se_log = log(se)) %>%
  dplyr::select(year, seas, fleet, cpue, se_log) %>%
  arrange(fleet, year) %>%
  write_csv("./data/model_development/stock_synthesis/ss_ksh_trawl_index.csv")
