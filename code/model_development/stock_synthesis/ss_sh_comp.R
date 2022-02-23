# notes ----
## prepare shell height composition for stock synthesis models
## Tyler Jackson
## tyler.jackson@alaska.gov

# load ----
library(tidyverse)

## functions for observer data mgmt 
source("./code/misc/general_observer_data_functions.R")

# custom function to correct timeseries stations for trawl survey
f_timeseries_station_correct <- function(data, select, station_metadata){
  
  # data for function
  std_stations <- station_metadata
  
  # change data object name to avoiding masking
  dat = data
  
  # minor function needed within function
  ## pull mean from reference data 
  f_pmfd <- function(input, data, select) {
    # change names to avoid masking
    input %>%
      rename(stn = station, 
             yr = year) -> input
    
    # fill in data for stations two in a given year
    if(input$stn %in% (data %>% filter(year == input$yr) %>% pull(station))) {
      data %>%
        filter(year == input$yr, station == input$stn) %>%
        pull(select) %>%
        .[1] -> out
    }
    # fill in data for stations that need avg from references in given year
    if(!(input$stn %in% (data %>% filter(year == input$yr) %>% pull(station)))) {
      data %>%
        filter(year == input$yr, station %in% c(input$stn, input$reference_station, input$reference_station2, input$reference_station3)) %>%
        pull(select) %>%
        mean -> res
      out <- ifelse(is.nan(res), NA, res)
      
    }
    
    # return 
    return(out)
  }
  
  expand_grid(year = min(data$year):max(data$year), std_stations) %>%
    # remove the text stations
    dplyr::select(-district, -section, -subsection) %>%
    # create dummy column year/station
    mutate(yrstn = paste(year, station, sep = "_")) %>%
    group_by(yrstn) %>%
    nest %>%
    mutate(select = purrr::map_dbl(data, f_pmfd, data = dat, select = select)) %>%
    unnest() %>%
    ungroup() %>%
    dplyr::select(year, station, select) -> out
  
  names(out)[3] <- select
  
  return(out)
  
}

# data ----

## dredge specimen data
dredge_shad <- read.csv("./data/statewide_scallop_survey/specimen/survey_specimen_ts_temporary.csv")

## catch data (for expansion)
dredge_catch_raw <- read.csv("./data/statewide_scallop_survey/catch/survey_catch_ts_temporary.csv")

## tow code rosetta stone
tow_code <- read.csv("./data/statewide_scallop_survey/temp_tow_to_event_id.csv")

## fishery age data from adu 
fishery_shad <- lapply(list.files("data/observer/shell_height/", pattern = "ShellHeights", full.names = T), read_csv) %>% do.call("bind_rows", .)

## fishery catch 2009 - present
fishery_catch <- lapply(list.files("data/observer/catch/", pattern = "CatchByHaul", full.names = T), read_csv) %>% do.call("bind_rows", .)

## fishery discards 2009 - present
fishery_discards <- lapply(list.files("data/observer/bycatch/", pattern = "BycatchByHaul", full.names = T), read_csv) %>% do.call("bind_rows", .)

## trawl survey specimen
trawl_spec <- lapply(list.files("data/westward_standard_lm_survey/specimen_dump", pattern = "specimenDump", full.names = T), read_csv) %>% do.call("bind_rows", .)

## trawl survey catch
trawl_catch <- lapply(list.files("data/westward_standard_lm_survey/catch_dump", pattern = "catchDump", full.names = T), read_csv) %>% do.call("bind_rows", .)

## trawl standard station meta data
trawl_std_stations <- read_csv("./data/westward_standard_lm_survey/tanner_standard_stations.csv")

# survey data mgmt ----

## summarise dredge survey catch for expansion
dredge_catch_raw %>%
  rename(event_id = tow) %>% 
  filter(cruise_year < 2019) %>%
  left_join(tow_code, by = "event_id") %>%
  bind_rows(dredge_catch_raw %>%
              filter(cruise_year >= 2019) %>%
              mutate(tow = as.numeric(tow))) %>%
  filter(rcode == 74120) %>%
  dplyr::select(tow, samp_grp, samp_cnt, samp_wt) %>%
  group_by(tow, samp_grp) %>%
  filter(!is.na(samp_grp)) %>%
  summarise_all(sum, na.rm = T) %>%
  ungroup() -> dredge_catch 

# fishery data mgmt ----
fishery_catch %>%
  f_catch_rename() %>%
  mutate(Set_date = mdy(Set_date)) %>%
  # join to discards
  left_join(fishery_discards %>%
              f_bycatch_rename() %>%
              mutate(Set_date = mdy(Set_date))) %>%
  f_add_season() %>%
  # revise district
  f_revise_district() %>%
  filter(dredge_hrs != 0) %>%
  ## coerce date to date class
  ## fix issue with missing basket weight in 2018/19
  mutate(round_weight = ifelse(Season == "2018/19" & District == "O",
                               54.1 * rtnd_basket, round_weight)) %>%
  group_by(Season, District, Set_date) %>%
  mutate(daily_disc_rate = sum(disc_wt, broken_wt, rem_disc_wt, na.rm = T) / sum(sample_hrs, na.rm = T)) %>%
  ungroup() %>%
  mutate(discard_lb = ifelse(sample_hrs > 0, 
                             dredge_hrs * ((disc_wt + broken_wt + rem_disc_wt) / sample_hrs),
                             dredge_hrs * daily_disc_rate)) %>%
  replace_na(list(discard_lb = 0)) %>%
  dplyr::select(Season, District, Haul_ID, round_weight, discard_lb) %>%
  rename_all(tolower) -> fishery_join

# trawl data mgmt ----

trawl_catch %>%
  # add year, rename
  rename_all(tolower) %>%
  mutate(year = as.numeric(substring(survey, 5, 8))) %>%
  # filter for scallops 
  filter(race_code == 74120) %>%
  dplyr::select(year, station, final_cnt) %>%
  f_timeseries_station_correct(., "final_cnt", trawl_std_stations) %>%
  left_join(trawl_std_stations %>%
              dplyr::select(station, scallop_index_dist),
            by = "station") %>%
  group_by(year, scallop_index_dist) %>%
  summarise(catch = sum(final_cnt, na.rm = T)) %>%
  filter(!is.na(scallop_index_dist)) -> trawl_join


# survey sh comp ----

## compute age composition
dredge_shad %>%
  rename(event_id = tow,
         year = cruise_year) %>%
  # update tow numbers
  left_join(tow_code, by = "event_id") %>%
  mutate(tow = as.numeric(ifelse(year >= 2019, event_id, tow))) %>%
  # filter for only shells in the model
  filter(!is.na( shell_height),
         shell_height >= 20) %>%
  left_join(dredge_catch, by = c("tow", "samp_grp")) %>%
  # add number of shells measured by tow 
  group_by(tow, samp_grp) %>%
  mutate(n_measured = n()) %>% ungroup %>%
  mutate(sh_bin = floor(shell_height / 5) * 5,
         sh_bin = ifelse(shell_height < 180, paste0("l", sh_bin/10), "l18")) %>%
  filter(!is.na(sh_bin)) %>%
  # add sample factor
  group_by(year, tow, district, samp_grp, samp_cnt, n_measured, sh_bin) %>%
  summarise(count = n()) %>%
  ungroup %>%
  mutate(sample_factor = samp_cnt * (count / n_measured)) %>%
  # aggregate by district
  group_by(year, district, sh_bin) %>%
  summarise(count = sum(sample_factor, na.rm = T)) %>%
  group_by(year, district) %>%
  mutate(prop = as.numeric(sprintf('%.4f', count / sum(count)))) %>%
  ungroup() %>%
  dplyr::select(-count) %>%
  pivot_wider(names_from = sh_bin, values_from = prop) %>%
  dplyr::select(year, district, paste0("l", seq(2, 18, 0.5))) %>%
  replace(is.na(.), 0) -> dredge_sh_comp

## compute effective sample size by district and year
dredge_shad %>%
  filter(!is.na(shell_height),
         shell_height >= 20) %>%
  rename(year = cruise_year) %>%
  group_by(year, district) %>%
  summarise(nsamp = round(min(0.1*n(), 100))) -> dredge_nsamp

# fishery sh comp ----

fishery_shad %>% 
  f_shell_height_rename() %>% rename_all(tolower) %>%
  # filter for only retained and discarded shells
  filter(rtnd_disc %in% c("R", "D")) %>%
  # change age to number
  # add age bin, plus group starts at age-18
  mutate(sh_bin = floor(sh / 5) * 5,
         sh_bin = ifelse(sh < 180, paste0("l", sh_bin/10), "l18")) %>%
  filter(!is.na(sh_bin)) %>%
  dplyr::select(haul_id, rtnd_disc, sh_bin) %>%
  # join to fishery catch
  left_join(fishery_join, by = "haul_id") %>%
  # set expansion wt
  mutate(wt = case_when(rtnd_disc == "R" ~ round_weight,
                        rtnd_disc == "D" ~ discard_lb)) %>%
  group_by(season, district, sh_bin) %>%
  summarise(count = n(),
            wt = sum(wt)) %>%
  group_by(season, district) %>%
  mutate(prop = as.numeric(sprintf('%.4f', wt / sum(wt, na.rm = T)))) %>%
  ungroup() %>%
  dplyr::select(-count, -wt) %>%
  pivot_wider(names_from = sh_bin, values_from = prop) %>%
  dplyr::select(season, district, paste0("l", seq(2, 18, 0.5))) %>%
  replace(is.na(.), 0) -> fishery_sh_comp


## compute effective sample size by district and year
fishery_shad %>%
  f_shell_height_rename() %>% rename_all(tolower) %>%
  filter(!is.na(sh),
         sh >= 20) %>%
  left_join(fishery_join %>%
              dplyr::select(season, district, haul_id), by = c("haul_id", "district")) %>%
  mutate(year = as.numeric(substring(season, 1, 4))) %>%
  group_by(year, district) %>%
  summarise(nsamp = round(min(0.1*n(), 100))) -> fishery_nsamp

# trawl sh comp ----

trawl_spec %>%
  # add year, rename
  rename_all(tolower) %>%
  mutate(year = as.numeric(substring(survey, 5, 8))) %>%
  # filter for scallops 
  filter(race_code == 74120) %>%
  # add size bin
  mutate(sh_bin = floor(nsize / 5) * 5,
         sh_bin = ifelse(nsize < 180, paste0("l", sh_bin/10), "l18")) %>%
  filter(!is.na(sh_bin)) %>%
  # join to index district
  left_join(trawl_std_stations %>%
              dplyr::select(station, scallop_index_dist),
            by = "station") %>%
  group_by(year, scallop_index_dist, sh_bin) %>%
  summarise(count = sum(sampfrac)) %>%
  # join to trawl catch
  left_join(trawl_join) %>%
  mutate(prop = count / catch) %>%
  dplyr::select(-count, -catch) %>%
  ungroup() %>%
  pivot_wider(names_from = sh_bin, values_from = prop) %>%
  dplyr::select(year, scallop_index_dist, paste0("l", seq(2, 18, 0.5))) %>%
  filter(!is.na(scallop_index_dist)) %>%
  replace(is.na(.), 0) -> trawl_sh_comp

## compute effective sample size by district and year
trawl_spec %>%
  rename_all(tolower) %>%
  mutate(year = as.numeric(substring(survey, 5, 8))) %>%
  filter(!is.na(nsize),
         nsize >= 20) %>%
  left_join(trawl_std_stations %>%
              dplyr::select(station, scallop_index_dist),
            by = "station") %>%
  group_by(year, scallop_index_dist) %>%
  filter(!is.na(scallop_index_dist)) %>%
  summarise(nsamp = round(min(0.1*n(), 100))) -> trawl_nsamp
  

# save matrices for ss ----

## kamn input
dredge_sh_comp %>%
  filter(district == "KAM") %>%
  mutate(month = 7, 
         fleet = 2,
         sex = 0, 
         part = 0) %>%
  left_join(dredge_nsamp, by = c("district", "year")) %>%
  dplyr::select(year, month, fleet, sex, part, nsamp, paste0("l", seq(2, 18, 0.5))) %>%
  arrange(fleet, year) %>%
  write_csv("./data/model_development/stock_synthesis/ss_kam_sh_comp.csv")

## ksh input
fishery_sh_comp %>%
  filter(district == "KSH") %>%
  mutate(year = as.numeric(substring(season, 1, 4)),
         month = 7, 
         fleet = 1,
         sex = 0, 
         part = 0) %>%
  left_join(fishery_nsamp, by = c("district", "year")) %>%
  dplyr::select(year, month, fleet, sex, part,  nsamp, paste0("l", seq(2, 18, 0.5))) -> ss_fishery_sh

dredge_sh_comp %>%
  filter(district == "KSH") %>%
  mutate(month = 7, 
         fleet = 2,
         sex = 0, 
         part = 0) %>%
  left_join(dredge_nsamp, by = c("district", "year")) %>%
  dplyr::select(year, month, fleet, sex, part, nsamp, paste0("l", seq(2, 18, 0.5))) -> ss_dredge_sh

trawl_sh_comp %>%
  filter(scallop_index_dist == "KSH") %>%
  mutate(month = 9, 
         fleet = 3,
         sex = 0, 
         part = 0) %>%
  left_join(trawl_nsamp, by = c("scallop_index_dist", "year")) %>%
  dplyr::select(year, month, fleet, sex, part, nsamp, paste0("l", seq(2, 18, 0.5))) -> ss_trawl_sh

rbind(ss_fishery_sh, ss_dredge_sh, ss_trawl_sh) %>%
  arrange(fleet, year) %>%
  write_csv("./data/model_development/stock_synthesis/ss_ksh_sh_comp.csv")
