# notes ----
## prepare age composition for stock synthesis models
## Tyler Jackson
## tyler.jackson@alaska.gov

# load ----
library(tidyverse)

## functions for observer data mgmt 
source("./code/misc/general_observer_data_functions.R")

# data ----

## dredge survey age data
dredge_ages <- read.csv("./data/statewide_scallop_survey/ages/dredge_survey_age_dump.csv", na.strings = "NULL")

## catch data (for expansion)
dredge_catch_raw <- read.csv("./data/statewide_scallop_survey/catch/survey_catch_ts_temporary.csv")

## tow code rosetta stone
tow_code <- read.csv("./data/statewide_scallop_survey/temp_tow_to_event_id.csv")

## fishery age data from adu 
fishery_ages_adu <- read.csv("./data/observer/age/adu_fishery_age_dump.csv", na.strings = "NULL")

## fishery catch 2009 - present
fishery_catch <- lapply(list.files("data/observer/catch/", pattern = "CatchByHaul", full.names = T), read_csv) %>% do.call("bind_rows", .)
## fishery discards 2009 - present
fishery_discards <- lapply(list.files("data/observer/bycatch/", pattern = "BycatchByHaul", full.names = T), read_csv) %>% do.call("bind_rows", .)

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



# survey age comp ----

## compute age composition
dredge_ages %>%
  filter(!is.na(age),
         age > 0) %>%
  rename(year = cruise_year) %>%
  left_join(dredge_catch, by = c("tow", "samp_grp")) %>%
  # add number of shells aged by tow 
  group_by(tow, samp_grp) %>%
  mutate(n_aged = n()) %>%
  # add age bin, plus group starts at age-18
  mutate(age_bin = ifelse(age < 18, paste0("a", age), "a18"),
         age_bin = factor(age_bin, levels = paste0("a", 1:18))) %>%
  filter(!is.na(age_bin)) %>%
  # add sample factor
  group_by(year, tow, district, samp_grp, samp_cnt, n_aged, age_bin) %>%
  summarise(count = n()) %>%
  ungroup %>%
  mutate(sample_factor = samp_cnt * (count / n_aged)) %>%
  # aggregate by district
  group_by(year, district, age_bin) %>%
  summarise(count = sum(sample_factor, na.rm = T)) %>%
  group_by(year, district) %>%
  mutate(prop = as.numeric(sprintf('%.4f', count / sum(count)))) %>%
  ungroup() %>%
  dplyr::select(-count) %>%
  pivot_wider(names_from = age_bin, values_from = prop) %>%
  replace(is.na(.), 0) -> survey_age_comp

## compute effective sample size by district and year
dredge_ages %>%
  filter(!is.na(age),
         age > 0) %>%
  rename(year = cruise_year) %>%
  group_by(year, district) %>%
  summarise(nsamp = round(min(0.1*n(), 100))) -> survey_nsamp

# fishery age comp ----

fishery_ages_adu %>% 
  # filter for only retained and discarded shells
  filter(shell_source %in% c("R", "D")) %>%
  # change age to number
  # add age bin, plus group starts at age-18
  mutate(age = as.numeric(age),
         age_bin = ifelse(age < 18, paste0("a", age), "a18"),
         age_bin = factor(age_bin, levels = paste0("a", 1:18))) %>%
  filter(!is.na(age_bin)) %>%
  dplyr::select(haul_id, shell_source, age_bin) %>%
  # join to fishery catch
  left_join(fishery_join, by = "haul_id") %>%
  # set expansion wt
  mutate(wt = case_when(shell_source == "R" ~ round_weight,
                        shell_source == "D" ~ discard_lb)) %>%
  group_by(season, district, age_bin) %>%
  summarise(count = n(),
            wt = sum(wt)) %>%
  group_by(season, district) %>%
  mutate(prop = as.numeric(sprintf('%.4f', wt / sum(wt, na.rm = T))))
  ungroup()  %>%
  dplyr::select(-count, -wt) %>%
  arrange(age_bin)  %>%
  pivot_wider(names_from = age_bin, values_from = prop) %>%
  replace(is.na(.), 0) -> fishery_age_comp


## compute effective sample size by district and year
fishery_ages_adu %>%
  mutate(age = as.numeric(age)) %>%
  filter(!is.na(age),
         age > 0) %>%
  left_join(fishery_join %>%
              dplyr::select(season, district, haul_id), by = "haul_id") %>%
  mutate(year = as.numeric(substring(season, 1, 4))) %>%
  group_by(year, district)  %>%
  summarise(nsamp = round(min(0.1*n(), 100))) -> fishery_nsamp

# save comp matrices for ss ----

## kam input
survey_age_comp %>%
  filter(district == "KAM") %>%
  mutate(month = 6, 
         fleet = 2,
         sex = 0, 
         part = 0,
         agerr = 1,
         lbin_lo = 1,
         lbin_hi = -1) %>%
  left_join(survey_nsamp, by = c("district", "year")) %>%
  dplyr::select(year, month, fleet, sex, part, agerr, lbin_lo, lbin_hi, nsamp, paste0("a", 1:18)) -> ss_dredge_age

ss_dredge_age %>%
  arrange(fleet, year) %>%
  write_csv("./data/model_development/stock_synthesis/ss_kam_age_comp.csv")
  
## ksh input
fishery_age_comp %>%
  filter(district == "KSH") %>%
  mutate(year = as.numeric(substring(season, 1, 4)),
         month = 7, 
         fleet = 1,
         sex = 0, 
         part = 0,
         agerr = 1,
         lbin_lo = 1,
         lbin_hi = -1) %>%
  left_join(fishery_nsamp, by = c("district", "year")) %>%
  dplyr::select(year, month, fleet, sex, part, agerr, lbin_lo, lbin_hi, nsamp, paste0("a", 1:18)) -> ss_fishery_age

survey_age_comp %>%
  filter(district == "KSH") %>%
  mutate(month = 5, 
         fleet = 2,
         sex = 0, 
         part = 0,
         agerr = 1,
         lbin_lo = 1,
         lbin_hi = -1) %>%
  left_join(survey_nsamp, by = c("district", "year")) %>%
  dplyr::select(year, month, fleet, sex, part, agerr, lbin_lo, lbin_hi, nsamp, paste0("a", 1:18)) -> ss_dredge_age

rbind(ss_fishery_age, ss_dredge_age) %>%
  arrange(fleet, year) %>%
  write_csv("./data/model_development/stock_synthesis/ss_ksh_age_comp.csv")

# survey mean size at age ----

## compute size at age
dredge_ages %>%
  as_tibble() %>%
  mutate(age_bin = ifelse(age < 18, paste0("a", age), "a18")) %>%
  group_by(cruise_year, district, age_bin) %>%
  summarise(size = mean(shell_height, na.rm = T) / 10) %>%
  filter(age_bin != "a0") %>%
  ungroup %>%
  pivot_wider(names_from = age_bin, values_from = size) %>%
  dplyr::select(district, cruise_year, paste0("a", 1:18)) %>%
  replace(is.na(.), 0) -> mean_saa_dredge

# fishery mean size at age ----

## compute size at age
fishery_ages_adu %>%
  left_join(fishery_join, by = "haul_id") %>%
  as_tibble() %>%
  mutate(year = as.numeric(substring(season, 1, 4))) %>%
  mutate(age_bin = ifelse(age < 18, paste0("a", age), "a18")) %>%
  group_by(year, district, age_bin) %>%
  summarise(size = mean(shell_height, na.rm = T) / 10) %>%
  filter(age_bin != "a0") %>%
  ungroup %>%
  pivot_wider(names_from = age_bin, values_from = size) %>%
  dplyr::select(district, year, paste0("a", 1:18)) %>%
  replace(is.na(.), -1) -> mean_saa_fishery


# save mean size at age matrix for ss ----

## kamishak
mean_saa_dredge %>%
  filter(district == "KAM") %>%
  mutate(year = cruise_year,
         month = 6,
         fleet = 2, 
         sex = 0,
         part = 0,
         ign = 1) %>%
  dplyr::select(year, month, fleet, sex, part, ign, paste0("a", 1:18)) %>%
  bind_cols(., tibble(saa = rep(20, 18)) %>% t() %>% as.tibble()) %>%
  write_csv("./data/model_development/stock_synthesis/ss_kam_mean_saa.csv")


## kodiak shelikof
### fishery
mean_saa_fishery %>%
  filter(district == "KSH") %>%
  mutate(year = year,
         month = 6,
         fleet = 1, 
         sex = 0,
         part = 0,
         agerr = 2,
         ign = 1) %>%
  dplyr::select(year, month, fleet, sex, part, ign, paste0("a", 1:18)) %>%
  bind_cols(., tibble(saa = rep(20, 18)) %>% t() %>% as.tibble()) -> fish_mat

### survey
mean_saa_dredge %>%
  filter(district == "KSH") %>%
  mutate(year = cruise_year,
         month = 6,
         fleet = 1, 
         sex = 0,
         part = 0,
         agerr = 2,
         ign = 1) %>%
  dplyr::select(year, month, fleet, sex, part, ign, paste0("a", 1:18)) %>%
  bind_cols(., tibble(saa = rep(20, 18)) %>% t() %>% as.tibble()) -> dredge_mat

bind_rows(fish_mat, dredge_mat) %>%
  write_csv("./data/model_development/stock_synthesis/ss_ksh_mean_saa.csv")
