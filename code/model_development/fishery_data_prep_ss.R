# notes ----
## generate fishery shell height and age composition for stock synthesis, by district
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 10/26/2020

# load libraries and set global options ----

## packages
library(tidyverse)
library(magrittr)

## sourced scripts
### general observer data functions
source("./code/misc/general_observer_data_functions.R")

# data ----

## observer/logbook data
### scallop haul data 2009/10 - Present
catch <- do.call(bind_rows,
                 lapply(paste0("data/observer/catch/", list.files("data/observer/catch/")), read_csv))
### bycatch by day 2009/10 - Present
bycatch <- do.call(bind_rows,
                   lapply(paste0("data/observer/bycatch/", list.files("data/observer/bycatch/")), read_csv))
### shell heights 2009/10 - Present
shell_height <- do.call(bind_rows,
                        lapply(paste0("data/observer/shell_height/", 
                                      list.files("data/observer/shell_height/")), read_csv))
### ages 2009/10 - 2015
ages <- read_csv("./data/observer/age/age_data_observer_shells_1996_2015.csv")


# data mgmt ----

## remove phantom rows in catch tibble (can't figure out where they come from...)
catch %>%
  drop_na(Haul_ID) -> catch

## rename fields in current data (2009 - present)
f_catch_rename(catch) %>%
  ## add Season to data
  f_add_season() %>%
  ## classify Karluk bed as KSW district instead of KSH
  f_revise_district() %>% 
  ## coerce date to date class
  mutate(Set_date = lubridate::mdy(Set_date)) %>%
  ## remove tows with zero dredge hours (logbook mistake)
  filter(dredge_hrs != 0) %>%
  ## fix issue with missing basket weight in 2018/19
  mutate(round_weight = ifelse(Season == "2018/19" & District == "O",
                               54.1 * rtnd_basket, round_weight)) -> catch

## rename fields in bycatch data (2009 - present)
f_bycatch_rename(bycatch) %>%
  ## add Season to data
  f_add_season() %>%
  ## revise district (replace with f_revise district after beds are added to data)
  mutate(District = ifelse(District %in% c("D", "YAK", "D16"), "YAK", District)) %>%
  ## coerce date to date class
  mutate(Set_date = lubridate::mdy(Set_date)) %>%
  ## fix issue with missing basket weight in 2018/19
  mutate(est_rnd_wt = ifelse(Season == "2018/19" & District == "O",
                             catch %>%
                               filter(Season == "2018/19" & District == "O") %>%
                               pull(round_weight) %>%
                               sum,
                             est_rnd_wt)) -> bycatch

## rename fields in shell_height data (2009 - present) 
f_shell_height_rename(shell_height) %>%
  ## add Season to data
  f_add_season() %>%
  ## revise District as in catch data
  mutate(District = catch$District[match(.$Haul_ID, catch$Haul_ID)]) -> shell_height

# shell height composition based on relative biomass ----

## get number of length samples of retained catch by district
shell_height %>%
  filter(Rtnd_disc == "R") %>%
  mutate(yr = as.numeric(substring(Season, 1, 4))) %>%
  rename(district = District) %>%
  group_by(yr, district) %>%
  summarise(Nsamp = n(), .groups = "drop") -> sh_nsamp

## expand shell height composition for retained catch
shell_height %>%
  filter(Rtnd_disc == "R") %>%
  mutate(yr = as.numeric(substring(Season, 1, 4))) %>%
  # consolidate by sh within haul
  group_by(yr, District, Haul_ID, sh) %>%
  count(name = "count") %>%
  # join to retained catch
  left_join(catch %>%
              dplyr::select(Haul_ID, round_weight)) %>%
  # expand wt
  mutate(w = count * round_weight) %>%
  # summarise by district
  group_by(yr, District, sh) %>%
  summarise(w = sum(w, na.rm = T)) %>%
  # add sum of w by district, convert w to prop
  group_by(yr, District) %>%
  mutate(sum_w = sum(w, na.rm = T),
         w_prop = w / sum_w) %>%
  rename_all(tolower) %>%
  # join to nsamp
  left_join(sh_nsamp) %>%
  dplyr::select(yr, district, Nsamp, sh, w_prop) -> fishery_sh


# bycatch %>%
#   # compute a daily discard rate (lbs/dregde hr)
#   group_by(Season, District, Set_date) %>%
#   summarise(disc_wt = sum(disc_wt, broken_wt, rem_disc_wt),
#             sample = sum(sample_hrs)) %>%
#   group_by(Season, District) %>%
#   mutate(disc_rate = ifelse(sample != 0, 
#                             disc_wt / sample, 
#                             sum(disc_wt) / sum(sample))) %>%
#   # join to catch data by haul
#   dplyr::select(Season, District, Set_date, disc_rate) %>%
#   right_join(catch, by = c("Season", "District", "Set_date")) %>%
#   # estimate discards by haul
#   mutate(disc_est_lbs = dredge_hrs * disc_rate) %>%
#   # estimate weights for shell height histogram (prop of annual catch)
#   dplyr::select(Season, District, Haul_ID, round_weight, disc_est_lbs) %>%
#   pivot_longer(c(round_weight, disc_est_lbs), 
#                names_to = "Rtnd_disc", values_to = "wt_lbs") %>%
#   mutate(Rtnd_disc = ifelse(Rtnd_disc == "round_weight", "R", "D"),
#          w = wt_lbs / sum(wt_lbs, na.rm = T)) %>%
#   ungroup() %>%
#   dplyr::select(Haul_ID, Rtnd_disc, w) %>%
#   right_join(shell_height, by = c("Haul_ID", "Rtnd_disc")) %>%
#   group_by(Season, District, sh) %>%
#   summarise(w = sum(w)) %>%
#   group_by(Season, District) %>%
#   mutate(w_adj = w / sum(w)) %>%
#   left_join(shell_height %>%
#               group_by(Season, District) %>%
#               summarise(Nsamp = n()),
#             by = c("Season", "District")) %>%
#   mutate(Year = as.numeric(substring(Season, 1, 4))) %>%
#   ungroup() %>%
#   select(Year, District, Nsamp, sh, w_adj) %>%
#   # rename fields for consistency
#   rename(yr = Year,
#          district = District) -> fishery_sh




# age composition based on relative biomass ----

## compute nsamples for ages by year and district
catch %>%
  rename_all(tolower) %>%
  mutate(yr = as.numeric(substring(season, 1, 4))) %>%
  dplyr::select(yr, haul_id, district) %>%
  right_join(ages %>%
               dplyr::select(haul_id, tot_annuli),
             by = "haul_id") %>%
  group_by(yr, district) %>%
  summarise(Nsamp = n()) -> age_nsamp

## unexpanded age composition
catch %>%
  rename_all(tolower) %>%
  mutate(yr = as.numeric(substring(season, 1, 4))) %>%
  dplyr::select(yr, haul_id, district) %>%
  right_join(ages %>%
               dplyr::select(haul_id, tot_annuli),
             by = "haul_id") %>%
  rename(age = tot_annuli) %>%
  group_by(yr, district, age) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(age_nsamp, by = c("yr", "district")) %>%
  dplyr::select(yr, district, Nsamp, age, count) -> fishery_ages
  
  

