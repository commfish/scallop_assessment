# notes ----
## Prepare survey age data for ss
## current ony focused on KSH so 2019/20 data not needed
## Tyler Jackson
## tyler.jackson@alaska.gov
## last update 10/27/2020

# load ----
library(here)

### functions for survey data
source("./code/misc/statewide_scallop_survey_functions.R")


# data ----

## ages
read_csv(here("data/statewide_scallop_survey/ages", "2016-2018_scallop_survey_ages.csv")) %>%
  rename_all(tolower) %>%
  rename(tow = event_id) %>%
  dplyr::select(year, tow, shell_num, age) %>%
  mutate(samp_grp = ifelse(shell_num > 10, 2, 1)) -> age

## specimen
read_csv(here("data/statewide_scallop_survey/specimen", "awl_180719.csv")) %>%
  rename_all(tolower) %>%
  filter(race_code == 74120) %>%
  rename(tow = event_id) %>%
  dplyr::select(tow, scallop_number, shell_height_mm) %>%
  mutate(samp_grp = ifelse(scallop_number > 10, 2, 1)) -> sh

## catch data (for expansion)
read_csv(here("data/statewide_scallop_survey/catch", "catchComp_180719.csv")) %>%
  rename_all(tolower) %>%
  filter(race_code == 74120) %>%
  dplyr::select(event_id, race_code, scal_size_class, count, sample_wt_kg) %>%
  rename(tow = event_id,
         rcode = race_code,
         samp_grp = scal_size_class,
         samp_cnt = count, 
         samp_wt = sample_wt_kg) %>%
  group_by(tow, rcode, samp_grp) %>%
  filter(!is.na(samp_grp)) %>%
  summarise_all(sum, na.rm = T) -> catch 

## tow data to get district and area swept (place holder for new format)
read_csv(here("data/statewide_scallop_survey/logbook", "events_180719.csv")) %>%
  rename_all(tolower) %>%
  rename(start_lat = start_latitude,
         start_lon = start_longitude,
         end_lat = end_latitude,
         end_lon = end_longitude,
         distance = tow_length_designated,
         tow = event_id,
         haul = event_num,
         gear_perf = gear_performance_code_sw, 
         bed_code = bed_sw,
         avg_depth = depth_start_f) %>%
  mutate(haul_type = 10,
         cruise = as.numeric(paste0(substring(year, 3, 4), 01))) %>%
  f_clean_log() -> tows

## bed area data
beds <- read_csv("./data/statewide_scallop_survey/bed_strata.csv")
  
# abundance index ----
catch %>%
  # combine catches by sample group
  group_by(tow) %>%
  summarise(samp_wt = sum(samp_wt)) %>%
  left_join(tows, by = "tow") %>%
  # compute cpue biomass t
  mutate(cpue_tow = samp_wt / 1000 / area_swept) %>%
  # mean and var by bed
  group_by(year, bed_code) %>%
  summarise(cpue = mean(cpue_tow, na.rm = T),
            var = var(cpue_tow, na.rm = T) / n()) %>%
  left_join(beds, by = "bed_code") %>%
  # compute biomass (t) and log se
  mutate(biomass = cpue * area_nm2,
         se_log = log(sqrt(var * area_nm2^2))) -> dredge_survey_biomass
          



# shell height comp ----
sh %>%
  left_join(catch, by = c("tow", "samp_grp")) %>%
  filter(!is.na(shell_height_mm)) %>%
  # add number of shells aged by tow 
  group_by(tow, samp_grp) %>%
  mutate(n_measured = n(),
         year = as.numeric(substring(tow, 1, 4))) %>%
  #add sample factor
  group_by(year, tow, shell_height_mm, samp_grp, samp_cnt, n_measured) %>%
  summarise(count = n()) %>%
  ungroup %>%
  mutate(sample_factor = samp_cnt * (count / n_measured),
         n_sh_est = round(sample_factor)) %>%
  # joing to bed
  left_join(tows, by = c("year", "tow")) %>%
  rename(yr = year) %>%
  # summarise by bed
  group_by(yr, bed_code, shell_height_mm , .drop = F) %>%
  summarise(n = sum(n_sh_est, na.rm = T)) %>%
  rename(sh = shell_height_mm) -> dredge_survey_sh

# join to number of shells measure per bed by yr
sh %>%
  # add number of shells aged by tow 
  group_by(tow) %>%
  summarise(Nsamp = n()) %>%
  left_join(tows, by = "tow") %>%
  rename(yr = year) %>%
  group_by(yr, bed_code) %>%
  summarise(Nsamp = sum(Nsamp)) %>%
  right_join(dredge_survey_sh, by = c("yr", "bed_code")) -> dredge_survey_sh


# age comp ----
age %>%
  filter(!is.na(age)) %>%
  left_join(catch, by = c("tow", "samp_grp")) %>%
  # add number of shells aged by tow 
  group_by(tow, samp_grp) %>%
  mutate(n_aged = n()) %>%
  #add sample factor
  group_by(year, tow, age, samp_grp, samp_cnt, n_aged) %>%
  summarise(count = n()) %>%
  ungroup %>%
  mutate(sample_factor = samp_cnt * (count / n_aged),
         n_age_est = round(sample_factor)) %>%
  # joing to bed
  left_join(tows, by = c("year", "tow")) %>%
  rename(yr = year) %>%
  # summarise by bed
  group_by(yr, bed_code, age, .drop = F) %>%
  summarise(n = sum(n_age_est, na.rm = T)) -> dredge_survey_ages
  
# join to number of shells measure per bed by year
age %>%
  # add number of shells aged by tow 
  group_by(tow) %>%
  summarise(Nsamp = n()) %>%
  left_join(tows, by = "tow") %>%
  rename(yr = year) %>%
  group_by(yr, bed_code) %>%
  summarise(Nsamp = sum(Nsamp)) %>%
  right_join(dredge_survey_ages, by = c("yr", "bed_code")) -> dredge_survey_ages

# bed area
beds <- read_csv("./data/statewide_scallop_survey/bed_strata.csv")





