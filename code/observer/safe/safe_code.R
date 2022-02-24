# notes ----
# SAFE report observer data summary
# Scallop catch tables and figures
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov

# load ----
source("./code/safe/safe_fishery_functions.R")

YEAR <- 2020

# data load ----

## scallop haul data ~1993/94 - 2008/09
## FOR NOW THIS IS JUST A SUMMARY TABLE, BUT SHOULD BE REPLACED TO HAUL DATA
old_catch <- read_csv("./data/old_catch/catch_summary_1992-2008.csv")

## scallop haul data 2009/10 - Present
catch <- do.call(bind_rows,
                 lapply(paste0("data/catch/", list.files("data/catch/")), read_csv))
## shell height data
shell_height <- do.call(bind_rows,
                 lapply(paste0("data/shell_height/", list.files("data/shell_height/")), read_csv))

## bycatch data (crab, fish and scallop discards)
bycatch <- do.call(bind_rows,
                        lapply(paste0("data/bycatch/", list.files("data/bycatch/")), read_csv))

## crab bycatch measurements
crab_size <- do.call(bind_rows,
                lapply(paste0("data/crab_size/", list.files("data/crab_size/")), read_csv))

## GHLs by area, district, season
ghl <- read_csv("./data/metadata/ghl.csv")

## names and abbreviations of management units
mgmt_unit <- read_csv("./data/metadata/management_units.csv")


# data mgmt ----

## rename fields in current data (2009 - present)
### scallop haul data
names(catch) <- c("Fishery", "District", "ADFG", "Trip_ID", "Haul_ID", 
                     "haul", "gear_perf", "haul_sampled", "Set_date", "set_lat",
                     "set_lon", "statarea", "depth", "dredge_count", 
                     "dredge_width", "dredge_down", "dredge_up", "duration", 
                     "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                     "rtnd_basket", "round_weight", "meat_weight", "est_yield",
                     "tot_rtnd_basket", "tot_day_meat_weight")
### bycatch data
names(bycatch) <- c("Fishery", "ADFG", "Set_date", "District", "hauls", "dredge_hrs", 
                    "est_rnd_wt", "mt_wt", "sample_hrs", "bairdi_count", "opilio_count",
                    "dungeness_count", "halibut_count", "disc_count", "disc_wt", "broken_wt",
                    "rem_disc_wt", "clapper_count", "king_count")
                    
### shell height data
names(shell_height)[c(6, 7)] <- c("shell_height", "shell_num")

### crab data
names(crab_size) <- c("Fishery", "District", "race_code", "sex", "size", "sampfrac_num_crab")

## create a vector of districts fished in the most recent season
## change number to current year
catch %>%
  filter(grepl("18", Fishery)) %>%
  pull(District) %>%
  unique() -> fished_districts


# tables and figures ----

## fishery catch statistics
fished_districts %>%
  purrr::map(~f_fishery_stats(catch, bycatch, specific_m = F, old_data_table = T, district = .))

## raw and standardized cpue
### subset fished districts so that GAMs may run
tibble(fished_districts) %>%
  filter(!(fished_districts %in% c("C", "KSE", "WKI", "D"))) %>%
  pull(fished_districts) %>%
  purrr::map(~f_fishery_cpue(catch, district = ., weight_type = "round"))

## shell height composition
fished_districts %>%
  purrr::map(~f_sh_comp(shell_height, district = .))

## crab bycatch
f_bycatch_chionoecetes(bycatch, crab_size, all_seasons = F)
f_bycatch_king(bycatch, all_seasons = F)


# clean up: re-do figures that do not suit the generalized function ----
## fishery cpue YAK District
catch %>%
  mutate(District = ifelse(District %in% c("YAK", "D", "D16"), "YAK", District)) %>%
  filter(District == "YAK") %>%
  f_fishery_cpue(., district = "YAK", weight_type = "round")

## shell height for YAK District
shell_height %>%
  mutate(District = ifelse(District %in% c("YAK", "D", "D16"), "YAK", District)) %>%
  filter(District == "YAK") %>%
  f_sh_comp(., district = "YAK")

## CPUE for YAK District
catch %>%
  mutate(District = ifelse(District %in% c("YAK", "D", "D16"), "YAK", District)) %>%
  filter(District == "YAK") %>%
  f_fishery_cpue(., district = "YAK", weight_type = "round")
