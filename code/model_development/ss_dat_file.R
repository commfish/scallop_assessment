# notes ----

## Edit .dat file (i.e. use KAMN inputs as guide)
## Tyler Jackson
## 10/29/2020

# load ----

library(r4ss)
library(tidyverse)


# data prep ----

## fishery shell height and age composition
source("./code/model_development/fishery_data_prep_ss.R")

## trawl survey data
source("./code/fishery_independent/trawl_surveys/westward_largemesh_survey_ss.R")

## dredge survey data
source("./code/model_development/dredge_survey_data_prep_ss.R")



# version specification ----
## sourcefile 
sourcefile <- "./code/stock_synthesis/kodiak_shelikof/ksh.dat"

## type 
type <- "Stock_Synthesis_data_file"

## ss version 
ReadVersion <- "3.30"

# basic options ----
## start year
styr <- 1993

## end year
endyr <- 2019

## number of seasons, months per season - unchanged
nseas <- 1
months_per_seas <- 12

## number of subseasons (not in KAMN dat file v3.24)
Nsubseasons <- 2 # minimum

## spawning month (not in KAMN dat file v3.24)
spawn_month <- 3 # march

## number of sexes (Ngenders in KAMN dat file v3.24)
## 1 = current one sex, ignore fraction female input in the control file;
## 2 = current two sex, use fraction female in the control file; and
## -1 = one sex and multiply the spawning biomass by the fraction female in the control file.
Nsexes <- 1

## number of ages
Nages <- 18

## number of areas
Nareas <- 1

# define fleets ----

## total number of fishing and survey fleets combined
Nfleets <- 3 

## fleet info
fleetinfo <- data.frame(type = c(1, 3, 3),
                                surveytiming = c(-1, 1, 1),
                                area = c(1, 1, 1),
                                units = c(1, 1, 1),
                                need_catch_mult = 0,
                                fleetname = c("FISHERY1", "DREDGESURVEY", "TRAWLSURVEY"))

## fleet names
fleetnames <- c("FISHERY1", "DREDGESURVEY", "TRAWLSURVEY")

# catch ----

## survey timing (pg 36 of SS 3.30.16 user manual)
surveytiming <- c(-1, 1, 1) 

## catch units (pg 37 of SS 3.30.16 user manual)
units_of_catch <- c(1, 1, 1)

## areas (pg 37 of SS 3.30.16 user manual)
## area each fleet operates in 
areas <- c(1, 1, 1)

## catch (timeseries of retained catch by fleet)
## convert retained round weight to mertic tons
## set arbitrary low se on retain catch
read_csv("./data/stock_synthesis/ksh/ksh_catch_summary.csv") %>%
  # define data inputs
  mutate(year = as.numeric(substring(season, 1, 4)),
         seas = rep(1, nrow(.)),
         fleet = rep(1, nrow(.)),
         catch = ret_rnd_lb * 0.000453592,
         catch_se = rep(0.01, nrow(.))) %>%
  # add equilibrium catch year
  add_row(year = -999, seas = 1, fleet = 1, catch = 0, catch_se = 0.01, .before = 1) %>%
  # select columns for .dat file
  dplyr::select(year, seas, fleet, catch, catch_se) %>%
  # omit years with no fishery
  filter(!is.na(catch)) %>%
  # coerce to data frame
  as.data.frame() -> catch

# survey biomass ----
  
## cpue index info (pg 42 of SS 3.30.16 user manual)
CPUEinfo <- data.frame(Fleet = c(1, 2, 3),
                       Units = c(1, 1, 1),
                       Errtype = c(0, 0, 0),
                       SD_Report = c(0, 0, 0))
row.names(CPUEinfo) <- fleetnames
  
## cpue data by survey
### dredge survey
dredge_survey_biomass %>%
  filter(bed_code == "KSH1") %>%
  mutate(seas = 5, 
         index = 2) %>%
  rename(obs = biomass,
         yr = year) %>%
  dplyr::select(yr, seas, index, obs, se_log) %>%
  ungroup() %>%
  as.data.frame() -> dredge_survey_biomass

### trawl survey
wslm_cpue %>%
  filter(district == "KSH") %>%
  mutate(seas = 7,
         index = 3) %>%
  rename(obs = biomass,
         yr = Year) %>%
  dplyr::select(yr, seas, index, obs, se_log) %>%
  ungroup() %>%
  as.data.frame() -> trawl_survey_biomass

### join surveys
CPUE <- rbind(dredge_survey_biomass, trawl_survey_biomass)  

# discards ----
## number of fleets with discard
N_discard_fleets <- 0

# shell height comps ----
## length bin method (pg 49 of SS 3.30.16 user manual)
lbin_method <- 2

## use length comp (1, 0)
use_lencomp <- 1

## length info (pgs 50 - 51 of SS 3.30.16 user manual)
len_info <- data.frame(mintailcomp = c(0, 0, 0),
                       addtocomp = c(0.001, 0.001, 0.001),
                       combine_M_F = c(0, 0, 0),
                       CompressBins = c(0, 0, 0),
                       CompError = c(0, 0, 0),
                       ParmSelect = c(0, 0, 0),
                       minsamplesize = c(1, 1, 1))

## number of length bins
N_lbins <- 33

## vector of length bin divisors(cenimeters)
lbin_vector <- seq(2.1, 18.1, by = 0.5)

## length composition

### fishery
fishery_sh %>%
  #filter for district
  filter(district == "KSH") %>%
  # bin data to lbin_vector
  mutate(lbin = cut(sh / 10, 
                    breaks = c(0, lbin_vector[1:length(lbin_vector) - 1], 100),
                    labels = paste0("l", lbin_vector))) %>%
  group_by(yr, Nsamp, lbin, .drop = F) %>%
  summarise(w_prop = sum(w_prop),
            .groups = "drop") %>%
  # add season (month of survey - July) 
  # fleet  - 2 (SURVEY1)
  mutate(month = 7,
         fleet = 1, 
         sex = 0,
         part = 0) %>%
  dplyr::select(yr, month, fleet, sex, part, Nsamp, lbin, w_prop) %>%
  # pivot to wide format
  pivot_wider(names_from = lbin, values_from = w_prop) %>%
  as.data.frame() -> fishery1_lencomp

### dredge survey 
dredge_survey_sh %>%
  filter(bed_code == "KSH1") %>%
  # bin data to lbin_vector
  mutate(lbin = cut(sh / 10, 
                    breaks = c(0, lbin_vector[1:length(lbin_vector) - 1], 100),
                    labels = paste0("l", lbin_vector))) %>%
  group_by(yr, Nsamp, lbin, .drop = F) %>%
  summarise(count = sum(n),
            .groups = "drop") %>%
  # add season (month of survey - July) 
  # fleet  - 2 (DREDGESURVEY)
  mutate(month = 5,
         fleet = 2, 
         sex = 0,
         part = 0) %>%
  dplyr::select(yr, month, fleet, sex, part, Nsamp, lbin, count) %>%
  # pivot to wide format
  pivot_wider(names_from = lbin, values_from = count) %>%
  as.data.frame() -> dredge_lencomp

### trawl survey (expanded count)
wslm_sh %>%
  # filter for district
  filter(district == "KSH") %>%
  # bin data to lbin_vector
  mutate(lbin = cut(sh / 10, 
                    breaks = c(0, lbin_vector[1:length(lbin_vector) - 1], 100),
                    labels = paste0("l", lbin_vector))) %>%
  group_by(yr, Nsamp, lbin, .drop = F) %>%
  summarise(count = sum(count),
            .groups = "drop") %>%
  # add season (month of survey - July) 
  # fleet  - 2 (TRAWLSURVEY)
  mutate(month = 7,
         fleet = 3, 
         sex = 0,
         part = 0) %>%
  dplyr::select(yr, month, fleet, sex, part, Nsamp, lbin, count) %>%
  # pivot to wide format
  pivot_wider(names_from = lbin, values_from = count) %>%
  as.data.frame() -> trawl_lencomp



lencomp <- rbind(fishery1_lencomp, dredge_lencomp, trawl_lencomp)  

## use mean body size to fill missing data (pg 47 of SS 3.30.16 user manual)
use_meanbodywt <- 0

# ages ----

## number of age bins
N_agebins <- 18

## vector of age bins
agebin_vector <- 1:18

## number of ageing error matrices to generate
N_ageerror_definitions <- 2

## aging error matrix
ageerror <- read.csv("./data/stock_synthesis/ksh/age_error_defs.csv")

## age info (pg 57 of SS 3.30.16 user manual)
age_info <- data.frame(mintailcomp = c(0, 0, 0),
                       addtocomp = c(0.001, 0.001, 0.001),
                       combine_M_F = c(0, 0, 0),
                       CompressBins = c(0, 0, 0),
                       CompError = c(0, 0, 0),
                       ParmSelect = c(0, 0, 0),
                       minsamplesize = c(1, 1, 1))

## age composition
### fishery ages
fishery_ages %>%
  filter(district == "KSH") %>%
  # bin ages to align with agebin_vector (assuming its chronological)
  mutate(age = ifelse(age < agebin_vector[1], agebin_vector[1],
                      ifelse(age > agebin_vector[length(agebin_vector)], agebin_vector[length(agebin_vector)], 
                             age))) %>%
  dplyr::select(-district) %>%
  group_by(yr, Nsamp, age, .drop = F) %>%
  summarise_all(sum) %>%
  ungroup %>%
  mutate(month = 7, 
         fleet = 3, 
         sex = 0,
         part = 0,
         ageerr = 1,
         Lbin_lo = -1, 
         Lbin_hi = -1,
         age = paste0("a", age)) %>%
  dplyr::select(yr, month, fleet, sex, part, ageerr, Lbin_lo, Lbin_hi, Nsamp, age, count) %>%
  pivot_wider(names_from = age, values_from = count) -> fishery1_agecomp


### dredge survey ages
dredge_survey_ages %>%
  filter(bed_code == "KSH1") %>%
  # bin ages to align with agebin_vector (assuming its chronological)
  mutate(age = ifelse(age < agebin_vector[1], agebin_vector[1],
               ifelse(age > agebin_vector[length(agebin_vector)], agebin_vector[length(agebin_vector)], 
                      age))) %>%
  dplyr::select(-bed_code) %>%
  group_by(yr, Nsamp, age, .drop = F) %>%
  summarise_all(sum) %>%
  ungroup %>%
  mutate(month = 7, 
         fleet = 2, 
         sex = 0,
         part = 0,
         ageerr = 1,
         Lbin_lo = -1, 
         Lbin_hi = -1,
         age = paste0("a", age)) %>%
  dplyr::select(yr, month, fleet, sex, part, ageerr, Lbin_lo, Lbin_hi, Nsamp, age, n) %>%
  pivot_wider(names_from = age, values_from = n) -> dredge_agecomp

bind_rows(fishery1_agecomp, dredge_agecomp) %>%
  # reorder columns so ages are in order
  dplyr::select(1:9, 26:27, 10:23, 25, 24) %>%
  replace_na(list(a1 = 0, a2 = 0, a17 = 0, a18 = 0)) %>%
  as.data.frame() -> agecomp
  

# additional information ----

## environmental variables (pg 61 of SS 3.30.16 user manual)
N_environ_variables <- 0

## tagging data (pg 65 of SS 3.30.16 user manual)
do_tags <- 0 # none

## morph composition (pg 66 of SS 3.30.16 user manual)
morphcomp_data <- 0

## prior on selectivity or additional data (pg 67 of SS 3.30.16 user manual)
use_selectivity_priors <- 0

## output a .dat file ----
list(sourcefile = sourcefile, 
     type = type,
     ReadVersion = ReadVersion,
     styr = styr, 
     endyr = endyr, 
     nseas = nseas, 
     months_per_seas = months_per_seas, 
     Nsubseasons = Nsubseasons, 
     spawn_month = spawn_month, 
     Nsexes = Nsexes, 
     Nages = Nages, 
     Nareas = Nareas, 
     Nfleets = Nfleets, 
     fleetinfo = fleetinfo, 
     fleetnames = fleetnames, 
     surveytiming = surveytiming,    
     units_of_catch = units_of_catch,
     areas = areas, 
     catch = catch, 
     CPUEinfo = CPUEinfo, 
     N_discard_fleets = N_discard_fleets,
     use_meanbodywt = use_meanbodywt, 
     lbin_method = lbin_method,
     use_lencomp = use_lencomp,
     len_info = len_info, 
     N_lbins = N_lbins, 
     lbin_vector = lbin_vector, 
     lencomp = lencomp, 
     N_agebins = N_agebins,
     agebin_vector = agebin_vector,   
     N_ageerror_definitions = N_ageerror_definitions, 
     age_info = age_info, 
     agecomp = agecomp, 
     N_environ_variables = N_environ_variables, 
     do_tags = do_tags, 
     morphcomp_data = morphcomp_data, 
     use_selectivity_priors = use_selectivity_priors) %>%
  SS_writedat(., "./code/stock_synthesis/ksh/ksh_dat.ss", overwrite = T)
  



