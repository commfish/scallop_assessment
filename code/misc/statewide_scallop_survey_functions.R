# Functions for survey data analysis
# Tyler Jackson
# tyler.jackson@alaska.gov
# last updated 6/10/2020

# load ----
library(tidyverse)


# data prep functions ----

## trim necessary logbook data and compute mid lat/lon, area swept (nm2)
##  8 ft to nm conversion was taken from B. Williams code for consistency
## args
### x - raw logbook data (2019 - present format) + year
### drop - logical, drop unneeded columns, default = T
f_clean_log <- function(x, drop = T){ 
  x %>%
    mutate(lat = (start_lat + end_lat) / 2,
           lon = (start_lon + end_lon) /2,
           area_swept = distance * 0.00131663) %>%
    filter(haul_type == 10) -> tmp
  if(drop == T){
    tmp %>%
      dplyr::select(year, cruise, tow, haul, gear_perf, bed_code, lat, lon, avg_depth, area_swept)
  } else{tmp}
}

## compute total catch and cpue by tow 
## args
### x - raw catch data (2019 - present format)
### y - cleaned logbook data (see f_clean_log)
f_catch_by_tow <- function(x, y){
  
  # temporarily given small scallops a different rcode for ease of data summary
  x %>%
    mutate(rcode = ifelse((rcode == 74120 & samp_grp == 2), rcode + 9999999, rcode)) -> x
  
  # deal with all whole hauled samples
  x %>%
    filter(whole_haul == "Y") %>%
    group_by(tow, rcode, samp_grp) %>%
    summarise(samp_cnt = sum(samp_cnt, na.rm = T),
              samp_wt = sum(samp_wt, na.rm = T)) -> tmp
  
  # deal with non-whole hauled samples
  # join and include non-catch hauls
  # compute cpue
  x %>%
    filter(whole_haul == "N") %>%
    dplyr::select(tow, rcode, samp_cnt, samp_wt) %>%
    group_by(tow) %>%
    mutate(sub_wt = sum(samp_wt, na.rm = T)) %>%
    left_join(tmp %>%
                ungroup() %>%
                filter(rcode == 99997) %>%
                dplyr::select(tow, samp_wt) %>%
                rename(bulk_wt = samp_wt), by = "tow") %>%
    mutate(bulk_wt = bulk_wt + sub_wt) %>%
    mutate(samp_wt = bulk_wt / sub_wt * samp_wt,
           samp_cnt = bulk_wt / sub_wt * samp_cnt) %>%
    dplyr::select(-sub_wt, -bulk_wt) %>%
    bind_rows(tmp) %>%
    right_join(expand_grid(rcode = unique(x$rcode), 
                           tow = unique(y$tow)) %>%
                 mutate(samp_grp = case_when(rcode == 74120 ~ 1,
                                             rcode == (74120 + 9999999) ~ 2)), 
               by = c("rcode", "tow", "samp_grp"))  %>%
    filter(rcode != 99997) %>%
    replace_na(list(samp_wt = 0)) %>%
    mutate(samp_cnt = ifelse(samp_wt == 0, 0, samp_cnt)) %>%
    right_join(y, by = "tow") %>%
    mutate(cpue_cnt = ifelse(rcode %in% c(74120, 74120 + 9999999), samp_cnt / (0.83*area_swept), samp_cnt / area_swept),
           cpue_wt = ifelse(rcode %in% c(74120, 74120 + 9999999), samp_wt / (0.83*area_swept), samp_wt / area_swept)) %>%
    left_join(x %>%
                dplyr::select(rcode, comname) %>%
                distinct(),
              by = "rcode") %>%
    dplyr::select(6, 7, 1, 8:13, 14, 2, 17, 5, 3:4, 15:16) %>%
    # change rcode of small scallops back
    mutate(rcode = ifelse(rcode == (74120 + 9999999), 74120, rcode))
}

## separate SHAW data from all speciemen data
## args
### x - raw specimen data (2019 - present format)
### y - cleaned catch by tow data (see output of f_catch_by_tow)
f_get_shaw <- function(x, y) {
  x %>%
    filter(!is.na(whole_wt)) %>%
    dplyr::select(-samptime) %>%
    left_join(y %>%
                dplyr::select(tow, year, bed_code), 
              by = "tow") %>%
    dplyr::select(16:17, 1:15)
} 

## separate shell height and damage data from all specimen data, compute sample factor
## args
### x - raw specimen data (2019 - present format)
### y - cleaned catch by tow data (see output of f_catch_by_tow)
f_get_shad <- function(x, y){
  specimen %>%
    dplyr::select(-whole_wt, -sex, -shell_num, -shell_id, -gonad, -meat_condition, -mud_blister,
                  -shell_worm, -shell_retained, -meat_weight, -samptime) %>%
    group_by(tow, rcode, samp_grp, size, damage) %>%
    summarise(count = n()) %>%
    group_by(tow, samp_grp) %>%
    mutate(n_measured = sum(count)) %>%
    left_join(y %>%
                dplyr::select(year, cruise, tow, haul, bed_code, lat, lon, avg_depth,
                               area_swept, rcode, samp_grp, samp_cnt),
              by = c("tow", "rcode", "samp_grp")) %>%
    mutate(sample_factor = samp_cnt * (count / n_measured)) %>%
    dplyr::select(year, cruise, tow, haul, bed_code, lat, lon, avg_depth, area_swept,
                  rcode, samp_grp, size, damage, sample_factor)
}

## boostrap ci for abundance and biomass estimate
## args
### split - splits from bootstrap resampling
### strata - bed strata data
boot_ci <- function(split, strata){
  rsample::analysis(split) %>%
    ungroup() %>%
    summarise(cpue_cnt = mean(cpue_cnt),
              cpue_wt = mean(cpue_wt))
}



