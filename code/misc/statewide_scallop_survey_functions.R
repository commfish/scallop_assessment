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
    mutate(lat = (lat_start + lat_end) / 2,
           lon = (lon_start + lon_end) /2,
           area_swept = distance_nm * 0.00131663) %>%
    filter(haul_type %in% c("10", "Standard")) %>%
    rename(year = cruise_year) -> tmp
  if(drop == T){
    tmp %>%
      dplyr::select(year, cruise, tow, haul, gear_perf, district, bed_code, lat, lon, avg_depth, area_swept)
  } else{tmp}
}

## compute total catch and cpue by tow 
## args
### x - raw catch data (2019 - present format)
### y - cleaned logbook data (see f_clean_log)
f_catch_by_tow <- function(x, y){
  # remove clappers and empty shells
  x %>%
    filter(!(comname %in% c("Empty shells", "Clapper", "Clappers", "Empty_Shells, Scallop", "Empty Scallop Shells"))) %>%
    mutate(comname = ifelse(grepl("eathervane", comname), "weathervane scallop", comname)) -> x
  
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
f_get_shaw <- function(x) {
  x %>%
    filter(!is.na(sex)) 
} 

## separate shell height and damage data from all specimen data, compute sample factor
## args
### x - raw specimen data (2019 - present format)
### y - cleaned catch by tow data (see output of f_catch_by_tow)
f_get_shad <- function(x, y){
  x %>%
    filter(samp_grp %in% c(1, 2)) %>%
    dplyr::select(-whole_wt, -sex, -shell_num, -gonad, -meat_condition, -mud_blister,
                  -shell_worm, -shell_retained, -meat_wt) %>%
    group_by(year, tow, samp_grp, size, damage) %>%
    summarise(count = n()) %>%
    group_by(tow, samp_grp) %>%
    mutate(n_measured = sum(count)) %>%
    left_join(y %>%
                filter(rcode == 74120) %>%
                dplyr::select(year, cruise, tow, haul, district, bed_code, lat, lon, avg_depth,
                               area_swept, rcode, samp_grp, samp_cnt),
              by = c("tow", "samp_grp", "year")) %>%
    mutate(sample_factor = samp_cnt * (count / n_measured)) %>%
    dplyr::select(year, cruise, tow, haul, district, bed_code, lat, lon, avg_depth, area_swept,
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

## compile dredge survey shell height composition
## args
### shad - cleaned shad data
### raw_speciment_data - raw specimen data
### tows - cleaned tow data 
### sh_bin - optional shell height bin width, leave null if not binning
f_dredge_sh_comp <- function(shad, raw_specimen, tows, sh_bin = NULL){
  
  shad %>%
    group_by(year, district, bed_code, size) %>%
    summarise(n = sum(sample_factor, na.rm = T)) %>%
    mutate(prop = n / sum(n, na.rm = T)) %>%
    # join to n_measured
    left_join(specimen %>%
                dplyr::select(tow, size, samp_grp) %>%
                ## remove extra tow
                filter(tow != 19010054, samp_grp %in% 1:2, !is.na(size)) %>%
                left_join(tows) %>%
                group_by(year, district, bed_code) %>%
                summarise(n_measured = n())) %>%
    rename(sh = size) %>%
    #rearrange columnss
    dplyr::select(year, district, bed_code, n_measured, sh, n, prop) -> out
  
  # bin and aggregate if appropriate
  if(!is.null(sh_bin)){
    out %>%
      mutate(sh = floor(sh / sh_bin) * sh_bin) %>%
      group_by(year, district, bed_code, n_measured, sh) %>%
      summarise(n = sum(n),
                prop = sum(prop)) -> out
  }
  
  return(out)
}

