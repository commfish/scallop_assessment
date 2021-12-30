# Functions for survey data analysis
# Tyler Jackson
# tyler.jackson@alaska.gov
# last updated 6/10/2020

# load ----
library(tidyverse)
library(patchwork)
library(FNGr)
library(scales)

## global options
### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
### set theme (from FNGr)
theme_set(theme_sleek() + theme(legend.position = "bottom"))

# load bed strata
strata <- read_csv("./data/statewide_scallop_survey/bed_strata.csv")


# data prep functions ----

## trim necessary logbook data and compute mid lat/lon, area swept (nm2)
##  8 ft to nm conversion was taken from B. Williams code for consistency
## args
### x - raw logbook data (2019 - present format) + year
### drop - logical, drop unneeded columns, default = T
f_clean_log <- function(x, drop = T){ 
  x %>%
    as_tibble() %>%
    mutate(lat = (lat_start + lat_end) / 2,
           lon = (lon_start + lon_end) / 2,
           area_swept = distance_nm * 0.00131663,
           depth_avg = ifelse(is.na(depth_avg), (depth_start + depth_end) / 2, depth_avg)) %>%
    filter(haul_type %in% c("10", "Standard")) %>%
    rename(year = cruise_year,
           bed_code = bed_name) -> tmp
  if(drop == T){
    tmp %>%
      dplyr::select(year, cruise, tow, haul, perform, district, bed_code, lat, lon, depth_avg, area_swept)
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
    mutate(comname = ifelse(grepl("eathervane", comname), "weathervane scallop", comname)) %>%
  
  # temporarily give small scallops a different rcode for ease of data summary

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
    mutate(rcode = ifelse(rcode == (74120 + 9999999), 74120, rcode)) %>%
    ungroup
}

## separate SHAW data from all speciemen data
## args
### x - raw specimen data (2019 - present format)
f_get_shaw <- function(x) {
  x %>%
    as_tibble() %>%
    rename(year = cruise_year,
           bed_code = bed_name) %>%
    filter(!is.na(sex)) 
} 

## separate shell height and damage data from all specimen data, compute sample factor
## args
### x - raw specimen data (2019 - present format)
### y - cleaned catch by tow data (see output of f_catch_by_tow)
f_get_shad <- function(x, y){
  x %>%
    as_tibble() %>%
    filter(samp_grp %in% c(1, 2)) %>%
    dplyr::select(-whole_wt, -sex, -shell_num, -gonad, -meat_condition, -mud_blister,
                  -shell_worm, -shell_retained, -meat_wt) %>%
    rename(year = cruise_year,
           bed_code = bed_name) %>%
    group_by(year, tow, samp_grp, shell_height, damage) %>%
    summarise(count = n()) %>%
    group_by(tow, samp_grp) %>%
    mutate(n_measured = sum(count)) %>% ungroup %>%
    left_join(y %>%
                filter(rcode == 74120) %>%
                dplyr::select(year, cruise, tow, haul, district, bed_code, lat, lon, depth_avg,
                               area_swept, rcode, samp_grp, samp_cnt),
              by = c("tow", "samp_grp", "year")) %>%
    mutate(sample_factor = samp_cnt * (count / n_measured)) %>%
    dplyr::select(year, cruise, tow, haul, district, bed_code, lat, lon, depth_avg, area_swept,
                  rcode, samp_grp, shell_height, damage, sample_factor)
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


# plotting functions ----

## plot of abundance, round biomass, or meat biomass timeseries by bed
### data = abundance biomaass data
### group = sample group (1 - large, 2 - small)
### years = year range to plot
f_plot_abundance <- function(data, group, years){
  
  data %>%
    left_join(strata, by = "bed_code") -> tmp
  tmp %>%
    # filter for sample group
    filter(samp_grp == group) %>%
    # plot
    ggplot()+
    geom_point(aes(x = year, y = abundance/1e6, color = bed_code))+
    geom_line(aes(x = year, y = abundance/1e6, group = bed_code, color = bed_code))+
    geom_errorbar(aes(x = year, ymin = abund_log_l95/1e6, ymax = abund_log_u95/1e6, color = bed_code),
                  width = 0.05)+
    scale_x_continuous(limits = c(years[1]-0.25, years[length(years)]+0.25), breaks = years)+
    labs(x = NULL, y = "Abundance (millions)", color = NULL)+
    ggtitle(tmp$district_full)+
    theme(legend.justification = c(0, 1),
          legend.position = c(0, 1),
          plot.title = element_text(hjust = 0.5))+
    scale_color_manual(values = cb_palette[1:length(unique(data$bed_code))]) -> p
  
  if(length(unique(data$bed_code)) == 1) {
    p + theme(legend.position = "none") -> p}
  
  return(p)
  
}
f_plot_biomass <- function(data, group, years){
  
  data %>%
    left_join(strata, by = "bed_code") -> tmp
  tmp %>%
    # filter for sample group
    filter(samp_grp == group) %>%
    # plot
    ggplot()+
    geom_point(aes(x = year, y = biomass/1e3, color = bed_code))+
    geom_line(aes(x = year, y = biomass/1e3, group = bed_code, color = bed_code))+
    geom_errorbar(aes(x = year, ymin = biomass_log_l95/1e3, ymax = biomass_log_u95/1e3, color = bed_code),
                  width = 0.05)+
    scale_x_continuous(limits = c(years[1]-0.25, years[length(years)]+0.25), breaks = years)+
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL, y = "Round Biomass (1,000 lb)", color = NULL)+
    ggtitle(tmp$district_full)+
    theme(legend.justification = c(0, 1),
          legend.position = c(0, 1),
          plot.title = element_text(hjust = 0.5))+
    scale_color_manual(values = cb_palette[1:length(unique(data$bed_code))]) -> p
  
  if(length(unique(data$bed_code)) == 1) {
    p + theme(legend.position = "none") -> p}
  
  return(p)
  
}
f_plot_mt_biomass <- function(data, years){
  
  data %>%
    left_join(strata, by = "bed_code") -> tmp
  tmp %>%
    # plot
    ggplot()+
    geom_point(aes(x = year, y = mw_biomass/1e3, color = bed_code))+
    geom_line(aes(x = year, y = mw_biomass/1e3, group = bed_code, color = bed_code))+
    geom_errorbar(aes(x = year, ymin = ln_l95/1e3, ymax = ln_u95/1e3, color = bed_code),
                  width = 0.05)+
    scale_x_continuous(limits = c(years[1]-0.25, years[length(years)]+0.25), breaks = years)+
    scale_y_continuous(labels = scales::comma)+
    labs(x = NULL, y = "Meat Biomass (1,000 lb)", color = NULL)+
    ggtitle(tmp$district_full)+
    theme(legend.justification = c(0, 1),
          legend.position = c(0, 1),
          plot.title = element_text(hjust = 0.5))+
    scale_color_manual(values = cb_palette[1:length(unique(data$bed_code))]) -> p
  
  if(length(unique(data$bed_code)) == 1) {
    p + theme(legend.position = "none") -> p}
  
  return(p)
  
}

## plot of shell height composition
### data = SHAD data
f_plot_sh_comp <- function(data) {
  data %>%
    left_join(strata, by = "bed_code") -> tmp
  tmp %>%
    ggplot()+
    geom_histogram(aes(x = shell_height, y = ..density.., weight = sample_factor, fill = bed_code),
                   binwidth = 3, color = "black")+
    facet_wrap(~year, ncol = 1, scales = "free_y")+
    labs(x = "Shell Height (mm)", y = "Density", fill = NULL)+
    scale_fill_manual(values = cb_palette[1:length(unique(data$bed_code))])+
    ggtitle(tmp$district_full)+
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) -> p
  
  if(length(unique(data$bed_code)) == 1) {
    p + theme(legend.position = "none") -> p}
  
  return(p)
}

## plot weak meats
f_plot_wkmt <- function(data, years){
  
  data %>%
    left_join(strata, by = "bed_code") -> tmp
  tmp %>%
    # plot
    ggplot()+
    geom_point(aes(x = year, y = wk_mts, color = bed_code))+
    geom_line(aes(x = year, y = wk_mts, group = bed_code, color = bed_code))+
    scale_x_continuous(limits = c(years[1]-0.25, years[length(years)]+0.25), breaks = years)+
    labs(x = NULL, y = "Proportion Weak Meats", color = NULL)+
    ggtitle(tmp$district_full)+
    theme(legend.justification = c(0, 1),
          legend.position = c(0, 1),
          plot.title = element_text(hjust = 0.5))+
    scale_color_manual(values = cb_palette[1:length(unique(data$bed_code))]) -> p
  
  if(length(unique(data$bed_code)) == 1) {
    p + theme(legend.position = "none") -> p}
  
  return(p)
  
}

## stack plots stored in tibble using library patchwork
### data = data with list of plots as a title
### plots = name of column that is list of ggplot grobs
f_stack_plots <- function(data, plots){
  # extract list and give elements names
  data %>% 
    ungroup () %>%
    pull(plots) -> tmp
  names(tmp) <- letters[1:length(tmp)]
  
  # create string to evaluate
  string <- NULL
  for(i in 1:length(names(tmp))){
    if(i < length(names(tmp))){string <- paste0(string, "tmp$", names(tmp)[i], "/")}
    if(i == length(names(tmp))){string <- paste0(string, "tmp$", names(tmp)[i])}
  }
  
  return(eval(parse(text = string)))
}
f_sxs_plots <- function(data, plots){
  # extract list and give elements names
  data %>% 
    ungroup () %>%
    pull(plots) -> tmp
  names(tmp) <- letters[1:length(tmp)]
  
  # create string to evaluate
  string <- NULL
  for(i in 1:length(names(tmp))){
    if(i < length(names(tmp))){string <- paste0(string, "tmp$", names(tmp)[i], "|")}
    if(i == length(names(tmp))){string <- paste0(string, "tmp$", names(tmp)[i])}
  }
  
  return(eval(parse(text = string)))
}
