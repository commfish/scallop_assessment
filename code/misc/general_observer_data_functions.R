# notes ----
## general functions applicable to all observer data analysis
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/2/27

# load libraries ----
library(here)
library(lubridate)
library(patchwork)
library(scales)
library(tidyverse)
library(mgcv)
library(mgcViz)
library(spatstat)

# functions ----

# rename catch data to appropriate style
# args:
## x - catch data (as downloaded directly from wiki)
f_catch_rename <- function(x){
  names(x) <- c("fishery", "district", "adfg", "trip_id", "haul_id", 
                "haul", "gear_perf", "haul_sampled", "set_date", "bed_code", "set_lat",
                "set_lon", "statarea", "depth", "dredge_count", 
                "dredge_width", "dredge_down", "dredge_up", "duration", 
                "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                "rtnd_basket", "scallop_count", "round_weight", "meat_weight", "est_yield",
                "tot_rtnd_basket", "tot_day_meat_weight")
  x
}

# rename bycatch data to appropriate style
# args:
## x - daily bycatch data (as downloaded directly from wiki)
f_bycatch_rename <- function(x){
  names(x) <- c("fishery", "district", "adfg", "haul_id", "haul", "gear_perf", "set_date", "bed_code", "dredge_hrs", 
                    "est_rnd_wt", "mt_wt", "sample_hrs", "bairdi_count", "opilio_count",
                    "dungeness_count", "king_count", "halibut_count", "disc_count", "disc_wt", "broken_wt",
                    "rem_disc_wt", "clapper_count")
  x
}

# rename crab_size data to appropriate style
# args:
## x - crab size data (as downloaded directly from wiki)
f_crab_size_rename <- function(x){
  names(x) <- c("fishery", "district", "race_code", "sex", "cw", "samp_frac")
  x
}

# rename shell_height data to appropriate style
# args:
## x - shell height data (as downloaded directly from wiki)
f_shell_height_rename <- function(x){
  names(x) <- c("fishery", "district", "haul_id", "adfg", "rtnd_disc", "sh", "shell_num")
  x
}

# add season to data (based on Fishery field)
# args:
## x - tibble of observer or logbook data
## fishery_col - name of column that denotes fishery. Default = "Fishery"
f_add_season <- function(x, fishery_col = "fishery"){
  x %>%
    pull(grep(fishery_col, names(.))) %>%
    str_sub(., 3, 4) %>%
    as.numeric() %>%
    tibble(season = .) %>%
    mutate(season = ifelse(season < 80, season + 2000, season + 1900),
           season = factor(paste0(season, "/", substring(season + 1, 3, 4)))) %>%
    bind_cols(x)
}

f_season_yr <- function(season){
  as.numeric(substring(season, 1, 4))
}

# revise district to align with current mgmt structure
# args: x - any tibble containing the field 'district' or 'District'
f_revise_district <- function(x){
  
  if(!("district" %in% names(x))){
    x %>%
      mutate(district = ifelse(bed_code %in% c("KSH4", "KSH5", "KSH6", "KSH7"), "KSW", district),
             district = ifelse(district == "KSH" & ((set_lat < 57.7 & set_lon <= -154.35) | (is.na(set_lat))),
                               "KSW", district),
             district = ifelse(district %in% c("D16", "D", "YAK"),
                               "YAK", district))
  } else{
    x %>%
      mutate(district = ifelse(bed_code %in% c("KSH4", "KSH5", "KSH6", "KSH7"), "KSW", district),
             district = ifelse(district == "KSH" & ((set_lat < 57.7 & set_lon <= -154.35) | (is.na(set_lat))),
                               "KSW", district),
             district = ifelse(district %in% c("D16", "D", "YAK"),
                               "YAK", district))
  }
}

# data mgmt for raw catch data prior to downstream analysis
# args:
## x - catch data (as downloaded directly from wiki)
f_clean_catch <- function(x) {
  x %>% 
    ## rename fields in current data (2009 - present)
    f_catch_rename() %>%
    ## drop phantom rows
    drop_na(haul_id) %>%
    ## add Season to data
    f_add_season() %>%
    ## classify Karluk bed as KSW district instead of KSH
    f_revise_district() %>% 
    ## coerce date to date class
    mutate(set_date = lubridate::mdy(set_date)) %>%
    ## remove tows with zero dredge hours (logbook mistake)
    filter(dredge_hrs != 0)  %>%
    ## coerce date to date class
    ## fix issue with missing basket weight in 2018/19
    mutate(round_weight = ifelse(season == "2018/19" & district == "O",
                                 54.1 * rtnd_basket, round_weight),
           bed_code = ifelse(bed_code == "YAKB", "EK1", bed_code))
}
  
# data mgmt for raw bycatch data prior to downstream analysis
# args:
## x - bycatch data (as downloaded directly from wiki)  
## catch - cleaned catch data
f_clean_bycatch <- function(x, catch) { 
  bycatch_wiki %>%
    ## rename fields in bycatch data (2009 - present)
    f_bycatch_rename() %>%
    ## add Season to data
    f_add_season() %>%
    ## coerce date to date class
    mutate(set_date = lubridate::mdy(set_date)) %>%
    ## remove district info
    dplyr::select(-1:-4) %>%
    ## get beds/district info from catch data
    ## ie no bycatch data that does not match with catch data !
    right_join(catch %>% dplyr::select(season, adfg, district, haul_id, set_lat, set_lon), 
              by = c("haul_id")) 
}

# data mgmt for raw crab size composition data prior to downstream analysis
# args:
## x - crab size data (as downloaded directly from wiki) 
f_clean_crab_size <- function(x) { 
  x %>%
    ## reason fields in crab size data (2009 - present)
    f_crab_size_rename() %>%
    ## drp the empty column
    dplyr::select(-7) %>%
    ## add Season to data
    f_add_season() %>%
    ## revise district
    ## unable to correct data for KSH / KSW
    mutate(district = ifelse(district %in% c("D", "YAK", "D16"), "YAK", district)) %>%
    ## add Species and Sex
    mutate(species = case_when(race_code == 68560 ~ "tanner_crab",
                               race_code == 68541 ~ "snow_crab"),
           species = factor(species, levels = c("tanner_crab", "snow_crab")),
           sex = case_when(sex == 1 ~ "Male",
                           sex == 2 ~ "Female",
                           sex == 3 ~ "Unknown"),
           sex = factor(sex, levels = c("Male", "Female", "Unknown")))
}

# data mgmt for raw crab size composition data prior to downstream analysis
# args:
## x - crab size data (as downloaded directly from wiki) 
## catch - cleaned catch data
f_clean_sh <- function(x, catch) { 
  x %>%
    ## rename fields in shell_height data (2009 - present)
    f_shell_height_rename() %>%
    ## add season to data
    f_add_season() %>%
    ## revise district as in catch data
    mutate(district = catch$district[match(.$haul_id, catch$haul_id)]) 
}

# data mgmt for raw meat weight data prior to downstream analysis
# args:
## x - meat weight data (as downloaded directly from wiki) 
## catch - cleaned catch data
f_clean_meat <-function(x, catch){
  
  x %>%
    ## rename haul_id to join to catch
    rename_all(tolower) %>%
    ## add season to meat weight data
    f_add_season(fishery_col = "fishery") %>%
    ## create haul id for 2020/21
    mutate(set_date = mdy(set_date),
           haul_id = ifelse(season == "2020/21", 
                            paste0(fishery,
                                   sprintf("%06d", adfg),
                                   year(set_date),
                                   sprintf("%02d", month(set_date)),
                                   sprintf("%02d", day(set_date)),
                                   sprintf("%04d", haul)), haul_id)) %>%
    ## join with catch data to get location (district, bed)
    left_join(catch %>%
                dplyr::select(haul_id, district, bed_code, set_lat, set_lon),
              by = c("haul_id")) %>%
    ## add retained - discard factor
    mutate(rtnd_disc = ifelse(shell_num < 11, "retained", "discarded")) -> meat
}
  
 
# quick summary of fishery statistics
# args:
## x - cleaned catch data
## district - abbrev. for any districts to summarise over
## ghl - logical include cmobine ghl. Default = F
## path - optional. Writes .csv file to path provided
f_fish_stats <- function(x, dist, add_ghl = F, path){
  # add Season if it is not found
  if(!("season" %in% names(x))) {x <- add_season(x)}
  # summarize catch data
  x %>%
    # filter to district
    filter(district %in% dist) %>%
    # summarise data 
    group_by(season) %>%
    summarise(mt_wt = round(sum(meat_weight, na.rm = T)),
              rnd_wt = round(sum(round_weight, na.rm = T)),
              rnd_num = round(sum(scallop_count, na.rm = T)),
              dredge_hrs = sum(dredge_hrs, na.rm = T),
              number_hauls = n(),
              mw_cpue = mt_wt / dredge_hrs,
              rw_cpue = rnd_wt / dredge_hrs) -> tmp
  # add ghl if necessary
  if(add_ghl == T){
    if(!exists("ghl")){stop("tibble named 'ghl' not found")}
    else{
    ghl %>%
      filter(district %in% dist) %>%
      group_by(season) %>%
      summarise(ghl = sum(ghl, na.rm = T)) %>%
      left_join(., tmp, by = "season") %>%
      replace_na(list(mt_wt = 0, rnd_wt = 0, rnd_num = 0, dredge_hrs = 0, number_hauls = 0)) %>%
      filter(as.numeric(substring(season, 1, 4)) > 2008) -> tmp
    }
  }
  # write to a csv if necessary
  if(missing(path)) {tmp}
  else{
    write_csv(tmp, path)
    tmp
  }
}


# 2D density polygon dataframe for ggplot2 map, no weighting
## args:
## data: tibble contain plotting data
## x: name of column denoting longitude
## y: name of column denoting latitude
## h: 2D bandwidth. Default = c(0.1, 0.1).
## facet: Optional. Will facet wrap by variable provided.
f_density_2D <- function(data, x, y, h = c(0.1, 0.1), facet){
  if(!missing(facet)){
  ### create facet labels for ggplot_build workaround of 2D density plot issue
  tibble(var = unique(pull(data, facet)),
         PANEL = factor(1:length(unique(pull(data, facet))))) -> pan_join
  
  data %>%
    rename(long = x,
           lat = y, 
           wrap = facet) %>%
  # create map layers to pull data from
    ggplot()+
    stat_density_2d(aes(x = long, y = lat, fill = stat(level)), geom = "polygon", 
                    h = h)+
    scale_fill_gradientn(colors = topo.colors(10))+
    facet_wrap(~wrap) -> density
  # pull data and join facet labels
  ggplot_build(density)$data[[1]] %>%
    left_join(pan_join, by = "PANEL") -> x
  names(x)[ncol(x)] <- facet
  as_tibble(x)
  }
  else{
    data %>%
      rename(long = x,
             lat = y) %>%
      # create map layers to pull data from
      ggplot()+
      stat_density_2d(aes(x = long, y = lat, fill = stat(level)), geom = "polygon", 
                      h = h)+
      scale_fill_gradientn(colors = topo.colors(10)) -> density
    # pull data and join facet labels
    ggplot_build(density)$data[[1]] 
  }
}


## graphical extent of roundweight catch (mean distance between dredges in graphical units)
### args:
### x - logbook catch data
### quant - cut off quantile for contribution to catch. Default = 0.9.
f_extent_catch <- function(x, quant = 0.9){
  x %>%
    arrange(-round_weight) %>%
    mutate(cum_prop = cumsum(round_weight) / sum(round_weight, na.rm = T)) %>%
    filter(cum_prop <= quant) %>%
    dplyr::select(set_lon, set_lat) %>%
    dist() %>%
    mean()
}


## compute standardized cpue based on nominal round weight catch data
### args:
### x - catch data to standardize containing fields for covariates. Must include 
### 'Season', 'Bed', 'Vessel', 'Month', 'depth', 'set_lon'.
### path - file path to save plots. Optional, if provided, function saves effect plots of Month, Season, Bed, and Vessel
### Outputs a point estimate for each season and effects plots of Season, Bed, 
### Vessel, and Month
### lon - include covariate set_lon T/F
f_standardize_cpue <- function(x, path, lon = T){
  mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  x %>%
    # filter for only satisfacory dredges
    # remove anomallous data
    filter(gear_perf == 1,
           bed != "Unknown",
           depth >= quantile(depth, 0.025, na.rm = T),
           depth <= quantile(depth, 0.975, na.rm = T),
           round_weight > 0) %>% 
    # compute cpue by dredge
    # cut off prefix of season for plotting
    mutate(rw_cpue = round_weight / dredge_hrs,
           season = factor(substring(season, 3, 4))) %>%
    # only complete data
    dplyr::select(season, vessel, month, bed, depth, set_lon, rw_cpue) %>%
    filter(complete.cases(.)) -> y
  
  # set reference levels
  y$bed = relevel(y$bed, as.character(mode(y$bed)))
  y$month = relevel(y$month, as.character(mode(y$month)))
  y$vessel = relevel(y$vessel, as.character(mode(y$vessel)))
  
  # fit model
  if(lon == T) {
    mod <- bam(rw_cpue ~ s(depth, k = 4, by = bed)  + s(set_lon, by = bed) + 
                            month + vessel + bed + season, data = y, gamma = 1.4, 
                          family = Gamma(link = log), select = T)
    
    # print diagnostics
    mod_viz <- getViz(mod)
    print(check(mod_viz,
                a.qq = list(method = "tnorm",
                            a.cipoly = list(fill = "light blue")),
                a.respoi = list(size = 0.5),
                a.hist = list(bins = 10)))
    if(!missing(path)){
      # save vessel, month, Season, and Bed effect
      n_start <- length(unique(y$bed)) + length(unique(y$bed)) + 1
      n_end <- n_start + 3
      png(path, height = 4, width = 7, units = "in", res = 300)
      print(plot(mod_viz, allTerms = F, select = (n_start:n_end))+ 
              l_points(size = 1, col = "grey")+
              l_ciBar(linetype = 1)+
              l_fitPoints(size = 1, col = 1)+
              geom_hline(yintercept = 0, linetype = 2)+
              theme_sleek()+
              theme(axis.text = element_text(size = 9)), pages = 1)
      dev.off()
    }
    }
  else{
    mod <- bam(rw_cpue ~ s(depth, k = 4, by = bed) + 
                 month + vessel + bed + season, data = y, gamma = 1.4, 
               family = Gamma(link = log), select = T)
    # print diagnostics
    mod_viz <- getViz(mod)
    print(check(mod_viz,
                a.qq = list(method = "tnorm",
                            a.cipoly = list(fill = "light blue")),
                a.respoi = list(size = 0.5),
                a.hist = list(bins = 10)))
    if(!missing(path)){
      # save vessel, month, Season, and Bed effect
      n_start <- length(unique(y$bed)) + 1
      n_end <- n_start + 3
      png(path, height = 4, width = 7, units = "in", res = 300)
      print(plot(mod_viz, allTerms = F, select = (n_start:n_end))+ 
              l_points(size = 1, col = "grey")+
              l_ciBar(linetype = 1)+
              l_fitPoints(size = 1, col = 1)+
              geom_hline(yintercept = 0, linetype = 2)+
              theme_sleek()+
              theme(axis.text = element_text(size = 9)), pages = 1)
      dev.off()
    }
  }

  # extract year effect
  tibble(par = names(mod$coefficients), 
         est = mod$coefficients,
         se = sqrt(diag(vcov(mod)))) %>%
    filter(grepl("Intercept|season", names(mod$coefficients))) %>%
    mutate(par_est = ifelse(par != "(Intercept)", est + est[par == "(Intercept)"], est),
           par_se = se,
           std_cpue = exp(par_est + se/2),
           se = std_cpue * se,
           season = 2000 + as.numeric(levels(y$season))) %>%
    arrange(as.numeric(as.character(season))) %>%
    dplyr::select(season, par_est, par_se, std_cpue, se) -> out 
  
  return(out)
  
}


## compute standardized cpue based on nominal round weight catch data IN DISTRICTS WITH ONLY 1 BED
### args:
### x - catch data to standardize containing fields for covariates. Must include 
### 'Season', Vessel', 'Month', 'depth', 'set_lon'.
### path - file path to save plots. Optional, if provided, function saves effect plots of Month, Season, and Vessel
### Outputs a point estimate for each season and effects plots of Season, Vessel, and Month
f_standardize_cpue2 <- function(x, path){
  mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  x %>%
    # filter for only satisfacory dredges
    # remove anomallous data
    filter(gear_perf == 1,
           bed != "Unknown",
           depth >= quantile(depth, 0.025, na.rm = T),
           depth <= quantile(depth, 0.975, na.rm = T),
           round_weight > 0) %>% 
    # compute cpue by dredge
    # cut off prefix of season for plotting
    mutate(rw_cpue = round_weight / dredge_hrs,
           season = factor(substring(season, 3, 4))) %>%
    # only complete data
    dplyr::select(season, vessel, month, bed, depth, set_lon, rw_cpue) %>%
    filter(complete.cases(.)) -> y
  
  # set reference levels
  y$month = relevel(y$month, as.character(mode(y$month)))
  y$vessel = relevel(y$vessel, as.character(mode(y$vessel)))
  
  # fit model
  mod <- bam(rw_cpue ~ s(depth, k = 4) + s(set_lon) + 
               month + vessel + season, data = y, gamma = 1.4, 
             family = Gamma(link = log), select = T)
  # print diagnostics
  mod_viz <- getViz(mod)
  print(check(mod_viz,
              a.qq = list(method = "tnorm",
                          a.cipoly = list(fill = "light blue")),
              a.respoi = list(size = 0.5),
              a.hist = list(bins = 10)))
  if(!missing(path)){
    # save vessel, month, Season
    n_start <- 3
    n_end <- n_start + 4
    png(path, height = 4, width = 7, units = "in", res = 300)
    print(plot(mod_viz, allTerms = F, select = (n_start:n_end))+ 
            l_points(size = 1, col = "grey")+
            l_ciBar(linetype = 1)+
            l_fitPoints(size = 1, col = 1)+
            geom_hline(yintercept = 0, linetype = 2)+
            theme_sleek()+
            theme(axis.text = element_text(size = 9)), pages = 1)
    dev.off()
  }
  
  # extract year effect
  tibble(par = names(mod$coefficients), 
         est = mod$coefficients,
         se = sqrt(diag(vcov(mod)))) %>%
    filter(grepl("Intercept|season", names(mod$coefficients))) %>%
    mutate(par_est = ifelse(par != "(Intercept)", est + est[par == "(Intercept)"], est),
           par_se = se,
           std_cpue = exp(par_est + se/2),
           se = std_cpue * se,
           season = 2000 + as.numeric(levels(y$season))) %>%
    arrange(as.numeric(as.character(season))) %>%
    dplyr::select(season, par_est, par_se, std_cpue, se) -> out 
  
  return(out)
}  


## estimate discards two different ways to account for observer data collection error in YAK and KSH in 2018
### args:
### nh - dummy variable representing day when observer made mistake
### data - discard/bycatch report by day
f_disc_nh <- function(nh, data){
  
  if(nh == F){
    data %>%
      summarise(effort = sum(dredge_hrs, na.rm = T),
                discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt, na.rm = T) / sum(sample_hrs, na.rm = T),
                discard_lb = discard_rate_lb * effort,
                disc_per_lb = sum(disc_count, na.rm = T) / sum(disc_wt, broken_wt, na.rm = T),
                discard_rate_num = (sum(disc_count, na.rm = T) + disc_per_lb * sum(rem_disc_wt, na.rm = T)) / sum(sample_hrs, na.rm = T),
                discard_num = discard_rate_num * effort)
  } else{
    data %>%
      summarise(effort = sum(dredge_hrs, na.rm = T),
                discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt, na.rm = T) / sum(sample_hrs, na.rm = T),
                discard_lb = discard_rate_lb * effort,
                disc_per_lb = sum(disc_count, na.rm = T) / sum(disc_wt, na.rm = T),
                discard_rate_num = (sum(disc_count, na.rm = T) + disc_per_lb * sum(rem_disc_wt, na.rm = T)) / sum(sample_hrs, na.rm = T),
                discard_num = discard_rate_num * effort)
  }
}

## compile observer shell height composition
### args:
### sh_data - dataframe of cleaned shell height data
### catch_data - dataframe of cleaned catch data
### bycatch_data - dataframe of cleaned bycatch data
### sh_bin - shell height bin size in mm, if not bins, do not include
f_observer_sh_comp <- function(sh_data, catch_data, bycatch_data, sh_bin = NULL){
  
  
  
  ## get number of length samples of retained catch by district
  data %>%
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
  
  ## expand to bycatch
  bycatch %>%
    # compute a daily discard rate (lbs/dregde hr)
    group_by(Season, District, Set_date) %>%
    summarise(disc_wt = sum(disc_wt, broken_wt, rem_disc_wt),
              sample = sum(sample_hrs)) %>%
    group_by(Season, District) %>%
    mutate(disc_rate = ifelse(sample != 0,
                              disc_wt / sample,
                              sum(disc_wt) / sum(sample))) %>%
    # join to catch data by haul
    dplyr::select(Season, District, Set_date, disc_rate) %>%
    right_join(catch, by = c("Season", "District", "Set_date")) %>%
    # estimate discards by haul
    mutate(disc_est_lbs = dredge_hrs * disc_rate) %>%
    # estimate weights for shell height histogram (prop of annual catch)
    dplyr::select(Season, District, Haul_ID, round_weight, disc_est_lbs) %>%
    pivot_longer(c(round_weight, disc_est_lbs),
                 names_to = "Rtnd_disc", values_to = "wt_lbs") %>%
    mutate(Rtnd_disc = ifelse(Rtnd_disc == "round_weight", "R", "D"),
           w = wt_lbs / sum(wt_lbs, na.rm = T)) %>%
    ungroup() %>%
    dplyr::select(Haul_ID, Rtnd_disc, w) %>%
    right_join(shell_height, by = c("Haul_ID", "Rtnd_disc")) %>%
    group_by(Season, District, sh) %>%
    summarise(w = sum(w)) %>%
    group_by(Season, District) %>%
    mutate(w_adj = w / sum(w)) %>%
    left_join(shell_height %>%
                group_by(Season, District) %>%
                summarise(Nsamp = n()),
              by = c("Season", "District")) %>%
    mutate(Year = as.numeric(substring(Season, 1, 4))) %>%
    ungroup() %>%
    select(Year, District, Nsamp, sh, w_adj) %>%
    # rename fields for consistency
    rename(year = Year,
           district = District,
           n_measured = Nsamp) -> out
  
  # bin and aggregate if appropriate
  if(!is.null(sh_bin)){
    out %>%
      mutate(sh = floor(sh / sh_bin) * sh_bin) %>%
      group_by(year, district, n_measured, sh) %>%
      summarise(w_adj = sum(w_adj)) -> out
  }
  
  
  return(out)
  
}

# objects ----

## base map
## high resolution map of alaska, canada
usa <- raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps")
can <- raster::getData("GADM", country = c("CAN"), level = 1, path = "./data/maps")
bind_rows(fortify(usa), fortify(can)) %>%
  filter(long > -180, long < -129, lat > 45, lat < 62) -> canam
ggplot()+
  geom_polygon(data = canam, aes(x = long, y = lat, group = group), 
               color = NA, fill = "grey90")+
  labs(x = expression(paste(Longitude^o,~'W')), 
       y = expression(paste(Latitude^o,~'N')))+
  theme(panel.background = element_rect(fill = "grey70", color = "black")) -> f_base_map

## ditrict specific coordinate projections
KNE_proj <- coord_quickmap(xlim = c(-153.2, -150), ylim = c(56.5, 58.7))
KSH_proj <- coord_quickmap(xlim = c(-155, -152.8), ylim = c(58, 59))
KSW_proj <- coord_quickmap(xlim = c(-156.4, -154.3), ylim = c(56, 58))
KSE_proj <- coord_quickmap(xlim = c(-155.6, -152.3), ylim = c(55.5, 57.5))
areaM_proj <- coord_quickmap(xlim = c(-165, -158), ylim = c(53, 56))
areaO_proj <- coord_quickmap(xlim = c(-169, -164.2), ylim = c(52.5, 54.5))
areaQ_proj <- coord_quickmap(xlim = c(-176, -164), ylim = c(53, 57.6))
WKI_proj <- coord_quickmap(xlim = c(-145.2, -144), ylim = c(59.5, 60.1))
EKI_proj <- coord_quickmap(xlim = c(-145, -143), ylim = c(59.5, 60.1))
YAK_proj <- coord_quickmap(xlim = c(-144.5, -136.5), ylim = c(57, 60.5))


        
# plotting functions ----

### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# graphic options
theme_sleek <- function(base_size = 12, base_family = "Times") {
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      #axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
  
}

# Depends on dplyr
tickr <- function(
    data, # dataframe
    var, # column of interest
    to # break point definition
) {
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}
theme_set(theme_sleek())
yraxis <- tickr(tibble(yr = 1980:2100), yr, 5)