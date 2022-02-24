# notes ----
## general functions applicable to all observer data analysis
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/2/27

# load libraries ----
library(here)
library(lubridate)
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
  names(x) <- c("Fishery", "District", "ADFG", "Trip_ID", "Haul_ID", 
                "haul", "gear_perf", "haul_sampled", "Set_date", "bed_code", "set_lat",
                "set_lon", "statarea", "depth", "dredge_count", 
                "dredge_width", "dredge_down", "dredge_up", "duration", 
                "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                "rtnd_basket", "round_weight", "meat_weight", "est_yield",
                "tot_rtnd_basket", "tot_day_meat_weight")
  x
}

# rename bycatch data to appropriate style
# args:
## x - daily bycatch data (as downloaded directly from wiki)
f_bycatch_rename <- function(x){
  names(x) <- c("Fishery", "District", "ADFG", "Haul_ID", "haul", "gear_perf", "Set_date", "bed_code", "dredge_hrs", 
                    "est_rnd_wt", "mt_wt", "sample_hrs", "bairdi_count", "opilio_count",
                    "dungeness_count", "king_count", "halibut_count", "disc_count", "disc_wt", "broken_wt",
                    "rem_disc_wt", "clapper_count")
  x
}

# rename crab_size data to appropriate style
# args:
## x - crab size data (as downloaded directly from wiki)
f_crab_size_rename <- function(x){
  names(x) <- c("Fishery", "District", "RACE_code", "sex", "cw", "samp_frac")
  x
}

# rename shell_height data to appropriate style
# args:
## x - shell height data (as downloaded directly from wiki)
f_shell_height_rename <- function(x){
  names(x) <- c("Fishery", "District", "Haul_ID", "ADFG", "Rtnd_disc", "sh", "shell_num")
  x
}

# add season to data (based on Fishery field)
# args:
## x - tibble of observer or logbook data
## fishery_col - name of column that denotes fishery. Default = "Fishery"
f_add_season <- function(x, fishery_col = "Fishery"){
  x %>%
    pull(grep(fishery_col, names(.))) %>%
    str_sub(., 3, 4) %>%
    as.numeric() %>%
    tibble(Season = .) %>%
    mutate(Season = ifelse(Season < 80, Season + 2000, Season + 1900),
           Season = factor(paste0(Season, "/", substring(Season + 1, 3, 4)))) %>%
    bind_cols(x)
}


# revise district to align with current mgmt structure
# args: x - any tibble containing the field 'district' or 'District'
f_revise_district <- function(x){
  
  if(!("district" %in% names(x))){
    x %>%
      mutate(District = ifelse(bed_code %in% c("KSH5", "KSH6", "KSH7"), "KSW", District),
             District = ifelse(District == "KSH" & set_lat < 57.7 & set_lon <= -154.35,
                               "KSW", District),
             District = ifelse(District %in% c("D16", "D", "YAK"),
                               "YAK", District))
  } else{
    x %>%
      mutate(district = ifelse(bed_code %in% c("KSH5", "KSH6", "KSH7"), "KSW", district),
             district = ifelse(district == "KSH" & set_lat < 57.7 & set_lon <= -154.35,
                               "KSW", district),
             district = ifelse(district %in% c("D16", "D", "YAK"),
                               "YAK", district))
  }
}

# quick summary of fishery statistics
# args:
## x - catch data (as downloaded directly from wiki)
## district - abbrev. for any districts to summarise over
## ghl - logical include cmobine ghl. Default = F
## path - optional. Writes .csv file to path provided
f_fish_stats <- function(x, district, add_ghl = F, path){
  # add Season if it is not found
  if(!("Season" %in% names(x))) {x <- add_season(x)}
  # summarize catch data
  x %>%
    # filter to area D
    filter(District %in% district) %>%
    # summarise data 
    group_by(Season) %>%
    summarise(mt_wt = sum(meat_weight, na.rm = T),
              rnd_wt = sum(round_weight, na.rm = T),
              dredge_hrs = sum(dredge_hrs, na.rm = T),
              number_hauls = n(),
              mw_cpue = mt_wt / dredge_hrs,
              rw_cpue = rnd_wt / dredge_hrs) -> tmp
  # add ghl if necessary
  if(add_ghl == T){
    if(!exists("ghl")){stop("tibble named 'ghl' not found")}
    else{
    ghl %>%
      filter(District %in% district) %>%
      group_by(Season) %>%
      summarise(ghl = sum(ghl, na.rm = T)) %>%
      left_join(., tmp, by = "Season") %>%
      replace_na(list(mt_wt = 0, rnd_wt = 0, dredge_hrs = 0, number_hauls = 0)) %>%
      filter(as.numeric(substring(Season, 1, 4)) > 2008) -> tmp
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
### by - level to summarise standardized coue over. "Season" or "Bed"
### compute_predicted - T/F comput predicted value of CPUE based on season and all other inputs on average
### Outputs a point estimate for each season and effects plots of Season, Bed, 
### Vessel, and Month
f_standardize_cpue <- function(x, path, by, compute_predicted){
  
  x %>%
    # filter for only satisfacory dredges
    filter(gear_perf == 1) %>% 
    # compute cpue by dredge
    # cut off prefix of season for plotting
    mutate(rw_cpue = round_weight / dredge_hrs,
           Season = factor(substring(Season, 3, 4))) -> y

  # create cpue modifer
  adj <- mean(y$rw_cpue)
  
  # set reference levels
  y$Bed = relevel(y$Bed, mode(y$Bed))
  y$Month = relevel(y$Month, mode(y$Month))
  y$Vessel = relevel(y$Vessel, mode(y$Vessel))
  
  # fit model
  mod <- bam((rw_cpue + adj) ~ s(depth, k = 4, by = Bed) + s(set_lon, by = Bed) + 
              Month + Vessel + Bed + Season, data = y, gamma = 1.4, 
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
  n_start <- length(unique(y$Bed)) + length(unique(y$Bed)) + 1
  n_end <- n_start + 3
  ggsave(path, 
         plot = print(plot(mod_viz, allTerms = F, select = (n_start:n_end))+ 
                      l_points(size = 1, col = "grey")+
                      l_ciBar(linetype = 1)+
                      l_fitPoints(size = 1, col = 1)+
                      geom_hline(yintercept = 0, linetype = 2)+
                      theme_sleek()+
                      theme(axis.text = element_text(size = 9)), pages = 1), 
        height = 4, width = 7, units = "in")
  }
  
  # extract year effect
  tibble(par = names(mod$coefficients), 
         est = mod$coefficients,
         se = sqrt(diag(vcov(mod)))) %>%
    filter(grepl("Intercept|Season", names(mod$coefficients))) %>%
    mutate(par_est = ifelse(par != "(Intercept)", est + est[par == "(Intercept)"], est),
           par_se = se,
           std_cpue = exp(par_est + se/2) - adj,
           se = std_cpue * se,
           Season = unique(x$Season)) %>%
    dplyr::select(Season, par_est, par_se, std_cpue, se) -> out 
    
  if(compute_predicted == T){
  # compute standardized cpue
  expand_grid(Season = unique(y$Season), 
              Month = unique(y$Month),
              Bed = unique(y$Bed),
              Vessel = unique(y$Vessel)) %>%
    # set depth as mode of depth, and set_lon as mean of set_lon
    left_join(y %>%
                group_by(Bed) %>%
                summarise(depth = mode(depth),
                          set_lon = mean(set_lon, na.rm = T)),
              by = c("Bed")) -> tmp
  # add weights for averaging cpue
  if(by == "Season"){
    tmp %>%
      left_join(y %>%
                  group_by(Season, Bed, Month, Vessel) %>%
                  summarise(n = n()) %>%
                  group_by(Season) %>%
                  mutate(w = n / sum(n, na.rm = T)),
                by = c("Season", "Bed", "Month", "Vessel")) %>% 
      replace_na(list(n = 0, w = 0)) %>%
      bind_cols(tibble(fit = predict(mod, newdata = ., type = "response"), 
                       cv_fit = predict(mod, newdata = ., type = "response", se.fit = T)$se.fit / fit)) %>%
      mutate(fit = fit - adj,
             se_fit = cv_fit * fit) %>%
      group_by(Season) %>%
      # remove if sum of weights for season/bed is zero
      mutate(sum_w = sum(w)) %>%
      filter(sum_w > 0) %>%
      # take the median weighted by proportion of effort allocated to each factor level within year to acheive standardized cpue   
      summarise(std_cpue = weighted.median(fit, w = w),
                std_cpue_se = sum(se_fit * w)) %>%
      # correct season
      mutate(Season = ifelse(as.numeric(as.character(Season)) < 80, 
                             as.numeric(as.character(Season)) + 2000,
                             as.numeric(as.character(Season)) + 1900),
             Season = paste0(Season, "/", substring(Season + 1, 3, 4))) %>%
      # join with nominal cpue
      left_join(x %>%
                  group_by(Season) %>%
                  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
                            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
                            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)),
                by = "Season") %>%
      dplyr::select(Season, nom_cpue, nom_cpue_median, nom_cpue_sd, std_cpue, std_cpue_se) %>%
      as_tibble(.)
  } else{if(by == "Bed"){
    tmp %>%
      left_join(y %>%
                  group_by(Season, Bed, Month, Vessel) %>%
                  summarise(n = n()) %>%
                  group_by(Season, Bed) %>%
                  mutate(w = n / sum(n, na.rm = T)),
                by = c("Season", "Bed", "Month", "Vessel")) %>% 
      replace_na(list(n = 0, w = 0)) %>%
      # add fitted values from models
      mutate(fit = predict(mod, newdata = ., type = "response") - adj) %>%
      group_by(Season, Bed) %>%
      # remove if sum of weights for season/bed is zero
      mutate(sum_w = sum(w)) %>%
      filter(sum_w > 0) %>%
      # take the median weighted by proportion of effort allocated to each factor level within year to acheive standardized cpue   
      summarise(std_cpue = weighted.median(fit, w = w)) %>%
      ungroup() %>%
      # correct season
      mutate(Season = ifelse(as.numeric(as.character(Season)) < 80, 
                             as.numeric(as.character(Season)) + 2000,
                             as.numeric(as.character(Season)) + 1900),
             Season = paste0(Season, "/", substring(Season + 1, 3, 4))) %>%
      # join with nominal cpue
      left_join(x %>%
                  group_by(Season, Bed) %>%
                  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
                            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
                            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)),
                by = c("Season", "Bed")) %>%
      
      dplyr::select(Season, Bed, nom_cpue, nom_cpue_median, nom_cpue_sd, std_cpue) %>%
      as_tibble(.) -> out
  }
    }
  }
  
  return(out)
  
}


## compute standardized cpue based on nominal round weight catch data IN DISTRICTS WITH ONLY 1 BED
### args:
### x - catch data to standardize containing fields for covariates. Must include 
### 'Season', Vessel', 'Month', 'depth', 'set_lon'.
### path - file path to save plots. Optional, if provided, function saves effect plots of Month, Season, and Vessel
### Outputs a point estimate for each season and effects plots of Season, Vessel, and Month
f_standardize_cpue2 <- function(x, path, compute_predicted = T){
  
  x %>%
    # filter for only satisfacory dredges
    filter(gear_perf == 1) %>% 
    # compute cpue by dredge
    # cut off prefix of season for plotting
    mutate(rw_cpue = round_weight / dredge_hrs,
           Season = factor(substring(Season, 3, 4))) -> y
  
  # create cpue modifer
  adj <- mean(y$rw_cpue, na.rm = T)
  
  
  # set reference levels
  y$Month = relevel(y$Month, mode(y$Month))
  y$Vessel = relevel(y$Vessel, mode(y$Vessel))
  
  # fit model
  mod <- bam(rw_cpue + adj ~ s(depth, k = 4) + s(set_lon) + 
               Month + Vessel + Season, data = y, gamma = 1.4, 
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
    ggsave(path, 
           plot = print(plot(mod_viz, allTerms = F, select = (n_start:n_end))+ 
                          l_points(size = 1, col = "grey")+
                          l_ciBar(linetype = 1)+
                          l_fitPoints(size = 1, col = 1)+
                          geom_hline(yintercept = 0, linetype = 2)+
                          theme_sleek(), pages = 1), 
           height = 4, width = 6, units = "in")
  }
  
  # extract year effect
  tibble(par = names(mod$coefficients), 
         est = mod$coefficients,
         se = sqrt(diag(vcov(mod)))) %>%
    filter(grepl("Intercept|Season", names(mod$coefficients))) %>%
    mutate(std_cpue = ifelse(par != "(Intercept)", est + est[par == "(Intercept)"], est),
           std_cpue = exp(std_cpue + se/2) - adj,
           Season = unique(x$Season)) %>%
    dplyr::select(Season, std_cpue) -> out 
  
  if(compute_predicted == T){
  # compute standardized cpue
  expand_grid(Season = unique(y$Season), 
              Month = unique(y$Month),
              Vessel = unique(y$Vessel),
              depth = mode(y$depth),
              set_lon = mean(y$set_lon, na.rm = T)) %>%
      left_join(y %>%
                  group_by(Season, Month, Vessel) %>%
                  summarise(n = n()) %>%
                  group_by(Season) %>%
                  mutate(w = n / sum(n, na.rm = T)),
                by = c("Season", "Month", "Vessel")) %>% 
      replace_na(list(n = 0, w = 0)) %>%
      # add fitted values from models
      mutate(fit = predict(mod, newdata = ., type = "response") - adj) %>%
      # take the median weighted by proportion of effort allocated to each factor level within year to acheive standardized cpue   
      group_by(Season) %>%
      summarise(std_cpue = weighted.median(fit, w = w)) %>%
      # correct season
      mutate(Season = ifelse(as.numeric(as.character(Season)) < 80, 
                             as.numeric(as.character(Season)) + 2000,
                             as.numeric(as.character(Season)) + 1900),
             Season = paste0(Season, "/", substring(Season + 1, 3, 4))) %>%
      # join with nominal cpue
      left_join(x %>%
                  group_by(Season) %>%
                  summarise(nom_cpue = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
                            nom_cpue_median = median(round_weight / dredge_hrs, na.rm = T),
                            nom_cpue_sd = sd(round_weight / dredge_hrs, na.rm = T)),
                by = "Season") %>%
      dplyr::select(Season, nom_cpue, nom_cpue_median, nom_cpue_sd, std_cpue) %>%
      as_tibble(.)}
  
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
  theme(panel.background = element_rect(fill = "grey70")) -> f_base_map

## ditrict specific coordinate projections
KNE_proj <- coord_quickmap(xlim = c(-153.2, -150), ylim = c(56.5, 58.7))
KSH_proj <- coord_quickmap(xlim = c(-155, -152.8), ylim = c(58, 59))
KSW_proj <- coord_quickmap(xlim = c(-156.4, -154.3), ylim = c(56, 58))
KSE_proj <- coord_quickmap(xlim = c(-155.6, -152.3), ylim = c(55.5, 57.5))
areaM_proj <- coord_quickmap(xlim = c(-165, -158), ylim = c(53, 56))
areaO_proj <- coord_quickmap(xlim = c(-169, -164.2), ylim = c(52.5, 54.5))
areaQ_proj <- coord_quickmap(xlim = c(-176, -164), ylim = c(53, 57.6))
YAK_proj <- coord_quickmap(xlim = c(-144.5, -136.5), ylim = c(58, 60.5))


        