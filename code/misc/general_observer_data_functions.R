# notes ----
## general functions applicable to all observer data analysis
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/2/27

# load libraries ----
library(tidyverse)
library(mgcv)
library(mgcViz)

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
  names(x) <- c("Fishery", "ADFG", "Set_date", "District", "hauls", "dredge_hrs", 
                    "est_rnd_wt", "mt_wt", "sample_hrs", "bairdi_count", "opilio_count",
                    "dungeness_count", "halibut_count", "disc_count", "disc_wt", "broken_wt",
                    "rem_disc_wt", "clapper_count", "king_count")
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
             District = ifelse(District == "KSH" & set_lat < 57.7 & set_lon <= -154,
                               "KSW", District),
             District = ifelse(District %in% c("D16", "D", "YAK"),
                               "YAK", District))
  } else{
    x %>%
      mutate(district = ifelse(bed_code %in% c("KSH5", "KSH6", "KSH7"), "KSW", district),
             district = ifelse(district == "KSH" & set_lat < 57.7 & set_lon <= -154,
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
      right_join(tmp, by = "Season") -> tmp
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
### Outputs a point estimate for each season and effects plots of Season, Bed, 
### Vessel, and Month
f_standardize_cpue <- function(x, path, by){
  
  x %>%
    # filter for only satisfacory dredges
    filter(gear_perf == 1) %>% 
    # compute cpue by dredge
    # cut off prefix of season for plotting
    mutate(rw_cpue = round_weight / dredge_hrs,
           Season = factor(substring(Season, 3, 4))) -> y

  # create cpue modifer
  adj <- mean(y$rw_cpue, na.rm = T)
  # fit model
  mod <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(set_lon, by = Bed) + 
              Month + Vessel + Season * Bed, data = y, gamma = 1.4, 
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
      # add fitted values from models
      mutate(fit = predict(mod, newdata = ., type = "response") - adj) %>%
      group_by(Season) %>%
      # remove if sum of weights for season/bed is zero
      mutate(sum_w = sum(w)) %>%
      filter(sum_w > 0) %>%
      # take the median weighted by proportion of effort allocated to each factor level within year to acheive standardized cpue   
      summarise(std_cpue = spatstat::weighted.median(fit, w = w)) %>%
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
      summarise(std_cpue = spatstat::weighted.median(fit, w = w)) %>%
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
      as_tibble(.)
    }
  }
}


## compute standardized cpue based on nominal round weight catch data IN DISTRICTS WITH ONLY 1 BED
### args:
### x - catch data to standardize containing fields for covariates. Must include 
### 'Season', Vessel', 'Month', 'depth', 'set_lon'.
### path - file path to save plots. Optional, if provided, function saves effect plots of Month, Season, and Vessel
### Outputs a point estimate for each season and effects plots of Season, Vessel, and Month
f_standardize_cpue2 <- function(x, path){
  
  x %>%
    # filter for only satisfacory dredges
    filter(gear_perf == 1) %>% 
    # compute cpue by dredge
    # cut off prefix of season for plotting
    mutate(rw_cpue = round_weight / dredge_hrs,
           Season = factor(substring(Season, 3, 4))) -> y
  
  # create cpue modifer
  adj <- mean(y$rw_cpue, na.rm = T)
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
      summarise(std_cpue = spatstat::weighted.median(fit, w = w)) %>%
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
      as_tibble(.)
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


        