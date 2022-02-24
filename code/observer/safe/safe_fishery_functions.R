# notes ----
# SAFE report 
# Functions for Summarizing Fishery Data
# author: Tyler Jackson (some functions by Ben Williams)
# contact: tyler.jackson@alaska.gov

# load ----
library(tidyverse)
library(latticeExtra)
library(rsample)
library(lubridate)
library(mgcv)
library(zoo)
library(cowplot)
library(FNGr); theme_set(theme_sleek())

# functions ----

## catch statistics
f_fishery_stats <- function(catch, district, bycatch, old_data_table = F, specific_m = F){
  
  # manipulate data
  catch %>%
    filter(District == district) %>%
    mutate(set_date = mdy(Set_date),
           month = month(set_date),
           year = year(set_date),
           Season = ifelse(month >= 7, 
                           paste0(year,"/", substring(year + 1, 3, 4)), 
                           paste0(year - 1,"/", substring(year, 3, 4)))) %>%
    group_by(Season, Fishery) %>%
    summarise(n_vessels = length(unique(ADFG)),
              mt_wt = sum(meat_weight, na.rm = T),
              rnd_wt = sum(round_weight, na.rm = T),
              dredge_hrs = sum(dredge_hrs, na.rm = T),
              mt_cpue = mt_wt / dredge_hrs,
              rnd_cpue = rnd_wt / dredge_hrs) %>%
    ungroup() %>%
    mutate(year = as.numeric(substring(Season, 1, 4))) %>%
    arrange(year) -> dat
  
  # combine with old data table (if necessary)
  if(old_data_table == T){
    old_catch %>%
      filter(District == district) %>%
      select(-District) %>%
      bind_rows(dat) -> dat
  }
  
  # add empty rows for years with no fishing
  tibble(yrs = min(as.numeric(substring(dat$Season,1, 4))):max(as.numeric(substring(dat$Season,1, 4)))) %>%
    mutate(yrs_plus1 = as.character(yrs + 1),
           Seasons = paste0(yrs, "/", substring(yrs_plus1, 3, 4))) %>%
    filter(!(Seasons %in% dat$Season)) %>%
    pull(Seasons) -> missing_seasons

  if(length(missing_seasons) > 0){
    dat %>%
      add_row(Season = missing_seasons) %>%
      mutate(year = as.numeric(substring(Season, 1, 4))) %>%
      arrange(year) -> dat
  }
  
  # bring in GHL and finish table
  ghl %>%
    filter(District == district) %>%
    right_join(dat, "Season") %>%
    select(-Area, -District, -Fishery, -year) -> dat
  
  # add discard mortality
  f_scal_discard(bycatch, district, specific_m, old_data_table) %>%
    select(Season, meat_disc_mort_lbs) %>%
    right_join(dat, by = "Season") %>%
    select(1, 3:9, 2) -> dat
  
  # generate plot using latticeExtra package
  bar <- barchart(mt_wt~Season, dat, col = "grey", scales = list(x = list(rot=90)), 
                  ylab = "Harvest (lb scallop meat) \n")
  pts <- xyplot(mt_cpue~c(1:nrow(dat)), dat, col = "black", type = "o", pch = 8, 
                ylab = "CPUE (lb scallop meat / dredge hr)", ylim = c(0, 1.1*max(dat$mt_cpue, na.rm = T)),
                scales = list(y = list(at = seq(0, 1000, 15))))
  plot <- doubleYScale(bar, pts, add.ylab2 = TRUE, use.style = F)
  
  # save plot  
  png(paste0("./figures/safe/", YEAR, "/fishery_statistic_", district, ".png"), 
      width = 5, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
  
  #save table
  write.csv(dat, paste0('./output/safe/', YEAR, '/fishery_statistics_', district, '.csv'), row.names = F)
}

## cpue standarization meat weight
f_gam_mw <- function(data){
  
  adj <- mean(data$mw_cpue, na.rm = T) * 0.10
  
  if('Bed' %in% colnames(data)){
    fit <- bam(mw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(set_lon, by = Bed) + 
                 Month + Vessel + Bed * Year, 
               data = data, gamma = 1.4, family=Gamma(link=log), select = T)
  } else {
    fit <- bam(mw_cpue + adj ~ s(depth, k = 4) + s(set_lon) + 
                 Month + Vessel + Year, 
               data = data, gamma = 1.4, family=Gamma(link=log), select = T)
  }
  return(fit)
}

## cpue standarization round weight
f_gam_rw <- function(data){
  
  adj <- mean(data$rw_cpue, na.rm = T) * 0.10
  
  if('Bed' %in% colnames(data)){
    fit <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(set_lon, by = Bed) + 
                 Month + Vessel + Bed * Year, 
               data = data, gamma = 1.4, family=Gamma(link=log), select = T)
  } else {
    fit <- bam(rw_cpue + adj ~ s(depth, k = 4) + s(set_lon) + 
                 Month + Vessel + Year, 
               data = data, gamma = 1.4, family=Gamma(link=log), select = T)
  }
  return(fit)
}

## raw and standarized cpue
f_fishery_cpue <- function(x, district, weight_type){
  
  # manipulate data to get Season
  x %>%
    filter(District == district) %>%
    mutate(set_date = mdy(Set_date),
           month = month(set_date),
           year = year(set_date),
           Season = ifelse(month >= 7, 
                           paste0(year,"/", substring(year + 1, 3, 4)), 
                           paste0(year - 1,"/", substring(year, 3, 4))),
           Year = factor(substring(Season, 1, 4)),
           Month = factor(month),
           Vessel = factor(ADFG)) -> x
  
  # obtain summary stats on raw and standarized cpue
  if(weight_type %in% c("meat_weight", "meat")){
    ## raw cpue stats
    x %>%
      group_by(Season) %>%
      summarise(raw_cpue_median = round(median(meat_weight / dredge_hrs, na.rm = T), 1),
                raw_cpue_mean = round(mean(meat_weight / dredge_hrs, na.rm = T), 1),
                raw_cpue_sd = round(sd(meat_weight / dredge_hrs, na.rm = T), 1)) -> x_raw
    ## standardized cpue
    x %>%
      mutate(mw_cpue = meat_weight / dredge_hrs) %>%  
      select(names(.)[names(.) %in% c("Season","Year", "Vessel", 
                                         "set_lat", "set_lon", "depth", "mw_cpue")]) %>% 
      .[complete.cases(.),] -> x_std
    
    fit <- f_gam_mw(x_std)
    
    x_std %>%
      mutate(fitted = fit$fitted.values) %>%
      group_by(Season) %>%
      mutate(adj = mean(mw_cpue, na.rm = T) * 0.1) %>%
      mutate(std_cpue = fitted - adj) %>%
      summarise(std_cpue = mean(std_cpue, na.rm = T)) %>%
      right_join(x_raw, by = "Season") %>%
      select(1, 3:5, 2) -> x_cpue
  }
  
  if(weight_type %in% c("round_weight", "round") & !("Bed" %in% names(x))){
    ## raw cpue stats
    x %>%
      group_by(Season) %>%
      summarise(raw_cpue_median = round(median(round_weight / dredge_hrs, na.rm = T), 1),
                raw_cpue_mean = round(mean(round_weight / dredge_hrs, na.rm = T), 1),
                raw_cpue_sd = round(sd(round_weight / dredge_hrs, na.rm = T), 1)) -> x_raw
    
    ## fit standardized cpue model
    x %>%
      mutate(rw_cpue = round_weight / dredge_hrs) %>%
      select(names(.)[names(.) %in% c("Season","Year", "Month", "Vessel", 
                                      "set_lat", "set_lon", "depth", "rw_cpue")]) %>% 
      .[complete.cases(.),] -> x_std
    
    fit <- f_gam_rw(x_std)
    
    ## get standardized estimates
      expand_grid(Season = levels(factor(x$Season)),
                Year = levels(x$Year), 
                Month = levels(x$Month),
                Vessel = levels(x$Vessel)) %>%
      #set depth as mode of depth, and set_lon as mean of set_lon
      left_join(x %>%
                  group_by(Season, Year, Month, Vessel) %>%
                  summarise(depth = mode(depth),
                            set_lon = mean(set_lon, na.rm = T)),
                by = c("Season", "Year", "Month", "Vessel")) %>% 
      #add weights for averaging cpue
      left_join(x %>%
                  group_by(Season, Year, Month, Vessel) %>%
                  summarise(n = n()) %>%
                  group_by(Year) %>%
                  mutate(w = n / sum(n, na.rm = T)),
                by = c("Season", "Year", "Month", "Vessel")) %>% 
      drop_na(n) %>%
      #add fitted values from models
      mutate(fit = predict(fit, newdata = ., type = "response") - adj) %>%
      #take the median weighted by proportion of effort allocated to each factor level within year to acheive standardized cpue 
      group_by(Season) %>%
      summarise(std_cpue = spatstat::weighted.median(fit, w = w)) %>%
      left_join(x_raw, by = "Season") %>%
      dplyr::select(1, 3:5, 2) -> x_cpue
  }
  
  # save cpue table
  write.csv(x_cpue, paste0("./output/safe/", YEAR, "/fishery_cpue_", weight_type, "_", district, ".csv"), 
            row.names = F)
  
  
  # create plots
  
  ## raw plot
  x_cpue %>%
    mutate(raw_grand_mean = mean(raw_cpue_mean, na.rm = T),
           raw_roll_mean = rollmean(raw_cpue_mean, 5, fill = NA, align = "right")) %>%
    select(Season, raw_cpue_mean, raw_grand_mean, raw_roll_mean) %>%
    pivot_longer(c(raw_cpue_mean, raw_roll_mean, raw_grand_mean), names_to = "Name") %>%
    mutate(Name = case_when(Name == "raw_cpue_mean" ~ "Raw CPUE",
                            Name == "raw_roll_mean" ~ "5-yr Rolling Mean", 
                            Name == "raw_grand_mean" ~ "Mean"),
           Name = factor(Name, levels = c("Raw CPUE", "Mean", "5-yr Rolling Mean"))) -> pdat 
    ggplot(pdat)+
    geom_line(aes(x = Season, y = value, group = Name, linetype = Name, color = Name))+
    geom_point(data = pdat[pdat$Name != "Mean",], aes(x = Season, y = value, color = Name), 
               show.legend = F)+
    scale_color_manual(values = c("forestgreen", "black", "black"))+
    labs(x = "Season", y = "Raw CPUE (lbs / dredge hr)", color = NULL, linetype = NULL)+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) -> p_raw
    
  ## standardized plot
  x_cpue %>%
    mutate(std_grand_mean = mean(std_cpue, na.rm = T),
           std_roll_mean = rollmean(std_cpue, 5, fill = NA, align = "right")) %>%
    select(Season, std_cpue, std_grand_mean, std_roll_mean) %>%
    pivot_longer(c(std_cpue, std_grand_mean, std_roll_mean), names_to = "Name") %>%
    mutate(Name = case_when(Name == "std_cpue" ~ "Std. CPUE",
                            Name == "std_roll_mean" ~ "5-yr Rolling Mean", 
                            Name == "std_grand_mean" ~ "Mean"),
           Name = factor(Name, levels = c("Std. CPUE", "Mean", "5-yr Rolling Mean"))) -> pdat 
    ggplot(pdat)+
    geom_line(aes(x = Season, y = value, group = Name, linetype = Name, color = Name))+
    geom_point(data = pdat[pdat$Name != "Mean",], aes(x = Season, y = value, color = Name), 
               show.legend = F)+
    scale_color_manual(values = c("forestgreen", "black", "black"))+
    labs(x = "Season", y = "Standardized CPUE (lbs / dredge hr)", color = NULL, linetype = NULL)+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) -> p_std
  
  ## combine and save plots
  png(paste0("./figures/safe/", YEAR, "/fishery_cpue_", weight_type, "_", district, ".png"), 
      width = 7, height = 8, units = "in", res = 300)
  print(plot_grid(p_raw, p_std, ncol = 1))
  dev.off()
}

## shell height composition
f_sh_comp <- function(sh, district){
  # manipulate data 
  sh %>%
    filter(District == district) %>%
    mutate(Season = as.numeric(substring(Fishery, 3, 4)),
           Season = ifelse(Season < 70, Season + 2000, Season + 1900),
           Season = factor(paste0(Season, "/", substring(Season + 1, 3, 4))),
           Rtnd_Disc = case_when(Rtnd_Disc == "D" ~ "Discard",
                                 Rtnd_Disc == "R" ~ "Retained")) -> sh
  
  # create a base plot
  ggplot(sh, aes(x = shell_height, fill = Rtnd_Disc, color = Rtnd_Disc)) -> plot
    
  # create a histogram
  plot+
    geom_histogram(binwidth = 3, alpha = 0.2)+
    scale_color_manual(values = c("#e41a1c", "#377eb8"))+
    labs(x = "Shell height (mm)", y = "Count")+
    facet_grid(rows = vars(Season))+
    theme(legend.position = "none") -> sh_hist
  
  # create a density plot
  plot+
    geom_density(alpha = 0.2)+
    scale_color_manual(values = c("#e41a1c", "#377eb8"))+
    labs(x = "Shell height (mm)", y = "Density", fill = NULL, color = NULL)+
    facet_grid(rows = vars(Season)) -> sh_dens
  
  # save the legend from the plot
  legend <- get_legend(sh_dens + theme(legend.position = "bottom"))
  
  # combine plots
  ggdraw()+
    draw_plot(sh_dens + theme(legend.position = "none"), x = 0, y = 0.075, width = 0.5, height = 0.9)+
    draw_plot(sh_hist, x = 0.5, y = 0.075, width = 0.5, height = 0.9)+
    draw_plot(legend, x = 0, y = 0, width = 1, height = 0.1) -> sh_plot
  
  #print and save
  png(paste0("./figures/safe/", YEAR, "/shell_height_", district, ".png"), 
      width = 6, height = 8, units = "in", res = 300)
  print(sh_plot)
  dev.off()
}

## estimate scallop discard rate, total meat weights discarded, and discard mortality
f_scal_discard <- function(bycatch, district, specific_m = F, old_data_table = F){

  bycatch %>%
    filter(District == district) %>%
    mutate(set_date = mdy(Set_date),
          month = month(set_date),
          year = year(set_date),
          Season = ifelse(month >= 7, 
                          paste0(year,"/", substring(year + 1, 3, 4)), 
                          paste0(year - 1,"/", substring(year, 3, 4)))) -> bc
  
    boot_ci <- function(split){
      
      rsample::analysis(split) %>%
        mutate(scal_disc_rate = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
               scal_disc_est_lbs = scal_disc_rate * sum(dredge_hrs)) %>%
        summarise(scal_disc_rate = mean(scal_disc_rate, na.rm = T),
                  scal_disc_est_lbs = mean(scal_disc_est_lbs, na.rm = T))
    }  
    
  if(specific_m == F){
    

  # Assume average 10% meat recovery
  # Assume 20% mortality rate on discards (no split for broken vs whole discards)
  bc %>%
    nest(-Season) %>%
    mutate(samp = map(data, ~rsample::bootstraps(., 1000))) %>%
    unnest(samp) %>%
    mutate(models = map(splits, ~boot_ci(.x))) %>%
    unnest(models) %>%
    group_by(Season) %>%
    summarise(lwr95 = quantile(scal_disc_est_lbs, 0.025, na.rm = T),
              upp95 = quantile(scal_disc_est_lbs, 0.975, na.rm = T)) -> ci
      
  bc %>%
    group_by(Season) %>%
    summarise(scal_disc_rate = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
              scal_disc_est_lbs = scal_disc_rate * sum(dredge_hrs),
              meat_disc_lbs = scal_disc_est_lbs * 0.10,
              meat_disc_mort_lbs = meat_disc_lbs * 0.20) %>%
    left_join(ci, by = "Season") %>%
    select(1, 2, 3, 6, 7, 4, 5) -> disc
  
  } else{
    

  # Assume average 10% meat recovery
  # Assume different discard mortality rates (20% - intact, 100% - broken)
  bc %>%
    nest(-Season) %>%
    mutate(samp = map(data, ~rsample::bootstraps(., 1000))) %>%
    unnest(samp) %>%
    mutate(models = map(splits, ~boot_ci(.x))) %>%
    unnest(models) %>%
    group_by(Season) %>%
    summarise(lwr95 = quantile(scal_disc_est_lbs, 0.025, na.rm = T),
              upp95 = quantile(scal_disc_est_lbs, 0.975, na.rm = T)) -> ci
  bc %>%
    group_by(Season) %>%
    summarise(scal_disc_rate = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
              scal_disc_est_lbs = scal_disc_rate * sum(dredge_hrs),
              meat_disc_lbs = scal_disc_est_lbs * 0.10,
              intact_prop = sum(disc_wt) / sum(disc_wt, broken_wt),
              broken_prop = 1 - intact_prop,
              meat_disc_mort_lbs = (meat_disc_lbs * intact_prop * 0.2) + (meat_disc_lbs * broken_prop)) %>%
    left_join(ci, by = "Season") %>%
    select(1, 2, 3, 8, 9, 4:7) -> disc
  }   
  disc
}

## estimate chionoecetes crab bycatch
f_bycatch_chionoecetes <- function(bycatch, crab_size, all_seasons = F) {

  bycatch %>%
    mutate(set_date = mdy(Set_date),
           month = month(set_date),
           year = year(set_date),
           Season = ifelse(month >= 7, 
                           paste0(year,"/", substring(year + 1, 3, 4)), 
                           paste0(year - 1,"/", substring(year, 3, 4)))) -> bc
  
  # find most recent season
  seas <- bc$Season[bc$set_date == max(bc$set_date)]
  
  bc %>%
    group_by(Season, District) %>%
    summarize(dredge_hrs = sum(dredge_hrs),
              sample_hrs = sum(sample_hrs),
              bairdi = sum(bairdi_count) / sample_hrs * dredge_hrs,
              opilio = sum(opilio_count) / sample_hrs * dredge_hrs) %>%
      select(Season, District, bairdi, opilio) %>%
      pivot_longer(c(bairdi, opilio), names_to = "spp", values_to = "est") %>%
      filter(est != 0) -> chionoecetes
 
  # estimate chionoecetes weight
  crab_size %>%
    left_join(bc %>%
               select(Fishery, Season),
               by = "Fishery") %>%
    mutate(spp = case_when(race_code == 68560 ~ "bairdi",
                           race_code == 68541 ~ "opilio")) %>%
    group_by(Season, District, size, sex, spp) %>%
    summarise(num_crab = round(sum(sampfrac_num_crab))) %>%
    ungroup() %>%
    mutate(calc_wt = case_when(spp == "bairdi" & sex == 1 ~ num_crab * 0.00027 * size^3.022134,
                               spp == "bairdi" & sex == 2 ~ num_crab * 0.000562 * size^2.816928,
                               spp == "opilio" & sex == 1 ~ num_crab * 0.000267 * size^3.097253,
                               spp == "opilio" & sex == 2 ~ num_crab * 0.001158 * size^2.827784)) %>%
    group_by(Season, District, spp) %>%
    summarise(avg_wt_g = sum(calc_wt) / sum(num_crab)) %>%
    left_join(chionoecetes, by =c("Season", "District", "spp")) %>%
    mutate(bycatch_lb = avg_wt_g * est * 0.00220462) -> chionoecetes_bycatch
    
  if(all_seasons == F){
     chionoecetes_bycatch %>%
       filter(Season == seas) -> chionoecetes_bycatch
      
    write_csv(chionoecetes_bycatch, paste0("./output/safe/", YEAR, "/chionoecetes_bycatch_", 
                                           gsub("/", "-", seas), ".csv"))
    } else{
      write_csv(chionoecetes_bycatch, paset0("./output/safe/", YEAR, "/chionoecetes_bycatch_all_seasons.csv"))
    }
    
  # chionoecetes size comps
  crab_size %>%
    mutate(Season = bc$Season[match(.$Fishery, bc$Fishery)])%>%
    filter(Season == seas) %>%
    mutate(spp = case_when(race_code == 68560 ~ "bairdi",
                           race_code == 68541 ~ "opilio"),
           size_bin = cut(size, breaks = seq(3, 253, 5), labels = F),
           size_bin = seq(0, 250, 5)[size_bin],
           Sex = case_when(sex == 1 ~ "Male",
                           sex == 2 ~ "Female"),
           Sex = factor(Sex, levels = c("Male", "Female")),
           District = ifelse(District == "D", "YAK", District)) %>%
    group_by(Season, District, size_bin, Sex, spp) %>%
    summarise(num_crab = round(sum(sampfrac_num_crab))) %>%
    left_join(mgmt_unit, by = c("District" = "District_code")) %>%
    mutate(panel_name = paste(Area, District_name, "Tanner Crab"),
           panel_name = ifelse(panel_name == "Yakutat Yakutat Tanner Crab",
                               "Yakutat Tanner Crab", panel_name),
           panel_name = ifelse(panel_name == "Dutch Harbor Dutch Harbor Tanner Crab",
                               "Dutch Harbor Tanner Crab", panel_name),
           panel_name = ifelse(panel_name == "Bering Sea Bering Sea Tanner Crab",
                               "Bering Sea Tanner Crab", panel_name),
           panel_name = ifelse(panel_name == "Prince William Sound West Kayak Island Tanner Crab",
                               "PWS West Kayak Island Tanner Crab", panel_name),
           panel_name = ifelse(panel_name == "Prince William Sound East Kayak Island Tanner Crab",
                               "PWS East Kayak Island Tanner Crab", panel_name),
           panel_name = ifelse(spp == "opilio", "Bering Sea Snow Crab", panel_name)) %>%
    group_by(panel_name) %>%
    mutate(sum_crab = sum(num_crab)) -> plot_data
  
  # make a note of which districts were not plotted
  print(paste("The following districts were not plotted since there were < 50 crab measured:",
              paste(plot_data %>%
                      filter(sum_crab < 50) %>%
                      pull(District) %>%
                      unique(),
                    collapse = ", ")))
              
              
  # print size comp plot
  plot_data%>%
    filter(sum_crab >= 50) %>%
    ggplot()+
    geom_point(aes(x = size_bin, y = num_crab, shape = factor(Sex)))+
    scale_shape_manual(values = c(8, 1))+
    geom_line(aes(x = size_bin, y = num_crab, linetype = factor(Sex)))+
    labs(x = "Carapace width (mm)", y = "Number of crab", shape = NULL, linetype = NULL)+
    facet_wrap(~panel_name, scales = "free_y", ncol = 2)+
    theme(legend.position = "bottom") -> crab_plot
    
  png(paste0("./figures/safe/", YEAR, "/chionoecetes_size_comp.png"), 
      width = 6, height = 8, units = "in", res = 300)
  print(crab_plot)
  dev.off()
    
}  

## estimate king crab bycatch    
f_bycatch_king <- function(bycatch, all_seasons = F) {
  
  bycatch %>%
    mutate(set_date = mdy(Set_date),
           month = month(set_date),
           year = year(set_date),
           Season = ifelse(month >= 7, 
                           paste0(year,"/", substring(year + 1, 3, 4)), 
                           paste0(year - 1,"/", substring(year, 3, 4)))) -> bc
  
  # find most recent season
  seas <- bc$Season[bc$set_date == max(bc$set_date)]
  
  bc %>%
    group_by(Season, District) %>%
    summarize(est = sum(king_count) / sum(sample_hrs) * sum(dredge_hrs)) -> king_bycatch
              
  if(all_seasons == F){
    king_bycatch %>%
      filter(Season == seas) -> king_bycatch
    
    write_csv(king_bycatch, paste0("./output/safe/", YEAR, "/king_bycatch_", 
                                           gsub("/", "-", seas), ".csv"))
  } else{
    write_csv(king_bycatch, paset0("./output/safe/", YEAR, "/king_bycatch_all_seasons.csv"))
  }
}      
    





