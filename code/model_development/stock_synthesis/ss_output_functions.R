# notes ----
## custom functions for summarise SS models
## used in combination with r4ss library
## Tyler Jackson

# load ----
library(tidyverse)
library(ggpmisc)
library(r4ss)

## ggplot axis ticks
yr_axis <- scale_x_continuous(breaks = FNGr::tickr(tibble(yr = 1900:2100), yr, 5)$breaks,
                              labels = FNGr::tickr(tibble(yr = 1900:2100), yr, 5)$labels)

# change ggplot default colors
ggplot <- function(...) ggplot2::ggplot(...) + scale_colour_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1")

theme_set(FNGr::theme_sleek())

# functions -----

## write new control file with different natural mortality
f_change_nat_M <- function(scenario_path, new_M){
  
  ### read ctl file
  ctl = r4ss::SS_readctl(file = list.files(scenario_path, pattern = ".ctl", full.names = T), version = "3.30",
                         verbose = FALSE, use_datlist = TRUE, datlist = list.files(scenario_path, pattern = ".dat", full.names = T))
  ### change parameter
  ctl$MG_parms %>%
    rownames_to_column() %>% 
    mutate(INIT = ifelse(rowname == "NatM_p_1_Fem_GP_1", new_M, INIT)) %>%
    column_to_rownames() -> ctl$MG_parms
  ### overwrite ctl file
  SS_writectl_3.30(ctl, outfile = list.files(scenario_path, pattern = ".ctl", full.names = T), overwrite = T, verbose = F)
  
  
}
## survey index plots by model scenario
f_plot_index_comparison <- function(summaryoutput, yaxis_titles, models = mod_names, index_fleets, file_paths) {
  
  summaryoutput$indices %>%
    as_tibble() %>%
    rename_all(tolower) %>%
    mutate(scenario = models[imodel]) -> index_dat

  
  plot_list = list()
  for (i in 1:length(index_fleets)) {
    index_dat %>%
      filter(fleet == index_fleets[i]) %>%
      
      # plot
      ggplot()+
      geom_point(aes(x = yr, y = obs))+
      geom_errorbar(aes(x = yr,
                        ymin = qlnorm(0.025, meanlog = log(obs), sdlog = se), 
                        ymax = qlnorm(0.975, meanlog = log(obs), sdlog = se)), 
                    width = 0)+
      geom_line(aes(x = yr, y = exp, color = scenario, linetype = scenario))+
      labs(x = NULL, y = yaxis_titles[i], color = NULL, linetype = NULL)+
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
      yr_axis+
      theme(legend.justification = c(1, 1), legend.position = c(1, 1)) -> plot_list[[i]]
  }
  # write plot to directory
  purrr::map2(file_paths, plot_list, ggsave, height = 3, width = 5, units = "in")
  
  return(plot_list)
  

  }
## ssb plots by model scenario
f_plot_ssb_comparison <- function(summaryoutput, models = mod_names, fleets, file_path, ylim = c(NA, NA)) {
  # estimate
  summaryoutput$SpawnBio %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "ssb") -> ssb_dat
  # lower
  summaryoutput$SpawnBioLower %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "lwr") -> ssblwr_dat
  # upper
  summaryoutput$SpawnBioUpper %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "upr") -> ssbupr_dat
  
  # plot
  left_join(ssb_dat, ssblwr_dat, by = c("yr", "scenario")) %>%
    left_join(ssbupr_dat, by = c("yr", "scenario")) %>%
      # plot
      ggplot()+
      geom_point(data = ssb_dat %>% filter(yr == min(yr)),
                 aes(x = yr, y = ssb, color = scenario, shape = scenario))+
      geom_ribbon(aes(x = yr, ymin = lwr, ymax = upr, fill = scenario), alpha = 0.2)+
      geom_line(aes(x = yr, y = ssb, color = scenario))+
      labs(x = NULL, y = "Spawning Biomass (t)", color = NULL, fill = NULL, shape = NULL)+
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
      coord_cartesian(ylim = ylim)+
      yr_axis+
      theme(legend.justification = c(1, 1), legend.position = c(1, 1)) -> x
    
  # write plot to directory
  ggsave(file_path, plot = x,  height = 3, width = 5, units = "in")
  
  return(x)
  
}
## size and age selectivities by model scenario
## selectivity is not time varying
f_plot_selex_comparison <- function(summaryoutput, models = mod_names, fleets, file_path, type = c("size", "age")) {
  
  if(type == "size"){
    summaryoutput$sizesel %>%
      as_tibble() %>%
      rename_all(tolower) %>%
      filter(yr == max(yr),
             fleet %in% fleets) %>%
      mutate(scenario = models[imodel],
             fleet = unique(unlist(mod_summaries$FleetNames))[fleet]) %>%
      dplyr::select(fleet, scenario, grep(pattern = "[[:digit:]]", names(.), value = T)) %>%
      pivot_longer(3:ncol(.), names_to = "sh", values_to = "sel") %>%
      mutate(sh = as.numeric(sh)) %>%
      
      ggplot()+
      geom_line(aes(x = sh, y = sel, group = scenario, color = scenario, linetype = scenario))+
      labs(x = "Shell Height (cm)", y = "Selectivity", color = NULL, linetype = NULL)+
      theme(legend.justification = c(0, 1), legend.position = c(0, 1))+
      facet_wrap(~fleet) -> x}
  
  if(type == "age"){
    summaryoutput$agesel %>%
      as_tibble() %>%
      rename_all(tolower) %>%
      filter(yr == max(yr),
             fleet %in% fleets) %>%
      mutate(scenario = models[imodel],
             fleet = unique(unlist(mod_summaries$FleetNames))[fleet]) %>%
      dplyr::select(fleet, scenario, grep(pattern = "[[:digit:]]", names(.), value = T)) %>%
      pivot_longer(3:ncol(.), names_to = "age", values_to = "sel") %>%
      mutate(age = as.numeric(age)) %>%
      
      ggplot()+
      geom_line(aes(x = age, y = sel, group = scenario, color = scenario, linetype = scenario))+
      labs(x = "Age", y = "Selectivity", color = NULL, linetype = NULL)+
      theme(legend.justification = c(0, 1), legend.position = c(0, 1))+
      facet_wrap(~fleet) -> x}
  # write plot to directory
  ggsave(file_path, plot = x,  height = 3, width = 6, units = "in")
  
  return(x)
  
}
## comp fit by model scenario
f_plot_comp_comparison <- function(dirs, models = mod_names, type = c("size", "age"), fleets, file_path, height = 6, width = 8) {
  ## load all rep files
  biglist <- SSgetoutput(dirvec = dirs, verbose = F)
  
  ## prep data
  tibble(model = 1:length(biglist),
         replist = biglist) %>%
    ## extract comps
    mutate(size_comp = purrr::map(replist, function(x){return(x$lendbase %>% as_tibble)}),
           age_comp = purrr::map(replist, function(x){return(x$agedbase %>% as_tibble)}),
           scenario = models[model]) -> tmp
  
  ## comp figures
  plot_list <- NULL
  if(type == "size"){for(i in fleets){
    tmp %>%
      dplyr::select(scenario, size_comp) %>%
      unnest(size_comp) %>%
      dplyr::select(-sex) %>%
      rename_all(tolower) %>%
      filter(fleet == i) %>%
      mutate(annotation = paste0("N adj = ", nsamp_adj, "\n", "N eff = ", round(effn, 1))) %>%
      
      ggplot()+
      geom_bar(aes(x = bin, y = obs), stat = "identity", position = "identity", color = 1, fill = "grey80")+
      geom_line(aes(x = bin, y = exp, color = scenario))+
      geom_text_npc(aes(npcx = "left", npcy = "top", label = yr), check_overlap = T)+
      geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation), check_overlap = T, size = 3)+
      labs(x = "Shell Height (cm)", y = "Proportion", color = NULL)+
      scale_y_continuous(expand = expansion(mult = c(0, 0.5)))+
      facet_wrap(~yr)+
      theme(panel.spacing = unit(0, "lines"),
            strip.text.x = element_blank(),
            strip.background = element_blank()) -> plot_list[[grep(i, fleets)]]
  }}
  if(type == "age"){for(i in fleets){
    tmp %>%
      dplyr::select(scenario, age_comp) %>%
      unnest(age_comp) %>%
      dplyr::select(-sex) %>%
      rename_all(tolower) %>%
      filter(fleet == i) %>%
      mutate(annotation = paste0("N adj = ", nsamp_adj, "\n", "N eff = ", round(effn, 1))) %>%
      
      ggplot()+
      geom_bar(aes(x = bin, y = obs), stat = "identity", position = "identity", color = 1, fill = "grey80")+
      geom_line(aes(x = bin, y = exp, color = scenario))+
      geom_text_npc(aes(npcx = "left", npcy = "top", label = yr), check_overlap = T)+
      geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation), check_overlap = T, size = 3)+
      labs(x = "Age", y = "Proportion", color = NULL)+
      scale_y_continuous(expand = expansion(mult = c(0, 0.5)))+
      facet_wrap(~yr)+
      theme(panel.spacing = unit(0, "lines"),
            strip.text.x = element_blank(),
            strip.background = element_blank()) -> plot_list[[grep(i, fleets)]]
  }}
  
  # write plots to directory
  purrr::map2(file_path, plot_list, ggsave, height = height, width = width, units = "in")
  
}
## recruimtent by model scenario
f_plot_recruit_comparison <- function(summaryoutput, models = mod_names, file_path, ylim = c(NA, NA)) {
 
  # estimate
  summaryoutput$recruits %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "rec") -> rec_dat
  # lower
  summaryoutput$recruitsLower %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "lwr") -> reclwr_dat
  # upper
  summaryoutput$recruitsUpper %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "upr") -> recupr_dat
 
  # plot
  left_join(rec_dat, reclwr_dat, by = c("yr", "scenario")) %>%
    left_join(recupr_dat, by = c("yr", "scenario")) %>%
    # plot
    ggplot()+
    geom_point(data = rec_dat %>% filter(yr == min(yr)),
               aes(x = yr, y = rec, color = scenario, shape = scenario))+
    geom_ribbon(aes(x = yr, ymin = lwr, ymax = upr, fill = scenario), alpha = 0.2)+
    geom_line(aes(x = yr, y = rec, color = scenario))+
    labs(x = NULL, y = "Recruitment (1,000s)", color = NULL, fill = NULL, shape = NULL)+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
    yr_axis+
    coord_cartesian(ylim = ylim)+
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) -> x
  
  # write plot to directory
  ggsave(file_path, plot = x,  height = 3, width = 5, units = "in")
  
  return(x)
  
}
## rec devs by model scenario
f_plot_recdev_comparison <- function(summaryoutput, models = mod_names, file_path, 
                                     legend.justification = c(1, 1), legend.position = c(1, 1),
                                     height = 3, width = 5) {
  
  # estimate
  summaryoutput$recdevs %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "rec") -> recdevs
  # lower
  summaryoutput$recdevsLower %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "lwr") -> lwr_dat
  # upper
  summaryoutput$recdevsUpper %>%
    as_tibble() %>%
    rename_all(~c(models, "label", "yr")) %>%
    dplyr::select(-label) %>%
    pivot_longer(models, names_to = "scenario", values_to = "upr") -> upr_dat
    
    # plot
  
  left_join(recdevs, lwr_dat, by = c("yr", "scenario")) %>%
    left_join(upr_dat, by = c("yr", "scenario")) %>%
    ggplot()+
    geom_hline(yintercept = 0, linetype = 2, color = "grey40")+
    geom_point(aes(x = yr, y = rec, color = scenario))+
    geom_errorbar(aes(x = yr, ymin = lwr, ymax = upr, color = scenario), width = 0, alpha = 0.3)+
    labs(x = NULL, y = "Log Recruitment Deviation", color = NULL, fill = NULL, shape = NULL)+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
    yr_axis+
    theme(legend.justification = legend.justification, legend.position = legend.position) -> x
  
  # write plot to directory
  ggsave(file_path, plot = x,  height = height, width = width, units = "in")
  
  return(x)
  
}
## args:
### retro_summary - model summary from r4ss
### terminal_yrs - retro model terminal years in decending order
### file_path - root path for saving plots
f_plot_retro <- function(retro_summary, terminal_yrs, file_path) {
  ## gather ssb data
  retro_summary$SpawnBio %>%
    dplyr::select(-Label) %>%
    pivot_longer(1:length(terminal_yrs), names_to = "end_yr", values_to = "ssb") %>%
    mutate(end_yr = terminal_yrs[as.numeric(gsub("replist", "", end_yr))]) %>%
    rename(prediction_yr = Yr) -> tmp_ssb
  ## compute mohns rho ssb
  tmp_ssb %>%
    filter(end_yr == terminal_yrs[1]) %>%
    rename(ssb_all_yr = ssb) %>%
    dplyr::select(prediction_yr, ssb_all_yr) %>%
    left_join(tmp_ssb, by = "prediction_yr") %>%
    filter(prediction_yr == end_yr,
           end_yr != 2018) %>%
    mutate(rho = (ssb - ssb_all_yr) / ssb_all_yr) %>%
    pull(rho) %>% mean %>% round(., 3) -> mohns_rho_ssb
  ## ssb plot
  tmp_ssb %>%
    filter(prediction_yr <= end_yr) %>%
    mutate(end_yr = factor(end_yr, levels = terminal_yrs)) %>%
    
    ggplot()+
    geom_line(aes(x = prediction_yr, y = ssb, group = end_yr, color = end_yr, linetype = end_yr))+
    viridis::scale_color_viridis(discrete = TRUE)+
    labs(x = NULL, y = "Spawning Biomass (t)", color = NULL, linetype = NULL)+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
    geom_text_npc(aes(npcx = "left", npcy = "top", label = paste0("Mohn's rho = ", mohns_rho_ssb)), 
                  check_overlap = T)+
    yr_axis -> plot_ssb
  ggsave(file.path(file_path, "retro_ssb.png"), plot = plot_ssb, height = 3, width = 5.5, units = "in")
  ## gather recruitment data
  retro_summary$recruits %>%
    dplyr::select(-Label) %>%
    pivot_longer(1:length(terminal_yrs), names_to = "end_yr", values_to = "rec") %>%
    mutate(end_yr = terminal_yrs[as.numeric(gsub("replist", "", end_yr))]) %>%
    rename(prediction_yr = Yr) -> tmp_rec
  ## compute mohns rho ssb
  tmp_rec %>%
    filter(end_yr == terminal_yrs[1]) %>%
    rename(rec_all_yr = rec) %>%
    dplyr::select(prediction_yr, rec_all_yr) %>%
    left_join(tmp_rec, by = "prediction_yr") %>%
    filter(prediction_yr == end_yr,
           end_yr != 2018) %>%
    mutate(rho = (rec - rec_all_yr) / rec_all_yr) %>%
    pull(rho) %>% mean %>% round(., 3) -> mohns_rho_rec
  ## rec plots
  tmp_rec %>%
    filter(prediction_yr <= end_yr) %>%
    mutate(end_yr = factor(end_yr, levels = terminal_yrs)) %>%
    
    ggplot()+
    geom_line(aes(x = prediction_yr, y = rec / 1000, group = end_yr, color = end_yr, linetype = end_yr))+
    viridis::scale_color_viridis(discrete = TRUE)+
    labs(x = NULL, y = "Recruitment (thousands)", color = NULL, linetype = NULL)+
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(5))+
    geom_text_npc(aes(npcx = "left", npcy = "top", label = paste0("Mohn's rho = ", mohns_rho_rec)), 
                  check_overlap = T)+
    yr_axis -> plot_rec
  ggsave(file.path(file_path, "retro_rec.png"), plot = plot_rec, height = 3, width = 5.5, units = "in")
  
}



f_extract_par_status <- function(mod_dir){
  
  ## read the model output and print some diagnostic messages 
  replist <- SS_output(dir = mod_dir, verbose=TRUE, printstats=TRUE)
  
  ## extract estimated parameters excluding devs
  replist$parameters %>%
    as_tibble() %>%
    rename_all(tolower) %>%
    filter(phase > 0, status != "act") %>%
    dplyr::select(label, value, parm_stdev, status, gradient) %>%
    
    write_csv(., file.path(mod_dir, "parameter_status.csv"))
  
}
f_extract_likelihood_comp <- function(mod_dir){
  
  ## read the model output and print some diagnostic messages 
  replist <- SS_output(dir = mod_dir, verbose=TRUE, printstats=TRUE)
  
  replist$likelihoods_by_fleet %>%
    as_tibble() %>%
    filter(!is.na(ALL)) %>%
    # remove all column
    dplyr::select(-ALL) %>%
    pivot_longer(c(2:ncol(.))) %>%
    rename_all(~c("process", "fleet", "nll")) %>%
    
    write_csv(., file.path(mod_dir, "likelihood_components_by_fleet.csv"))
}














