# notes ----
# Compare estimates of variance on round weight biomass and meat weight biomass
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# last updated: 2020/10/16

# load ----
library(tidyverse)
library(magrittr)
library(FNGr)
library(here)

### functions for survey data
source("./code/misc/statewide_scallop_survey_functions.R")

## global options
### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### set theme (from FNGr)
theme_set(theme_sleek() + theme(legend.position = "bottom"))


### set random number seed
set.seed(8456)

# data ----

## logbook data
logbook <- read_csv("./data/statewide_scallop_survey/logbook/HaulTableData_Cruise1901.csv") %>%
  mutate(year = 2019)

## catch data
catch_raw <- read_csv("./data/statewide_scallop_survey/catch/CatchTableData_Cruise1901.csv")

## specimen data
specimen <- read_csv("./data/statewide_scallop_survey/specimen/ScalMeatTableData_Cruise1901.csv")

## bed strata data
strata <- read_csv("./data/statewide_scallop_survey/bed_strata.csv")

# data mgmt ----
tows <- f_clean_log(logbook)
catch <- f_catch_by_tow(catch_raw, tows)
shaw <- f_get_shaw(specimen)
shad <- f_get_shad(specimen, catch)

# estimate abundance and variance ----

## design based estimates
catch %>%
  filter(rcode == 74120) %>%
  left_join(strata, by = "bed_code") %>%
  group_by(year, samp_grp, bed_code) %>%
  summarise(abundance = mean(cpue_cnt, na.rm = T) * mean(area_nm2),
            var = (var(cpue_cnt, na.rm = T) / n()) * mean(area_nm2)^2,
            cv = sqrt(var(cpue_cnt, na.rm = T) / n()) / mean(cpue_cnt, na.rm = T)) -> design_est_abund

## bootstrap estimates 
catch %>%
  filter(rcode == 74120) %>%
  left_join(strata, by = "bed_code") %>%
  group_by(year, samp_grp, bed_code) %>%
  summarise(abundance = mean(cpue_cnt, na.rm = T) * mean(area_nm2)) %>%
  left_join(catch %>%
              filter(rcode == 74120) %>%
              group_by(year, bed_code, samp_grp) %>%
              nest() %>%
              # resampling
              mutate(samp = purrr::map(data, ~rsample::bootstraps(., 1000))) %>%
              unnest(samp) %>%
              # compute mean cpue
              mutate(models = purrr::map(splits, ~boot_ci(.x))) %>%
              unnest(models) %>%
              # extrapolate cpue to area (nm2)
              left_join(strata, by = "bed_code") %>%
              mutate(abundance = cpue_cnt * area_nm2) %>%
              # compute variance of bootstrap samples
              group_by(year, bed_code, samp_grp) %>%
              summarise(var = var(abundance)),
            by = c("year", "bed_code", "samp_grp")) %>%
  mutate(cv = sqrt(var) / abundance) -> boot_est_abund


# estimate round weight biomass and variance ----

## design based estimates in kg
catch %>%
  filter(rcode == 74120) %>%
  left_join(strata, by = "bed_code") %>%
  group_by(year, samp_grp, bed_code) %>%
  summarise(biomass = mean(cpue_wt, na.rm = T) * mean(area_nm2),
            var = (var(cpue_wt, na.rm = T) / n()) * mean(area_nm2)^2,
            cv = sqrt(var(cpue_wt, na.rm = T) / n()) / mean(cpue_wt, na.rm = T)) -> design_est

## bootstrap estimates in kg 
catch %>%
  filter(rcode == 74120) %>%
  left_join(strata, by = "bed_code") %>%
  group_by(year, samp_grp, bed_code) %>%
  summarise(biomass = mean(cpue_wt, na.rm = T) * mean(area_nm2)) %>%
  left_join(catch %>%
              filter(rcode == 74120) %>%
              group_by(year, bed_code, samp_grp) %>%
              nest() %>%
              # resampling
              mutate(samp = purrr::map(data, ~rsample::bootstraps(., 1000))) %>%
              unnest(samp) %>%
              # compute mean cpue
              mutate(models = purrr::map(splits, ~boot_ci(.x))) %>%
              unnest(models) %>%
              # extrapolate cpue to area (nm2)
              left_join(strata, by = "bed_code") %>%
              mutate(biomass = cpue_wt * area_nm2) %>%
              # compute variance of bootstrap samples
              group_by(year, bed_code, samp_grp) %>%
              summarise(var = var(biomass)),
            by = c("year", "bed_code", "samp_grp")) %>%
  mutate(cv = sqrt(var) / biomass) -> boot_est


  


# estimate meat weight biomass and variance ----

## two-stage design based estimates in kg

### number of scallops by bed
catch %>%
  filter(rcode == 74120,
         samp_grp == 1) %>%
  group_by(year, bed_code, samp_grp, tow) %>%
  summarise(c = sum(samp_cnt, na.rm = T)) %>%
  ungroup() -> c

### number of tows by bed
tows %>%
  group_by(year, bed_code) %>%
  summarise(n_tow = n()) %>%
  ungroup() -> n_tows

### design based estimator
shaw %>%
  filter(rcode == 74120,
         samp_grp == 1) %>%
  # calculate estimate sample variance of meat weight by tow, within bed
  group_by(year, bed_code, tow) %>%
  summarise(m = n(),
            mw_bar = mean(meat_weight, na.rm = T), 
            var_mw_bar = var(meat_weight, na.rm = T) / m,
            .groups = "drop") %>%
  right_join(c, by = c("year", "bed_code", "tow")) %>%
  # compute mw_hat and mw cpue
  left_join(tows, by = c("year", "bed_code", "tow")) %>%
  mutate(mw_hat = c * mw_bar,
         u_i = mw_hat / (area_swept * 0.83)) %>%
  # change na's from hauls that had zero catch to 0
  replace_na(list(m = 0,
             mw_hat = 0,
             u_i = 0)) %>%
# compute total meat weight biomass
  left_join(n_tows, by = c("year", "bed_code")) %>%
  left_join(strata, by = "bed_code") %>%

  group_by(year, bed_code) %>%
  summarise(mw_biomass = mean(u_i, na.rm = T) * mean(area_nm2),
            var_mw_biomass = mean(area_nm2)^2 * (var(u_i, na.rm = T) / mean(n_tow)) + mean(area_nm2) * sum(c^2 * var_mw_bar, na.rm = T) / mean(n_tow)) %>%
  mutate(cv = sqrt(var_mw_biomass) / mw_biomass) -> mw_design_est
  
### bootstrap estimator

#### component 2
boot_ci_2 <- function(split){
  
  rsample::analysis(split) %>%
    summarize(mw_bar = mean(meat_weight, na.rm = T))
  
}

shaw %>%
  filter(rcode == 74120,
         samp_grp == 1) %>%
  group_by(year, bed_code, tow) %>%
  nest() %>%
  mutate(boot_2 = map(data, ~rsample::bootstraps(., 1000))) %>%
  unnest(boot_2) %>%
  mutate(models = map(splits, ~boot_ci_2(.x))) %>%
  unnest(models) %>%
  summarise(var_mw_bar = var(mw_bar)) %>%
  right_join(c, by = c("year", "bed_code", "tow")) %>%
  left_join(strata, by = "bed_code") %>%
  left_join(n_tows, by = c("year", "bed_code")) %>%
  group_by(year, bed_code) %>%
  summarise(comp2 = mean(area_nm2) * sum(c^2 * var_mw_bar, na.rm = T) / mean(n_tow)) -> var_comp2

#### component 1

boot_ci_1 <- function(split){
  
  rsample::analysis(split) %>%
    summarize(u_i = mean(u_i, na.rm = T))
  
}

shaw %>%
  filter(rcode == 74120,
         samp_grp == 1) %>%
  # calculate estimate sample variance of meat weight by tow, within bed
  group_by(year, bed_code, tow) %>%
  summarise(m = n(),
            mw_bar = mean(meat_weight, na.rm = T),
            .groups = "drop") %>%
  right_join(c, by = c("year", "bed_code", "tow")) %>%
  # compute mw_hat and mw cpue
  left_join(tows, by = c("year", "bed_code", "tow")) %>%
  mutate(mw_hat = c * mw_bar,
         u_i = mw_hat / (area_swept * 0.83)) %>%
  group_by(year, bed_code) %>%
  # boot strap variance component 1
  nest() %>%
  mutate(boot_1 = map(data, ~rsample::bootstraps(., 1000))) %>%
  unnest(boot_1) %>%
  mutate(models = map(splits, ~boot_ci_1(.x))) %>%
  unnest(models) %>%
  summarise(var = var(u_i)) %>%
  left_join(strata, by = "bed_code") %>%
  mutate(comp1 = area_nm2^2 * var) %>%
  dplyr::select(year, bed_code, comp1) %>%
  # join variance components and sum
  left_join(var_comp2, by = c("year", "bed_code")) %>%
  mutate(var_mw_biomass = comp1 + comp2) %>%
  # join to point estimate
  left_join(mw_design_est %>%
              dplyr::select(year, bed_code, mw_biomass),
            by = c("year", "bed_code")) %>%
  mutate(cv = sqrt(var_mw_biomass) / mw_biomass) %>%
  dplyr::select(year, bed_code, mw_biomass, var_mw_biomass, cv) -> mw_boot_est


  


# plots ----

## round weight biomass

design_est %>%
  dplyr::select(-cv) %>%
  left_join(boot_est %>%
              rename(boot_var = var) %>%
              dplyr::select(-cv)) %>%
  pivot_longer(c(var, boot_var), names_to = "type", values_to = "variance") %>%
  mutate(samp_grp = ifelse(samp_grp == 1, "Large", "Small"),
         cv = sqrt(variance) / biomass) %>%
  ggplot()+
  geom_point(aes(x = bed_code, y = biomass))+
  geom_errorbar(aes(x = bed_code, 
                    ymin = biomass * exp(-1.96 * sqrt(log(1 + cv^2))),
                    ymax = biomass * exp(1.96 * sqrt(log(1 + cv^2))),
                    color = type),
                width = 0.25)+
  scale_color_manual(values = 2:3, labels = c("Bootstrap", "Design"))+
  scale_y_continuous(labels = scales::comma)+
  facet_wrap(~samp_grp, nrow = 2, scales = "free")+
  labs(x = NULL, y = "Biomass (round kg)", color = NULL) -> x

ggsave(here("figures/fishery_independent/2019", "biomass_var_est_rnd.png"), plot = x, width = 5, height = 6, units = "in")

## meat weight biomass

mw_design_est %>%
  dplyr::select(-cv) %>%
  left_join(mw_boot_est %>%
              rename(boot_var = var_mw_biomass) %>%
              dplyr::select(-cv)) %>%
  pivot_longer(c(var_mw_biomass, boot_var), names_to = "type", values_to = "variance") %>%
  mutate(cv = sqrt(variance) / mw_biomass,
         mw_biomass = mw_biomass / 1000,
         variance = variance / 1000^2) %>%
  ggplot()+
  geom_point(aes(x = bed_code, y = mw_biomass))+
  geom_errorbar(aes(x = bed_code, 
                    ymin = mw_biomass * exp(-1.96 * sqrt(log(1 + cv^2))),
                    ymax = mw_biomass * exp(1.96 * sqrt(log(1 + cv^2))),
                    color = type),
                width = 0.25)+
  scale_color_manual(values = 2:3, labels = c("Bootstrap", "Design"))+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, y = "Biomass (meats kg)", color = NULL) -> x

ggsave(here("figures/fishery_independent/2019", "biomass_var_est_mw.png"), plot = x, width = 5, height = 3.5, units = "in")


# tables ----

write_csv(design_est_abund, here("output/fishery_independent/2019", "abundance_var_est_design.csv"))
write_csv(boot_est_abund, here("output/fishery_independent/2019", "abundance_var_est_boot.csv"))
write_csv(design_est, here("output/fishery_independent/2019", "biomass_var_est_design.csv"))
write_csv(boot_est, here("output/fishery_independent/2019", "biomass_var_est_boot.csv"))
write_csv(mw_design_est, here("output/fishery_independent/2019", "mw_biomass_var_est_design.csv"))
write_csv(mw_boot_est, here("output/fishery_independent/2019", "mw_biomass_var_est_boot.csv"))

