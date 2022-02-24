# notes ----
# 2019-2020 Statewide Scallops Survey data analysis
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# last updated: 2020/6/10

# load ----
library(tidyverse)
library(sf)
library(magrittr)
library(scales)
library(FNGr)
# for fitting lme and glme
library(lme4)

### functions for mapping
source("./code/misc/adfg_map_functions.R")
### functions for survey data
source("./code/misc/statewide_scallop_survey_functions.R")

## global options
### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### set theme (from FNGr)
theme_set(theme_sleek() + theme(legend.position = "bottom"))

### bed levels
bed_levels <- c("WK1", "EK1", "YAK3", "YAK4", "YAK5", "KSH1", "KNE1", "KNE2", "KNE3", "KNE4", "KNE5", "KNE6")

### set random number seed
set.seed(8456)

# data ----

## logbook data
read.csv("./data/statewide_scallop_survey/logbook/survey_log_ts_temporary.csv") %>%
  rename(year = cruise_year,
         start_lat = lat_start,
         start_lon = lon_start,
         end_lat = lat_end,
         end_lon = lon_end,
         distance = distance_nm,
         gear_perf = perform,
         bed_code = bed_name,
         avg_depth = depth_avg) -> logbook

## catch data
read.csv("./data/statewide_scallop_survey/catch/survey_catch_ts_temporary.csv") %>%
  rename(year = cruise_year,
         bed_code = bed_name) -> catch_raw

## specimen data
read.csv("./data/statewide_scallop_survey/specimen/survey_specimen_ts_temporary.csv") %>%
  #mutate(damage = as.character(damage)) %>%
  #bind_rows(read_csv("./data/statewide_scallop_survey/specimen/Cruise1901_2001_ScallopShellHeightAndWeightData.csv")) %>%
  rename(year = cruise_year,
         size = shell_height,
         bed_code = bed_name)  -> specimen
                    
## bed strata data
strata <- read_csv("./data/statewide_scallop_survey/bed_strata.csv")

# data mgmt ----

## tows
tows <- f_clean_log(logbook) 
catch <- f_catch_by_tow(catch_raw, tows)
shaw <- f_get_shaw(specimen)
shad <- f_get_shad(specimen, catch)

## remove tow 54 from 2019 (towed twice) and change name of bed (YAKB -> EK1)
filter(tows, tow != 19010054) %>%
  ## change the name of the east side of EK1
  mutate(bed_code = ifelse(bed_code == "YAKB", "EK1", bed_code)) -> tows
filter(catch, tow != 19010054) %>%
  ## change the name of the east side of EK1
  mutate(bed_code = ifelse(bed_code == "YAKB", "EK1", bed_code)) -> catch
filter(shaw, tow != 19010054) %>%
  ## change the name of the east side of EK1
  mutate(bed_code = ifelse(bed_code == "YAKB", "EK1", bed_code)) -> shaw
filter(shad, tow != 19010054) %>%
  ## change the name of the east side of EK1
  mutate(bed_code = ifelse(bed_code == "YAKB", "EK1", bed_code)) -> shad

## edit strata so that YAKB gets the code EK1 for consistency
strata %>%
  # filter for only active stations
  filter(status == "active") %>%
  ## remove data for west side of EK1
  filter(bed_code != "EK1") %>%
  ## change the name of the east side of EK1
  mutate(bed_code = ifelse(bed_code == "YAKB", "EK1", bed_code)) %>%
  # compute bed area
  group_by(bed_code) %>%
  summarise(area_nm2 = sum(area_nmi2_alb),
            n_stations = n()) %>%
  # filter for beds surveyed
  filter(bed_code %in% bed_levels) -> bed_area

# maps of tows ----

## vessel tracks
logbook %>%
  filter(haul_type %in% c("10", "Standard"),
         tow != 19010054) %>%
  dplyr::select(bed_code, stn_id, start_lat, start_lon, end_lat, end_lon) %>%
  unite(start_lat, start_lon, col = "start") %>%
  unite(end_lat, end_lon, col = "end") %>%
  pivot_longer(c(start, end), names_to = "position", values_to = "coords") %>%
  separate(col = coords, into = c("y", "x"), sep = "_", convert = T) %>%
  mutate(station = ifelse(bed_code == "WK1", 
                          paste0(substring(stn_id, 2, 2), " ", substring(stn_id , 3, 4)),
                          paste0(substring(stn_id, 2, 3), " ", substring(stn_id , 4, 6)))) %>%
  dplyr::select(-bed_code) -> vessel_track

## grid shapefile
f_shp_prep(path = "./data/maps/statewide_scallop_survey_grid", 
           layer = "scalGrid2020_standard_wgs84",
           fortify = F) %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs")) -> survey_grid

fortify(survey_grid) %>%
  full_join(fortify(survey_grid@data), by = "id") %>%
  mutate(towed = station %in% vessel_track$station) -> survey_grid

##KSH and KNE map
usa %>%
  st_as_sf() %>%
  ggplot()+
  geom_sf(fill = "grey 60", size = 0.4)+
  coord_sf(xlim = c(-154, -151.2), ylim = c(56.6, 58.8))+
  ggspatial::annotation_scale(location = "br")+
  geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
  #geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
  scale_fill_manual(values = c(NA, "firebrick"))+
  labs(x = "Longitude", y = "Latitude")+
  theme(legend.position = "none") -> x

ggsave("./figures/fishery_independent/2020/kshkne_dredge_survey_map.png", plot = x,
       height = 6, width = 6, units = "in")

##YAK map
usa %>%
  st_as_sf() %>%
  ggplot()+
  geom_sf(fill = "grey 60", size = 0.4)+
  coord_sf(xlim = c(-145, -140), ylim = c(59.2, 60.1))+
  ggspatial::annotation_scale(location = "br")+
  geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
  #geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
  scale_fill_manual(values = c(NA, "firebrick"))+
  labs(x = "Longitude", y = "Latitude")+
  theme(legend.position = "none") -> x

ggsave("./figures/fishery_independent/2020/yak_dredge_survey_map_a.png", plot = x,
       height = 4, width = 6, units = "in")

rbind(usa, can) %>%
  st_as_sf() %>%
  ggplot()+
  geom_sf(fill = "grey 60", size = 0.4)+
  coord_sf(xlim = c(-140, -137.5), ylim = c(58.4, 59.5))+
  ggspatial::annotation_scale(location = "br")+
  geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
  #geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
  scale_fill_manual(values = c(NA, "firebrick"))+
  labs(x = "Longitude", y = "Latitude")+
  theme(legend.position = "none") -> x

ggsave("./figures/fishery_independent/2020/yak_dredge_survey_map_b.png", plot = x,
       height = 4, width = 6, units = "in")


## KSH map
usa %>%
  st_as_sf() %>%
  ggplot()+
  geom_sf(fill = "grey 60")+
  coord_sf(xlim = c(-154.1, -153.2), ylim = c(58.4, 58.8))+
  ggspatial::annotation_scale(location = "tl")+
  geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
  geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
  scale_fill_manual(values = c(NA, "grey80"))+
  labs(x = "Longitude", y = "Latitude")+
  theme(legend.position = "none") -> x

ggsave("./figures/fishery_independent/2020/ksh_dredge_survey_map.png", plot = x,
       height = 5, width = 5, units = "in")

## KNE maps
usa %>%
  st_as_sf() %>%
  ggplot()+
  geom_sf(fill = "grey 60")+
  coord_sf(xlim = c(-153, -151), ylim = c(56.6, 58.1))+
  geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
  geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
  scale_fill_manual(values = c(NA, "grey80"))+
  labs(x = "Longitude", y = "Latitude")+
  theme(legend.position = "none") -> x

ggsave("./figures/fishery_independent/2020/kne_dredge_survey_map.png", plot = x,
       height = 5, width = 5, units = "in")

x +
  ggspatial::annotation_scale(location = "bl")+
  coord_sf(xlim = c(-153, -151.7), ylim = c(56.6, 57.4)) -> p1
ggsave("./figures/fishery_independent/2020/kne_dredge_survey_map_lower.png", plot = p1,
       height = 5, width = 5, units = "in")

x +
  ggspatial::annotation_scale(location = "br")+
  coord_sf(xlim = c(-152.3, -151), ylim = c(57.2, 58.1)) -> p1
ggsave("./figures/fishery_independent/2020/kne_dredge_survey_map_upper.png", plot = p1,
       height = 5, width = 5, units = "in")

# maps of cpue ----

## KSH1
catch %>%
  filter(bed_code == "KSH1", 
         rcode == 74120,
         year == 2020,
         samp_grp == 1) %>%
  left_join(logbook %>%
              dplyr::select(tow, stn_id), by = "tow") %>%
  mutate(station = paste0(substring(stn_id, 2, 3), " ", substring(stn_id , 4, 6))) %>%
  ungroup() %>%
  dplyr::select(year, station, samp_grp, cpue_cnt) %>%
  right_join(survey_grid, by = "station") -> cpue_grid


usa %>%
  st_as_sf() %>%
  ggplot()+
  geom_sf(fill = "grey 60")+
  coord_sf(xlim = c(-154.1, -153.2), ylim = c(58.4, 58.8))+
  geom_polygon(data = filter(cpue_grid, bed_code == "KSH1"), aes(x = long, y = lat, group = group, fill = cpue_cnt), alpha = 0.75, color = 1)+
  scale_fill_gradientn(colours = heat.colors(10, rev = T))+
  theme(legend.position = "right")+
  labs(x = "Long", y = "Lat", fill = "CPUE")-> x
ggsave("./figures/fishery_independent/2020/ksh_large_cpue_abund_map.png", plot = x,
       height = 12, width = 6, units = "in")


## KNE
catch %>%
  filter(grepl("KNE", bed_code), 
         rcode == 74120,
         year == 2020,
         samp_grp == 1) %>%
  left_join(logbook %>%
              dplyr::select(tow, stn_id), by = "tow") %>%
  mutate(station = paste0(substring(stn_id, 2, 3), " ", substring(stn_id , 4, 6))) %>%
  ungroup() %>%
  dplyr::select(year, station, samp_grp, cpue_cnt) %>%
  right_join(survey_grid, by = "station") -> cpue_grid


usa %>%
  st_as_sf() %>%
  ggplot()+
  geom_sf(fill = "grey 60")+
  coord_sf(xlim = c(-153, -151), ylim = c(56.6, 58.1))+
  geom_polygon(data = filter(cpue_grid, grepl("KNE", bed_code)), aes(x = long, y = lat, group = group, fill = cpue_cnt), alpha = 0.75, color = 1)+
  scale_fill_gradientn(colours = heat.colors(10, rev = T), trans = "log10")+
  theme(legend.position = "right")+
  labs(x = "Long", y = "Lat", fill = "CPUE")-> x
ggsave("./figures/fishery_independent/2020/kne_large_cpue_abund_map.png", plot = x,
       height = 12, width = 6, units = "in")

  
# table of tows by bed ----
tows %>%
  count(year, bed_code) %>%
  left_join(bed_area, by = "bed_code") %>%
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  arrange(bed_code) %>%
  dplyr::select(year, bed_code, n, n_stations, area_nm2) %T>%
  # write output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2020/2019_2020_strata.csv")

  
# calculate abundance and biomass ----

## point estimates (rnd wt biomass in lbs)
catch %>%
  filter(rcode == 74120) %>%
  left_join(bed_area, by = "bed_code") %>%
  group_by(year, samp_grp, bed_code) %>%
  summarise(cpue_abund = mean(cpue_cnt, na.rm = T),
            abundance = mean(cpue_cnt, na.rm = T) * mean(area_nm2),
            se_abund = sqrt(var(cpue_cnt, na.rm = T) / n() * mean(area_nm2)^2),
            cv_abund = sqrt(var(cpue_cnt, na.rm = T) / n()) / mean(cpue_cnt, na.rm = T),
            cpue_biomass = mean(cpue_wt, na.rm = T) * 2.20462,
            biomass = mean(cpue_wt, na.rm = T) * mean(area_nm2) * 2.20462,
            se_biomass = sqrt(var(cpue_wt, na.rm = T) / n() * mean(area_nm2)^2 * 2.20462^2),
            cv_biomass = sqrt(var(cpue_wt, na.rm = T) / n()) / mean(cpue_wt, na.rm = T)) %>%
  # add lognormal confience intervals
  mutate(abund_log_l95 = abundance * exp(-1.96 * sqrt(log(1 + cv_abund^2))),
         abund_log_u95 = abundance * exp(1.96 * sqrt(log(1 + cv_abund^2))),
         abund_ln_ci = paste0("[", round(abund_log_l95, 0), ", ", round(abund_log_u95, 0), "]"),
         biomass_log_l95 = biomass * exp(-1.96 * sqrt(log(1 + cv_biomass^2))),
         biomass_log_u95 = biomass * exp(1.96 * sqrt(log(1 + cv_biomass^2))),
         biomass_ln_ci = paste0("[", round(biomass_log_l95, 0), ", ", round(biomass_log_u95, 0), "]")) -> abundance_biomass

## print output table
abundance_biomass %>%
  dplyr::select(year, samp_grp, bed_code, cpue_abund, abundance, abund_ln_ci, cv_abund, cpue_biomass, biomass, 
                biomass_ln_ci, cv_biomass) %>%
  # rearrange rows
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  arrange(year, samp_grp, bed_code) %T>%
  # print output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2020/abundance_rnd_biomass_by_bed.csv")

## plots of abundance and biomass for presentation
### KSH1
#### abundance
abundance_biomass %>%
  filter(bed_code == "KSH1") %>%
  ungroup() %>%
  # add empty rows for skipped years
  add_row(year = 2019, samp_grp = 1:2) %>%
  ggplot(aes(x = year, y = abundance / 1000000, color = factor(samp_grp), shape = factor(samp_grp)))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(x = year, ymin = abund_log_l95 / 1000000, ymax = abund_log_u95 / 1000000), width = 0.1)+
  scale_color_manual(values = cb_palette[3:4], labels = c("large", "small"))+
  scale_shape_discrete(labels = c("large", "small"))+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Abundance (millions)", shape = NULL, color = NULL) -> x
ggsave("./figures/fishery_independent/2020/ksh_dredge_survey_abundance.png", plot = x,
       width = 5, height = 3, units = "in")
#### biomass
abundance_biomass %>%
  filter(bed_code == "KSH1") %>%
  ungroup() %>%
  # add empty rows for skipped years
  add_row(year = 2019, samp_grp = 1:2) %>%
  ggplot(aes(x = year, y = biomass, color = factor(samp_grp), shape = factor(samp_grp)))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(x = year, ymin = biomass_log_l95, ymax = biomass_log_u95), width = 0.1)+
  scale_color_manual(values = cb_palette[3:4], labels = c("large", "small"))+
  scale_shape_discrete(labels = c("large", "small"))+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Biomass (round lb)", shape = NULL, color = NULL) -> x
ggsave("./figures/fishery_independent/2020/ksh_dredge_survey_biomass.png", plot = x,
       width = 5, height = 3, units = "in")

### KNE
#### abundance
abundance_biomass %>%
  filter(grepl("KNE", bed_code)) %>%
  ungroup() %>%
  mutate(class = case_when(samp_grp == 1 ~ "large", 
                           samp_grp == 2 ~ "small")) %>%
  ggplot(aes(x = bed_code, y = abundance / 1000000, fill = factor(year)))+
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  geom_errorbar(aes(x = bed_code, ymin = abund_log_l95 / 1000000, ymax = abund_log_u95 / 1000000), 
                position = position_dodge2(preserve = "single"))+
  facet_wrap(~class, scales = "free", ncol = 1)+
  scale_fill_manual(values = cb_palette[c(3,8)])+
  coord_cartesian(ylim = c(0, 6))+
  labs(x = NULL, y = "Abundance (millions)", fill = NULL) -> x
ggsave("./figures/fishery_independent/2020/kne_dredge_survey_abundance.png", plot = x,
       width = 4, height = 5, units = "in")
#### biomass
abundance_biomass %>%
  filter(grepl("KNE", bed_code)) %>%
  ungroup() %>%
  mutate(class = case_when(samp_grp == 1 ~ "large", 
                           samp_grp == 2 ~ "small")) %>%
  ggplot(aes(x = bed_code, y = biomass, fill = factor(year)))+
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  geom_errorbar(aes(x = bed_code, ymin =biomass_log_l95, ymax = biomass_log_u95), 
                position = position_dodge2(preserve = "single"))+
  scale_y_continuous(labels = comma)+
  facet_wrap(~class, scales = "free", ncol = 1)+
  scale_fill_manual(values = cb_palette[c(3,8)])+
  labs(x = NULL, y = "Biomass (round lbs)", fill = NULL) -> x
ggsave("./figures/fishery_independent/2020/kne_dredge_survey_biomass.png", plot = x,
       width = 4, height = 5, units = "in")

## meat weight biomass
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
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(samp_grp == 1) %>%
  # calculate estimate sample variance of meat weight by tow, within bed
  group_by(year, bed_code, tow) %>%
  summarise(m = n(),
            mw_bar = mean(meat_wt, na.rm = T), 
            var_mw_bar = var(meat_wt, na.rm = T) / m,
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
  left_join(bed_area, by = "bed_code") %>%
  group_by(year, bed_code) %>%
  summarise(mw_biomass = mean(u_i, na.rm = T) * mean(area_nm2),
            var_mw_biomass = mean(area_nm2)^2 * (var(u_i, na.rm = T) / mean(n_tow)) + mean(area_nm2) * sum(c^2 * var_mw_bar, na.rm = T) / mean(n_tow)) %>%
  # convert grams to pounds
  mutate(mw_biomass = mw_biomass * 0.00220462,
         var_mw_biomass = var_mw_biomass * 0.00220462^2) %>%
  # compute cv and 95 % ci
  mutate(cv = sqrt(var_mw_biomass) / mw_biomass, 
         ln_l95 = mw_biomass * exp(-1.96 * sqrt(log(1 + cv^2))),
         ln_u95 = mw_biomass * exp(1.96 * sqrt(log(1 + cv^2))),
         ln_ci = paste0("[", round(ln_l95, 0), ", ", round(ln_u95, 0), "]")) %>%
  # rearrange rows
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  arrange(year, bed_code) -> mw_biomass

mw_biomass %>%
  ## print output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2020/mw_biomass_by_bed.csv")

## plots of meat weight biomass in mtons for presentation
### KSH1 
mw_biomass %>%
  filter(bed_code == "KSH1") %>%
  ungroup() %>%
  # add empty rows for skipped years
  add_row(year = 2019) %>%
  ggplot(aes(x = year, y = mw_biomass))+
  geom_point(color = cb_palette[3])+
  geom_line(color = cb_palette[3])+
  geom_errorbar(aes(x = year, ymin = ln_l95, ymax = ln_u95), width = 0.1, color = cb_palette[3])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Biomass (lb meat)", shape = NULL, color = NULL) -> x
ggsave("./figures/fishery_independent/2020/ksh_dredge_survey_mw_biomass.png", plot = x,
       width = 5, height = 3, units = "in")


### KNE
mw_biomass %>%
  filter(grepl("KNE", bed_code)) %>%
  ungroup() %>%
  ggplot(aes(x = bed_code, y = mw_biomass, fill = factor(year)))+
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  geom_errorbar(aes(x = bed_code, ymin = ln_l95, ymax = ln_u95), 
                position = position_dodge2(preserve = "single"))+
  scale_fill_manual(values = cb_palette[c(3,8)])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Biomass (lb meat)", fill = NULL) -> x
ggsave("./figures/fishery_independent/2020/kne_dredge_survey_mw_biomass.png", plot = x,
       width = 6, height = 4, units = "in")

## bootstrap confidence intervals
# catch %>%
#   filter(rcode == 74120) %>%
#   group_by(year, bed_code, samp_grp) %>%
#   nest() %>%
#   # resampling
#   mutate(samp = purrr::map(data, ~rsample::bootstraps(., 1000))) %>%
#   unnest(samp) %>%
#   # compute mean cpue
#   mutate(models = purrr::map(splits, ~boot_ci(.x))) %>%
#   unnest(models) %>%
#   # extrapolate cpue to area (nm2)
#   left_join(strata, by = "bed_code") %>%
#   mutate(abundance = cpue_cnt * area_nm2,
#          biomass = cpue_wt * area_nm2 * 2.20462) %>%
#   # get 95% quantiles
#   group_by(year, bed_code, samp_grp) %>%
#   summarise(l95_abundance = quantile(abundance, 0.025),
#             u95_abundance = quantile(abundance, 0.975),
#             l95_biomass = quantile(biomass, 0.025),
#             u95_biomass = quantile(biomass, 0.975)) %>%
#   # join to point estimates
#   left_join(est, by = c("year", "bed_code", "samp_grp")) %>%
#   dplyr::select(1:3, 8, 4:5, 9, 6:7) %T>%
#   # print output table
#   write_csv("./output/fishery_independent/statewide_scallop_survey/2019/abundance_biomass_by_bed.csv")
#   # print a plot
  



# shell height distribution ----

## 2019 all beds
shad %>%
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  ggplot()+
  geom_histogram(aes(x = size, y = ..density.., weight = sample_factor),
                 binwidth = 3, color = "black", fill = cb_palette[3])+
  facet_wrap(~bed_code, ncol = 2, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Density") -> p1

## 2020 all beds
shad %>%
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  filter(year == 2020) %>%
  ggplot()+
  geom_histogram(aes(x = size, y = ..density.., weight = sample_factor),
                 binwidth = 3, color = "black", fill = cb_palette[3])+
  facet_wrap(~bed_code, ncol = 2, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Density") -> p2

ggsave("./figures/fishery_independent/2020/2019_2020_dredge_survey_size_comp.png", plot = p1,
       width = 6, height = 7, units = "in")
ggsave("./figures/fishery_independent/2020/2020_dredge_survey_size_comp.png", plot = p2,
       width = 6, height = 4, units = "in")


## by bed and year
### KSH
shad %>%
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(bed_code == "KSH1") %>%
  ggplot()+
  geom_histogram(aes(x = size, y = ..density.., weight = sample_factor),
                 binwidth = 3, color = "black", fill = cb_palette[3])+
  geom_vline(xintercept = 100, linetype = 2)+
  facet_wrap(~year, ncol = 1, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Density") -> x

ggsave("./figures/fishery_independent/2020/ksh1_dredge_survey_size_comp.png", plot = x,
       width = 4, height = 6, units = "in")

### KNE
#### KNE 1-3
shad %>%
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(bed_code %in% c("KNE1", "KNE2", "KNE3")) %>%
  ggplot()+
  geom_histogram(aes(x = size, y = ..density.., weight = sample_factor),
                 binwidth = 3, color = "black", fill = cb_palette[3])+
  geom_vline(xintercept = 100, linetype = 2)+
  facet_grid(year~bed_code, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Density") -> x

ggsave("./figures/fishery_independent/2020/kne1-3_dredge_survey_size_comp.png", plot = x,
       width = 12, height = 4, units = "in")

#### KNE 4-6
shad %>%
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(bed_code %in% c("KNE4", "KNE5", "KNE6")) %>%
  ggplot()+
  geom_histogram(aes(x = size, y = ..density.., weight = sample_factor),
                 binwidth = 3, color = "black", fill = cb_palette[3])+
  geom_vline(xintercept = 100, linetype = 2)+
  facet_grid(year~bed_code, scales = "free_y")+
  labs(x = "Shell Height (mm)", y = "Density") -> x

ggsave("./figures/fishery_independent/2020/kne4-6_dredge_survey_size_comp.png", plot = x,
       width = 12, height = 4, units = "in")

# meat weight ~ shell height ----

## fit a random intercept model
shaw  %>%
  filter(!is.na(meat_wt), 
         !is.na(size)) %>%
  lmer(log(meat_wt) ~ log(size) + (1 | bed_code), data = .) -> lme_mwsh_intercept

## fit a random slope model
shaw  %>%
  filter(!is.na(meat_wt), 
         !is.na(size)) %>%
  lmer(log(meat_wt) ~ log(size) + (log(size) - 1| bed_code), data = .) -> lme_mwsh_slope

## model comparison
AIC(lme_mwsh_intercept, lme_mwsh_slope)

## coefficients
coef(lme_mwsh_intercept)$bed_code %>%
  rownames_to_column(var = "bed_code") %>%
  rename(alpha = `(Intercept)`,
         beta = `log(size)`) %>%
  write_csv("./output/fishery_independent/statewide_scallop_survey/2020/growth_params_log_by_bed.csv")

## plot
### scatter plot
shaw %>%
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  filter(!is.na(meat_wt), 
         !is.na(size)) %>%
  bind_cols(merTools::predictInterval(lme_mwsh_intercept, newdata = ., type = "linear.prediction")) %>%
  mutate_at(c("fit", "upr", "lwr"), exp) %>%
  ggplot()+
  geom_point(aes(x = size, y = meat_wt), color = cb_palette[1], alpha = 0.5)+
  geom_line(aes(x = size, y = fit))+
  geom_ribbon(aes(x = size, ymin = lwr, ymax = upr), fill = cb_palette[3], alpha = 0.5)+
  facet_wrap(~bed_code, ncol = 3)+
  labs(x = "Shell Height (mm)", y = "Meat Weight (g)") -> p1

### random effects
as_tibble(lme4::ranef(lme_mwsh_intercept, which = "bed_code", condVar = TRUE, drop = F)) %>%
  mutate(grp = factor(grp, levels = bed_levels)) %>%
  ggplot()+
  geom_point(aes(x = condval, y = grp))+
  geom_errorbar(aes(xmin = condval - 2 * condsd,
                    xmax = condval + 2 * condsd,
                    y = grp),
                width = 0)+
  geom_vline(xintercept = 0, linetype = 2)+
  scale_y_discrete(limits = rev(bed_levels))+
  scale_x_continuous(limits = c(-0.155, 0.155))+
  labs(x = "Random Effects", y = NULL) -> p2


cowplot::plot_grid(p1, p2, rel_widths = c(1, 0.4)) -> x
       

ggsave("./figures/fishery_independent/2020/2019_2020_statewide_scallop_survey_mw_sh.png", plot = x, 
      height = 5, width = 8, units = "in")


### mw ~ shell height by bed
#### KSH1
shaw %>%
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(bed_code == "KSH1",
         !is.na(meat_wt), 
         !is.na(size)) %>%
  ggplot()+
  geom_jitter(aes(x = size, y = meat_wt, color = factor(year)), alpha = 0.5)+
  geom_smooth(aes(x = size, y = meat_wt, color = factor(year)), method = "gam", se = F)+
  scale_color_manual(values = cb_palette[c(2:4, 8)])+
  labs(x = "Shell Height (mm)", y = "Meat Weight (g)", color = NULL)+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) -> x


ggsave("./figures/fishery_independent/2020/ksh1_mw_sh.png", plot = x, 
       height = 4, width = 6, units = "in")


#### KNE
shaw %>%
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(grepl("KNE", bed_code),
         !is.na(meat_wt), 
         !is.na(size)) %>%
  ggplot()+
  geom_jitter(aes(x = size, y = meat_wt, color = factor(year), shape = bed_code), alpha = 0.5)+
  geom_smooth(aes(x = size, y = meat_wt, color = factor(year)), method = "gam", se = F)+
  scale_color_manual(values = cb_palette[c(3, 8)])+
  labs(x = "Shell Height (mm)", y = "Meat Weight (g)", color = NULL, shape = NULL)+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) -> x

ggsave("./figures/fishery_independent/2020/kne_mw_sh.png", plot = x, 
       height = 4, width = 6, units = "in")


# meat weight ~ round weight ----
shaw %>%
  filter(!is.na(meat_wt)) %>%
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  ggplot()+
  geom_point(aes(x = whole_wt, y = meat_wt), color = cb_palette[1], alpha = 0.3)+
  geom_smooth(aes(x = whole_wt, y = meat_wt), method = "gam", se = F, color = "black")+
  geom_line(aes(x = whole_wt, y = whole_wt * 0.1), linetype = 2)+
  facet_wrap(~bed_code, scales = "free", nrow = 4)+
  labs(x = "Round Weight (g)", y = "Meat Weight (g)") -> p1

shaw %>%
  filter(!is.na(meat_wt),
         year == 2020) %>%
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  ggplot()+
  geom_point(aes(x = whole_wt, y = meat_wt), color = cb_palette[1], alpha = 0.3)+
  geom_smooth(aes(x = whole_wt, y = meat_wt), method = "gam", se = F, color = "black")+
  geom_line(aes(x = whole_wt, y = whole_wt * 0.1), linetype = 2)+
  facet_wrap(~bed_code, scales = "free")+
  labs(x = "Round Weight (g)", y = "Meat Weight (g)") -> p2

ggsave("./figures/fishery_independent/2020/2019_2020_dredge_survey_rwt_mwt.png", plot = p1,
       width = 6, height = 7, units = "in")
ggsave("./figures/fishery_independent/2020/2020_dredge_survey_rwt_mwt.png", plot = p2,
       width = 6, height = 4, units = "in")


### mw ~ rnd weight by bed
#### KSH1
shaw %>%
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(bed_code == "KSH1",
         !is.na(meat_wt)) %>%
  ggplot()+
  geom_jitter(aes(x = whole_wt, y = meat_wt, color = factor(year)), alpha = 0.5)+
  geom_smooth(aes(x = whole_wt, y = meat_wt, color = factor(year)), method = "gam", se = F)+
  geom_line(aes(x = whole_wt, y = whole_wt * 0.1), linetype = 2, size = 1)+
  scale_color_manual(values = cb_palette[c(2:4, 8)])+
  labs(x = "Round Weight(g)", y = "Meat Weight (g)", color = NULL)+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) -> x

ggsave("./figures/fishery_independent/2020/ksh1_mw_rw.png", plot = x, 
       height = 4, width = 6, units = "in")

#### KNE
shaw %>%
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(grepl("KNE", bed_code),
         !is.na(meat_wt)) %>%
  ggplot()+
  geom_jitter(aes(x = whole_wt, y = meat_wt, color = factor(year), shape = bed_code), alpha = 0.5)+
  geom_smooth(aes(x = whole_wt, y = meat_wt, color = factor(year)), method = "gam", se = F)+
  geom_line(aes(x = whole_wt, y = whole_wt * 0.1), linetype = 2, size = 1)+
  scale_color_manual(values = cb_palette[c(3, 8)])+
  labs(x = "Round Weight(g)", y = "Meat Weight (g)", color = NULL, shape = NULL)+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) -> x

ggsave("./figures/fishery_independent/2020/kne_mw_rw.png", plot = x, 
       height = 4, width = 6, units = "in")

# pathologies and sex ratio ----

# extract prop estimate and p-value
f_p_prop <- function(x) {
  tibble(prop_est = x$estimate,
         prop_conf = paste0("[", round(x$conf.int[1], 2), ", ", round(x$conf.int[2], 2), "]"),
         pval = x$p.value)
}

shaw %>%
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(samp_grp == 1) %>%
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  group_by(year, bed_code) %>%
  summarise(sex_rat = sum(sex == 1) / sum(sex == 2),
            n_male = sum(sex == 1),
            n_shucked = n()) %>%
  mutate(prop_test = purrr::map2(n_male, n_shucked, prop.test),
         res = purrr::map(prop_test, f_p_prop)) %>%
  unnest(res) %>%
  # write output table
  dplyr::select(-prop_test) %T>%
  write_csv("./output/fishery_independent/statewide_scallop_survey/2020/sex_ratio_by_bed.csv")


## weak meats
shaw %>% 
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  
  rename(bed_code = bed_code.y) %>%
  #mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  group_by(year, bed_code, samp_grp) %>%
  summarise(n_shucked = n(),
            wk_mts = sum(meat_condition) / n() * 100) -> weak_meat

## mud blisters
shaw %>% 
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  filter(!is.na(mud_blister)) %>%
  group_by(year, bed_code, samp_grp) %>%
  mutate(n_shucked = n()) %>%
  group_by(year, bed_code, mud_blister, samp_grp) %>%
  summarise(perc = n() / mean(n_shucked) * 100) %>%
  mutate(mud_blister = paste0("mb", mud_blister)) %>%
  pivot_wider(names_from = mud_blister, values_from = perc) %>%
  replace_na(list(mb0 = 0, mb1 = 0, mb2 = 0, mb3 = 0)) %>%
  mutate(mb4 = 0) -> mud_blister

## shell worms
shaw %>% 
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  filter(!is.na(shell_worm)) %>%
  group_by(year, bed_code, samp_grp) %>%
  mutate(n_shucked = n()) %>%
  group_by(year, bed_code, samp_grp, shell_worm) %>%
  summarise(perc = n() / mean(n_shucked) * 100) %>%
  mutate(shell_worm = paste0("sw", shell_worm)) %>%
  pivot_wider(names_from = shell_worm, values_from = perc) %>%
  replace_na(list(sw0 = 0, sw1 = 0, sw2 = 0, sw3 = 0)) %>%
  mutate(sw4 = 0) -> shell_worm

## join and print output table
left_join(weak_meat, mud_blister, by = c("year", "bed_code", "samp_grp")) %>%
  left_join(shell_worm, by = c("year", "bed_code", "samp_grp")) %>%
  # rearrange rows
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  arrange(samp_grp, bed_code) %T>%
  write_csv("./output/fishery_independent/statewide_scallop_survey/2020/pathologies_by_bed.csv")

# kne weak meat plot
weak_meat %>%
  filter(grepl("KNE", bed_code),
         samp_grp == 1) %>%
  ggplot()+
  geom_bar(aes(x = bed_code, y = wk_mts), stat = "identity")+
  facet_wrap(~year, nrow = 2)+
  labs(x = NULL, y = "Percent Weak Meats") -> x
ggsave("./figures/fishery_independent/2020/kns_weak_meats.png", plot = x, 
       height = 5, width = 3, units = "in")  

# gonad condition ----
shaw %>% 
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  #mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  filter(!is.na(gonad),
         samp_grp == 1) %>%
  group_by(year, bed_code) %>%
  mutate(n_shucked = n()) %>%
  group_by(year, bed_code, samp_grp, gonad) %>%
  summarise(perc = n() / mean(n_shucked) * 100) %>%
  mutate(gonad = paste0("g", gonad)) %>%
  pivot_wider(names_from = gonad, values_from = perc) %>%
  replace_na(list(g0 = 0, g2 = 0, g3 = 0, g4 = 0)) %>%
  mutate(g1 = 0,
         g5 = 0) %>%
  # reorder columns
  dplyr::select(year, bed_code, samp_grp, g0, g1, g2, g3, g4, g5) %>%
  # rename data codes
  rename_at(4:9, ~tolower(c("immature", "empty", "initial_recovery", "filling", "full", "cannot_determine"))) %>%
  # write output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2020/2019_2020_gonad_condition.csv")

# gonad size comp
shaw %>% 
  left_join(logbook %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  #mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  filter(!is.na(gonad),
         !is.na(size)) %>%
  dplyr::select(year, bed_code, gonad, size) %>%
  # 10 mm size bin
  mutate(size_bin = round(size / 10, 0) * 10) %>%
  count(bed_code, year, size_bin, gonad) %>%
  group_by(bed_code, year, size_bin) %>%
  mutate(prop = n / sum(n))  %>%
  ungroup() %>%
  mutate(gonad = case_when(gonad == 0 ~ "immature", 
                           gonad == 1 ~ "empty",
                           gonad == 2 ~ "initial recovery",
                           gonad == 3 ~ "filling",
                           gonad == 4 ~ "full",
                           gonad == 5 ~ "cannot determine"),
         gonad = factor(gonad, levels = c("immature", "empty", "initial recovery",
                                          "filling", "full", "cannot determine"))) %>%
  
  ggplot()+
  geom_bar(aes(x = size_bin, y = prop, fill = gonad), position = "stack", stat = "identity")+
  facet_wrap(year~bed_code, scales = "free")+
  labs(x = "Size Bin (10 mm)", y = "Proportion", fill = NULL) -> x
  
ggsave("./figures/fishery_independent/2020/gonad_at_size.png", plot = x, 
       height = 10, width = 12, units = "in")




# size at maturity ----

shaw %>% 
  filter(!is.na(gonad),
         gonad != 5) %>%
  mutate(maturity = ifelse(gonad == 0, 0, 1)) %>%
  glmer(maturity ~ size + (1 | bed_code), family = "binomial", data = .) -> glmer_intercept

shaw %>% 
  filter(!is.na(gonad),
         gonad != 5) %>%
  mutate(maturity = ifelse(gonad == 0, 0, 1)) %>%
  glmer(maturity ~ size + (size - 1 | bed_code), family = "binomial", data = .) -> glmer_slope

AIC(glmer_slope, glmer_intercept)


## estimate size at 50% maturity
coef(glmer_slope)$bed_code %>%
  rownames_to_column(var = "bed_code") %>%
  rename(beta_0 = `(Intercept)`,
         beta_1 = size) %>%
  mutate(l50 = -beta_0 / beta_1) -> size_maturity

## function for estimating size at maturity by bed
# f_sh50 <- function(x){
#   (-as.data.frame(coef(x)[[1]])[[1]] / as.data.frame(coef(x)[[1]])[[2]])
# }

## run bootstrap 
# lme4::bootMer(glmer_slope, FUN = f_sh50, nsim = 5000) %>%
#   saveRDS("./output/fishery_independent/2020/boot_glmer.RDS")

## read bootstrap estimates of l50
readRDS("./output/fishery_independent/2020/boot_glmer.RDS")[[2]] %>%
  as_tibble() %>%
  rename_all(~rownames(coef(glmer_slope)[[1]])) %>%
  # compute confidence intervals from quantiles
  purrr::map(., .f = quantile, probs = c(0.0275, 0.975)) %>%
  # reformat
  purrr::map(., t) %>%
  purrr::map(., as.data.frame) %>%
  do.call("rbind", .) %>%
  rownames_to_column(var = "bed_code") %>%
  setNames(., c("bed_code", "l95", "u95")) %>%
  # join to point estimates
  left_join(size_maturity, ., by = "bed_code") %>%
  # create column of combined confidence intervals
  mutate(ci95 = paste0("[", round(l95, 1), ", ", round(u95, 1), "]")) %>%
  # rearrange columns
  mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  arrange(bed_code) %>%
  # write to output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2020/2019_2020_statewide_scallop_survey_maturity.csv")


## plot

## model fit
tibble(size = seq(min(shaw$size, na.rm = T), max(shaw$size, na.rm = T), 0.1)) %>%
  expand_grid(bed_code = bed_levels) %>%
  # filter for appropriate prediction range
  left_join(shaw %>%
              group_by(bed_code) %>%
              summarise(min_size = min(size, na.rm = T),
                        max_size = max(size, na.rm = T)),
            by = "bed_code") %>%
  filter(size >= min_size, 
         size <= max_size) %>%
  bind_cols(merTools::predictInterval(glmer_slope, newdata = ., type = "probability")) %>%
  mutate(bed_code = factor(bed_code, levels = bed_levels)) -> curve

## scatter plot
shaw %>% 
  filter(!is.na(gonad),
         gonad != 5) %>%
  mutate(maturity = ifelse(gonad == 0, 0, 1),
         bed_code = factor(bed_code, levels = bed_levels)) %>%
  ggplot()+ 
  geom_point(aes(x = size, y = maturity), shape = 124, alpha = 0.8, size = 0.5, color = cb_palette[1])+
  geom_ribbon(data = curve, aes(x = size, ymin = lwr, ymax = upr), fill = cb_palette[3], alpha = 0.5)+
  geom_line(data = curve, aes(x = size, y = fit))+
  facet_wrap(~bed_code, nrow = 4)+
  labs(x = "Shell Height (mm)", y = "Probability") -> p1

## random effects
as_tibble(lme4::ranef(glmer_slope, which = "bed_code", condVar = TRUE, drop = F)) %>%
  mutate(grp = factor(grp, levels = bed_levels)) %>%
  ggplot()+
  geom_point(aes(x = condval, y = grp))+
  geom_errorbar(aes(xmin = condval - 2 * condsd,
                    xmax = condval + 2 * condsd,
                    y = grp),
                width = 0)+
  geom_vline(xintercept = 0, linetype = 2)+
  scale_y_discrete(limits = rev(bed_levels))+
  scale_x_continuous(limits = c(-0.08, 0.08))+
  labs(x = "Random Effects", y = NULL) -> p2

cowplot::plot_grid(p1, p2, rel_widths = c(1, 0.4)) -> x


ggsave("./figures/fishery_independent/2020/2019_2020_statewide_scallop_survey_maturity.png", plot = x, 
       height = 5, width = 8, units = "in")







fish_stats %>%
  filter(year == 2020)

## object from statewide_fishery_performance.R
exists("fish_stats")

# cpue with fishery data
read_csv("./output/fishery_independent/statewide_scallop_survey/2020/abundance_rnd_biomass_by_bed.csv") %>%
  filter(bed_code %in% grep("KSH|KNE", bed_code, value = T),
         samp_grp == 1) %>%
  right_join(expand_grid(bed_code = c("KNE1", "KNE2", "KNE3", "KNE4", "KNE5", "KNE6", "KSH1"),
                         year = 2016:2020)) %>%
  mutate(district = ifelse(bed_code == "KSH1", "KSH", "KNE")) %>%
  
  # join to fishery cpue
  left_join(fish_stats %>%
              dplyr::select(year, district, mt_cpue) %>%
               filter(year >= 2016)) %>%
  
  ggplot()+ 
  geom_point(aes(x = factor(year), y = mt_cpue*1000))+
  geom_line(aes(x = factor(year), y = mt_cpue *1000, group = 1), linetype = 2)+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Fishery CPUE (mt lb / dredg hr)\n "), labels = comma)+
  geom_point(aes(x = factor(year), y = cpue_abund, color= bed_code))+
  geom_line(aes(x = factor(year), y = cpue_abund, group = bed_code, color= bed_code))+
  scale_color_manual(values = cb_palette[2:8])+
  labs(x = NULL, y = bquote("Survey CPUE (Scallops /" ~ " "~ nm^-2~ ")"), color = NULL)+
  facet_wrap(~district, nrow = 2, scales = "free") -> x
ggsave("./figures/fishery_independent/2020/2020_survey_fishery_cpue.png", plot = x, 
       height = 6, width = 6, units = "in")

