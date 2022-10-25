# notes ----
# 2022 Statewide Scallops Survey data analysis
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# last updated: 2022/10/14

# load ----
library(sf)

### functions for mapping
source("./code/misc/adfg_map_functions.R")
### functions for survey data
source("./code/misc/statewide_scallop_survey_functions.R")

### bed levels
bed_levels <- c("KSH1", paste0("KNE", 1:6))

### set random number seed
set.seed(8456)

## create directories 

if(!dir.exists("./output/fishery_independent/statewide_scallop_survey/2022")){
  dir.create("./output/fishery_independent/statewide_scallop_survey/2022")
}
if(!dir.exists("./figures/fishery_independent/2022")){
  dir.create("./figures/fishery_independent/2022")
}



# data ----

## logbook data
logbook <- read.csv("./data/statewide_scallop_survey/logbook/DredgeSurvey_FishLogData_RegularSurveyHauls.csv") 

## catch data
catch_raw <- read.csv("./data/statewide_scallop_survey/catch/DredgeSurvey_CatchData_RegularSurveyHauls.csv") 

## specimen data
specimen <- read.csv("./data/statewide_scallop_survey/specimen/DredgeSurvey_ScallopBiologicalData_RegularSurveyHauls.csv")

## ctd data
#ctd <- read.csv("./data/statewide_scallop_survey/survey2101_CTDpH_datasummary.csv", na.strings = "no data")

# data mgmt ----

## tows
tows <- f_clean_log(logbook) 
catch <- f_catch_by_tow(catch_raw, tows)
shaw <- f_get_shaw(specimen)
shad <- f_get_shad(specimen, catch)

## edit strata so that YAKB gets the code EK1 for consistency
strata %>% # strata loaded in custom functions
  # filter for only active stations
  filter(status == "A") %>% 
  ## compute bed area
  group_by(bed_code, bed_name, district) %>%
  summarise(area_nm2 = sum(sq_nmi_alb),
            n_stations = n()) -> bed_area

# # maps of tows ----
# 
# ## vessel tracks
# logbook %>%
#   filter(haul_type %in% c("10", "Standard"),
#          tow != 19010054) %>%
#   dplyr::select(bed_code, stn_id, start_lat, start_lon, end_lat, end_lon) %>%
#   unite(start_lat, start_lon, col = "start") %>%
#   unite(end_lat, end_lon, col = "end") %>%
#   pivot_longer(c(start, end), names_to = "position", values_to = "coords") %>%
#   separate(col = coords, into = c("y", "x"), sep = "_", convert = T) %>%
#   mutate(station = ifelse(bed_code == "WK1", 
#                           paste0(substring(stn_id, 2, 2), " ", substring(stn_id , 3, 4)),
#                           paste0(substring(stn_id, 2, 3), " ", substring(stn_id , 4, 6)))) %>%
#   dplyr::select(-bed_code) -> vessel_track
# 
# ## grid shapefile
# f_shp_prep(path = "./data/maps/statewide_scallop_survey_grid", 
#            layer = "scalGrid2020_standard_wgs84",
#            fortify = F) %>%
#   spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs")) -> survey_grid
# 
# fortify(survey_grid) %>%
#   full_join(fortify(survey_grid@data), by = "id") %>%
#   mutate(towed = station %in% vessel_track$station) -> survey_grid
# 
# ##KSH and KNE map
# usa %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60", size = 0.4)+
#   coord_sf(xlim = c(-154, -151.2), ylim = c(56.6, 58.8))+
#   ggspatial::annotation_scale(location = "br")+
#   geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
#   #geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
#   scale_fill_manual(values = c(NA, "firebrick"))+
#   labs(x = "Longitude", y = "Latitude")+
#   theme(legend.position = "none") -> x
# 
# ggsave("./figures/fishery_independent/2020/kshkne_dredge_survey_map.png", plot = x,
#        height = 6, width = 6, units = "in")
# 
# ##YAK map
# usa %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60", size = 0.4)+
#   coord_sf(xlim = c(-145, -140), ylim = c(59.2, 60.1))+
#   ggspatial::annotation_scale(location = "br")+
#   geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
#   #geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
#   scale_fill_manual(values = c(NA, "firebrick"))+
#   labs(x = "Longitude", y = "Latitude")+
#   theme(legend.position = "none") -> x
# 
# ggsave("./figures/fishery_independent/2020/yak_dredge_survey_map_a.png", plot = x,
#        height = 4, width = 6, units = "in")
# 
# rbind(usa, can) %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60", size = 0.4)+
#   coord_sf(xlim = c(-140, -137.5), ylim = c(58.4, 59.5))+
#   ggspatial::annotation_scale(location = "br")+
#   geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
#   #geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
#   scale_fill_manual(values = c(NA, "firebrick"))+
#   labs(x = "Longitude", y = "Latitude")+
#   theme(legend.position = "none") -> x
# 
# ggsave("./figures/fishery_independent/2020/yak_dredge_survey_map_b.png", plot = x,
#        height = 4, width = 6, units = "in")
# 
# 
# ## KSH map
# usa %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60")+
#   coord_sf(xlim = c(-154.1, -153.2), ylim = c(58.4, 58.8))+
#   ggspatial::annotation_scale(location = "tl")+
#   geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
#   geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
#   scale_fill_manual(values = c(NA, "grey80"))+
#   labs(x = "Longitude", y = "Latitude")+
#   theme(legend.position = "none") -> x
# 
# ggsave("./figures/fishery_independent/2020/ksh_dredge_survey_map.png", plot = x,
#        height = 5, width = 5, units = "in")
# 
# ## KNE maps
# usa %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60")+
#   coord_sf(xlim = c(-153, -151), ylim = c(56.6, 58.1))+
#   geom_polygon(data = survey_grid, aes(x = long, y = lat, group = group, fill = towed), color = 1)+
#   geom_line(data = vessel_track, aes(x = x, y = y, group = stn_id), color = "red", size = 1)+
#   scale_fill_manual(values = c(NA, "grey80"))+
#   labs(x = "Longitude", y = "Latitude")+
#   theme(legend.position = "none") -> x
# 
# ggsave("./figures/fishery_independent/2020/kne_dredge_survey_map.png", plot = x,
#        height = 5, width = 5, units = "in")
# 
# x +
#   ggspatial::annotation_scale(location = "bl")+
#   coord_sf(xlim = c(-153, -151.7), ylim = c(56.6, 57.4)) -> p1
# ggsave("./figures/fishery_independent/2020/kne_dredge_survey_map_lower.png", plot = p1,
#        height = 5, width = 5, units = "in")
# 
# x +
#   ggspatial::annotation_scale(location = "br")+
#   coord_sf(xlim = c(-152.3, -151), ylim = c(57.2, 58.1)) -> p1
# ggsave("./figures/fishery_independent/2020/kne_dredge_survey_map_upper.png", plot = p1,
#        height = 5, width = 5, units = "in")
# 
# 
# # ctd maps ----
# 
# ## temperature
# usa %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60", size = 0.4)+
#   coord_sf(xlim = c(-145, -140), ylim = c(59.2, 60.1))+
#   ggspatial::annotation_scale(location = "br")+
#   geom_point(data = ctd, aes(x = longitude_upcast, y = latitude_upcast, color = temp_atmaxdepth), size = 2)+
#   scale_color_gradientn(colours = topo.colors(10))+
#   labs(x = NULL, y = NULL, color = expression('Temperature ('*~degree*C*')'))+
#   theme(legend.position = "right")-> x
# 
# 
# ## pH
# usa %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60", size = 0.4)+
#   coord_sf(xlim = c(-145, -140), ylim = c(59.2, 60.1))+
#   ggspatial::annotation_scale(location = "br")+
#   geom_point(data = as_tibble(ctd), aes(x = longitude_upcast, y = latitude_upcast, color = avg_pH), size = 2)+
#   scale_color_gradientn(colours = terrain.colors(10))+
#   labs(x = NULL, y = NULL, color = "pH")+
#   theme(legend.position = "right")-> y
# 
# ## salinity
# usa %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60", size = 0.4)+
#   coord_sf(xlim = c(-145, -140), ylim = c(59.2, 60.1))+
#   ggspatial::annotation_scale(location = "br")+
#   geom_point(data = as_tibble(ctd), aes(x = longitude_upcast, y = latitude_upcast, color = sal_atmaxdepth), size = 2)+
#   labs(x = NULL, y = NULL, color = "Salinity (ppm)")+
#   theme(legend.position = "right") -> z
# 
# ggsave("./figures/fishery_independent/2021/ctd_casts.png", plot = x / y / z,
#        height = 9, width = 6, units = "in")
# 
# # maps of cpue ----
# 
# ## KSH1
# catch %>%
#   filter(bed_code == "KSH1", 
#          rcode == 74120,
#          year == 2020,
#          samp_grp == 1) %>%
#   left_join(logbook %>%
#               dplyr::select(tow, stn_id), by = "tow") %>%
#   mutate(station = paste0(substring(stn_id, 2, 3), " ", substring(stn_id , 4, 6))) %>%
#   ungroup() %>%
#   dplyr::select(year, station, samp_grp, cpue_cnt) %>%
#   right_join(survey_grid, by = "station") -> cpue_grid
# 
# 
# usa %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60")+
#   coord_sf(xlim = c(-154.1, -153.2), ylim = c(58.4, 58.8))+
#   geom_polygon(data = filter(cpue_grid, bed_code == "KSH1"), aes(x = long, y = lat, group = group, fill = cpue_cnt), alpha = 0.75, color = 1)+
#   scale_fill_gradientn(colours = heat.colors(10, rev = T))+
#   theme(legend.position = "right")+
#   labs(x = "Long", y = "Lat", fill = "CPUE")-> x
# ggsave("./figures/fishery_independent/2020/ksh_large_cpue_abund_map.png", plot = x,
#        height = 12, width = 6, units = "in")
# 
# 
# ## KNE
# catch %>%
#   filter(grepl("KNE", bed_code), 
#          rcode == 74120,
#          year == 2020,
#          samp_grp == 1) %>%
#   left_join(logbook %>%
#               dplyr::select(tow, stn_id), by = "tow") %>%
#   mutate(station = paste0(substring(stn_id, 2, 3), " ", substring(stn_id , 4, 6))) %>%
#   ungroup() %>%
#   dplyr::select(year, station, samp_grp, cpue_cnt) %>%
#   right_join(survey_grid, by = "station") -> cpue_grid
# 
# 
# usa %>%
#   st_as_sf() %>%
#   ggplot()+
#   geom_sf(fill = "grey 60")+
#   coord_sf(xlim = c(-153, -151), ylim = c(56.6, 58.1))+
#   geom_polygon(data = filter(cpue_grid, grepl("KNE", bed_code)), aes(x = long, y = lat, group = group, fill = cpue_cnt), alpha = 0.75, color = 1)+
#   scale_fill_gradientn(colours = heat.colors(10, rev = T), trans = "log10")+
#   theme(legend.position = "right")+
#   labs(x = "Long", y = "Lat", fill = "CPUE")-> x
# ggsave("./figures/fishery_independent/2020/kne_large_cpue_abund_map.png", plot = x,
#        height = 12, width = 6, units = "in")
# 
#   
# table of tows by bed ----

tows %>%
  count(year,district, bed_name) %>%
  right_join(bed_area, by = c("bed_name", "district")) %>%
  #mutate(bed_code = factor(bed_code, levels = bed_levels)) %>%
  arrange(bed_name) %>%
  dplyr::select(year, district, bed_name, n, n_stations, area_nm2) %>%
  filter(year == 2022) %>%
  # write output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2022/2022_strata.csv")

  
# calculate abundance and biomass ----

## point estimates (rnd wt biomass in lbs)
catch %>%
  filter(rcode == 74120) %>%
  left_join(bed_area, by = c("bed_code", "bed_name", "district")) %>%
  group_by(year, samp_grp, district, bed_code, bed_name) %>%
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
         biomass_ln_ci = paste0("[", round(biomass_log_l95, 0), ", ", round(biomass_log_u95, 0), "]")) %>%
  ungroup -> abundance_biomass

## print output table
abundance_biomass %>%
  dplyr::select(year, samp_grp, district, bed_name, cpue_abund, abundance, abund_ln_ci, cv_abund, cpue_biomass, biomass, 
                biomass_ln_ci, cv_biomass) %>%
  # rearrange rows
  arrange(year, samp_grp, bed_name) %>%
  # print output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2022/2022_abundance_rnd_biomass_by_bed.csv")

## plots of abundance and biomass for presentation
abundance_biomass %>%
  # create duplicate district column so one can be used for the plot function
  mutate(dist = district) %>%
  group_by(dist) %>%
  nest() %>%
  filter(dist %in% c("KAM", "KSH", "KNE")) %>%
  mutate(abund_plot_large = purrr::map(data, f_plot_abundance, group = 1, years = 2016:2022),
         abund_plot_small = purrr::map(data, f_plot_abundance, group = 2, years = 2016:2022),
         biomass_plot_large = purrr::map(data, f_plot_biomass, group = 1, years = 2016:2022),
         biomass_plot_small = purrr::map(data, f_plot_biomass, group = 2, years = 2016:2022)) -> ab_plots
## save plots
### large scallop abundance
ggsave("./figures/fishery_independent/2022/dredge_survey_abundance_large.png", 
       plot = f_stack_plots(ab_plots  %>% ungroup %>% dplyr::slice(2, 1, 3), "abund_plot_large"),
       width = 5, height = 7, units = "in")
### small scallop abundance
ggsave("./figures/fishery_independent/2022/dredge_survey_abundance_small.png", 
       plot = f_stack_plots(ab_plots  %>% ungroup %>% dplyr::slice(2, 1, 3), "abund_plot_small"),
       width = 5, height = 7, units = "in")
### large scallop biomass
ggsave("./figures/fishery_independent/2022/dredge_survey_biomass_large.png", 
       plot = f_stack_plots(ab_plots  %>% ungroup %>% dplyr::slice(2, 1, 3), "biomass_plot_large"),
       width = 5, height = 7, units = "in")
### small scallop biomass
ggsave("./figures/fishery_independent/2022/dredge_survey_biomass_small.png", 
       plot = f_stack_plots(ab_plots  %>% ungroup %>% dplyr::slice(2, 1, 3), "biomass_plot_small"),
       width = 5, height = 7, units = "in")

# meat weight biomass two stage ----

### number of scallops by bed
catch %>%
  filter(rcode == 74120,
         samp_grp == 1) %>%
  group_by(year, district, bed_name, samp_grp, tow) %>%
  summarise(c = sum(samp_cnt, na.rm = T)) %>%
  ungroup() -> c

### number of tows by bed
tows %>%
  group_by(year, district, bed_name) %>%
  summarise(n_tow = n()) %>%
  ungroup() -> n_tows

### design based estimator
shaw %>%
   left_join(tows %>%
                dplyr::select(tow, district, bed_name),
             by = c("tow")) %>%
  rename(bed_name = bed_name.y,
         district = district.y) %>%

  filter(samp_grp == 1) %>%
  # calculate estimate sample variance of meat weight by tow, within bed
  group_by(year, district, bed_name, tow) %>%
  summarise(m = n(),
            mw_bar = mean(meat_wt, na.rm = T), 
            var_mw_bar = var(meat_wt, na.rm = T) / m,
            .groups = "drop") %>%
  
  right_join(c, by = c("year", "district", "bed_name", "tow")) %>%
  # compute mw_hat and mw cpue
  left_join(tows, by = c("year", "district", "bed_name", "tow")) %>%
  mutate(mw_hat = c * mw_bar,
         u_i = mw_hat / (area_swept * 0.83)) %>%
  # change na's from hauls that had zero catch to 0
  replace_na(list(m = 0,
                  mw_hat = 0,
                  u_i = 0)) %>%
  # compute total meat weight biomass
  left_join(n_tows, by = c("year", "district", "bed_name")) %>%
  left_join(bed_area, by = c("district", "bed_name")) %>%
  group_by(year, district, bed_name) %>%
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
  arrange(year, district, bed_name) %>%
  ungroup -> mw_biomass

mw_biomass %>%
  ## print output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2022/2022_mw_biomass_by_bed.csv")

## plots of meat weight biomass
mw_biomass %>%
  left_join(strata %>%
              dplyr::select(bed_name, district), 
            by = c("district", "bed_name")) %>%
  # create duplicate district column so one can be used for the plot function
  mutate(dist = district) %>%
  group_by(dist) %>%
  nest() %>%
  filter(dist %in% c("KAM", "KSH", "KNE")) %>%
  mutate(mt_biomass_plot = purrr::map(data, f_plot_mt_biomass, years = 2016:2022)) %>%
  ungroup %>%
  dplyr::slice(2, 1, 3) %>%
  f_stack_plots(., "mt_biomass_plot") -> x
ggsave("./figures/fishery_independent/2022/dredge_survey_mw_biomass.png", plot = x,
       width = 5, height = 7, units = "in")
  



# meat weight biomass calculated mw ----

shaw %>%
  # compute annual mwsh pars
  group_by(year, district) %>%
  nest() %>%
  mutate(pars = purrr::map(data, f_coef_mwsh)) %>%
  unnest(pars) %>%
  dplyr::select(-data) %>%
  # join to shad data
  right_join(shad) %>%
  # compute calculated wt in lbs
  mutate(calc_mw = alpha * shell_height^beta * sample_factor * 0.00220462) %>%
  # follow as single stage estimate
  group_by(year, district, bed_name, tow) %>%
  summarise(mw = sum(calc_mw)) %>%
  right_join(tows) %>%
  replace_na(list(mw = 0)) %>%
  mutate(mw_cpue = mw / (area_swept * 0.83)) %>%
  left_join(bed_area) %>%
  group_by(year, district, bed_name) %>%
  summarise(mw_biomass = mean(mw_cpue) * unique(area_nm2),
            var_mw_biomass = (var(mw_cpue) / n()) * unique(area_nm2)^2,
            cv = sqrt(var_mw_biomass) / mw_biomass,
            ln_l95 = mw_biomass * exp(-1.96 * sqrt(log(1 + cv^2))),
            ln_u95 = mw_biomass * exp(1.96 * sqrt(log(1 + cv^2))),
            ln_ci = paste0("[", round(ln_l95, 0), ", ", round(ln_u95, 0), "]")) -> mw_biomass_calc

mw_biomass_calc %>%
  ## print output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2022/calculated_mw_biomass_by_bed.csv")

  

# shell height distribution ----

shad %>%
  filter(tow %in% tows$tow) %>%
  mutate(dist = district) %>%
  group_by(dist) %>%
  nest() %>% 
  filter(dist %in% c("KAM", "KSH", "KNE")) %>%
  mutate(sh_plot = purrr::map(data, f_plot_sh_comp, by = "bed", stat = "pmf")) -> sh_plots
ggsave("./figures/fishery_independent/2022/KSH_dredge_survey_size_comp.png", 
       plot = sh_plots$sh_plot[[1]], width = 5, height = 5, units = "in")
ggsave("./figures/fishery_independent/2022/KNE_dredge_survey_size_comp.png", 
       plot = sh_plots$sh_plot[[2]], width = 5, height = 5, units = "in")
ggsave("./figures/fishery_independent/2022/KAM_dredge_survey_size_comp.png", 
       plot = sh_plots$sh_plot[[3]], width = 5, height = 6, units = "in")

shad %>%
  filter(tow %in% tows$tow) %>%
  mutate(dist = district) %>%
  group_by(dist) %>%
  nest() %>% 
  filter(dist %in% c("KAM", "KSH", "KNE")) %>%
  mutate(sh_plot = purrr::map(data, f_plot_sh_comp, by = "district", stat = "freq")) -> sh_plots
ggsave("./figures/fishery_independent/2022/KSH_dredge_survey_size_comp_count.png", 
       plot = sh_plots$sh_plot[[1]], width = 5, height = 5, units = "in")
ggsave("./figures/fishery_independent/2022/KNE_dredge_survey_size_comp_count.png", 
       plot = sh_plots$sh_plot[[2]], width = 5, height = 5, units = "in")
ggsave("./figures/fishery_independent/2022/KAM_dredge_survey_size_comp_count.png", 
       plot = sh_plots$sh_plot[[3]], width = 5, height = 6, units = "in")

# meat weight ~ shell height ----

shaw  %>%
  left_join(tows %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(tow %in% tows$tow,
         !is.na(meat_wt), 
         !is.na(shell_height),
         shell_height > 50) %>%
  group_by(district) %>%
  nest %>%
  filter(district %in% c("KAM", "KSH", "KNE")) %>%
  mutate(lm = purrr::map(data, function(x){lm(log(meat_wt) ~ log(shell_height) +  factor(year), data = x)}),
         fit = purrr::map(lm, function(x){as.data.frame(predict(x, interval = "confidence", level = 0.95))})) %>%
  dplyr::select(-lm) %>%
  unnest(c(data, fit))  %>%
  ungroup() %>%
  mutate_at(c("fit", "upr", "lwr"), exp) %>%
  # join to strata to get full district name for plot
  left_join(strata %>%
              distinct(district, district_full),
            by = "district") %>%
  #mutate(district_full = factor(district_full, levels = c("West Kayak Island", "East Kayak Island", "Yakutat"))) %>%
  
  # plot
  ggplot()+
  geom_point(aes(x = shell_height, y = meat_wt, color = factor(year)), alpha = 0.1)+
  geom_line(aes(x = shell_height, y = fit, group = factor(year)))+
  geom_ribbon(aes(x = shell_height, ymin = lwr, ymax = upr, fill = factor(year)), alpha = 0.5)+
  facet_wrap(~district_full)+
  scale_color_manual(values = cb_palette[1:6])+
  scale_fill_manual(values = cb_palette[1:6])+
  labs(x = "Shell Height (mm)", y = "Meat Weight (g)", fill = NULL, color = NULL) -> x

ggsave("./figures/fishery_independent/2022/dredge_survey_mw_sh.png", plot = x, 
       height = 4, width = 8, units = "in")


# meat weight ~ round weight ----
shaw %>%
  left_join(tows %>%
              dplyr::select(tow, bed_code),
            by = "tow") %>%
  rename(bed_code = bed_code.y) %>%
  filter(tow %in% tows$tow,
         bed_code %in% bed_levels,
         !is.na(meat_wt)) %>%
  # join to strata to get full district name for plot
  left_join(strata %>%
              distinct(district, district_full),
            by = "district") %>%
  #mutate(district_full = factor(district_full, levels = c("West Kayak Island", "East Kayak Island", "Yakutat"))) %>%
  ggplot()+
  geom_point(aes(x = whole_wt, y = meat_wt, color = factor(year)), alpha = 0.1)+
  geom_smooth(aes(x = whole_wt, y = meat_wt, color = factor(year)), method = "gam", se = F)+
  geom_line(aes(x = whole_wt, y = whole_wt * 0.1), linetype = 2)+
  scale_color_manual(values = cb_palette[1:5])+
  facet_wrap(~district_full, scales = "free", nrow = 1)+
  labs(x = "Round Weight (g)", y = "Meat Weight (g)", color = NULL) -> x

ggsave("./figures/fishery_independent/2022/dredge_survey_rwt_mwt.png", plot = x,
       width = 8, height = 4, units = "in")

# pathologies ----

## weak meats
shaw %>% 
  left_join(tows %>%
              dplyr::select(tow, bed_name),
            by = "tow") %>%
  rename(bed_name = bed_name.y) %>%
  filter(tow %in% tows$tow) %>%
  group_by(year, district, bed_name) %>%
  summarise(n_shucked = n(),
            wk_mts = sum(meat_condition, na.rm = T) / n() * 100) %>%
  ungroup() -> wk_mt
write_csv(wk_mt, "./output/fishery_independent/statewide_scallop_survey/2022/weak_mts.csv")
wk_mt %>% 
  # create duplicate district to nest by for plot function
  mutate(dist = district) %>%
  group_by(dist) %>%
  nest %>%
  # filter districts surveyed this year
  filter(dist %in% c("KAM", "KSH", "KNE")) %>%
  mutate(plot = purrr::map(data, f_plot_wkmt, years = 2016:2022)) %>%
  ungroup %>%
  dplyr::slice(2, 1, 3) %>%
  f_stack_plots(., "plot") -> x
ggsave("./figures/fishery_independent/2022/dredge_survey_wk_mts.png", plot = x,
       width = 5, height = 7, units = "in")

## mud blisters
shaw %>% 
  left_join(tows %>%
              dplyr::select(tow, bed_name),
            by = "tow") %>%
  rename(bed_name = bed_name.y) %>%
  mutate(district = ifelse(grepl("KNE", bed_name), "KNE", district)) %>%
  filter(tow %in% tows$tow) %>%
  filter(!is.na(mud_blister)) %>%
  group_by(year, bed_name, district, samp_grp) %>%
  mutate(n_shucked = n()) %>%
  group_by(year, bed_name, district, mud_blister, samp_grp) %>%
  summarise(perc = n() / mean(n_shucked) * 100) %>% ungroup %>%
  mutate(mud_blister = paste0("mb", mud_blister)) %>%
  pivot_wider(names_from = mud_blister, values_from = perc) %>%
  replace_na(list(mb0 = 0, mb1 = 0, mb2 = 0, mb3 = 0)) %>%
  mutate(mb4 = 0) -> mud_blister
write_csv(mud_blister, "./output/fishery_independent/statewide_scallop_survey/2022/mud_blister.csv")

# plot
mud_blister %>%
  left_join(strata %>% distinct(district, district_full, bed_name), 
            by = c("bed_name", "district")) %>%
  filter(samp_grp == 1) %>%
  pivot_longer(c(mb0, mb1, mb2, mb3, mb4), names_to = "code", values_to = "percent") %>%
  mutate(code = case_when(code == "mb0" ~ "0%",
                          code == "mb1" ~ "25%",
                          code == "mb2" ~ "50%",
                          code == "mb3" ~ "75%",
                          code == "mb4" ~ "100%"),
         code = factor(code, levels = c("0%", "25%", "50%", "75%", "100%"))) %>%
  filter(district %in% c("KAM", "KSH", "KNE")) %>%
  ggplot()+
  geom_bar(aes(x = factor(year), y = percent, fill = code), 
           stat = "identity", position = "stack")+
  scale_fill_manual(values = cb_palette[2:6])+
  facet_wrap(district_full~bed_name)+
  labs(x = NULL, y = "Percent Mud Blisters", fill = NULL) -> x
    
ggsave("./figures/fishery_independent/2022/dredge_survey_mud_blister.png", plot = x,
       width = 8, height = 7, units = "in")    


## shell worms
shaw %>% 
  left_join(tows %>%
              dplyr::select(tow, bed_name),
            by = "tow") %>%
  rename(bed_name = bed_name.y) %>%
  mutate(district = ifelse(grepl("KNE", bed_name), "KNE", district)) %>%
  filter(tow %in% tows$tow) %>%
  filter(!is.na(shell_worm)) %>%
  group_by(year, bed_name, district, samp_grp) %>%
  mutate(n_shucked = n()) %>%
  group_by(year, bed_name, samp_grp, district, shell_worm) %>%
  summarise(perc = n() / mean(n_shucked) * 100) %>%
  mutate(shell_worm = paste0("sw", shell_worm)) %>%
  pivot_wider(names_from = shell_worm, values_from = perc) %>%
  replace_na(list(sw0 = 0, sw1 = 0, sw2 = 0, sw3 = 0)) %>%
  mutate(sw4 = 0) -> shell_worm
write_csv(shell_worm, "./output/fishery_independent/statewide_scallop_survey/2022/shell_worm.csv")

# plot
shell_worm %>%
  left_join(strata %>% distinct(district, district_full, bed_name), 
            by = c("bed_name", "district")) %>%
  filter(samp_grp == 1) %>%
  pivot_longer(c(sw0, sw1, sw2, sw3, sw4), names_to = "code", values_to = "percent") %>%
  mutate(code = case_when(code == "sw0" ~ "0%",
                          code == "sw1" ~ "25%",
                          code == "sw2" ~ "50%",
                          code == "sw3" ~ "75%",
                          code == "sw4" ~ "100%"),
         code = factor(code, levels = c("0%", "25%", "50%", "75%", "100%"))) %>%
  filter(district %in% c("WKI", "EKI", "YAK")) %>%
  ggplot()+
  geom_bar(aes(x = factor(year), y = percent, fill = code), 
           stat = "identity", position = "stack")+
  scale_fill_manual(values = cb_palette[2:6])+
  facet_wrap(district_full~bed_name)+
  labs(x = NULL, y = "Percent Shell Worm", fill = NULL) -> x

ggsave("./figures/fishery_independent/2022/dredge_survey_shell_worm.png", plot = x,
       width = 8, height = 7, units = "in")  

# gonad condition ----

shaw %>% 
  left_join(tows %>%
              dplyr::select(tow, bed_name),
            by = "tow") %>%
  rename(bed_name = bed_name.y) %>%
  filter(tow %in% tows$tow) %>%
  filter(!is.na(gonad),
         samp_grp == 1) %>%
  group_by(year, bed_name, district) %>%
  mutate(n_shucked = n()) %>%
  group_by(year, bed_name, district, samp_grp, gonad) %>%
  summarise(perc = n() / mean(n_shucked) * 100) %>%
  mutate(gonad = paste0("g", gonad)) %>%
  pivot_wider(names_from = gonad, values_from = perc) %>%
  replace_na(list(g0 = 0, g2 = 0, g3 = 0, g4 = 0)) %>%
  mutate(g1 = 0,
         g5 = 0) %>%
  # reorder columns
  dplyr::select(year, bed_name, samp_grp, g0, g1, g2, g3, g4, g5) %>%
  # rename data codes
  rename_at(4:9, ~tolower(c("immature", "empty", "initial_recovery", "filling", "full", "cannot_determine"))) %>%
  # write output table
  write_csv("./output/fishery_independent/statewide_scallop_survey/2022/gonad_condition.csv")

# gonad at size
shaw %>% 
  left_join(tows %>%
              dplyr::select(tow, bed_name),
            by = "tow") %>%
  rename(bed_name = bed_name.y) %>%
  filter(tow %in% tows$tow) %>%
  filter(!is.na(gonad),
         !is.na(shell_height)) %>%
  dplyr::select(year, district, gonad, shell_height) %>%
  # 10 mm size bin
  mutate(size_bin = round(shell_height / 10, 0) * 10) %>%
  count(district, year, size_bin, gonad) %>%
  group_by(district, year, size_bin) %>%
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
  # filter for districts surveyed
  filter(district %in% c("KAM", "KSH", "KNE")) %>%
  
  ggplot()+
  geom_bar(aes(x = size_bin, y = prop, fill = gonad), position = "stack", stat = "identity")+
  facet_grid(year~district, scales = "free")+
  scale_fill_manual(values = cb_palette[2:7])+
  labs(x = "Size Bin (10 mm)", y = "Proportion", fill = NULL) -> x
  
ggsave("./figures/fishery_independent/2022/gonad_at_size.png", plot = x, 
       height = 10, width = 12, units = "in")





# # weight at size ----
# 
# f_wl <- function(data) {
#   sh = data$shell_height/10
#   wt = data$whole_wt/1000
#   
#   fit = lm(log(wt) ~ log(sh))
#   coef = coef(fit)
#   #coef[1] = exp(coef[1] + sqrt(diag(vcov(fit)))[1] / 2)
#   
#   return(coef)
# }
# 
# shaw %>%
#   group_by(district) %>%
#   nest() %>%
#   mutate(par = purrr::map(data, f_wl)) -> wl_fit
# 
# ## extract KSH
# wl_fit %>%
#   #filter(district == "KSH") %>%
#   dplyr::select(district, par) %>%
#   unnest(par)
# 
# # plot for 2022 SPT presentation on SS models
# shaw %>%
#   filter(district == "KSH") %>%
#   ggplot()+
#   geom_point(aes(x = shell_height/10, y = whole_wt/1000), color = "lightgrey", alpha = 0.2)+
#   # ksh fit
#   geom_line(aes(x = shell_height/10, y = 0.000148*(shell_height/10)^2.79))+
#   # kam fit 
#   geom_line(aes(x = shell_height/10, y = 0.000143*(shell_height/10)^2.873), color = "red")+
#   labs(x = "Shell Height(cm)", y = "Round Weight (kg)") -> x
# 
# ggsave("./figures/fishery_independent/2021/2021_wt_at_sh.png", plot = x, 
#        height = 3, width = 5, units = "in")
# 
# 
# 
# 
# 
# 
# 
