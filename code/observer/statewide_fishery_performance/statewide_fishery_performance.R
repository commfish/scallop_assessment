# notes ----
## update on statewide fishery performance
## Tyler Jackson
## 1/22/2021

# load ----
library(scales)
library(FNGr)
library(here)

## libraries and custom functions
source(here("code/misc", "general_observer_data_functions.R"))
source(here("code/misc", "adfg_map_functions.R"))

## global options
### custom color/fill pallete
cb_palette <- c("#999999", "tomato4", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "mediumpurple")

### set theme (from FNGr)
theme_set(theme_sleek() + theme(legend.position = "bottom"))
### cutsom axis ticks for yrs (from FNGr)

dist_levels <- c("KNE", "KSH", "KSW", "KSE", "M", "O", "Q", "H", "E", "D")
minor_ticks <- scale_x_continuous(breaks = tickr(tibble(yr = 1999:2020), yr, 5)$breaks, 
                                  labels = tickr(tibble(yr = 1999:2020), yr, 5)$labels,
                                  limits = c(1999, 2020.5))


# data ----
## fish ticket data
## raw tickets 1985 - 2018
tickets_raw <- read_csv("./data/observer/fish_tickets/scallop_ft_raw_1985_2018.csv")

## 2020/21 ghls
tibble(district = c("KNE", "KSH", "KSW", "KSE", "M", "O", "Q", "H", "D"),
       ghl = c(15000, 40000, 35000, 15000, 15000, 5000, 7500, 0, 145000),
       season = "2020/21") %>%
  left_join(tibble(year = 2020,
                   district = c("KSH", "KNE", "KSW", "D"),
                   ret_lbs_mt = c(40060, 15095, 25950, 145025),
                   dredge_hrs = c(433, 189, 589, 3092),
                   mt_cpue = c(93, 80, 44, 47))) -> stats_2020

files <- grep("fishery_table", 
              list.files("./data/observer/old_catch", full.names = T), 
              value = T)
purrr::map(files, read_csv) %>%
  purrr::map(., mutate_at, .vars = 2, .funs = as.character) %>%
  do.call("rbind", .) %>%
  bind_rows(stats_2020) %>%
  mutate(year = as.numeric(substring(season, 1, 4)),
         district = factor(district, levels = dist_levels)) -> fish_stats

## observer/logbook data
### scallop haul data 2009/10 - Present
catch <- do.call(bind_rows,
                 lapply(paste0("data/observer/catch/", list.files("data/observer/catch/")), read_csv))
### bycatch by day 2009/10 - Present
bycatch <- do.call(bind_rows,
                   lapply(paste0("data/observer/bycatch/", list.files("data/observer/bycatch/")), read_csv))

## biological reference points
tibble(year = 1993:2020,
       OFL = c(rep(1800000, 5), rep(1240000, 13), rep(1284000, 10)),
       ABC = OFL * 0.9) %>%
  pivot_longer(c(OFL, ABC), names_to = "ref", values_to = "mt_wt") -> refs



# data mgmt ----

## pre-observer 1993 - 2008 data retained catch
tickets_raw %>%
  rename_all(tolower) %>%
  dplyr::select(batch_year, scallop_mgt_area, scallop_mgt_district, statarea, landed_weight) %>%
  rename_all(~c("year", "reg_area", "district", "statarea", "meat_weight")) %>%
  mutate(district = case_when(district == "AKPEN" ~ "M",
                              district == "BSEA" ~ "Q",
                              district == "DHRBOR" ~ "O",
                              district == "KAMAK" ~ "H",
                              district == "CIOUT" ~ "H",
                              district == "NEKOD" ~ "KNE",
                              district == "PWSE" ~ "E",
                              district == "SEMID" ~ "KSEM",
                              district == "SHELK" ~ "KSH",
                              district == "Southeast Kodiak" ~ "KSE",
                              district == "SWKOD" ~ "KSW",
                              district == "YAKT" ~ "D",
                              reg_area == "R" ~ "R",
                              is.na(district) & statarea %in% c(525701, 525807, 505832)  ~ "KNE", 
                              is.na(district) & statarea %in% c(575631, 565631) ~ "KSEM",
                              is.na(district) & statarea %in% c(555600, 555630, 565702) ~ "KSW")) %>%
  group_by(year, reg_area, district) %>%
  summarise(meat_weight = sum(meat_weight)) %>%
  filter(!is.na(district),
         year %in% 1993:2008) -> ret_catch_1993_2008

## remove phantom rows in catch tibble (can't figure out where they come from...)
catch %>%
  drop_na(Haul_ID) -> catch

## rename fields in current data (2009 - present)
f_catch_rename(catch) %>%
  ## add Season to data
  f_add_season() %>%
  ## classify Karluk bed as KSW district instead of KSH
  f_revise_district() %>% 
  ## coerce date to date class
  mutate(Set_date = lubridate::mdy(Set_date)) %>%
  ## remove tows with zero dredge hours (logbook mistake)
  filter(dredge_hrs != 0) %>%
  ## fix issue with missing basket weight in 2018/19
  mutate(round_weight = ifelse(Season == "2018/19" & District == "O",
                               54.1 * rtnd_basket, round_weight)) -> catch

## rename fields in bycatch data (2009 - present)
f_bycatch_rename(bycatch) %>%
  ## add Season to data
  f_add_season() %>%
  ## revise district (replace with f_revise district after beds are added to data)
  mutate(District = ifelse(District %in% c("D", "YAK", "D16"), "YAK", District)) %>%
  ## coerce date to date class
  mutate(Set_date = lubridate::mdy(Set_date)) %>%
  ## fix issue with missing basket weight in 2018/19
  mutate(est_rnd_wt = ifelse(Season == "2018/19" & District == "O",
                             catch %>%
                               filter(Season == "2018/19" & District == "O") %>%
                               pull(round_weight) %>%
                               sum,
                             est_rnd_wt)) -> bycatch


# map of districts ----

f_shp_prep("./data/maps/mgmt_units", "Scallop_Statewide_Areas_PY") %>%
  .[[1]] -> area_bounds

f_shp_prep("./data/maps/mgmt_units", "AreasKM_ScallopDistrictBoundaries", fortify = F) %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84 +no_defs")) -> district_bounds


ggplot()+
  geom_polygon(data = area_bounds, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA)+
  geom_line(data = district_bounds, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA)+
  geom_line(data = line, aes(lon, lat, group = group), linetype = 2)+
  geom_polygon(data = canam, aes(x = long, y = lat, group = group), 
               color = NA, fill = "grey90")+
  coord_quickmap(ylim = c(49, 62), xlim = c(-173, -140))+
  geom_vline(xintercept = c(-161:-160))+
  theme(panel.background = element_rect(fill = "grey70")) -> x
ggsave("./figures/statewide_fishery/boundaries_map.png", plot = x, width = 8, height = 6, units = "in")


# fish stats for safe ----

## total landed catch and GHL
fish_stats %>%
  group_by(season) %>%
  summarise(landed_catch = sum(ret_lbs_mt, na.rm = T),
            landed_catch_t = landed_catch * 0.000453592,
            tot_dredge_hr = sum(dredge_hrs,na.rm = T),
            tot_ghl = sum(ghl, na.rm = T),
            percent_ghl = landed_catch / tot_ghl * 100) %>%
  mutate(cum_cpue = landed_catch / tot_dredge_hr) %>%
  print(., n = 100)
  
## total catch and OFL

# compute discards function
f_disc_nh <- function(nh, data){
  if(nh == F){
    data %>%
      summarise(effort = sum(dredge_hrs),
                discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
                discard_lb = discard_rate_lb * effort,
                disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
                discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
                discard_num = discard_rate_num * effort)
  } else{
    data %>%
      summarise(effort = sum(dredge_hrs),
                discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
                discard_lb = discard_rate_lb * effort,
                disc_per_lb = sum(disc_count) / sum(disc_wt),
                discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
                discard_num = discard_rate_num * effort)
  }
}

bycatch %>%
  # add dummy variable for incorrectly collected data
  mutate(nh = (Set_date >= "2018-07-14" & Set_date <= "2018-07-27" & District == "KSH") | (Set_date >= "2018-07-01" & Set_date <= "2018-07-07" & District == "YAK")) %>%
  # computet discards with or without nh sampling error
  group_by(Season, District, nh) %>%
  nest()  %>%
  mutate(summary = purrr::map2(nh, data, f_disc_nh)) %>%
  ungroup() %>%
  dplyr::select(-data, -nh) %>%
  unnest(summary) %>%
  # summarise by season
  group_by(Season) %>%
  summarise(discard_mw = sum(discard_lb, na.rm = T) * 0.1) %>%
  # join to retained catch
  left_join(catch %>%
              group_by(Season) %>%
              summarise(ret_mw = sum(meat_weight, na.rm = T))) %>%
  # join to reference points
  mutate(year = as.numeric(substring(Season, 1, 4))) %>%
  left_join(pivot_wider(refs, names_from = "ref", values_from = "mt_wt")) %>%
  # add total catch and percent OFL
  mutate(total_catch = discard_mw + ret_mw,
         total_catch_T = total_catch * 0.000453592,
         percent_ofl = total_catch / OFL * 100)


bycatch %>%
  # add dummy variable for incorrectly collected data
  mutate(nh = (Set_date >= "2018-07-14" & Set_date <= "2018-07-27" & District == "KSH") | (Set_date >= "2018-07-01" & Set_date <= "2018-07-07" & District == "YAK")) %>%
  # computet discards with or without nh sampling error
  group_by(Season, District, nh) %>%
  nest()  %>%
  mutate(summary = purrr::map2(nh, data, f_disc_nh)) %>%
  ungroup() %>%
  dplyr::select(-data, -nh) %>%
  unnest(summary) %>%
  mutate(discard_mort_est = discard_lb * 0.1 * 0.2) %>%
  filter(Season == "2019/20") %>%
  print(n = 1000)



# ghl plot ----
fish_stats %>%
  filter(year >= 2000) %>%
  ggplot()+
  geom_col(aes(x = year, y = ghl, fill = district), position = "stack")+

  scale_y_continuous(labels = comma)+
  minor_ticks+
  scale_fill_manual(values = cb_palette)+
  labs(x = NULL, y = "GHL (lbs meat)", fill = NULL) -> x
ggsave("./figures/statewide_fishery/ghl_stacked_bar.png", plot = x, width = 6, height = 4, units = "in")

# retained meat with ghl plot ----
fish_stats %>%
  filter(year >= 2000) %>%
  group_by(year) %>%
  summarise(ghl = sum(ghl, na.rm = T)) -> tot_ghl

fish_stats %>%
  filter(year >= 2000) %>%
  ggplot()+
  geom_col(aes(x = year, y = ret_lbs_mt, fill = district), position = "stack")+
  geom_point(data = tot_ghl, aes(x = year, y = ghl))+
  geom_line(data = tot_ghl, aes(x = year, y = ghl))+
  scale_y_continuous(labels = comma, sec.axis = sec_axis(~., name = "GHL (lbs meat)", labels = comma))+
  minor_ticks+
  scale_fill_manual(values = cb_palette)+
  labs(x = NULL, y = "Retained Meats (lbs)", fill = NULL) -> x
ggsave("./figures/statewide_fishery/retained_mt_w_ghl.png", plot = x, width = 6, height = 4, units = "in")



# retained meat prop of ofl ----



fish_stats %>%
  filter(year >= 2000) %>%
  mutate(district = factor(district, levels = dist_levels)) %>%
  ggplot()+
  geom_col(aes(x = year, y = ret_lbs_mt, fill = district), position = "stack")+
  geom_point(data = refs, aes(x = year, y = mt_wt, color = ref))+
  geom_line(data = refs, aes(x = year, y = mt_wt, color = ref))+
  scale_y_continuous(labels = comma, sec.axis = sec_axis(~., name = "OFL / ABC (lbs meat)", labels = comma))+
  scale_x_continuous(breaks = tickr(tibble(yr = 2000:2020), yr, 5)$breaks, 
                     labels = tickr(tibble(yr =  2000:2020), yr, 5)$labels,
                     limits = c(1999.5, 2020.5))+
  scale_fill_manual(values = cb_palette)+
  scale_color_manual(values = c("blue", "red"))+
  labs(x = NULL, y = "Retained Meats (lbs)", fill = NULL, color = NULL) -> x
ggsave("./figures/statewide_fishery/ofl_stacked_bar.png", plot = x, width = 6, height = 4, units = "in")

fish_stats %>%
  filter(year >= 2000) %>%
  group_by(year) %>%
  summarise(ret_lbs_mt = sum(ret_lbs_mt, na.rm = T)) %>%
  print(n=100)
  left_join(filter(refs, ref == "ABC"), by = "year") %>%
  mutate(percent_abc = ret_lbs_mt / mt_wt) %>%
  ggplot()+
  geom_point(aes(x = year, y = percent_abc * 100))+
  geom_line(aes(x = year, y = percent_abc * 100))+
  minor_ticks+
  labs(x = NULL, y = "Percent of ABC Retained") -> x
ggsave("./figures/statewide_fishery/percent_abc_retained.png", plot = x, width = 6, height = 4, units = "in")


# discards ----
bycatch %>%
  # add dummy variable for incorrectly collected data
  mutate(nh = (Set_date >= "2018-07-14" & Set_date <= "2018-07-27" & District == "KSH") | (Set_date >= "2018-07-01" & Set_date <= "2018-07-07" & District == "YAK")) %>%
  # computet discards with or without nh sampling error
  group_by(Season, District, nh) %>%
  nest()  %>%
  mutate(summary = purrr::map2(nh, data, f_disc_nh)) %>%
  ungroup() %>%
  dplyr::select(-data, -nh) %>%
  unnest(summary) %>%
  rename_all(tolower) -> discards

## total meat ret and disc y season
catch %>%
  rename_all(tolower) %>%
  mutate(year = as.numeric(substring(season, 1, 4))) %>%
  group_by(year) %>%
  summarise(ret_mw = sum(meat_weight, na.rm = T)) %>%
  left_join(discards %>%
              group_by(year) %>%
              summarise(mw_disc = sum(discard_mw_lb, na.rm = T))) %>%
  rename(retained = ret_mw, 
         discarded = mw_disc) %>%
  pivot_longer(c(retained, discarded), names_to = "type", values_to = "lbs") %>%
  ggplot()+
  geom_bar(aes(x = year, y = lbs, fill = type), position = "stack", stat = "identity")+
  scale_x_continuous(breaks = tickr(tibble(yr = 2009:2020), yr, 5)$breaks, 
                     labels = tickr(tibble(yr = 2009:2020), yr, 5)$labels,
                     limits = c(2008, 2020))+
  scale_fill_manual(values = cb_palette[c(3, 4)])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Total Catch Estimate (lbs meat)", fill = NULL) -> x
ggsave("./figures/statewide_fishery/total_catch_estimate.png", plot = x, width = 6, height = 4, units = "in")


## discard rate 
discards %>%
  # join to retainted catch by season district
  left_join(catch %>%
              rename_all(tolower) %>%
              group_by(season, district) %>%
              summarise(ret_rnd_wt = sum(round_weight, na.rm = T))) %>%
  filter(ret_rnd_wt > 0) %>%
  
  
  ggplot()+
  geom_point(aes(x = season, y = discard_lb / ret_rnd_wt, color = district))+
  geom_smooth(aes(x = as.numeric(season), y = discard_lb / ret_rnd_wt), method = "gam", color = 1, se = F)+
  scale_color_manual(values = c(cb_palette, 1, 2))+
  labs(x = NULL, y = "Discard ratio (lb discarded / lb retained)", color = NULL) -> x
ggsave("./figures/statewide_fishery/discard_ratio.png", plot = x, width = 6, height = 4, units = "in")


## total fishery mortality estimate
catch %>%
  rename_all(tolower) %>%
  mutate(year = as.numeric(substring(season, 1, 4))) %>%
  group_by(year) %>%
  summarise(ret_mw = sum(meat_weight, na.rm = T)) %>%
  left_join(discards %>%
              group_by(year) %>%
              summarise(mw_disc = sum(discard_mw_lb, na.rm = T) * 0.2)) %>%
  mutate(tot_mortality = mw_disc + ret_mw) %>%
  left_join(filter(refs, ref == "ABC"), by = "year") %>%
  mutate(percent_abc = tot_mortality / mt_wt) %>%
  ggplot()+
  geom_point(aes(x = year, y = percent_abc * 100))+
  geom_line(aes(x = year, y = percent_abc * 100))+
  scale_x_continuous(breaks = tickr(tibble(yr = 2009:2020), yr, 5)$breaks, 
                     labels = tickr(tibble(yr = 2009:2020), yr, 5)$labels,
                     limits = c(2008, 2020))+
  labs(x = NULL, y = "Percent of ABC (Total Fishing Mortality)") -> x
ggsave("./figures/statewide_fishery/percent_abc_total_fishing_mortality.png", plot = x, width = 6, height = 4, units = "in")


# mt cpue plot ----
fish_stats %>%
  filter(year >= 2000) %>%
  group_by(district) %>%
  ggplot()+
  geom_point(aes(x = year, y = mt_cpue, color = district), alpha = 0.5)+
  geom_line(aes(x = year, y = mt_cpue, color = district, group = district), alpha = 0.5)+
  geom_smooth(aes(x = year, y = mt_cpue), se = F, color = 1)+
  minor_ticks+
  scale_color_manual(values = cb_palette)+
  labs(x = NULL, y = "Nominal CPUE (lbs meat / dredge hr)", color = NULL) -> x
ggsave("./figures/statewide_fishery/nominal_mw_cpue.png", plot = x, width = 6, height = 4, units = "in")

# extent ----
catch %>%
  rename_all(tolower) %>%
  mutate(year = as.numeric(substring(season, 1, 4))) %>%
  # adjust district to match the analysis
  mutate(district = gsub("EKI|WKI", "E", district),
         district = gsub("UB|C", "M", district),
         district = gsub("KAMN|KAMS", "H", district),
         district = gsub("YAK", "D", district)) %>%
  # remove tows with missing lat/lon
  filter(!is.na(set_lon), !is.na(set_lat),
         district %in% dist_levels) %>%
  mutate(district = factor(district, levels = dist_levels)) %>%
  group_by(year, district) %>%
  nest %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  group_by(district) %>%
  mutate(extent_scaled = scale(extent)[,1]) %>%
  dplyr::select(year, district, extent_scaled) %>%
  right_join(expand_grid(year = unique(2009:2019),
                         district = factor(dist_levels, levels = dist_levels)),
             by = c("year", "district")) %>%
  filter(district != "H") %>%
  ggplot()+
  geom_point(aes(x = year, y = extent_scaled, color = district), alpha = 0.5)+
  geom_line(aes(x = year, y = extent_scaled, color = district), alpha = 0.5)+
  geom_smooth(aes(x = year, y = extent_scaled), method = "gam", se = F, color = 1)+
  scale_x_continuous(breaks = tickr(tibble(yr = 2009:2020), yr, 5)$breaks, 
                     labels = tickr(tibble(yr = 2009:2020), yr, 5)$labels,
                     limits = c(2009, 2020))+
  scale_color_manual(values = cb_palette[-8])+
  labs(x = NULL, y = "Fishery Extent Index", color = NULL) -> x
ggsave("./figures/statewide_fishery/statewide_catch_extent.png", plot = x, width = 6, height = 4, units = "in")

# KSH example
catch %>%
  rename_all(tolower) %>%
  filter(district == "KSH", 
         season %in% c("2013/14", "2019/20")) %>%
  ggplot()+
  geom_polygon(data = canam, aes(x = long, y = lat, group = group), fill = "grey70")+
  geom_point(aes(x = set_lon, y = set_lat), color = cb_palette[4], alpha = 0.5)+
  facet_wrap(~season)+
  coord_quickmap(xlim = c(-154.5, -152.9), ylim = c(58, 59)) -> x
ggsave("./figures/statewide_fishery/statewide_catch_extent_example.png", plot = x, width = 8, height = 4, units = "in")

catch %>%
  rename_all(tolower) %>%
  filter(district == "KSH") %>%
  group_by(season) %>%
  nest() %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  ungroup() %>%
  mutate(extent_scaled = scale(extent)[,1]) %>%
  mutate(year = as.numeric(substring(season, 1, 4))) %>%
  ggplot()+
  geom_point(aes(x = year, y = extent_scaled))+
  geom_line(aes(x = year, y = extent_scaled))+
  scale_x_continuous(breaks = tickr(tibble(yr = 2009:2020), yr, 5)$breaks,
                     limits = c(2009, 2020))+
  labs(x = NULL, y = "Fishery Extent Index") -> x
ggsave("./figures/statewide_fishery/statewide_catch_extent_ksh_ts.png", plot = x, width = 6, height = 3, units = "in")

         




