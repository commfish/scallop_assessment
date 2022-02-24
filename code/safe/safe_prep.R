# notes -----
## relevant high level summarization for SAFE documents
## Tyler Jackson
## tyler.jackson@alaska.gov

# load ----
library(tidyverse)
library(FNGr)
library(patchwork)
theme_set(theme_sleek())

### general observer data functions
source("./code/misc/general_observer_data_functions.R")

# data and data mgmt ----
### scallop haul data 2009/10 - Present
catch <- do.call(bind_rows,
                 lapply(paste0("data/observer/catch/", list.files("data/observer/catch/")), read_csv))
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
  filter(dredge_hrs != 0)  %>%
  ## coerce date to date class
  ## fix issue with missing basket weight in 2018/19
  mutate(round_weight = ifelse(Season == "2018/19" & District == "O",
                               54.1 * rtnd_basket, round_weight)) -> catch

# statewide total removals ----

## data
purrr::map(list.files("./output/observer/2021", "fish_stats", full.names = T), read_csv) %>%
  do.call("rbind", .) %>%
  filter(Season == "2020/21") %>%
  summarise_at(2:5, sum) -> tot_retained
purrr::map(list.files("./output/observer/2021", "discard_summary", full.names = T), read_csv) %>%
  do.call("rbind", .) %>%
  filter(Season == "2020/21") %>%
  summarise_at(c(3:4, 8:9), sum) -> tot_discarded
table_2.1 <- read_csv("./output/observer/2021/total_removals_table.csv")

## total meat removal (Table 2-1)
### in lbs
tot_retained$mt_wt + (tot_discarded$discard_M_lbs*0.1)
### in t
(tot_retained$mt_wt + (tot_discarded$discard_M_lbs*0.1)) * 0.000453592

table_2.1 %>%
  add_row(season = "2020/21",
          tot_removal = tot_retained$mt_wt + (tot_discarded$discard_M_lbs*0.1),
          ofl = 1284000,
          abc = 1156000) %>%
  mutate(percent_ofl = tot_removal / ofl,
         percent_abc = tot_removal / abc) -> table_2.1
write_csv(table_2.1, "./output/observer/2021/total_removals_table.csv")

## plot (Figure 2-1)
table_2.1 %>%
  ggplot()+
  geom_bar(aes(x = season, y = tot_removal/1e6), stat = "identity", fill = "black", color = NA, width = 0.75)+
  geom_line(aes(x = season, y = ofl/1e6, group = ofl), size = 1)+
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 2, 0.2), limits = c(0, 2.1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25))+
  labs(x = NULL, y = "Total Removals (million lb)") -> x
ggsave("./figures/safe/2022/total_removals.png", plot = x, height = 4, width = 6, units = "in")  

# crab bycatch ----

## data
tibble(District = unlist(str_extract_all(list.files("./output/observer/2021", "bycatch"), "[A-Z]+")),
       data = purrr::map(list.files("./output/observer/2021", "bycatch", full.names = T), read_csv)) %>%
  unnest(data) -> bycatch

## king crab totals by district
bycatch %>%
  filter(Season== "2020/21") %>%
  dplyr::select(District, grep("king", names(.)))

## tanner, snow, and dungeness crab
bycatch %>%
  filter(Season== "2020/21") %>%
  dplyr::select(District, grep("tanner|snow|dungeness", names(.))) -> tanner


## eatimate weight of Tanner crab (size comp calcs from observer data report analysis)

### estimate average weight
crab_size <- do.call(bind_rows,
                     lapply(paste0("data/observer/crab_size/", list.files("data/observer/crab_size/")), read_csv))

  ## reason fields in crab size data (2009 - present)
f_crab_size_rename(crab_size) %>%
  ## drp the empty column
  dplyr::select(-7) %>%
  ## add Season to data
  f_add_season() %>%
  ## revise district
  mutate(District = ifelse(District %in% c("D", "YAK", "D16"), "YAK", District)) %>%
  ## add Species and Sex
  mutate(Species = case_when(RACE_code == 68560 ~ "Tanner crab",
                             RACE_code == 68541 ~ "snow crab"),
         Species = factor(Species, levels = c("Tanner crab", "snow crab")),
         Sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female",
                         sex == 3 ~ "Unknown"),
         Sex = factor(Sex, levels = c("Male", "Female", "Unknown"))) %>%
  mutate(cw_bin = cut(cw, breaks = seq(0, 250, 5), labels = F),
         cw_bin = seq(5, 250, 5)[cw_bin]) %>%
  group_by(Season, District,  Species, Sex, cw_bin) %>%
  summarise(num_crab = round(sum(samp_frac))) %>%
  mutate(year = as.numeric(substring(Season, 1, 4))) -> size_comp
size_comp %>%
  # filter for year 
  filter(year == 2020) %>%
    
  # compute calculated weight in lbs
  mutate(wt = ifelse(Sex == "Female", 
                     (0.000441*cw_bin^2.898686)/1000*2.20462,
                     (0.00027*cw_bin^3.022134)/1000*2.20462)) %>%
  group_by(District) %>%
  summarise(avg_wt = weighted.mean(wt, num_crab)) %>%
  left_join(tanner) %>%
  mutate(tot_wt = avg_wt * total_tanner) %>%
  dplyr::select(District, tot_wt)
  
## crab size composition plot
size_comp %>%
  filter(year == 2020) %>%
  mutate(District = case_when(District == "KNE" ~ "Kodiak Northeast\nTanner Crab",
                              District == "KSH" ~ "Kodiak Shelikof\nTanner Crab",
                              District == "KSW" ~ "Kodiak Southwest\nTanner Crab",
                              District == "YAK" ~ "Yakutat\nTanner Crab")) %>%
  ggplot()+
  geom_point(aes(x = cw_bin, y = num_crab, shape = Sex))+
  geom_line(aes(x = cw_bin, y = num_crab, linetype = Sex))+
  scale_shape_manual(values = c(8, 1))+
  scale_x_continuous(breaks = seq(0,200,15))+
  facet_wrap(~District, scales = "free_y")+
  labs(x = "Carapace Width (mm)", y = "Frequency", shape = NULL, linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/safe/2022/crab_size_comp.png", plot = x, height = 5, width = 7, units = "in")    
  


# regional fishery performance, yak ----

## harvest cpue plot
read_csv("./data/observer/old_catch/d_fishery_table_1995_2020.csv") %>%
  slice(-1:-2) %>%
  left_join(catch %>%
              # remove tows with missing lat/lon
              filter(District == "KNE",
                     !is.na(set_lon), !is.na(set_lat)) %>%
              nest(data = -Season) %>%
              mutate(extent = purrr::map_dbl(data, f_extent_catch),
                     season = as.character(Season))) %>%
  ggplot()+
  geom_bar(aes(x = season, y = ret_lbs_mt), stat = "identity", fill = "grey80")+
  geom_point(aes(x = season, y = mt_cpue * 4000), shape = 8)+
  geom_line(aes(x = season, y = mt_cpue * 4000, group = 1))+
  scale_y_continuous(breaks = seq(0, 300000, 50000),
                     labels = scales::comma, 
                     sec.axis = sec_axis(~./4000, 
                                         name = "CPUE (lb scallop meat / dredge hr)\n",
                                         breaks = seq(0, 100, 10)),
                     expand = c(0, NA),
                     limits = c(0, 300000))+
  labs(x = NULL, y = "Harvest (lb scallop meat)\n")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x

## extent
catch %>%
  # remove tows with missing lat/lon
  filter(District == "YAK",
         !is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  
  full_join(read_csv("./data/observer/old_catch/d_fishery_table_1995_2020.csv") %>%
              slice(-1:-2) %>%
              rename(Season = season)) %>%
  ggplot()+
  geom_point(aes(x = Season, y = extent))+
  geom_line(aes(x = Season, y = extent, group = 1), linetype = 2)+
  scale_y_continuous(limits = c(0.6, 2.5))+
  labs(x = NULL, y = "Extent")+
  theme(axis.text.x = element_blank()) -> y

ggsave("./figures/safe/2022/harvest_cpue_yak.png", 
       plot = y/x + plot_layout(heights = c(1, 3)),
       height = 4, width = 6, units = "in")

# regional fishery performance, eki ----
## harvest cpue plot
read_csv("./data/observer/old_catch/eki_fishery_table_1995_2020.csv") %>%
  slice(-1:-2) %>%
  left_join(catch %>%
              # remove tows with missing lat/lon
              filter(District == "EKI",
                     !is.na(set_lon), !is.na(set_lat)) %>%
              nest(data = -Season) %>%
              mutate(extent = purrr::map_dbl(data, f_extent_catch),
                     season = as.character(Season))) %>%
  ggplot()+
  geom_bar(aes(x = season, y = ret_lbs_mt), stat = "identity", fill = "grey80")+
  geom_point(aes(x = season, y = mt_cpue * 400), shape = 8)+
  geom_line(aes(x = season, y = mt_cpue * 400, group = 1))+
  scale_y_continuous(breaks = seq(0, 50000, 10000),
                     labels = scales::comma, 
                     sec.axis = sec_axis(~./400, 
                                         name = "CPUE (lb scallop meat / dredge hr)\n",
                                         breaks = seq(0, 1000, 10)),
                     expand = c(0, NA),
                     limits = c(0, 50000))+
  labs(x = NULL, y = "Harvest (lb scallop meat)\n")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x

## extent
catch %>%
  # remove tows with missing lat/lon
  filter(District == "EKI",
         !is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  
  full_join(read_csv("./data/observer/old_catch/eki_fishery_table_1995_2020.csv") %>%
              slice(-1:-2) %>% rename(Season = season)) %>%
  ggplot()+
  geom_point(aes(x = Season, y = extent))+
  geom_line(aes(x = Season, y = extent, group = 1), linetype = 2)+
  scale_y_continuous(limits = c(0, 0.8))+
  labs(x = NULL, y = "Extent")+
  theme(axis.text.x = element_blank()) -> y

ggsave("./figures/safe/2022/harvest_cpue_eki.png", 
       plot = y/x + plot_layout(heights = c(1, 3)),
       height = 4, width = 6, units = "in")


# regional fishery performance, wki ----
## harvest cpue plot
read_csv("./data/observer/old_catch/wki_fishery_table_1995_2020.csv") %>%
  slice(-1:-2) %>%
  left_join(catch %>%
              # remove tows with missing lat/lon
              filter(District == "WKI",
                     !is.na(set_lon), !is.na(set_lat)) %>%
              nest(data = -Season) %>%
              mutate(extent = purrr::map_dbl(data, f_extent_catch),
                     season = as.character(Season))) %>%
  ggplot()+
  geom_bar(aes(x = season, y = ret_lbs_mt), stat = "identity", fill = "grey80")+
  geom_point(aes(x = season, y = mt_cpue * 400), shape = 8)+
  geom_line(aes(x = season, y = mt_cpue * 400, group = 1))+
  scale_y_continuous(breaks = seq(0, 90000, 10000),
                     labels = scales::comma, 
                     sec.axis = sec_axis(~./400, 
                                         name = "CPUE (lb scallop meat / dredge hr)\n",
                                         breaks = seq(0, 1000, 20)),
                     expand = c(0, NA),
                     limits = c(0, 80000))+
  labs(x = NULL, y = "Harvest (lb scallop meat)\n")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x

## extent
catch %>%
  # remove tows with missing lat/lon
  filter(District == "WKI",
         !is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  
  full_join(read_csv("./data/observer/old_catch/wki_fishery_table_1995_2020.csv") %>%
              slice(-1:-2) %>% rename(Season = season)) %>%
  ggplot()+
  geom_point(aes(x = Season, y = extent))+
  geom_line(aes(x = Season, y = extent, group = 1), linetype = 2)+
  scale_y_continuous(limits = c(0, 0.1))+
  labs(x = NULL, y = "Extent")+
  theme(axis.text.x = element_blank()) -> y

ggsave("./figures/safe/2022/harvest_cpue_wki.png", 
       plot = y/x + plot_layout(heights = c(1, 3)),
       height = 4, width = 6, units = "in")

# regional fishery performance, kne ----

## harvest cpue plot
read_csv("./data/observer/old_catch/kne_fishery_table_1993_2020.csv") %>%
  ggplot()+
  geom_bar(aes(x = season, y = ret_lbs_mt), stat = "identity", fill = "grey80")+
  geom_point(aes(x = season, y = mt_cpue * 1500), shape = 8)+
  geom_line(aes(x = season, y = mt_cpue * 1500, group = 1))+
  scale_y_continuous(breaks = seq(0, 160000, 50000),
                     labels = scales::comma, 
                     sec.axis = sec_axis(~./1500, 
                                         name = "CPUE (lb scallop meat / dredge hr)\n",
                                         breaks = seq(0, 100, 10)),
                     expand = c(0, NA),
                     limits = c(0, 160000))+
  labs(x = NULL, y = "Harvest (lb scallop meat)\n")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x

## extent
catch %>%
  # remove tows with missing lat/lon
  filter(District == "KNE",
         !is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  
  full_join(read_csv("./data/observer/old_catch/kne_fishery_table_1993_2020.csv") %>%
              rename(Season = season)) %>%
  ggplot()+
  geom_point(aes(x = Season, y = extent))+
  geom_line(aes(x = Season, y = extent, group = 1), linetype = 2)+
  scale_y_continuous(limits = c(0, 0.8))+
  labs(x = NULL, y = "Extent")+
  theme(axis.text.x = element_blank()) -> y

ggsave("./figures/safe/2022/harvest_cpue_kne.png", 
       plot = y/x + plot_layout(heights = c(1, 3)),
       height = 4, width = 6, units = "in")




# regional fishery performance, ksh ----

## harvest cpue plot
read_csv("./data/observer/old_catch/ksh_fishery_table_1993_2020.csv") %>%
  ggplot()+
  geom_bar(aes(x = season, y = ret_lbs_mt), stat = "identity", fill = "grey80")+
  geom_point(aes(x = season, y = mt_cpue * 2500), shape = 8)+
  geom_line(aes(x = season, y = mt_cpue * 2500, group = 1))+
  scale_y_continuous(breaks = seq(0, 1e10, 5e4),
                     labels = scales::comma, 
                     sec.axis = sec_axis(~./2500, 
                                         name = "CPUE (lb scallop meat / dredge hr)\n",
                                         breaks = seq(0, 200, 10)),
                     expand = c(0, NA),
                     limits = c(0, 3.3e5))+
  labs(x = NULL, y = "Harvest (lb scallop meat)\n")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x

## extent
catch %>%
  # remove tows with missing lat/lon
  filter(District == "KSH",
         !is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  
  full_join(read_csv("./data/observer/old_catch/KSH_fishery_table_1993_2020.csv") %>%
              rename(Season = season)) %>%
  ggplot()+
  geom_point(aes(x = Season, y = extent))+
  geom_line(aes(x = Season, y = extent, group = 1), linetype = 2)+
  scale_y_continuous(limits = c(0.1, 0.3))+
  labs(x = NULL, y = "Extent")+
  theme(axis.text.x = element_blank()) -> y

ggsave("./figures/safe/2022/harvest_cpue_ksh.png", 
       plot = y/x + plot_layout(heights = c(1, 3)),
       height = 4, width = 6, units = "in")


# regional fishery performance, ksw ----

## harvest cpue plot
read_csv("./data/observer/old_catch/ksw_fishery_table_2009_2020.csv") %>%
  ggplot()+
  geom_bar(aes(x = season, y = ret_lbs_mt), stat = "identity", fill = "grey80")+
  geom_point(aes(x = season, y = mt_cpue * 600), shape = 8)+
  geom_line(aes(x = season, y = mt_cpue * 600, group = 1))+
  scale_y_continuous(breaks = seq(0, 1e10, 1e4),
                     labels = scales::comma, 
                     sec.axis = sec_axis(~./600, 
                                         name = "CPUE (lb scallop meat / dredge hr)\n",
                                         breaks = seq(0, 200, 10)),
                     expand = c(0, NA),
                     limits = c(0, 0.45e5))+
  labs(x = NULL, y = "Harvest (lb scallop meat)\n")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x

## extent
catch %>%
  # remove tows with missing lat/lon
  filter(District == "KSW",
         !is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  
  full_join(read_csv("./data/observer/old_catch/KSW_fishery_table_2009_2020.csv") %>%
              rename(Season = season)) %>%
  ggplot()+
  geom_point(aes(x = Season, y = extent))+
  geom_line(aes(x = Season, y = extent, group = 1), linetype = 2)+
  scale_y_continuous(limits = c(0.1, 0.7))+
  labs(x = NULL, y = "Extent")+
  theme(axis.text.x = element_blank()) -> y

ggsave("./figures/safe/2022/harvest_cpue_ksw.png", 
       plot = y/x + plot_layout(heights = c(1, 3)),
       height = 4, width = 6, units = "in")

# regional fishery performance, m ----
## harvest cpue plot
read_csv("./data/observer/old_catch/m_fishery_table_1993_2020.csv") %>%
  ggplot()+
  geom_bar(aes(x = season, y = ret_lbs_mt), stat = "identity", fill = "grey80")+
  geom_point(aes(x = season, y = mt_cpue * 300), shape = 8)+
  geom_line(aes(x = season, y = mt_cpue * 300, group = 1))+
  scale_y_continuous(breaks = seq(0, 1e10, 1e4),
                     labels = scales::comma, 
                     sec.axis = sec_axis(~./300, 
                                         name = "CPUE (lb scallop meat / dredge hr)\n",
                                         breaks = seq(0, 1000, 50)),
                     expand = c(0, NA),
                     limits = c(0, 1.15e5))+
  labs(x = NULL, y = "Harvest (lb scallop meat)\n")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x

## extent
catch %>%
  # remove tows with missing lat/lon
  filter(District %in% c("UB", "C", "WC"),
         !is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  
  full_join(read_csv("./data/observer/old_catch/m_fishery_table_1993_2020.csv") %>%
              rename(Season = season)) %>% 
  ggplot()+
  geom_point(aes(x = Season, y = extent))+
  geom_line(aes(x = Season, y = extent, group = 1), linetype = 2)+
  scale_y_continuous(limits = c(0, 0.5))+
  labs(x = NULL, y = "Extent")+
  theme(axis.text.x = element_blank()) -> y
  
  ggsave("./figures/safe/2022/harvest_cpue_m.png", 
         plot = y/x + plot_layout(heights = c(1, 3)),
         height = 4, width = 6, units = "in")

# regional fishery performance, o ----
  ## harvest cpue plot
  read_csv("./data/observer/old_catch/o_fishery_table_1993_2020.csv") %>%
    ggplot()+
    geom_bar(aes(x = season, y = ret_lbs_mt), stat = "identity", fill = "grey80")+
    geom_point(aes(x = season, y = mt_cpue * 500), shape = 8)+
    geom_line(aes(x = season, y = mt_cpue * 500, group = 1))+
    scale_y_continuous(breaks = seq(0, 1e10, 10e3),
                       labels = scales::comma, 
                       sec.axis = sec_axis(~./500, 
                                           name = "CPUE (lb scallop meat / dredge hr)\n",
                                           breaks = seq(0, 300, 25)),
                       expand = c(0, NA),
                       limits = c(0, 5e4))+
    labs(x = NULL, y = "Harvest (lb scallop meat)\n")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x
  
  ## extent
  catch %>%
    # remove tows with missing lat/lon
    filter(District %in% c("O"),
           !is.na(set_lon), !is.na(set_lat)) %>%
    nest(data = -Season) %>%
    mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
    
    full_join(read_csv("./data/observer/old_catch/o_fishery_table_1993_2020.csv") %>%
                rename(Season = season)) %>% 
    ggplot()+
    geom_point(aes(x = Season, y = extent))+
    geom_line(aes(x = Season, y = extent, group = 1), linetype = 2)+
    scale_y_continuous(limits = c(0, 1.1))+
    labs(x = NULL, y = "Extent")+
    theme(axis.text.x = element_blank()) -> y
  
  ggsave("./figures/safe/2022/harvest_cpue_o.png", 
         plot = y/x + plot_layout(heights = c(1, 3)),
         height = 4, width = 6, units = "in")
  
# regional fishery performance, q ----
  ## harvest cpue plot
  read_csv("./data/observer/old_catch/q_fishery_table_1993_2020.csv") %>%
    ggplot()+
    geom_bar(aes(x = season, y = ret_lbs_mt), stat = "identity", fill = "grey80")+
    geom_point(aes(x = season, y = mt_cpue * 4000), shape = 8)+
    geom_line(aes(x = season, y = mt_cpue * 4000, group = 1))+
    scale_y_continuous(breaks = seq(0, 1e10, 50e3),
                       labels = scales::comma, 
                       sec.axis = sec_axis(~./4000, 
                                           name = "CPUE (lb scallop meat / dredge hr)\n",
                                           breaks = seq(0, 4000, 20)),
                       expand = c(0, NA),
                       limits = c(0, 510e3))+
    labs(x = NULL, y = "Harvest (lb scallop meat)\n")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x
  
  ## extent
  catch %>%
    # remove tows with missing lat/lon
    filter(District %in% c("Q"),
           !is.na(set_lon), !is.na(set_lat)) %>%
    nest(data = -Season) %>%
    mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
    
    full_join(read_csv("./data/observer/old_catch/q_fishery_table_1993_2020.csv") %>%
                rename(Season = season)) %>% 
    ggplot()+
    geom_point(aes(x = Season, y = extent))+
    geom_line(aes(x = Season, y = extent, group = 1), linetype = 2)+
    scale_y_continuous(limits = c(0, 0.5))+
    labs(x = NULL, y = "Extent")+
    theme(axis.text.x = element_blank()) -> y
  
  ggsave("./figures/safe/2022/harvest_cpue_q.png", 
         plot = y/x + plot_layout(heights = c(1, 3)),
         height = 4, width = 6, units = "in")
  
  
