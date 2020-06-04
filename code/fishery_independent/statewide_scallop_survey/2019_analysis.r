# notes ----
# 2019 Statewide Scallops Survey data analysis
# author: Tyler Jackson, Ben Williams
# contact: tyler.jackson@alaska.gov
# last updated: 2020/2/11

# load ----
library(scallopr)
library(tidyverse)
library(FNGr)
theme_set(FNGr::theme_sleek())

# shapfile prep function for survey grid map
source("./misc_code/maps/adfg_map_functions.R", echo = F)
# high resolution map of alaska and canada (plots slow when layer in turned on)
# raster library required, but if library is loaded dplyr::select is masked
us <- raster::getData("GADM",country="USA",level=1, path = "./data/maps")
can <- raster::getData("GADM",country="CAN",level=1, path = "./data/maps")

# constants for 2019 survey ----
YEAR <- 2019
Q <- 0.83  #dredge efficieny
Bed_levels <- c("WK1", "YAKB", "YAK3", "YAK4", "YAK5") #bed factor levels for plotting
set.seed(8456) #seed for random number generator

# data ----
events_data <- read_csv(paste0('./data/', YEAR, '/HaulTableData_Cruise1901.csv')) 
catch_data <- read_csv(paste0('./data/', YEAR, '/CatchTableData_Cruise1901.csv'))
awl_data <- read_csv(paste0('./data/', YEAR, '/ScalMeatTableData_Cruise1901.csv'))
shell_data <- read_csv(paste0('./data/', YEAR, '/ScalShellTableData_Cruise1901.csv'))


# clean data ----
tows <- clean_tow(events_data)
beds <- clean_bed(tows)
scal_catch <- clean_catch(catch_data, tows, YEAR)
scal_awl <- clean_awl(awl_data, tows)

# tables ----

## catch by bed
tbl_catch <- tbl_catch(scal_catch, beds)

## abundance estimates by bed
abund_est <- scallop_est(scal_catch, beds, Q = Q, abundance = TRUE, boot = TRUE)

## round weight biomass estimates by bed
biom_est <- scallop_est(scal_catch, beds, Q = Q, abundance = FALSE, boot = TRUE)

## meat weight estimates by bed
meat_wt_est(scal_awl, scal_catch, Q = Q, beds, boot = TRUE)

## clappers by bed
tbl_clap(scal_catch, scal_awl)

## estimate of large scallops sex ratio by bed
tbl_sex(scal_awl, tows)

## gonad status by bed
tbl_gonad(scal_awl, tows)

## presence of boring worms by bed
tbl_worm(scal_awl, tows)

## presence of mud blisters by bed
tbl_blister(scal_awl, tows)

# figures ----

## catch rate
plot_catch_rate(scal_catch, tows, YEAR, Bed_levels)

## cv by bed
tbl_catch %>%
  ggplot()+
  geom_point(aes(x = Bed, y = CV, shape = Size))+
  labs(shape = "Size class")+
  scale_shape_manual(values = c(1, 16))+
  geom_hline(yintercept = 20, linetype = 2) -> x
ggsave(paste0("./figs/", YEAR, "/catch_cv.png"), plot = x,
       width = 6.5, height = 4)

## abundance estimates by bed
plot_scal_est(abund_est, abundance = TRUE, YEAR)

## round weight biomass estimates by bed
plot_scal_est(biom_est, abundance = FALSE, YEAR)

## meat weight ~ round weight, by district
plot_mw_rw(scal_awl, tows, YEAR)

## meat weight ~ shell height, by district
plot_mw_sh(scal_awl, tows, YEAR)

## SH size composition, by bed
plot_size_dist(scal_awl, scal_catch, tows, YEAR)

## map of survey areas
### prepare polygon data
grid <- f_shp_prep("./data/maps/statewide_scallop_survey_grid_2019", 
                   "scalGrid2019_all_albers")                   
grid <- f_albers_to_nad83(grid)            

### map of survey area
#### get bed label positions
grid %>%
  group_by(bed_code) %>%
  summarise(lat = mean(cent_lat) - 0.1,
            long = mean(cent_long)) %>%
  filter(bed_code %in% Bed_levels) -> grid_labels
#### plot map
grid %>%
  filter(bed_code %in% Bed_levels) %>% 
  mutate(bed_code = factor(bed_code, levels = Bed_levels)) %>%
  ggplot()+
  geom_polygon(data = rbind(us, can), aes(x = long, y = lat, group = group),  
               fill = "grey70", color = NA, size = 0.4)+
  geom_polygon(aes(x = long, y = lat, group = group), color = "grey20", fill = "grey20")+
  geom_text(data = grid_labels, aes(x = long, y = lat, label = bed_code), 
            nudge_x = c(0, 0 , -0.5 , -0.5, 0),
            nudge_y = c(-0.1, -0.1, 0, 0, -0.1),
            size = 3)+
  coord_map(projection = "albers", lat0 = 55, lat = 62,
            xlim = c(-145, -137.9), ylim = c(58.8,60.1))+
  labs(x = "Longitude", y = "Latitude")+
  theme_sleek()+
  theme(legend.position = "none") -> x
ggsave("./figs/2019/survey_map.png", plot = x, width = 6, height = 6, units = "in")    

## map cpue for each bed on map                      
### join polygon data with scallop catch data 
events_data %>%
  select(tow, bed_code, station) %>%
  #filter(bed_code == "WK1") %>%
  mutate(station = substring(station, 2, 6)) %>%
  rename(tow_id = tow) %>%
  left_join(scal_catch, by = "tow_id") %>%
  right_join(grid %>%
               mutate(station = gsub(" ", "", station)),
             by = c("bed_code", "station")) %>%
  filter(bed_code %in% Bed_levels) -> scal_catch_map


scal_catch_map %>%
  filter(Size %in% c("large", NA),
         bed_code == "WK1") %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group, fill = count / area_swept), 
               color = "black")+
  scale_fill_gradient(low = "yellow", high = "red", trans = "log10")+
  labs(x = NULL, y = NULL, fill = "Catch Rate")+
  coord_map(projection = "albers", lat0 = 55, lat = 62)+
  theme_sleek()+
  theme(axis.text = element_blank())







# comparison of results across survey years ----
## YAKB, YAK4, and YAK5 were surveyed in 2018, 2017, and 2016
## YAK3 was surveyed in 2018, 2017
## WK1 was surveyed in 2017

## plot of abundance est by year
bind_rows(read_csv("./output/2018/scal_abund_estimates.csv") %>%
            mutate(Year = 2018),
          read_csv("./output/2017/scal_abund_estimates.csv") %>%
            mutate(Year = 2017),
          read_csv("./output/2016/scal_abund_estimates.csv") %>%
            mutate(Year = 2016)) %>%
  filter(Bed %in% Bed_levels,
         Size != "all") %>%
  bind_rows(abund_est %>%
              mutate(Year = 2019)) %>%
  mutate(Bed = factor(Bed, levels = Bed_levels),
         Size = factor(Size, levels = c("small", "large")),
         Year = factor(Year)) %>%
  ggplot(aes(x = Year, y = est / 1000000, color = Size)) +
  geom_point(size = 1.5, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = l95 / 1000000, ymax = u95 / 1000000),
                size = 0.5,
                width = 0.4,
                position = position_dodge(.5)) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("small" = "grey70", "large" = "black"),
                     name = "Size class") +
  labs(y = "Abundance (millions)\n") +
  facet_wrap(~Bed, nrow = 1) -> x
ggsave(paste0("./figs/", YEAR, "/abund_est_1619.png"), plot = x,
       width = 10, height = 4)

## plot of biomass est by year
bind_rows(read_csv("./output/2018/scal_biom_estimates.csv") %>%
            mutate(Year = 2018),
          read_csv("./output/2017/scal_biom_estimates.csv") %>%
            mutate(Year = 2017),
          read_csv("./output/2016/scal_biom_estimates.csv") %>%
            mutate(Year = 2016)) %>%
  filter(Bed %in% Bed_levels,
         Size != "all") %>%
  bind_rows(abund_est %>%
              mutate(Year = 2019)) %>%
  mutate(Bed = factor(Bed, levels = Bed_levels),
         Size = factor(Size, levels = c("small", "large")),
         Year = factor(Year)) %>%
  ggplot(aes(x = Year, y = est / 1000000, color = Size)) +
  geom_point(size = 1.5, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = l95 / 1000000, ymax = u95 / 1000000),
                size = 0.5,
                width = 0.4,
                position = position_dodge(.5)) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("small" = "grey70", "large" = "black"),
                     name = "Size class") +
  labs(y = "Round Weight Biomass (million lb)\n") +
  facet_wrap(~Bed, nrow = 1) -> x
ggsave(paste0("./figs/", YEAR, "/biomass_est_1619.png"), plot = x,
       width = 10, height = 4)

## shell height size comp

### necessary functions
f_weighted_count_step1 <- function(awl, catch){
  catch %>%
    ungroup %>%
    filter(Size != "clapper") %>%
    select(tow_id, N = count, size = Size) -> tow_count
  
  awl %>%
    dplyr::select(tow_id, size, sh) %>%
    filter(!is.na(sh), sh > 0, !is.na(size)) %>%
    add_count(tow_id, size) %>% # add subsample sizes by size class within each tow
    left_join(tow_count, by = c("tow_id", "size")) #%>%
  #left_join(tows, by = "tow_id") %>%
  #filter(n <= N, N * n != 0) %>%
  # mutate(wt = (N / n) / area_swept,
  #Bed = factor(Bed, levels = Bed_levels)) %>%
  #add_count(Bed) 
}
f_weighted_count_step2 <- function(int, tows){
  int %>%
    left_join(tows, by = "tow_id") %>%
    filter(n <= N, N * n != 0) %>%
    mutate(wt = (N / n) / area_swept,
           Bed = factor(Bed, levels = Bed_levels)) %>%
    add_count(Bed) 
}
f_sh_size_comp <- function(pbed){
  
  tibble(Year = c(2016:2019),
         awl = list(read_csv("./output/2016/awl_tbl.csv"),
                    read_csv("./output/2017/awl_tbl.csv"),
                    read_csv("./output/2018/awl_tbl.csv"),
                    read_csv("./output/2019/awl_tbl.csv") %>%
                      mutate(tow_id = as.character(tow_id))),
         catch = list(read_csv("./output/2016/scal_catch.csv"),
                      read_csv("./output/2017/scal_catch.csv"),
                      read_csv("./output/2018/scal_catch.csv"),
                      read_csv("./output/2019/scal_catch.csv") %>%
                        mutate(tow_id = as.character(tow_id))),
         tows = list(read_csv("./output/2016/tows.csv"),
                     read_csv("./output/2017/tows.csv"),
                     read_csv("./output/2018/tows.csv"),
                     read_csv("./output/2019/tows.csv") %>%
                       mutate(tow_id = as.character(tow_id)))) %>%
    mutate(int = map2(awl, catch, f_weighted_count_step1),
           wt_sh = map2(int, tows, f_weighted_count_step2)) %>%
    select(Year, wt_sh) %>%
    pull(wt_sh) %>%
    do.call("rbind", .) %>%
    mutate(Year = substring(tow_id, 1, 4),
           Year = factor(ifelse(Year == "1901", 2019, Year))) %>%
    filter(Bed == pbed) %>%
    ggplot(aes(x = sh, weight=wt)) +
    geom_histogram(color = "black", fill = "darkgray", binwidth = 2) +
    facet_grid(cols = vars(Bed), rows = vars(Year)) +
    xlab("\nShell height (mm)") +
    scale_y_continuous("Weighted shell height counts\n", label = scales::comma) +
    theme(strip.background = element_blank()) -> x
  
  ggsave(here::here(paste0("figs/", YEAR, "/", pbed, "_size_dist.png")), plot = x, width = 6.5, height = 4)
  
  x
}
### print plots
Bed_levels %>%
  purrr::map(~f_sh_size_comp(.))

## meat weight ~ shell height
f_mw_sh <- function(scal_awl, tows){
  scal_awl %>%
    filter(sh >= 100, !is.na(mwt_lb), mwt_lb * rwt_lb > 0, mwt_lb < rwt_lb) %>%
    left_join(tows, by = "tow_id") %>%
    left_join(bed_dist_area_names, by = c("Bed" = "Bed_Code"))
}
f_mw_sh_plot <- function(pbed){
  tibble(Year = c(2016:2019),
         scal_awl = list(read_csv("./output/2016/awl_tbl.csv"),
                         read_csv("./output/2017/awl_tbl.csv"),
                         read_csv("./output/2018/awl_tbl.csv"),
                         read_csv("./output/2019/awl_tbl.csv")),
         tows = list(read_csv("./output/2016/tows.csv"),
                     read_csv("./output/2017/tows.csv"),
                     read_csv("./output/2018/tows.csv"),
                     read_csv("./output/2019/tows.csv") %>%
                       select(-station))) %>%
    mutate(mw_sh = map2(scal_awl, tows, f_mw_sh)) %>%
    pull(mw_sh) %>%
    do.call("rbind", .) %>%
    mutate(Year = substring(tow_id, 1, 4),
           Year = factor(ifelse(Year == "1901", 2019, Year))) %>%
    filter(Bed == pbed) %>%
    ggplot(aes(x = sh, y = mwt_lb, color = Year)) +
    geom_point(alpha = 0.3) +
    geom_smooth(se = F, method = "gam", formula = y ~ s(x, bs = "cs")) +
    labs(y = "Meat weight (lb)", x = "Shell height (mm)", title = pbed) +
    scale_color_grey() +
    theme(legend.justification=c(1,0), 
          legend.position=c(0.2,0.65),
          plot.title = element_text(hjust = 0.5)) -> x
  
  ggsave(here::here(paste0("figs/", YEAR, "/", pbed, "_mwt_sh.png")),
         plot = x, width = 4, height = 3, units = "in")
  x
}
### print plots
Bed_levels %>%
  purrr::map(~f_mw_sh_plot(.))
### all YAK beds combined
tibble(Year = c(2016:2019),
       scal_awl = list(read_csv("./output/2016/awl_tbl.csv"),
                       read_csv("./output/2017/awl_tbl.csv"),
                       read_csv("./output/2018/awl_tbl.csv"),
                       read_csv("./output/2019/awl_tbl.csv")),
       tows = list(read_csv("./output/2016/tows.csv"),
                   read_csv("./output/2017/tows.csv"),
                   read_csv("./output/2018/tows.csv"),
                   read_csv("./output/2019/tows.csv") %>%
                     select(-station))) %>%
  mutate(mw_sh = map2(scal_awl, tows, f_mw_sh)) %>%
  pull(mw_sh) %>%
  do.call("rbind", .) %>%
  mutate(Year = substring(tow_id, 1, 4),
         Year = factor(ifelse(Year == "1901", 2019, Year))) %>%
  filter(District == "YAK") %>%
  #filter(gonad == 4) %>%
  ggplot(aes(x = sh, y = mwt_lb, color = Year)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = F, method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(y = "Meat weight (lb)", x = "Shell height (mm)", title = "Yakutat Combined") +
  scale_color_grey() +
  theme(legend.justification=c(1,0), 
        legend.position=c(0.2,0.6),
        plot.title = element_text(hjust = 0.5)) -> x

ggsave("figs/2019/yak_combined_mwt_sh.png", plot = x, width = 4, height = 3, units = "in")


## meat weight ~ round weight
f_mw_rw <- function(scal_awl, tows){
  scal_awl %>%
    filter(sh >= 100, !is.na(mwt_lb), mwt_lb * rwt_lb > 0, mwt_lb < rwt_lb) %>%
    left_join(tows, by = "tow_id") %>%
    left_join(bed_dist_area_names, by = c("Bed" = "Bed_Code"))
}
f_mw_rw_plot <- function(pbed){
  tibble(Year = c(2016:2019),
         scal_awl = list(read_csv("./output/2016/awl_tbl.csv"),
                         read_csv("./output/2017/awl_tbl.csv"),
                         read_csv("./output/2018/awl_tbl.csv"),
                         read_csv("./output/2019/awl_tbl.csv")),
         tows = list(read_csv("./output/2016/tows.csv"),
                     read_csv("./output/2017/tows.csv"),
                     read_csv("./output/2018/tows.csv"),
                     read_csv("./output/2019/tows.csv") %>%
                       select(-station))) %>%
    mutate(mw_rw = map2(scal_awl, tows, f_mw_rw)) %>%
    pull(mw_rw) %>%
    do.call("rbind", .) %>%
    mutate(Year = substring(tow_id, 1, 4),
           Year = factor(ifelse(Year == "1901", 2019, Year))) %>%
    filter(Bed == pbed) %>%
    ggplot(aes(x = rwt_lb, y = mwt_lb, color = Year)) +
    geom_point(alpha = 0.3) +
    geom_smooth(se = F, method = "gam", formula = y ~ s(x, bs = "cs")) +
    labs(y = "Meat weight (lb)", x = "Round weight (lb)", title = pbed) +
    scale_color_grey() +
    theme(legend.justification=c(1,0), 
          legend.position=c(0.2,0.65),
          plot.title = element_text(hjust = 0.5)) -> x
  
  ggsave(paste0("figs/", YEAR, "/", pbed, "_mwt_rw.png"), plot = x, width = 4, height = 3, units = "in")
  x
}
### print plots
Bed_levels %>%
  purrr::map(~f_mw_rw_plot(.))
### all YAK beds combined
tibble(Year = c(2016:2019),
       scal_awl = list(read_csv("./output/2016/awl_tbl.csv"),
                       read_csv("./output/2017/awl_tbl.csv"),
                       read_csv("./output/2018/awl_tbl.csv"),
                       read_csv("./output/2019/awl_tbl.csv")),
       tows = list(read_csv("./output/2016/tows.csv"),
                   read_csv("./output/2017/tows.csv"),
                   read_csv("./output/2018/tows.csv"),
                   read_csv("./output/2019/tows.csv") %>%
                     select(-station))) %>%
  mutate(mw_rw = map2(scal_awl, tows, f_mw_rw)) %>%
  pull(mw_rw) %>%
  do.call("rbind", .) %>%
  mutate(Year = substring(tow_id, 1, 4),
         Year = factor(ifelse(Year == "1901", 2019, Year))) %>%
  filter(District == "YAK") %>%
  ggplot(aes(x = rwt_lb, y = mwt_lb, color = Year)) +
  geom_point(alpha = 0.1) +
  geom_smooth(se = F, method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(y = "Meat weight (lb)", x = "Round weight (lb)", title = "Yakutat Combined") +
  scale_color_grey() +
  theme(legend.justification=c(1,0), 
        legend.position=c(0.2,0.6),
        plot.title = element_text(hjust = 0.5)) -> x

ggsave("figs/2019/yak_combined_mwt_rw.png", plot = x, width = 4, height = 3, units = "in")

## Gonad development within years
tibble(Year = c(2016:2019),
       scal_awl = list(read_csv("./output/2016/awl_tbl.csv"),
                       read_csv("./output/2017/awl_tbl.csv"),
                       read_csv("./output/2018/awl_tbl.csv"),
                       read_csv("./output/2019/awl_tbl.csv") %>%
                         mutate(tow_id = as.character(tow_id))),
       tows = list(read_csv("./output/2016/tows.csv"),
                   read_csv("./output/2017/tows.csv"),
                   read_csv("./output/2018/tows.csv"),
                   read_csv("./output/2019/tows.csv") %>%
                     mutate(tow_id = as.character(tow_id)) %>%
                     select(-station))) %>%
  mutate(mw_sh = map2(scal_awl, tows, f_mw_rw)) %>%
  select(Year, mw_sh) %>%
  unnest(mw_sh) %>%
  mutate(Gonad_name = case_when(gonad == 0 ~ "Immature",
                                gonad == 1 ~ "Empty",
                                gonad == 2 ~ "Initial Recovery",
                                gonad == 3 ~ "Filling",
                                gonad == 4 ~ "Full",
                                gonad == 5 ~ "Cannot Determine"),
         Gonad_name = factor(Gonad_name, levels = c("Immature", "Empty", "Initial Recovery",
                                                    "Filling", "Full", "Cannot Determine"))) %>%
  filter(size == "large",
         Bed %in% Bed_levels) %>%
  ggplot()+
  geom_bar(aes(x = factor(Year), fill = Gonad_name), position = "fill")+
  scale_fill_grey(na.value = "white")+
  labs(x = NULL, y = "Proportion", fill = "Gonad Development") -> x 

ggsave("./figs/2019/gonad_score.png", plot = x, width = 4, height = 4, units = "in")

# Examine shell height meat wieght with only gonad == 3 individuals
tibble(Year = c(2016:2019),
       scal_awl = list(read_csv("./output/2016/awl_tbl.csv"),
                       read_csv("./output/2017/awl_tbl.csv"),
                       read_csv("./output/2018/awl_tbl.csv"),
                       read_csv("./output/2019/awl_tbl.csv")),
       tows = list(read_csv("./output/2016/tows.csv"),
                   read_csv("./output/2017/tows.csv"),
                   read_csv("./output/2018/tows.csv"),
                   read_csv("./output/2019/tows.csv") %>%
                     select(-station))) %>%
  mutate(mw_sh = map2(scal_awl, tows, f_mw_rw)) %>%
  pull(mw_sh) %>%
  do.call("rbind", .) %>%
  mutate(Year = substring(tow_id, 1, 4),
         Year = factor(ifelse(Year == "1901", 2019, Year))) %>%
  filter(District == "YAK",
         size == "large",
         gonad == 3) %>%
ggplot(aes(x = rwt_lb, y = mwt_lb, color = Year)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = F, method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(y = "Meat weight (lb)", x = "Round weight (lb)", title = "Yakutat Combined") +
  scale_color_grey() +
  theme(legend.justification=c(1,0), 
        legend.position=c(0.2,0.65),
        plot.title = element_text(hjust = 0.5)) -> x
ggsave("figs/2019/yak_combined_mwt_rw_gonad3.png", plot = x, width = 6.5, height = 4, units = "in")
  

## map cpue for each bed on map
# start with the tows for each year
bind_rows(read_csv("./data/2018/events_180719.csv") %>%
            clean_tow(., year = 2018) %>%
            mutate(Year = as.numeric(substring(tow_id, 1, 4)),
                   station = gsub(" ", "", station)),
          read_csv("./data/2019/HaulTableData_Cruise1901.csv") %>%
            clean_tow(., year = 2019) %>%
            mutate(Year = 2019,
                   tow_id = as.character(tow_id),
                   station = substring(station, 2, 6))) %>%
  # add scallop catch for each year
  left_join(bind_rows(read_csv("./output/2016/scal_catch.csv"),
                      read_csv("./output/2017/scal_catch.csv"),
                      read_csv("./output/2018/scal_catch.csv"),
                      read_csv("./output/2019/scal_catch.csv") %>%
                        mutate(tow_id = as.character(tow_id)) %>%
                        select(-station)),
            by = c("tow_id", "Bed", "area_swept")) %>%
  filter(station == "G34")


# join with each year's grid by station and year
right_join(expand_grid(grid, Year = 2016:2019) %>%
             mutate(station = gsub(" ", "", station)),
           by = c("station", "Year")) %>%
  # remove beds that aren't surveyed in 2019
  filter(bed_code %in% Bed_levels) -> multi_yr_grid

f_catch_heat_map <- function(bed, height = 3, width = 6, nrow = 1){
  multi_yr_grid %>%
    group_by(Year, bed_code) %>%
    filter(sum(wt_lb, na.rm = T) > 0,
           Size %in% c("large", NA),
           bed_code == bed) %>% 
    ggplot()+
    geom_polygon(aes(x = long, y = lat, group = group, fill = count), 
                 color = "black")+
    scale_fill_gradient(low = "yellow", high = "red", trans = "log10")+
    labs(x = NULL, y = NULL, fill = "Catch Rate", title = bed)+
    facet_wrap(~Year, nrow = nrow)+
    coord_map(projection = "albers", lat0 = 55, lat = 62)+
    theme_sleek()+
    theme(axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5)) -> x
  ggsave(paste0("./figs/2019/catch_heat_map_", bed, ".png"), plot = x, height = height, width = width,
         units = "in")
}

# create maps for all beds
f_catch_heat_map("WK1", height = 4, width = 4)
f_catch_heat_map("YAKB", height = 4, width = 4)
f_catch_heat_map("YAK3", height = 6, width = 6, nrow = 2)
f_catch_heat_map("YAK4", height = 6, width = 6, nrow = 2)
f_catch_heat_map("YAK5", height = 4, width = 4, nrow = 2)
