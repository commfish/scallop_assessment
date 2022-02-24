# notes ----
## examine survey reduction in Yakutat and Kayak Island districts
## Tyler Jackson  
## 2/22/2021

# load ----

### functions for survey data
source("./code/misc/statewide_scallop_survey_functions.R")

library(knitr)
library(kableExtra)

### beds
beds <- c("WK1", "EK1", "YAKB", "YAK1", "YAK2", "YAK3", "YAK4", "YAK5")

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
filter(tows, tow != 19010054) -> tows
filter(catch, tow != 19010054) -> catch
filter(shaw, tow != 19010054) -> shaw
filter(shad, tow != 19010054) -> shad

## edit strata so that YAKB gets the code EK1 for consistency
strata %>%
  # filter for only active stations
  filter(status == "active") %>%
  # compute bed area
  group_by(bed_code) %>%
  summarise(area_nm2 = sum(area_nmi2_alb),
            n_stations = n()) %>%
  # filter for beds surveyed
  filter(bed_code %in% beds) -> bed_area


# compute baseline estimates at 30% sample rate ----

## get cpue, abundance, and cv for current sampling rate 30%
catch %>%
  ungroup() %>%
  filter(rcode == 74120,
         samp_grp == 1,
         bed_code %in% beds) %>%
  # separate YAKB and EK1
  mutate(bed_code = ifelse(bed_code == "EK1" & year %in% c(2017, 2019), "YAKB", bed_code)) %>%
  dplyr::select(year, tow, bed_code, area_swept, samp_cnt, cpue_cnt) %>%
  left_join(bed_area, by = "bed_code") %>%
  group_by(year, bed_code) %>%
  summarise(cpue = mean(cpue_cnt),
            abundance = cpue * mean(area_nm2),
            cv = sqrt(var(cpue_cnt) / n()) / cpue) -> baseline

# define function for bootstrapping ----
sample_sim <- function(data, n, rate, rep = 1000){
  replicate(rep, data, simplify = F) %>%
    purrr::map(sample_n, size = round(n * rate), replace = T) %>%
    purrr::map(function(x){
      tibble(cpue = mean(x$cpue_cnt),
             abundance = mean(x$cpue_cnt) * mean(x$area_nm2),
             cv = sqrt(var(x$cpue_cnt) / nrow(x)) / mean(x$cpue_cnt))
    }) -> tmp
  tibble(iteration = 1:rep,
         data = tmp) %>%
    unnest(data) -> out
  
  return(out)
}


# create bootstrap samples ----
catch %>%
  ungroup() %>%
  filter(rcode == 74120,
         samp_grp == 1,
         bed_code %in% beds) %>%
  # separate YAKB and EK1
  mutate(bed_code = ifelse(bed_code == "EK1" & year %in% c(2017, 2019), "YAKB", bed_code)) %>%
  dplyr::select(year, tow, bed_code, area_swept, samp_cnt, cpue_cnt) %>%
  left_join(bed_area, by = "bed_code") %>%
  group_by(year, bed_code, n_stations) %>%
  nest() %>%
  mutate(nrow_data = purrr::map_dbl(data, nrow))
,
         boot = purrr::map2(data, nrow_data, sample_sim, rate = 1, rep = 1000),
         sim1 = purrr::map2(data, n_stations, sample_sim, rate = 0.25, rep = 1000),
         sim2 = purrr::map2(data, n_stations, sample_sim, rate = 0.20, rep = 1000)) %>%
  dplyr::select(year, bed_code, boot, sim1, sim2) %>%
  saveRDS(., "./output/fishery_independent/survey_design/egoa_sample_rate.RDS")
  



# read results ----

## differences in cv
readRDS("./output/fishery_independent/survey_design/egoa_sample_rate.RDS") %>%
  unnest(sim1, sim2) %>%
  ungroup() %>%
  group_by(year, bed_code) %>%
  summarise(boot_cv_25 = mean(cv),
            boot_cv_20 = mean(cv1)) %>%
  # join to baseline
  left_join(baseline %>%
              dplyr::select(year, bed_code, cv) %>%
              rename(cv33 = cv),
            by = c("year", "bed_code")) %>%
  dplyr::select(year, bed_code, cv33, boot_cv_25, boot_cv_20) %>%
  arrange(bed_code)

readRDS("./output/fishery_independent/survey_design/egoa_sample_rate.RDS") %>%
  unnest(boot, sim1, sim2) %>%
  ungroup() %>%
  dplyr::select(year, bed_code, abundance, abundance1, abundance2) %>%
  pivot_longer(c(abundance, abundance1, abundance2), names_to = "rate", values_to = "abundance_sim") %>%
  mutate(rate = case_when(rate == "abundance" ~ "33%",
                          rate == "abundance1" ~ "25%",
                          rate == "abundance2" ~ "20%"),
         rate = factor(rate, levels = c("33%", "25%", "20%"))) %>%
  left_join(baseline %>%
              dplyr::select(year, bed_code, abundance),
            by = c("year", "bed_code")) %>%
  group_by(year, bed_code, rate) %>%
  summarise(cv = sqrt(var(abundance_sim)) / mean(abundance_sim))



readRDS("./output/fishery_independent/survey_design/egoa_sample_rate.RDS") %>%
  unnest(boot, sim1, sim2) %>%
  ungroup() %>%
  dplyr::select(year, bed_code, abundance, abundance1, abundance2) %>%
  pivot_longer(c(abundance, abundance1, abundance2), names_to = "rate", values_to = "abundance_sim") %>%
  mutate(rate = case_when(rate == "abundance" ~ "33%",
                          rate == "abundance1" ~ "25%",
                          rate == "abundance2" ~ "20%"),
         rate = factor(rate, levels = c("33%", "25%", "20%"))) %>%
  left_join(baseline %>%
              dplyr::select(year, bed_code, abundance),
            by = c("year", "bed_code")) %>%
  
  ggplot()+
  geom_density(aes(x = abundance_sim / 1000000, fill = rate), alpha = 0.5)+
  geom_vline(aes(xintercept = abundance / 1000000), linetype = 2)+
  facet_wrap(bed_code~year, scales = "free")+
  labs(x = "Abundance (millions)", y = "Density", fill = "Sampling Rate")+
  theme_bw()+
  theme(strip.background = element_rect(color = NA),
        legend.position = "bottom") -> x
ggsave("./figures/fishery_independent/survey_design/sampling_rate_egoa.png",
       height = 7, width = 8, units = "in")


