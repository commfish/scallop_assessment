# notes ----

# Compare scallop discard and bycatch timeseries (old calc method vs new 2020 method, see 2020 SPT presentation)
# Tyler Jackson
# last updated: 2020/1/30

# load ----

source("./code/safe_fishery_functions.R")

set.seed(7918)

# bootstrap CI function for bycatch discards
boot_bycatch_ci <- function(split){
  
  rsample::analysis(split) %>%
    mutate(dredge_hrs = sum(dredge_hrs),
           sample_hrs = sum(sample_hrs),
           bairdi = sum(bairdi_count) / sample_hrs * dredge_hrs,
           halibut = sum(halibut_count) / sample_hrs * dredge_hrs) %>%
    summarise(bairdi_number = mean(bairdi, na.rm = T),
              halibut_number = mean(halibut, na.rm = T))
}

# data ----

## old time series
old_ts <- read_csv("./data/bycatch/old_bycatch/discard_bycatch_estimates_oldmethod_2009_2019.csv")

## bycatch data (crab, fish and scallop discards)
bycatch <- do.call(bind_rows,
                   lapply(paste0("data/bycatch/", list.files("data/bycatch/")), read_csv))

### bycatch data names
names(bycatch) <- c("Fishery", "ADFG", "Set_date", "District", "hauls", "dredge_hrs", 
                    "est_rnd_wt", "mt_wt", "sample_hrs", "bairdi_count", "opilio_count",
                    "dungeness_count", "halibut_count", "disc_count", "disc_wt", "broken_wt",
                    "rem_disc_wt", "clapper_count", "king_count")

### change Yakutat Ditrict code to YAK
bycatch %>%
  mutate(District = ifelse(District %in% c("YAK", "D", "D16"), "YAK", District)) -> bycatch

dist_list <- unique(old_ts$District_code)


# scallop discards ----

## compute new scallop discard timeseries
tibble(district = dist_list,
       disc = dist_list %>%
         purrr::map(~f_scal_discard(bycatch, district = ., specific_m = T))) %>%
  unnest(disc) -> new_ts_scal

## plot scallop discard estimates
old_ts %>%
  mutate(District_code = ifelse(District %in% c("YAK", "D", "D16"), "YAK", District_code)) %>%
  select(1, 3, 4, 5, 6) %>%
  rename("district" = District_code,
         "scal_disc_est_lbs" = discard_est,
         "lwr95" = discard_lwr95,
         "upp95" = discard_upp95) %>%
  mutate(ts = "old") %>%
  group_by(Season, district, ts) %>%
  summarise(scal_disc_est_lbs = sum(scal_disc_est_lbs),
            lwr95 = sum(lwr95),
            upp95 = sum(upp95)) %>%
  bind_rows(new_ts_scal %>%
              select(2, 1, 4:6) %>%
              mutate(ts = "new")) %>%
  filter(district %in% c("KNE", "YAK", "KSH", "Q")) %>%
  ggplot()+
  geom_line(aes(x = Season, y = scal_disc_est_lbs / 1000, group = ts, linetype = ts), size = 1)+
  geom_errorbar(aes(x = Season, ymin = lwr95 / 1000, ymax = upp95 / 1000), width = 0.2)+
  labs(x = NULL, y = "Scallop Discards (thousand lbs)", linetype = "Timeseries")+
  facet_wrap(~district, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90)) -> x
  ggsave("./figures/bycatch_estimation/scallop_discard_timeseries.png", plot = x, width = 9, height = 5, units = "in")
  
## plot scallop discard mortality (meat weight lbs)
old_ts %>%
  mutate(District_code = ifelse(District %in% c("YAK", "D", "D16"), "YAK", District_code),
         meat_disc_mort_lbs = discard_est * meat_rate * 0.20,
         disc_mort_lwr95 = discard_lwr95 * meat_rate * 0.20,
         disc_mort_upp95 = discard_upp95 * meat_rate * 0.20) %>%
  select(1, 3, 17:19) %>%
  rename("district" = District_code) %>%
  mutate(ts = "old") %>%
  group_by(Season, district, ts) %>%
  summarise(meat_disc_mort_lbs = sum(meat_disc_mort_lbs),
            disc_mort_lwr95 = sum(disc_mort_lwr95),
            disc_mort_upp95 = sum(disc_mort_upp95)) %>%
  bind_rows(new_ts_scal %>%
              mutate(disc_mort_lwr95 = (lwr95 * 0.10 * intact_prop * 0.2) + (lwr95 * 0.10 * broken_prop),
                     disc_mort_upp95 = (upp95 * 0.10 * intact_prop * 0.2) + (upp95 * 0.10 * broken_prop)) %>%
              select(2, 1, 10:12) %>%
              mutate(ts = "new")) %>%
    filter(district %in% c("KNE", "YAK", "KSH", "Q")) %>%
    ggplot()+
    geom_line(aes(x = Season, y = meat_disc_mort_lbs, group = ts, linetype = ts), size = 1)+
    geom_errorbar(aes(x = Season, ymin = disc_mort_lwr95, ymax = disc_mort_upp95 ), width = 0.2)+
    labs(x = NULL, y = "Discards Mortality (lbs meat weight)", linetype = "Timeseries")+
    facet_wrap(~district, scales = "free_y")+
    theme(axis.text.x = element_text(angle = 90)) -> x
  ggsave("./figures/bycatch_estimation/scallop_discard_mortality_timeseries.png", plot = x, width = 9, height = 5, units = "in")

# tanner crab discards ----
  
bycatch %>%
  mutate(set_date = mdy(Set_date),
         month = month(set_date),
         year = year(set_date),
         Season = ifelse(month >= 7, 
                         paste0(year,"/", substring(year + 1, 3, 4)), 
                         paste0(year - 1,"/", substring(year, 3, 4)))) -> bc
  
bc %>%
  nest(-Season, -District) %>%
  mutate(samp = map(data, ~rsample::bootstraps(., 1000))) %>%
  unnest(samp) %>%
  mutate(models = map(splits, ~boot_bycatch_ci(.x))) %>%
  unnest(models) %>%
  group_by(Season, District) %>%
  summarise(lwr95 = quantile(bairdi_number, 0.025, na.rm = T),
            upp95 = quantile(bairdi_number, 0.975, na.rm = T)) -> ci  
  
bc %>%
  group_by(Season, District) %>%
  summarize(dredge_hrs = sum(dredge_hrs),
            sample_hrs = sum(sample_hrs),
            bairdi = sum(bairdi_count) / sample_hrs * dredge_hrs) %>%
    select(Season, District, bairdi) %>%
  left_join(ci, by = c("Season", "District")) %>%
  mutate(ts = "new") %>%
  bind_rows(old_ts %>%
              select(1, 3, 8:10) %>%
              rename(District = District_code, 
                     bairdi = tanner_est,
                     lwr95 = tanner_lwr95,
                     upp95 = tanner_upp95) %>%
              group_by(Season, District) %>%
              summarise(bairdi = sum(bairdi),
                        lwr95 = sum(lwr95),
                        upp95 = sum(upp95)) %>%
              mutate(ts = "old")) %>%
  filter(District %in% c("KNE", "YAK", "KSH", "Q")) %>%
  ggplot()+
  geom_line(aes(x = Season, y = bairdi / 1000, group = ts, linetype = ts), size = 1)+
  geom_errorbar(aes(x = Season, ymin = lwr95 / 1000, ymax = upp95 / 1000), width = 0.2)+
  labs(x = NULL, y = "Tanner Crab (tousands of crab)", linetype = "Timeseries")+
  facet_wrap(~District, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90)) -> x
  
ggsave("./figures/bycatch_estimation/tanner_crab_bycatch_timeseries.png", plot = x, width = 9, height = 5, units = "in")


# halibut crab discards ----

bc %>%
  nest(-Season, -District) %>%
  mutate(samp = map(data, ~rsample::bootstraps(., 1000))) %>%
  unnest(samp) %>%
  mutate(models = map(splits, ~boot_bycatch_ci(.x))) %>%
  unnest(models) %>%
  group_by(Season, District) %>%
  summarise(lwr95 = quantile(halibut_number, 0.025, na.rm = T),
            upp95 = quantile(halibut_number, 0.975, na.rm = T)) -> ci  

bc %>%
  group_by(Season, District) %>%
  summarize(dredge_hrs = sum(dredge_hrs),
            sample_hrs = sum(sample_hrs),
            halibut = sum(halibut_count) / sample_hrs * dredge_hrs) %>%
  select(Season, District, halibut) %>%
  left_join(ci, by = c("Season", "District")) %>%
  mutate(ts = "new") %>%
  bind_rows(old_ts %>%
              select(1, 3, 14:16) %>%
              rename(District = District_code, 
                     halibut = halibut_est,
                     lwr95 = halibut_lwr95,
                     upp95 = halibut_upp95) %>%
              group_by(Season, District) %>%
              summarise(halibut = sum(halibut),
                        lwr95 = sum(lwr95),
                        upp95 = sum(upp95)) %>%
              mutate(ts = "old")) %>%
  filter(District %in% c("KNE", "YAK", "KSH", "Q")) %>%
  ggplot()+
  geom_line(aes(x = Season, y = halibut, group = ts, linetype = ts), size = 1)+
  geom_errorbar(aes(x = Season, ymin = lwr95, ymax = upp95), width = 0.2)+
  labs(x = NULL, y = "Halibut (# fish)", linetype = "Timeseries")+
  facet_wrap(~District, scales = "free_y")+
  theme(axis.text.x = element_text(angle = 90)) -> x

ggsave("./figures/bycatch_estimation/halibut_bycatch_timeseries.png", plot = x, width = 9, height = 5, units = "in")
