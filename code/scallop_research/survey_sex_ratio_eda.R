# load ----
library(tidyverse)
library(here)
library(FNGr)

### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### set theme (from FNGr)
theme_set(theme_sleek() + theme(legend.position = "bottom"))


# data ----

## survey ages
ages <- read_csv(here("data/statewide_scallop_survey/ages", "2016-2018_scallop_survey_ages.csv"))

## catch data
### 2016 - 2018
catch <- read_csv(here("data/statewide_scallop_survey/catch", "catchComp_180719.csv"))
### 2019 - 2020
catch2 <- read_csv(here("data/statewide_scallop_survey/catch", "Cruise1901_2001_ScallopCatchAndBycatchData.csv"))

## specimen data
### 2016 - 2018
specimen <- read_csv(here("data/statewide_scallop_survey/specimen", "awl_180719.csv"))
### 2019 - 2020
specimen2 <- read_csv(here("data/statewide_scallop_survey/specimen", "Cruise1901_2001_ScallopShellHeightAndWeightData.csv"))
specimen2.1 <- read_csv(here("data/statewide_scallop_survey/specimen", "Cruise1901_2001_ScallopShellHeightAndDamageData.csv"))


## logbook
### 2016 - 2018
logbook <- read_csv(here("data/statewide_scallop_survey/logbook", "events_180719.csv"))
### 2019 - 2020
logbook2 <- read_csv(here("data/statewide_scallop_survey/logbook", "Cruise1901_2001_FishingLogData.csv"))


# data mgmt ----
## build specimen data
specimen %>%
  rename_all(tolower) %>%
  rename(tow = event_id,
         shell_num = scallop_number,
         sex = sex_sw, 
         shell_height = shell_height_mm,
         samp_grp = scal_size_class) %>%
  dplyr::select(tow, shell_num, samp_grp, shell_height, sex) %>%
  bind_rows(bind_rows(specimen2, mutate(specimen2.1, damage = as.character(damage))) %>%
              mutate(tow = as.character(tow)) %>%
              dplyr::select(tow, shell_num, samp_grp, shell_height, sex)) %>%
  # join to age data
  left_join(ages %>%
              rename_all(tolower) %>%
              rename(tow = event_id) %>%
              dplyr::select(tow, shell_num, age),
            by = c("tow", "shell_num")) %>%
  # join to district
  left_join(logbook %>% 
              rename_all(tolower) %>%
              dplyr::select(year, event_id, district, bed_sw) %>%
              rename(tow = event_id,
                     bed_name = bed_sw) %>%
              bind_rows(logbook2 %>%
                          mutate(tow = as.character(tow)) %>%
                          rename(year = cruise_year) %>%
                          dplyr::select(year, tow, district, bed_name)),
            by = "tow") %>%
  # join to catch at tow
  left_join(catch %>%
              rename_all(tolower) %>%
              rename(tow = event_id,
                     samp_grp = scal_size_class,
                     samp_cnt = count, 
                     samp_wt = sample_wt_kg) %>%
              filter(!is.na(samp_grp),
                     race_code == 74120) %>%
              dplyr::select(tow, samp_grp, samp_cnt, samp_wt) %>%
              bind_rows(catch2 %>%
                          filter(!is.na(samp_grp),
                                 rcode == 74120) %>%
                          mutate(tow = as.character(tow)) %>%
                          dplyr::select(tow, samp_grp, samp_cnt, samp_wt)) %>%
              group_by(tow, samp_grp) %>%
              summarise_all(sum, na.rm = T),
            by = c("tow", "samp_grp")) %>%
  # filter out scallops without size, or sex
  filter(!(year <= 2018 & is.na(age)),
         !is.na(shell_height)) %>%
  # reorder columns
  dplyr::select(year, tow, district, bed_name, shell_num, samp_grp, samp_cnt, samp_wt, age, sex, shell_height) -> scal


# eda ----

## size at age by district
scal %>%
  filter(district %in% c("KSH", "YAK"),
         !is.na(age),
         !is.na(shell_height),
         sex %in% c(0, 1, 2)) %>%
  mutate(age = ifelse(age > 20, 20, age)) %>%
  ggplot()+
  geom_boxplot(aes(x = factor(age), y = shell_height, fill = factor(sex)), outlier.color = NA)+
  facet_wrap(~district, ncol = 1)+
  scale_fill_manual(values = cb_palette[c(1, 3, 7)],
                    labels = c("Undetermined", "Male", "Female"))+
  labs(x = "Age", y = "Shell Height (mm)", fill = NULL) -> x
ggsave(here("./figures/scallop_research/protandry", "size_at_age.png"), plot = x, height = 6, width = 5, units = "in")

## expanded size comp
scal %>%
  filter(district %in% c("KSH", "YAK"),
         samp_grp %in% c(1, 2)) %>%
  group_by(year, tow, district, bed_name, samp_grp, samp_cnt, shell_height) %>%
  summarise(count = n()) %>%
  group_by(tow, samp_grp) %>%
  mutate(n_measured = sum(count)) %>%
  ungroup() %>%
  mutate(sample_factor = samp_cnt * (count / n_measured)) %>%
  ggplot()+
  geom_histogram(aes(x = shell_height, y = ..density.., weight = sample_factor), 
                 binwidth = 3, color = "black", fill = cb_palette[4])+
  facet_wrap(district ~ year, scales = "free", ncol = 2)+
  labs(x = "Shell Height (mm)", y = "Density") -> x
ggsave(here("./figures/scallop_research/protandry", "shell_height_composition.png"), plot = x, 
       height = 6, width = 6, units = "in")

## bar chart of sex ratio at age
scal %>%
  filter(district %in% c("KSH", "YAK"),
         sex %in% c(1, 2),
         year < 2019,
         !is.na(age)) %>%
  # round up shell_height to nearest 5mm
  mutate(year = factor(year),
         age = ifelse(age > 18, 18, age)) %>%
  # compute proportion in each bin
  group_by(year, district, age) %>%
  summarise(Male = sum(sex == 1) / n(),
            Female = sum(sex == 2) / n(),
            n_samp = n()) %>%
  pivot_longer(c(Male, Female), values_to = "prop", names_to = "sex") %>%
  ggplot()+
  geom_bar(aes(x = age, y = prop, fill = sex), 
           stat = "identity", position = "dodge", 
           color = 1, width = 0.7)+
  geom_hline(yintercept = 0.5, linetype = 2)+
  scale_fill_manual(values = cb_palette[c(7, 3)])+
  facet_wrap(district ~ year, nrow = 3)+
  labs(x = "Age", y = "Proportion", fill = NULL) -> x
ggsave(here("./figures/scallop_research/protandry", "sex_ratio_age.png"), plot = x, height = 6, width = 6, units = "in")

# sex ratio in age composition
scal %>%
  filter(district %in% c("KSH", "YAK"),
         year <= 2018,
         samp_grp %in% c(1, 2)) %>%
  mutate(year = factor(year),
         age = ifelse(age > 18, 18, age)) %>%
  group_by(year, tow, district, bed_name, samp_grp, samp_cnt, age, sex) %>%
  summarise(count = n()) %>%
  group_by(tow, samp_grp) %>%
  mutate(n_shucked = sum(count)) %>%
  ungroup() %>%
  mutate(sample_factor = samp_cnt * (count / n_shucked)) %>%
  group_by(year, district) %>%
  mutate(tot_catch = sum(sample_factor, na.rm = T)) %>%
  group_by(year, district, age, sex, drop = F) %>%
  summarise(prop = sum(sample_factor, na.rm = T) / mean(tot_catch)) %>%
  filter(sex %in% c(1, 2)) %>%

  ggplot()+
  geom_bar(aes(x = age, y = prop, fill = factor(sex)), 
           stat = "identity", position = position_dodge(preserve = "single"), 
           color = 1, width = 0.7)+
  #geom_hline(yintercept = 0.5, linetype = 2)+
  scale_fill_manual(values = cb_palette[c(3, 7)], labels = c("Male", "Female"))+
  facet_wrap(district ~ year, nrow = 3, scales = "free")+
  labs(x = "Age", y = "Proportion of Catch", fill = NULL) -> x
ggsave(here("./figures/scallop_research/protandry", "sex_ratio_age_comp.png"), plot = x, height = 6, width = 6, units = "in")

# sex ratio in scatter plot by shell height in 5mm bins
scal %>%
  filter(district %in% c("KSH", "YAK"),
         sex %in% c(1, 2)) %>%
  # round up shell_height to nearest 5mm
  mutate(sh_bin = ceiling(shell_height / 5) * 5, 
         year = factor(year)) %>%
  # combine all bins above 160 mm (arbitrary) to avoid small sample size
  mutate(sh_bin = ifelse(sh_bin > 160, 160, sh_bin)) %>%
  # compute proportion male in each bin
  group_by(year, district, sh_bin) %>%
  summarise(prop_m = sum(sex == 1) / n(),
            n_samp = n()) %>%
  ggplot()+
  geom_point(aes(x = sh_bin, y = prop_m, color = year, size = n_samp))+
  geom_smooth(aes(x = sh_bin, y = prop_m, color = year), se = F, method = "gam")+
  geom_hline(yintercept = 0.5, linetype = 2)+
  labs(x = "Shell Height (mm)", y = "Proportion Male", color = "Year", size = "Sample Size")+
  scale_color_manual(values = cb_palette[2:6])+
  scale_x_continuous(breaks = seq(0, 200, 10))+
  facet_wrap(~district, ncol = 1)+
  theme(legend.box = "vertical", legend.margin = margin())-> x
ggsave(here("./figures/scallop_research/protandry", "sex_ratio_shell_height.png"),
       plot = x, height = 6, width = 5)

# sex ratio in sh composition
scal %>%
  filter(district %in% c("KSH", "YAK"),
         samp_grp %in% c(1, 2)) %>%
  # round up shell_height to nearest 5mm
  mutate(sh_bin = ceiling(shell_height / 5) * 5, 
         sh_bin = ifelse(sh_bin > 160, 160, sh_bin),
         year = factor(year)) %>%
  group_by(year, tow, district, bed_name, samp_grp, samp_cnt, sh_bin, sex) %>%
  summarise(count = n()) %>%
  group_by(tow, samp_grp) %>%
  mutate(n_shucked = sum(count)) %>%
  ungroup() %>%
  mutate(sample_factor = samp_cnt * (count / n_shucked)) %>%
  group_by(year, district) %>%
  mutate(tot_catch = sum(sample_factor, na.rm = T)) %>%
  group_by(year, district, sh_bin, sex, drop = F) %>%
  summarise(prop = sum(sample_factor, na.rm = T) / mean(tot_catch)) %>%
  filter(sex %in% c(1, 2)) %>%
  
  ggplot()+
  geom_bar(aes(x = sh_bin, y = prop, fill = factor(sex)), 
           stat = "identity", position = position_dodge(preserve = "single"), 
           color = 1, width = 3)+
  #geom_hline(yintercept = 0.5, linetype = 2)+
  scale_fill_manual(values = cb_palette[c(3, 7)], labels = c("Male", "Female"))+
  facet_wrap(district ~ year, nrow = 4, scales = "free")+
  labs(x = "Age", y = "Proportion of Catch", fill = NULL) -> x
ggsave(here("./figures/scallop_research/protandry", "sex_ratio_sh_comp.png"), plot = x, height = 7, width = 6, units = "in")

# table of sex ratio by cohort
scal %>%
  filter(district %in% c("KSH", "YAK"),
         sex %in% c(1, 2),
         !is.na(age)) %>%
  mutate(year = factor(year),
         age = ifelse(age > 18, 18, age)) %>%
  group_by(year, district) %>%
  mutate(n_shucked = n()) %>%
  group_by(year, district, age) %>%
  summarise(prop_male = sum(sex == 1, na.rm = T) / mean(n_shucked, na.rm = T)) %>%
  pivot_wider(names_from = year, values_from = prop_male) %>%
  arrange(district, age) %>%
  write_csv("./output/scallop_research/protandry/sex_ratio_by_age.csv")
  



