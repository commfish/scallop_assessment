# notes ----
## Estimate inputs for Kamishak CASA (sea scallop) assessment
## Tyler Jackson

# load ----
library(tidyverse)
library(bbmle)
library(r4ss)

# data from stock synthesis ---- 
ss_dat <- SS_readdat("./code/model_development/stock_synthesis/kamishak/kam_v3.30/kam_ssv3.30_dat.ss")

# estimate LvB pars ----

## ages 
read_csv("./data/statewide_scallop_survey/ages/2016-2018_scallop_survey_ages.csv") %>%
  rename_all(tolower) -> ages

## shell heights
read_csv("./data/statewide_scallop_survey/specimen/awl_180719.csv") %>%
  rename_all(tolower) -> sh

## haul info
read_csv("./data/statewide_scallop_survey/logbook/events_180719.csv") %>%
  rename_all(tolower) -> hauls

## join data 
sh %>%
  dplyr::select(event_id, scallop_number, shell_height_mm) %>%
  rename(shell_num = scallop_number) %>%
  # join to age
  right_join(ages, by = c("event_id", "shell_num")) %>%
  # join to district
  left_join(hauls %>% dplyr::select(event_id, district), by = "event_id") %>%
  # dump unneeded columns
  dplyr::select(year, district, age, shell_height_mm) %>%
  rename(sh = shell_height_mm) -> sh_age

## create LvB function
f_LVB <- function(data, Linf, k, t0, age_name = "age"){
  
  # pull age from data
  if(!is.vector(data)){
    age = pull(data, age_name)
  } else{age = data}
  
  # predict function
  La = Linf * (1 - exp(-k * (age - t0)))
  
  # output
  return(La)
}
## create NLL function
f_NLL <- function(data, ln_Linf, ln_k, t0, ln_sigma, age_name = "age", length_name = "sh"){
  
  # pull age and observed length from data
  age = pull(data, age_name)
  obs_len = pull(data, length_name)
  
  # exponentiate lvb parameters
  Linf = exp(ln_Linf)
  k = exp(ln_k)
  
  # predict length
  pred_len = f_LVB(data = data, Linf = Linf, k = k, t0 = t0)
  
  # compute NLL
  nll = -sum(dnorm(x = log(obs_len + 1e-6), mean = log(pred_len + 1e-6), sd = exp(ln_sigma), log = TRUE))
  
  # output
  return(nll)
}
## wrapper for mle2 function so it works better in purrr
f_mle2 <- function(data, .fun, start){
  
  # coerce data to list
  data_mle2 = list(data = data)
  
  # optimizer
  fit = bbmle::mle2(.fun,
                    start = start,
                    data = data_mle2,
                    method = "Nelder-Mead",
                    optimizer = "nlminb",
                    control = list(maxit=1e6))
  
  # output
  return(fit)
  
}

sh_age %>%
  filter(!is.na(age),
         !is.na(sh),
         district == "KSH") %>%
  f_mle2(data = ., f_NLL, start = list(ln_Linf = log(150), ln_k = log(0.3), t0 = -1, 
                                       ln_sigma = log(0.2))) %>%
  coef %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate_at(1:2, exp) %>%
  dplyr::select(-ln_sigma) %>%
  rename_all(~c("L_inf", "k", "t0"))

# weight at length parameters ----

sh %>%
  dplyr::select(event_id, scallop_number, shell_height_mm, whole_wt_grams) %>%
  rename(shell_num = scallop_number) %>%
  # join to district
  left_join(hauls %>% dplyr::select(event_id, district), by = "event_id") %>%
  # filter for KSH
  filter(district == "KSH") -> wal_data
  
lm(log(whole_wt_grams) ~ log(shell_height_mm), data = wal_data) -> wal

a = coef(wal)[1]
b = coef(wal)[2]





# first size comp ----

ss_dat$lencomp [1, -1:-6] %>%
  t() %>% as.numeric() -> tmp
round(tmp / sum(tmp), 4)

# commercial landings ----
ss_dat$catch[c(2:nrow(ss_dat$catch)),] %>%
  dplyr::select(year, catch) %>%
  mutate(blank = 0,
         blank2 = 1)
# fishery cpue ----
ss_dat$CPUE
# estimate Rdevs ----
ss_dat$lencomp %>%
  as_tibble() %>%
  filter(FltSvy == 1) %>%
  dplyr::select(1, 7:24) %>%
  group_by(Yr) %>%
  nest() %>%
  mutate(sum = purrr::map_dbl(data, rowSums)) %>%
  left(join(tibble))
  pull(sum) %>%
  scale %>%
  round(., 3) %>%
  as.numeric()

         