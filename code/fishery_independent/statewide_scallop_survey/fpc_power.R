# notes ----
## power analysis for dredge fishing power difference
## tyler jackson
## tyler.jackson@alaska.gov

# load ----

### functions for survey data
source("./code/misc/statewide_scallop_survey_functions.R")

# data ----

## logbook data
logbook <- read.csv("./data/statewide_scallop_survey/logbook/survey_log_ts_temporary.csv") 

## catch data
catch_raw <- read.csv("./data/statewide_scallop_survey/catch/survey_catch_ts_temporary.csv") 

## tows
tows <- f_clean_log(logbook) 
catch <- f_catch_by_tow(catch_raw, tows)

# power analysis using 2020 ksh large scallop data ----

f_analysis <- function(sample_size, fpd, mu, var, jitter_sd){
  
  # simulate cpue from standard dredge
  std = exp(rnorm(sample_size, mean = mu, sd = sqrt(var)))
  # jitter data and apply fpd to get new dredge
  new = sapply((std + std * rnorm(length(std), sd = jitter_sd)) * fpd, function(x){max(x, 0)})
  
  # set up data and test for fishing power correction
  tibble(dredge = "std",
         cpue = std,
         pair = factor(1:length(std))) %>%
    bind_rows(tibble(dredge = "new",
                     cpue = new,
                     pair = factor(1:length(new)))) %>%
    mutate(dredge = factor(dredge, levels = c("new", "std"))) -> x
  
  # set linear model contrasts appropriately
  options(contrasts = rep("contr.sum", 2))
  
  # fit model
  lm(log(cpue + 1) ~ pair + dredge, data = x) -> fit
  coefs <- summary(fit)[["coefficients"]]
  # old dredge treatment effect
  nu = coefs[nrow(coefs), 1] 
  nu_se = coefs[nrow(coefs), 2] 
  # fpc
  fpc = exp(2 * nu * (1 + (0.5 * nu_se^2))) 
  l95 = exp((2 * nu) + (qnorm(0.025) * 2 * nu_se))
  u95 = exp((2 * nu) + (qnorm(0.975) * 2 * nu_se))   
  
  # test if FPC includes 1
  sig = 1 - (1 < u95 & 1 > l95)
  
  return(sig)
  
}

# get sample data from large scallops catches in 2020 to based simulations on
catch %>%
  filter(samp_grp == 1,
         year == 2020) -> sample_data

# do the power analysis
expand_grid(iteration = 1:1000,
            sample_size = c(10, 20, 30, 40, 50, 70, 80, 100),
            fpd = seq(0.1, 0.9, 0.1),
            jitter_sd = c(0.1, 0.8),
            # lognormal mean and variance of sample data
            # parameters for generating simulated data
            mu = log(mean(sample_data$cpue_cnt)) - 0.5*log(1+(sd(sample_data$cpue_cnt)/mean(sample_data$cpue_cnt))^2),
            var = log(1+(sd(sample_data$cpue_cnt)/mean(sample_data$cpue_cnt))^2)) %>%
  # run analysis
  ## simulate data
  ## estimate fpc, test if its different than 1
  mutate(fpc_sig = purrr::pmap_dbl(list(sample_size, fpd, jitter_sd, mu, var), f_analysis)) -> out
# save huge data object
saveRDS(out, "./code/scallop_research/fpc_power.RDS")


readRDS("./code/scallop_research/fpc_power.RDS") %>%
  #filter(jitter_sd == 0.75) %>%
  group_by(sample_size, fpd, jitter_sd) %>%
  summarise(power = sum(fpc_sig) / n()) %>%
  pivot_wider(names_from = jitter_sd, values_from = "power") %>%
  group_by(sample_size) %>%
  ggplot()+
  geom_ribbon(aes(x = -fpd, ymin = `0.1`, ymax = `0.8`, fill = factor(sample_size)), alpha = 0.3)+
  scale_x_continuous(breaks = seq(-0.9, -0.1, 0.1), labels = seq(0.9, 0.1, -0.1),
                     limits = c(-0.9,-0.1), expand = c(0,0))+
  
  labs(x = "Fishing Power Difference", y = "Statistical Power", fill = "Number of Tows") -> plot

ggsave("./figures/scallop_research/fishing_power/fpc_power.png", plot = plot,
       height = 4, width = 5, unit = "in")



