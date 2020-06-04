# notes ----
## CPUE standardization exploratory analysis
## focused on YAK district
## author: Tyler Jackson
## contact: tyler.jackson@alaska.gov
## date: 2020-2-10

## this script builds off a FISH 604 class project to examine CPUE standardization, with YAK district 
## as the 'test' data

# load ----

## functions used for model comparison
source("./code/cpue_standardization/gam_functions.R")

## define function for fitting smoother computing derivative (tidal index)
f_smooth_deriv <- function(x){
  spl <- smooth.spline(x$tide_level ~ x$t)
  x$tidal_rate <- predict(spl, deriv = 1)$y
  x
}

## multiplier on effective degrees of freedom, force smoother fit via penalization (Kim and Gu 2004, JRSS)
gamma <- 1.4

# data ----
# Observer catch data 2009 - 2018
## Catch by Haul from the Wiki
scallops <- do.call(bind_rows,
                    lapply(paste0("data/catch/", list.files("data/catch/")), read_csv))
# Yakutat Bay hourly tide level (annomally in ft from mean water level)
## NOAA Tides and Currents
tide <- read.table("./data/environmental/yakutat bay tide 2009 - 2019/yakutat_tide_predictions_jan2009_dec2019.txt",
                   header = T, skip = 13)

# Cap Suckling Data Buoy
## National Data Buoy Center
buoy <- do.call(bind_rows,
                lapply(paste0("data/environmental/cape suckling buoy/", 
                              list.files("data/environmental/cape suckling buoy")), read_csv))


## rename scallop data fields for consistency
names(scallops) <- c("Fishery", "District", "ADFG", "Trip_ID", "Haul_ID", 
                     "haul", "gear_perf", "haul_sampled", "Set_date", "set_lat",
                     "set_lon", "statarea", "depth", "dredge_count", 
                     "dredge_width", "dredge_down", "dredge_up", "duration", 
                     "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                     "rtnd_basket", "round_weight", "meat_weight", "est_yield",
                     "tot_rtnd_basket", 'tot_day_meat_weight')

## Scallop data manipulation
## adjust dates and set variables
## remove data where dredge hours & area swept are zero
## calculate round weight cpue by time
## calculate meat weight cpue by time
## only include Yakutat Area districts (combined ~2018)
## add bed 
## retain only complete data
scallops %>%
  mutate(set_date = mdy(Set_date),
         year = year(set_date),
         date = yday(set_date),
         set_hr = hour(dredge_down),
         year = ifelse(date < 100, year - 1, year), # fishing year
         Year = factor(year),
         Vessel = factor(ADFG),
         FY = paste0(year, "/", sprintf('%02d', (year + 1) %% 100)),
         dum = 1) %>%
  filter(area_swept>0) %>%
  mutate(rw_cpue = round_weight / dredge_hrs,
         mw_cpue = meat_weight / dredge_hrs,
         bed = case_when(set_lon <= -138.3  & set_lon > -138.79 ~ 5,
                         set_lon <= -138.79 & set_lon > -140.1  ~ 4,
                         set_lon <= -140.1  & set_lon > -142.15 ~ 3,
                         set_lon <= -142.15 & set_lon > -142.7  ~ 2,
                         set_lon <= -142.7  & set_lon > -143.5  ~ 1,
                         set_lon <= -143.5 ~ 0,
                         TRUE ~ 6),
         Bed = factor(bed)) %>%
  filter(District %in% c("YAK", "D16", "D")) %>%
  .[complete.cases(.),] -> scallops

## Tide data manipulation
## change filed names
## separate date into year, month, day, hr
## nest with day (to improve smoother resolution)
## fit a smoother using a cubic spline and retrun the derivative (proxy for index tidal current intensity)
## join to scallop data
tide %>%
  as_tibble %>%
  mutate(set_date = ymd(Date), 
         year = year(set_date),
         month = month(set_date),
         day = day(set_date),
         set_hr = as.numeric(substring(Time, 1, 2))) %>%
  arrange(set_date) %>%
  mutate(t = 1:nrow(.)) %>%
  rename(tide_level = Pred) %>%
  select(5:10, 4) %>%
  nest(-set_date) %>%
  mutate(data = purrr::map(data, f_smooth_deriv)) %>%
  unnest(data) %>%
  mutate(tidal_rate = abs(tidal_rate),
         set_hr = set_hr) -> tide 

## Buoy data manipulation
## filter out superfluous rows
## create set_date
## rename fields
## change missing data to NA
## covert wave height to ft
## join with scallop data
buoy %>%
  filter(complete.cases(.)) %>%
  mutate(set_date = paste0(`#YY`, "-",MM, "-", DD),
         set_date = ymd(set_date),
         set_hr = hh) %>%
  select(set_date, set_hr, WDI, `R WSP`, WVHT) %>%
  rename(wind_dir = WDI, 
         wind_sp = `R WSP`,
         wave_ht = WVHT) %>%
  mutate(wind_dir = ifelse(wind_dir == 999, NA, wind_dir),
         wind_sp = ifelse(wind_sp == 99, NA, wind_sp),
         wave_ht = ifelse(wave_ht == 99, NA, wave_ht),
         wave_ht = wave_ht * 3.28084) -> buoy

## join scallop, tide, and buoy data
buoy %>%
  right_join(scallops, by = c("set_date", "set_hr")) %>%
  left_join(tide, by = c("set_date", "set_hr")) %>%
  select(-year.y, -month, -day, -t) -> scallops_comb

## check for missing buoy data
scallops_comb %>%
  filter(is.na(wave_ht)) %>%
  count(Year, month(set_date))

## compute the month average weather conditions
buoy %>%
  mutate(month = month(set_date)) %>%
  group_by(month) %>%
  summarize(wind_dir_avg = mean(wind_dir, na.rm = T),
            wind_sp_avg = mean(wind_sp, na.rm=T),
            wave_ht_avg = mean(wave_ht, na.rm = T)) -> mon.avg

## replace missing weather data with the long term monthly average
scallops_comb %>%
  mutate(month = month(set_date)) %>%
  left_join(mon.avg, by = "month") %>%
  mutate(wind_dir = ifelse(is.na(wind_dir), wind_dir_avg, wind_dir),
         wind_sp = ifelse(is.na(wind_sp), wind_sp_avg, wind_sp),
         wave_ht = ifelse(is.na(wave_ht), wave_ht_avg, wave_ht),
         month = factor(month, levels = c(7:12, 1, 2))) %>%
  rename(Month = month) %>%
  select(-wind_dir_avg, -wind_sp_avg, -wave_ht_avg) -> scallops_comb


## dump dredges that were not satisfactory
scallops_comb %>%
  filter(gear_perf == 1) -> scallops_comb

## correlogram of continuous variables
#scallops_comb %>%
  #select(depth, wind_sp, wave_ht, tidal_rate, haul_speed, area_swept, rw_cpue) %>%
  #GGally::ggcorr(label = T, label_round = 2) -> x
#ggsave("./figures/cpue_standarization/continuous_vars_corr.png", 
      # plot = x, 
      # width = 7, height = 5, units = "in")



# model fitting, GAMs with gamma distribution ----

## define cpue adjustment as 10% of the all time average (CPUE must be non-zero for gamma distribution)
adj <- mean(scallops_comb$rw_cpue, na.rm = T) * 0.10

## Fit a "full" model that includes:
### smoothed depth, wave height, wind speed, haul speed, tidal rate
### tensor product of lat and lon, by bed
### parametric interaction between bed and year, and vessel
m_g0 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht) + s(wind_sp) + s(haul_speed) + s(tidal_rate) + 
               te(set_lon, set_lat, by = Bed) + Year * Bed + Month + Vessel, data = scallops_comb, gamma = gamma, 
             family=Gamma(link=log), select = T, subset = haul_speed >= 4)
summary(m_g0)
### partial effect plots of depth by bed
print(plot(getViz(m_g0), select = c(1:7)), pages = 1)
ggsave("./figures/cpue_standarization/pe_depth_full_mod_gamma.png", 
       plot = print(plot(getViz(m_g0), select = c(1:7)), pages = 1), 
       width = 8, height = 5, units = "in")

### look at partial effects of wave height, wind speed, haul speed, and tidal rate separately
print(plot(getViz(m_g0), select = 8:11) + l_points(alpha = 0.5, color = "grey 60") + l_fitLine() + l_ciLine(), pages = 1) 
ggsave("./figures/cpue_standarization/pe_env_full_mod_gamma.png", 
       plot = print(plot(getViz(m_g0), select = 8:11) + l_points(alpha = 0.5, color = "grey 60") + 
                      l_fitLine() + l_ciLine(), pages = 1), 
       width = 5, height = 5, units = "in")

### look at partial effects of tensor product of lat and lon
print(plot(getViz(m_g0), select = 12:18, too.far = 0.05) + l_fitRaster() + l_fitContour() + l_points(), pages = 1)
ggsave("./figures/cpue_standarization/pe_telatlon_full_mod_gamma.png", 
       plot = print(plot(getViz(m_g0), select = 12:18, too.far = 0.05) + l_fitRaster() + l_fitContour() + l_points(), pages = 1), 
       width = 12, height = 6, units = "in")

### look at partial effects of discrete variables
plot = print(plot(getViz(m_g0), select = 19:22), pages = 1)
ggsave("./figures/cpue_standarization/discrete_covariates_full_mod_gamma.png", 
       plot = print(plot(getViz(m_g0), select = 19:22), pages = 1), 
       width = 9, height = 5, units = "in")

### diagnostics
gam.check(m_g0)
plot(x = m_g0$linear.predictors, y = m_g0$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth by bed, wave height, longitude by bed
### parametric year, bed, month and vessel
m_g1 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht) + s(set_lon, by = Bed) + Month + Vessel + Bed * Year, 
            data = scallops_comb, gamma = gamma, family=Gamma(link=log), select = T)
summary(m_g1)

### partial effect plots
print(plot(getViz(m_g1), allTerms = T), pages = 1)

### partial effect plot of just s(set_lon)
print(plot(getViz(m_g1), select = c(9:15)), pages = 1)
ggsave("./figures/cpue_standarization/smooth_lon_red_mod_gamma.png", 
       plot = print(plot(getViz(m_g1), select = c(9:15)), pages = 1), 
       width = 12, height = 6, units = "in")
### diagnostics
gam.check(m_g1) # there are several outlier linear predictors
plot(x = m_g1$linear.predictors, y = m_g1$residuals, xlim = c(5.5, 7.5))


## Fit the a reduced model indentical to m_g1, but with Vessel as a random effect
m_g1.1 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht) + s(set_lon, by = Bed) + 
              s(Vessel, bs = "re") + Month + Bed * Year, 
            data = scallops_comb, gamma = gamma, family=Gamma(link=log), select = T)
summary(m_g1.1)
### random effects of Vessel
print(plot(getViz(m_g1.1), select = 16), pages = 1)
### diagnostics
gam.check(m_g1.1) # not really any different that m_g1



## Fit the a reduced model that includes:
### smoothed depth by bed, wave height
### parametric year, bed, month, and vessel
m_g2 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht) + Year * Bed + Month + Vessel, 
            data = scallops_comb, gamma = gamma, family=Gamma(link=log), select = T)
summary(m_g2)

### partial effect plots
print(plot(getViz(m_g2), allTerms = T), pages = 1)

### diagnostics
gam.check(m_g2) # there are several outlier linear predictors
plot(x = m_g2$linear.predictors, y = m_g2$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth, wave height
### parametric year, bed, month, and vessel
m_g3 <- bam(rw_cpue + adj ~ s(depth, k = 4) + s(wave_ht) + Year * Bed + Month + Vessel, 
            data = scallops_comb, gamma = gamma, family=Gamma(link=log), select = T)
summary(m_g3)

### partial effect plots
print(plot(getViz(m_g3), allTerms = T) + l_points(alpha = 0.5, color = "grey 60") + 
        l_fitLine() + l_ciLine(), pages = 1)

### diagnostics
gam.check(m_g3) # there are several outlier linear predictors
plot(x = m_g3$linear.predictors, y = m_g3$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth, wave height
### parametric year, bed, month, and vessel, no bed /  year interaction
m_g4 <- bam(rw_cpue + adj ~ s(depth, k = 4) + s(wave_ht) + Year + Bed + Month + Vessel, 
            data = scallops_comb, gamma = gamma, family=Gamma(link=log), select = T)
summary(m_g4)

### partial effect plots
print(plot(getViz(m_g4), allTerms = T) + l_points(alpha = 0.5, color = "grey 60") + 
        l_fitLine() + l_ciLine(), pages = 1)

### diagnostics
gam.check(m_g4) # there are several outlier linear predictors
plot(x = m_g4$linear.predictors, y = m_g4$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth by bed
### parametric year, bed, month, and vessel
m_g5 <- bam(rw_cpue + adj ~ s(depth, k = 4) + Year * Bed + Month + Vessel, 
            data = scallops_comb, gamma = gamma, family=Gamma(link=log), select = T)
summary(m_g5)

### partial effect plots
print(plot(getViz(m_g5), allTerms = T) + l_points(alpha = 0.5, color = "grey 60") + 
        l_fitLine() + l_ciLine(), pages = 1)

### diagnostics
gam.check(m_g5) # there are several outlier linear predictors
plot(x = m_g5$linear.predictors, y = m_g5$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth by bed and smooth longitude by bed
### parametric year, bed, month, and vessel
m_g6 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(set_lon, by = Bed) + Month + Vessel + Year * Bed, 
            data = scallops_comb, gamma = gamma, family=Gamma(link=log), select = T)
summary(m_g6)

### partial effect plots
print(plot(getViz(m_g6), allTerms = T) + l_points(alpha = 0.5, color = "grey 60") + 
        l_fitLine() + l_ciLine(), pages = 1)

### diagnostics
gam.check(m_g6) # there are several outlier linear predictors
plot(x = m_g6$linear.predictors, y = m_g6$residuals, xlim = c(5.5, 7.5))


# model comparison, GAMs with gamma distribution ----

## create list of models
mget(ls(pattern = "m_g")) %>%
  tibble(gam = .) %>%
  mutate(name = names(gam)) -> model_comp

## select only models we wish to compare
## add estimaed df, GCV score, AIC, and delta AIC
model_comp %>%
  mutate(edf = map_dbl(gam, f_edf),
         dev_exp = map_dbl(gam, f_dev),
         gcv = map_dbl(gam, f_gcv),
         BIC = map_dbl(gam, BIC),
         loglik = purrr::map(gam, logLik.gam),
         AIC = map_dbl(loglik, AIC),
         delta_AIC = AIC - min(AIC)) %>%
  arrange(AIC) 


# model fitting, GAMs with log-normal distribution ----

## define cpue adjustment as 10% of the all time average (CPUE must be non-zero for lognormal distribution)
adj <- mean(scallops_comb$rw_cpue, na.rm = T) * 0.10

## Fit a "full" model that includes:
### smoothed depth, wave height, wind speed, haul speed, tidal rate
### tensor product of lat and lon, by bed
### parametric interaction between bed and year, and vessel
m_ln0 <- bam(log(rw_cpue + adj) ~ s(depth, k = 4, by = Bed) + s(wave_ht) + s(wind_sp) + s(haul_speed) + s(tidal_rate) + 
              te(set_lon, set_lat, by = Bed) + Year * Bed + Month + Vessel, data = scallops_comb, gamma = gamma, 
            family=gaussian, select = T, subset = haul_speed >= 4)
summary(m_ln0)
### partial effect plots of depth by bed
print(plot(getViz(m_ln0), select = c(1:7)), pages = 1)

### look at partial effects of wave height, wind speed, haul speed, and tidal rate separately
print(plot(getViz(m_ln0), select = 8:11) + l_points(alpha = 0.5, color = "grey 60") + l_fitLine() + l_ciLine(), pages = 1) 

### look at partial effects of tensor product of lat and lon
print(plot = print(plot(getViz(m_ln0), select = 12:18) + l_fitRaster() + l_fitContour() + l_points(), pages = 1))

### look at partial effects of discrete variables
plot = print(plot(getViz(m_ln0), select = 19:22), pages = 1)

### diagnostics
gam.check(m_ln0)
plot(x = m_ln0$linear.predictors, y = m_ln0$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth by bed, wave height, longitude
### parametric year, bed, month and vessel
m_ln1 <- bam(log(rw_cpue + adj) ~ s(depth, k = 4, by = Bed) + s(wave_ht) + s(set_lon, by = Bed) + Year * Bed + Month + Vessel, 
            data = scallops_comb, gamma = gamma, family = gaussian, select = T)
summary(m_ln1)

### partial effect plots
print(plot(getViz(m_ln1), allTerms = T), pages = 1)

### partial effect plot of just s(set_lon)
print(plot(getViz(m_ln1), select = 9) + l_points(alpha = 0.5, color = "grey 60") + l_fitLine() + l_ciLine(), pages = 1)

### diagnostics
gam.check(m_ln1) # there are several outlier linear predictors
plot(x = m_ln1$linear.predictors, y = m_ln1$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth by bed, wave height
### parametric year, bed, month, and vessel
m_ln2 <- bam(log(rw_cpue + adj) ~ s(depth, k = 4, by = Bed) + s(wave_ht) + Year * Bed + Month + Vessel, 
            data = scallops_comb, gamma = gamma, family = gaussian, select = T)
summary(m_ln2)

### partial effect plots
print(plot(getViz(m_ln2), allTerms = T), pages = 1)

### diagnostics
gam.check(m_ln2) # there are several outlier linear predictors
plot(x = m_ln2$linear.predictors, y = m_ln2$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth, wave height
### parametric year, bed, month, and vessel
m_ln3 <- bam(log(rw_cpue + adj) ~ s(depth, k = 4) + s(wave_ht) + Year * Bed + Month + Vessel, 
            data = scallops_comb, gamma = gamma, family = gaussian, select = T)
summary(m_ln3)

### partial effect plots
print(plot(getViz(m_ln3), allTerms = T) + l_points(alpha = 0.5, color = "grey 60") + 
        l_fitLine() + l_ciLine(), pages = 1)

### diagnostics
gam.check(m_ln3) # there are several outlier linear predictors
plot(x = m_ln3$linear.predictors, y = m_ln3$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth, wave height
### parametric year, bed, month, and vessel, no bed /  year interaction
m_ln4 <- bam(log(rw_cpue + adj) ~ s(depth, k = 4) + s(wave_ht) + Year + Bed + Month + Vessel, 
            data = scallops_comb, gamma = gamma, family = gaussian, select = T)
summary(m_ln4)

### partial effect plots
print(plot(getViz(m_ln4), allTerms = T) + l_points(alpha = 0.5, color = "grey 60") + 
        l_fitLine() + l_ciLine(), pages = 1)

### diagnostics
gam.check(m_ln4) # there are several outlier linear predictors
plot(x = m_ln4$linear.predictors, y = m_ln4$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth by bed
### parametric year, bed, month, and vessel
m_ln5 <- bam(log(rw_cpue + adj) ~ s(depth, k = 4) + Year * Bed + Month + Vessel, 
            data = scallops_comb, gamma = gamma, family = gaussian, select = T)
summary(m_ln5)

### partial effect plots
print(plot(getViz(m_ln5), allTerms = T) + l_points(alpha = 0.5, color = "grey 60") + 
        l_fitLine() + l_ciLine(), pages = 1)

### diagnostics
gam.check(m_ln5) # there are several outlier linear predictors
plot(x = m_ln5$linear.predictors, y = m_ln5$residuals, xlim = c(5.5, 7.5))



## Fit the a reduced model that includes:
### smoothed depth by bed
### parametric year, bed, month, and vessel
m_ln6 <- bam(log(rw_cpue + adj) ~ s(depth, k = 4, by = Bed) + s(set_lon, by = Bed) + Month + Vessel + Year * Bed, 
            data = scallops_comb, gamma = gamma, family = gaussian, select = T)
summary(m_ln6)

### partial effect plots
print(plot(getViz(m_ln6), allTerms = T) + l_points(alpha = 0.5, color = "grey 60") + 
        l_fitLine() + l_ciLine(), pages = 1)

### diagnostics
gam.check(m_ln6) # there are several outlier linear predictors
plot(x = m_ln6$linear.predictors, y = m_ln6$residuals, xlim = c(5.5, 7.5))





# model comparison, GAMs with log-normal distribution ----

## create list of models
mget(ls(pattern = "m_ln")) %>%
  tibble(gam = .) %>%
  mutate(name = names(gam)) -> model_comp

## select only models we wish to compare
## add estimaed df, GCV score, AIC, and delta AIC
model_comp %>%
  mutate(edf = map_dbl(gam, f_edf),
         dev_exp = map_dbl(gam, f_dev),
         gcv = map_dbl(gam, f_gcv),
         BIC = map_dbl(gam, BIC),
         loglik = purrr::map(gam, logLik.gam),
         AIC = map_dbl(loglik, AIC),
         delta_AIC = AIC - min(AIC)) %>%
  arrange(AIC) 







# comparision of error distributions ----

## residuals vs fitted values
tibble(resid = residuals(m_g1, type = "response"),
       fit = predict(m_g1, type = "response")) %>%
  mutate(dist = "Gamma") %>%
  bind_rows(tibble(resid = scallops_comb$rw_cpue + adj - exp(predict(m_ln1, type = "response")),
                   fit = exp(predict(m_ln1, type = "response"))) %>%
              mutate(dist = "Log-normal")) -> tmp
### mean and sd by distribution  
tmp %>%
  group_by(dist) %>%
  summarise(mean = mean(resid),
            sd = sd(resid))
### plot
tmp %>%
  ggplot()+
  geom_point(aes(x = fit, y = resid), alpha = 0.2, color = "grey40")+
  geom_hline(yintercept = c(-2, 0, 2), linetype = 2)+
  labs(x = "Fitted values", y = "Residuals")+
  facet_wrap(~dist, ncol = 1, scales = "free") -> x
ggsave("./figures/cpue_standarization/resid_fitted.png", 
       plot = x,
       width = 3, height = 4, units = "in")



## response vs fitted values
tibble(resp = scallops_comb$rw_cpue,
       fit = predict(m_g1, type = "response") - adj) %>%
  mutate(dist = "Gamma") %>%
  bind_rows(tibble(resp = scallops_comb$rw_cpue,
                   fit = exp(predict(m_ln1, type = "response")) - adj) %>%
              mutate(dist = "Log-normal")) -> tmp
### pearson correlation
tmp %>%
  group_by(dist) %>%
  summarise(cor = cor(resp, fit, method = "pearson"))
###plot  
tmp %>%
  ggplot()+
  geom_point(aes(x = fit, y = resp), alpha = 0.2, color = "grey40")+
  geom_smooth(aes(x = fit, y = resp), se = F, method = "loess")+
  geom_line(aes(x = resp, y = resp), linetype = 2)+
  #geom_line(aes(x = resp, y = resp + 500), linetype = 2, size = 0.25)+
  #geom_line(aes(x = resp, y = resp - 500), linetype = 2, size = 0.25)+
  labs(x = "Fitted values", y = "Response values")+
  scale_x_continuous(limits = c(0, 2000))+
  scale_y_continuous(limits = c(0, 2000))+
  facet_wrap(~dist, ncol = 1) -> x
ggsave("./figures/cpue_standarization/response_fitted.png", 
       plot = x,
       width = 3, height = 4, units = "in")



## fitted values gamma vs fitted values log-normal
tibble(fit_g = predict(m_g1, type = "response") - adj,
       fit_ln = exp(predict(m_ln1, type = "response")) - adj,
       lp_g = m_g1$linear.predictors,
       lp_ln = m_ln1$linear.predictors) %>%
  ggplot()+
  geom_point(aes(x = fit_g, y = fit_ln), alpha = 0.2, color = "grey40")+
  geom_smooth(aes(x = fit_g, y = fit_ln), se = F, method = "lm")+
  geom_line(aes(x = fit_g, y = fit_g), linetype = 2)+
  geom_line(aes(x = fit_ln, y = 0), linetype = 2, size = 0.25)+
  labs(x = "Fitted values (Gamma)", y = "Fitted values (Log-normal)") -> x
ggsave("./figures/cpue_standarization/fitted_gamma_lognormal.png", 
       plot = x,
       width = 3, height = 3, units = "in")

## Extract the partial effect of year in both gamma and log-normal models
##
tibble(Year = scallops_comb$Year,
       ypred_g = predict(m_g1, type = "terms")[,1],
       ypred_ln = predict(m_ln1, type = "terms")[,1],
       rw_g = residuals(m_g1, type = "working"),
       rw_ln = residuals(m_ln1, type = "working"),
       Gamma = ypred_g + rw_g,
       `Log-normal` = ypred_ln + rw_ln) %>%
  select(Year, Gamma, `Log-normal`) %>%
  pivot_longer(c("Gamma", "Log-normal"), names_to = "dist", values_to = "year_effect") %>%
  group_by(Year, dist) %>%
  summarise(mean = mean(year_effect),
            se = sd(year_effect) / sqrt(n()),
            l95 = mean - 1.96 * se,
            u95 = mean + 1.96 * se) %>%
  ggplot()+
  geom_point(aes(x = Year, y = mean, color = dist, group = dist))






# comparison of standardized cpue with nominal cpue ----
## standardized cpue models: m_g1 (Reduced w/ wave_ht), m_g6 (Reduced w/o wave_ht)

## nominal cpue by year
scallops_comb %>%
  group_by(Year) %>%
  summarise(cpue_nom = sum(round_weight) / sum(dredge_hrs)) -> cpue_nom

## standardized cpue by year
#expand grid for factor variables and wave height
expand_grid(Year = unique(scallops_comb$Year), 
            Month = unique(scallops_comb$Month),
            Bed = unique(scallops_comb$Bed),
            Vessel = unique(scallops_comb$Vessel),
            wave_ht = mean(scallops_comb$wave_ht, na.rm = T)) %>% 
  #set depth as mode of depth, and set_lon as mean of set_lon
  left_join(scallops_comb %>%
              group_by(Bed) %>%
              summarise(depth = mode(depth),
                        set_lon = mean(set_lon)),
            by = c("Bed")) %>% 
  #add weights for averaging cpue
  left_join(scallops_comb %>%
              group_by(Year, Bed, Month, Vessel) %>%
              summarise(n = n()) %>%
              group_by(Year) %>%
              mutate(w = n / sum(n, na.rm = T)),
            by = c("Year", "Bed", "Month", "Vessel")) %>% 
  replace_na(list(n = 0, w = 0)) %>%
  #add fitted values from models
  mutate(fit_m_g1 = predict(m_g1, newdata = ., type = "response") - adj,
         fit_m_g6 = predict(m_g6, newdata = ., type = "response") - adj) %>%
#take the median weighted by proportion of effort allocated to each factor level within year to acheive standardized cpue 
  group_by(Year) %>%
  summarise(fit_m_g1 = spatstat::weighted.median(fit_m_g1, w = w),
            fit_m_g6 = spatstat::weighted.median(fit_m_g6, w = w)) %>%
  #join with nominal cpue estimates 
  full_join(cpue_nom, by = "Year") -> cpue_stand

## plot nominal cpue and standardized cpue trend
cpue_stand %>%
  pivot_longer(c(2:4), names_to = "type", values_to = "cpue") %>%
  ggplot()+
  geom_point(aes(x = Year, y = cpue, shape = type, color = type))+
  geom_line(aes(x = Year, y = cpue, group = type, linetype = type, color = type))+
  labs(x = NULL, linetype = NULL, shape = NULL, color = NULL, y = "Round Weight CPUE (lbs / dregde hr)")+
  scale_shape_manual(values = c(16, 3, 4), 
                     labels = c("Nominal CPUE", "Standardized CPUE (w/ wave ht)", "Standardized CPUE (w/o wave ht)"))+
  scale_color_manual(values = c("black", "grey40", "grey40"), 
                     labels = c("Nominal CPUE", "Standardized CPUE (w/ wave ht)", "Standardized CPUE (w/o wave ht)"))+
  scale_linetype_manual(values = c(1:3),
                        labels = c("Nominal CPUE", "Standardized CPUE (w/ wave ht)", "Standardized CPUE (w/o wave ht)")) -> x
ggsave("./figures/cpue_standarization/nom_vs_stnd_cpue.png", plot = x, width = 8, height = 3, units = "in")
  
