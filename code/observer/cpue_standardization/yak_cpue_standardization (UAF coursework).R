# notes ----
# CPUE standardization in YAK district
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# date: 2019-11-13

# load ----

source("./code/gam_functions.R")

### define function for fitting smoother computing derivative (tidal index)
f_smooth_deriv <- function(x){
  spl <- smooth.spline(x$tide_level ~ x$t)
  x$tidal_rate <- predict(spl, deriv = 1)$y
  x
}

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
         wave_ht = ifelse(is.na(wave_ht), wave_ht_avg, wave_ht)) %>%
  select(-month, -wind_dir_avg, -wind_sp_avg, -wave_ht_avg) -> scallops_comb


## dump dredges that were not satisfactory
scallops_comb %>%
  filter(gear_perf == 1) -> scallops_comb

# eda ----

## correlation of all relevant continuous variables
scallops_comb %>%
  select(depth, wind_dir, wind_sp, wave_ht, tidal_rate, tide_level, haul_speed, area_swept, rw_cpue) %>%
  GGally::ggpairs()

## view interannual trend in mean rw_cpue by bed
scallops_comb %>%
  group_by(Bed, Year) %>%
  summarize(rw_cpue = mean(rw_cpue, na.rm=T)) %>%
  ggplot()+
  geom_point(aes(x = Year, y = rw_cpue, col = Bed))+
  geom_line(aes(x = Year, y = rw_cpue, group = Bed, col = Bed))

## view differences in rw_cpue by vessel and bed within year
scallops_comb %>%
  ggplot()+
  geom_boxplot(aes(x = Vessel, y = rw_cpue, col = Bed))+
  facet_wrap(~Year)

## view relationship between rw_cpue and latitdue, longitude
scallops_comb %>%
  ggplot()+
  geom_point(aes(x = set_lat, y = rw_cpue, color = Bed))+
  labs(x = "Latitude", y = "Round Weight CPUE") -> p_lat
scallops_comb %>%
  ggplot()+
  geom_point(aes(x = set_lon, y = rw_cpue, color = Bed))+
  labs(x = "Longitude", y = "Round Weight CPUE") -> p_lon
plot_grid(p_lat, p_lon, nrow = 1)

scallops_comb %>%
  ggplot()+
  geom_point(aes(x = set_lon,  y = set_lat, color = rw_cpue), alpha = 0.2)+
  scale_color_gradientn(colours = terrain.colors(10))+
  labs(y = "Latitude", x = "Longitude", color = "CPUE")

# model fitting ----

## define cpue adjustment as 10% of the all time average (CPUE must be positive for gamma distribution)
adj <- mean(scallops_comb$rw_cpue, na.rm = T) * 0.10

## Fit the existing model 
mod_0 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + te(set_lon, set_lat, by = Bed) + Year + 
               Bed + Vessel, data = scallops_comb, gamma=1.4, family=Gamma(link=log), select = T)
summary(mod_0)
### partial effects
print(plot(getViz(mod_0)), pages = 1)
### diagnostics
gam.check(mod_0)

## Fit several versions of a 'Full Model' that includes: 

### smoothed depth, wave height, wind speed, haul_speed, tidal rate, location, and linear year, bed, vessel
mod_1 <- bam(rw_cpue + adj ~ s(depth) + s(wave_ht) + s(wind_sp) + s(haul_speed) + s(tidal_rate) + 
               te(set_lon, set_lat, by = Bed) + Year + Bed + Vessel, data = scallops_comb, gamma=1.4, 
             family=Gamma(link=log), select = T)
summary(mod_1)
anova(mod_1)
print(plot(getViz(mod_1)) + l_fitRaster() + l_fitContour() + l_points(color = "grey60")+ l_fitLine() + l_ciLine() 
      + l_ciBar(linetype = 1) + l_fitPoints(size = 1), pages = 2)
gam.check(mod_1)

### restrict k on depth and subset data to remove haul speed outliers
mod_1.1 <- bam(rw_cpue + adj ~ s(depth, k = 4) + s(wave_ht) + s(wind_sp) + s(haul_speed) + s(tidal_rate) + 
                 te(set_lon, set_lat, by = Bed) + Year + Bed + Vessel, data = scallops_comb[-ex,], gamma=1.4, 
               family=Gamma(link=log), select = T, subset = haul_speed >= 4)
summary(mod_1.1)
print(plot(getViz(mod_1.1)) + l_fitRaster() + l_fitContour() + l_points(color = "grey60")+ l_fitLine() + l_ciLine() 
      + l_ciBar(linetype = 1) + l_fitPoints(size = 1), pages = 2)
gam.check(mod_1.1)

### same as mod_1.1, but nest s(depth) by bed
mod_2 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht) + s(wind_sp) + s(haul_speed) + s(tidal_rate) + 
               te(set_lon, set_lat, by = Bed) + Year + Bed + Vessel, data = scallops_comb[-ex,], gamma=1.4, 
             family=Gamma(link=log), select = T, subset = haul_speed >= 4)
summary(mod_2)
print(plot(getViz(mod_2)) + l_fitRaster() + l_fitContour() + l_points(color = "grey60")+ l_fitLine() + l_ciLine() 
      + l_ciBar(linetype = 1) + l_fitPoints(size = 1), pages = 2)
gam.check(mod_2)

### same as mod_2, but include interaction between year and Bed
mod_3 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht) + s(wind_sp) + s(haul_speed) + s(tidal_rate) + 
               te(set_lon, set_lat, by = Bed) + Year * Bed + Vessel, data = scallops_comb, gamma=1.4, 
             family=Gamma(link=log), select = T, subset = haul_speed >= 4)
summary(mod_3)
anova(mod_3)
print(plot(getViz(mod_3)) + l_fitRaster() + l_fitContour() + l_points(color = "grey60")+ l_fitLine() + l_ciLine() 
      + l_ciBar(linetype = 1) + l_fitPoints(size = 1), pages = 2)
gam.check(mod_3)

### mod_3 with wind speed, haul speed, and tidal rate as parametric effects
mod_3.1 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht) + wind_sp + haul_speed + tidal_rate + 
                 te(set_lon, set_lat, by = Bed) + Year * Bed + Vessel, data = scallops_comb, gamma=1.4, 
               family=Gamma(link=log), select = T, subset = haul_speed >= 4)
summary(mod_3.1)
anova(mod_3.1)
### refit mod3.1 after dropping haul speed and tidal rate
mod_3.1 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht) + wind_sp + 
                 te(set_lon, set_lat, by = Bed) + Year * Bed + Vessel, data = scallops_comb, gamma=1.4, 
               family=Gamma(link=log), select = T, subset = haul_speed >= 4)
summary(mod_3.1)
gam.check(mod_3.1)

## Fit reduced models that include:

### smoothed depth, wave height, location, and linear year, bed, and Vessel
mod_4 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht) + te(set_lon, set_lat, by = Bed) + 
               Year * Bed + Vessel, data = scallops_comb, gamma=1.4, family=Gamma(link=log), select = T)
summary(mod_4)
gam.check(mod_4)
### refit mod_4 with restrict degrees of freedom on wave height
mod_4.1 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht, k = 3) + te(set_lon, set_lat, by = Bed) + 
                 Year * Bed + Vessel, data = scallops_comb, gamma=1.4, family=Gamma(link=log), select = T)
summary(mod_4.1)
gam.check(mod_4.1)

### same as mod_4.1, but without te(set_lat, set_lon) nested within bed
mod_5 <- bam(rw_cpue + adj ~ s(depth, k = 4, by = Bed) + s(wave_ht, k = 3) + te(set_lon, set_lat) + 
               Year * Bed + Vessel, data = scallops_comb[-ex,], gamma=1.4, family=Gamma(link=log), select = T)
summary(mod_5)
gam.check(mod_5)

## in linear predicator vs residual plot that are several point in each model with combinations of linear predictors,
## could be considered for removal as outliers

# model comparison ----

## compile models into a list
mget(ls(pattern = "mod_")) %>%
  tibble(gam = .) %>%
  mutate(name = names(gam)) -> model_comp

## select only models we wish to compare
## add estimaed df, GCV score, AIC, and delta AIC
model_comp %>%
  mutate(edf = map_dbl(gam, f_edf),
         gcv = map_dbl(gam, f_gcv),
         loglik = purrr::map(gam, logLik.gam),
         AIC = map_dbl(loglik, AIC),
         delta_AIC = AIC - min(AIC)) %>%
  arrange(AIC) 

## anova between mod_4.1 and mod_3
anova(update(mod_4.1, ~., subset = haul_speed >= 4), mod_3.1, test = "Chisq")


# compare raw and standardized cpue estimates ----

## average CPUE per year
scallops_comb %>%
  mutate(mod_4.1_fit = predict.gam(mod_4.1, ., type = "response") - adj,
         mod_0_fit = predict.gam(mod_0,  ., type = "response") - adj) %>%
  group_by(Year) %>%
  summarise(raw = median(rw_cpue),
            raw_se = sd(rw_cpue) / sqrt(n() - 1),
            mod_4.1 = median(mod_4.1_fit),
            mod_4.1_se = sd(mod_4.1_fit) / sqrt(n() - 1),
            mod_0 = median(mod_4.1_fit),
            mod_0_se = sd(mod_0_fit) / sqrt(n() - 1)) %>%
  unite("Raw", raw:raw_se) %>%
  unite("Model_4", mod_4.1:mod_4.1_se) %>%
  unite("Model_0", mod_0:mod_0_se) %>%
  pivot_longer(cols = c(Raw, Model_4, Model_0)) %>%
  separate(value, into = c("mean", "se"), sep = "_") %>%
  rename(Model = name) %>%
  mutate(mean = as.numeric(mean), 
         se = as.numeric(se)) %>%
  ggplot()+
  geom_point(aes(x = Year, y = mean, col = Model))+
  geom_line(aes(x = Year, y = mean, col = Model, linetype = Model, group = Model))+
  geom_errorbar(aes(x = Year, ymin = mean - se, ymax = mean + se, group = Model), width = 0.2)+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)", color = NULL, linetype = NULL)

## standardized vs nominal values
scallops_comb %>%
  mutate(mod_4.1_fit = predict.gam(mod_4.1, ., type = "response") - adj,
         mod_0_fit = predict.gam(mod_0,  ., type = "response") - adj) %>%
  ggplot()+
  geom_point(aes(x = mod_4.1_fit, y = rw_cpue))+
  labs(x = "Standardized CPUE (Model 4)", y = "Round Weight CPUE")

### correlation
scallops_comb %>%
  mutate(mod_4.1_fit = predict.gam(mod_4.1, ., type = "response") - adj,
         mod_0_fit = predict.gam(mod_0,  ., type = "response") - adj,
         mod_3_fit = predict.gam(mod_3,  ., type = "response") - adj,) -> tmp.dat
with(tmp.dat, cor(mod_4.1_fit, rw_cpue))
with(tmp.dat, cor(mod_3_fit, rw_cpue))
with(tmp.dat, cor(mod_0_fit, rw_cpue))



# examine best model with subsets of data (ongoing) ----

# creat a list of samples
scal_samp <- list()
for(i in 1:100) {
  scallops_comb %>%
    sample_n(1000) -> scal_samp[[i]]
}

# define a function for the GAM model
f_mod_10 <- function(x){
  bam(rw_cpue + adj ~ s(depth, k=4, by = Bed) + te(set_lon, set_lat, by = Bed) + s(wave_ht, k = 3) + Year * 
        Bed + Vessel, data = x, gamma=1.4, family=Gamma(link=log), select = T)
}

tibble(scal_samp) %>%
  mutate(gam = map(scal_samp, f_mod_10)) -> scal_samp_mod

saveRDS(scal_samp_mod, "./scal_samp.RDS")

red_mod <- tibble(edf = summary(mod_10)$edf,
                  covar = 1:15)

scal_samp_mod %>%
  mutate(sm_edf = map(gam, f_smooth_func_edf),
         sample_name = c(1:100)) %>%
  select(sm_edf, sample_name) %>%
  unnest(sm_edf) %>%
  mutate(covar = rep(c(1:15), 100)) %>%
  ggplot()+
  geom_point(aes(x = covar, y = sm_edf)) +
  geom_point(data = red_mod, aes(x = covar, y = edf), col = "red")

scallops_comb %>%
  mutate(fitted = predict(mod_10, type = "response")) %>%
  group_by(Year) %>%
  summarise(mean_std_cpue = mean(fitted)) -> mod_10_dat



scal_samp_mod %>%
  mutate(fitted = map(gam, f_predict),
         sample = 1:100) %>%
  unnest(scal_samp, fitted) %>%
  group_by(Year, sample) %>%
  summarise(mean_std_cpue = mean(fitted)) %>%
  ggplot()+
  geom_point(aes(x = Year, y = mean_std_cpue))+
  geom_point(data = mod_10_dat, aes(x = Year, y = mean_std_cpue), col = "red")

