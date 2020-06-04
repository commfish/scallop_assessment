# 2018 analysis

# load -------------------------------------------------------------------------

source('code/helper.R')
source('code/functions.R')

# globals ----------------------------------------------------------------------

YEAR = 2018 # examining the prior fishery season data

# define color values - MANUAL adjustment
values <- c('2017'='#2b8cbebb','2016'='#54278840','2015'='#998ec340',
            '2014'='#d8daeb40','2013'='#fee0b640','2012'='#f1a34040',
            '2011'='#b3580640', '2010'=1, '2009'=1) 
breaks <- as.character((YEAR-1):2009) # define breaks for plotting

# Append the newest GHL to the end of each area
yak_ghl = data.frame(ghl=c(rep(160000,3),rep(120000,5), 140000))
d16_ghl = data.frame(ghl=c(rep(25000,7),rep(5000,2)))
ki_ghl = data.frame(ghl = c(20000,8500,8500,6300, 6300))
ksh_ghl = data.frame(ghl=c(170000,170000,170000,135000,105000,105000,75000, 25000, 25000))
ksw_ghl = data.frame(ghl=rep(25000,8))
kne_ghl = data.frame(ghl=c(75000,65000,70000,60000,rep(55000,5)))
o_ghl = data.frame(ghl=c(10000,10000,10000,10000,10000,10000,10000, 10000, 10000))
q_ghl = data.frame(ghl=c(rep(50000,6),7500,7500,7500))
m_ghl = data.frame(ghl=rep(15000, 6))

# data -------------------------------------------------------------------------
read_csv(paste0('output/', YEAR,'/sh.csv')) -> sh 
read_csv(here(paste0('output/', YEAR,'/by.csv'))) %>% 
  mutate(set_date = as.Date(Set_Date, "%m-%d-%Y"),
         date = yday(set_date),
         Year = factor(year),
         Vessel = factor(ADFG)) -> by

read_csv(here(paste0('output/',YEAR,'/scallops.csv'))) -> scallops

read_csv(here(paste0('output/',YEAR,'/yak.csv'))) %>% 
  mutate(Year=factor(Year),
         Vessel = factor(Vessel),
         FY = factor(FY),
         Bed = factor(Bed, levels = c('B', '1', '2', '3', '4', '5', 'D16')),
         dum=1) -> yak

yak %>%
  filter(District=='D16') -> d16

read_csv(here(paste0('output/',YEAR,'/ki.csv'))) %>% 
  mutate(Year=factor(Year),
         Vessel = factor(Vessel),
         FY = factor(FY),
         Bed = factor(Bed, levels = c('ek1', 'wk1')),
         dum=1) -> ki

read_csv(here(paste0('output/',YEAR,'/ksh.csv'))) %>% 
  mutate(Year=factor(Year),
         Vessel = factor(Vessel),
         FY = factor(FY),
         Bed = factor(Bed, levels = c('1', '2-7')),
         dum=1) -> ksh

read_csv(here(paste0('output/',YEAR,'/ksw.csv'))) %>% 
  mutate(Year=factor(Year),
         Vessel = factor(Vessel),
         FY = factor(FY),
         Bed = factor(Bed, levels = c('1', '2')),
         dum=1) -> ksw

read.csv(here(paste0('output/',YEAR,'/kne.csv'))) %>% 
  mutate(Year=factor(Year),
         Vessel = factor(Vessel),
         FY = factor(FY),
         Bed = factor(Bed, levels = c('1', '2', '3', '4', '5', '6')),
         dum=1) -> kne

read_csv(here(paste0('output/',YEAR,'/m.csv'))) %>% 
  mutate(Year=factor(Year),
         Vessel = factor(Vessel),
         FY = factor(FY),
         dum=1) -> m

read_csv(here(paste0('output/',YEAR,'/o.csv'))) %>% 
  mutate(Year=factor(Year),
         Vessel = factor(Vessel),
         FY = factor(FY),
         dum=1, 
         Bed = factor(bed)) -> o

read_csv(here(paste0('output/',YEAR,'/q.csv'))) %>% 
  mutate(Year=factor(Year),
         Vessel = factor(Vessel),
         FY = factor(FY),
         dum=1) -> q

# maps -------------------------------------------------------------------------

# global 
f_global_map(scallops, YEAR)
f_global_map_year(scallops, YEAR)

# district
# "YAK"  "EKI"  "WKI"  "KSH"  "KSW"  "KNE"  "O"    "Q"    "UB"   "KSEM" "C"   
f_district_map(yak, breaks=2, YEAR)
f_district_map(ki, breaks=0.5, YEAR)
f_district_map(ksh, breaks=1, YEAR)
f_district_map(ksw, breaks=0.5, YEAR)
f_district_map(kne, breaks=1, YEAR)
f_district_map(o, breaks=2, YEAR)
f_district_map(q, breaks=1, YEAR)
f_district_map(m, breaks=0.5, YEAR)

# raw tables -----------------------------------------------------------------------
f_raw_cpue_tbl(yak, yak_ghl, YEAR)
f_raw_cpue_tbl(d16, d16_ghl, YEAR)
f_raw_cpue_tbl(ki, ki_ghl, YEAR)
f_raw_cpue_tbl(ksh, ksh_ghl, YEAR)
f_raw_cpue_tbl(ksw, ksw_ghl, YEAR)
f_raw_cpue_tbl(kne, kne_ghl, YEAR)
f_raw_cpue_tbl(o, o_ghl, YEAR)
f_raw_cpue_tbl(q, q_ghl, YEAR)
f_raw_cpue_tbl(m, m_ghl, YEAR)

# model fits -------------------------------------------------------------------

yak_mw <- f_gam_mw(yak) 
yak_rw <- f_gam_rw(yak) 
saveRDS(yak_mw,paste0("models/", YEAR, "/yak_mw.rds"))
saveRDS(yak_rw,paste0("models/", YEAR, "/yak_rw.rds"))
                      
ki_mw <- f_gam_mw(ki) 
ki_rw <- f_gam_rw(ki) 
saveRDS(ki_mw,paste0("models/", YEAR, "/ki_mw.rds"))
saveRDS(ki_rw,paste0("models/", YEAR, "/ki_rw.rds"))
                      

ksh_mw <- f_gam_mw(ksh) 
ksh_rw <- f_gam_rw(ksh) 
saveRDS(ksh_mw,paste0("models/", YEAR, "/ksh_mw.rds"))
saveRDS(ksh_rw,paste0("models/", YEAR, "/ksh_rw.rds"))

kne_mw <- f_gam_mw(kne) 
kne_rw <- f_gam_rw(kne)
saveRDS(kne_mw,paste0("models/", YEAR, "/kne_mw.rds"))
saveRDS(kne_rw,paste0("models/", YEAR, "/kne_rw.rds"))

ksw_mw <- f_gam_mw(ksw) 
ksw_rw <- f_gam_rw(ksw) 
saveRDS(ksw_mw,paste0("models/", YEAR, "/ksw_mw.rds"))
saveRDS(ksw_rw,paste0("models/", YEAR, "/ksw_rw.rds"))

o_mw <- f_gam_mw(o) 
o_rw <- f_gam_rw(o) 
saveRDS(o_mw,paste0("models/", YEAR, "/o_mw.rds"))
saveRDS(o_rw,paste0("models/", YEAR, "/o_rw.rds"))

q_mw <- f_gam_mw(q) 
q_rw <- f_gam_rw(q) 
saveRDS(q_mw,paste0("models/", YEAR, "/q_mw.rds"))
saveRDS(q_rw,paste0("models/", YEAR, "/q_rw.rds"))

m_mw <- f_gam_mw(m) 
m_rw <- f_gam_rw(m) 
saveRDS(m_mw,paste0("models/", YEAR, "/m_mw.rds"))
saveRDS(m_rw,paste0("models/", YEAR, "/m_rw.rds"))

# prediction data --------------------------------------------------------------
new_yak <- pred_data(yak) 
new_ki <- pred_data(ki) 
new_ksh <- pred_data(ksh) 
new_kne <- pred_data(kne) 
new_ksw <- pred_data(ksw) 
new_o <- pred_data(o) 
new_q <- pred_data(q) 
new_m <- pred_data(m) 

# predictions ------------------------------------------------------------------
yak %>% 
  mutate(fit_mw = predict(yak_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(yak_rw, ., type = 'response')) -> yak_fit

new_yak %>% 
  mutate(fit_mw = predict(yak_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(yak_rw, ., type = 'response')) -> new_yak

ki %>% 
  mutate(fit_mw = predict(ki_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(ki_rw, ., type = 'response')) -> ki_fit

new_ki %>% 
  mutate(fit_mw = predict(ki_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(ki_rw, ., type = 'response')) -> new_ki

ksh %>% 
  mutate(fit_mw = predict(ksh_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(ksh_rw, ., type = 'response')) -> ksh_fit

new_ksh %>% 
  mutate(fit_mw = predict(ksh_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(ksh_rw, ., type = 'response')) -> new_ksh

kne %>% 
  mutate(fit_mw = predict(kne_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(kne_rw, ., type = 'response')) -> kne_fit

new_kne %>% 
  mutate(fit_mw = predict(kne_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(kne_rw, ., type = 'response')) -> new_kne

ksw %>% 
  mutate(fit_mw = predict(ksw_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(ksw_rw, ., type = 'response')) -> ksw_fit

new_ksw %>% 
  mutate(fit_mw = predict(ksw_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(ksw_rw, ., type = 'response')) -> new_ksw

o %>% 
  mutate(fit_mw = predict(o_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(o_rw, ., type = 'response')) -> o_fit

new_o %>% 
  mutate(fit_mw = predict(o_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(o_rw, ., type = 'response')) -> new_o

q %>% 
  mutate(fit_mw = predict(q_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(q_rw, ., type = 'response')) -> q_fit

new_q %>% 
  mutate(fit_mw = predict(q_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(q_rw, ., type = 'response')) -> new_q

m %>% 
  mutate(fit_mw = predict(m_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(m_rw, ., type = 'response')) -> m_fit

new_m %>% 
  mutate(fit_mw = predict(m_mw, ., type = 'response')) %>% 
  mutate(fit_rw = predict(m_rw, ., type = 'response')) -> new_m

# # std tables -----------------------------------------------------------------------
f_std_cpue_tbl(yak_fit, yak_ghl, YEAR)
f_std_cpue_tbl(d16_fit, d16_ghl, YEAR)
f_std_cpue_tbl(ki_fit, ki_ghl, YEAR)
f_std_cpue_tbl(ksh_fit, ksh_ghl, YEAR)
f_std_cpue_tbl(ksw_fit, ksw_ghl, YEAR)
f_std_cpue_tbl(kne_fit, kne_ghl, YEAR)
f_std_cpue_tbl(o_fit, o_ghl, YEAR)
f_std_cpue_tbl(q_fit, q_ghl, YEAR)
f_std_cpue_tbl(m_fit, m_ghl, YEAR)

# figures ----------------------------------------------------------------------
# yak ----
fig_mw_cpue(yak)
fig_rw_cpue(yak)

#fig_mw_fit(yak_fit)
fig_rw_fit(yak_fit)

fig_rw_vessel(yak_fit)
fig_sh(yak)
fig_sh_year(yak, YEAR)
fig_rd(yak)
fig_discard(yak)
fig_discard_rat(yak)
fig_bycatch(yak)
fig_bycatch_rat(yak)
fig_clap(yak)

# ki ----
fig_mw_cpue(ki)
fig_rw_cpue(ki)

#fig_mw_fit(ki_fit)
fig_rw_fit(ki_fit)

fig_rw_vessel(ki_fit)
fig_sh(ki)
fig_sh_year(ki, YEAR)
fig_rd(ki)
fig_discard(ki)
fig_discard_rat(ki)
fig_bycatch(ki)
fig_bycatch_rat(ki)
fig_clap(ki)


# ksh ----
fig_mw_cpue(ksh)
fig_rw_cpue(ksh)

#fig_mw_fit(new_ksh)
fig_rw_fit(ksh_fit)

fig_rw_vessel(ksh_fit)
fig_sh(ksh)
fig_sh_year(ksh, YEAR)
fig_rd(ksh)
fig_discard(ksh)
fig_discard_rat(ksh)
fig_bycatch(ksh)
fig_bycatch_rat(ksh)
fig_clap(ksh)

# kne ----
fig_mw_cpue(kne)
fig_rw_cpue(kne)

#fig_mw_fit(kne_fit)
fig_rw_fit(kne_fit)

fig_rw_vessel(kne_fit)
fig_sh(kne)
fig_sh_year(kne, YEAR)
fig_rd(kne)
fig_discard(kne)
fig_discard_rat(kne)
fig_bycatch(kne)
fig_bycatch_rat(kne)
fig_clap(kne)

# ksw ----
fig_mw_cpue(ksw)
fig_rw_cpue(ksw)

#fig_mw_fit(new_ksw)
fig_rw_fit(ksw_fit)

fig_rw_vessel(ksw_fit)
fig_sh(ksw)
fig_sh_year(ksw, YEAR)
fig_rd(ksw)
fig_discard(ksw)
fig_discard_rat(ksw)
fig_bycatch(ksw)
fig_bycatch_rat(ksw)
fig_clap(ksw)

# o ----
fig_mw_cpue(o)
fig_rw_cpue(o)

#fig_mw_fit(new_o)
fig_rw_fit(o_fit)

fig_rw_vessel(o_fit)
fig_sh(o)
fig_sh_year(o, YEAR)
fig_rd(o)
fig_discard(o)
fig_discard_rat(o)
fig_bycatch(o)
fig_bycatch_rat(o)
fig_clap(o)

# q ----
fig_mw_cpue(q)
fig_rw_cpue(q)

#fig_mw_fit(q_fit)
fig_rw_fit(q_fit)

fig_rw_vessel(q_fit)
fig_sh(q)
fig_sh_year(q, YEAR)
fig_rd(q)
fig_discard(q)
fig_discard_rat(q)
fig_bycatch(q)
fig_bycatch_rat(q)
fig_clap(q)


# m ----
fig_mw_cpue(m)
fig_rw_cpue(m)

#fig_mw_fit(new_m)
fig_rw_fit(m_fit)

fig_rw_vessel(m_fit)
fig_sh(m)
fig_sh_year(m, YEAR)
fig_rd(m)
fig_discard(m)
fig_discard_rat(m)
fig_bycatch(m)
fig_bycatch_rat(m)
fig_clap(m)
