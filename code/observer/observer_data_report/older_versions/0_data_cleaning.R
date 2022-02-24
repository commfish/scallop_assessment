# load ----
source("code/helper.R")

# set global
YEAR = 2019

# data ----
# call in data by pasting the directory to the filenames then reading in all .csv and bind them by row

scallops <- do.call(bind_rows,
                    lapply(gsub(" ", "",paste("data/catch/", 
                                              list.files("data/catch/")), fixed=TRUE), read_csv))
# rename for consistency

names(scallops) <- c("Fishery", "District", "ADFG", "Trip_ID", "Haul_ID", 
                     "haul", "gear_perf", "haul_sampled", "Set_date", "set_lat",
                     "set_lon", "statarea", "depth", "dredge_count", 
                     "dredge_width", "dredge_down", "dredge_up", "duration", 
                     "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                     "rtnd_basket", "round_weight", "meat_weight", "est_yield",
                     "tot_rtnd_basket", 'tot_day_meat_weight')

scallops %>%
  # adjust dates and set variables
  # remove data where dredge hours & area swept are zero
  # calculate round weight cpue by time and by area
  # calculate meat weight cpue by time and by area
  mutate(set_date = mdy(Set_date),
         year = year(set_date),
         date = yday(set_date),
         year = ifelse(date<100, year-1, year), # fishing year
         Year = factor(year),
         Vessel = factor(ADFG),
         FY = paste0(year, "/", sprintf('%02d', (year + 1) %% 100)),
         dum = 1) %>%
  filter(area_swept>0) %>%
  mutate(rw_cpue = round_weight/dredge_hrs,
         mw_cpue = meat_weight/dredge_hrs,
         District = ifelse(District=='D16' | District =='D', 'YAK', District)) -> scallops 
  
write_csv(scallops, paste0("output/", YEAR, "/scallops.csv"))

# d16 ----
# scallops %>% 
#   filter(District=='D16') %>%
#   write_csv(paste0("output/", YEAR, "/d16.csv"))

# yak ----

scallops %>% 
  filter(District=='YAK' | District=='D16' | District =='D') %>%
  mutate(bed = case_when(set_lon <= -138.3  & set_lon > -138.79 ~ 5,
                         set_lon <= -138.79 & set_lon > -140.1  ~ 4,
                         set_lon <= -140.1  & set_lon > -142.15 ~ 3,
                         set_lon <= -142.15 & set_lon > -142.7  ~ 2,
                         set_lon <= -142.7  & set_lon > -143.5  ~ 1,
                         set_lon <= -143.5 ~ 0,
                         TRUE ~ 6),
         District = 'YAK') %>% 
  filter(complete.cases(.)) %>% 
  write_csv(paste0("output/", YEAR, "/yak.csv"))

# ksh ----
scallops %>% 
  filter(District=='KSH') %>% 
  mutate(bed = case_when(set_lat > 58.35 ~ 1,
                         set_lat<= 58.35 ~ 2),
         Bed = factor(bed),
         Bed = recode(Bed, "2"= '2-7')) %>% 
  write_csv(paste0("output/", YEAR, "/ksh.csv"))

# kne ----
scallops %>% 
  filter(District=='KNE') %>% 
  mutate(bed = case_when(set_lat > 57.75 ~ 1,
                         set_lat <= 57.8 & set_lat >= 57.17 & set_lon >= -151.95 ~ 2, 
                         set_lat>57 & set_lat< 57.5 & set_lon > -151.8 ~ 2, 
                         set_lon < -152 & set_lat > 57.05 ~ 3, 
                         set_lat>56.8 & set_lat < 57.2 & set_lon > -152.5 & set_lon < -152.12 ~ 3,
                         set_lon > -152.12 & set_lat<57.05 & set_lat> 56.9 ~ 4, 
                         set_lat < 56.9 & set_lon > -152.5 ~ 5, 
                         TRUE ~ 6),
         Bed = factor(bed)) %>% 
  write_csv(paste0("output/", YEAR, "/kne.csv"))

# kse ----
scallops %>% 
  filter(District=='KSE') %>% 
  write_csv(paste0("output/", YEAR, "/kse.csv"))

# ksw ----
scallops %>% 
  filter(District=='KSW') %>% 
  mutate(bed = case_when(set_lat > 56.7 ~ 1,
                         set_lat < 56.7 ~ 2),
         Bed = factor(bed)) %>% 
  write_csv(paste0("output/", YEAR, "/ksw.csv"))

# m ----

scallops %>% 
  filter(District=='UB') %>% 
  write_csv(paste0("output/", YEAR, "/m.csv"))

# o ----
scallops %>% 
  filter(District=='O') %>% 
  mutate(bed = case_when(set_lon > -168 & set_lat > 53.2 ~ 1,
                         set_lon < -168 & set_lat > 53.2 ~ 4,
                         set_lat < 53.2 ~ 2), # no idea where bed 3 is...
         Bed = factor(bed)) %>% 
  write_csv(paste0("output/", YEAR, "/o.csv"))

# q ----

scallops %>% 
  filter(District=='Q') %>% 
  #excluded samples outside of the main bed
  filter(set_lon>-166) %>% 
  write_csv(paste0("output/", YEAR, "/q.csv"))


# ki ----
scallops %>% 
  filter(District=='WKI'| District =='EKI') %>% 
  mutate(bed = case_when(District=='WKI' ~ "wk1",
                         District=='EKI' ~ 'ek1'),
         Bed = factor(bed)) %>% 
  write_csv(paste0("output/", YEAR, "/ki.csv"))

# ksem ----

# scallops %>% 
# filter(District=='KSEM') -> ksem

# write_csv(q, "output/ksem.csv")

# shell height ----

do.call(bind_rows,
          lapply(gsub(" ", "",
                      paste("./data/shell_height/",
                            list.files("./data/shell_height/")), 
                          fixed=TRUE), read_csv)) %>% 
  write_csv(paste0("output/", YEAR, "/sh.csv"))

# log book ----
do.call(bind_rows,
               lapply(gsub(" ", "",
                           paste("./data/log/",
                                 list.files("./data/log/")), 
                           fixed=TRUE), read_csv)) %>% 
  write_csv(paste0("output/", YEAR, "/log.csv"))

# crab ----

do.call(bind_rows,
                lapply(gsub(" ", "",paste("./data/crab_size/", 
                                          list.files("./data/crab_size/")), fixed=TRUE), read_csv)) %>% 
  write_csv(paste0("output/", YEAR, "/crab.csv"))

# bycatch ----

do.call(bind_rows,
              lapply(gsub(" ", "",paste("./data/bycatch/", 
                                        list.files("./data/bycatch/")), fixed=TRUE), read_csv)) %>%  
  mutate(set_date = as.Date(Set_Date, "%m-%d-%Y"),
         year = as.numeric(format(set_date, "%Y")),
         date = yday(set_date),
         year = ifelse(date<100, year-1, year),
         Year = factor(year),
         Vessel = factor(ADFG)) %>% 
  write_csv(paste0("output/", YEAR, "/by.csv"))

# age ----
# this age data needs to be cleaned up and provide in a uniform framework from a database - not these individual files
ages <- read.csv("data/age/scallop_ages_bcw.csv")
ages <- ages[,2:20]

#add 2014 data
ages14 <- read.csv("data/age/2014ScallopShellAgeData.csv")
ages14 <- ages14[,1:17]
#adjust names to match other files
names(ages14) <- c('id', 'trip_id', 'haul_id', 'aged_by','fishery',
                   'fishing_season','district','vessel','trip_or_packet','date',
                   'small_or_retained','haul','shell','annuli','letter_code',
                   'number_code','shell_height')
ages14 %>%
  mutate(shell = factor(shell)) -> ages14

# smartbind is from the gtools package
ages <- smartbind(ages, ages14)

#add 2015 data
ages15 <- read.csv("data/age/2015ShellAgeData_5.10.16.csv")
ages15 <- ages15[,1:16]

names(ages15) <- c( 'trip_id', 'haul_id', 'aged_by','fishery', 'fishing_season',
                    'district','vessel','trip_or_packet','date',
                    'small_or_retained','haul','shell','annuli','letter_code',
                    'number_code','shell_height')
ages15 %>%
  mutate(letter_code = factor(letter_code), shell = factor(shell)) -> ages15

ages <- smartbind(ages, ages15)

names(ages) <- c('id','Trip_ID', 'Haul_ID', 'aged_by','date_aged','Fishery',
                 'Season','District','ADFG','packetnum','stat_area','Set_Date',
                 'Rtnd_Disc','haul','shell_num','annuli','code_let','code_num',
                 'Shell_Height')

ages %>% 
  mutate(set_date = as.Date(ages$Set_Date, "%m/%d/%Y"),
         year = as.numeric(format(set_date, "%Y")),
         date = yday(set_date),
         year = ifelse(date<100, year-1, year),
         Year = factor(year)) %>%
  mutate(Set_Date = format(Set_Date, format = "%Y-%m-%d")) %>%
  select(-id, -aged_by, -date_aged, -stat_area, -code_let, -code_num, -shell_num) %>% 
  write_csv(paste0("output/", YEAR, "/ages.csv"))

# shell height/meat weight ----

read_csv(paste0('data/meat_weight/2018ScallopObsSHMWData.csv')) %>% 
  mutate(set_date = mdy(set_date))  %>% 
  left_join(scallops, .) %>% 
  filter(year>=2018) %>% filter(!is.na(meat_weight_g)) %>% 
  write_csv(paste0("output/", YEAR, "/shmw.csv"))

  ggplot(aes(shell_height, meat_weight_g, color = District)) + geom_point() +
  stat_smooth(method = 'gam', formula = y~s(x, k = 4))

read_csv(paste0('data/meat_weight/2018ScallopObsSHMWData.csv')) %>% 
  mutate(set_date = mdy(set_date))  %>% 
  left_join(scallops, .) %>% 
  filter(year>=2018) %>% filter(!is.na(meat_weight_g)) %>% 
  ggplot(aes(date, meat_weight_g, color = District)) + geom_point() +
  stat_smooth(method = 'gam', formula = y~s(x, k = 4))

  write_csv(paste0("output/", YEAR, "/shmw.csv"))


