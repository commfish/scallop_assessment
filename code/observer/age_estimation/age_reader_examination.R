# Examination of age data by ager and year aged

# notes ----

# authour: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# date: 2019 - 10 - 25

# load ---- 
library(tidyverse)
library(FNGr)
theme_set(theme_sleek())

# data ----
fish_log <- read_csv("./data/age/fish_log_data_1996_2015.csv")
age <- read_csv("./data/age/age_data_observer_shells_1996_2015.csv")

# combine fishery data with age data
# add unique shell_id and fishery_year
age %>%
  right_join(fish_log, by = "haul_id") %>%
  mutate(shell_id = paste0(haul_id, "_", shell_num),
         fishery_year = as.numeric(substring(fishery, 3, 4)),
         fishery_year = case_when(fishery_year >= 90 ~ fishery_year+1900,
                                  fishery_year < 90 ~ fishery_year+2000),
         Reader_yrs = case_when(fishery_year <= 2005 ~ "2000 - 2005",
                                fishery_year %in% c(2006:2014) ~ "2006 - 2014", 
                                fishery_year == 2015 ~ "2015"), 
         Reader_yrs = factor(Reader_yrs, levels = c("2000 - 2005", "2006 - 2014", "2015"))) -> age

# add field cohort
age %>%
  mutate(cohort = fishery_year - tot_annuli) -> age

# tibble with only first annuli
age %>%
  filter(annulus == 1,
         !(code_let %in% c("B", "AB", "BC", "BD"))) -> age_fa

# tibble with only intermediate annuli
age %>%
  filter(annulus > 1 & annulus < tot_annuli,
         !(code_let %in% c("B", "AB", "BC", "BD"))) -> age_int


# eda ----

## Number of scallops aged ====
### Number of shells aged by reader
png('./figures/age_reader/age_reader_fig1.png', width = 7, height = 3, units = "in", res = 300)
age %>%
  filter(!is.na(tot_annuli)) %>%
  distinct(shell_id, .keep_all = T) %>%
  group_by(fishery_year, ager_id) %>%
  summarize(number_aged = n()) -> p_dat

x_axis <- tickr(as.data.frame(p_dat), fishery_year, 5)  

ggplot(p_dat)+
  geom_bar(aes(x = fishery_year, y = number_aged, fill = factor(ager_id)), color = "black", stat = "identity", alpha = 0.5)+
  labs(x = "Year", y = "Number of Scallops Aged", fill = "Reader")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  scale_y_continuous(expand = c(0,0), limits  = c(0, 2400)) 
dev.off()

### Number of shell aged within each district
age %>%
  filter(!is.na(tot_annuli)) %>%
  distinct(shell_id, .keep_all = T) %>%
  group_by(district, ager_id, fishery_year) %>%
  summarize(number_aged = n()) -> p_dat

x_axis <- tickr(as.data.frame(p_dat), fishery_year, 5)  

ggplot(p_dat)+
  geom_bar(aes(x = fishery_year, y = number_aged, fill = factor(ager_id)), color = "black", stat = "identity")+
  labs(x = "Year", y = "Number of Scallops Aged", fill = "Reader")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 800))+
  facet_wrap(~ district)

### Number of shell aged within each district, by bed
age %>%
  filter(!is.na(tot_annuli)) %>%
  distinct(shell_id, .keep_all = T) %>%
  group_by(district, bed_code, ager_id, fishery_year) %>%
  summarize(number_aged = n()) -> p_dat

x_axis <- tickr(as.data.frame(p_dat), fishery_year, 5)

ggplot(p_dat)+
  geom_bar(aes(x = fishery_year, y = number_aged, fill = factor(bed_code)), color = "black", stat = "identity")+
  labs(x = "Year", y = "Number of Scallops Aged", fill = "Bed")+
  scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 800))+
  facet_wrap(~ district)



## Density plots of shell height and reader year group ====

### Denisty plot of shell height by district and year group 
age %>%
  filter(!is.na(tot_annuli),
         district %in% c("KNE", "KSH", "YAK")) %>%
  distinct(shell_id, .keep_all = T) %>%
  ggplot()+
  geom_density(aes(x = shell_height), fill = "grey", adjust = 2, alpha = 0.5)+
  labs(x = "Shell Height (mm)", y = "Density")+
  facet_wrap(~ district) 

png('./figures/age_reader/age_reader_fig2.png', width = 7, height = 3, units = "in", res = 300)
age %>%
  filter(!is.na(tot_annuli),
         district %in% c("KNE", "KSH", "YAK")) %>%
  distinct(shell_id, .keep_all = T) %>%
  ggplot()+
  geom_density(aes(x = shell_height, fill = Reader_yrs), adjust = 2, alpha = 0.5)+
  labs(x = "Shell Height (mm)", y = "Density", fill = "Reader")+
  facet_wrap(~ district)+
  theme(legend.position = "bottom")
dev.off()

### Density plot of age by district and year group

age %>%
  filter(!is.na(tot_annuli),
         district %in% c("KNE", "KSH", "YAK")) %>%
  distinct(shell_id, .keep_all = T) %>%
  ggplot()+
  geom_density(aes(x = tot_annuli), fill = "grey", adjust = 2, alpha = 0.5)+
  labs(x = "Age", y = "Density")+
  facet_wrap(~ district)

png('./figures/age_reader/age_reader_fig3.png', width = 7, height = 3, units = "in", res = 300)
age %>%
  filter(!is.na(tot_annuli),
         district %in% c("KNE", "KSH", "YAK")) %>%
  distinct(shell_id, .keep_all = T) %>%
  ggplot()+
  geom_density(aes(x = tot_annuli, fill = Reader_yrs), adjust = 2, alpha = 0.5)+
  labs(x = "Age", y = "Density", fill = "Reader")+
  facet_wrap(~ district)+
  theme(legend.position = "bottom")
dev.off()


## Fit shell height at age growth curves ====

### Define LvB model parameterization
f_LvB <- function(x) {nls(formula = shell_height ~ l_inf * (1 - exp(-k * (tot_annuli - t0))), 
                          start = sv, 
                          data = x)}

### KNE
##### subset data sets that differ by readers invloved
age %>%
  filter(!is.na(tot_annuli),
         district == "KNE",
         fishery_year %in% c(2000:2015)) %>%
  distinct(shell_id, .keep_all = T) -> age_2000_2015

##### Estimate starting values
age_2000_2015 %>%
  group_by(tot_annuli) %>%
  summarize(lt = mean(shell_height, na.rm = T)) %>%
  mutate(lt_1 = c(lt[2:length(lt)], NA)) %>%
  filter(complete.cases(.)) %>%
  lm(lt_1 ~ lt, data = .) %>%
  coef(.) %>%
  list(l_inf = as.numeric(.[1] / (1 - .[2])),
       k = as.numeric(-log(.[2])), 
       t0 = -5) %>%
  magrittr::extract(2:4) -> sv 

##### fit LvB model nested within year groups
age_2000_2015 %>%
  nest(-Reader_yrs) %>%
  mutate(LvB = map(data, f_LvB),
         fit_sh = map2(LvB, data, predict),
         Reader_yrs = factor(Reader_yrs, levels = c("2000 - 2005", "2006 - 2014", "2015"))) -> LvB_fits

#### rename the factor Reader_yrs so it does not get included in facet wrap
LvB_fits %>%
  unnest(data, fit_sh) %>%
  rename(Yrs = Reader_yrs) -> LvB_fits2

#### create scatter plot with fitted line for all reader year groups
LvB_fits %>%
  unnest(data, fit_sh) %>%
  ggplot()+
  geom_point(aes(x = tot_annuli, y = shell_height), position = "jitter", color = "grey40")+
  geom_line(data = LvB_fits2, aes(x = tot_annuli, y = fit_sh, color = Yrs), size = 1)+
  facet_wrap(~Reader_yrs)+
  labs(x = "Age", y = "Shell Height (mm)", color = "Year group") -> LvB_scat_KNE

#### create a residual plot for each reader, with year group 20006 - 2015 as the only model
LvB_fits %>%
  pull(LvB) %>%
  pluck(2) %>%
  coef() -> coefs

LvB_fits %>%
  unnest(data, fit_sh) %>%
  mutate(fit_sh_2006_2015 = coefs[1] * (1 - exp(-coefs[2] * (tot_annuli - coefs[3])))) %>%
  ggplot()+
  geom_point(aes(x = fit_sh_2006_2015, y = shell_height - fit_sh_2006_2015))+
  geom_hline(yintercept = 0, linetype = 2, color = "red")+
  facet_wrap(~Reader_yrs)+
  labs(x = "Residual Shell Height (mm)", y = "Fitted Shell Height (mm) (2006 - 2014 model)", color = "Year group") 


### KSH 
##### subset data sets that differ by readers invloved
age %>%
  filter(!is.na(tot_annuli),
         district == "KSH",
         fishery_year %in% c(2000:2015)) %>%
  distinct(shell_id, .keep_all = T)  -> age_2000_2015

##### Estimate starting values
age_2000_2015 %>%
  group_by(tot_annuli) %>%
  summarize(lt = mean(shell_height, na.rm = T)) %>%
  mutate(lt_1 = c(lt[2:length(lt)], NA)) %>%
  filter(complete.cases(.)) %>%
  lm(lt_1 ~ lt, data = .) %>%
  coef(.) %>%
  list(l_inf = as.numeric(.[1] / (1 - .[2])),
       k = as.numeric(-log(.[2])), 
       t0 = -5) %>%
  magrittr::extract(2:4) -> sv 

##### fit LvB model nested within year groups
age_2000_2015 %>%
  nest(-Reader_yrs) %>%
  mutate(LvB = map(data, f_LvB),
         fit_sh = map2(LvB, data, predict),
         Reader_yrs = factor(Reader_yrs, levels = c("2000 - 2005", "2006 - 2014", "2015"))) -> LvB_fits

##### rename the factor Reader_yrs so it does not get included in facet wrap
LvB_fits %>%
  unnest(data, fit_sh) %>%
  rename(Yrs = Reader_yrs) -> LvB_fits2

##### create scatter plot with fitted line for all reader year groups
LvB_fits %>%
  unnest(data, fit_sh) %>%
  ggplot()+
  geom_point(aes(x = tot_annuli, y = shell_height), position = "jitter", color = "grey40")+
  geom_line(data = LvB_fits2, aes(x = tot_annuli, y = fit_sh, color = Yrs), size = 1)+
  facet_wrap(~Reader_yrs)+
  labs(x = "Age", y = "Shell Height (mm)", color = "Year group") -> LvB_scat_KSH

#### create a residual plot for each reader, with year group 20006 - 2015 as the only model
LvB_fits %>%
  pull(LvB) %>%
  pluck(2) %>%
  coef() -> coefs

LvB_fits %>%
  unnest(data, fit_sh) %>%
  mutate(fit_sh_2006_2015 = coefs[1] * (1 - exp(-coefs[2] * (tot_annuli - coefs[3])))) %>%
  ggplot()+
  geom_point(aes(x = fit_sh_2006_2015, y = shell_height - fit_sh_2006_2015))+
  geom_hline(yintercept = 0, linetype = 2, color = "red")+
  facet_wrap(~Reader_yrs)+
  labs(x = "Residual Shell Height (mm)", y = "Fitted Shell Height (mm) (2006 - 2014 model)", color = "Year group") 

### YAK
##### subset data sets that differ by readers invloved
age %>%
  filter(!is.na(tot_annuli),
         district == "YAK",
         fishery_year %in% c(2000:2015)) %>%
  distinct(shell_id, .keep_all = T) -> age_2000_2015

##### Estimate starting values
age_2000_2015 %>%
  group_by(tot_annuli) %>%
  summarize(lt = mean(shell_height, na.rm = T)) %>%
  mutate(lt_1 = c(lt[2:length(lt)], NA)) %>%
  filter(complete.cases(.)) %>%
  lm(lt_1 ~ lt, data = .) %>%
  coef(.) %>%
  list(l_inf = as.numeric(.[1] / (1 - .[2])),
       k = as.numeric(-log(.[2])), 
       t0 = -5) %>%
  magrittr::extract(2:4) -> sv 

##### fit LvB model nested within year groups
age_2000_2015 %>%
  nest(-Reader_yrs) %>%
  mutate(LvB = map(data, f_LvB),
         fit_sh = map2(LvB, data, predict),
         Reader_yrs = factor(Reader_yrs, levels = c("2000 - 2005", "2006 - 2014", "2015"))) -> LvB_fits

##### rename the factor Reader_yrs so it does not get included in facet wrap
LvB_fits %>%
  unnest(data, fit_sh) %>%
  rename(Yrs = Reader_yrs) -> LvB_fits2

##### create scatter plot with fitted line for all reader year groups
LvB_fits %>%
  unnest(data, fit_sh) %>%
  ggplot()+
  geom_point(aes(x = tot_annuli, y = shell_height), position = "jitter", color = "grey40")+
  geom_line(data = LvB_fits2, aes(x = tot_annuli, y = fit_sh, color = Yrs), size = 1)+
  facet_wrap(~Reader_yrs)+
  labs(x = "Age", y = "Shell Height (mm)", color = "Year group") -> LvB_scat_YAK

#### create a residual plot for each reader, with year group 20006 - 2015 as the only model
LvB_fits %>%
  pull(LvB) %>%
  pluck(2) %>%
  coef() -> coefs

LvB_fits %>%
  unnest(data, fit_sh) %>%
  mutate(fit_sh_2006_2015 = coefs[1] * (1 - exp(-coefs[2] * (tot_annuli - coefs[3])))) %>%
  ggplot()+
  geom_point(aes(x = fit_sh_2006_2015, y = shell_height - fit_sh_2006_2015))+
  geom_hline(yintercept = 0, linetype = 2, color = "red")+
  facet_wrap(~Reader_yrs)+
  labs(x = "Residual Shell Height (mm)", y = "Fitted Shell Height (mm) (2006 - 2014 model)", color = "Year group") 

### jitter plots with fitted lines of shell height-at-age
png('./figures/age_reader/age_reader_fig4.png', width = 7, height = 8, units = "in", res = 300)
cowplot::plot_grid(LvB_scat_KNE, LvB_scat_KSH, LvB_scat_YAK, labels = c("a)", "b)", "c)"), nrow = 3)
dev.off()



## Shell height at the first annulus ====

### Sample numbers by reader 
age %>%
  filter(annulus == 1) %>%
  count(ager_id, ager_name)

### Sample numbers by district
age %>%
  filter(annulus == 1) %>%
  count(district)

### Number of 1st annulus measurements by data quality code. 
### 'B' = unusual location of first annulus
age %>%
  filter(annulus == 1) %>%
  count(code_let)

### Boxplot of SH at 1st annulus by reader in KNE, KSH, YAK 1992 - 2015

age_fa %>%
  filter(district %in% c("YAK", "KNE", "KSH"),
         cohort > 1992) %>%
  ggplot()+
  geom_boxplot(aes(x = factor(ager_id), y = annulus_height, fill = factor(ager_id)), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(district))+
  labs(x = NULL, y = "SH at 1st annulus (mm)", fill = "Reader")+
  theme(axis.text.x = element_blank())

### Boxplot of SH at 1st annulus by reader yr group in KNE, KSH, YAK 1992 - 2015
age_fa %>%
  filter(district %in% c("YAK", "KNE", "KSH"),
         cohort > 1992)  %>%
  ggplot()+
  geom_boxplot(aes(x = Reader_yrs, y = annulus_height, fill = Reader_yrs), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(district))+
  labs(x = NULL, y = "SH at 1st annulus (mm)", fill = "Year Group")+
  theme(axis.text.x = element_blank())

### Boxplot of SH at 1st annulus by reader yr group and bed within each district 1992 - 2015
age_fa %>%
  filter(district %in% c("KNE"),
         cohort > 1992)  %>%
  ggplot()+
  geom_boxplot(aes(x = Reader_yrs, y = annulus_height, fill = Reader_yrs), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(bed_code))+
  labs(x = NULL, y = "SH at 1st annulus (mm)", fill = "Year Group")+
  theme(axis.text.x = element_blank())

age_fa %>%
  filter(district %in% c("KSH"),
         cohort > 1992)  %>%
  ggplot()+
  geom_boxplot(aes(x = Reader_yrs, y = annulus_height, fill = Reader_yrs), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(bed_code))+
  labs(x = NULL, y = "SH at 1st annulus (mm)", fill = "Year Group")+
  theme(axis.text.x = element_blank())

age_fa %>%
  filter(district %in% c("YAK"),
         cohort > 1992)  %>%
  ggplot()+
  geom_boxplot(aes(x = Reader_yrs, y = annulus_height, fill = Reader_yrs), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(bed_code))+
  labs(x = NULL, y = "SH at 1st annulus (mm)", fill = "Year Group")+
  theme(axis.text.x = element_blank())

### Boxplots of SH at 1st annulus by reader yr group for summary document 
png('./figures/age_reader_fig5.png', width = 7, height = 5, units = "in", res = 300)
age_fa %>%
  filter(district %in% c("KNE", "KSH", "YAK"),
         cohort %in% c(1996 : 2005))  %>%
  ggplot()+
  geom_boxplot(aes(x = Reader_yrs, y = annulus_height, fill = Reader_yrs), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(district))+
  labs(x = NULL, y = "SH at 1st annulus (mm)", fill = "Year Group")+
  theme(axis.text.x = element_blank()) 
dev.off()

### Density function of height at first annullus by reader
age_fa %>%
  ggplot()+
  geom_density(aes(x = annulus_height, fill = factor(ager_id)), alpha = 0.5)+
  labs(x = "SH at 1st annulus (mm)", y = "Density", fill = "Reader")+
  facet_wrap(~district)

### Size at age of scallops by reader year group and cohort
age_fa %>%
  filter(district %in% c("KNE", "KSH", "YAK"),
         cohort %in% c(1996 : 2005))  %>%
  ggplot()+
  geom_jitter(aes(x = tot_annuli, y = shell_height, color = Reader_yrs), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(district))+
  labs(x = "Total Age", y = "Shell Height (mm)", fill = "Year Group") #scallops caught in earlier years were smaller, and younger...

## Shell height at intermediate annuli ====

age_int %>%
  count(annulus, Reader_yrs) %>%
  print(n = nrow(.))

### Boxplot of SH at 2nd annuli by reader group, district
age_int %>%
  filter(annulus == 2,
         district %in% c("KNE", "KSH", "YAK"),
         cohort %in% c(1996 : 2005))  %>%
  ggplot()+
  geom_boxplot(aes(x = Reader_yrs, y = annulus_height, fill = Reader_yrs), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(district))+
  labs(x = NULL, y = "SH at 2nd annulus (mm)", fill = "Year Group")+
  theme(axis.text.x = element_blank()) 

### Boxplot of SH at 4th annuli by reader group, district
age_int %>%
  filter(annulus == 4,
         district %in% c("KNE", "KSH", "YAK"),
         cohort %in% c(1996 : 2005))  %>%
  ggplot()+
  geom_boxplot(aes(x = Reader_yrs, y = annulus_height, fill = Reader_yrs), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(district))+
  labs(x = NULL, y = "SH at 2nd annulus (mm)", fill = "Year Group")+
  theme(axis.text.x = element_blank())

### Boxplot of SH at 6th annuli by reader group, district
age_int %>%
  filter(annulus == 6,
         district %in% c("KNE", "KSH", "YAK"),
         cohort %in% c(1996 : 2005))  %>%
  ggplot()+
  geom_boxplot(aes(x = Reader_yrs, y = annulus_height, fill = Reader_yrs), alpha = 0.5)+ 
  facet_grid(cols = vars(cohort), rows = vars(district))+
  labs(x = NULL, y = "SH at 2nd annulus (mm)", fill = "Year Group")+
  theme(axis.text.x = element_blank())

### Compare SH at first annulus to SH at intermediate annuli 
age_int %>%
  filter(annulus <= 13) %>%
  left_join(age_fa %>%
              select(annulus_height, shell_id) %>%
              rename(first_annulus_height = annulus_height), by = "shell_id") %>%
  ggplot()+
  geom_jitter(aes(x = first_annulus_height, y = annulus_height, color = Reader_yrs), alpha = 0.2)+
  geom_smooth(aes(x = first_annulus_height, y = annulus_height), method = "lm")+
  labs(x = "SH at first annulus (mm)", y = "SH at intermediate annulus (mm)")+
  facet_wrap(~annulus)
  
### Compare SH at first annulus to SH at intermediate annuli within cohort
age_int %>%
  filter(annulus %in% c(3:5),
         cohort %in% c(1996:2005)) %>% #only a subset will fit in the plot window
  left_join(age_fa %>%
              select(annulus_height, shell_id) %>%
              rename(first_annulus_height = annulus_height), by = "shell_id") %>%
  ggplot()+
  geom_jitter(aes(x = first_annulus_height, y = annulus_height, color = Reader_yrs), alpha = 0.2)+
  labs(x = "SH at first annulus (mm)", y = "SH at intermediate annulus (mm)")+
  facet_grid(cols = vars(annulus), rows = vars(cohort))

### fit lm models to SH at first annulus and subsequent annuli by district
#### define functions
f_ols_mod <- function(x){lm(annulus_height ~ first_annulus_height, data = x)}
f_etxract_p <- function (x){
  f <- summary(x)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
f_extract_slope <- function(x){coef(x)[2]}
f_slope_coef_ci <- function(x){
  upp <- round(x$coefficients[2,1] + x$coefficients[2,2] * qnorm(0.975), 2)
  lwr <- round(x$coefficients[2,1] + x$coefficients[2,2] * qnorm(0.025), 2)
  paste0("(", lwr, ", ", upp, ")")
}
                                
#### fit models
age_int %>%
  filter(annulus %in% c(2:10),
         district %in% c("KNE", "KSH", "YAK")) %>% 
  left_join(age_fa %>%
              select(annulus_height, shell_id) %>%
              rename(first_annulus_height = annulus_height), by = "shell_id") %>%
  nest(-annulus, -district) %>%
  mutate(ols_mod = map(data, f_ols_mod),
         summary = map(ols_mod, summary),
         r2 = map_dbl(summary, "r.squared"),
         rse = map_dbl(summary, "sigma"),
         p = map_dbl(ols_mod, f_etxract_p),
         coef = map_dbl(ols_mod, f_extract_slope),
         coef_ci = map_chr(summary, f_slope_coef_ci)) -> ols_mods

ols_mods %>%
  select(annulus, district, r2, rse, p, coef, coef_ci) %>%
  write.csv("./tables/age_reader/age_reader_ols_mod_table3.csv", row.names = F)

#### plot annulus_height ~ first_annulus_height w/ fit
#### include loess curve to evaluation linear trend compared to non-linear trend
png('./figures/age_reader/age_reader_fig6.png', width = 7, height = 5, units = "in", res = 300)
age_int %>%
  filter(annulus %in% c(2:10),
         district %in% c("KNE", "KSH", "YAK")) %>% 
  left_join(age_fa %>%
              select(annulus_height, shell_id) %>%
              rename(first_annulus_height = annulus_height), by = "shell_id") %>%
  ggplot()+
  geom_jitter(aes(x = first_annulus_height, y = annulus_height), color = "grey40", alpha = 0.2)+
  geom_smooth(aes(x = first_annulus_height, y = annulus_height), method = "lm", se = F, color = "black", size = 0.7)+
  #geom_smooth(aes(x = first_annulus_height, y = annulus_height), method = "loess", se = F, color = "red")+
  geom_hline(yintercept = 100, linetype = 2, size = 0.5)+
  scale_x_continuous(breaks = seq(10, 50, 5), labels = c("", "", 20, "", "", 35, "", "", 50))+
  labs(x = "SH at first annulus (mm)", y = "SH at intermediate annulus (mm)")+
  facet_grid(cols = vars(annulus), rows = vars(district))+
  theme(legend.position = "bottom")
dev.off()

#### fitted-residual plot
f_fit_resid_plot <- function(x){
  resid <- resid(x)
  fitted <- fitted(x)
  ggplot()+
    geom_jitter(aes(x = fitted, y = resid))+
    geom_hline(yintercept = 0, linetype = 2)+
    labs(x = "Fitted Values", y = "Residuals")
}

ols_mods %>%
  mutate(resid_plot = map(ols_mod, f_fit_resid_plot)) %>%
  pull(resid_plot) -> resid_plot

cowplot::plot_grid(plotlist = resid_plots, labels = c(1:27)) # doesn't look like any serious issues



