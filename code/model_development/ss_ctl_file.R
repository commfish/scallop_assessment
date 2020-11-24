# notes ----

## Edit .ctl file (i.e. use KAMN inputs as guide)
## Tyler Jackson
## tyler.jackson@alska.gov
## 10/29/2020

library(r4ss)
library(tidyverse)

ctl <- SS_readctl("./code/stock_synthesis/SSI/default/simple_ctl.ss")

SS_readctl("./code/stock_synthesis/test/ksh_ctl.ctl")

SS_readctl("./code/stock_synthesis/kamishak/ks1.ctl", version = "3.24")

## clear env so you don't introduce unwanted object
rm(list = ls())

# begining of ctl ----

## use weight at age data (pg 73 of SS 3.30 user manual)
type <- "Stock_Synthesis_control_file"

ReadVersion <- "3.30"

eof <- FALSE

EmpiricalWAA <- 0

N_areas <- 1

# platoon info ----

## number of growth pattenrs (pg 74 of SS 3.30 user manual)
N_GP <- 1

## number of platoons within a growth pattern (pg 74 of SS 3.30 user manual)
N_platoon <- 1

## ratio in amount of variability between platoons to within platoons (pg 74 of SS 3.30 user manual)
sd_ratio <- 1

## distribution among platoons (pg 74 of SS 3.30 user manual)
submorphdist <- 1

# recruitment ----

## recruitment distribution method (pg 75 of SS 3.30 user manual)
recr_dist_method <- 4

## spawner-recruitment options (not implemented) (pg 75 of SS 3.30 user manual)
recr_global_area <- 1

## number of recrutment settlement assignments (pg 76 of SS 3.30 user manual)
recr_dist_read <- 1

## recr_dist_inx (unused option)
recr_dist_inx <- 0

## recruitment pattern info (pg 76 of SS 3.30 user manual)
recr_dist_pattern <- data.frame(GPattern = 1,
                                month = 1, 
                                area = 1,
                                age = 0)

# migration ----
## not included when areas = 1 (pg 78 of SS 3.30 user manual)
# time blocks ----

## number of time blocks (pg 80 of SS 3.30 user manual)
N_Block_Designs <- 0 # no time blocks
blocks_per_pattern <- 0
## auto generation
time_vary_adjust_method <- 1
time_vary_auto_generation <- c(1, 1, 1, 1, 1)


# natural mortality ----

# set number of M parameters
natM_type <- 0 # 1 parameter (pg 81 of SS 3.30 user manual)

# growth and maturity ----

## choose growth model (pg 84 of SS 3.30 user manual)
GrowthModel <- 1 #von Bertalanffy growth

# reference age for 1st size-at-age parameter
Growth_Age_for_L1 <- 0

# reference age for 2nd size-at-age parameter
Growth_Age_for_L2 <- 18

# expontial decay for growth above maximum age
Exp_Decay <- -999 # (pg 84 of SS 3.30 user manual)

# placeholder for paramter in future verison (pg 84 of SS 3.30 user manual)
Growth_Placeholder <- 0

# standard deviation added to length-at-age:
SD_add_to_LAA <- 0 # (pg 85 of SS 3.30 user manual)

# cv pattern (pg 85 of SS 3.30 user manual)
CV_Growth_Pattern <- 1

## nature of maturity function (starts pg 85 of SS 3.30 user manual)
maturity_option <- 1 # length logistic

## age at first maturity
First_Mature_Age <- 1

## fecundity function
fecundity_option <- 1

## hermaphroditism option
hermaphroditism_option <- 0 # none

## sex ratio at recruitment - fixed at 0.5 (default)

## parameter offset method 
parameter_offset_approach <- 1 # none

## growth parameter settings (from kamishak ctl file)
read_csv("./data/stock_synthesis/ksh/growth_maturity_parameter_info.csv") %>%
  column_to_rownames(., var = "param") %>%
  as.data.frame() -> MG_parms

## seasonal biology parameters (starts pg 98 of SS 3.30 user manual)
MGparm_seas_effects <- rep(0, 10) 
names(MGparm_seas_effects) <- c("femwtlen1", "femwtlen2", "mat1", "mat2", "fec1", "fec2",
                                "Malewtlen1", "malewtlen2", "L1", "K")

# spawner recruitment ---- 

## spawner recruitment function (starts pg 99 of SS 3.30 user manual)
SR_function <- 4 
Use_steep_init_equi <- 0
Sigma_R_FofCurvature <- 0

## sr parameter setup
read_csv("./data/stock_synthesis/ksh/spawner_recruitment_parameter_info.csv") %>%
  column_to_rownames(., var = "param") %>%
  as.data.frame() -> SRparm

## recruitment deviations (starts pg 105 of SS 3.30 user manual) 
### do recruitment deviations (0 - 4)
do_recdev <- 1
### main recruitment deviations begin year
MainRdevYrFirst <- 1966 # from KAMN ctl
### main recruitment deviations end year
MainRdevYrLast <- 2019 # last year of the model
### main recruitment deviations phase
recdev_phase <- 2
### advanced options
recdev_adv <- 1 # use advanced options - next 11 objects
### early recruitment deviation start year
recdev_early_start <- 0
### early recruitment deviation phase
recdev_early_phase <- -4
### forecast recruitment phase
Fcast_recr_phase <- 0
### forecast recruitment deviations lambda
lambda4Fcast_recr_like <-  1
### last year with no bias adjustment
last_early_yr_nobias_adj <- 1990
### first year with full bias adjustment
first_yr_fullbias_adj <- 1990
### last year with full biass adjustment
last_yr_fullbias_adj <- 2015
### first recent year with no bias adjustment
first_recent_yr_nobias_adj <- 2016
### maximum bias adjustment
max_bias_adj <- 1
### period for recruitment cycles
period_of_cycles_in_recr <- 0
### minimum recruitment deviation
min_rec_dev <- -5
### maximum recruitment deviation
max_rec_dev <- 5
### number of recruitment deviations to read
N_Read_recdevs <- 0


# fishing mortality ----

## f ballpark (pg 111 of SS 3.30 user manual)
F_ballpark <- 0.15
F_ballpark_year <- -2019 # negative value to disable

## f method (pg 112 of SS 3.30 user manual)
F_Method <- 3 # recommended hybrid method

## max f
maxF <- 2.9 # from KAMN ctl

# number of iterations for tuning f in hybrid method 
F_iter <- 4

# inital f parameters
tibble(LO = 0, 
       HI = 1, 
       INIT = 0, 
       PROR = 0.01, 
       SD = 99,
       PR_type = 0, 
       PHASE = -1, 
       env_var = 0,
       use_dev = 0,
       dev_mnyr = 0,
       dev_mxyr = 0,
       dev_PH = 0,
       Block = 0,
       Blk_Fxn = 0,
       param = "InitF_1FISHERY1") %>%
  column_to_rownames(., var = "param") %>%
  as.data.frame() -> init_F


# catchability ----

## Q set up (pg 114 of SS 3.30 user manual)
### enter one line for every fleet with an abundance index
Q_options <- tibble(fleet = c(2, 3),
                        link = c(1, 2), 
                        link_info = c(0, 2),
                        extra_se = c(1, 0), 
                        biasadj = c(0, 0),
                        float = c(0, 0),
                        fleetname = c("DREDGESURVEY", "TRAWLSURVEY")) %>%
  column_to_rownames(., var = "fleetname") %>%
  as.data.frame 


### catability parameters (likely needs adjustment)
tibble(LO = c(0.2, 0),
       HI = c(1, 0.5),
       INIT = c(0.83, 0),
       PRIOR = c(0.83, 0.05),
       PR_SD = c(0.1, 0),
       PR_type = c(-1, 1),
       PHASE = c(5, -4),
       env_var = c(0, 0),
       use_dev = c(0, 0),
       dev_mnyr = c(0, 0),
       dev_mxyr = c(0, 0),
       dev_PH = c(0, 0),
       Block = c(0, 0),
       Blk_Fxn = c(0, 0)) %>%
  # add row for mirror parameters (trawl survey)
  bind_rows(., .) %>%
  mutate(param = c("LnQ_base_DREDGESURVEY", "Q_extaSD_DREDGESURVEY", "LnQ_base_TRAWLSURVEY", "Q_extaSD_TRAWLSURVEY")) %>%
  column_to_rownames(., var = "param") %>%
  as.data.frame -> Q_params


# selectivity ----

## size selectivty definitions for each fleet (pg 118-119 of SS 3.30 user manual)
tibble(Pattern = c(1, 1, 5),
       Discard = c(0, 0, 0),
       Male = c(0, 0, 0),
       Special = c(0, 0, 2),
       fleet = c("FISHERY1", "DREDGESURVEY", "TRAWLSURVEY")) %>%
  column_to_rownames(., var = "fleet") %>%
  as.data.frame() -> size_selex_types

## size selectivity parameters (not time varying)
tibble(LO = c(2, 0.01, 2, 0.01, 2, 0.01),
       HI = c(20, 8, 45, 15, 45, 15), 
       INIT = c(13, 3.5, 10.5, 6.5, 10.5, 6.5),
       PRIOR = c(13, 3.5, 10.5, 6.5, 10.5, 6.5),
       PR_SD = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01),
       PR_type = c(1, 1, 1, 1, 1, 1),
       PHASE = c(2, 3, 4, 5, 4, 5),
       env_var = c(0, 0, 0, 0, 0, 0),
       use_dev = c(0, 0, 0, 0, 0, 0),
       dev_mnyr = c(0, 0, 0, 0, 0, 0),
       dev_mxyr = c(0, 0, 0, 0, 0, 0),
       dev_PH = c(0, 0, 0, 0, 0, 0),
       Block = c(0, 0, 0, 0, 0, 0),
       Blk_Fxn = c(0, 0, 0, 0, 0, 0),
       param = c("SizeSel_P1_FISHERY1", "SizeSel_P2_FISHERY1", "SizeSel_P1_DREDGESURVEY", 
                 "SizeSel_P2_DREDGESURVEY", "SizeSel_P1_TRAWLSURVEY", "SizeSel_P2_TRAWLSURVEY")) %>%
  column_to_rownames(., var = "param") %>%
  as.data.frame -> size_selex_parms

## age selectivty definitions for each fleet (pg 119 of SS 3.30 user manual)
tibble(Pattern = c(12, 12, 15),
       Discard = c(0, 0, 0),
       Male = c(0, 0, 0),
       Special = c(0, 0, 2),
       fleet = c("FISHERY1", "DREDGESURVEY", "TRAWLSURVEY")) %>%
  column_to_rownames(., var = "fleet") %>%
  as.data.frame() -> age_selex_types

## age selectivity parameters (not time varying)
tibble(LO = c(2, 0.01, 2, 0.01),
       HI = c(8, 8, 8, 12), 
       INIT = c(5, 3.5, 4, 3.5),
       PRIOR = c(5, 3.5, 4, 3.5),
       PR_SD = c(0.05, 0.05, 0.05, 0.05),
       PR_type = c(1, 1, 1, 1),
       PHASE = c(4, 5, 4, 5),
       env_var = c(0, 0, 0, 0),
       use_dev = c(0, 0, 0, 0),
       dev_mnyr = c(0, 0, 0, 0),
       dev_mxyr = c(0, 0, 0, 0),
       dev_PH = c(0, 0, 0, 0),
       Block = c(0, 0, 0, 0),
       Blk_Fxn = c(0, 0, 0, 0),
       param = c("AgeSel_P1_FISHERY1", "AgeSel_P2_FISHERY1", "AgeSel_P1_DREDGESURVEY", "AgeSel_P2_DREDGESURVEY")) %>%
  column_to_rownames(., var = "param") %>%
  as.data.frame -> age_selex_parms

# use two-dimensional autoregressive selectivity
Use_2D_AR1_selectivity <- 0

# tagging, var_adjust, lambdas ----

## tag loss and tag reporting parameters go next
TG_custom <- 0 # no tagging

## input variance adjustments factors
DoVar_adjust <- 0 # not read in SS_writectl_3.30 (append to ctl file afterwards)


## lambdas
### max lambda phase
maxlambdaphase <- 4
### sd offset
sd_offset <- 1
### number of lambdas to change from default value of 1
N_lambdas <- 3
### lambdas
lambdas <- data.frame(like_comp = c(1, 4, 4),
                      fleet = c(2, 2, 2),
                      phase = c(2, 2, 3),
                      value = c(1, 1, 1), 
                      sizefreq_method = c(1, 1, 1))

# additional std dev reporting
more_stddev_reporting <- 0


# list objects and write file ----

as.list(environment()) %>%
  SS_writectl("./code/stock_synthesis/ksh/ksh_ctl.ss", overwrite = T, verbose = T)
