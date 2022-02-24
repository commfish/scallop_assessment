# notes ----
## write starter file for stock synthesis
## Tyler Jackson
## last updated 11/4/2020

# load ---- 
library(tidyverse)
library(r4ss)


## clear env so you don't introduce unwanted object
rm(list = ls())

# version control ----
sourcefile <- "./code/stock_synthesis/ksh/starter.ss"
type <- "Stock_Synthesis_starter_file"
SSversion <- "3.30"

# .dat and .ctl files ----
datfile <- "ksh_dat.ss"
ctlfile <- "ksh_ctl.ss"

# settings ----
## (pg 9 SS 3.30.16 user manual)

## initial parameter values
init_values_src <- 0 # use values in control file

## run display details
run_display_detail <- 1 # one brief line of display for each iteration

## detailed age-structured report 
detailed_age_structure <- 1 # write a full report file

## write 1st iteration details
checkup <- 0 # omit

## parameter trace
parmtrace <- 0 # omit

## cumulative report
cumreport <- 1 # brief summary report

## full priors
prior_like <- 1 # calculate priors for all parameters that have a defined prior

## soft bounds 
soft_bounds <- 1 # allow penalty help selectivity parameters from hittin bounds

## data file output
N_bootstraps <- 1 # outputs add to data.ss_new file

## turn off estimation
last_estimation_phase <- 99 # exit model after completing this estimation phase 
# (should be max phase in parameter set up)

## number of iterations to discard at the start of an mcmc run
MCMCburn <- 1000

## number of iterations to remove between the main period of the mcmc run
MCMCthin <- 200

## add jittering to parameter initial values
jitter_fraction <- 0 # no jitter

## sd report start and end
minyr_sdreport <- -1 # begin the start year of model
maxyr_sdreport <- -1 # end the last year of model

## extra sd report years
N_STD_yrs <- 0 # none, manual says '0', but length must be 0
STD_yr_vec <- NULL


## reasonable default value for the change in log likelihood denoting convergence
converge_criterion <- 0.0001

## adjust model end year, disregard data after this year
retro_yr <- 0

## summary biomass minimum age
min_age_summary_bio <- 0 # include all ages

## depletion basis
depl_basis <- 0 # skip

## fraction for depletion denominator
depl_denom_frac <- 1

## spr report basis
SPR_basis <- 0 # skip

## annual f units
F_report_units <- 0 # skip

## denomination when reporting F
F_report_basis <- 0 # report raw values

## format of mcmc output
MCMC_output_detail <- 0.001 # default format with small bump to log(R0)

## age-length-key tolerance level
ALK_tolerance <- 0 # no compression

## final (?...)
final <- 3.3 # not sure, possibly relates to version

# list objects and write file ----

as.list(environment()) %>%
  SS_writestarter("./code/stock_synthesis/ksh", overwrite = T, verbose = T)
