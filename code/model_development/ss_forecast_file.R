# notes ----
## write forecast file for stock synthesis
## Tyler Jackson
## last updated 11/4/2020

# load ---- 
library(tidyverse)
library(r4ss)

# SS_readforecast("./code/stock_synthesis/ksh/forecast.ss")

## clear env so you don't introduce unwanted object
rm(list = ls())

# version control ----

sourcefile <- "./code/stock_synthesis/ksh/forecast.ss"
type <- "Stock_Synthesis_forecast_file"
SSversion <- "3.30"

# forecast settings ----

## compute benchmarks
benchmarks <- 0 # omit

## msy estimation method
MSY <- 1 # Fspr as a proxy

## SPR target value
SPRtarget <- 0.45 # from manual - update later

## realtive biomass target
Btarget <- 0.4 # from manual

## benchmark years
Bmark_years <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

## benchmark relative F basis
Bmark_relF_Basis <- 2 # set range for relF the same as the forecast

## forecast
Forecast <- -1 # none

# list objects and write file ----

as.list(environment()) %>%
  SS_writeforecast("./code/stock_synthesis/ksh", overwrite = T, verbose = T)
