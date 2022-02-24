# Helper file for scallop observer summary
# ben.williams@alaska.gov


# NOTE: Neither catch data nor catch output can be pushed to github due to confidentiality! 

# last updated 2019-04


# load ----
library(tidyverse)
library(lubridate)
library(magrittr)
library(gtools)
library(PBSmapping)
library(mapproj)
library(mgcv)
library(zoo)
library(gridExtra)
# devtools::install_github("ben-williams/FNGr")
library(FNGr)
library(xtable)
library(here)
library(scales)

theme_set(theme_sleek())

# map data ----

#load PBSmapping data set for N Pacific - much better resolution than world highres...
data('nepacLLhigh')

nepacLLhigh %>% 
  dplyr::select(group = PID, POS = POS, long = X, lat = Y) -> ak
rm(nepacLLhigh)
