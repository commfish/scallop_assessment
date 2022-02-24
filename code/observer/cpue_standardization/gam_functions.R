# notes ----
# libraries and functions used for GAM model diagnostics
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# date: 2019-11-11

# libraries
library(tidyverse)
library(GGally)
library(mgcViz)
library(cowplot)
library(lubridate)
library(mgcv)
library(FNGr); theme_set(theme_sleek())

# extract estimated degrees of freedom
# argument x: gam or bam object created in mcgv
f_edf <- function(x){sum(x$edf)}

# extract deviance explained
# argument x: gam or bam object created in mcgv
f_dev <- function(x){summary(x)$dev.expl * 100}

# extract GCV score
# argument x: gam or bam object created in mcgv
f_gcv <- function(x){x$gcv}

# extract model paramaterization
# argument x: gam or bam object created in mcgv
f_gam_form <- function(x){as.character(x$formula)[3]}

# plot edf of each covariate in sample data models
f_smooth_func_edf <- function(x){
  summary <- summary(x)
  summary$edf
  summary
  
  plot(x, residuals = T)
  gam.check(x)

} 


f_predict <- function(x){
  predict(x, type = "response")
}




