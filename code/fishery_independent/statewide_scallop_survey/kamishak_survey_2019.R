# notes ----
## Abundance and biomass estimates from Kamishak survey 2019
## This script analyzes data from the 2019 survey, by request from Mike Byrely (ADF&G Homer), prior to it being made available in the database
## author: Tyler Jackson
## last updated: 2020/5/7

# load ----
library(tidyverse)

# data ----

## Shell height and damage data
shad <- read_csv("./data/statewide_scallop_survey/kamishak_2019_notfromdatabase/Cruise1902_SHAD.csv")

## Shell height and weight data
shaw <- read_csv("./data/statewide_scallop_survey/kamishak_2019_notfromdatabase/Cruise1902_SHAW.csv")

## crab data
crab <- read_csv("./data/statewide_scallop_survey/kamishak_2019_notfromdatabase/Cruise1902_Crab.csv")

# data mgmt ----

## rename data object columns
names(shad) <- c("cruise", "app", "serial", "date", "time", "haul", "sample_group", "shell_number",
                 "sh", "shell_damage")
names(shaw) <- c("order", "cruise", "app", "serial", "date", "time", "haul", "sample_group", "shell_number",
                 "meat_weight", "round_weight", "sex", "gonad", "meat_condition", "sh", "mud_blisters",
                 "shell_worms", "shell_damage", "sample_collected")



names(crab) <- c("cruise", "app", "serial", "date", "time", "haul", "")