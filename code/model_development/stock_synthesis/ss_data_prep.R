# notes ----
# stock synthesis fishery data prep, new 2023
# tyler jackson
# last update 12/23/2022

# load ----

library(tidyverse)

# fishery retained catch ----

read_csv("./data/observer/old_catch/ksh_fishery_table_1993_2020.csv") %>%
  transmute(year = as.numeric(substring(season, 1, 4)),
            season = 1, 
            fleet = 1,
            catch = round(ret_lbs_rnd * 0.000453592, 2),
            catch_se = 0.01) %>% filter(year <= 2008) %>%
  bind_rows(read_csv("./output/observer/2023/fish_stats_KSH.csv") %>%
              transmute(year = as.numeric(substring(season, 1, 4)),
                        season = 1, 
                        fleet = 1,
                        catch = round(rnd_wt * 0.000453592, 2),
                        catch_se = 0.01)) %>%
  # remove years with no catch
  filter(!is.na(catch)) %>%
  # save
  write_delim("./data/model_development/stock_synthesis/2023/catch.txt", delim = "\t")
  
  

# survey biomass ----

read_csv("./output/fishery_independent/statewide_scallop_survey/2022/2022_abundance_rnd_biomass_by_bed.csv") %>%
  filter(district == "KSH", bed_name == "KSH1") %>%
  group_by(year) %>%
  # compute cv for all sizes
  mutate(cv = sum(cv_biomass * biomass) / sum(biomass)) %>%
  summarise(month = 2, 
            fleet = 2,
            obs = round(sum(biomass) * 0.000453592, 2),
            log_se = round(sqrt(log(1 + mean(cv)^2)), 2),
            `#_fleet_name` = "#_DREDGE") %>%
  # save
  write_delim("./data/model_development/stock_synthesis/2023/dredge_index.txt", delim = "\t")

# fishery nominal cpue 1993 - 2009 ----

read_csv("./data/observer/old_catch/ksh_fishery_table_1993_2020.csv") %>%
  transmute(year = as.numeric(substring(season, 1, 4)),
            month = 7 - 3, 
            fleet = 4,
            obs = round(rnd_cpue * 0.000453592, 3),
            log_se = 0.40,
            `#_fleet_name` = "#_FISHERYCPUEnom") %>%
  filter(year <= 2008,
         !is.na(obs)) %>%
  # save
  write_delim("./data/model_development/stock_synthesis/2023/fishery_nom_index.txt", delim = "\t")

# fishery cpue 2009 - present ----

read_csv("./output/observer/2023/ksh_std_cpue.csv") %>%
  left_join(read_csv("./output/observer/2023/fishery_avg_month.csv") %>%
              filter(district == "KSH") %>%
              transmute(year = as.numeric(substring(season, 1, 4)), month)) %>%
  transmute(year,
            month = month - 3, 
            fleet = 3,
            obs = round(index, 3),
            log_se = round(sqrt(log(1 + cv^2)), 2),
            `#_fleet_name` = "#_FISHERYCPUE") %>%
  # save
  write_delim("./data/model_development/stock_synthesis/2023/fishery_std_index.txt", delim = "\t")


# fishery discards ----

# discard mortality from the 2016 SAFE (Table 1-9)
tibble(year = 1996:2008,
       month = 4,
       fleet = 1,
       obs = round(c(4018, 1900, 4409, 5907, 2621, 4880, 10120, 8209,
                     8883, 4767, 4789, 7685, 658) / 0.102 / 0.2 * 0.000453592, 
                   2),
       log_se = 0.1,
       fleet_name = "#_FISHERY") %>%
  # total fishery discards since 2009 (rnd wt)
  bind_rows(read_csv("./output/observer/2023/discard_summary_KSH.csv") %>%
              left_join(read_csv("./output/observer/2023/fishery_avg_month.csv") %>%
                          filter(district == "KSH")) %>%
              transmute(year = as.numeric(substring(season, 1, 4)),
                        month = month - 3,
                        fleet = 1, 
                        obs = round(discard_lb * 0.000453592, 2),
                        log_se = 0.1,
                        fleet_name = "#_FISHERY")) -> tot_disc
# save total discards
write_delim(tot_disc, "./data/model_development/stock_synthesis/2023/total_discards.txt", delim = "\t")

# save discard mortality
tot_disc %>%
  mutate(obs = round(obs * 0.2, 2)) %>% 
  write_delim("./data/model_development/stock_synthesis/2023/dead_discards.txt", delim = "\t")

  
# survey size composition ----

## read data from survey analysis
read_csv("./output/fishery_independent/statewide_scallop_survey/2022/2022_size_comp_pmf.csv") %>%
  filter(bed_name == "KSH1",
         shell_height >= 20) %>%
  # add sh bin 
  mutate(sh = sprintf("%.1f", floor(shell_height / 5) * 5 / 10),
         bin = ifelse(shell_height > 180, "l18.0", paste0("l",sh))) %>%
  group_by(year, bin, nmeas) %>%
  summarise(prop =  sprintf("%.4f", sum(prop))) %>%
  # format
  pivot_wider(names_from = bin, values_from = prop) %>%
  transmute(year, 
            month = 2,
            fleet = 2, 
            sex = 0, part = 0,
            Nsamp = round(min(0.1*nmeas, 100)),
            l2.0, l2.5, l3.0, l3.5, l4.0, l4.5, l5.0, l5.5, l6.0, l6.5, l7.0, l7.5, l8.0, l8.5, l9.0, l9.5, l10.0,
            l10.5, l11.0, l11.5, l12.0, l12.5, l13.0, l13.5, l14.0, l14.5, l15.0, l15.5, l16.0, l16.5, l17.0, l17.5,
            l18.0) %>%
  replace(is.na(.), sprintf("%.4f", 0.0000)) %>%
  # save
  write_delim(., "./data/model_development/stock_synthesis/2023/dredge_size_comp.txt", delim = "\t")

# fishery size composition all ----

# load fishery size composition weights
read_csv("./output/observer/2023/fishery_size_comp_wts.csv") %>%
  filter(district == "KSH",
         sh >= 20) %>%
  # add year 
  mutate(year = as.numeric(substring(season, 1, 4)),
         sh_bin = sprintf("%.1f", floor(sh / 5) * 5 / 10),
         bin = ifelse(sh > 180, "l18.0", paste0("l", sh_bin))) %>% 
  # add number measured
  group_by(year) %>%
  mutate(nmeas = n()) %>%
  # summarize weights
  group_by(year, bin, nmeas) %>%
  summarise(prop =  sprintf("%.4f", sum(w))) %>%
  # join to unobserved sizes
  right_join(expand_grid(year = 2009:2022,
                         bin = paste0("l", sprintf("%.1f", seq(2, 18 , 0.5))))) %>%
  replace_na(list(prop = "0.0000")) %>%
  # refill missing measured number
  group_by(year) %>% mutate(nmeas = mean(nmeas, na.rm = T)) %>%
  # join to month of fishery
  left_join(read_csv("./output/observer/2023/fishery_avg_month.csv") %>%
            filter(district == "KSH") %>%
            transmute(year = as.numeric(substring(season, 1, 4)), month)) %>%
  # format
  pivot_wider(names_from = bin, values_from = prop) %>%
  transmute(year,
            month = month - 3,
            fleet = 1, 
            sex = 0, part = 0,
            Nsamp = round(min(0.1*nmeas, 100)),
            l2.0, l2.5, l3.0, l3.5, l4.0, l4.5, l5.0, l5.5, l6.0, l6.5, l7.0, l7.5, l8.0, l8.5, l9.0, l9.5, l10.0,
            l10.5, l11.0, l11.5, l12.0, l12.5, l13.0, l13.5, l14.0, l14.5, l15.0, l15.5, l16.0, l16.5, l17.0, l17.5,
            l18.0) %>%
  # save
  write_delim(., "./data/model_development/stock_synthesis/2023/fishery_size_comp.txt", delim = "\t")

# fishery size composition retained ----

# load fishery size composition weights
read_csv("./output/observer/2023/fishery_size_comp_wts.csv") %>%
  filter(district == "KSH",
         sh >= 20,
         rtnd_disc == "R") %>%
  # add year 
  mutate(year = as.numeric(substring(season, 1, 4)),
         sh_bin = sprintf("%.1f", floor(sh / 5) * 5 / 10),
         bin = ifelse(sh > 180, "l18.0", paste0("l", sh_bin))) %>% 
  # add number measured
  group_by(year) %>%
  mutate(nmeas = n()) %>%
  # summarize weights
  group_by(year, bin, nmeas) %>%
  summarise(w =  sum(w)) %>%
  group_by(year) %>%
  mutate(prop = sprintf("%.4f", w / sum(w))) %>%
  # join to unobserved sizes
  right_join(expand_grid(year = 2009:2022,
                         bin = paste0("l", sprintf("%.1f", seq(2, 18 , 0.5))))) %>%
  replace_na(list(prop = "0.0000")) %>%
  dplyr::select(-w) %>%
  # refill missing measured number
  group_by(year) %>% mutate(nmeas = mean(nmeas, na.rm = T)) %>%
  # join to month of fishery
  left_join(read_csv("./output/observer/2023/fishery_avg_month.csv") %>%
              filter(district == "KSH") %>%
              transmute(year = as.numeric(substring(season, 1, 4)), month)) %>%
  # format
  pivot_wider(names_from = bin, values_from = prop) %>%
  transmute(year,
            month = month - 3,
            fleet = 1, 
            sex = 0, part = 2,
            Nsamp = round(min(0.1*nmeas, 100)),
            l2.0, l2.5, l3.0, l3.5, l4.0, l4.5, l5.0, l5.5, l6.0, l6.5, l7.0, l7.5, l8.0, l8.5, l9.0, l9.5, l10.0,
            l10.5, l11.0, l11.5, l12.0, l12.5, l13.0, l13.5, l14.0, l14.5, l15.0, l15.5, l16.0, l16.5, l17.0, l17.5,
            l18.0) %>%
  # save
  write_delim(., "./data/model_development/stock_synthesis/2023/fishery_size_comp_retained.txt", delim = "\t")

# fishery size composition discarded ----

# load fishery size composition weights
read_csv("./output/observer/2023/fishery_size_comp_wts.csv") %>%
  filter(district == "KSH",
         sh >= 20,
         rtnd_disc == "D") %>%
  # add year 
  mutate(year = as.numeric(substring(season, 1, 4)),
         sh_bin = sprintf("%.1f", floor(sh / 5) * 5 / 10),
         bin = ifelse(sh > 180, "l18.0", paste0("l", sh_bin))) %>% 
  # add number measured
  group_by(year) %>%
  mutate(nmeas = n()) %>%
  # summarize weights
  group_by(year, bin, nmeas) %>%
  summarise(w =  sum(w)) %>%
  group_by(year) %>%
  mutate(prop = sprintf("%.4f", w / sum(w))) %>%
  # join to unobserved sizes
  right_join(expand_grid(year = 2009:2022,
                         bin = paste0("l", sprintf("%.1f", seq(2, 18 , 0.5))))) %>%
  replace_na(list(prop = "0.0000")) %>%
  dplyr::select(-w) %>%
  # refill missing measured number
  group_by(year) %>% mutate(nmeas = mean(nmeas, na.rm = T)) %>%
  # join to month of fishery
  left_join(read_csv("./output/observer/2023/fishery_avg_month.csv") %>%
              filter(district == "KSH") %>%
              transmute(year = as.numeric(substring(season, 1, 4)), month)) %>%
  # format
  pivot_wider(names_from = bin, values_from = prop) %>%
  transmute(year,
            month = month - 3,
            fleet = 1, 
            sex = 0, part = 1,
            Nsamp = round(min(0.1*nmeas, 100)),
            l2.0, l2.5, l3.0, l3.5, l4.0, l4.5, l5.0, l5.5, l6.0, l6.5, l7.0, l7.5, l8.0, l8.5, l9.0, l9.5, l10.0,
            l10.5, l11.0, l11.5, l12.0, l12.5, l13.0, l13.5, l14.0, l14.5, l15.0, l15.5, l16.0, l16.5, l17.0, l17.5,
            l18.0) %>%
  # save
  write_delim(., "./data/model_development/stock_synthesis/2023/fishery_size_comp_discarded.txt", delim = "\t")

# survey age composition ----

tibble(bin = 1:33,
       sh_bin = seq(2.0, 18.0, 0.5)) -> pop_bin

read_csv("./output/fishery_independent/statewide_scallop_survey/2022/2022_conditional_age_comp.csv") %>%
  filter(bed_name == "KSH1", 
         !is.na(shell_height)) %>%
  # add age bin and size bin
  mutate(age_bin = ifelse(age < 18, paste0("a", age), "a18"),
         sh_bin = ifelse(shell_height > 180, 
                         18, 
                         floor(shell_height / 5) * 5 / 10)) %>%
  # compute total aged in sh bin
  group_by(year, sh_bin) %>%
  mutate(n_aged = n()) %>%
  # get number by age within sh bins
  group_by(year, sh_bin, age_bin, n_aged) %>%
  summarise(prop = sprintf("%.4f", n() / mean(n_aged))) %>%
  # pivot wider
  pivot_wider(names_from = age_bin, values_from = prop) %>%
  ungroup %>%
  left_join(pop_bin) %>%
  transmute(year, 
            month = 2,
            fleet = 2, 
            sex = 0, part = 0, ageerr = 1, Lbin_lo = bin, Lbin_hi = bin,
            Nsamp = n_aged,
            a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) %>%
  replace(is.na(.), sprintf("%.4f", 0.0000)) %>%
  # save
  write_delim(., "./data/model_development/stock_synthesis/2023/dredge_age_comp.txt", delim = "\t")

# fishery age composition ----

tibble(bin = 1:33,
       sh_bin = seq(2.0, 18.0, 0.5)) -> pop_bin

# conditional age composition, no partition
read_csv("./output/observer/2023/conditional_age_composition.csv") %>%
  # filter for ksh
  filter(district == "KSH") %>%
  # add age bin and size bin
  mutate(age_bin = ifelse(age < 18, paste0("a", age), "a18"),
         year = as.numeric(substring(season, 1, 4)),
         sh_bin = ifelse(shell_height > 180, 
                         18, 
                         floor(shell_height / 5) * 5 / 10)) %>%
  # compute total aged in sh bin
  group_by(year, sh_bin) %>%
  mutate(n_aged = n()) %>%
  # get number by age within sh bins
  group_by(year, sh_bin, age_bin, n_aged) %>%
  summarise(prop = sprintf("%.4f", n() / mean(n_aged))) %>%
  # pivot wider
  pivot_wider(names_from = age_bin, values_from = prop) %>%
  ungroup %>%
  left_join(pop_bin) %>%
  # join to month of fishery
  left_join(read_csv("./output/observer/2023/fishery_avg_month.csv") %>%
              filter(district == "KSH") %>%
              transmute(year = as.numeric(substring(season, 1, 4)), month)) %>%
  
  transmute(year, 
            month = month - 3,
            fleet = 1, 
            sex = 0, part = 0, ageerr = 1, Lbin_lo = bin, Lbin_hi = bin,
            Nsamp = n_aged,
            a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) %>%
  replace(is.na(.), sprintf("%.4f", 0.0000)) %>%
  # save
  write_delim(., "./data/model_development/stock_synthesis/2023/fishery_age_comp.txt", delim = "\t")


# survey mean size at age ----

read_csv("./output/fishery_independent/statewide_scallop_survey/2022/mean_size_at_age.csv") %>%
  # filter for ksh
  filter(bed_name == "KSH1", 
         age > 0) %>%
  # change older ages to 18
  mutate(age = ifelse(age > 18, 18, age)) %>%
  group_by(year, age) %>%
  summarise(sh = round(weighted.mean(sh, n) / 10, 2),
            n = sum(n)) %>%
  # add age bin and n bin
  transmute(year, sh, n,
            age_bin = paste0("a", age),
            n_bin = paste0("n", age)) %>%
  # get in correct long format
  pivot_longer(c(sh, n), names_to = "data", values_to = "value") %>%
  pivot_longer(c(age_bin, n_bin), names_to = "data1", values_to = "bin") %>%
  filter((data == "sh" & data1 == "age_bin") | (data == "n" & data1 == "n_bin")) %>%
  dplyr::select(-data, -data1) %>%
  # pivot to wide
  pivot_wider(names_from = bin, values_from = value) %>%
  # format for ss
  transmute(year, 
            month = 2, 
            fleet = 2, 
            sex =0, part = 0, ageer = 2, ign = 1,
            a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18,
            n1 = 20, n2 = 20, n3 = 20, n4 = 20, n5 = 20, n6 = 20, n7 = 20, n8 = 20, n9 = 20, n10 = 20,
            n11 = 20, n12 = 20, n13 = 20, n14 = 20, n15 = 20, n16 = 20, n17 = 20, n18 = 20) %>%
  replace(is.na(.), -1) %>%
  # save
  write_delim(., "./data/model_development/stock_synthesis/2023/dredge_mean_size_at_age.txt", delim = "\t")

  
  

# fishery mean size at age ----

read_csv("./output/observer/2023/mean_size_ata_age.csv") %>%
# filter for ksh
filter(district == "KSH", 
       age > 0) %>%
  # change older ages to 18
  mutate(age = ifelse(age > 18, 18, age)) %>%
  group_by(season, age) %>%
  summarise(sh = round(weighted.mean(sh, n) / 10, 2),
            n = sum(n)) %>%
  # add age bin and n bin
  transmute(year = as.numeric(substring(season, 1, 4)), sh, n,
            age_bin = paste0("a", age),
            n_bin = paste0("n", age)) %>%
  # get in correct long format
  pivot_longer(c(sh, n), names_to = "data", values_to = "value") %>%
  pivot_longer(c(age_bin, n_bin), names_to = "data1", values_to = "bin") %>%
  filter((data == "sh" & data1 == "age_bin") | (data == "n" & data1 == "n_bin")) %>%
  dplyr::select(-data, -data1) %>%
  # join to month of fishery
  left_join(read_csv("./output/observer/2023/fishery_avg_month.csv") %>%
              filter(district == "KSH") %>%
              transmute(year = as.numeric(substring(season, 1, 4)), month)) %>%
  # pivot to wide
  pivot_wider(names_from = bin, values_from = value) %>% ungroup %>%
  # format for ss
  transmute(year, 
            month = month - 3, 
            fleet = 1, 
            sex = 0, part = 0, ageerr = 2, ign = 1,
            a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18,
            n1 = 20, n2 = 20, n3 = 20, n4 = 20, n5 = 20, n6 = 20, n7 = 20, n8 = 20, n9 = 20, n10 = 20,
            n11 = 20, n12 = 20, n13 = 20, n14 = 20, n15 = 20, n16 = 20, n17 = 20, n18 = 20) %>%
  replace(is.na(.), -1) %>%
  # save
  write_delim(., "./data/model_development/stock_synthesis/2023/fishery_mean_size_at_age.txt", delim = "\t")
