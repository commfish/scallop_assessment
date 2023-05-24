# notes ----
## view results for 2023 KSH SS models
## tyler jakson
## tyler.jackson@alaska.gov
## 12/29/2022

# load ----

source("./code/model_development/stock_synthesis/ss_output_functions.R")

# base model, 22.1a ----

## directory with output files
dir_22.1a <- "./code/model_development/stock_synthesis/2023/22.1a"
## report
report_22.1a <- SS_output(dir = dir_22.1a, verbose=TRUE, printstats=TRUE)
## standard SS plots
SS_plots(report_22.1a, btarg = F, sprtarg = F)

## retrospective analysis, peel 5 yr
retro_dir <- file.path(getwd(),"/code/model_development/stock_synthesis/2023/22.1a/retro")
copy_SS_inputs(dir.old = "./code/model_development/stock_synthesis/2023/22.1a",
               dir.new = retro_dir)
file.copy("./code/model_development/stock_synthesis/2023/22.1a/ss.exe",
          "./code/model_development/stock_synthesis/2023/22.1a/retro/ss.exe")

## do analysis
SS_doRetro(masterdir = retro_dir, oldsubdir = "", newsubdir = "retrospectives",
           years = 0:-5, exefile = "ss.exe")

## view retrospective output
retro_models <- SSgetoutput(dirvec = file.path(retro_dir, "retrospectives",
                                               paste("retro", 0:-5, sep = "")))
retro_summary <- SSsummarize(retro_models)
endyrvec <- retro_summary[["endyrs"]] + 0:-5

dir.create(file.path(retro_dir, "plots"))
SSplotComparisons(retro_summary, endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"), btarg = F, sprtarg = F,
                  plotdir =  "./code/model_development/stock_synthesis/2023/22.1a/retro/plots", print = T)

f_plot_retro(retro_summary, terminal_yrs = 2022:2017,
             file_path = "./code/model_development/stock_synthesis/2023/22.1a/retro/plots")



# model 23.0 ----

## directory with output files
dir_23.0 <- "./code/model_development/stock_synthesis/2023/23.0"
## report
report_23.0 <- SS_output(dir = dir_23.0 , verbose=TRUE, printstats=TRUE)
## standard SS plots
SS_plots(report_23.0, btarg = F, sprtarg = F)

## retrospective analysis, peel 5 yr
retro_dir <- file.path(getwd(),"/code/model_development/stock_synthesis/2023/23.0/retro")
copy_SS_inputs(dir.old = "./code/model_development/stock_synthesis/2023/23.0",
               dir.new = retro_dir)
file.copy("./code/model_development/stock_synthesis/2023/23.0/ss.exe",
          "./code/model_development/stock_synthesis/2023/23.0/retro/ss.exe")

## do analysis
SS_doRetro(masterdir = retro_dir, oldsubdir = "", newsubdir = "retrospectives",
           years = 0:-5, exefile = "ss.exe")

## view retrospective output
retro_models <- SSgetoutput(dirvec = file.path(retro_dir, "retrospectives",
                                               paste("retro", 0:-5, sep = "")))
retro_summary <- SSsummarize(retro_models)
endyrvec <- retro_summary[["endyrs"]] + 0:-5

dir.create(file.path(retro_dir, "plots"))
SSplotComparisons(retro_summary, endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"), btarg = F, sprtarg = F,
                  plotdir =  "./code/model_development/stock_synthesis/2023/23.0/retro/plots", print = T)

f_plot_retro(retro_summary = retro_summary, terminal_yrs = 2022:2017,
             file_path = "./code/model_development/stock_synthesis/2023/23.0/retro/plots")


# model 23.0a ----

## directory with output files
dir_23.0a <- "./code/model_development/stock_synthesis/2023/23.0a"
## report
report_23.0a <- SS_output(dir = dir_23.0a, verbose=TRUE, printstats=TRUE)
## standard SS plots
SS_plots(report_23.0a, btarg = F, sprtarg = F)

## retrospective analysis, peel 5 yr
retro_dir <- file.path(getwd(),"/code/model_development/stock_synthesis/2023/23.0a/retro")
copy_SS_inputs(dir.old = "./code/model_development/stock_synthesis/2023/23.0a",
               dir.new = retro_dir)
file.copy("./code/model_development/stock_synthesis/2023/23.0a/ss.exe",
          "./code/model_development/stock_synthesis/2023/23.0a/retro/ss.exe")

## do analysis
SS_doRetro(masterdir = retro_dir, oldsubdir = "", newsubdir = "retrospectives",
           years = 0:-5, exefile = "ss.exe")

## view retrospective output
retro_models <- SSgetoutput(dirvec = file.path(retro_dir, "retrospectives",
                                               paste("retro", 0:-5, sep = "")))
retro_summary <- SSsummarize(retro_models)
endyrvec <- retro_summary[["endyrs"]] + 0:-5

dir.create(file.path(retro_dir, "plots"))
SSplotComparisons(retro_summary, endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"), btarg = F, sprtarg = F,
                  plotdir =  "./code/model_development/stock_synthesis/2023/23.0a/retro/plots", print = T)

f_plot_retro(retro_summary, terminal_yrs = 2022:2017,
             file_path = "./code/model_development/stock_synthesis/2023/23.0a/retro/plots")



# model 23.0a1 ----

## directory with output files
dir_23.0a1 <- "./code/model_development/stock_synthesis/2023/23.0a1"
## report
report_23.0a1 <- SS_output(dir = dir_23.0a1, verbose=TRUE, printstats=TRUE)
## standard SS plots
SS_plots(report_23.0a1, btarg = F, sprtarg = F)



## retrospective analysis, peel 5 yr
retro_dir <- file.path(getwd(),"/code/model_development/stock_synthesis/2023/23.0a1/retro")
copy_SS_inputs(dir.old = "./code/model_development/stock_synthesis/2023/23.0a1",
               dir.new = retro_dir)
file.copy("./code/model_development/stock_synthesis/2023/23.0a1/ss.exe",
          "./code/model_development/stock_synthesis/2023/23.0a1/retro/ss.exe")

## do analysis
SS_doRetro(masterdir = retro_dir, oldsubdir = "", newsubdir = "retrospectives",
           years = 0:-5, exefile = "ss.exe")

## view retrospective output
retro_models <- SSgetoutput(dirvec = file.path(retro_dir, "retrospectives",
                                               paste("retro", 0:-5, sep = "")))
retro_summary <- SSsummarize(retro_models)
endyrvec <- retro_summary[["endyrs"]] + 0:-5

dir.create(file.path(retro_dir, "plots"))
SSplotComparisons(retro_summary, endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"), btarg = F, sprtarg = F,
                  plotdir =  "./code/model_development/stock_synthesis/2023/23.0a1/retro/plots", print = T)

f_plot_retro(retro_summary, terminal_yrs = 2022:2017,
             file_path = "./code/model_development/stock_synthesis/2023/23.0a1/retro/plots")



# model 23.0a2 ----

## directory with output files
dir_23.0a2 <- "./code/model_development/stock_synthesis/2023/23.0a2"
## report
report_23.0a2 <- SS_output(dir = dir_23.0a2, verbose=TRUE, printstats=TRUE)
## standard SS plots
SS_plots(report_23.0a2, btarg = F, sprtarg = F)

# model 23.0a3 ----

## directory with output files
dir_23.0a3 <- "./code/model_development/stock_synthesis/2023/23.0a3"
## report
report_23.0a3 <- SS_output(dir = dir_23.0a3, verbose=TRUE, printstats=TRUE)
## standard SS plots
SS_plots(report_23.0a3, btarg = F, sprtarg = F)


## retrospective analysis, peel 5 yr
retro_dir <- file.path(getwd(),"/code/model_development/stock_synthesis/2023/23.0a3/retro")
copy_SS_inputs(dir.old = "./code/model_development/stock_synthesis/2023/23.0a3",
               dir.new = retro_dir)
file.copy("./code/model_development/stock_synthesis/2023/23.0a3/ss.exe",
          "./code/model_development/stock_synthesis/2023/23.0a3/retro/ss.exe")

## do analysis
SS_doRetro(masterdir = retro_dir, oldsubdir = "", newsubdir = "retrospectives",
           years = 0:-5, exefile = "ss.exe")

## view retrospective output
retro_models <- SSgetoutput(dirvec = file.path(retro_dir, "retrospectives",
                                               paste("retro", 0:-5, sep = "")))
retro_summary <- SSsummarize(retro_models)
endyrvec <- retro_summary[["endyrs"]] + 0:-5

dir.create(file.path(retro_dir, "plots"))
SSplotComparisons(retro_summary, endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"), btarg = F, sprtarg = F,
                  plotdir =  "./code/model_development/stock_synthesis/2023/23.0a3/retro/plots", print = T)

f_plot_retro(retro_summary, terminal_yrs = 2022:2017,
             file_path = "./code/model_development/stock_synthesis/2023/23.0a3/retro/plots")



# model 23.1 ----

## directory with output files
dir_23.1 <- "./code/model_development/stock_synthesis/2023/23.1"
# ## report
# report_23.1 <- SS_output(dir = dir_23.1, verbose=TRUE, printstats=TRUE)
# ## standard SS plots
# SS_plots(report_23.1, btarg = F, sprtarg = F)


# model 23.2 ----

## directory with output files
dir_23.2 <- "./code/model_development/stock_synthesis/2023/23.2"
## report
report_23.2 <- SS_output(dir = dir_23.2, verbose=TRUE, printstats=TRUE)
## standard SS plots
SS_plots(report_23.2, btarg = F, sprtarg = F)

# model 23.3 ----

## directory with output files
dir_23.3 <- "./code/model_development/stock_synthesis/2023/23.3"
## report
report_23.3 <- SS_output(dir = dir_23.3, verbose=TRUE, printstats=TRUE)
## standard SS plots
SS_plots(report_23.3, btarg = F, sprtarg = F)

## retrospective analysis, peel 5 yr
retro_dir <- file.path(getwd(),"/code/model_development/stock_synthesis/2023/23.3/retro")
copy_SS_inputs(dir.old = "./code/model_development/stock_synthesis/2023/23.3",
               dir.new = retro_dir)
file.copy("./code/model_development/stock_synthesis/2023/23.3/ss.exe",
          "./code/model_development/stock_synthesis/2023/23.3/retro/ss.exe")

## do analysis
SS_doRetro(masterdir = retro_dir, oldsubdir = "", newsubdir = "retrospectives",
           years = 0:-5, exefile = "ss.exe")

## view retrospective output
retro_models <- SSgetoutput(dirvec = file.path(retro_dir, "retrospectives",
                                               paste("retro", 0:-5, sep = "")))
retro_summary <- SSsummarize(retro_models)
endyrvec <- retro_summary[["endyrs"]] + 0:-5

dir.create(file.path(retro_dir, "plots"))
SSplotComparisons(retro_summary, endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"), btarg = F, sprtarg = F,
                  plotdir =  "./code/model_development/stock_synthesis/2023/23.3/retro/plots", print = T)

f_plot_retro(retro_summary, terminal_yrs = 2022:2017,
             file_path = "./code/model_development/stock_synthesis/2023/23.3/retro/plots")

# likelihood comp table ----

tibble(dir = c(dir_22.1a, dir_23.0, dir_23.0a, dir_23.0a1, dir_23.0a3, dir_23.3),
       mod = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3")) %>%
  mutate(lik = purrr::map(dir, f_extract_likelihood_comp)) %>%
  dplyr::select(-dir) %>%
  unnest(lik) %>%
  # remove null likelihood components
  filter(nll != 0) %>%
  # pivot wider
  pivot_wider(names_from = mod, values_from = nll) %>%
  # change names 
  transmute(process = case_when(process == "Catch_like" ~ "Catch",
                             process == "Disc_like" ~ "Discards",
                             process == "TOTAL" ~ "Total",
                             process == "Recruitment" ~ "Recruitment Deviations",
                             process == "Parm_priors" ~ "Parameter Priors",
                             process == "Parm_devs" ~ "Parameter Deviations",
                             process == "Surv_like" & fleet == "DREDGE" ~ "Survey Biomass",
                             process == "Surv_like" & fleet == "FISHERYCPUE" ~ "CPUE 2009-2022",
                             process == "Surv_like" & fleet == "FISHERYCPUEnom" ~ "CPUE 1992-2008",
                             process == "Length_like" & fleet == "DREDGE" ~ "Survey Length Comp.",
                             process == "Length_like" & fleet == "FISHERY" ~ "Fishery Length Comp.",
                             process == "Age_like" & fleet == "DREDGE" ~ "Survey Age Comp.",
                             process == "Age_like" & fleet == "FISHERY" ~ "Fishery Age Comp.",
                             process == "sizeatage_like" & fleet == "FISHERY" ~ "Fishery Size at Age",
                             process == "sizeatage_like" & fleet == "DREDGE" ~ "Survey Size at Age"),
            `22.1a`, `23.0`, `23.0a`, `23.0a1`, `23.0a3`, `23.3`) %>%
  slice(11, 1, 14, 12, 4, 3, 2, 5:10, 13, 15) %>%
  mutate_at(2:7, function(x) {ifelse((abs(x) < 0.001 | abs(x) > 1e4), 
                                     formatC(x, format = "e", digits = 3), 
                                     formatC(x, format = "f", digits = 3, big.mark = ","))}) -> lik_comp
  
write_delim(lik_comp, "./code/model_development/stock_synthesis/2023/likelihood_comp.txt")




# parameter estimate table ----

par_tab <- tibble(label = c("NatM_p_1_Fem_GP_1", "Wtlen_1_Fem_GP_1", "Wtlen_2_Fem_GP_1",
                            "Mat50%_Fem_GP_1", "Mat_slope_Fem_GP_1", "SR_LN(R0)", "SR_sigmaR",
                            "L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "VonBert_K_Fem_GP_1",
                            "CV_young_Fem_GP_1", "CV_old_Fem_GP_1", "LnQ_base_FISHERYCPUE(3)",
                            "Q_extraSD_FISHERYCPUE(3)", "LnQ_base_FISHERYCPUEnom(4)",
                            "Size_inflection_FISHERY(1)", "Size_95%width_FISHERY(1)",
                            "Size_inflection_DREDGE(2)", "Size_95%width_DREDGE(2)"),
                  text_label = c("Natural Mortality", "Weight-at-SH $\\alpha$", "Weight-at-SH $\\beta$",
                                 "Size at 50\\% maturity", "Maturity Slope", "Log Virgin Rec", 
                                 "SD Log Rec", "LvB $L_{1}$", "LvB $L_{2}$", "LvB $\\kappa$",
                                 "CV growth $<$ min SH", "CV growth $>$ max SH", "CPUE 2009-2022 ln Q",
                                 "CPUE 2009-2022 extra $\\sigma$", "CPUE 1993-2008 ln Q",
                                 "Fishery Selectivity $\\beta_1$", "Fishery Selectivity $\\beta_2$",
                                 "Dredge Selectivity $\\beta_1$", "Dredge Selectivity $\\beta_2$"))



tibble(dir = c(dir_22.1a, dir_23.0, dir_23.0a, dir_23.0a1, dir_23.0a3, dir_23.3),
       mod = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3")) %>%
  mutate(lik = purrr::map(dir, f_extract_par_status))%>% 
  dplyr::select(-dir) %>%
  unnest(lik) %>%
  # join to latex parameter labels
  right_join(par_tab, by = "label") %>%
  mutate(text_label = ifelse((is.na(gradient) & text_label != "CPUE 2009-2022 extra $\\sigma$"), 
                             paste0(text_label, "*"), text_label)) %>%
  # adjust bounds
  group_by(text_label) %>%
  mutate(min = min(min), max = max(max)) %>%
  ungroup %>%
  # change text to red if hitting bound
  mutate(bounds = paste0("(", min, ", ", max, ")"),
         value = ifelse(abs(value) < 0.001, 
                        formatC(value, format = "e", digits = 3),
                        formatC(value, format = "f", digits = 3, big.mark = ",")),
         value = ifelse(status %in% c("LO", "HI"), paste0("\\textcolor{red}{", value, "}"), value),
         bounds = ifelse((is.na(gradient) & text_label != "CPUE 2009-2022 extra $\\sigma$"), NA, bounds)) %>%
  # select columns
  dplyr::select(mod, text_label, value, bounds) %>%
  # pivot format
  pivot_wider(names_from = "mod", values_from = "value")  %>%
  # change values for extra cpue sd to zero for 22.1a and 23.0
  mutate(`22.1a` = ifelse(text_label == "CPUE 2009-2022 extra $\\sigma$", NA, `22.1a`),
         `23.0` = ifelse(text_label == "CPUE 2009-2022 extra $\\sigma$", NA, `23.0`)) %>%
  write_delim("./code/model_development/stock_synthesis/2023/par_est.txt")

  
  

# recruitment and ssb table ----

tibble(mod = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"),
       rep = list(report_22.1a$recruit, report_23.0$recruit, report_23.0a$recruit, 
                  report_23.0a1$recruit, report_23.0a3$recruit, report_23.3$recruit)) %>%
  unnest(rep) %>%
  rename_all(tolower) %>%
  # filter for virgin and forecasted conditions
  filter(yr %in% c(1991, 2023)) %>%
  transmute(mod, yr, spawnbio, exp_recr) %>%
  pivot_longer(c(spawnbio, exp_recr), names_to = "stat", values_to = "est") %>%
  filter(!(yr == 2023 & stat == "exp_recr")) %>%
  mutate(yr = case_when(yr == 1991 & stat == "exp_recr" ~ "Unfished $R$ (thousands)",
                        yr == 1991 & stat == "spawnbio" ~ "Unfished SSB (t)", 
                        yr == 2023 & stat == "spawnbio" ~ "Forecast (2023) SSB (t)"),
         yr = factor(yr, levels = c("Unfished $R$ (thousands)", "Unfished SSB (t)", 
                                    "Forecast (2023) SSB (t)"))) %>%
  dplyr::select(-stat) %>%
  pivot_wider(names_from = "mod", values_from = "est") %>%
  write_delim("./code/model_development/stock_synthesis/2023/ssb_rec.txt")

  



# model comparison plots ----

## get summaries
SSgetoutput(dirvec = c(dir_22.1a, dir_23.0, dir_23.0a, dir_23.0a1, dir_23.0a3, dir_23.3)) %>%
  SSsummarize() -> mod_summaries

# ssb
f_plot_ssb(mod_summaries, models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"), 
          file_path = "./code/model_development/stock_synthesis/2023/plots/ssb.png")
f_plot_ssb(mod_summaries, models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"), se = F, 
           file_path = "./code/model_development/stock_synthesis/2023/plots/ssb_no_se.png")
### recruitment
f_plot_recruit(summaryoutput = mod_summaries, models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"), 
               file_path = "./code/model_development/stock_synthesis/2023/plots/rec.png")

f_plot_recdev(summaryoutput = mod_summaries, models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"), 
              file_path = "./code/model_development/stock_synthesis/2023/plots/rec_devs.png")

### fit to fishery CPUE
f_plot_index(summaryoutput = mod_summaries, models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"),
             fleet_num = 3, y_title = "CPUE Index",
             file_path = "./code/model_development/stock_synthesis/2023/plots/cpue2009-2022_fit.png")

f_plot_index(summaryoutput = mod_summaries, models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"),
             fleet_num = 4, y_title = "CPUE Index",
             file_path = "./code/model_development/stock_synthesis/2023/plots/cpue1992-2008_fit.png")

### fit to survey CPUE
f_plot_index(summaryoutput = mod_summaries, models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"),
             fleet_num = 2, y_title = "Biomass (t)",
             file_path = "./code/model_development/stock_synthesis/2023/plots/dredge_biomass_fit.png")

### fit to fishery length comp
# no partition
f_plot_lencomp(dirs = dir_22.1a, "22.1a", fleet_num = 1, partition = 0, 
               file_path = "./code/model_development/stock_synthesis/2023/plots/fishery_lencomp_fit_22.1a.png")
# discards
f_plot_lencomp(dirs = c(dir_23.0, dir_23.0a, dir_23.0a1, dir_23.0a3, dir_23.3), 
               models = c("23.0", "23.0a", "23.0a1", "23.0a3", "23.3"), fleet_num = 1, partition = 1, 
               file_path = "./code/model_development/stock_synthesis/2023/plots/fishery_lencomp_fit_discards.png")
# retained
f_plot_lencomp(dirs = c(dir_23.0, dir_23.0a, dir_23.0a1, dir_23.0a3, dir_23.3), 
               models = c("23.0", "23.0a", "23.0a1", "23.0a3", "23.3"), fleet_num = 1, partition = 2, 
               file_path = "./code/model_development/stock_synthesis/2023/plots/fishery_lencomp_fit_retained.png")

### fit to survey length comp
f_plot_lencomp(dirs = c(dir_22.1a, dir_23.0, dir_23.0a, dir_23.0a1, dir_23.0a3, dir_23.3), 
               models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"), fleet_num = 2, partition = 0, 
               height = 5,
               file_path = "./code/model_development/stock_synthesis/2023/plots/dredge_lencomp_fit.png")

### fit to fishery age comp
f_plot_agecomp(dirs = c(dir_22.1a, dir_23.0, dir_23.0a, dir_23.0a1, dir_23.0a3, dir_23.3), 
               models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"), fleet_num = 1, 
               prefix = "./code/model_development/stock_synthesis/2023/plots")

### fit to dredge age comp
f_plot_agecomp(dirs = c(dir_22.1a, dir_23.0, dir_23.0a, dir_23.0a1, dir_23.0a3, dir_23.3), 
               models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"), fleet_num = 2, 
               prefix = "./code/model_development/stock_synthesis/2023/plots")

### plot n matrices

f_plot_n_matrix("22.1a", dir_22.1a, "./code/model_development/stock_synthesis/2023/plots")
f_plot_n_matrix("23.0", dir_23.0, "./code/model_development/stock_synthesis/2023/plots")
f_plot_n_matrix("23.0a", dir_23.0a, "./code/model_development/stock_synthesis/2023/plots")
f_plot_n_matrix("23.0a1", dir_23.0a1, "./code/model_development/stock_synthesis/2023/plots")
f_plot_n_matrix("23.0a3", dir_23.0a3, "./code/model_development/stock_synthesis/2023/plots")
f_plot_n_matrix("23.3", dir_23.3, "./code/model_development/stock_synthesis/2023/plots")

### plot sh selectivity in fishery and dredge survey
f_plot_selex(summaryoutput = mod_summaries, 
             models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"),
             fleets = 1:2,
             file_path = "./code/model_development/stock_synthesis/2023/plots/selex_all.png")

### plot sh retention in fishery 
f_plot_retention(dirs = c(dir_22.1a, dir_23.0, dir_23.0a, dir_23.0a1, dir_23.0a3, dir_23.3), 
                 models = c("22.1a", "23.0", "23.0a", "23.0a1", "23.0a3", "23.3"),
                 file_path = "./code/model_development/stock_synthesis/2023/plots/retention.png")

