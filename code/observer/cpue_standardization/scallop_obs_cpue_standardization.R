## 2023 cpue standardization ksh
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2023/1/23

# load libraries and set global options ----

## packages
library(splines); library(ggfortify); library(visreg)

### general observer data functions
setwd("../../..")
source("./code/misc/general_observer_data_functions.R")
source("./code/observer/cpue_standardization/stepCPUE_function.R") # from siddeek
setwd("./code/observer/cpue_standardization")

# data ----

## observer/logbook data
### scallop haul data 2009/10 - Present
catch_wiki <- do.call(bind_rows,
                      lapply(list.files("../../../data/observer/catch/", full.names = T), read_csv))

## observer bycatch data
### scallop haul data 2009/10 - Present
bycatch_wiki <- do.call(bind_rows,
                      lapply(list.files("../../../data/observer/bycatch/", full.names = T, pattern = ".csv"), read_csv))


# data mgmt ----

## clean catch data
catch <- f_clean_catch(catch_wiki)
## clean bycatch data
bycatch <- f_clean_bycatch(bycatch_wiki, catch)

# filter a KSH dataset ----

catch %>%
  ## compute cpue
  mutate(cpue = round_weight / dredge_hrs) %>%
  dplyr::select(season, district, haul_id, adfg, trip_id, set_date, gear_perf, haul_sampled, bed_code, set_lat, set_lon,
                haul_speed, distance_nm, depth, dredge_hrs, dredge_width, round_weight, cpue) %>%
  ## filter for no missing data
  filter(complete.cases(.)) %>%
  ## only KSH data
  ## only 26 or 30ft dredges
  ## only observed hauls
  filter(district == "KSH",
         dredge_width %in% c(26, 30),
         haul_sampled == 1) %>%
  ## remove hauls at the extremities for round weight and depth
  filter(cpue >= quantile(cpue, 0.025),
         cpue <= quantile(cpue, 0.975),
         depth >= quantile(depth, 0.025),
         depth <= quantile(depth, 0.975)) %>%
  ## extract month and relevel so that it aligns with the season progression
  ## make bed a factor
  ## make season a factor
  mutate(month = month.abb[lubridate::month(set_date)],
         month = factor(month, levels = month.abb[c(7:12, 1:6)]),
         adfg = factor(adfg, levels = c(58200, 40924, 32554, 303)),
         dredge_width = factor(dredge_width, levels = c(30, 26)),
         bed = factor(bed_code, levels = c("KSH1", "KSH2", "KSH3"))) -> ksh

# add total catch cpue
bycatch %>%
  # compute discard weight
  transmute(haul_id,
            disc_wt = disc_wt + broken_wt + rem_disc_wt) %>%
  # join to ksh 
  right_join(ksh) %>%
  # total catch cpue
  mutate(tot_cpue = (round_weight + disc_wt) / dredge_hrs) -> ksh


# glm model selection (gamma distribution) ----

# degrees of freedom on depth
AIC(glm(cpue ~ ns(depth, df = 2), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 3), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 4), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 5), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 6), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 7), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 8), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 9), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 10), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 11), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 12), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 13), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 14), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 15), family = Gamma(link = log), data = ksh),
    glm(cpue ~ ns(depth, df = 16), family = Gamma(link = log), data = ksh)) 
# df = 12 appears to be the best, but 4 makes the most sense biologically

# fit a null model
null_glm_gamma <- glm(cpue ~ season, family = Gamma(link = log), data = ksh)

# stepwise forward and backward selection based on r-squared stat
stepCPUE(object = null_glm_gamma,
              scope = ~(ns(depth, df = 4) + 
                          dredge_width + adfg + bed_code + month + season),
              steps = 1000,
              direction = "both",
              trace = 2) -> best_glm_gamma

# diagnostics
diagnostics_best_glm_gamma <- autoplot(best_glm_gamma)

# fit a null model with total cpue
null_glm_gamma_tot <- glm(tot_cpue ~ season, family = Gamma(link = log), data = ksh)

# stepwise forward and backward selection based on r-squared stat
stepCPUE(object = null_glm_gamma_tot,
         scope = ~(ns(depth, df = 4) + dredge_width + adfg + bed_code + month + season),
         steps = 1000,
         direction = "both",
         trace = 2) -> best_glm_gamma_tot

# diagnostics
diagnostics_best_glm_gamma_tot <- autoplot(best_glm_gamma_tot)

# glm model selection (lognormal distribution) ----

# degrees of freedom on depth
AIC(glm(log(cpue) ~ ns(depth, df = 2), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 3), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 4), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 5), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 6), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 7), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 8), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 9), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 10), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 11), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 12), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 13), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 14), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 15), data = ksh),
    glm(log(cpue) ~ ns(depth, df = 16), data = ksh))
# df = 12 appears to be the best, but 4 makes the most sense biologically

# fit a null model
null_glm_ln <- glm(log(cpue) ~ season, data = ksh)

# stepwise forward and backward selection based on r-squared stat
stepCPUE(object = null_glm_ln,
         scope = ~(ns(depth, df = 4) + 
                     dredge_width + adfg + bed_code + month + season),
         steps = 1000,
         direction = "both",
         trace = 2) -> best_glm_ln

# model selection kicked out depth, add it for distribution comparison
best_glm_ln <- update(best_glm_ln, ~. + ns(depth, df = 4))
# diagnostics
diagnostics_best_glm_ln <- autoplot(best_glm_ln)


# fit a null model
null_glm_ln_tot <- glm(log(tot_cpue) ~ season, data = ksh)

# stepwise forward and backward selection based on r-squared stat
stepCPUE(object = null_glm_ln_tot,
         scope = ~(ns(depth, df = 4) + 
                     dredge_width + adfg + bed_code + month + season),
         steps = 1000,
         direction = "both",
         trace = 2) -> best_glm_ln_tot

# diagnostics
diagnostics_best_glm_ln_tot <- autoplot(best_glm_ln_tot)

# marginal effect plots ----

# marginal effects in retained cpue model
visreg(best_glm_gamma, xvar = c("depth"), gg = T)+
  xlab("Depth (fa)") + ylab("f(Depth)") -> depth_marg
visreg(best_glm_gamma, xvar = c("month"), gg = T)+
  xlab("Month") + ylab("f(Month)") -> month_marg
visreg(best_glm_gamma, xvar = c("dredge_width"), gg = T)+
  xlab("Dredge Width (ft)") + ylab("f(Dredge Width)") -> dredge_width_marg

# marginal effects in total cpue model
visreg(best_glm_gamma_tot, xvar = c("depth"), gg = T)+
  xlab("Depth (fa)") + ylab("f(Depth)") -> depth_marg_tot
visreg(best_glm_gamma_tot, xvar = c("month"), gg = T)+
  xlab("Month") + ylab("f(Month)") -> month_marg_tot

# model result table retained ----
null_df <- null_glm_gamma$df.null - null_glm_gamma$df.residual
null_r2 <- round((null_glm_gamma$null.deviance - null_glm_gamma$deviance) / null_glm_gamma$null.deviance, 2)
null_aic <- round(AIC(null_glm_gamma))

final_df <- best_glm_gamma$df.null - best_glm_gamma$df.residual
final_r2 <- round((best_glm_gamma$null.deviance - best_glm_gamma$deviance) / best_glm_gamma$null.deviance, 2)
final_aic <- round(AIC(best_glm_gamma) - AIC(null_glm_gamma))

# final model plus bed
final_bed <- update(best_glm_gamma, ~. + bed_code)
bed_df <- final_bed$df.null - final_bed$df.residual
bed_r2 <- round((final_bed$null.deviance - final_bed$deviance) / final_bed$null.deviance, 2)
bed_aic <- round(AIC(final_bed) - AIC(null_glm_gamma))

# final model plus vessel
final_adfg <- update(best_glm_gamma, ~. + adfg)
adfg_df <- final_adfg$df.null - final_adfg$df.residual
adfg_r2 <- round((final_adfg$null.deviance - final_adfg$deviance) / final_adfg$null.deviance, 2)
adfg_aic <- round(AIC(final_adfg) - AIC(null_glm_gamma))

# model result table total ----
tot_null_df <- null_glm_gamma_tot$df.null - null_glm_gamma_tot$df.residual
tot_null_r2 <- round((null_glm_gamma_tot$null.deviance - null_glm_gamma_tot$deviance) / null_glm_gamma_tot$null.deviance, 2)
tot_null_aic <- round(AIC(null_glm_gamma_tot))

tot_final_df <- best_glm_gamma_tot$df.null - best_glm_gamma_tot$df.residual
tot_final_r2 <- round((best_glm_gamma_tot$null.deviance - best_glm_gamma_tot$deviance) / best_glm_gamma_tot$null.deviance, 2)
tot_final_aic <- round(AIC(best_glm_gamma_tot) - AIC(null_glm_gamma_tot))

# final model plus bed
tot_final_bed <- update(best_glm_gamma_tot, ~. + bed_code)
tot_bed_df <- tot_final_bed$df.null - tot_final_bed$df.residual
tot_bed_r2 <- round((tot_final_bed$null.deviance - tot_final_bed$deviance) / tot_final_bed$null.deviance, 2)
tot_bed_aic <- round(AIC(tot_final_bed) - AIC(null_glm_gamma_tot))

# final model plus vessel
tot_final_adfg <- update(best_glm_gamma_tot, ~. + adfg)
tot_adfg_df <- tot_final_adfg$df.null - tot_final_adfg$df.residual
tot_adfg_r2 <- round((tot_final_adfg$null.deviance - tot_final_adfg$deviance) / tot_final_adfg$null.deviance, 2)
tot_adfg_aic <- round(AIC(tot_final_adfg) - AIC(null_glm_gamma_tot))

# final model plus dredgewidth 
tot_final_dw <- update(best_glm_gamma_tot, ~. + dredge_width)
tot_dw_df <- tot_final_dw$df.null - tot_final_dw$df.residual
tot_dw_r2 <- round((tot_final_dw$null.deviance - tot_final_dw$deviance) / tot_final_dw$null.deviance, 2)
tot_dw_aic <- round(AIC(tot_final_dw) - AIC(null_glm_gamma_tot))

# extract glm index for retained cpue ----

# extract location of season effects
loc <- grep("Intercept|season", names(coef(best_glm_gamma)))
# season effects relative to reference
season_offset <- as.numeric(c(0, coef(best_glm_gamma)[loc[-1]]))
# factor levels (in years)
yrs <- as.numeric(substring(levels(ksh$season), 1, 4))
# compute canonical index
std <- exp(season_offset) - mean(season_offset)
## standard errors
Q <- matrix(-1 / length(loc), nrow = length(loc), ncol= length(loc) - 1)
Q[-1,] <- Q[-1,] + diag(rep(1, length(loc) - 1))
var <- (Q %*% summary(best_glm_gamma)$cov.scaled[loc[-1], loc[-1]]) %*% t(Q)
se <- sqrt(diag(var))

## data frame of std cpue values
std_cpue_glm <- tibble(year = yrs,
                       index = std,
                       se = se,
                       cv = se / std) %>%
  write_csv("../../../output/observer/2023/ksh_std_cpue.csv")


# extract glm index for total cpue ----

# extract location of season effects
loc <- grep("Intercept|season", names(coef(best_glm_gamma_tot)))
# season effects relative to reference
season_offset <- as.numeric(c(0, coef(best_glm_gamma_tot)[loc[-1]]))
# factor levels (in years)
yrs <- as.numeric(substring(levels(ksh$season), 1, 4))
# compute canonical index
std <- exp(season_offset) - mean(season_offset)
## standard errors
Q <- matrix(-1 / length(loc), nrow = length(loc), ncol= length(loc) - 1)
Q[-1,] <- Q[-1,] + diag(rep(1, length(loc) - 1))
var <- (Q %*% summary(best_glm_gamma_tot)$cov.scaled[loc[-1], loc[-1]]) %*% t(Q)
se <- sqrt(diag(var))

## data frame of std cpue values
std_cpue_glm_tot <- tibble(year = yrs,
                       index = std,
                       se = se,
                       cv = se / std) %>%
  write_csv("../../../output/observer/2023/ksh_std_tot_cpue.csv")


# plot cpue indices
std_cpue_glm_tot %>%
  mutate(type = "Total Catch") %>%
  bind_rows(std_cpue_glm %>%
              mutate(type = "Retained Catch")) %>%
  ggplot()+
  geom_point(aes(x = year, y = index, color = type))+
  geom_line(aes(x = year, y = index, color = type))+
  geom_errorbar(aes(x = year, 
                    ymin = index / exp(1.96 * sqrt(log(1 + cv^2))),
                    ymax = index * exp(1.96 * sqrt(log(1 + cv^2))),
                    color = type),
                width = 0)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "Standardized CPUE index", color = NULL)+
  scale_color_manual(values = cb_palette[1:2])+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))-> std_index_plot

# compare with gam cpue standardization and nominal cpue ----

read_csv("../../../output/observer/2023/standardized_cpue_season_KSH.csv") %>%
  transmute(year = season, 
            GAM = std_cpue / prod(std_cpue)^(1 / nrow(.)),
            Nominal = nom_cpue / prod(nom_cpue)^(1 / nrow(.))) %>%
  pivot_longer(2:3, names_to = "cpue", values_to = "index") %>%
  # join to glm method
  bind_rows(std_cpue_glm) %>%
  replace_na(list(cpue = "GLM (Retained Catch)")) %>%
  bind_rows(std_cpue_glm_tot) %>%
  replace_na(list(cpue = "GLM (Total Catch)")) %>%
  mutate(cpue = factor(cpue, levels = c("GLM (Retained Catch)", "GLM (Total Catch)", 
                                        "GAM", "Nominal"))) %>%
  
  # plot
  ggplot()+
  geom_point(aes(x = year, y = index, color = cpue))+
  geom_line(aes(x = year, y = index, color = cpue))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "CPUE index", color = NULL)+
  scale_color_manual(values = cb_palette[1:4])+
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) -> index_compare_plot


