# notes ----

## evaluate differing sample designs loosely based on scallop beds
## Tyler Jackson
## 7/14/2022

# load ----

library(SimSurvey)
library(tidyverse)
library(patchwork)
library(FNGr)

theme_set(FNGr::theme_sleek())
yrs <- tickr(tibble(yr = 1:20), var = yr, to = 5)

# functions ----

## aggregate N at age matrix
f_aggegrate_N <- function(pop, ages, size_range) {
  
  if(missing(size_range)){
    # reshape N at age matrix data
    pop$N %>%
      t() %>%
      as_tibble() %>%
      rowid_to_column(var = "year") %>%
      pivot_longer(2:ncol(.), names_to = "age", values_to = "N") %>%
      # filter for ages of interest
      filter(age %in% ages) %>%
      # aggregate over years
      group_by(year) %>%
      summarise(N = sum(N))
  }
  
  if(missing(ages)){
    # reshape N at age matrix data
    pop$N_at_length %>%
      t() %>%
      as_tibble() %>%
      rowid_to_column(var = "year") %>%
      pivot_longer(2:ncol(.), names_to = "sh", values_to = "N") %>%
      # filter for ages of interest
      filter(sh %in% size_range) %>%
      # aggregate over years
      group_by(year) %>%
      summarise(N = sum(N))
  }  
  
}

## plot age distribution
f_plot_dist <- function(distribution, survey_grid, ages){
  
  # strata lines
  tibble(grid = c("Grid - 1", "Grid - 1", "Grid - 2"),
         yintercept = c(-6, 6, 0)) %>%
    filter(grid == survey_grid) -> hline
  
  # plot
  as_tibble(distribution$sp_N) %>%
    left_join(as_tibble(distribution$grid_xy)) %>%
    filter(age %in% ages) %>%
    group_by(year, x, y, cell, division, strat, depth) %>%
    summarise(N = sum(N)) %>%
    mutate(year_text = factor(paste0("Year - ", year), level = paste0("Year - ", 1:20))) %>%
    ggplot()+
    geom_tile(aes(x = y, y = -x, fill = N), alpha = 0.7)+
    # geom_contour(aes(x = y, y = -x, z = depth), 
    #              bins = 3, breaks = c(50, 95, 105, 150), color = 1)+
    geom_hline(data = hline, aes(yintercept = yintercept), size = 1)+
    #scale_fill_continuous()+
    labs(x = NULL, y = NULL)+
    coord_cartesian(expand = 0)+
    scale_fill_viridis_c(labels = scales::comma)+
    facet_wrap(~year_text)+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(color = 1, size = 1),
          legend.position = "right") -> out
  return(out)
  
}

## systematic sampling
f_systematic_sample <- function(distributed_pop, iterations, years = 1:20) {
  
  # prepare grid and numbers at age
  grid <- distributed_pop$grid_xy %>%
    # switch x and y so grid is shaped horizontally
    rename(x = y, y = x) %>%
    # arrange in rows of cells
    arrange(-y, x) 
  sp_N <- distributed_pop$sp_N
  
  ## tow stations
  # compute area swept and cell area
  area_swept <- (8 * 0.0003048) * 1.852
  cell_area <- 4
  q <- 1
  # probabilty for binomial sampling
  p <- (area_swept / cell_area) * q
  
  ## station selection process
  expand.grid(set = 1:iterations,
              year = years) %>%
    # add year's start location (random selection)
    mutate(start = sample(0:2, nrow(.), replace = T)) %>%
    group_by(set, year, start) %>%
    mutate(stations = purrr::map(start, function(start) {
      grid %>%
        # selection
        filter(row_number() %% 3 == start) 
    })) %>%
    unnest(stations) %>%
    # join to data from the distribution
    left_join(sp_N, by = c("year", "cell")) %>%
    # do tows
    rowwise() %>%
    mutate(n = rbinom(1, round(N), p), 
           area_swept = area_swept, 
           cell_area = 4,
           total_area = cell_area * 200) %>%
    ungroup %>%
    # create speciment data
    mutate(dat = purrr::map2(age, n, function(age, n) {
      table(size = growth_fun(rep(age, n))) %>%
        as.data.frame() 
    })) %>%
    unnest(dat) %>%
    rename(sample_factor = Freq) -> out
  
  
}

## analyze systematic sample
f_systematic_analysis <- function(data, population, size_range, plot = T, path_root){
  
  data$size = as.numeric(as.character(data$size))
  
  # compute population timeseries of abundance
  pop_abund = f_aggegrate_N(population, size_range = size_range)
  
  # compute sample timeseries of abundance within size range
  data %>%
    ungroup %>%
    # filter for exploitable size classes
    filter(size %in% size_range) %>%
    # compute cpue per station
    group_by(set, year, cell, area_swept, total_area) %>%
    summarise(catch = sum(sample_factor)) %>%
    group_by(set, year, total_area) %>%
    summarise(cpue = mean(catch / area_swept) ,
              var = var(catch / area_swept) / n()) %>%
    ungroup %>%
    # expand to abundance, compute cv
    mutate(N_hat = cpue * total_area,
           se = sqrt(var * total_area^2),
           cv = se / N_hat) -> abund
  
  ## rmse
  rmse_age = function(ssq){sqrt(ssq / (length(0:18) * 20 * length(unique(data$set))))}
  rmse_size =  function(ssq){sqrt(ssq / (length(seq(20, 200, 5)) * 20 * length(unique(data$set))))}
  
  ### rmse for abundance at age 0-18
  data %>%
    ungroup %>%
    # compute cpue per station
    group_by(set, year, cell, age, area_swept, total_area) %>%
    summarise(catch = sum(sample_factor)) %>%
    group_by(set, year, age, total_area) %>%
    summarise(cpue = mean(catch / area_swept)) %>%
    ungroup %>%
    # expand to abundance
    mutate(N_hat = cpue * total_area)  %>%
    dplyr::select(set, year, age, N_hat) %>%
    # join to true abundance
    left_join(population1$N %>%
                as_tibble() %>%
                rownames_to_column(var = "age") %>%
                mutate(age = 0:18) %>%
                pivot_longer(2:ncol(.), names_to = "year", values_to = "N") %>%
                mutate(year = as.numeric(year),
                       age = as.numeric(age)),
              by = c("year", "age")) %>%
    # compute ssq
    mutate(sq_diff = (N_hat - N)^2) %>%
    pull(sq_diff) %>%
    sum() %>%
    #compute rmse
    rmse_age -> rmse_age
  
  ### rmse for abundance at size 20 - 200
  data %>%
    ungroup %>%
    mutate(size_bin = round(size / 5) * 5) %>%
    filter(size_bin >= 20, size_bin <= 200) %>%
    # compute cpue per station
    group_by(set, year, cell, size_bin, area_swept, total_area) %>%
    summarise(catch = sum(sample_factor)) %>%
    group_by(set, year, size_bin, total_area) %>%
    summarise(cpue = mean(catch / area_swept)) %>%
    ungroup %>%
    # expand to abundance
    mutate(N_hat = cpue * total_area)  %>%
    dplyr::select(set, year, size_bin, N_hat) %>%
    # join to trend abundance
    left_join(population1$N_at_length %>%
                as.data.frame() %>%
                rownames_to_column(var = "size") %>%
                pivot_longer(2:ncol(.), names_to = "year", values_to = "N") %>%
                mutate(size = as.numeric(size), 
                       year = as.numeric(year), 
                       size_bin = round(size / 5) * 5) %>%
                group_by(year, size_bin) %>%
                summarise(N = sum(N)),
              by = c("year", "size_bin")) -> size_tmp
  size_tmp %>%
    # compute ssq
    mutate(sq_diff = (N_hat - N)^2) %>%
    pull(sq_diff) %>%
    sum() %>%
    #compute rmse
    rmse_size -> rmse_size
  
  
  if(plot == T){
    
    ## plot abundance  
    abund %>%
      ggplot()+
      geom_line(aes(x = year, y = N_hat / 1e6, group = set), color = "lightblue", size = 0.3)+
      geom_line(data = pop_abund, aes(x = year, y = N / 1e6, group = 1), color = "black")+
      labs(x = "Year", y = "Abundance (mil)") -> x
    ggsave(paste0(path_root, "_N_hat.png"), plot = x, height = 3, width = 5, units = "in")
    
    ## N hat residuals
    abund %>%
      left_join(pop_abund) %>%
      ggplot()+
      geom_boxplot(aes(x = factor(year), y = N_hat - N))+
      geom_hline(yintercept = 0, linetype = 2)+
      labs(x = "Year", y = expression(hat(N) - N)) -> x
    ggsave(paste0(path_root, "_N_hat_residuals.png"), plot = x, height = 3, width = 5, units = "in")
    
    ## plot cv  
    abund %>%
      ggplot()+
      geom_line(aes(x = year, y = cv, group = set), color = "lightblue", size = 0.3)+
      labs(x = "Year", y = "CV") -> x
    ggsave(paste0(path_root, "_cv.png"), plot = x, height = 3, width = 5, units = "in")
    
    # dot plot of residuals in size comp
    size_tmp %>%
      mutate(diff = N_hat - N) %>%
      group_by(year, size_bin) %>%
      summarise(diff = mean(diff)) %>%
      mutate(sign = ifelse(diff > 0, "> 0", "< 0")) %>%
      ggplot()+
      geom_point(aes(x = year, y = size_bin, size = abs(diff), color = sign))+
      labs(x = "Year", y = "Shell Height (mm)", color = "Sign", size = "Residual") -> x
    ggsave(paste0(path_root, "_size_comp_residual.png"), plot = x, height = 4, width = 6, units = "in")
    
  }
  
  return(list(N_hat = dplyr::select(abund, set, year, cpue, N_hat, se, cv) %>%
                left_join(pop_abund, by = "year"),
              rmse_age = rmse_age,
              rmse_size = rmse_size))
  
  
}

## random stratified sampling
f_stratified_random_sample <- function(distributed_pop, iterations, years = 1:20, sample_rate) { 
  
  # prepare grid and numbers at age
  grid <- distributed_pop$grid_xy %>%
    # switch x and y so grid is shaped horizontally
    rename(x = y, y = x) %>%
    # arrange in rows of cells
    arrange(-y, x) 
  sp_N <- distributed_pop$sp_N
  
  ## tow stations
  # compute area swept and cell area
  area_swept <- (8 * 0.0003048) * 1.852
  cell_area <- 4
  q <- 1
  # probabilty for binomial sampling
  p <- (area_swept / cell_area) * q
  # calc strata area
  grid %>%
    group_by(strat) %>%
    summarise(strat_area = n() * 4) -> strat_area
  
  
  expand.grid(set = 1:iterations, 
              year = years,
              strat = sort(unique(grid$strat))) %>%
    # join to stratum sampling rates and years
    left_join(tibble(strat = sort(unique(grid$strat)),
                     m = sample_rate), by = "strat") %>%
    as_tibble() %>%
    # randomly sample stratum
    mutate(sample = purrr::map2(strat, m, function(stratum, m) {sample_n(grid[grid$strat == stratum,], m, replace = F)[,-"strat"]})) %>%
    unnest(sample) %>%
    # join to N at age data
    left_join(sp_N, by = c("year", "cell")) %>%
    # do tows
    rowwise() %>%
    mutate(n = rbinom(1, round(N), p), 
           area_swept = area_swept, 
           cell_area = 4,
           total_area = cell_area * 200) %>%
    left_join(strat_area, by = "strat") %>%
    ungroup %>%
    # create specimen data
    mutate(dat = purrr::map2(age, n, function(age, n) {
      table(size = growth_fun(rep(age, n))) %>%
        as.data.frame() 
    })) %>%
    unnest(dat) %>%
    rename(sample_factor = Freq) %>%
    mutate(size = as.numeric(as.character(size))) -> out
  
  return(out)
  
}

## stratified systematic sampling
f_stratified_systematic_sample <- function(distributed_pop, iterations, years = 1:20, sample_rate) { 
  
  # prepare grid and numbers at age
  grid <- distributed_pop$grid_xy %>%
    # switch x and y so grid is shaped horizontally
    rename(x = y, y = x) %>%
    # arrange in rows of cells
    arrange(-y, x) 
  sp_N <- distributed_pop$sp_N
  
  ## tow stations
  # compute area swept and cell area
  area_swept <- (8 * 0.0003048) * 1.852
  cell_area <- 4
  q <- 1
  # probabilty for binomial sampling
  p <- (area_swept / cell_area) * q
  # calc strata area
  grid %>%
    group_by(strat) %>%
    summarise(strat_area = n() * 4) -> strat_area
  
  
  expand.grid(set = 1:iterations, 
              year = years,
              strat = sort(unique(grid$strat))) %>%
    # join to stratum sampling rates and years
    left_join(tibble(strat = sort(unique(grid$strat)),
                     r = sample_rate), by = "strat") %>%
    as_tibble() %>%
    # randomly sample stratum
    mutate(sample = purrr::map2(strat, r, function(stratum, r) {
      
      # start location
      start <- sample(0:(r-1), 1)
      # selection
      grid[grid$strat == stratum,] %>%
        filter(row_number() %% r == start) %>%
        dplyr::select(-strat)
      
    })) %>%
    unnest(sample) %>%
    # join to population data
    left_join(sp_N, by = c("year", "cell")) %>%
    rowwise() %>%
    mutate(n = rbinom(1, round(N), p), 
           area_swept = area_swept, 
           cell_area = 4,
           total_area = cell_area * 200) %>%
    left_join(strat_area, by = "strat") %>%
    ungroup %>%
    # create speciment data
    mutate(dat = purrr::map2(age, n, function(age, n) {
      table(size = growth_fun(rep(age, n))) %>%
        as.data.frame() 
    })) %>%
    unnest(dat) %>%
    rename(sample_factor = Freq) %>%
    mutate(size = as.numeric(as.character(size))) -> out
  
  return(out)
  
}

## analyze stratified sample
f_stratified_analysis <- function(data, population, size_range, plot = T, path_root){
  
  # compute population timeseries of abundance
  pop_abund = f_aggegrate_N(population, size_range = size_range)
  
  # compute sample timeseries of abundance within size range
  data %>%
    ungroup %>%
    # filter for exploitable size classes
    filter(size %in% size_range) %>%
    # compute cpue per station
    group_by(set, year, strat, cell, area_swept, strat_area) %>%
    summarise(catch = sum(sample_factor)) %>%
    group_by(set, year, strat_area) %>%
    summarise(cpue = mean(catch / area_swept) ,
              var = var(catch / area_swept) / n()) %>%
    ungroup %>%
    # expand to abundance, compute cv
    mutate(N_hat_h = cpue * strat_area,
           var_N_hat_h = var * strat_area^2) %>%
    group_by(set, year) %>%
    summarise(N_hat = sum(N_hat_h),
              se = sqrt(sum(var_N_hat_h)),
              cv = se / N_hat) %>%
    ungroup -> abund
  
  ## rmse
  rmse_age = function(ssq){sqrt(ssq / (length(0:18) * 20 * length(unique(data$set))))}
  rmse_size =  function(ssq){sqrt(ssq / (length(seq(20, 200, 5)) * 20 * length(unique(data$set))))}
  
  ### rmse for abundance at age 0-18
  data %>%
    ungroup %>%
    # compute cpue per station
    group_by(set, year, age, strat, cell, area_swept, strat_area) %>%
    summarise(catch = sum(sample_factor)) %>%
    group_by(set, age, year, strat_area) %>%
    summarise(cpue = mean(catch / area_swept)) %>%
    ungroup %>%
    # expand to abundance, compute cv
    mutate(N_hat_h = cpue * strat_area) %>%
    group_by(set, year, age) %>%
    summarise(N_hat = sum(N_hat_h))  %>%
    dplyr::select(set, year, age, N_hat) %>%
    # join to true abundance
    left_join(population1$N %>%
                as_tibble() %>%
                rownames_to_column(var = "age") %>%
                mutate(age = 0:18) %>%
                pivot_longer(2:ncol(.), names_to = "year", values_to = "N") %>%
                mutate(year = as.numeric(year),
                       age = as.numeric(age)),
              by = c("year", "age")) %>%
    # compute ssq
    mutate(sq_diff = (N_hat - N)^2) %>%
    pull(sq_diff) %>%
    sum() %>%
    #compute rmse
    rmse_age -> rmse_age
  
  ### rmse for abundance at size 20 - 200
 data %>%
    mutate(size_bin = round(size / 5) * 5) %>%
    filter(size_bin >= 20, size_bin <= 200) %>% 
    # compute cpue per station
    group_by(set, year, size_bin, strat, cell, area_swept, strat_area) %>%
    summarise(catch = sum(sample_factor)) %>%
    group_by(set, size_bin, year, strat_area) %>%
    summarise(cpue = mean(catch / area_swept)) %>%
    ungroup %>%
    # expand to abundance, compute cv
    mutate(N_hat_h = cpue * strat_area) %>%
    group_by(set, year, size_bin) %>%
    summarise(N_hat = sum(N_hat_h))  %>%
    dplyr::select(set, year, size_bin, N_hat) %>%
    # join to trend abundance
    left_join(population1$N_at_length %>%
                as.data.frame() %>%
                rownames_to_column(var = "size") %>%
                pivot_longer(2:ncol(.), names_to = "year", values_to = "N") %>%
                mutate(size = as.numeric(size), 
                       year = as.numeric(year), 
                       size_bin = round(size / 5) * 5) %>%
                group_by(year, size_bin) %>%
                summarise(N = sum(N)),
              by = c("year", "size_bin")) -> size_tmp
  size_tmp %>%
    # compute ssq
    mutate(sq_diff = (N_hat - N)^2) %>%
    pull(sq_diff) %>%
    sum() %>%
    #compute rmse
    rmse_size -> rmse_size
  
  
  if(plot == T){
    
    ## plot abundance  
    abund %>%
      ggplot()+
      geom_line(aes(x = year, y = N_hat / 1e6, group = set), color = "lightblue", size = 0.3)+
      geom_line(data = pop_abund, aes(x = year, y = N / 1e6, group = 1), color = "black")+
      labs(x = "Year", y = "Abundance (mil)") -> x
    ggsave(paste0(path_root, "_N_hat.png"), plot = x, height = 3, width = 5, units = "in")
    
    ## N hat residuals
    abund %>%
      left_join(pop_abund) %>%
      ggplot()+
      geom_boxplot(aes(x = factor(year), y = N_hat - N))+
      geom_hline(yintercept = 0, linetype = 2)+
      labs(x = "Year", y = expression(hat(N) - N)) -> x
    ggsave(paste0(path_root, "_N_hat_residuals.png"), plot = x, height = 3, width = 5, units = "in")
    
    ## plot cv  
    abund %>%
      ggplot()+
      geom_line(aes(x = year, y = cv, group = set), color = "lightblue", size = 0.3)+
      labs(x = "Year", y = "CV") -> x
    ggsave(paste0(path_root, "_cv.png"), plot = x, height = 3, width = 5, units = "in")
    
    # dot plot of residuals in size comp
    size_tmp %>%
      mutate(diff = N_hat - N) %>%
      group_by(year, size_bin) %>%
      summarise(diff = mean(diff)) %>%
      mutate(sign = ifelse(diff > 0, "> 0", "< 0")) %>%
      ggplot()+
      geom_point(aes(x = year, y = size_bin, size = abs(diff), color = sign))+
      labs(x = "Year", y = "Shell Height (mm)", color = "Sign", size = "Residual") -> x
    ggsave(paste0(path_root, "_size_comp_residual.png"), plot = x, height = 4, width = 6, units = "in")
    
    rm(x)
    
  }
  
  return(data.frame(rmse_age = rmse_age, rmse_size = rmse_size))
  
  # return(list(N_hat = dplyr::select(abund, set, year, N_hat, se, cv) %>%
  #               left_join(pop_abund, by = "year"),
  #             rmse_age = rmse_age,
  #             rmse_size = rmse_size))
  
  
}

# survey areas ----

# specify a survey area and grid
# depth profile 1
grid1 <- make_grid(x_range = c(-10, 10),
                   y_range = c(-20, 20),
                   res = c(2, 2),
                   depth_range = c(50, 150),
                   shelf_depth = 100,
                   shelf_width = 3,
                   n_div = 1,
                   strat_breaks = c(50, 95, 105, 150),
                   strat_splits = 1)

# depth profile 2
grid2 <- make_grid(x_range = c(-10, 10),
                   y_range = c(-20, 20),
                   res = c(2, 2),
                   depth_range = c(50, 150),
                   shelf_depth = 100,
                   shelf_width = 0,
                   n_div = 1,
                   strat_breaks = c(50, 100, 150),
                   strat_splits = 1)

# depth profile 3
grid3 <- make_grid(x_range = c(-10, 10),
                   y_range = c(-20, 20),
                   res = c(2, 2),
                   depth_range = c(100, 100),
                   shelf_depth = 100,
                   shelf_width = 0,
                   n_div = 1,
                   strat_breaks = c(100),
                   strat_splits = 1)

# plot grids and depth profiles
# as_tibble(grid1@data@values) %>%
#   rename_all(~grid1@data@names) %>%
#   bind_cols(as_tibble(sp::coordinates(grid1)), .) %>%
#   mutate(grid = "Grid - 1") %>%
#   bind_rows(as_tibble(grid2@data@values) %>% 
#               rename_all(~grid2@data@names) %>%
#               bind_cols(as_tibble(sp::coordinates(grid2)), .) %>%
#               mutate(grid = "Grid - 2")) %>%
#   bind_rows(as_tibble(grid3@data@values) %>% 
#               rename_all(~grid3@data@names) %>%
#               bind_cols(as_tibble(sp::coordinates(grid3)), .) %>%
#               mutate(grid = "Grid - 3")) -> grid_cells
# 
# tibble(grid = c("Grid - 1", "Grid - 1", "Grid - 2"),
#        yintercept = c(-6, 6, 0)) -> hline
# 
# # plot different survey grids
# ggplot()+
#   geom_tile(data = grid_cells, aes(x = y, y = -x, fill = depth), color = "grey20")+
#   geom_hline(data = hline, aes(yintercept = yintercept), size = 1)+
#   scale_fill_gradient(low = "darkslategray1", high = "deepskyblue4")+
#   coord_cartesian(expand = 0)+
#   labs(x = NULL, y = NULL, fill = "Depth (m)")+
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.background = element_blank(),
#         panel.border = element_rect(color = 1, size = 1),
#         strip.text.x = element_blank())+
#   facet_wrap(~grid) -> p1
# 
# ## depth profile 1
# grid_cells %>%
#   ggplot()+
#   geom_line(aes(x = depth, y = -x))+
#   labs(x = "Depth (m)", y = "Grid y-axis")+
#   facet_wrap(~grid) -> p2
  
# ggsave("./figures/fishery_independent/survey_design/survey_grids.png",
#        plot = p1 / p2,
#        height = 4, width = 6, units = "in")

# population ----

## number of ages, growth, and total mortality
ages <- 0:18
growth_fun <- sim_vonB(Linf = 172.86, L0 = 20.70, K = 0.2)
Z <- sim_Z(log_mean = matrix(c(rep(log(0.19), 80), rep(log(0.7), 300)), byrow = T, ncol = 20, nrow = 19))

## simulate population
set.seed(1716)
population1 <- sim_abundance(ages = ages,
                             years = 1:20,
                             Z = Z, 
                             growth = growth_fun)


f_aggegrate_N(population1, size_range = 100:200) %>%
  ggplot()+
  geom_point(aes(x = year, y = N / 1e6))+
  geom_line(aes(x = year, y = N / 1e6, group = 1))+
  labs(x = "Year", y = "Abundance (millions)")+
  scale_x_continuous(labels = yrs$labels, breaks = yrs$breaks)-> p1

ggsave("./figures/fishery_independent/survey_design/exploitable_animal_timeseries.png",
       plot = p1,
       height = 3, width = 5, units = "in")

# # plot age structured population
# 
#   
# # plot size comp
# N_at_age %>%
#   mutate(year_text = paste0("Year - ", year),
#          age = as.numeric(age)) %>%
#   filter(year == 1) %>%
#   ggplot()+
#   geom_bar(aes(x = age, y = N), 
#            stat = "identity", position = "dodge", width = 1, 
#            color = NA, fill = "grey60", alpha = 0.5)+
#   geom_text_npc(aes(npcx = "left", npcy = 0.3, label = year_text), check_overlap = T)+
#   labs(x = "Age", y = NULL)+
#   facet_wrap(~year_text, ncol = 1)+
#   theme(panel.border= element_blank(),
#           panel.spacing = unit(-1, "lines"),
#           strip.background = element_blank(),
#           strip.text.x = element_blank(),
#           axis.text.y = element_blank(),
#           axis.line.y = element_blank(),
#           axis.line.x = element_line(size = 0.1, color = "grey70"),
#           axis.ticks.y = element_blank(),
#           panel.background = element_blank()) -> x
# ggsave("./figures/fishery_independent/survey_design/population_1_size_comp.png",
#        height = 6, width = 4, units = "in")


# distribute population ----

## depth distribution of population
### mu = depth at highest animal density
### sigma = dispersion around mu
depth_fun <- sim_parabola(alpha = 0, mu = 100, sigma = 15)

## age-year-space covariance
## clustered
### strong covariance year to year
ays_cov1 <- sim_ays_covar(phi_age = 0.80, phi_year = 0.95, range = 300) 
### week covariance year to year
ays_cov2 <- sim_ays_covar(phi_age = 0.80, phi_year = 0.1, range = 300) 
## diffuse
### strong covariance year to year
ays_cov3 <- sim_ays_covar(phi_age = 0.80, phi_year = 0.95, range = 2000) 
### week covariance year to year
ays_cov4 <- sim_ays_covar(phi_age = 0.80, phi_year = 0.1, range = 2000) 

## grid 1
### age-year covariance 1
set.seed(2834)
dist1 <- sim_distribution(sim = population1,
                          grid = grid1, 
                          ays_covar = ays_cov1,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist1.png",
#        plot = f_plot_dist(dist1, "Grid - 1", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")
### age-year covariance 2
set.seed(2834)
dist2 <- sim_distribution(sim = population1,
                          grid = grid1, 
                          ays_covar = ays_cov2,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist2.png",
#        plot = f_plot_dist(dist2, "Grid - 1", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")
### age-year covariance 3
set.seed(2834)
dist3 <- sim_distribution(sim = population1,
                          grid = grid1, 
                          ays_covar = ays_cov3,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist3.png",
#        plot = f_plot_dist(dist3, "Grid - 1", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")
### age-year covariance 4
set.seed(2834)
dist4 <- sim_distribution(sim = population1,
                          grid = grid1, 
                          ays_covar = ays_cov4,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist4.png",
#        plot = f_plot_dist(dist4, "Grid - 1", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")

## grid 2
### age-year covariance 1
set.seed(2834)
dist5 <- sim_distribution(sim = population1,
                          grid = grid2, 
                          ays_covar = ays_cov1,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist5.png",
#        plot = f_plot_dist(dist5, "Grid - 2", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")

### age-year covariance 2
set.seed(2834)
dist6 <- sim_distribution(sim = population1,
                          grid = grid2, 
                          ays_covar = ays_cov2,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist6.png",
#        plot = f_plot_dist(dist6, "Grid - 2", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")

### age-year covariance 3
set.seed(2834)
dist7 <- sim_distribution(sim = population1,
                          grid = grid2, 
                          ays_covar = ays_cov3,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist7.png",
#        plot = f_plot_dist(dist7, "Grid - 2", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")

### age-year covariance 4
set.seed(2834)
dist8 <- sim_distribution(sim = population1,
                          grid = grid2, 
                          ays_covar = ays_cov4,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist8.png",
#        plot = f_plot_dist(dist8, "Grid - 2", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")

## grid 3
### age-year covariance 1
set.seed(2834)
dist9 <- sim_distribution(sim = population1,
                          grid = grid3, 
                          ays_covar = ays_cov1,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist9.png",
#        plot = f_plot_dist(dist9, "Grid - 3", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")

### age-year covariance 2
set.seed(2834)
dist10 <- sim_distribution(sim = population1,
                          grid = grid3, 
                          ays_covar = ays_cov2,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist10.png",
#        plot = f_plot_dist(dist10, "Grid - 3", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")

### age-year covariance 3
set.seed(2834)
dist11 <- sim_distribution(sim = population1,
                          grid = grid3, 
                          ays_covar = ays_cov3,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist11.png",
#        plot = f_plot_dist(dist11, "Grid - 3", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")

### age-year covariance 4
set.seed(2834)
dist12 <- sim_distribution(sim = population1,
                          grid = grid3, 
                          ays_covar = ays_cov4,
                          depth_par = depth_fun)
# ggsave("./figures/fishery_independent/survey_design/dist12.png",
#        plot = f_plot_dist(dist12, "Grid - 3", ages = 4:18),
#        heigh = 5, width = 6, unit = "in")


# density plot for distributions in grid 1
# tibble(distribution = list(dist1, dist2, dist3, dist4)) %>%
#   mutate(data = purrr::map(distribution, function(x) {
#     x$sp_N %>%
#       left_join(x$grid_xy, by = "cell") %>%
#       group_by(year, depth) %>%
#       summarise(N = sum(N)) 
#   }),
#   dist = letters[1:nrow(.)]) %>%
#   unnest(data) %>%
#   
#   ggplot()+
#   geom_density(aes(x = depth, weight = N, group = factor(dist)))+
#   labs(x = "Depth (m)", y = "Density", title = "Grid - 1")+
#   theme(plot.title = element_text(hjust = 0.5)) -> p1
# 
# # density plot for distributions in grid 2
# tibble(distribution = list(dist5, dist6, dist7, dist8)) %>%
#   mutate(data = purrr::map(distribution, function(x) {
#     x$sp_N %>%
#       left_join(x$grid_xy, by = "cell") %>%
#       group_by(year, depth) %>%
#       summarise(N = sum(N)) 
#   }),
#   dist = letters[1:nrow(.)]) %>%
#   unnest(data) %>%
#   
#   ggplot()+
#   geom_density(aes(x = depth, weight = N, group = factor(dist)))+
#   labs(x = "Depth (m)", y = "Density", title = "Grid - 2")+
#   theme(plot.title = element_text(hjust = 0.5)) -> p2
# 
# ggsave("./figures/fishery_independent/survey_design/depth_parabola.png",
#        plot = p1 + p2,
#        heigh = 3, width = 6, unit = "in")
  

# systematic samples ----

# loop through systematic sampling of distributions
distributions <- c("dist1", "dist2", "dist3", "dist4", "dist5", "dist6", "dist7", "dist8", "dist9", "dist10", "dist11", "dist12")

for(i in distributions){
  f_systematic_sample(eval(as.name(i)), iterations = 1) %>%
    saveRDS(paste0("./output/fishery_independent/survey_design/sample_design_systematic_",
           i, ".RDS"))
  print(i)
}

# stratified random samples ----

# loop through systematic sampling of distributions
distributions <- c("dist1", "dist2", "dist3", "dist4")

for(i in distributions){
  f_stratified_random_sample(eval(as.name(i)), iterations = 1, sample_rate = c(13, 40, 13)) %>%
    saveRDS(paste0("./output/fishery_independent/survey_design/sample_design_stratified_random_",
                   i, ".RDS"))
  print(i)
}

# loop through systematic sampling of distributions
distributions <- c("dist5", "dist6", "dist7", "dist8")

for(i in distributions){
  f_stratified_random_sample(eval(as.name(i)), iterations = 1, sample_rate = c(33, 33)) %>%
    saveRDS(paste0("./output/fishery_independent/survey_design/sample_design_stratified_random_",
                   i, ".RDS"))
  print(i)
}


# loop through systematic sampling of distributions
distributions <- c("dist9", "dist10", "dist11", "dist12")
distributions <- c("dist10")

for(i in distributions){
  f_stratified_random_sample(eval(as.name(i)), iterations = 100, sample_rate = 66) %>%
    saveRDS(paste0("./output/fishery_independent/survey_design/sample_design_stratified_random_",
                   i, ".RDS"))
  print(i)
}


# stratified systematic samples ---- 

# loop through systematic sampling of distributions
distributions <- c("dist1", "dist2", "dist3", "dist4")

for(i in distributions){
  f_stratified_systematic_sample(eval(as.name(i)), iterations = 1, sample_rate = c(4, 2, 4)) %>%
    saveRDS(paste0("./output/fishery_independent/survey_design/sample_design_stratified_systematic_",
                   i, ".RDS"))
  print(i)
}

# loop through systematic sampling of distributions
distributions <- c("dist5", "dist6", "dist7", "dist8")

for(i in distributions){
  f_stratified_systematic_sample(eval(as.name(i)), iterations = 1, sample_rate = c(3, 3)) %>%
    saveRDS(paste0("./output/fishery_independent/survey_design/sample_design_stratified_systematic_",
                   i, ".RDS"))
  print(i)
}



# analysis ----

# systematic analysis
tibble(output_data = list.files("./output/fishery_independent/survey_design/", 
                                pattern = "design_systematic_dist", full.names = T),
  
            distribution = gsub("\\.", "", str_extract(output_data, c("dist..")))) %>%
  mutate(analysis = purrr::map2(output_data, distribution, function(output_data, distribution){
    
    # read data 
    readRDS(output_data) %>%
      #do analysis
      f_systematic_analysis(., population1, size_range = 100:200, plot = T, 
                            path_root = paste0("./figures/fishery_independent/survey_design/", 
                                               distribution, "_systematic")) -> x
    
    out <- data.frame(rmse_age = x$rmse_age, rmse_size = x$rmse_size)
      
    return(out)
    
  }))  %>%
  saveRDS("./output/fishery_independent/survey_design/systematic_analysis.RDS")
  

# stratified random analysis

tibble(output_data = list.files("./output/fishery_independent/survey_design/", 
                                pattern = "design_stratified_random_dist", full.names = T),
       
       distribution = gsub("\\.", "", str_extract(output_data, c("dist..")))) %>%
  mutate(analysis = purrr::map2(output_data, distribution, function(output_data, distribution){
    
    # read data 
    readRDS(output_data) %>%
      #do analysis
      f_stratified_analysis(., population1, size_range = 100:200, plot = T, 
                            path_root = paste0("./figures/fishery_independent/survey_design/", 
                                               distribution,"_stratified_random")) -> out
    return(out)
    
  }))  %>%
  saveRDS("./output/fishery_independent/survey_design/stratified_random_analysis.RDS")

# stratified systematic analysis
tibble(output_data = list.files("./output/fishery_independent/survey_design/", 
                                pattern = "design_stratified_systematic_dist", full.names = T),
       
       distribution = gsub("\\.", "", str_extract(output_data, c("dist..")))) %>%
  mutate(analysis = purrr::map2(output_data, distribution, function(output_data, distribution){
    
    # read data 
    readRDS(output_data) %>%
      #do analysis
      f_stratified_analysis(., population1, size_range = 100:200, plot = T, 
                            path_root = paste0("./figures/fishery_independent/survey_design/", 
                                               distribution,"_stratified_systematic")) -> out
    return(out)
    
  }))  %>%
  saveRDS("./output/fishery_independent/survey_design/stratified_systematic_analysis3.RDS")


# join data
readRDS("./output/fishery_independent/survey_design/systematic_analysis.RDS") %>%
  unnest(analysis) %>%
  dplyr::select(-output_data) %>%
  mutate(design = "Systematic") %>%
  
  bind_rows(readRDS("./output/fishery_independent/survey_design/stratified_random_analysis.RDS") %>%
              unnest(analysis) %>%
              dplyr::select(-output_data) %>%
              mutate(design = "Stratified Random")) %>%

  bind_rows(readRDS("./output/fishery_independent/survey_design/stratified_systematic_analysis.RDS") %>%
              unnest(analysis) %>%
              dplyr::select(-output_data) %>%
              mutate(design = "Stratified Systematic")) %>%
  mutate(distribution = as.numeric(gsub("dist", "", distribution)),
         design = factor(design, levels = c("Systematic", "Stratified Random", "Stratified Systematic"))) -> rmse
  
# RMSE plot

rmse %>%
  rename(Age = rmse_age, `Shell Height` = rmse_size) %>%
  pivot_longer(c(Age, `Shell Height`), names_to = "type", values_to = "rmse") %>%
  ggplot()+
  geom_point(aes(y = distribution, x = rmse, shape = design))+
  scale_y_reverse(breaks = 1:12)+
  labs(x = "RMSE", y = "Distribution", shape = NULL)+
  scale_shape_manual(values = c(19, 8, 1))+
  facet_wrap(~type) -> x
ggsave("./figures/fishery_independent/survey_design/rmse_result.png", height = 5, width = 6, plot = x)


