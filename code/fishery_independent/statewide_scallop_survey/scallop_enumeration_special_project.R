# notes ----
## scallop survey catch estimation special project
## Tyler Jackson
## tyler.jackson@alaska.gv
## last updated: 7/17/2020

# load ----
library(tidyverse)
library(FNGr); theme_set(theme_sleek() + theme(legend.position = "bottom"))

# data ----
catch <- read_csv(list.files(pattern = "scallop_enumeration_catch_sample.csv", recursive = TRUE))

size <- read_csv(list.files(pattern = "scallop_enumeration_size_sample.csv", recursive = TRUE))

# compute total catch estimae for haul ----

## known total catch
catch %>%
  dplyr::select(sg, number, wt_kg) %>%
  group_by(sg) %>%
  summarise_all(sum) -> known

## estimate total catch
catch %>%
  dplyr::select(sg_special, sg, measured_special, number, wt_kg) %>%
  group_by(sg_special, sg, measured_special) %>%
  summarise_all(sum) %>%
  # compute denisty by sg_special group
  group_by(sg_special) %>%
  mutate(density = number[measured_special == T] / wt_kg[measured_special == T]) %>%
  # compute estimated number
  mutate(est_number = wt_kg * density) %>%
  group_by(sg) %>%
  summarise(est_number = sum(est_number),
            wt_kg = sum(wt_kg)) %>%
  # join to known catch 
  left_join(known, by = c("sg", "wt_kg")) %>%
  # reorder to make a nicer table
  dplyr::select(sg, number, est_number, wt_kg) -> catch_estimate

write_csv(catch_estimate, "./output/fishery_independent/2020/scallop_enumeration_catch_estimate.csv")
  



# compute catch size composition ----

## density plot
### standard survey sample size comp
size %>%
  # subset for the sample of 40
  filter(sg_special %in% 1:2) %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., fill = factor(sg)), binwidth = 5, color = "black")+
  labs(x = "Shell Height (mm)", y = "Density", fill = "Shell Group") -> p1
### larger sample size
size %>%
  filter(!(sg_special %in% 1:2)) %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., fill = factor(sg_special )), binwidth = 5, color = "black")+
  labs(x = "Shell Height (mm)", y = "Density", fill = "Shell Group") -> p2

ggsave("./figures/fishery_independent/2020/scallop_enumeration_size_comp.png", 
       plot = cowplot::plot_grid(p1, p2, nrow = 2), height = 5, width = 6, units = "in")
  
## estimate of number of scallops in 5 mm size bin
### standard sampling
size %>%
  # subset for the sample of 40
  filter(sg_special %in% 1:2) %>%
  mutate(sh_bin = ceiling(sh / 5) * 5) %>%
  count(sh_bin) %>%
  mutate(w_s = n / sum(n)) %>%
  dplyr::select(sh_bin, w_s) -> sh_w_s

### extra sampling
size %>%
  filter(!(sg_special %in% 1:2)) %>%
  mutate(sh_bin = ceiling(sh / 5) * 5) %>%
  count(sh_bin) %>%
  mutate(w_e = n / sum(n)) %>%
  dplyr::select(sh_bin, w_e) -> sh_w_e

### estimate total catch by size bin
right_join(sh_w_s, sh_w_e, by = "sh_bin") %>%
  # join to total catch of scallops
  mutate(n = sum(known$number), 
         est_s = w_s * n,
         est_e = w_e * n) %>%
  replace_na(list(est_s = 0, est_e = 0)) %>%
  dplyr::select(sh_bin, est_s, est_e) -> size_comp

write_csv(size_comp, "./output/fishery_independent/2020/scallop_enumeration_size_comp.csv")


  
