# notes ----
## Round Weight vs Meat Wwight, data exploration
## Tyler Jackson
## tyler.jackson@alaska.gov

# load ----

library(tidyverse)
library(FNGr); theme_set(theme_sleek() + theme(legend.position = "bottom"))

### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# data ----

awl <- read_csv("./data/statewide_scallop_survey/ScalMeatTableData_2016_2019.csv")

# mw~rw plot ----

awl %>%
  filter(!is.na(meat_weight), !is.na(whole_wt)) %>%
  ggplot()+
  geom_point(aes(x = whole_wt, y = meat_weight), color = cb_palette[1], alpha = 0.2)+
  geom_smooth(aes(x = whole_wt, y = whole_wt * 0.1), method = "lm", color = 1, linetype = 2)+
  geom_smooth(aes(x = whole_wt, y = meat_weight), method = "loess", color = cb_palette[6], se = F)+
  coord_cartesian(expand = c(0, 0), xlim = c(0, 605), ylim = c(0, 60.5))+
  labs(x = "Round weight (g)", y = "Meat weight (g)") -> x
ggsave("./figures/ghl_supplement/2020/dredge_survey_mw_rw_scatterplot.png", plot = x,
       height = 4, width = 6, units = "in")

# actual ratio
awl %>%
  filter(!is.na(meat_weight), !is.na(whole_wt)) %>%
  summarise(ratio = mean(meat_weight / whole_wt))
