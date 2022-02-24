# notes ----
## evaluate difference in top and botom shell from 2021 dredge survey in YAK
## Tyler Jackson
## 8/11/2021

# load ----

library(tidyverse)
library(ggfortify)

# default ggplot theme
theme_set(theme_bw())

# data ----

sh <- read_csv("./data/statewide_scallop_survey/special_project/topbottomshell_2101.csv")

## remove outliers (see eda)
sh %>%
  slice(-92, -329, -392) -> sh2

# eda ----

## look at data
ggplot(sh)+
  geom_point(aes(x = topshell, y = bottomshell), color = "lightblue", alpha = 0.5)+
  geom_line(aes(x = topshell, y = topshell), linetype = 2)+
  labs(x = "Top Valve (mm)", y = "Bottom Valve (mm)") -> x
ggsave("./figures/scallop_research/shell_measurement/scatter_all_data.png", plot = x, height = 3, width = 4, units = "in")

## find outliers
lm_fit <- lm(bottomshell ~ topshell, data = sh)
plot(lm_fit, which = 4) # 392, 329, 92

## create and export sample table
sh2 %>%
  # group by 10 mm bins
  mutate(topbin = paste0(floor(topshell / 10) * 10, "-", (floor(topshell / 10) * 10) + 9, " mm")) %>%
  group_by(topbin) %>%
  summarise(n = n(),
            n_hauls = length(unique(haul)),
            mean_top = mean(topshell),
            mean_diff = mean(bottomshell - topshell)) %>%
  # reorder rows
  mutate(lower_bin = as.numeric(gsub("-\\d\\d mm|-\\d\\d\\d mm", "", topbin))) %>%
  arrange(lower_bin) %>% dplyr::select(-lower_bin) %>%
  write_csv("./output/scallop_research/shell_measurement/sample_table.csv")

## plot of sample size by bin
read_csv("./output/scallop_research/shell_measurement/sample_table.csv") %>%
  # reorder rows
  mutate(lower_bin = as.numeric(gsub("-\\d\\d mm|-\\d\\d\\d mm", "", topbin)) + 5) %>%
  ggplot()+
  geom_bar(aes(x = lower_bin, y = n), stat = "identity", fill = "mediumpurple")+
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 200, 10), limits = c(0, 60))+
  scale_x_continuous(breaks = seq(0, 200, 10))+
  labs(x = "Top Valve Shell Height (mm)", y = "Sample Size") -> x
ggsave("./figures/scallop_research/shell_measurement/sample_size.png", plot = x, height = 3, width = 6, units = "in")


# linear conversion ----

## model with intercept
shlm <- lm(bottomshell ~ topshell, data = sh2)
autoplot(shlm)
summary(shlm)

## no intercept model
shlm2 <- lm(bottomshell ~ topshell - 1, data = sh2)
autoplot(shlm2)
summary(shlm2)

##  compare models
anova(shlm2, shlm)

## save plot of no intercept model
sh2 %>%
  mutate(bsh_pred = predict(shlm2)) %>%
  ggplot()+
  geom_point(aes(x = topshell, y = bottomshell), color = "lightblue", alpha = 0.5)+
  geom_line(aes(x = topshell, y = bsh_pred), color = "red", alpha = 0.5)+
  geom_line(aes(x = topshell, y = topshell), linetype = 2)+
  labs(x = "Top Valve (mm)", y = "Bottom Valve (mm)") -> x
ggsave("./figures/scallop_research/shell_measurement/scatter_fit.png", plot = x, height = 3, width = 4, units = "in")


