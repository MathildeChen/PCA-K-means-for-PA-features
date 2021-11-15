# Script name: 02_DESCRIPTIVE_PA_METRICS.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Compute means and SD for all physical activity (PA) metrics.
# - for all days, difference between week days & weekend days
# - difference between sex
# - difference between outcomes [to do]

# -----------------------------
# Packages
library(tidyverse)
library(testthat)
library(corrplot)
library(wesanderson)
library(ggplot2)
library(cowplot)

# -----------------------------
# Palette colors 
pal <- wes_palette("Zissou1", 5, "discrete")

# -----------------------------
# Data

# > All PA metrics (with number of bouts of different lengths)
path_to_data <- "E:/PC_FIXE/Analysis/02_ARTICLE_2/00_DATA"
#   Non-standardized metrics
load(file.path(path_to_data, "00_data_full_PCA_k_means_july.rda"))   # non-standardized metrics, full set of PA metrics (including nb of bouts of different length)

# -----------------------------
# Table with PA metrics' name
source("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\00_DATA\\00_tab-name_PA_features.R")
# > Join data and tab.name to have a clean version of variables
data_named <- data_full %>%
  map(., ~ { 
    .x %>%
      tidyr::gather(key = "var", value = "value", -stno) %>% 
      left_join(tab.name, by = "var") %>% 
      dplyr::select(-var) %>% 
      tidyr::spread(key = "varname", value = "value") 
  })

# > Check variables names
data_named %>% 
  map_dfr(., ~{ data.frame(var = names(.x))}, .id = "dataset") %>%
  filter(var != "stno",
         !var %in% unique(tab.name$varname))
# 0 x 0 table --> OK

# -----------------------------
# PA metrics - mean & standard deviation - for all days, difference between week days & weekend days

# > Descriptive table of the (non-standardized) variables 
desc.tab <- data_named$wei %>% 
  left_join(data_named$WD_WE, by = "stno") %>% 
  gather(key = "feature", value = "value", -stno) %>% 
  separate(feature, sep = " - ", into = c("feature", "day_type")) %>% 
  group_by(feature, day_type) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            min = min(value),
            q25 = quantile(value, probs = c(0.25)),
            q50 = quantile(value, probs = c(0.5)),
            q75 = quantile(value, probs = c(0.75)),
            max = max(value)) %>% 
  arrange(day_type)

# > t-tests for difference between week days and weekend days

# Skewed variables: does t test and Wilcoxon test provide different conclusion? 
t.test.num <- tibble::tribble(
  ~feature,                                       ~p.t.test,                                                                                                                                                             ~p.wilcoxon, 
  "Acceleration (mg)",                            t.test(data_named$WD$`Acceleration (mg) - Week days`,                            data_named$WE$`Acceleration (mg) - Weekend days`)$p.value,                            wilcox.test(data_named$WD$`Acceleration (mg) - Week days`,                            data_named$WE$`Acceleration (mg) - Weekend days`)$p.value, 
  "Duration IN (min/day)",                        t.test(data_named$WD$`Duration IN (min/day) - Week days`,                        data_named$WE$`Duration IN (min/day) - Weekend days`)$p.value,                        wilcox.test(data_named$WD$`Duration IN (min/day) - Week days`,                        data_named$WE$`Duration IN (min/day) - Weekend days`)$p.value,
  "Duration LIPA (min/day)",                      t.test(data_named$WD$`Duration LIPA (min/day) - Week days`,                      data_named$WE$`Duration LIPA (min/day) - Weekend days`)$p.value,                      NA,
  "Duration MVPA (min/day)",                      t.test(data_named$WD$`Duration MVPA (min/day) - Week days`,                      data_named$WE$`Duration MVPA (min/day) - Weekend days`)$p.value,                      wilcox.test(data_named$WD$`Duration MVPA (min/day) - Week days`,                      data_named$WE$`Duration MVPA (min/day) - Weekend days`)$p.value,
  "Intensity gradient intercept",                 t.test(data_named$WD$`Intensity gradient intercept - Week days`,                 data_named$WE$`Intensity gradient intercept - Weekend days`)$p.value,                 NA,
  "Intensity gradient slope",                     t.test(data_named$WD$`Intensity gradient slope - Week days`,                     data_named$WE$`Intensity gradient slope - Weekend days`)$p.value,                     NA,
  "Mean duration of IN bouts",                    t.test(data_named$WD$`Mean duration of IN bouts - Week days`,                    data_named$WE$`Mean duration of IN bouts - Weekend days`)$p.value,                    NA,
  "Mean duration of LIPA bouts",                  t.test(data_named$WD$`Mean duration of LIPA bouts - Week days`,                  data_named$WE$`Mean duration of LIPA bouts - Weekend days`)$p.value,                  NA,
  "Mean duration of MVPA bouts",                  t.test(data_named$WD$`Mean duration of MVPA bouts - Week days`,                  data_named$WE$`Mean duration of MVPA bouts - Weekend days`)$p.value,                  wilcox.test(data_named$WD$`Mean duration of MVPA bouts - Week days`,                  data_named$WE$`Mean duration of MVPA bouts - Weekend days`)$p.value,
  "Most active 5hrs timing",                      t.test(data_named$WD$`Most active 5hrs timing - Week days`,                      data_named$WE$`Most active 5hrs timing - Weekend days`)$p.value,                      NA,
  "Number of IN bouts",                           t.test(data_named$WD$`Number of IN bouts - Week days`,                           data_named$WE$`Number of IN bouts - Weekend days`)$p.value,                           NA,
  "Number of LIPA bouts",                         t.test(data_named$WD$`Number of LIPA bouts - Week days`,                         data_named$WE$`Number of LIPA bouts - Weekend days`)$p.value,                         NA,
  "Number of MVPA bouts",                         t.test(data_named$WD$`Number of MVPA bouts - Week days`,                         data_named$WE$`Number of MVPA bouts - Weekend days`)$p.value,                         wilcox.test(data_named$WD$`Number of MVPA bouts - Week days`,                         data_named$WE$`Number of MVPA bouts - Weekend days`)$p.value,
  "Number of < 10 min IN fragments (unbouted)",   t.test(data_named$WD$`Number of < 10 min IN fragments (unbouted) - Week days`,   data_named$WE$`Number of < 10 min IN fragments (unbouted) - Weekend days`)$p.value,   NA,
  "Number of 10-30 min IN fragments",             t.test(data_named$WD$`Number of 10-30 min IN fragments - Week days`,             data_named$WE$`Number of 10-30 min IN fragments - Weekend days`)$p.value,             NA,
  "Number of > 30 min IN fragments",              t.test(data_named$WD$`Number of > 30 min IN fragments - Week days`,              data_named$WE$`Number of > 30 min IN fragments - Weekend days`)$p.value,              NA,
  "Number of < 10 min LIPA fragments (unbouted)", t.test(data_named$WD$`Number of < 10 min LIPA fragments (unbouted) - Week days`, data_named$WE$`Number of < 10 min LIPA fragments (unbouted) - Weekend days`)$p.value, NA,
  "Number of > 10 min LIPA fragments",            t.test(data_named$WD$`Number of > 10 min LIPA fragments - Week days`,            data_named$WE$`Number of > 10 min LIPA fragments - Weekend days`)$p.value,            wilcox.test(data_named$WD$`Number of > 10 min LIPA fragments - Week days`,            data_named$WE$`Number of > 10 min LIPA fragments - Weekend days`)$p.value,
  "Number of < 10 min MVPA fragments (unbouted)", t.test(data_named$WD$`Number of < 10 min MVPA fragments (unbouted) - Week days`, data_named$WE$`Number of < 10 min MVPA fragments (unbouted) - Weekend days`)$p.value, wilcox.test(data_named$WD$`Number of < 10 min MVPA fragments (unbouted) - Week days`, data_named$WE$`Number of < 10 min MVPA fragments (unbouted) - Weekend days`)$p.value,
  "Number of > 10 min MVPA fragments",            t.test(data_named$WD$`Number of > 10 min MVPA fragments - Week days`,            data_named$WE$`Number of > 10 min MVPA fragments - Weekend days`)$p.value,            wilcox.test(data_named$WD$`Number of > 10 min MVPA fragments - Week days`,            data_named$WE$`Number of > 10 min MVPA fragments - Weekend days`)$p.value
  
) %>% 
  # Labels
  mutate(p.t.test.round   = if_else(format(round(p.t.test, digits = 4), nsmall = 3) < 0.001, "<0.001", as.character(format(round(p.t.test, digits = 4), nsmall = 3))),
         p.wilcoxon.round = if_else(is.na(p.wilcoxon) == T, "NA", if_else(format(round(p.wilcoxon, digits = 4), nsmall = 3) < 0.001, "<0.001", as.character(format(round(p.wilcoxon, digits = 4), nsmall = 3)))))

# --> in the selected set of PA metrics: yes
# --> in the full set: different conclusion between t-test and Wilcoxon test in the Number of > 10 min LIPA fragments (skewness of 1.04 for this variable)

# > Mean, sd of all PA metrics, p for difference between week days and weekend days (t-test; $: Wilcoxon test for skewed variables)
full_desc.tab <- desc.tab %>% 
  # Label containing mean and sd for each metric
  mutate(mean.sd.lab = paste0(format(round(mean, 1), nsmall = 1), " (", format(round(sd, 2), nsmall = 2), ")")) %>% 
  dplyr::select(-mean, -sd, -min, -max, -q25, -q50, -q75) %>%
  spread(key = "day_type", value = "mean.sd.lab") %>% 
  # Add p value
  left_join(t.test.num, by = "feature") %>%
  #mutate(p = if_else(p.wilcoxon.round == "NA", p.t.test.round, paste0(p.wilcoxon.round, "$"))) %>% 
  #dplyr::select(-p.t.test, -p.t.test.round, -p.wilcoxon, -p.wilcoxon.round) %>% 
  # Add a column for distinguish variables for sensitivity analysis
  mutate(analysis = if_else(feature %in% c("Number of < 10 min IN fragments (unbouted)", "Number of 10-30 min IN fragments", "Number of > 30 min IN fragments", 
                                           "Number of < 10 min LIPA fragments (unbouted)", "Number of > 10 min LIPA fragments", 
                                           "Number of < 10 min MVPA fragments (unbouted)","Number of > 10 min MVPA fragments"), "Sensitivity analysis", "Main analysis")) %>% 
  arrange(analysis, feature)
  
write.csv2(full_desc.tab, file = "E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\descriptive_table.csv")

# -----------------------------
# PA metrics - distribution and density 

sensitivity_analysis <- c("Number of < 10 min IN fragments (unbouted)", "Number of 10-30 min IN fragments", "Number of > 30 min IN fragments", 
                          "Number of < 10 min LIPA fragments (unbouted)", "Number of > 10 min LIPA fragments", 
                          "Number of < 10 min MVPA fragments (unbouted)","Number of > 10 min MVPA fragments")

# > Plots
#   histogram
list(data_named$wei,
     data_named$WD,
     data_named$WE) %>% 
  map(., ~ {
    
    # -------------------------
    # > Selected set of metrics
    set1 <- .x %>% select(-paste0(sensitivity_analysis, " - ", sub(".* - ", "", names(.x)[2])))
    
    # > Histogram
    p.hist.1 <- ggplot() + 
      geom_histogram(data = set1 %>% 
                       gather(key = "variable", value = "value", -stno),
                     aes(value), fill = "grey") + 
      geom_vline(data = set1 %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(med = median(value)), 
                 aes(xintercept = med), linetype = 2, col = pal[1]) +
      geom_boxplot(data = set1 %>% 
                     gather(key = "variable", value = "value", -stno),
                   aes(x = value, y = -100), width = 150, col = pal[1], fill = pal[2],
                   outlier.shape = NA) +
      facet_wrap(~ variable, scales = "free") +
      theme_bw() + 
      theme(strip.background = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
    
    # > Density
    p.dens.1 <- ggplot() + 
      geom_density(data = set1 %>% 
                     gather(key = "variable", value = "value", -stno),
                   aes(value), ) + 
      geom_vline(data = set1 %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(med = median(value)), 
                 aes(xintercept = med), linetype = 2, col = pal[5]) +
      geom_vline(data = set1 %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(IQR1 = quantile(value, 0.25)), 
                 aes(xintercept = IQR1), linetype = 2, col = pal[2]) +
      geom_vline(data = set1 %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(IQR3 = quantile(value, 0.75)), 
                 aes(xintercept = IQR3), linetype = 2, col = pal[2]) +
      facet_wrap(~ variable, scales = "free") +
      theme_bw() + 
      theme(strip.background = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
    
    # > Save
    ggsave(plot = p.hist.1, 
           filename = paste0("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\histograms\\p.hist.1_", sub(".* - ", "", names(.x)[2]), "_july.png"),
           height = 6, 
           width = 12)
    ggsave(plot = p.dens.1, 
           filename = paste0("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\densities\\p.dens.1_", sub(".* - ", "", names(.x)[2]), "_july.png"),
           height = 6, 
           width = 12)
    
    # -------------------------
    # > Set of metrics for sensitivity analysis
    set2 <- .x %>% select(stno, paste0(sensitivity_analysis, " - ", sub(".* - ", "", names(.x)[2])))
    
    # > Histogram
    p.hist.2 <- ggplot() + 
      geom_histogram(data = set2 %>% 
                       gather(key = "variable", value = "value", -stno),
                     aes(value), fill = "grey") + 
      geom_vline(data = set2 %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(med = median(value)), 
                 aes(xintercept = med), linetype = 2, col = pal[1]) +
      geom_boxplot(data = set2 %>% 
                     gather(key = "variable", value = "value", -stno),
                   aes(x = value, y = -100), width = 150, col = pal[1], fill = pal[2],
                   outlier.shape = NA) +
      facet_wrap(~ variable, scales = "free") +
      theme_bw() + 
      theme(strip.background = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
    
    # > Density
    p.dens.2 <- ggplot() + 
      geom_density(data = set2 %>% 
                     gather(key = "variable", value = "value", -stno),
                   aes(value), ) + 
      geom_vline(data = set2 %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(med = median(value)), 
                 aes(xintercept = med), linetype = 2, col = pal[5]) +
      geom_vline(data = set2 %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(IQR1 = quantile(value, 0.25)), 
                 aes(xintercept = IQR1), linetype = 2, col = pal[2]) +
      geom_vline(data = set2 %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(IQR3 = quantile(value, 0.75)), 
                 aes(xintercept = IQR3), linetype = 2, col = pal[2]) +
      facet_wrap(~ variable, scales = "free") +
      theme_bw() + 
      theme(strip.background = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
    
    # > Save
    ggsave(plot = p.hist.2, 
           filename = paste0("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\histograms\\p.hist.2_", sub(".* - ", "", names(.x)[2]), "_july.png"),
           height = 6, 
           width = 12)
    ggsave(plot = p.dens.2, 
           filename = paste0("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\densities\\p.dens.2_", sub(".* - ", "", names(.x)[2]), "_july.png"),
           height = 6, 
           width = 12)
    
  })

# > Plots for comparison between all days (blue boxplots) week days (grey histograms) and weekend days (yellow histograms)
# Histogram
ggplot() + 
  # > histogram week days vs weekend days
  geom_histogram(data = data_named$WD_WE %>% 
                   gather(key = "feature", value = "value", -stno) %>% 
                   separate(feature, sep = " - ", into = c("feature", "day_type")),
                 aes(value, fill = day_type)) + 
  # > median and boxplot from all days
  geom_vline(data = data_named$wei %>% 
               gather(key = "feature", value = "value", -stno) %>% 
               separate(feature, sep = " - ", into = c("feature", "day_type")) %>% 
               group_by(feature) %>% 
               summarise(med = median(value)), 
             aes(xintercept = med), linetype = 2, col = pal[1]) +
  geom_boxplot(data = data_named$wei %>% 
                 gather(key = "feature", value = "value", -stno) %>%
                 separate(feature, sep = " - ", into = c("feature", "day_type")),
               aes(x = value, y = -100), width = 150, col = pal[1], fill = pal[2],
               outlier.shape = NA) +
  # > graphics settings
  scale_fill_manual(values = c("grey", pal[3]), 
                    name = "Day type") + 
  facet_wrap(~ feature, scales = "free") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") 

ggsave(filename = paste0("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\histograms\\p.hist.All days vs Week days vs Weekend days_july.png"),
       height = 8, 
       width = 16)

# Density
ggplot() + 
  # > density week days vs weekend days
  geom_density(data = data_named$WD_WE %>% 
                 gather(key = "feature", value = "value", -stno) %>% 
                 separate(feature, sep = " - ", into = c("feature", "day_type")),
               aes(value, col = day_type, fill = day_type), alpha = 0.3) + 
  # > median and boxplot from all days
  geom_vline(data = data_named$wei %>% 
               gather(key = "feature", value = "value", -stno) %>% 
               separate(feature, sep = " - ", into = c("feature", "day_type")) %>% 
               group_by(feature) %>% 
               summarise(med = median(value)), 
             aes(xintercept = med), linetype = 2, col = pal[1]) +
  geom_vline(data = data_named$wei %>% 
               gather(key = "feature", value = "value", -stno) %>%
               separate(feature, sep = " - ", into = c("feature", "day_type")) %>%
               group_by(feature) %>% 
               summarise(IQR1 = quantile(value, 0.25)), 
             aes(xintercept = IQR1), linetype = 2, col = pal[2]) +
  geom_vline(data = data_named$wei %>% 
               gather(key = "feature", value = "value", -stno) %>% 
               separate(feature, sep = " - ", into = c("feature", "day_type")) %>%
               group_by(feature) %>% 
               summarise(IQR3 = quantile(value, 0.75)), 
             aes(xintercept = IQR3), linetype = 2, col = pal[2]) +
  # > graphics settings
  scale_color_manual(values = c("grey", pal[3]), 
                     name = "Day type") + 
  scale_fill_manual(values = c("grey", pal[3]), 
                    name = "Day type") +
  facet_wrap(~ feature, scales = "free") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") 

ggsave(filename = paste0("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\densities\\p.dens.All days vs Week days vs Weekend days_july.png"),
       height = 8, 
       width = 16)

stop()
# -----------------------------
# PA metrics - mean & standard deviation - difference between sexes

# > To do
library(haven)
particip <- read_dta("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Data\\05_WHITEHALL\\s1quest.dta") %>% select(stno, sex)

# > Descriptive table of the (non-standardized) variables 
wei_sex <- particip %>%
  right_join(data_named$wei, by = "stno")

# > Mean & sd for each feature, by sex
desc.tab.sex <- wei_sex %>% 
  gather(key = "feature", value = "value", -stno, -sex) %>% 
  group_by(feature, sex) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(lab = paste0(format(round(mean, 1), nsmall = 1), " (", format(round(sd, 2), nsmall = 2), ")")) %>% 
  select(-mean, -sd) %>% 
  spread(key = "sex", value = "lab") %>% 
  dplyr::rename("Men" = "1", "Women" = "2")

# > T-test or Wilcoxon test for skewed variables
t.test.num.sex <- tibble::tribble(
  ~feature,                                       ~p.t.test,                                                                                        ~p.wilcoxon, 
  "Acceleration (mg)",                            t.test(`Acceleration (mg) - All days` ~ sex, data = wei_sex)$p.value,                            wilcox.test(`Acceleration (mg) - All days` ~ sex, data = wei_sex)$p.value,
  "Duration IN (min/day)",                        t.test(`Duration IN (min/day) - All days` ~ sex, data = wei_sex)$p.value,                        wilcox.test(`Duration IN (min/day) - All days` ~ sex, data = wei_sex)$p.value,
  "Duration LIPA (min/day)",                      t.test(`Duration LIPA (min/day) - All days` ~ sex, data = wei_sex)$p.value,                      NA,
  "Duration MVPA (min/day)",                      t.test(`Duration MVPA (min/day) - All days` ~ sex, data = wei_sex)$p.value,                      wilcox.test(`Duration MVPA (min/day) - All days` ~ sex, data = wei_sex)$p.value,
  "Intensity gradient intercept",                 t.test(`Intensity gradient intercept - All days` ~ sex, data = wei_sex)$p.value,                 NA,
  "Intensity gradient slope",                     t.test(`Intensity gradient slope - All days` ~ sex, data = wei_sex)$p.value,                     NA,
  "Mean duration of IN bouts",                    t.test(`Mean duration of IN bouts - All days` ~ sex, data = wei_sex)$p.value,                    NA,
  "Mean duration of LIPA bouts",                  t.test(`Mean duration of LIPA bouts - All days` ~ sex, data = wei_sex)$p.value,                  NA,
  "Mean duration of MVPA bouts",                  t.test(`Mean duration of MVPA bouts - All days` ~ sex, data = wei_sex)$p.value,                  wilcox.test(`Mean duration of MVPA bouts - All days` ~ sex, data = wei_sex)$p.value,
  "Most active 5hrs timing",                      t.test(`Most active 5hrs timing - All days` ~ sex, data = wei_sex)$p.value,                      wilcox.test(`Most active 5hrs timing - All days` ~ sex, data = wei_sex)$p.value,
  "Number of IN bouts",                           t.test(`Number of IN bouts - All days` ~ sex, data = wei_sex)$p.value,                           NA,
  "Number of LIPA bouts",                         t.test(`Number of LIPA bouts - All days` ~ sex, data = wei_sex)$p.value,                         NA,
  "Number of MVPA bouts",                         t.test(`Number of MVPA bouts - All days` ~ sex, data = wei_sex)$p.value,                         wilcox.test(`Number of MVPA bouts - All days` ~ sex, data = wei_sex)$p.value,
  "Number of < 10 min IN fragments (unbouted)",   t.test(`Number of < 10 min IN fragments (unbouted) - All days` ~ sex, data = wei_sex)$p.value,   NA,
  "Number of 10-30 min IN fragments",             t.test(`Number of 10-30 min IN fragments - All days` ~ sex, data = wei_sex)$p.value,             NA,
  "Number of > 30 min IN fragments",              t.test(`Number of > 30 min IN fragments - All days` ~ sex, data = wei_sex)$p.value,              NA,
  "Number of < 10 min LIPA fragments (unbouted)", t.test(`Number of < 10 min LIPA fragments (unbouted) - All days` ~ sex, data = wei_sex)$p.value, NA,
  "Number of > 10 min LIPA fragments",            t.test(`Number of > 10 min LIPA fragments - All days` ~ sex, data = wei_sex)$p.value,            wilcox.test(`Number of > 10 min LIPA fragments - All days` ~ sex, data = wei_sex)$p.value,
  "Number of < 10 min MVPA fragments (unbouted)", t.test(`Number of < 10 min MVPA fragments (unbouted) - All days` ~ sex, data = wei_sex)$p.value, wilcox.test(`Number of < 10 min MVPA fragments (unbouted) - All days` ~ sex, data = wei_sex)$p.value,
  "Number of > 10 min MVPA fragments",            t.test(`Number of > 10 min MVPA fragments - All days` ~ sex, data = wei_sex)$p.value,            wilcox.test(`Number of > 10 min MVPA fragments - All days` ~ sex, data = wei_sex)$p.value) %>% 
  # Labels
  mutate(p.t.test.round   = if_else(format(round(p.t.test, digits = 4), nsmall = 3) < 0.001, "<0.001", as.character(format(round(p.t.test, digits = 4), nsmall = 3))),
         p.wilcoxon.round = if_else(is.na(p.wilcoxon) == T, "NA", if_else(format(round(p.wilcoxon, digits = 4), nsmall = 3) < 0.001, "<0.001", as.character(format(round(p.wilcoxon, digits = 4), nsmall = 3)))))

# Do t-test and Wilcoxon test provide the same conclusion?
# --> in the selected set of PA metrics: yes
# --> in the full set: yes

# > Mean, sd of all PA metrics, p for difference between week days and weekend days (t-test; $: Wilcoxon test for skewed variables)
desc.tab.sex %>% 
  mutate(feature = substr(feature, 1, nchar(feature) - 11)) %>%
  # Add p value
  left_join(t.test.num.sex, by = "feature") %>%
  mutate(p = if_else(p.wilcoxon.round == "NA", p.t.test.round, paste0(p.wilcoxon.round, "$"))) %>% 
  select(-p.t.test, -p.t.test.round, -p.wilcoxon, -p.wilcoxon.round) %>% 
  # Add a column for distinguish variables for sensitivity analysis
  mutate(analysis = if_else(feature %in% c("Number of < 10 min IN fragments (unbouted)", "Number of 10-30 min IN fragments", "Number of > 30 min IN fragments", 
                                           "Number of < 10 min LIPA fragments (unbouted)", "Number of > 10 min LIPA fragments", 
                                           "Number of < 10 min MVPA fragments (unbouted)","Number of > 10 min MVPA fragments"), "Sensitivity analysis", "Main analysis")) %>% 
  arrange(analysis, feature)

# -----------------------------
# PA metrics - mean & standard deviation - difference between outcomes

# > To do
