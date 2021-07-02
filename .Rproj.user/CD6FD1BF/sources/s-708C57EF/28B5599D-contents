# Script name: 02_DESCRIPTIVE_PA_METRICS.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 


# -----------------------------
# Packages
library(tidyverse)
library(testthat)

# -----------------------------
# Data

# > Selected set of metrics (without number of bouts of different lengths)
#   Non-standardized metrics
load("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\00_DATA\\00_data_PCA_k_means.rda")

# -----------------------------
# Table with PA metrics' name

tab.name <- tibble::tribble(
  ~var,                              ~varname,
  # Average acceleration
  "ACC_day_mg_wei",                  "Acceleration (mg) - All days",
  "ACC_day_mg_WD",                   "Acceleration (mg) - Week days",
  "ACC_day_mg_WE",                   "Acceleration (mg) - Weekend days",
  
  # Time in SB, LIPA and MVPA
  "dur_day_total_IN_min_wei",        "Duration IN (min/day) - All days",
  "dur_day_total_LIG_min_wei",       "Duration LIPA (min/day) - All days",
  "dur_day_total_MVPA_min_wei",      "Duration MVPA (min/day) - All days",
  
  "dur_day_total_IN_min_WD",         "Duration IN (min/day) - Week days",
  "dur_day_total_LIG_min_WD",        "Duration LIPA (min/day) - Week days",
  "dur_day_total_MVPA_min_WD",       "Duration MVPA (min/day) - Week days",
  
  "dur_day_total_IN_min_WE",         "Duration IN (min/day) - Weekend days",
  "dur_day_total_LIG_min_WE",        "Duration LIPA (min/day) - Weekend days",
  "dur_day_total_MVPA_min_WE",       "Duration MVPA (min/day) - Weekend days",
  
  # Mean duration of fragments of SB, LIPA and MVPA
  "FRAG_mean_dur_IN_day_wei",         "Mean duration of IN bouts - All days",
  "FRAG_mean_dur_LIPA_day_wei",       "Mean duration of LIPA bouts - All days",
  "FRAG_mean_dur_MVPA_day_wei",       "Mean duration of MVPA bouts - All days",
  
  "FRAG_mean_dur_IN_day_WD",          "Mean duration of IN bouts - Week days",
  "FRAG_mean_dur_LIPA_day_WD",        "Mean duration of LIPA bouts - Week days",
  "FRAG_mean_dur_MVPA_day_WD",        "Mean duration of MVPA bouts - Week days",
  "FRAG_mean_dur_IN_day_WE",          "Mean duration of IN bouts - Weekend days",
  "FRAG_mean_dur_LIPA_day_WE",        "Mean duration of LIPA bouts - Weekend days",
  "FRAG_mean_dur_MVPA_day_WE",        "Mean duration of MVPA bouts - Weekend days",
  
  
  # Number of fragents of SB, LIPA and MVPA
  "FRAG_Nfrag_IN_day_wei",            "Number of IN bouts - All days",
  "FRAG_Nfrag_LIPA_day_wei",          "Number of LIPA bouts - All days",
  "FRAG_Nfrag_MVPA_day_wei",          "Number of MVPA bouts - All days",
  "Nblocks_day_IN_unbt_wei",          "Number of < 10 min IN fragments (unbouted) - All days",
  "Nblocks_day_IN_bts_10_30_wei",     "Number of 10-30 min IN fragments - All days",
  "Nblocks_day_IN_bts_30_wei",        "Number of > 30 min IN fragments - All days",
  "Nblocks_day_LIG_unbt_wei",         "Number of < 10 min LIPA fragments (unbouted) - All days",
  "Nblocks_day_LIG_bts_10_wei",       "Number of > 10 min LIPA fragments - All days",
  "Nblocks_day_MVPA_unbt_wei",        "Number of < 10 min MVPA fragments (unbouted) - All days",
  "Nblocks_day_MVPA_bts_10_wei",      "Number of > 10 min MVPA fragments - All days",
  
  "FRAG_Nfrag_IN_day_WD",             "Number of IN bouts - Week days",
  "FRAG_Nfrag_LIPA_day_WD",           "Number of LIPA bouts - Week days",
  "FRAG_Nfrag_MVPA_day_WD",           "Number of MVPA bouts - Week days",
  "Nblocks_day_IN_unbt_WD",           "Number of < 10 min IN fragments (unbouted) - Week days",
  "Nblocks_day_IN_bts_10_30_WD",      "Number of 10-30 min IN fragments - Week days",
  "Nblocks_day_IN_bts_30_WD",         "Number of > 30 min IN fragments - Week days",
  "Nblocks_day_LIG_unbt_WD",          "Number of < 10 min LIPA fragments (unbouted) - Week days",
  "Nblocks_day_LIG_bts_10_WD",        "Number of > 10 min LIPA fragments - Week days",
  "Nblocks_day_MVPA_unbt_WD",         "Number of < 10 min MVPA fragments (unbouted) - Week days",
  "Nblocks_day_MVPA_bts_10_WD",       "Number of > 10 min MVPA fragments - Week days",
  
  "FRAG_Nfrag_IN_day_WE",             "Number of IN bouts - Weekend days",
  "FRAG_Nfrag_LIPA_day_WE",           "Number of LIPA bouts - Weekend days",
  "FRAG_Nfrag_MVPA_day_WE",           "Number of MVPA bouts - Weekend days",
  "Nblocks_day_IN_unbt_WE",           "Number of < 10 min IN fragments (unbouted) - Weekend days",
  "Nblocks_day_IN_bts_10_30_WE",      "Number of 10-30 min IN fragments - Weekend days",
  "Nblocks_day_IN_bts_30_WE",         "Number of > 30 min IN fragments - Weekend days",
  "Nblocks_day_LIG_unbt_WE",          "Number of < 10 min LIPA fragments (unbouted) - Weekend days",
  "Nblocks_day_LIG_bts_10_WE",        "Number of > 10 min LIPA fragments - Weekend days",
  "Nblocks_day_MVPA_unbt_WE",         "Number of < 10 min MVPA fragments (unbouted) - Weekend days",
  "Nblocks_day_MVPA_bts_10_WE",       "Number of > 10 min MVPA fragments - Weekend days",
  
  # Timing of the most 5 active hours
  "M5TIME_num_wei",                   "Most active 5hrs timing - All days",
  "M5TIME_num_WD",                    "Most active 5hrs timing - Week days",
  "M5TIME_num_WE",                    "Most active 5hrs timing - Weekend days",
  
  # Intensity gradient
  "ig_gradient_wei",                  "Intensity gradient slope - All days", 
  "ig_intercept_wei",                 "Intensity gradient intercept - All days",
  "ig_gradient_WD",                   "Intensity gradient slope - Week days", 
  "ig_intercept_WD",                  "Intensity gradient intercept - Week days",
  "ig_gradient_WE",                   "Intensity gradient slope - Weekend days", 
  "ig_intercept_WE",                  "Intensity gradient intercept - Weekend days",
  
  # Labels for transformed skewed variables
  "log_ACC_day_mg_wei",              "Acceleration* (mg) - All days",
  "log_ACC_day_mg_WD",               "Acceleration* (mg) - Week days",
  "log_ACC_day_mg_WE",               "Acceleration* (mg) - Weekend days",
  "sqrt_ACC_day_mg_wei",             "Acceleration* (mg) - All days",
  "sqrt_ACC_day_mg_WD",              "Acceleration* (mg) - Week days",
  "sqrt_ACC_day_mg_WE",              "Acceleration* (mg) - Weekend days",
  
  "log_dur_day_total_MVPA_min_wei",  "Duration MVPA* (min/day) - All days",
  "sqrt_dur_day_total_MVPA_min_wei", "Duration MVPA* (min/day) - All days",
  
  "log_dur_day_total_MVPA_min_WD",   "Duration MVPA* (min/day) - Week days",
  "sqrt_dur_day_total_MVPA_min_WD",  "Duration MVPA* (min/day) - Week days",
  
  "log_dur_day_total_MVPA_min_WE",   "Duration MVPA* (min/day) - Weekend days",
  "sqrt_dur_day_total_MVPA_min_WE",  "Duration MVPA* (min/day) - Weekend days",
  
  "log_FRAG_mean_dur_IN_day_wei",     "Mean duration of IN bouts* - All days",
  "sqrt_FRAG_mean_dur_IN_day_wei",    "Mean duration of IN bouts* - All days",
  "log_FRAG_mean_dur_MVPA_day_wei",   "Mean duration of MVPA bouts* - All days",
  "sqrt_FRAG_mean_dur_MVPA_day_wei",  "Mean duration of MVPA bouts* - All days",
  
  "log_FRAG_mean_dur_IN_day_WD",      "Mean duration of IN bouts* - Week days",
  "sqrt_FRAG_mean_dur_IN_day_WD",     "Mean duration of IN bouts* - Week days",
  "log_FRAG_mean_dur_MVPA_day_WD",    "Mean duration of MVPA bouts* - Week days",
  "sqrt_FRAG_mean_dur_MVPA_day_WD",   "Mean duration of MVPA bouts* - Week days",
  
  "log_FRAG_mean_dur_IN_day_WE",      "Mean duration of IN bouts* - Weekend days",
  "sqrt_FRAG_mean_dur_IN_day_WE",     "Mean duration of IN bouts* - Weekend days",
  "log_FRAG_mean_dur_MVPA_day_WE",    "Mean duration of MVPA bouts* - Weekend days",
  "sqrt_FRAG_mean_dur_MVPA_day_WE",   "Mean duration of MVPA bouts* - Weekend days",
  
  "log_Nblocks_day_LIG_bts_10_wei",   "Number of > 10 min LIPA fragments* - All days",
  "log_Nblocks_day_LIG_bts_10_wei",   "Number of > 10 min LIPA fragments* - All days",
  "sqrt_Nblocks_day_MVPA_bts_10_wei", "Number of > 10 min MVPA fragments* - All days",
  "sqrt_Nblocks_day_MVPA_bts_10_wei", "Number of > 10 min MVPA fragments* - All days",
  
  "log_M5TIME_num_wei",               "Most active 5hrs timing* - All days",
  "log_M5TIME_num_WD",                "Most active 5hrs timing* - Week days",
  "log_M5TIME_num_WE",                "Most active 5hrs timing* - Weekend days",
  "sqrt_M5TIME_num_wei",              "Most active 5hrs timing* - All days",
  "sqrt_M5TIME_num_WD",               "Most active 5hrs timing* - Week days",
  "sqrt_M5TIME_num_WE",               "Most active 5hrs timing* - Weekend days"
  
)

# > Join data and tab.name to have a clean version of variables
data_named <- data %>%
  map(., ~ { 
    .x %>%
      tidyr::gather(key = "var", value = "value", -stno) %>% 
      left_join(tab.name, by = "var") %>% 
      select(-var) %>% 
      tidyr::spread(key = "varname", value = "value") 
  })

# > Check variables names
data_named %>% 
  map_dfr(., ~{ data.frame(var = names(.x))}, .id = "dataset") %>%
  filter(var != "stno",
         !var %in% unique(tab.name$varname))
# 0 x 0 table --> OK

# > Split list into dataframes (useless?)
data_wei        <- data$wei 
data_WD         <- data$WD 
data_WE         <- data$WE 
data_WD_WE      <- data$WD_WE 
# with log transformed variables
data_wei_log    <- data$wei_log
data_WD_log     <- data$WD_log
data_WE_log     <- data$WE_log
data_WD_WE_log  <- data$WD_WE_log 
# with sqrt transformed variables
data_wei_sqrt   <- data$wei_sqrt
data_WD_sqrt    <- data$WD_sqrt
data_WE_sqrt    <- data$WE_sqrt
data_WD_WE_sqrt <- data$WD_WE_sqrt

# -----------------------------
# PA metrics - mean & standard deviation

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
acc_wd <- data_named$WD$`Acceleration (mg) - Week days`
acc_we <- data_named$WE$`Acceleration (mg) - Weekend days`

dur_mvpa_wd <- data_named$WD$`Duration MVPA (min/day) - Week days`
dur_mvpa_we <- data_named$WE$`Duration MVPA (min/day) - Weekend days`

mean_mvpa_wd <- data_named$WD$`Mean duration of MVPA bouts - Week days`
mean_mvpa_we <- data_named$WE$`Mean duration of MVPA bouts - Weekend days`


t.test(acc_wd, acc_we, paired = T)$p.value      # p<2.2e-16 
wilcox.test(acc_wd, acc_we, paired = T)$p.value # p<2.2e-16

t.test(dur_mvpa_wd, dur_mvpa_we, paired = T)$p.value      # p<2.2e-16 
wilcox.test(dur_mvpa_wd, dur_mvpa_we, paired = T)$p.value # p<2.2e-16

t.test(mean_mvpa_wd, mean_mvpa_we, paired = T)$p.value      # p=0.0008216 
wilcox.test(mean_mvpa_wd, mean_mvpa_we, paired = T)$p.value # p<2.2e-16
# t test and Wilcoxon test provide similar conclusion for skewed variables

# t-tests
t.test.num <- tibble::tribble(
  ~var, ~p, 
  "Acceleration (mg)", wilcox.test(data_named$WD$`Acceleration (mg) - Week days`, data_named$WE$`Acceleration (mg) - Weekend days`, paired = T)$p.value, 
  #"Acceleration* (mg)" = wilcox.test(data_named$WD$`Acceleration (mg) - Week days`, data_named$WE$`Acceleration (mg) - Weekend days`, paired = T)$p.value, 
  "Duration IN (min/day)", 
  "Duration LIPA (min/day)", 
  "Duration MVPA (min/day)", wilcox.test(data_named$WD$`Duration MVPA (min/day) - Week days`, data_named$WE$`Duration MVPA (min/day) - Weekend days`, paired = T)$p.value, 
  "Duration MVPA* (min/day)",
  "Intensity gradient intercept", 
  "Intensity gradient slope", 
  "Mean duration of IN bouts", 
  "Mean duration of LIPA bouts", 
  "Mean duration of MVPA bouts", wilcox.test(data_named$WD$`Mean duration of MVPA bouts - Week days`, data_named$WE$`Mean duration of MVPA bouts - Weekend days`, paired = T)$p.value, 
  "Most active 5hrs timing", 
  "Number of IN bouts", 
  "Number of LIPA bouts", 
  "Number of MVPA bouts",
  
  
)



# -----------------------------
# PA metrics - distribution and density 
