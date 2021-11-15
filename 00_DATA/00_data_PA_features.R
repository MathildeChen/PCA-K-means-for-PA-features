# Script name: 00_PCA_k_means_data.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 
#     1. Preparing data for PA clustering
#       PA features (overall activity level, total duration, frequency, 
#       typical duration, chronotype and activity intensity distribution) 
#       computed: 
#       - for weighted estimated on all days (_wei variables)
#       - for week days (_WD)
#       - for weekend days (_WE)
#       - for both week and weekend days (_WD and _WE)
#     2. Scaling (mean = 0 and sd = 1)

# -----------------------------
# Packages
#library(multcomp)
library(tidyverse)
library(haven)
library(testthat)
library(e1071)

# -----------------------------
# Data 
# > Sample participants (n = 4008)
#sample_stno <- read_dta("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Data\\04_DAILY_ACT_SUM\\2021-05-03\\data_03052021.dta")
sample_stno <- read_dta("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2021-05-03\\data_03052021.dta")
dim(sample_stno) #4008 684

# > Person level summary
data00 <- read.csv("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2021-07-12\\part5_personsummary_WW_L40M100V400_T5A5.csv") 
dim(data00) # 4098 658

# > Select participants with valid data
data0 <- data00 %>%
  rename("stno" = "ID") %>%
  filter(stno %in% unique(sample_stno$stno)) 
dim(data0) # 4008 658

# -----------------------------
# > Data check
# Number of blocks = total number of fragments
expect_equal(data0$FRAG_Nfrag_IN_day_wei, data0$Nblocks_day_total_IN_wei)
expect_equal(data0$FRAG_Nfrag_LIPA_day_wei, data0$Nblocks_day_total_LIG_wei)
expect_equal(data0$FRAG_Nfrag_MVPA_day_wei, data0$Nblocks_day_total_MOD_wei + data0$Nblocks_day_total_VIG_wei) # --> average diff: 1.47
expect_equal(data0$FRAG_Nfrag_MVPA_day_wei, data0$Nblocks_day_MOD_unbt_wei + data0$Nblocks_day_VIG_unbt_wei + data0$Nblocks_day_MVPA_bts_10_wei) # --> average diff: 0.61 

summary(data0$M5TIME_num_wei) # ranges from 4.9 to 17.8, mean: 10.2
hist(data0$M5TIME_num_wei) # shall look "normal"

# > All PA features
data <- data0 %>%
  # Derive total duration in MVPA (total duration MOD + total duration VIG)
  mutate(dur_day_total_MVPA_min_wei = dur_day_total_MOD_min_wei + dur_day_total_VIG_min_wei,
         dur_day_total_MVPA_min_WD  = dur_day_total_MOD_min_WD  + dur_day_total_VIG_min_WD,
         dur_day_total_MVPA_min_WE  = dur_day_total_MOD_min_WE  + dur_day_total_VIG_min_WE) %>%
  # Derive number of unbouted fragments of MVPA (unbouted MOD + VIG)
  mutate(Nblocks_day_MVPA_unbt_wei = Nblocks_day_total_MOD_wei + Nblocks_day_total_VIG_wei,
         Nblocks_day_MVPA_unbt_WD  = Nblocks_day_MOD_unbt_WD   + Nblocks_day_VIG_unbt_WD,
         Nblocks_day_MVPA_unbt_WE  = Nblocks_day_MOD_unbt_WE   + Nblocks_day_VIG_unbt_WE) %>% 
  # Select appropiate variables for all days (_wei, weighted estimates), weekd days (_WD), weekend days (_WE)
  dplyr::select(stno, 
         # > Mean acceleration
         ACC_day_mg_wei,               ACC_day_mg_WD,               ACC_day_mg_WE,
         # > Time in SB, LIPA and MVPA
         dur_day_total_IN_min_wei,     dur_day_total_IN_min_WD,     dur_day_total_IN_min_WE, 
         dur_day_total_LIG_min_wei,    dur_day_total_LIG_min_WD,    dur_day_total_LIG_min_WE, 
         dur_day_total_MVPA_min_wei,   dur_day_total_MVPA_min_WD,   dur_day_total_MVPA_min_WE,
         # > Total number of bouts in SB, LIPA and MVPA
         FRAG_Nfrag_IN_day_wei,        FRAG_Nfrag_IN_day_WD,        FRAG_Nfrag_IN_day_WE,
         FRAG_Nfrag_LIPA_day_wei,      FRAG_Nfrag_LIPA_day_WD,      FRAG_Nfrag_LIPA_day_WE,
         FRAG_Nfrag_MVPA_day_wei,      FRAG_Nfrag_MVPA_day_WD,      FRAG_Nfrag_MVPA_day_WE,
         # > Number of fragments of different lengths in SB, LIPA and MVPA
         Nblocks_day_IN_unbt_wei,      Nblocks_day_IN_unbt_WD,      Nblocks_day_IN_unbt_WE,
         Nblocks_day_IN_bts_10_30_wei, Nblocks_day_IN_bts_10_30_WD, Nblocks_day_IN_bts_10_30_WE,
         Nblocks_day_IN_bts_30_wei,    Nblocks_day_IN_bts_30_WD,    Nblocks_day_IN_bts_30_WE, 
         Nblocks_day_LIG_unbt_wei,     Nblocks_day_LIG_unbt_WD,     Nblocks_day_LIG_unbt_WE,
         Nblocks_day_LIG_bts_10_wei,   Nblocks_day_LIG_bts_10_WD,   Nblocks_day_LIG_bts_10_WE,
         Nblocks_day_MVPA_unbt_wei,    Nblocks_day_MVPA_unbt_WD,    Nblocks_day_MVPA_unbt_WE,
         Nblocks_day_MVPA_bts_10_wei,  Nblocks_day_MVPA_bts_10_WD,  Nblocks_day_MVPA_bts_10_WE,
         # > Mean duration of bouts in SB, LIPA and MVPA
         FRAG_mean_dur_IN_day_wei,     FRAG_mean_dur_IN_day_WD,     FRAG_mean_dur_IN_day_WE, 
         FRAG_mean_dur_LIPA_day_wei,   FRAG_mean_dur_LIPA_day_WD,   FRAG_mean_dur_LIPA_day_WE,
         FRAG_mean_dur_MVPA_day_wei,   FRAG_mean_dur_MVPA_day_WD,   FRAG_mean_dur_MVPA_day_WE,
         # > Timing of the most 5 active hours
         M5TIME_num_wei,               M5TIME_num_WD,               M5TIME_num_WE,
         # > Activity gradient
         ig_gradient_wei,              ig_gradient_WD,              ig_gradient_WE, 
         ig_intercept_wei,             ig_intercept_WD,             ig_intercept_WE)

dim(data) # 4008 61 --> 61 variables: 20 variables*3 + stno

# > Full set of PA features
# PA features (wei, WD and WE variables)
data_wei_full   <- data %>% dplyr::select(stno, ends_with("_wei"))
dim(data_wei_full) # 4008 21
data_WD_full    <- data %>% dplyr::select(stno, ends_with("_WD"))
dim(data_WD_full) # 4008 21
data_WE_full    <- data %>% dplyr::select(stno, ends_with("_WE"))
dim(data_WE_full) # 4008 21
data_WD_WE_full <- data %>% dplyr::select(-ends_with("_wei"))
dim(data_WD_WE_full) # 4008 41

# -----------------------------
# > Check data skewness
# Spot skewed variables, 
# i.e. skewness < 1 (Ana Gonzalez-Blanks, Jessie M. Bridgewater & Tuppett M. Yates (2020) Statistical Approaches for Highly Skewed Data: Evaluating Relations between Maltreatment and Young Adults' Non-Suicidal Self-injury, Journal of Clinical Child & Adolescent Psychology, 49:2, 147-161, DOI: 10.1080/15374416.2020.1724543)
skewed_wei  <- data.frame(skewness = apply(X = data_wei_full[,-1], MARGIN = 2, FUN = skewness)) %>% filter(abs(skewness) > 1)
skewed_WDWE <- data.frame(skewness = apply(X = data_WD_WE_full[,-1],  MARGIN = 2, FUN = skewness)) %>% filter(abs(skewness) > 1)

rbind(skewed_wei, skewed_WDWE) %>% arrange(desc(skewness))

# var                         skewness
#FRAG_mean_dur_IN_day_WE     6.925129
#FRAG_mean_dur_IN_day_wei    5.183152
#FRAG_mean_dur_IN_day_WD     5.124835
#FRAG_mean_dur_MVPA_day_WD   3.012427
#FRAG_mean_dur_MVPA_day_wei  2.738296
#FRAG_mean_dur_MVPA_day_WE   2.679977
#ACC_day_mg_WE               2.660863
#Nblocks_day_MVPA_bts_10_WE  2.553931
#Nblocks_day_MVPA_bts_10_WD  1.972974
#Nblocks_day_MVPA_bts_10_wei 1.964927
#Nblocks_day_LIG_bts_10_WE   1.632691
#dur_day_total_MVPA_min_WE   1.619348
#Nblocks_day_LIG_bts_10_wei  1.544249
#Nblocks_day_LIG_bts_10_WD   1.486489
#ACC_day_mg_wei              1.315057
#dur_day_total_MVPA_min_wei  1.139079
#dur_day_total_MVPA_min_WD   1.125570
#ACC_day_mg_WD               1.113560
#Nblocks_day_MVPA_unbt_WE    1.040414
#FRAG_Nfrag_MVPA_day_WE      1.013954

# Skewed variables with 0
data_wei_full %>% 
  left_join(data_WD_WE_full, by = "stno") %>% 
  gather(key = "var", value = "value", -stno) %>% 
  filter(var %in% c(rownames(skewed_WDWE), rownames(skewed_wei)),
         value == 0) %>% 
  group_by(var) %>% 
  summarise(n = n())

# var                             n
# dur_day_total_MVPA_min_WD       7
# dur_day_total_MVPA_min_WE      26
# dur_day_total_MVPA_min_wei      4

# FRAG_mean_dur_MVPA_day_WD       7
# FRAG_mean_dur_MVPA_day_WE      26
# FRAG_mean_dur_MVPA_day_wei      4

# FRAG_Nfrag_MVPA_day_WE         26

# Nblocks_day_LIG_bts_10_WD     138
# Nblocks_day_LIG_bts_10_WE     508
# Nblocks_day_LIG_bts_10_wei     70
# Nblocks_day_MVPA_bts_10_WD   1147
# Nblocks_day_MVPA_bts_10_WE   2059
# Nblocks_day_MVPA_bts_10_wei   952
# Nblocks_day_MVPA_unbt_WE       26

# -----------------------------
# Test different transformation
data_transform <- data_wei_full %>% 
  left_join(data_WD_WE_full, by = "stno") %>% 
  dplyr::select(ACC_day_mg_wei , dur_day_total_MVPA_min_wei , Nblocks_day_LIG_bts_10_wei , Nblocks_day_MVPA_bts_10_wei , FRAG_mean_dur_IN_day_wei , FRAG_mean_dur_MVPA_day_wei , ACC_day_mg_WD , ACC_day_mg_WE , dur_day_total_MVPA_min_WD , dur_day_total_MVPA_min_WE , FRAG_Nfrag_MVPA_day_WE , Nblocks_day_LIG_bts_10_WD , Nblocks_day_LIG_bts_10_WE , Nblocks_day_MVPA_unbt_WE , Nblocks_day_MVPA_bts_10_WD , Nblocks_day_MVPA_bts_10_WE , FRAG_mean_dur_IN_day_WD , FRAG_mean_dur_IN_day_WE , FRAG_mean_dur_MVPA_day_WD , FRAG_mean_dur_MVPA_day_WE) %>% 
  gather(key = "var", value = "value") %>% 
  group_by(var) %>%
  # Replace 0 by 0.001
  mutate(value_no_0 = if_else(value == 0, 0.001, value)) %>% 
  # Compute different transformations
  mutate(log0 = log(value_no_0),
         log1 = log(value+1),
         sqrt = sqrt(value)) 

# Compute variables skewness
data_transform %>% 
  group_by(var) %>% 
  summarise(s_0 = skewness(value),
            #s_1 = skewness(value_no_0),
            s_log0 = skewness(log0),
            s_log1 = skewness(log1),
            s_sqrt = skewness(sqrt)
            ) %>% 
  gather(key = "transformation", value = "skewness", -var) %>%
  mutate(transformation = factor(transformation, levels = rev(c("s_0", "s_log0", "s_log1", "s_sqrt")))) %>%
  ggplot(.) + 
  geom_col(aes(x = reorder(var, abs(skewness), max), y = abs(skewness), fill = transformation), position = position_dodge2()) +
  geom_hline(yintercept = 1, lty = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Skewness (absolute)") +
  scale_fill_manual(values = rev(c("darkred", "grey50", "grey75", "black")))
ggsave("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\02_PCA_and_K-MEANS\\plots\\skewed\\skewness_new.png", width = 8, height = 5)
# --> log(x+1) provides better results for all variables, with few exceptions

# Plot histograms
for(v in unique(data_transform$var))
{
  
  temp <- data_transform %>% filter(var == v) 
  temp %>% 
    dplyr::select(-value_no_0) %>%
    gather(key = "transformation", value = "value", - var) %>% 
    mutate(transformation = factor(transformation, levels = c("value", "log0", "log1", "sqrt"))) %>%
    ggplot(data = .) + 
    geom_histogram(aes(x = value, fill = transformation)) + 
    facet_wrap(~transformation, nrow = 2, scale = "free") +
    ggtitle(paste0(v)) +
    scale_fill_manual(values = c("darkred", "grey50", "grey75", "black"))
  
  ggsave(paste0("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\skewed\\", v, "_july.png"))
  
}

# --> log(x+1) or sqrt(x)
# We will check the results for both variables to see if it changes something

# -----------------------------
# > Transform the data 
data_wei_full <- data_wei_full %>% 
  # Option 1: log(x+1)
  mutate(log_ACC_day_mg_wei              = log(ACC_day_mg_wei+1), 
         log_dur_day_total_MVPA_min_wei  = log(dur_day_total_MVPA_min_wei+1), 
         log_Nblocks_day_LIG_bts_10_wei  = log(Nblocks_day_LIG_bts_10_wei+1), 
         log_Nblocks_day_MVPA_bts_10_wei = log(Nblocks_day_MVPA_bts_10_wei+1), 
         log_FRAG_mean_dur_IN_day_wei    = log(FRAG_mean_dur_IN_day_wei+1), 
         log_FRAG_mean_dur_MVPA_day_wei  = log(FRAG_mean_dur_MVPA_day_wei+1)) %>%
  # Option 2: sqrt(x)
  mutate(sqrt_ACC_day_mg_wei              = sqrt(ACC_day_mg_wei), 
         sqrt_dur_day_total_MVPA_min_wei  = sqrt(dur_day_total_MVPA_min_wei), 
         sqrt_Nblocks_day_LIG_bts_10_wei  = sqrt(Nblocks_day_LIG_bts_10_wei), 
         sqrt_Nblocks_day_MVPA_bts_10_wei = sqrt(Nblocks_day_MVPA_bts_10_wei), 
         sqrt_FRAG_mean_dur_IN_day_wei    = sqrt(FRAG_mean_dur_IN_day_wei), 
         sqrt_FRAG_mean_dur_MVPA_day_wei  = sqrt(FRAG_mean_dur_MVPA_day_wei))

data_WD_full <- data_WD_full %>% 
  # Option 1: log(x+1)
  mutate(log_ACC_day_mg_WD              = log(ACC_day_mg_WD+1), 
         log_dur_day_total_MVPA_min_WD  = log(dur_day_total_MVPA_min_WD+1), 
         log_Nblocks_day_LIG_bts_10_WD  = log(Nblocks_day_LIG_bts_10_WD+1), 
         log_Nblocks_day_MVPA_bts_10_WD = log(Nblocks_day_MVPA_bts_10_WD+1), 
         log_FRAG_mean_dur_IN_day_WD    = log(FRAG_mean_dur_IN_day_WD+1), 
         log_FRAG_mean_dur_MVPA_day_WD  = log(FRAG_mean_dur_MVPA_day_WD+1)) %>%
  # Option 2: sqrt(x)
  mutate(sqrt_ACC_day_mg_WD              = sqrt(ACC_day_mg_WD), 
         sqrt_dur_day_total_MVPA_min_WD  = sqrt(dur_day_total_MVPA_min_WD), 
         sqrt_Nblocks_day_LIG_bts_10_WD  = sqrt(Nblocks_day_LIG_bts_10_WD), 
         sqrt_Nblocks_day_MVPA_bts_10_WD = sqrt(Nblocks_day_MVPA_bts_10_WD), 
         sqrt_FRAG_mean_dur_IN_day_WD    = sqrt(FRAG_mean_dur_IN_day_WD), 
         sqrt_FRAG_mean_dur_MVPA_day_WD  = sqrt(FRAG_mean_dur_MVPA_day_WD))

data_WE_full <- data_WE_full %>% 
  # Option 1: log(x+1)
  mutate(log_ACC_day_mg_WE              = log(ACC_day_mg_WE+1), 
         log_dur_day_total_MVPA_min_WE  = log(dur_day_total_MVPA_min_WE+1), 
         log_Nblocks_day_LIG_bts_10_WE  = log(Nblocks_day_LIG_bts_10_WE+1), 
         log_Nblocks_day_MVPA_bts_10_WE = log(Nblocks_day_MVPA_bts_10_WE+1), 
         log_FRAG_mean_dur_IN_day_WE    = log(FRAG_mean_dur_IN_day_WE+1), 
         log_FRAG_mean_dur_MVPA_day_WE  = log(FRAG_mean_dur_MVPA_day_WE+1)) %>%
  # Option 2: sqrt(x)
  mutate(sqrt_ACC_day_mg_WE              = sqrt(ACC_day_mg_WE), 
         sqrt_dur_day_total_MVPA_min_WE  = sqrt(dur_day_total_MVPA_min_WE), 
         sqrt_Nblocks_day_LIG_bts_10_WE  = sqrt(Nblocks_day_LIG_bts_10_WE), 
         sqrt_Nblocks_day_MVPA_bts_10_WE = sqrt(Nblocks_day_MVPA_bts_10_WE), 
         sqrt_FRAG_mean_dur_IN_day_WE    = sqrt(FRAG_mean_dur_IN_day_WE), 
         sqrt_FRAG_mean_dur_MVPA_day_WE  = sqrt(FRAG_mean_dur_MVPA_day_WE))

data_WD_WE_full <- data_WD_WE_full %>% 
  # Option 1: log(x+1)
  mutate(log_ACC_day_mg_WD              = log(ACC_day_mg_WD+1), 
         log_dur_day_total_MVPA_min_WD  = log(dur_day_total_MVPA_min_WD+1), 
         log_Nblocks_day_LIG_bts_10_WD  = log(Nblocks_day_LIG_bts_10_WD+1), 
         log_Nblocks_day_MVPA_bts_10_WD = log(Nblocks_day_MVPA_bts_10_WD+1), 
         log_FRAG_mean_dur_IN_day_WD    = log(FRAG_mean_dur_IN_day_WD+1), 
         log_FRAG_mean_dur_MVPA_day_WD  = log(FRAG_mean_dur_MVPA_day_WD+1),
         log_ACC_day_mg_WE              = log(ACC_day_mg_WE+1), 
         log_dur_day_total_MVPA_min_WE  = log(dur_day_total_MVPA_min_WE+1), 
         log_Nblocks_day_LIG_bts_10_WE  = log(Nblocks_day_LIG_bts_10_WE+1), 
         log_Nblocks_day_MVPA_bts_10_WE = log(Nblocks_day_MVPA_bts_10_WE+1), 
         log_FRAG_mean_dur_IN_day_WE    = log(FRAG_mean_dur_IN_day_WE+1), 
         log_FRAG_mean_dur_MVPA_day_WE  = log(FRAG_mean_dur_MVPA_day_WE+1)) %>%
  # Option 2: sqrt(x)
  mutate(sqrt_ACC_day_mg_WE              = sqrt(ACC_day_mg_WE), 
         sqrt_dur_day_total_MVPA_min_WE  = sqrt(dur_day_total_MVPA_min_WE), 
         sqrt_Nblocks_day_LIG_bts_10_WE  = sqrt(Nblocks_day_LIG_bts_10_WE), 
         sqrt_Nblocks_day_MVPA_bts_10_WE = sqrt(Nblocks_day_MVPA_bts_10_WE), 
         sqrt_FRAG_mean_dur_IN_day_WE    = sqrt(FRAG_mean_dur_IN_day_WE), 
         sqrt_FRAG_mean_dur_MVPA_day_WE  = sqrt(FRAG_mean_dur_MVPA_day_WE), 
         sqrt_ACC_day_mg_WD              = sqrt(ACC_day_mg_WD), 
         sqrt_dur_day_total_MVPA_min_WD  = sqrt(dur_day_total_MVPA_min_WD), 
         sqrt_Nblocks_day_LIG_bts_10_WD  = sqrt(Nblocks_day_LIG_bts_10_WD), 
         sqrt_Nblocks_day_MVPA_bts_10_WD = sqrt(Nblocks_day_MVPA_bts_10_WD), 
         sqrt_FRAG_mean_dur_IN_day_WD    = sqrt(FRAG_mean_dur_IN_day_WD), 
         sqrt_FRAG_mean_dur_MVPA_day_WD  = sqrt(FRAG_mean_dur_MVPA_day_WD))

# > Check whether skewness in the transformed variables
data.frame(skewness = apply(X = data_wei_full %>% dplyr::select(starts_with("log"), starts_with("sqrt")), MARGIN = 2, FUN = skewness)) %>% filter(abs(skewness) > 1)
# log(x+1): does not improve for FRAG_mean_dur_IN_day_wei and dur_day_total_MVPA_min_wei (abs(skewness) > 1)
# sqrt(x):  does not improve for FRAG_mean_dur_IN_day_wei (abs(skewness) > 1)
data.frame(skewness = apply(X = data_WD_full %>% dplyr::select(starts_with("log"), starts_with("sqrt")), MARGIN = 2, FUN = skewness)) %>% filter(abs(skewness) > 1)
# log(x+1): does not improve for FRAG_mean_dur_IN_day_WD and dur_day_total_MVPA_min_WD (abs(skewness) > 1)
# sqrt(x):  does not improve for FRAG_mean_dur_IN_day_WD (abs(skewness) > 1)
data.frame(skewness = apply(X = data_WE_full %>% dplyr::select(starts_with("log"), starts_with("sqrt")), MARGIN = 2, FUN = skewness)) %>% filter(abs(skewness) > 1)
# log(x+1): does not improve for FRAG_mean_dur_IN_day_WE, dur_day_total_MVPA_min_WE and log_M5TIME_num_WE (abs(skewness) > 1)
# sqrt(x):  does not improve for FRAG_mean_dur_IN_day_WE (abs(skewness) > 1)

# Histograms of both data and log transformed data
data_wei_full %>% 
  dplyr::select(ACC_day_mg_wei, dur_day_total_MVPA_min_wei, Nblocks_day_LIG_bts_10_wei, Nblocks_day_MVPA_bts_10_wei, FRAG_mean_dur_IN_day_wei, FRAG_mean_dur_MVPA_day_wei, starts_with("log_"), starts_with("sqrt_")) %>% 
  gather(key = "var", value = "value") %>% 
  mutate(vartype = if_else(substr(var, 1, 3) == "log", "log (data + 1)", "data"),
         vartype = if_else(substr(var, 1, 4) == "sqrt", "sqrt (data)", vartype)) %>% 
  mutate(var = if_else(vartype == "log (data + 1)", substr(var, 5, nchar(var)), var),
         var = if_else(vartype == "sqrt (data)", substr(var, 6, nchar(var)), var)) %>% 
  ggplot(., aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(vartype ~ var, scale = "free", nrow = 3) + 
  theme(strip.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6))
#ggsave("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//plots//p.skewness_july.png",width = 12,height = 4)

# -----------------------------
# > Standardized PA features (and rename columns with "z_" to know that they are scaled
z_data_wei_full <- rename_with(data_wei_full[,-1], 
                               .fn = ~paste0("z_", .), 
                               .cols = names(data_wei_full[,-1])) %>% scale(.) %>% as.data.frame(.)

z_data_WD_full <- rename_with(data_WD_full[,-1], 
                              .fn = ~paste0("z_", .), 
                              .cols = names(data_WD_full[,-1])) %>% scale(.) %>% as.data.frame(.)

z_data_WE_full <- rename_with(data_WE_full[,-1], 
                              .fn = ~paste0("z_", .), 
                              .cols = names(data_WE_full[,-1])) %>% scale(.) %>% as.data.frame(.)

z_data_WD_WE_full <- rename_with(data_WD_WE_full[,-1], 
                                 .fn = ~paste0("z_", .), 
                                 .cols = names(data_WD_WE_full[,-1])) %>% scale(.) %>% as.data.frame(.)

# > Check standardization (mean = 0, sd = 1)
z_data_wei_full %>% 
  gather(key = "variable", value = "value") %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) 

z_data_WD_WE_full %>% 
  as.data.frame(.) %>% 
  gather(key = "variable", value = "value") %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value),
            sd = sd(value))
# --> OK

# -----------------------------
# > Store data in lists

# A. Full set of features (non-standardized)
data_full <- list(
  # Non-transformed data
  wei        = data_wei_full   %>% dplyr::select(-starts_with("log_"), -starts_with("sqrt")),
  WD         = data_WD_full    %>% dplyr::select(-starts_with("log_"), -starts_with("sqrt")),
  WE         = data_WE_full    %>% dplyr::select(-starts_with("log_"), -starts_with("sqrt")),
  WD_WE      = data_WD_WE_full %>% dplyr::select(-starts_with("log_"), -starts_with("sqrt")),
  # Log-transformed data
  wei_log    = data_wei_full   %>% dplyr::select(-ACC_day_mg_wei, -dur_day_total_MVPA_min_wei, -FRAG_mean_dur_IN_day_wei, -FRAG_mean_dur_MVPA_day_wei, -Nblocks_day_LIG_bts_10_wei, -Nblocks_day_MVPA_bts_10_wei, -starts_with("sqrt")),
  WD_log     = data_WD_full    %>% dplyr::select(-ACC_day_mg_WD,  -dur_day_total_MVPA_min_WD,  -FRAG_mean_dur_IN_day_WD,  -FRAG_mean_dur_MVPA_day_WD,  -Nblocks_day_LIG_bts_10_WD,  -Nblocks_day_MVPA_bts_10_WD,  -starts_with("sqrt")),
  WE_log     = data_WE_full    %>% dplyr::select(-ACC_day_mg_WE,  -dur_day_total_MVPA_min_WE,  -FRAG_mean_dur_IN_day_WE,  -FRAG_mean_dur_MVPA_day_WE,  -Nblocks_day_LIG_bts_10_WE,  -Nblocks_day_MVPA_bts_10_WE,  -starts_with("sqrt")),
  WD_WE_log  = data_WD_WE_full %>% dplyr::select(-ACC_day_mg_WD,  -dur_day_total_MVPA_min_WD,  -FRAG_mean_dur_IN_day_WD,  -FRAG_mean_dur_MVPA_day_WD,  -Nblocks_day_LIG_bts_10_WD,  -Nblocks_day_MVPA_bts_10_WD,
                                                 -ACC_day_mg_WE,  -dur_day_total_MVPA_min_WE,  -FRAG_mean_dur_IN_day_WE,  -FRAG_mean_dur_MVPA_day_WE,  -Nblocks_day_LIG_bts_10_WE,  -Nblocks_day_MVPA_bts_10_WE,  -starts_with("sqrt")),
  # Sqrt-transformed data
  wei_sqrt   = data_wei_full   %>% dplyr::select(-ACC_day_mg_wei, -dur_day_total_MVPA_min_wei, -FRAG_mean_dur_IN_day_wei, -FRAG_mean_dur_MVPA_day_wei, -Nblocks_day_LIG_bts_10_wei, -Nblocks_day_MVPA_bts_10_wei, -starts_with("log")),
  WD_sqrt    = data_WD_full    %>% dplyr::select(-ACC_day_mg_WD,  -dur_day_total_MVPA_min_WD,  -FRAG_mean_dur_IN_day_WD,  -FRAG_mean_dur_MVPA_day_WD,  -Nblocks_day_LIG_bts_10_WD,  -Nblocks_day_MVPA_bts_10_WD,  -starts_with("log")),
  WE_sqrt    = data_WE_full    %>% dplyr::select(-ACC_day_mg_WE,  -dur_day_total_MVPA_min_WE,  -FRAG_mean_dur_IN_day_WE,  -FRAG_mean_dur_MVPA_day_WE,  -Nblocks_day_LIG_bts_10_WE,  -Nblocks_day_MVPA_bts_10_WE,  -starts_with("log")),
  WD_WE_sqrt = data_WD_WE_full %>% dplyr::select(-ACC_day_mg_WD,  -dur_day_total_MVPA_min_WD,  -FRAG_mean_dur_IN_day_WD,  -FRAG_mean_dur_MVPA_day_WD,  -Nblocks_day_LIG_bts_10_WD,  -Nblocks_day_MVPA_bts_10_WD,
                                                 -ACC_day_mg_WE,  -dur_day_total_MVPA_min_WE,  -FRAG_mean_dur_IN_day_WE,  -FRAG_mean_dur_MVPA_day_WE,  -Nblocks_day_LIG_bts_10_WE,  -Nblocks_day_MVPA_bts_10_WE,  -starts_with("log"))
  
)

# B. Full set of features (standardized)
z_data_full <- list(
  # Non-transformed data
  wei        = z_data_wei_full   %>% dplyr::select(-starts_with("z_log_"), -starts_with("z_sqrt")),
  WD         = z_data_WD_full    %>% dplyr::select(-starts_with("z_log_"), -starts_with("z_sqrt")),
  WE         = z_data_WE_full    %>% dplyr::select(-starts_with("z_log_"), -starts_with("z_sqrt")),
  WD_WE      = z_data_WD_WE_full %>% dplyr::select(-starts_with("z_log_"), -starts_with("z_sqrt")),
  # Log-transformed data
  wei_log    = z_data_wei_full   %>% dplyr::select(-z_ACC_day_mg_wei, -z_dur_day_total_MVPA_min_wei, -z_FRAG_mean_dur_IN_day_wei, -z_FRAG_mean_dur_MVPA_day_wei, -z_Nblocks_day_LIG_bts_10_wei, -z_Nblocks_day_MVPA_bts_10_wei, -starts_with("z_sqrt")),
  WD_log     = z_data_WD_full    %>% dplyr::select(-z_ACC_day_mg_WD,  -z_dur_day_total_MVPA_min_WD,  -z_FRAG_mean_dur_IN_day_WD,  -z_FRAG_mean_dur_MVPA_day_WD,  -z_Nblocks_day_LIG_bts_10_WD,  -z_Nblocks_day_MVPA_bts_10_WD,  -starts_with("z_sqrt")),
  WE_log     = z_data_WE_full    %>% dplyr::select(-z_ACC_day_mg_WE,  -z_dur_day_total_MVPA_min_WE,  -z_FRAG_mean_dur_IN_day_WE,  -z_FRAG_mean_dur_MVPA_day_WE,  -z_Nblocks_day_LIG_bts_10_WE,  -z_Nblocks_day_MVPA_bts_10_WE,  -starts_with("z_sqrt")),
  WD_WE_log  = z_data_WD_WE_full %>% dplyr::select(-z_ACC_day_mg_WD,  -z_dur_day_total_MVPA_min_WD,  -z_FRAG_mean_dur_IN_day_WD,  -z_FRAG_mean_dur_MVPA_day_WD,  -z_Nblocks_day_LIG_bts_10_WD,  -z_Nblocks_day_MVPA_bts_10_WD,
                                                   -z_ACC_day_mg_WE,  -z_dur_day_total_MVPA_min_WE,  -z_FRAG_mean_dur_IN_day_WE,  -z_FRAG_mean_dur_MVPA_day_WE,  -z_Nblocks_day_LIG_bts_10_WE,  -z_Nblocks_day_MVPA_bts_10_WE,  -starts_with("z_sqrt")),
  # Sqrt-transformed data
  wei_sqrt   = z_data_wei_full   %>% dplyr::select(-z_ACC_day_mg_wei, -z_dur_day_total_MVPA_min_wei, -z_FRAG_mean_dur_IN_day_wei, -z_FRAG_mean_dur_MVPA_day_wei, -z_Nblocks_day_LIG_bts_10_wei, -z_Nblocks_day_MVPA_bts_10_wei, -starts_with("z_log")),
  WD_sqrt    = z_data_WD_full    %>% dplyr::select(-z_ACC_day_mg_WD,  -z_dur_day_total_MVPA_min_WD,  -z_FRAG_mean_dur_IN_day_WD,  -z_FRAG_mean_dur_MVPA_day_WD,  -z_Nblocks_day_LIG_bts_10_WD,  -z_Nblocks_day_MVPA_bts_10_WD,  -starts_with("z_log")),
  WE_sqrt    = z_data_WE_full    %>% dplyr::select(-z_ACC_day_mg_WE,  -z_dur_day_total_MVPA_min_WE,  -z_FRAG_mean_dur_IN_day_WE,  -z_FRAG_mean_dur_MVPA_day_WE,  -z_Nblocks_day_LIG_bts_10_WE,  -z_Nblocks_day_MVPA_bts_10_WE,  -starts_with("z_log")),
  WD_WE_sqrt = z_data_WD_WE_full %>% dplyr::select(-z_ACC_day_mg_WD,  -z_dur_day_total_MVPA_min_WD,  -z_FRAG_mean_dur_IN_day_WD,  -z_FRAG_mean_dur_MVPA_day_WD,  -z_Nblocks_day_LIG_bts_10_WD,  -z_Nblocks_day_MVPA_bts_10_WD,
                                                   -z_ACC_day_mg_WE,  -z_dur_day_total_MVPA_min_WE,  -z_FRAG_mean_dur_IN_day_WE,  -z_FRAG_mean_dur_MVPA_day_WE,  -z_Nblocks_day_LIG_bts_10_WE,  -z_Nblocks_day_MVPA_bts_10_WE,  -starts_with("z_log"))
  
)

map(.f = dim, .x = data_full)
map(.f = dim, .x = z_data_full)

# C. Selected set of features (non-standardized)
#    PA features without number of bouts of different lengths
data <- list(
  # Non-transformed data
  wei        = data_wei_full   %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-starts_with("log_"), -starts_with("sqrt")),
  WD         = data_WD_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-starts_with("log_"), -starts_with("sqrt")),
  WE         = data_WE_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-starts_with("log_"), -starts_with("sqrt")),
  WD_WE      = data_WD_WE_full %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-starts_with("log_"), -starts_with("sqrt")),
  # Log-transformed data
  wei_log    = data_wei_full   %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-ACC_day_mg_wei, -dur_day_total_MVPA_min_wei, -FRAG_mean_dur_IN_day_wei, -FRAG_mean_dur_MVPA_day_wei, -starts_with("sqrt")),
  WD_log     = data_WD_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-ACC_day_mg_WD,  -dur_day_total_MVPA_min_WD,  -FRAG_mean_dur_IN_day_WD,  -FRAG_mean_dur_MVPA_day_WD,  -starts_with("sqrt")),
  WE_log     = data_WE_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-ACC_day_mg_WE,  -dur_day_total_MVPA_min_WE,  -FRAG_mean_dur_IN_day_WE,  -FRAG_mean_dur_MVPA_day_WE,  -starts_with("sqrt")),
  WD_WE_log  = data_WD_WE_full %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-ACC_day_mg_WD,  -dur_day_total_MVPA_min_WD,  -FRAG_mean_dur_IN_day_WD,  -FRAG_mean_dur_MVPA_day_WD,  
                                                                                              -ACC_day_mg_WE,  -dur_day_total_MVPA_min_WE,  -FRAG_mean_dur_IN_day_WE,  -FRAG_mean_dur_MVPA_day_WE, -starts_with("sqrt")),
  # Sqrt-transformed data
  wei_sqrt   = data_wei_full   %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-ACC_day_mg_wei, -dur_day_total_MVPA_min_wei, -FRAG_mean_dur_IN_day_wei, -FRAG_mean_dur_MVPA_day_wei, -starts_with("log")),
  WD_sqrt    = data_WD_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-ACC_day_mg_WD,  -dur_day_total_MVPA_min_WD,  -FRAG_mean_dur_IN_day_WD,  -FRAG_mean_dur_MVPA_day_WD,  -starts_with("log")),
  WE_sqrt    = data_WE_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-ACC_day_mg_WE,  -dur_day_total_MVPA_min_WE,  -FRAG_mean_dur_IN_day_WE,  -FRAG_mean_dur_MVPA_day_WE,  -starts_with("log")),
  WD_WE_sqrt = data_WD_WE_full %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-ACC_day_mg_WD,  -dur_day_total_MVPA_min_WD,  -FRAG_mean_dur_IN_day_WD,  -FRAG_mean_dur_MVPA_day_WD,  
                                                                                              -ACC_day_mg_WE,  -dur_day_total_MVPA_min_WE,  -FRAG_mean_dur_IN_day_WE,  -FRAG_mean_dur_MVPA_day_WE,  -starts_with("log"))
  
)

# D. Selected set of features (standardized)
#    PA features without number of bouts of different lengths
z_data <- list(
  # Non-transformed data
  wei        = z_data_wei_full   %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-starts_with("z_log_"), -starts_with("z_sqrt")),
  WD         = z_data_WD_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-starts_with("z_log_"), -starts_with("z_sqrt")),
  WE         = z_data_WE_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-starts_with("z_log_"), -starts_with("z_sqrt")),
  WD_WE      = z_data_WD_WE_full %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-starts_with("z_log_"), -starts_with("z_sqrt")),
  # Log-transformed data
  wei_log    = z_data_wei_full   %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-z_ACC_day_mg_wei, -z_dur_day_total_MVPA_min_wei, -z_FRAG_mean_dur_IN_day_wei, -z_FRAG_mean_dur_MVPA_day_wei, -starts_with("z_sqrt")),
  WD_log     = z_data_WD_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-z_ACC_day_mg_WD,  -z_dur_day_total_MVPA_min_WD,  -z_FRAG_mean_dur_IN_day_WD,  -z_FRAG_mean_dur_MVPA_day_WD,  -starts_with("z_sqrt")),
  WE_log     = z_data_WE_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-z_ACC_day_mg_WE,  -z_dur_day_total_MVPA_min_WE,  -z_FRAG_mean_dur_IN_day_WE,  -z_FRAG_mean_dur_MVPA_day_WE,  -starts_with("z_sqrt")),
  WD_WE_log  = z_data_WD_WE_full %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-z_ACC_day_mg_WD,  -z_dur_day_total_MVPA_min_WD,  -z_FRAG_mean_dur_IN_day_WD,  -z_FRAG_mean_dur_MVPA_day_WD,  
                                                                                                -z_ACC_day_mg_WE,  -z_dur_day_total_MVPA_min_WE,  -z_FRAG_mean_dur_IN_day_WE,  -z_FRAG_mean_dur_MVPA_day_WE,  -starts_with("z_sqrt")),
  # Sqrt-transformed data
  wei_sqrt   = z_data_wei_full   %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-z_ACC_day_mg_wei, -z_dur_day_total_MVPA_min_wei, -z_FRAG_mean_dur_IN_day_wei, -z_FRAG_mean_dur_MVPA_day_wei, -starts_with("z_log")),
  WD_sqrt    = z_data_WD_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-z_ACC_day_mg_WD,  -z_dur_day_total_MVPA_min_WD,  -z_FRAG_mean_dur_IN_day_WD,  -z_FRAG_mean_dur_MVPA_day_WD,  -starts_with("z_log")),
  WE_sqrt    = z_data_WE_full    %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-z_ACC_day_mg_WE,  -z_dur_day_total_MVPA_min_WE,  -z_FRAG_mean_dur_IN_day_WE,  -z_FRAG_mean_dur_MVPA_day_WE,  -starts_with("z_log")),
  WD_WE_sqrt = z_data_WD_WE_full %>% dplyr::select(-contains("Nblocks_day_")) %>% dplyr::select(-z_ACC_day_mg_WD,  -z_dur_day_total_MVPA_min_WD,  -z_FRAG_mean_dur_IN_day_WD,  -z_FRAG_mean_dur_MVPA_day_WD,  
                                                                                                -z_ACC_day_mg_WE,  -z_dur_day_total_MVPA_min_WE,  -z_FRAG_mean_dur_IN_day_WE,  -z_FRAG_mean_dur_MVPA_day_WE,  -starts_with("z_log"))
  
)

map(.f = dim, .x = data_full) # 20+1 for wei, wd, we, 40+1 for wd_we
map(.f = dim, .x = z_data_full) # 20 for wei, wd, we, 40 for wd_we
map(.f = dim, .x = data) # 13+1 for wei, wd, we, 26+1 for wd_we
map(.f = dim, .x = z_data) # 13 for wei, wd, we, 26 for wd_we

# > Check variables in datasets
lapply(data, names)        # --> OK
lapply(z_data, names)      # --> OK
lapply(data_full, names)   # --> OK
lapply(z_data_full, names) # --> OK

# -----------------------------
# > Save 
save(data, file = "E:/PC_FIXE/Analysis/02_ARTICLE_2/00_DATA/00_data_PCA_k_means_july.rda")
save(z_data, file = "E:/PC_FIXE/Analysis/02_ARTICLE_2/00_DATA/00_z_data_PCA_k_means_july.rda")

save(data_full, file = "E:/PC_FIXE/Analysis/02_ARTICLE_2/00_DATA/00_data_full_PCA_k_means_july.rda")
save(z_data_full, file = "E:/PC_FIXE/Analysis/02_ARTICLE_2/00_DATA/00_z_data_full_PCA_k_means_july.rda")

