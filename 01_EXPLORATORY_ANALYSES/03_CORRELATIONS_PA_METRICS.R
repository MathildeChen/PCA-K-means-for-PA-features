# Script name: 04_CORRELATIONS_PA_METRICS.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 
# 1) Compute correlations coefficients between PA metrics 
#    - all days, week days or weekend days, 
#    - different transformations for skewed variables
#    - for the selected set of PA metrics and for the full set of metrics
# 2) Generate plots to visualize correlations between metrics (corplots)

# -----------------------------
# Packages
library(tidyverse)
library(corrr)
library(corrplot)
library(ggplot2)

# -----------------------------
# Data

path_to_data <- "E:/PC_FIXE/Analysis/02_ARTICLE_2/00_DATA"
# > Selected set of metrics (without number of bouts of different lengths)
#   Non-standardized metrics
load(file.path(path_to_data, "00_data_PCA_k_means_july.rda"))
data_wei         <- data$wei 
data_wei_no_M5   <- data$wei %>% dplyr::select(-M5TIME_num_wei)
data_WD_WE       <- data$WD_WE 
data_WD_WE_no_M5 <- data$WD_WE %>% dplyr::select(-M5TIME_num_WE, -M5TIME_num_WD)

# with transformed variables (log(x+1))
data_wei_log    <- data$wei_log
data_WD_WE_log  <- data$WD_WE_log 
# with transformed variables (sqrt(x))
data_wei_sqrt   <- data$wei_sqrt
data_WD_WE_sqrt <- data$WD_WE_sqrt

# > Full set of metrics (with number of bouts of different lengths) - for Sensitivity analyses
#   Non-standardized variables
load(file.path(path_to_data, "00_data_full_PCA_k_means_july.rda"))
data_wei_full        <- data_full$wei
data_WD_WE_full      <- data_full$WD_WE
# with transformed variables (log(x+1))
data_wei_full_log    <- data_full$wei_log
data_WD_WE_full_log  <- data_full$WD_WE_log
# with transformed variables (sqrt(x))
data_wei_full_sqrt   <- data_full$wei_sqrt
data_WD_WE_full_sqrt <- data_full$WD_WE_sqrt

# Table with PA metrics' name
source("00_DATA\\00_tab-name_PA_features.R")

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
# Correlations matrix

sensitivity_analysis <- c("Number of < 10 min IN fragments (unbouted)", "Number of 10-30 min IN fragments", "Number of > 30 min IN fragments", 
                          "Number of < 10 min LIPA fragments (unbouted)", "Number of > 10 min LIPA fragments", 
                          "Number of < 10 min MVPA fragments (unbouted)","Number of > 10 min MVPA fragments")

# > Weighted daily average
corrr_mat_wei <- list("Weighted daily average"                                         = data_wei, 
                      "Weighted daily average (without timing of 5 most active hours)" = data_wei_no_M5, 
                      "Weighted daily average (skewed variables are log(x+1))"         = data_wei_log, 
                      "Weighted daily average (skewed variables are sqrt(x))"          = data_wei_sqrt,
                      "Weighted daily average - Full set of features"                                 = data_wei_full, 
                      "Weighted daily average (skewed variables are log(x+1)) - Full set of features" = data_wei_full_log, 
                      "Weighted daily average (skewed variables are sqrt(x)) - Full set of features"  = data_wei_full_sqrt) %>% 
  
  map(., ~ { 
    .x %>%
      tidyr::gather(key = "var", value = "value", -stno) %>% 
      left_join(tab.name, by = "var") %>% 
      dplyr::select(-var) %>% 
      #mutate(varname = factor(varname, levels = c()))
      tidyr::spread(key = "varname", value = "value") 
  }) %>%
  
  # > Compute correlation matrix for the selected set of variables (all days)
  map(~ { 
    
    .x %>% 
      # Rename variables to remove the end of the labels
      gather(key = "feature", value = "value", -stno) %>% 
      mutate(feature = sub(" - .*", "", feature)) %>% 
      mutate(feature = factor(feature, levels = c('Intensity gradient intercept', 'Duration SB (min/day)', 'Number of SB bouts', 'Number of < 10 min SB fragments (unbouted)', 'Number of 10-30 min SB fragments', 'Number of > 30 min SB fragments','Mean duration of SB bouts', 'Mean duration of SB bouts*', 'Duration LIPA (min/day)', 'Number of LIPA bouts', 'Number of < 10 min LIPA fragments (unbouted)', 'Number of > 10 min LIPA fragments*', 'Number of > 10 min LIPA fragments', 'Mean duration of LIPA bouts', 'Duration MVPA (min/day)', 'Duration MVPA* (min/day)', 'Number of MVPA bouts', 'Number of < 10 min MVPA fragments (unbouted)', 'Number of > 10 min MVPA fragments*', 'Number of > 10 min MVPA fragments',  'Mean duration of MVPA bouts', 'Mean duration of MVPA bouts*', 'Intensity gradient slope','Acceleration (mg)', 'Acceleration* (mg)', 'Most active 5hrs timing'))) %>%
      spread(key = "feature", value = "value") %>% 
      # Remove useless variables
      dplyr::select(-stno) %>% 
      # Compute correlation coefficient
      cor(.)
    
    })

# > Week day and weekend days average
corrr_mat_wd_we <- list("Week and weekend variables"                                         = data_WD_WE, 
                        "Week and weekend variables (without timing of 5 most active hours)" = data_WD_WE_no_M5, 
                        "Week and weekend variables (skewed variables are log(x+1))"         = data_WD_WE_log, 
                        "Week and weekend variables (skewed variables are sqrt(x))"          = data_WD_WE_sqrt, 
                        "Week and weekend variables - Full set of features"                                 = data_WD_WE_full, 
                        "Week and weekend variables (skewed variables are log(x+1)) - Full set of features" = data_WD_WE_full_log, 
                        "Week and weekend variables (skewed variables are sqrt(x)) - Full set of features"  = data_WD_WE_full_sqrt) %>% 
  
  map(., ~ { 
    .x %>%
      tidyr::gather(key = "var", value = "value", -stno) %>% 
      left_join(tab.name, by = "var") %>% 
      dplyr::select(-var) %>% 
      tidyr::spread(key = "varname", value = "value") 
  }) %>%
  
  # > Compute correlation matrix for the selected set of variables (all days)
  map(~ { 
    
    .x %>% 
      # Rename variables to remove the end of the labels
      gather(key = "feature", value = "value", -stno) %>% 
      mutate(feature = factor(feature, levels = c('Intensity gradient intercept - Week days', 'Intensity gradient intercept - Weekend days', 'Duration SB (min/day) - Week days', 'Duration SB (min/day) - Weekend days', 'Mean duration of SB bouts - Week days', 'Mean duration of SB bouts - Weekend days', 'Mean duration of SB bouts* - Week days', 'Mean duration of SB bouts* - Weekend days', 'Number of SB bouts - Week days', 'Number of SB bouts - Weekend days', 'Number of < 10 min SB fragments (unbouted) - Week days', 'Number of < 10 min SB fragments (unbouted) - Weekend days', 'Number of 10-30 min SB fragments - Week days', 'Number of 10-30 min SB fragments - Weekend days', 'Number of > 30 min SB fragments - Week days', 'Number of > 30 min SB fragments - Weekend days', 'Duration LIPA (min/day) - Week days', 'Duration LIPA (min/day) - Weekend days', 'Number of LIPA bouts - Week days', 'Number of LIPA bouts - Weekend days', 'Number of < 10 min LIPA fragments (unbouted) - Week days', 'Number of < 10 min LIPA fragments (unbouted) - Weekend days', 'Number of > 10 min LIPA fragments - Week days', 'Number of > 10 min LIPA fragments - Weekend days', 'Number of > 10 min LIPA fragments* - Week days', 'Number of > 10 min LIPA fragments* - Weekend days', 'Mean duration of LIPA bouts - Week days', 'Mean duration of LIPA bouts - Weekend days', 'Duration MVPA (min/day) - Week days', 'Duration MVPA (min/day) - Weekend days', 'Duration MVPA* (min/day) - Week days', 'Duration MVPA* (min/day) - Weekend days', 'Number of MVPA bouts - Week days', 'Number of MVPA bouts - Weekend days', 'Number of < 10 min MVPA fragments (unbouted) - Week days', 'Number of < 10 min MVPA fragments (unbouted) - Weekend days', 'Number of > 10 min MVPA fragments - Week days', 'Number of > 10 min MVPA fragments - Weekend days', 'Number of > 10 min MVPA fragments* - Week days', 'Number of > 10 min MVPA fragments* - Weekend days', 'Mean duration of MVPA bouts - Week days', 'Mean duration of MVPA bouts - Weekend days', 'Mean duration of MVPA bouts* - Week days', 'Mean duration of MVPA bouts* - Weekend days', 'Intensity gradient slope - Week days', 'Intensity gradient slope - Weekend days', 'Acceleration (mg) - Week days', 'Acceleration (mg) - Weekend days', 'Acceleration* (mg) - Week days', 'Acceleration* (mg) - Weekend days', 'Most active 5hrs timing - Week days', 'Most active 5hrs timing - Weekend days'))) %>% 
      spread(key = "feature", value = "value") %>% 
      # Remove useless variables
      dplyr::select(-stno) %>% 
      # Compute correlation coefficient
      cor(.)
    
  })


# -----------------------------
# Correlation plots

# Weighted daily average
png(filename = "E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\corplots\\p.cor_Weighted daily average_july.png",
    height = 900, 
    width = 900)

corrplot(corrr_mat_wei$`Weighted daily average`, 
         method="color", 
         #col=col(200),  
         diag=FALSE, 
         type="lower", 
         order="original", 
         addCoef.col = "black",
         number.cex = 1.5,
         tl.cex = 1.5,
         tl.col = "black",
         cl.pos = "n",
         title = "Weighted daily average", 
         mar=c(0,0,1,0))

dev.off()

# Weighted daily average (without timing of 5 most active hours)
png(filename = "E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\corplots\\p.cor_Weighted daily average-no_M5_july.png",
    height = 900, 
    width = 900)

corrplot(corrr_mat_wei$`Weighted daily average (without timing of 5 most active hours)`, 
         method="color", 
         #col=col(200),  
         diag=FALSE, 
         type="lower", 
         order="hclust", 
         addCoef.col = "black",
         number.cex = 1.5,
         tl.cex = 1.5,
         tl.col = "black",
         cl.pos = "n",
         title = "Weighted daily average (without timing of 5 most active hours)", 
         mar=c(0,0,1,0))

dev.off()

# Weighted daily average - full set of features
png(filename = "E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\corplots\\p.cor_Weighted daily average - Full set of features_july.png",
    height = 1200, 
    width = 1200)

corrplot(corrr_mat_wei$`Weighted daily average - Full set of features`, 
         method="color", 
         #col=col(200),  
         diag=FALSE, 
         type="lower", 
         order="hclust", 
         addCoef.col = "black",
         number.cex = 1.5,
         tl.cex = 1.5,
         tl.col = "black",
         cl.pos = "n",
         title = "Weighted daily average - Full set of features", 
         mar=c(0,0,1,0))

dev.off()

# Week and weekend variables
png(filename = "E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\corplots\\p.cor_Week and weekend variables_july.png",
      height = 1600, 
      width = 1600)
  
corrplot(corrr_mat_wd_we$`Week and weekend variables`, 
           method="color", 
           #col=col(200),  
           diag=FALSE, 
           type="lower", 
           order="hclust", 
           addCoef.col = "black",
           number.cex = 1.5,
           tl.cex = 1.5,
           tl.col = "black",
           cl.pos = "n",
           title = "Week and weekend variables", 
           mar=c(0,0,1,0))
  
dev.off()


# Week and weekend variables (without timing of 5 most active hours)
png(filename = "E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\corplots\\p.cor_Week and weekend variables_no_M5_july.png",
    height = 1600, 
    width = 1600)

corrplot(corrr_mat_wd_we$`Week and weekend variables (without timing of 5 most active hours)`, 
         method="color", 
         #col=col(200),  
         diag=FALSE, 
         type="lower", 
         order="hclust", 
         addCoef.col = "black",
         number.cex = 1.5,
         tl.cex = 1.5,
         tl.col = "black",
         cl.pos = "n",
         title = "Week and weekend variables (without timing of 5 most active hours)", 
         mar=c(0,0,1,0))

dev.off()

# Week and weekend variables - Full set of features
png(filename = "E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\corplots\\p.cor_Week and weekend variables - Full set of features_july.png",
    height = 2400, 
    width = 2400)

corrplot(corrr_mat_wd_we$`Week and weekend variables - Full set of features`, 
         method="color", 
         #col=col(200),  
         diag=FALSE, 
         type="lower", 
         order="hclust", 
         addCoef.col = "black",
         number.cex = 1.5,
         tl.cex = 1.5,
         tl.col = "black",
         cl.pos = "n",
         title = "Week and weekend variables - Full set of features", 
         mar=c(0,0,1,0))

dev.off()

