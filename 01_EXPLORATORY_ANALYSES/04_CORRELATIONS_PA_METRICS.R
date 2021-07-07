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

# > All PA metrics (with number of bouts of different lengths)
#   Non-standardized metrics
load("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\00_DATA\\00_data_full_PCA_k_means.rda")

# Table with PA metrics' name
source("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\00_DATA\\00_tab-name_PA_features.R")

# > Join data and tab.name to have a clean version of variables
data_named <- data_full %>%
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

# -----------------------------
# Correlations matrix

sensitivity_analysis <- c("Number of < 10 min IN fragments (unbouted)", "Number of 10-30 min IN fragments", "Number of > 30 min IN fragments", 
                          "Number of < 10 min LIPA fragments (unbouted)", "Number of > 10 min LIPA fragments", 
                          "Number of < 10 min MVPA fragments (unbouted)","Number of > 10 min MVPA fragments")

corrr_mat <- list("All days - No transformation"                 = data_named$wei, 
                  "All days - Skewed variables log transformed"  = data_named$wei_log, 
                  "All days - Skewed variables sqrt transformed" = data_named$wei_sqrt,
                  "Week days - No transformation"                 = data_named$WD, 
                  "Week days - Skewed variables log transformed"  = data_named$WD_log, 
                  "Week days - Skewed variables sqrt transformed" = data_named$WD_sqrt,
                  "Weekend days - No transformation"                 = data_named$WE, 
                  "Weekend days - Skewed variables log transformed"  = data_named$WE_log, 
                  "Weekend days - Skewed variables sqrt transformed" = data_named$WE_sqrt) %>% 
  
  # > Compute correlation matrix for the selected set of variables (all days)
  map(~ { 
    
    .x %>% 
      # Rename variables to remove the end of the labels
      gather(key = "feature", value = "value", -stno) %>% 
      mutate(feature = sub(" - .*", "", feature)) %>% 
      spread(key = "feature", value = "value") %>% 
      # Remove useless variables
      select(-stno) %>% 
      # Compute correlation coefficient
      correlate(method = "pearson", use = "pairwise.complete.obs") %>%
      shave()
    
    })

# -----------------------------
# Correlations plots (corplots)

# > Generate the plots
#   - one for the selected set of PA features
#   - one for the full set of PA features
corrr_plot <- corrr_mat %>% 
  map(~ { 
    
    # > Selected set of metrics    
    plot.set1 <- .x %>% 
      focus(-starts_with(paste0(sensitivity_analysis)), mirror = TRUE) %>%
      rplot(., 
            legend = F,
            print_cor = T,
            colours = c("blue", "#78B7C5", "white", "#E1AF00", "#F21A00")) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    # > Full set of metrics    
    plot.set2 <- .x %>% 
      rplot(., 
            legend = F,
            print_cor = T,
            colours = c("blue", "#78B7C5", "white", "#E1AF00", "#F21A00")) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    # > list of plots
    list("Selected set" = plot.set1,
         "Full set" = plot.set2)
    
    })

# Save the plots
for(i in names(corrr_plot))
{
  # > Selected set
  ggsave(plot = corrr_plot[[paste0(i)]]$`Selected set`, 
         filename = paste0("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\01_EXPLORATORY_ANALYSES\\plots\\p.cor.1_", i, ".png"),
         height = 7, 
         width = 7)
  # > Full set
  ggsave(plot = corrr_plot[[paste0(i)]]$`Full set`, 
         filename = paste0("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\01_EXPLORATORY_ANALYSES\\plots\\p.cor.2_", i, ".png"),
         height = 8, 
         width = 8)
  
}

