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

# > Selected set of metrics (without number of bouts of different lengths)
#   Non-standardized metrics
load("00_DATA\\00_data_PCA_k_means.rda")
data_wei        <- data$wei 
data_WD_WE      <- data$WD_WE 
# with transformed variables (log(x+1))
data_wei_log    <- data$wei_log
data_WD_WE_log  <- data$WD_WE_log 
# with transformed variables (sqrt(x))
data_wei_sqrt   <- data$wei_sqrt
data_WD_WE_sqrt <- data$WD_WE_sqrt

# > Full set of metrics (with number of bouts of different lengths) - for Sensitivity analyses
#   Non-standardized variables
load("00_DATA\\00_data_full_PCA_k_means.rda")
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

corrr_mat <- list("Weighted daily average"                                 = data_wei, 
                  "Weighted daily average (skewed variables are log(x+1))" = data_wei_log  %>% dplyr::select(-km.5), 
                  "Weighted daily average (skewed variables are sqrt(x))"  = data_wei_sqrt  %>% dplyr::select(-km.5),
                  "Week and weekend variables"                                 = data_WD_WE, 
                  "Week and weekend variables (skewed variables are log(x+1))" = data_WD_WE_log  %>% dplyr::select(-km.5), 
                  "Week and weekend variables (skewed variables are sqrt(x))"  = data_WD_WE_sqrt  %>% dplyr::select(-km.5), 
                  "Weighted daily average - Full set of features"                                 = data_wei_full, 
                  "Weighted daily average (skewed variables are log(x+1)) - Full set of features" = data_wei_full_log  %>% dplyr::select(-km.5), 
                  "Weighted daily average (skewed variables are sqrt(x)) - Full set of features"  = data_wei_full_sqrt  %>% dplyr::select(-km.5),
                  "Week and weekend variables - Full set of features"                                 = data_WD_WE_full, 
                  "Week and weekend variables (skewed variables are log(x+1)) - Full set of features" = data_WD_WE_full_log  %>% dplyr::select(-km.5), 
                  "Week and weekend variables (skewed variables are sqrt(x)) - Full set of features"  = data_WD_WE_full_sqrt  %>% dplyr::select(-km.5)) %>% 
  
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
      mutate(feature = sub(" - .*", "", feature)) %>% 
      spread(key = "feature", value = "value") %>% 
      # Remove useless variables
      dplyr::select(-stno) %>% 
      # Compute correlation coefficient
      cor(.)
    
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

test <- cor(data_named$WD %>% 
              gather(key = "feature", value = "value", -stno) %>% 
              mutate(feature = sub(" - .*", "", feature)) %>% 
              spread(key = "feature", value = "value") %>% 
              dplyr::select(-stno))
png(filename = "01_EXPLORATORY_ANALYSES\\plots\\test2.png",
    height = 800, 
    width = 800)
corrplot(test, 
         method="color", 
         col=col(200),  
         diag=FALSE, 
         type="lower", 
         #order="hclust", 
         addCoef.col = "black",
         number.cex = 1.5,
         tl.cex = 1.5,
         tl.col = "black",
         cl.pos = "n",
         title = "Week days - full set of features", 
         mar=c(0,0,1,0))
dev.off()








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

