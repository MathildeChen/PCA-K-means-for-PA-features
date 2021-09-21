# Project: PCA-K-means-for-PA-features
# Script name: 02_models_PCA.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Data reduction using PCA on PA metrics

# -----------------------------
# Packages
library(tidyverse)
library(haven)

# packages for PCA
library(FactoMineR)
library(factoextra)

# -----------------------------
# Data 
# > Indicate the path to your data
path_to_data <- "E:/PC_FIXE/Analysis/02_ARTICLE_2"
load(file.path(path_to_data, "00_DATA", "00_data_PCA_k_means.rda"))        # non-standardized metrics, selected set of PA metrics 
load(file.path(path_to_data, "00_DATA", "00_z_data_PCA_k_means.rda"))      # standardized metrics, selected set of PA metrics 
load(file.path(path_to_data, "00_DATA", "00_data_full_PCA_k_means.rda"))   # non-standardized metrics, full set of PA metrics (including nb of bouts of different length)
load(file.path(path_to_data, "00_DATA", "00_z_data_full_PCA_k_means.rda")) # standardized metrics, full set of PA metrics 

# > Raw data
data_wei              <- data$wei
z_data_wei            <- z_data$wei # skewed variables non-transformed non-transformed 

# > Data for main analyses: weighted daily average, skewed variables transformed (log(x+1))
data_wei_log          <- data$wei_log   # non-standardized
z_data_wei_log        <- z_data$wei_log # standardized

# > Data for sensitivity analyses: 
# >> analyses on transformed variables (sqrt(x))
data_wei_sqrt         <- data$wei_sqrt   # non-standardized
z_data_wei_sqrt       <- z_data$wei_sqrt # standardized

# >> analyses on PA metrics computed for week days and weekend days separately
# skewed variables non-transformed
data_WD_WE            <- data$WD_WE   # non-standardized
z_data_WD_WE          <- z_data$WD_WE # standardized
# skewed variables log-transformed
data_WD_WE_log        <- data$WD_WE_log   # non-standardized
z_data_WD_WE_log      <- z_data$WD_WE_log # standardized
# skewed variables sqrt-transformed
data_WD_WE_sqrt       <- data$WD_WE_sqrt   # non-standardized
z_data_WD_WE_sqrt     <- z_data$WD_WE_sqrt # standardized

# >> analyses including the number of bouts of different lengths
# Weighted daily average metrics
# skewed variables non-transformed
data_wei_full         <- data_full$wei   # non-standardized
z_data_wei_full       <- z_data_full$wei # standardized
# skewed variables log-transformed
data_wei_full_log      <- data_full$wei_log   # non-standardized
z_data_wei_full_log    <- z_data_full$wei_log # standardized
# skewed variables sqrt-transformed
data_wei_full_sqrt     <- data_full$wei_sqrt   # non-standardized
z_data_wei_full_sqrt   <- z_data_full$wei_sqrt # standardized

# Metrics computed separately in week days and weekend days
# skewed variables non-transformed
data_WD_WE_full        <- data_full$WD_WE   # non-standardized
z_data_WD_WE_full      <- z_data_full$WD_WE # standardized
# skewed variables log-transformed
data_WD_WE_full_log    <- data_full$WD_WE_log   # non-standardized
z_data_WD_WE_full_log  <- z_data_full$WD_WE_log # standardized
# skewed variables sqrt-transformed
data_WD_WE_full_sqrt   <- data_full$WD_WE_sqrt   # non-standardized
z_data_WD_WE_full_sqrt <- z_data_full$WD_WE_sqrt # standardized

# -----------------------------
# Functions to run PCA and k-means
source("02_PCA-K-means\\00_functions.R")

# -----------------------------
# PCA 

# > First analysis - selected set of metrics
# All days 
PCA.wei_log  <- do.pca(z_data_wei_log)  # (log-transformed skewed variables)
PCA.wei_sqrt <- do.pca(z_data_wei_sqrt) # (sqrt-transformed skewed variables)
# WD & WE days
PCA.WDWE_log  <- do.pca(z_data_WD_WE_log)  # (log-transformed skewed variables)
PCA.WDWE_sqrt <- do.pca(z_data_WD_WE_sqrt) # (sqrt-transformed skewed variables)

# > Second analysis - full set of metrics (including number of bouts of different length)
# All days 
PCA.wei_full_log  <- do.pca(z_data_wei_full_log)  # (log-transformed skewed variables)
PCA.wei_full_sqrt <- do.pca(z_data_wei_full_sqrt) # (sqrt-transformed skewed variables)
# WD & WE days
PCA.WDWE_full_log  <- do.pca(z_data_WD_WE_full_log)  # (log-transformed skewed variables)
PCA.WDWE_full_sqrt <- do.pca(z_data_WD_WE_full_sqrt) # (sqrt-transformed skewed variables)

# > PCA outputs
PCA_wei_log_outputs  <- data.frame(cbind(stno = data_wei$stno, PCA.wei_log$pca$x))
PCA_wei_sqrt_outputs <- data.frame(cbind(stno = data_wei$stno, PCA.wei_sqrt$pca$x))

PCA_WDWE_log_outputs  <- data.frame(cbind(stno = data_WD_WE$stno, PCA.WDWE_log$pca$x))
PCA_WDWE_sqrt_outputs <- data.frame(cbind(stno = data_WD_WE$stno, PCA.WDWE_sqrt$pca$x))

PCA_wei_log_full_outputs  <- data.frame(cbind(stno = data_wei_full$stno, PCA.wei_full_log$pca$x))
PCA_wei_sqrt_full_outputs <- data.frame(cbind(stno = data_wei_full$stno, PCA.wei_full_sqrt$pca$x))

PCA_WDWE_log_full_outputs  <- data.frame(cbind(stno = data_WD_WE_full$stno, PCA.WDWE_full_log$pca$x))
PCA_WDWE_sqrt_full_outputs <- data.frame(cbind(stno = data_WD_WE_full$stno, PCA.WDWE_full_sqrt$pca$x))

# > List of outputs
list_outputs <- list(list(name = "PCA_outputs",        x = PCA_wei_log_outputs), 
                     list(name = "sensitivity_analyses/02_wei_sqrt",       x = PCA_wei_sqrt_outputs), 
                     list(name = "sensitivity_analyses/03_WDWE_log",       x = PCA_WDWE_log_outputs), 
                     list(name = "sensitivity_analyses/04_WDWE_sqrt",      x = PCA_WDWE_sqrt_outputs),
                     list(name = "sensitivity_analyses/05_wei_full_log",   x = PCA_wei_log_full_outputs), 
                     list(name = "sensitivity_analyses/06_wei_full_sqrt",  x = PCA_wei_sqrt_full_outputs), 
                     list(name = "sensitivity_analyses/07_WDWE_full_log",  x = PCA_WDWE_log_full_outputs), 
                     list(name = "sensitivity_analyses/08_WDWE_full_sqrt", x = PCA_WDWE_sqrt_full_outputs))

stop()
# > Save
list_outputs %>% 
  map(., ~ { 
    
    # Check if names are ok for stata format
    output_for_stata <- janitor::clean_names(.$x)
    output_for_R <- .$x
    
    # Save outputs 
    # for stata
    write_dta(output_for_stata, file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2", "03_RESULTS", "01_PCA", paste0(.$name, ".dta")))
    # for R
    save(output_for_R, file = file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2", "03_RESULTS", "01_PCA", paste0(.$name, ".rda")))
    
    })



