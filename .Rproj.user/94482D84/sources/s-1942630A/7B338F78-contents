# Project: PCA-K-means-for-PA-features
# Script name: 02_models.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 
#     1. Perform data dimension reduction through PCA,
#        - Compute principal components (PC), 
#        - Classify participants based on PC value (below or above the median) 
#     2. Cluster analysis using k-means 
#        - Cluster analysis (predefined number of clusters: k = 5)
#        - Store clustering in a new variable in the non-standardized data
#        - Save the dataset for regression analyses and plots

# Note: to properly load data and function scripts, open the PCA-K-means-for-PA-features R.project or 
# set the working directory within the corresponding folder on your computer

# Note 2: the scripts to plot and save results from the analyses are provided in a separate R script 
# source("02_PCA-K-means\\03_graphs.R")

# -----------------------------
# Packages
library(tidyverse)
library(haven)

# packages for PCA and clustering
library(cluster)
library(FactoMineR)
library(factoextra)

# -----------------------------
# Data 

# > Selected set of metrics (without number of bouts of different lengths)
#   Non-standardized metrics
load("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\00_DATA\\00_data_PCA_k_means.rda")
data_wei        <- data$wei 
data_WD_WE      <- data$WD_WE 
# with transformed variables (log(x+1))
data_wei_log    <- data$wei_log
data_WD_WE_log  <- data$WD_WE_log 
# with transformed variables (sqrt(x))
data_wei_sqrt   <- data$wei_sqrt
data_WD_WE_sqrt <- data$WD_WE_sqrt

#   Standardized metrics
load("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\00_DATA\\00_z_data_PCA_k_means.rda")
z_data_wei        <- z_data$wei
z_data_WD_WE      <- z_data$WD_WE
# with transformed variables (log(x+1))
z_data_wei_log    <- z_data$wei_log
z_data_WD_WE_log  <- z_data$WD_WE_log
# with transformed variables (sqrt(x))
z_data_wei_sqrt   <- z_data$wei_sqrt
z_data_WD_WE_sqrt <- z_data$WD_WE_sqrt


# > Full set of metrics (with number of bouts of different lengths) - for Sensitivity analyses
#   Non-standardized variables
load("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\00_DATA\\00_data_full_PCA_k_means.rda")
data_wei_full        <- data_full$wei
data_WD_WE_full      <- data_full$WD_WE
# with transformed variables (log(x+1))
data_wei_full_log    <- data_full$wei_log
data_WD_WE_full_log  <- data_full$WD_WE_log
# with transformed variables (sqrt(x))
data_wei_full_sqrt   <- data_full$wei_sqrt
data_WD_WE_full_sqrt <- data_full$WD_WE_sqrt

#   Standardized variables
load("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\00_DATA\\00_z_data_full_PCA_k_means.rda")
z_data_wei_full        <- z_data_full$wei
z_data_WD_WE_full      <- z_data_full$WD_WE
# with transformed variables (log(x+1))
z_data_wei_full_log    <- z_data_full$wei_log
z_data_WD_WE_full_log  <- z_data_full$WD_WE_log
# with transformed variables (sqrt(x))
z_data_wei_full_sqrt   <- z_data_full$wei_sqrt
z_data_WD_WE_full_sqrt <- z_data_full$WD_WE_sqrt

# -----------------------------
# Functions to run PCA and k-means
source("\\\\172.27.137.244\\vieillissement$\\Post doc & PhD\\Mathilde\\Analysis\\02_ARTICLE_2\\PCA-K-means-for-PA-features\\02_PCA-K-means\\00_functions.R")

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


# > Classify participants based on principal component value (below or above the median)
new_data_wei_log         <- class.n.PC(PCA.wei_log$pca,        n = 10,     input.data = data_wei_log)
new_data_wei_sqrt        <- class.n.PC(PCA.wei_sqrt$pca,       n = 10,     input.data = data_wei_sqrt)

new_data_WD_WE_log       <- class.n.PC(PCA.WDWE_log$pca,       n = 10,     input.data = data_WD_WE_log)
new_data_WD_WE_sqrt      <- class.n.PC(PCA.WDWE_sqrt$pca,      n = 10,     input.data = data_WD_WE_sqrt)

new_data_wei_full_log    <- class.n.PC(PCA.wei_full_log$pca,   n = 10,     input.data = data_wei_full_log)
new_data_wei_full_sqrt   <- class.n.PC(PCA.wei_full_sqrt$pca,  n = 10,     input.data = data_wei_full_sqrt)

new_data_WD_WE_full_log  <- class.n.PC(PCA.WDWE_full_log$pca,  n = 10,     input.data = data_WD_WE_full_log)
new_data_WD_WE_full_sqrt <- class.n.PC(PCA.WDWE_full_sqrt$pca, n = 10,     input.data = data_WD_WE_full_sqrt)

# -----------------------------
# Cluster analysis

# > Main analysis - selected set of metrics
set.seed(123)
# All days
KM.wei_log  <- kmeans(z_data_wei_log,  centers = 5, nstart = 25) # (log-transformed skewed variables)
KM.wei_sqrt <- kmeans(z_data_wei_sqrt, centers = 5, nstart = 25) # (sqrt-transformed skewed variables)
# WD & WE days
KM.WDWE_log  <- kmeans(z_data_WD_WE_log,  centers = 5, nstart = 25) # (log-transformed skewed variables) 
KM.WDWE_sqrt <- kmeans(z_data_WD_WE_sqrt, centers = 5, nstart = 25) # (sqrt-transformed skewed variables)

# Cluster membership variable
data_wei_log$km.5 <- KM.wei_log$cluster
data_wei_log$km.5 <- as.factor(data_wei_log$km.5)

data_wei_sqrt$km.5 <- KM.wei_sqrt$cluster
data_wei_sqrt$km.5 <- as.factor(data_wei_sqrt$km.5)

data_WD_WE_log$km.5 <- KM.WDWE_log$cluster
data_WD_WE_log$km.5 <- as.factor(data_WD_WE_log$km.5)

data_WD_WE_sqrt$km.5 <- KM.WDWE_sqrt$cluster
data_WD_WE_sqrt$km.5 <- as.factor(data_WD_WE_sqrt$km.5)

# > Sensitivity analysis - full set of metrics (including number of bouts of different length)
set.seed(123)
# All days
KM.wei_full_log  <- kmeans(z_data_wei_full_log,  centers = 5, nstart = 25) # (log-transformed skewed variables)
KM.wei_full_sqrt <- kmeans(z_data_wei_full_sqrt, centers = 5, nstart = 25) # (sqrt-transformed skewed variables)
# WD & WE days
KM.WDWE_full_log  <- kmeans(z_data_WD_WE_full_log,  centers = 5, nstart = 25) # (log-transformed skewed variables) 
KM.WDWE_full_sqrt <- kmeans(z_data_WD_WE_full_sqrt, centers = 5, nstart = 25) # (sqrt-transformed skewed variables)

# Cluster membership variable
data_wei_full_log$km.5    <- KM.wei_full_log$cluster
data_wei_full_log$km.5    <- as.factor(data_wei_full_log$km.5)

data_wei_full_sqrt$km.5   <- KM.wei_full_sqrt$cluster
data_wei_full_sqrt$km.5   <- as.factor(data_wei_full_sqrt$km.5)

data_WD_WE_full_log$km.5  <- KM.WDWE_full_log$cluster
data_WD_WE_full_log$km.5  <- as.factor(data_WD_WE_full_log$km.5)

data_WD_WE_full_sqrt$km.5 <- KM.WDWE_full_sqrt$cluster
data_WD_WE_full_sqrt$km.5 <- as.factor(data_WD_WE_full_sqrt$km.5)


stop()
# > Save

# Main analyses
save(data_wei_log,         file = "03_RESULTS\\02_K-means\\CLUSTERS\\data_wei_log_km.rda")
save(data_wei_sqrt,        file = "03_RESULTS\\02_K-means\\CLUSTERS\\data_wei_sqrt_km.rda")
save(data_WD_WE_log,       file = "03_RESULTS\\02_K-means\\CLUSTERS\\data_WD_WE_log_km.rda")
save(data_WD_WE_sqrt,      file = "03_RESULTS\\02_K-means\\CLUSTERS\\data_WD_WE_sqrt_km.rda")
# Sensitivity analyses
save(data_wei_full_log,    file = "03_RESULTS\\02_K-means\\CLUSTERS\\data_wei_full_log_km.rda")
save(data_wei_full_sqrt,   file = "03_RESULTS\\02_K-means\\CLUSTERS\\data_wei_full_sqrt_km.rda")
save(data_WD_WE_full_log,  file = "03_RESULTS\\02_K-means\\CLUSTERS\\data_WD_WE_full_log_km.rda")
save(data_WD_WE_full_sqrt, file = "03_RESULTS\\02_K-means\\CLUSTERS\\data_WD_WE_full_sqrt_km.rda")
