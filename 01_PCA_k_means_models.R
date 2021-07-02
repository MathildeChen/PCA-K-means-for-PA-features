# Script name: 01_PCA_k_means_1.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 
#     1. Running PCA 
#     2. Running k-means (k = 5)

# -----------------------------
# Packages
#library(multcomp)
library(tidyverse)
library(haven)
library(corrr)
library(xlsx)
library(testthat)

# packages for PCA and clustering
library(cluster)
library(FactoMineR)
library(factoextra)

# -----------------------------
# Data 

# > Selected set of metrics (without number of bouts of different lengths)
#   Non-standardized metrics
load("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//data//00_data_PCA_k_means.rda")
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

#   Standardized metrics
load("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//data//00_z_data_PCA_k_means.rda")
z_data_wei        <- z_data$wei
z_data_WD         <- z_data$WD
z_data_WE         <- z_data$WE
z_data_WD_WE      <- z_data$WD_WE
# with log transformed variables
z_data_wei_log    <- z_data$wei_log
z_data_WD_log     <- z_data$WD_log
z_data_WE_log     <- z_data$WE_log
z_data_WD_WE_log  <- z_data$WD_WE_log
# with sqrt transformed variables
z_data_wei_sqrt   <- z_data$wei_sqrt
z_data_WD_sqrt    <- z_data$WD_sqrt
z_data_WE_sqrt    <- z_data$WE_sqrt
z_data_WD_WE_sqrt <- z_data$WD_WE_sqrt


# > Full set of metrics (with number of bouts of different lengths) - for Sensitivity analyses
#   Non-standardized variables
load("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//data//00_data_full_PCA_k_means.rda")
data_wei_full        <- data_full$wei
data_WD_full         <- data_full$WD
data_WE_full         <- data_full$WE
data_WD_WE_full      <- data_full$WD_WE
# with log transformed variables
data_wei_full_log    <- data_full$wei_log
data_WD_full_log     <- data_full$WD_log
data_WE_full_log     <- data_full$WE_log
data_WD_WE_full_log  <- data_full$WD_WE_log
# with sqrt transformed variables
data_wei_full_sqrt   <- data_full$wei_sqrt
data_WD_full_sqrt    <- data_full$WD_sqrt
data_WE_full_sqrt    <- data_full$WE_sqrt
data_WD_WE_full_sqrt <- data_full$WD_WE_sqrt

#   Standardized variables
load("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//data//00_z_data_full_PCA_k_means.rda")
z_data_wei_full        <- z_data_full$wei
z_data_WD_full         <- z_data_full$WD
z_data_WE_full         <- z_data_full$WE
z_data_WD_WE_full      <- z_data_full$WD_WE
# with log transformed variables
z_data_wei_full_log    <- z_data_full$wei_log
z_data_WD_full_log     <- z_data_full$WD_log
z_data_WE_full_log     <- z_data_full$WE_log
z_data_WD_WE_full_log  <- z_data_full$WD_WE_log
# with sqrt transformed variables
z_data_wei_full_sqrt   <- z_data_full$wei_sqrt
z_data_WD_full_sqrt    <- z_data_full$WD_sqrt
z_data_WE_full_sqrt    <- z_data_full$WE_sqrt
z_data_WD_WE_full_sqrt <- z_data_full$WD_WE_sqrt

# > Covariates (N = 4006)
load("E://PC_FIXE//Analysis//02_ARTICLE_2//00_DATA//tab_full_cov_s11.rda")
# Drop variables with NA (N = 3893)
tab_11_fin <- tab_11 %>% 
  drop_na(sex, fage_s, ethnicity_i, fstatusx_i_2, edu_imp, flgrlump_i_ordinal,
          fesmoke_i, funitwk0_i_3, ffruitvg_i_3, 
          fbmi_i_3, hypertension, hyperlipidemia, prevalent_diabete,
          mmm_index) 

# -----------------------------
# Functions to run PCA and k-means
source("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//01_PCA_k_means_functions.R")

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

# -----------------------------
# Cluster analysis

# > Optimal number of clusters
# Elbow method
fviz_nbclust(z_data_wei_log, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + 
  ggtitle("log-transformed skewed variables")
# --> 5 clusters
fviz_nbclust(z_data_wei_sqrt, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+ 
  ggtitle("sqrt-transformed skewed variables")

# > First analysis - selected set of metrics
set.seed(123)
# All days
KM.wei_log  <- kmeans(z_data_wei_log,  centers = 5, nstart = 25) # (log-transformed skewed variables)
KM.wei_sqrt <- kmeans(z_data_wei_sqrt, centers = 5, nstart = 25) # (sqrt-transformed skewed variables)
# WD & WE days
KM.WDWE_log  <- kmeans(z_data_WD_WE_log,  centers = 5, nstart = 25) # (log-transformed skewed variables) 
KM.WDWE_sqrt <- kmeans(z_data_WD_WE_sqrt, centers = 5, nstart = 25) # (sqrt-transformed skewed variables)

# Store clustering in new variables
data_wei_log$km.5 <- KM.wei_log$cluster
data_wei_log$km.5 <- as.factor(data_wei_log$km.5)

data_wei_sqrt$km.5 <- KM.wei_sqrt$cluster
data_wei_sqrt$km.5 <- as.factor(data_wei_sqrt$km.5)

data_WD_WE_log$km.5 <- KM.WDWE_log$cluster
data_WD_WE_log$km.5 <- as.factor(data_WD_WE_log$km.5)

data_WD_WE_sqrt$km.5 <- KM.WDWE_sqrt$cluster
data_WD_WE_sqrt$km.5 <- as.factor(data_WD_WE_sqrt$km.5)

# > Second analysis - full set of metrics (including number of bouts of different length)
set.seed(123)
# All days
KM.wei_full_log  <- kmeans(z_data_wei_full_log,  centers = 5, nstart = 25) # (log-transformed skewed variables)
KM.wei_full_sqrt <- kmeans(z_data_wei_full_sqrt, centers = 5, nstart = 25) # (sqrt-transformed skewed variables)
# WD & WE days
KM.WDWE_full_log  <- kmeans(z_data_WD_WE_full_log,  centers = 5, nstart = 25) # (log-transformed skewed variables) 
KM.WDWE_full_sqrt <- kmeans(z_data_WD_WE_full_sqrt, centers = 5, nstart = 25) # (sqrt-transformed skewed variables)

# Store clustering in new variables
data_wei_full_log$km.5    <- KM.wei_full_log$cluster
data_wei_full_log$km.5    <- as.factor(data_wei_full_log$km.5)

data_wei_full_sqrt$km.5   <- KM.wei_full_sqrt$cluster
data_wei_full_sqrt$km.5   <- as.factor(data_wei_full_sqrt$km.5)

data_WD_WE_full_log$km.5  <- KM.WDWE_full_log$cluster
data_WD_WE_full_log$km.5  <- as.factor(data_WD_WE_full_log$km.5)

data_WD_WE_full_sqrt$km.5 <- KM.WDWE_full_sqrt$cluster
data_WD_WE_full_sqrt$km.5 <- as.factor(data_WD_WE_full_sqrt$km.5)


