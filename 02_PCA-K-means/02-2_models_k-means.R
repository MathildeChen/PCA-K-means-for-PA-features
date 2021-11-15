# Project: PCA-K-means-for-PA-features
# Script name: 02_models_k-means.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Identification of movement behavior profiles in the Whitehall accelerometer sub-study

# > Main analyses: Cluster analysis using k-means 
#        - Predefined number of clusters: k = 5, 4 or 3
#        - Using the selected set of PA features
#        - Features computed as weighted daily average (without distinguishing week days from weekend days)
#        - Skewed variables transformed as log(x+1)

# > Sensitivity analyses
#     S1. Cluster analysis excluding time of most 5 active hours from features set (with k = 5, 4, or 3)
#     S2. Perform data dimension reduction through PCA,
#        - Compute principal components (PC), 
#        - Classify participants based on PC value (below or above the median) 
#     S3. Using a different transformation of skewed variables (sqrt(x))
#     S4. Using a different set of features (including number of bouts of different lengths)
#     S5. Using features computed separately for week days and weekend days

# Note: to properly load data and function scripts, open the PCA-K-means-for-PA-features R.project or 
# set the working directory within the corresponding folder on your computer

# Note 2: the scripts to plot results from the analyses are provided in a separate R script 
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
# > path data 
path_to_data <- "E:/PC_FIXE/Analysis/02_ARTICLE_2/00_DATA"
# > loading data (GGIR 2.4-1)
load(file.path(path_to_data, "00_data_PCA_k_means_july.rda"))        # non-standardized features, selected set of PA features 
load(file.path(path_to_data, "00_z_data_PCA_k_means_july.rda"))      # standardized features, selected set of PA features 
load(file.path(path_to_data, "00_data_full_PCA_k_means_july.rda"))   # non-standardized features, full set of PA features (including nb of bouts of different length)
load(file.path(path_to_data, "00_z_data_full_PCA_k_means_july.rda")) # standardized features, full set of PA features 

# -----------------------------
# Functions to run PCA and k-means
source("02_PCA-K-means\\00_functions.R")

################################################
# Main analyses - Cluster analysis using k-means with different number of clusters (k = 5, 4 or 3)
#        - Using the selected set of PA features
#        - Features computed as weighted daily average (without distinguishing week days from weekend days)
#        - Skewed variables transformed as log(x+1)
# -----------------------------

# -----------------------------
# > Main analyses - k = 5 -----

# > Data
data_wei       <- data$wei       # raw data (data to which cluster membership variable will be added)
z_data_wei_log <- z_data$wei_log # standardized (data used for k-means)

# > Clustering
set.seed(123)
KM.wei_log <- kmeans(z_data_wei_log,  centers = 5, nstart = 25) 
# centers: number of predefined clusters
# nstart: number of random starting partitions when the argument "centers" is a number 
# nstart = 25. This means that R will try 25 different random starting assignments 
# and then select the best results corresponding to the one with the lowest within cluster variation. 
# (default value = 1, but nstart > 1 (25 or 50) is often recommended in order to have a more stable result)

# > Store cluster membership variable
data_wei$km <- KM.wei_log$cluster
data_wei$km <- as.factor(data_wei$km)

table(data_wei$km, exclude = NULL)
#   1    2    3    4    5 
# 730 1046  893  303 1036

# > Reorder labels to have Cluster 1 (best) to Cluster 5 (worst)
# Initial order <-> New order
# 1 -> 1 (best)
# 3 -> 2
# 5 -> 3
# 2 -> 4
# 4 -> 5 (worst)
data_wei <- data_wei %>% 
  mutate(km = recode(km, "1" = "1", "3" = "2", "5" = "3", "2" = "4", "4" = "5"),
         km = factor(km, levels = c("1", "2", "3", "4", "5")))

table(data_wei$km, exclude = NULL)
# Check that N correspond between old and new labels
#    1    2    3    4    5 
#  730  893 1036 1046  303

# -----------------------------
# > Main analyses - k = 4 -----

# > Data
data_wei_k4     <- data$wei        # raw data
z_data_wei_log  <- z_data$wei_log  # standardized (same data as main analysis)

# > Clustering
set.seed(123)
KM.wei_log_k4 <- kmeans(z_data_wei_log,  centers = 4, nstart = 25) 

# > Store cluster membership variable
data_wei_k4$km <- KM.wei_log_k4$cluster
data_wei_k4$km <- as.factor(data_wei_k4$km)

table(data_wei_k4$km, exclude = NULL)
#    1    2    3    4 
# 1283 1116  968  641

# > Reorder labels to have Cluster 1 (best) to Cluster 4 (worst)
# Initial order <-> New order
# 2 -> 1 
# 3 -> 2
# 1 -> 3
# 4 -> 4 (worst)
data_wei_k4 <- data_wei_k4 %>% 
  mutate(km = recode(km, "2" = "1", "3" = "2", "1" = "3", "4" = "4"),
         km = factor(km, levels = c("1", "2", "3", "4")))

table(data_wei_k4$km, exclude = NULL)
#    1    2    3    4 
# 1116  968 1283  641

# -----------------------------
# > Main analyses - k = 3 ----

# > Data
data_wei_k3      <- data$wei      # raw data
z_data_wei_log  <- z_data$wei_log # standardized (same data as main analysis)

# > Clustering
set.seed(123)
KM.wei_log_k3  <- kmeans(z_data_wei_log,  centers = 3, nstart = 25) 

# > Store cluster membership variable
data_wei_k3$km <- KM.wei_log_k3$cluster
data_wei_k3$km <- as.factor(data_wei_k3$km)

table(data_wei_k3$km, exclude = NULL)
#    1    2    3 
# 1408  698 1902

# > Reorder labels to have Cluster 1 (best) to Cluster 3 (worst)
# Initial order <-> New order
# 1 -> 1 (best)
# 3 -> 2
# 2 -> 3 (worst)
data_wei_k3 <- data_wei_k3 %>% 
  mutate(km = recode(km, "1" = "1", "3" = "2", "2" = "3"),
         km = factor(km, levels = c("1", "2", "3")))

table(data_wei_k3$km, exclude = NULL)
#    1    2    3 
# 1408 1902  698 



# -----------------------------


################################################
# Sensitivity analyses 

# -----------------------------
# S1. Cluster analysis excluding time of most 5 active hours from features set (with k = 5, 4, or 3) ----
#        - Excluding M5TIME of the set of PA features
#        - Features computed as weighted daily average (without distinguishing week days from weekend days)
#        - Skewed variables transformed as log(x+1)
# -----------------------------
# > S1.1 - Cluster analysis without M5TIME: k = 5 ----

# > Data:  
data_wei_no_M5       <- data_wei       %>% dplyr::select(-M5TIME_num_wei)   # raw data without M5TIME
z_data_wei_log_no_M5 <- z_data_wei_log %>% dplyr::select(-z_M5TIME_num_wei) # standardized

# > Clustering
set.seed(123)
KM.wei_log_no_M5 <- kmeans(z_data_wei_log_no_M5,  centers = 5, nstart = 25) 

# > Store cluster membership variable
data_wei_no_M5$km <- KM.wei_log_no_M5$cluster
data_wei_no_M5$km <- as.factor(data_wei_no_M5$km)

table(data_wei_no_M5$km, exclude = NULL)
#   1    2    3    4    5 
# 751  301  890 1029 1037 

# > Reorder labels to have Cluster 1 (best) to Cluster 5 (worst)
# Initial order <-> New order
# 1 -> 1 (best)
# 3 -> 2
# 4 -> 3
# 5 -> 4
# 2 -> 5 (worst)
data_wei_no_M5 <- data_wei_no_M5 %>% 
  mutate(km = recode(km, "1" = "1", "3" = "2", "4" = "3", "5" = "4", "2" = "5"),
         km = factor(km, levels = c("1", "2", "3", "4", "5")))

table(data_wei_no_M5$km, exclude = NULL)
#   1    2    3    4    5 
# 751  890 1029 1037  301

# -----------------------------
# > S1.2 - Cluster analysis without M5TIME: k = 4 ----

# > Data
# without M5TIME
data_wei_no_M5_k4    <- data_wei       %>% dplyr::select(-M5TIME_num_wei)   # raw data without M5TIME
z_data_wei_log_no_M5 <- z_data_wei_log %>% dplyr::select(-z_M5TIME_num_wei) # standardized  (same data as S3)

# > Clustering
set.seed(123)
KM.wei_log_4_no_M5 <- kmeans(z_data_wei_log_no_M5, centers = 4, nstart = 25)

# > Store cluster membership variable
data_wei_no_M5_k4$km <- KM.wei_log_4_no_M5$cluster
data_wei_no_M5_k4$km <- as.factor(data_wei_no_M5_k4$km)

table(data_wei_no_M5_k4$km, exclude = NULL)
#    1    2    3    4 
# 1127  640  951 1290

# > Reorder labels to have Cluster 1 (best) to Cluster 4 (worst)
# Initial order <-> New order
# 2 -> 1 (best)
# 1 -> 2
# 3 -> 3
# 4 -> 4 (worst)
data_wei_no_M5_k4 <- data_wei_no_M5_k4 %>% 
  mutate(km = recode(km, "2" = "1", "1" = "2", "3" = "3", "4" = "4"),
         km = factor(km, levels = c("1", "2", "3", "4")))

table(data_wei_no_M5_k4$km, exclude = NULL)
#    1    2    3    4 
# 1127  951 1290  640


# -----------------------------
# > S1.3 - Cluster analysis without M5TIME: k = 3 ----

# > Data
data_wei_no_M5_k3    <- data_wei       %>% dplyr::select(-M5TIME_num_wei)   # raw data without M5TIME
z_data_wei_log_no_M5 <- z_data_wei_log %>% dplyr::select(-z_M5TIME_num_wei) # standardized (same data as S3)

# > Clustering
set.seed(123)
KM.wei_log_3_no_M5       <- kmeans(z_data_wei_log_no_M5, centers = 3, nstart = 25)

# > Store cluster membership variable
data_wei_no_M5_k3$km <- KM.wei_log_3_no_M5$cluster
data_wei_no_M5_k3$km <- as.factor(data_wei_no_M5_k3$km)

table(data_wei_no_M5_k3$km, exclude = NULL)
#    1    2    3 
# 1907  697 1404

# > Reorder labels to have Cluster 1 (best) to Cluster 3 (worst)
# Initial order <-> New order
# 3 -> 1 (best)
# 1 -> 2
# 2 -> 3 (worst)
data_wei_no_M5_k3 <- data_wei_no_M5_k3 %>% 
  mutate(km = recode(km, "3" = "1", "1" = "2", "2" = "3"),
         km = factor(km, levels = c("1", "2", "3")))

table(data_wei_no_M5_k3$km, exclude = NULL)
#    1    2    3 
# 1404 1907  697 


# -----------------------------

# -----------------------------
# S2. k-means clustering based on principal components derived from PCA
#        - Using the selected set of PA features
#        - Features computed as weighted daily average (without distinguishing week days from weekend days)
#        - Skewed variables transformed as log(x+1)

# -----------------------------
# Data dimension reduction through PCA
PCA.wei_log  <- do.pca(z_data_wei_log)
# -----------------------------
# > S2.1 - Cluster analysis based on PC : with all 10 PC ----

# > Data
data_wei.pca    <- data$wei
data_wei.pca_k4 <- data$wei
data_wei.pca_k3 <- data$wei

# > Clustering
set.seed(123)
KM.pca.wei_log    <- kmeans(PCA.wei_log$pca$x, centers = 5, nstart = 25) 
set.seed(123)
KM.pca.wei_log_k4 <- kmeans(PCA.wei_log$pca$x, centers = 4, nstart = 25) 
set.seed(123)
KM.pca.wei_log_k3 <- kmeans(PCA.wei_log$pca$x, centers = 3, nstart = 25) 

# > Store cluster membership variable
data_wei.pca$km <- KM.pca.wei_log$cluster
data_wei.pca$km <- as.factor(data_wei.pca$km)

table(data_wei.pca$km, exclude = NULL)
#    1    2    3    4    5 
# 1033  884 1044  744  303 

data_wei.pca_k4$km <- KM.pca.wei_log_k4$cluster
data_wei.pca_k4$km <- as.factor(data_wei.pca_k4$km)

table(data_wei.pca_k4$km, exclude = NULL)
#   1    2    3    4 
# 646  966 1286 1110 

data_wei.pca_k3$km <- KM.pca.wei_log_k3$cluster
data_wei.pca_k3$km <- as.factor(data_wei.pca_k3$km)

table(data_wei.pca_k3$km, exclude = NULL)
#    1    2    3 
# 1906  698 1404 

# -----------------------------
# > S2.2 - Cluster analysis based on PC : 4 first PC (90% variance explained) ----

# > Data
data_wei.pca.4    <- data$wei
data_wei.pca.4_k4 <- data$wei
data_wei.pca.4_k3 <- data$wei

# > Clustering
set.seed(123)
KM.pca.4.wei_log    <- kmeans(PCA.wei_log$pca$x[,1:4], centers = 5, nstart = 25) # (4 first PC from PCA)

set.seed(123)
KM.pca.4.wei_log_k4 <- kmeans(PCA.wei_log$pca$x[,1:4], centers = 4, nstart = 25) # (4 first PC from PCA)

set.seed(123)
KM.pca.4.wei_log_k3 <- kmeans(PCA.wei_log$pca$x[,1:4], centers = 3, nstart = 25) # (4 first PC from PCA)


# > Store cluster membership variable
data_wei.pca.4$km <- KM.pca.4.wei_log$cluster
data_wei.pca.4$km <- as.factor(data_wei.pca.4$km)

table(data_wei.pca.4$km, exclude = NULL)
#   1    2    3    4    5 
# 734 1032  322 1022  898

data_wei.pca.4_k4$km <- KM.pca.4.wei_log_k4$cluster
data_wei.pca.4_k4$km <- as.factor(data_wei.pca.4_k4$km)

table(data_wei.pca.4_k4$km, exclude = NULL)
#   1    2    3    4 
# 633  948 1134 1293 

data_wei.pca.4_k3$km <- KM.pca.4.wei_log_k3$cluster
data_wei.pca.4_k3$km <- as.factor(data_wei.pca.4_k3$km)

table(data_wei.pca.4_k3$km, exclude = NULL)
#    1    2    3 
# 1385 1914  709 

# -----------------------------

# -----------------------------
# S3. Cluster analysis using a different transformation of skewed variables
#        - Using the selected set of PA features
#        - Features computed as weighted daily average (without distinguishing week days from weekend days)
#        - Skewed variables transformed as sqrt(x)

# -----------------------------
# > S3.1 - Cluster with skewed variables transformed as sqrt(x): k = 5 ----

# > Data
data_wei_sqrt     <- data$wei        # raw data
z_data_wei_sqrt   <- z_data$wei_sqrt # standardized data (skewed variables sqrt-transformed)

# > Clustering
set.seed(123)
KM.wei_sqrt <- kmeans(z_data_wei_sqrt, centers = 5, nstart = 25)

# > Store variable for cluster membership
data_wei_sqrt$km <- KM.wei_sqrt$cluster
data_wei_sqrt$km <- as.factor(data_wei_sqrt$km)

table(data_wei_sqrt$km, exclude = NULL)
#   1    2    3    4    5 
# 695  300  848 1066 1099 

# -----------------------------
# > S3.2 - Cluster with skewed variables transformed as sqrt(x): k = 4 ----

# > Data
data_wei_sqrt_k4 <- data$wei        # raw data
z_data_wei_sqrt  <- z_data$wei_sqrt # standardized data (skewed variables sqrt-transformed) (same data as S3.1)

# > Clustering
set.seed(123)
KM.wei_sqrt_k4   <- kmeans(z_data_wei_sqrt, centers = 4, nstart = 25)

# > Store variable for cluster membership
data_wei_sqrt_k4$km <- KM.wei_sqrt_k4$cluster
data_wei_sqrt_k4$km <- as.factor(data_wei_sqrt_k4$km)

table(data_wei_sqrt_k4$km, exclude = NULL)
#    1    2    3    4 
# 1364 1076  896  672 

# -----------------------------
# > S3.3 - Cluster with skewed variables transformed as sqrt(x): k = 3 ----

# > Data
data_wei_sqrt_k3 <- data$wei        # raw data
z_data_wei_sqrt  <- z_data$wei_sqrt # standardized data (skewed variables sqrt-transformed) (same data as S3.1)

# > Clustering
set.seed(123)
KM.wei_sqrt_k3 <- kmeans(z_data_wei_sqrt, centers = 3, nstart = 25)

# > Store variable for cluster membership
data_wei_sqrt_k3$km <- KM.wei_sqrt_k3$cluster
data_wei_sqrt_k3$km <- as.factor(data_wei_sqrt_k3$km)

table(data_wei_sqrt_k3$km, exclude = NULL)
#    1    2    3 
# 1329 1948  731
# -----------------------------

# -----------------------------
# S4. Cluster analysis using a different set of features
#        - Using the full set of PA features (including number of unbouted fragments, number of bouts of 1-10 min in SB, LIPA and MVPA, bouts of 10-30 and >30 min pf SB)
#        - Features computed as weighted daily average (without distinguishing week days from weekend days)
#        - Skewed variables transformed as log(x+1)

# -----------------------------
# > S4.1 - Cluster analysis using full set of features: k = 5 ----

# > Data
data_wei_full       <- data_full$wei       # raw data
z_data_wei_full_log <- z_data_full$wei_log # standardized data (skewed variables log-transformed)

# > Clustering
set.seed(123)
KM.wei_full_log     <- kmeans(z_data_wei_full_log, centers = 5, nstart = 25) 

# > Store variable for cluster membership
data_wei_full$km    <- KM.wei_full_log$cluster
data_wei_full$km    <- as.factor(data_wei_full$km)

table(data_wei_full$km, exclude = NULL)
#    1    2    3    4    5 
# 1073  408  991  872  664 

# -----------------------------
# > S4.2 - Cluster analysis using full set of features: k = 4 ----

# > Data
data_wei_full_k4     <- data_full$wei       # raw data
z_data_wei_full_log  <- z_data_full$wei_log # standardized data (skewed variables log-transformed), same as S4.1

# > Clustering
set.seed(123)
KM.wei_full_log_k4   <- kmeans(z_data_wei_full_log, centers = 4, nstart = 25) 

# > Store variable for cluster membership
data_wei_full_k4$km    <- KM.wei_full_log_k4$cluster
data_wei_full_k4$km    <- as.factor(data_wei_full_k4$km)

table(data_wei_full_k4$km, exclude = NULL)
#    1    2    3    4 
# 1285 1045  934  744 

# -----------------------------
# > S4.3 - Cluster analysis using full set of features: k = 3 ----

# > Data
data_wei_full_k3     <- data_full$wei       # raw data
z_data_wei_full_log  <- z_data_full$wei_log # standardized data (skewed variables log-transformed), same as S4.1

# > Clustering
set.seed(123)
KM.wei_full_log_k3   <- kmeans(z_data_wei_full_log, centers = 3, nstart = 25) 

# > Store variable for cluster membership
data_wei_full_k3$km    <- KM.wei_full_log_k3$cluster
data_wei_full_k3$km    <- as.factor(data_wei_full_k3$km)

table(data_wei_full_k3$km, exclude = NULL)
#    1    2    3 
# 1308  794 1906 

# -----------------------------

# -----------------------------
# S5. Cluster analysis using features computed separately for week days and weekend days
#        - Using the selected set of PA features
#        - Features computed separately for week days and weekend days
#        - Skewed variables transformed as log(x+1)

# -----------------------------
# > S5.1 - Cluster analysis using features computed separately for week days and weekend days: k = 5 ----

# > Data
data_WD_WE       <- data$WD_WE   # raw data
z_data_WD_WE_log <- z_data$WD_WE_log # standardized data (skewed variables log-transformed)

# > Clustering
set.seed(123)
KM.WDWE_log  <- kmeans(z_data_WD_WE_log, centers = 5, nstart = 25)

# > Store variable for cluster membership
data_WD_WE$km <- KM.WDWE_log$cluster
data_WD_WE$km <- as.factor(data_WD_WE$km)

table(data_WD_WE$km, exclude = NULL)
#   1    2    3    4    5 
# 380  970 1125  886  647 

# -----------------------------
# > S5.2 - Cluster analysis using features computed separately for week days and weekend days: k = 4 ----

# > Data
data_WD_WE_k4    <- data$WD_WE   # raw data
z_data_WD_WE_log <- z_data$WD_WE_log # standardized data (skewed variables log-transformed)

# > Clustering
set.seed(123)
KM.WDWE_log_k4  <- kmeans(z_data_WD_WE_log, centers = 4, nstart = 25)

# > Store variable for cluster membership
data_WD_WE_k4$km <- KM.WDWE_log_k4$cluster
data_WD_WE_k4$km <- as.factor(data_WD_WE_k4$km)

table(data_WD_WE_k4$km, exclude = NULL)
#    1    2    3    4 
# 1275 1152  955  626

# -----------------------------
# > S5.3 - Cluster analysis using features computed separately for week days and weekend days: k = 3 ----

# > Data
data_WD_WE_k3    <- data$WD_WE   # raw data
z_data_WD_WE_log <- z_data$WD_WE_log # standardized data (skewed variables log-transformed)

# > Clustering
set.seed(123)
KM.WDWE_log_k3  <- kmeans(z_data_WD_WE_log, centers = 3, nstart = 25)

# > Store variable for cluster membership
data_WD_WE_k3$km <- KM.WDWE_log_k3$cluster
data_WD_WE_k3$km <- as.factor(data_WD_WE_k3$km)

table(data_WD_WE_k3$km, exclude = NULL)
#    1    2    3 
# 1430  673 1905

# -----------------------------

stop()
# -----------------------------
# > Save analyses outputs

# > Main analyses ----

# Rename clustering membership variables
data_wei_k3 <- dplyr::rename(data_wei_k3, "km.3" = "km")
data_wei_k4 <- dplyr::rename(data_wei_k4, "km.4" = "km")
data_wei_k5 <- dplyr::rename(data_wei,    "km.5" = "km")

# Check names
names(data_wei_log_k3)
names(data_wei_log_k4)
names(data_wei_log_k5)

# Merge data sets, keep only the clustering membership variables
km_outputs <- left_join(data_wei_log_k3, data_wei_log_k4) %>% 
  left_join(., data_wei_log_k5) %>% 
  dplyr::select(stno, starts_with("km."))

# Check names and size of the different clusters
names(km_outputs)
summary(km_outputs)

# Check if names are OK for Stata format
km_outputs <- janitor::clean_names(km_outputs)

# Save outputs 
# for Stata
write_dta(km_outputs, file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/MAIN_ANALYSES/km_outputs_july.dta"))
# for R
save(km_outputs, file = file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/MAIN_ANALYSES/km_outputs_july.rda"))


# > S1 (excluding timing of 5 most active hours) ----
# Rename clustering membership variables
data_wei_no_M5_k3 <- dplyr::rename(data_wei_no_M5_k3, "km.3.no_M5" = "km")
data_wei_no_M5_k4 <- dplyr::rename(data_wei_no_M5_k4, "km.4.no_M5" = "km")
data_wei_no_M5_k5 <- dplyr::rename(data_wei_no_M5,    "km.5.no_M5" = "km")

# Check names
names(data_wei_no_M5_k3)
names(data_wei_no_M5_k4)
names(data_wei_no_M5_k5)

# Merge data sets, keep only the clustering membership variables
km_outputs_S1 <- left_join(data_wei_no_M5_k3, data_wei_no_M5_k4) %>% 
  left_join(., data_wei_no_M5_k5) %>% 
  dplyr::select(stno, starts_with("km."))

# Check names and size of the different clusters
names(km_outputs_S1)
summary(km_outputs_S1)

# Check if names are OK for Stata format
km_outputs_S1 <- janitor::clean_names(km_outputs_S1)

# Save outputs 
# for Stata
write_dta(km_outputs_S1, file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S1/km_outputs_S1.dta"))
# for R
save(km_outputs_S1, file = file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S1/km_outputs_S1.rda"))


# > S2 (with or without PCA) ----

# Rename cluster membership variables
data_wei.pca   <- dplyr::rename(data_wei.pca, "km.5.pca" = "km")
data_wei.pca.4 <- dplyr::rename(data_wei.pca.4, "km.5.pca.4" = "km")

data_wei.pca_k4   <- dplyr::rename(data_wei.pca_k4, "km.4.pca" = "km")
data_wei.pca.4_k4 <- dplyr::rename(data_wei.pca.4_k4, "km.4.pca.4" = "km")

data_wei.pca_k3   <- dplyr::rename(data_wei.pca_k3, "km.3.pca" = "km")
data_wei.pca.4_k3 <- dplyr::rename(data_wei.pca.4_k3, "km.3.pca.4" = "km")

# Check names
names(data_wei.pca)
names(data_wei.pca_k4)
names(data_wei.pca_k3)

names(data_wei.pca.4)
names(data_wei.pca.4_k4)
names(data_wei.pca.4_k3)

# Merge datasets, keep only the clustering membership variables
km_outputs_S2 <- data_wei.pca %>%
  left_join(., data_wei.pca.4) %>% 
  left_join(., data_wei.pca_k4) %>% 
  left_join(., data_wei.pca.4_k4) %>% 
  left_join(., data_wei.pca_k3) %>% 
  left_join(., data_wei.pca.4_k3) %>% 
  dplyr::select(stno, starts_with("km."))

# Check names and size of the different clusters
names(km_outputs_S2)
summary(km_outputs_S2)

# Check if names are OK for Stata format
km_outputs_S2 <- janitor::clean_names(km_outputs_S2)

# Save outputs for R
# for Stata
write_dta(km_outputs_S2, file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S2/km_outputs_S2.dta"))
# for R
save(km_outputs_S2, file = file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S2/km_outputs_S2.rda"))

# > S3 (skewed variables transformed as sqrt(x)) ----

# Rename clustering membership variables
data_wei_sqrt_k3 <- dplyr::rename(data_wei_sqrt_k3, "km.3.sqrt" = "km")
data_wei_sqrt_k4 <- dplyr::rename(data_wei_sqrt_k4, "km.4.sqrt" = "km")
data_wei_sqrt_k5 <- dplyr::rename(data_wei_sqrt,    "km.5.sqrt" = "km")

# Check names
names(data_wei_sqrt_k3)
names(data_wei_sqrt_k4)
names(data_wei_sqrt_k5)

# Merge data sets, keep only the clustering membership variables
km_outputs_S3 <- data_wei_sqrt_k3 %>%
  left_join(., data_wei_sqrt_k4) %>% 
  left_join(., data_wei_sqrt_k5) %>% 
  dplyr::select(stno, starts_with("km."))

# Check names and size of the different clusters
names(km_outputs_S3)
summary(km_outputs_S3)

# Check if names are OK for Stata format
km_outputs_S3 <- janitor::clean_names(km_outputs_S3)

# Save outputs 
# for Stata
write_dta(km_outputs_S3, file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S3/km_outputs_S3.dta"))
# for R
save(km_outputs_S3, file = file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S3/km_outputs_S3.rda"))

# > S4 (full set of features) ----

# Rename clustering membership variables
data_wei_full_k3 <- dplyr::rename(data_wei_full_k3, "km.3.full" = "km")
data_wei_full_k4 <- dplyr::rename(data_wei_full_k4, "km.4.full" = "km")
data_wei_full_k5 <- dplyr::rename(data_wei_full,    "km.5.full" = "km")

# Check names
names(data_wei_full_k3)
names(data_wei_full_k4)
names(data_wei_full_k5)

# Merge data sets, keep only the clustering membership variables
km_outputs_S4 <- data_wei_full_k3 %>%
  left_join(., data_wei_full_k4) %>% 
  left_join(., data_wei_full_k5) %>% 
  dplyr::select(stno, starts_with("km."))

# Check names and size of the different clusters
names(km_outputs_S4)
summary(km_outputs_S4)

# Check if names are OK for Stata format
km_outputs_S4 <- janitor::clean_names(km_outputs_S4)

# Save outputs 
# for Stata
write_dta(km_outputs_S4, file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S4/km_outputs_S4.dta"))
# for R
save(km_outputs_S4, file = file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S4/km_outputs_S4.rda"))

# > S5 (features derived for week days and weekend days) ----

# Rename clustering membership variables
data_WD_WE_k3 <- dplyr::rename(data_WD_WE_k3, "km.3.WD_WE" = "km")
data_WD_WE_k4 <- dplyr::rename(data_WD_WE_k4, "km.4.WD_WE" = "km")
data_WD_WE_k5 <- dplyr::rename(data_WD_WE,    "km.5.WD_WE" = "km")

# Check names
names(data_WD_WE_k3)
names(data_WD_WE_k4)
names(data_WD_WE_k5)

# Merge data sets, keep only the clustering membership variables
km_outputs_S5 <- data_WD_WE_k3 %>%
  left_join(., data_WD_WE_k4) %>% 
  left_join(., data_WD_WE_k5) %>% 
  dplyr::select(stno, starts_with("km."))

# Check names and size of the different clusters
names(km_outputs_S5)
summary(km_outputs_S5)

# Check if names are OK for Stata format
km_outputs_S5 <- janitor::clean_names(km_outputs_S5)

# Save outputs 
# for Stata
write_dta(km_outputs_S5, file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S5/km_outputs_S5.dta"))
# for R
save(km_outputs_S5, file = file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/S5/km_outputs_S5.rda"))












