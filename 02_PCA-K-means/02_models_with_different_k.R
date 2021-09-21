# Project: PCA-K-means-for-PA-features
# Script name: 02_models_with_different_k.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 
#     Cluster analysis using k-means with different number of clusters 
#        - Cluster analysis (predefined number of clusters: k = 3, 4 or 5)
#        - Store clustering in a new variable in the non-standardized data
#        - Save the dataset for regression analyses and plots

# Note: to properly load data and function scripts, open the PCA-K-means-for-PA-features R.project or 
# set the working directory within the corresponding folder on your computer

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
load("00_DATA\\00_data_PCA_k_means.rda")
data_wei        <- data$wei 
# with transformed variables (log(x+1))
data_wei_log       <- data$wei_log
# a copy for sensitivity analyses
data_wei_log_k3 <- data_wei_log
data_wei_log_k4 <- data_wei_log

#   Standardized metrics
load("00_DATA\\00_z_data_PCA_k_means.rda")
z_data_wei        <- z_data$wei
# with transformed variables (log(x+1))
z_data_wei_log    <- z_data$wei_log

# -----------------------------
# Functions to run PCA and k-means
source("02_PCA-K-means\\00_functions.R")

# -----------------------------
# Cluster analyses

# > Analysis on scaled PA features
# 1. Main analysis - selected set of metrics
set.seed(123)
KM.wei_log  <- kmeans(z_data_wei_log,  centers = 5, nstart = 25) # (log-transformed skewed variables)

# Cluster membership variable
data_wei_log$km.5 <- KM.wei_log$cluster
data_wei_log$km.5 <- as.factor(data_wei_log$km.5)

# > Sensitivity analysis - Test with different number of clusters
# k = 3
set.seed(123)
KM.wei_log_3  <- kmeans(z_data_wei_log,  centers = 3, nstart = 25) # (log-transformed skewed variables)
data_wei_log_k3$km.5 <- KM.wei_log_3$cluster
data_wei_log_k3$km.5 <- as.factor(data_wei_log_k3$km.5)

# k = 4
set.seed(123)
KM.wei_log_4  <- kmeans(z_data_wei_log,  centers = 4, nstart = 25) # (log-transformed skewed variables)
data_wei_log_k4$km.5 <- KM.wei_log_4$cluster
data_wei_log_k4$km.5 <- as.factor(data_wei_log_k4$km.5)

# -----------------------------
# Plots

# > Difference in activity features among and between clusters
#   comp_PA_feat() function analyseq the difference in PA features among and between clusters
#   see 00_functions.R script

# We want the difference in *non-transformed* PA features 
wei_log_k_means_comp    <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log %>% dplyr::select(stno, km.5), by = "stno"))
wei_log_k_means_comp_3    <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log_k3 %>% dplyr::select(stno, km.5), by = "stno"))
wei_log_k_means_comp_4    <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log_k4 %>% dplyr::select(stno, km.5), by = "stno"))


# > Comparison between the different cluster analyses

# Group kmeans objects and data in a list
list_for_comp <- list(
  list(kmeans.obj = KM.wei_log, 
       raw.data = data_wei_log,
       data.for.kmeans = z_data_wei_log,
       title.plot = "k = 5",
       title.save = "01_wei_log"),
  list(kmeans.obj = KM.wei_log_3, 
       raw.data = data_wei_log_k3,
       data.for.kmeans = z_data_wei_log,
       title.plot = "k = 3",
       title.save = "01_wei_log_3"),
  list(kmeans.obj = KM.wei_log_4, 
       raw.data = data_wei_log_k4,
       data.for.kmeans = z_data_wei_log,
       title.plot = "k = 4",
       title.save = "01_wei_log_4")
)

# Kmeans indicators
map_dfr(list_for_comp, ~{ 
  
  clusters_info <- data.frame(model = .x$title.save, 
                              #Cluster = c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5"),
                              # Within-cluster sum of square
                              withinss = .x$kmeans.obj$withinss, 
                              # Cluster size
                              size = .x$kmeans.obj$size) %>% 
    gather(key = "Indicator", value = "Indicator_value", withinss, size) 
})


map_dfr(list_for_comp, ~{ 
  
  data.frame(model = .x$title.save, 
             # Total sum of square: measures the total variance in the data
             totss = .x$kmeans.obj$totss, 
             # Total within-cluster sum of squares, i.e. sum(within-cluster sum of squares): measures the compactness of the clustering --> lower is better
             tot.withinss = .x$kmeans.obj$tot.withinss,
             # Between-cluster sum of squares, i.e. Total sum of square - Total within-cluster sum of squares: measures the difference between clusters --> higher is better
             betweenss = .x$kmeans.obj$betweenss)
  
})



wei_log_k_means_comp[[2]] %>% 
  mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
  dplyr::select(-x.mean, -x.sd, -letters) %>%
  spread(key = "Group.1", value = lab) %>% 
  View(.)

wei_log_k_means_comp_3[[2]] %>% 
  mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
  dplyr::select(-x.mean, -x.sd, -letters) %>%
  spread(key = "Group.1", value = lab)%>% 
  View(.)

wei_log_k_means_comp_4[[2]] %>% 
  mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
  dplyr::select(-x.mean, -x.sd, -letters) %>%
  spread(key = "Group.1", value = lab) %>%
  View(.)



# -----------------------------
# Cluster analysis outputs
# > One dataset with the different clustering for each stno

# Rename clustering for k = 3 and k = 4 
data_wei_log_k3 <- dplyr::rename(data_wei_log_k3, "km.3" = "km.5")
data_wei_log_k4 <- dplyr::rename(data_wei_log_k4, "km.4" = "km.5")

# Check names
names(data_wei_log_k3)
names(data_wei_log_k4)

# Merge datasets, keep only the clustering membership variables
km_outputs <- left_join(data_wei_log_k3, data_wei_log_k4) %>% 
  left_join(., data_wei_log) %>% 
  dplyr::select(stno, starts_with("km."))

# Check names and size of the different clusters
names(km_outputs)
summary(km_outputs)

# Check if names are ok for stata format
km_outputs <- janitor::clean_names(km_outputs)

# Save outputs 
# for stata
library(haven)
write_dta(km_outputs, file.path(getwd(), "03_RESULTS", "02_K-means", "km_outputs.dta"))

# for R
save(km_outputs, file = file.path(getwd(), "03_RESULTS", "02_K-means", "km_outputs.rda"))



