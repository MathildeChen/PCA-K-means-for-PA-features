# Project: PCA-K-means-for-PA-features
# Script name: 02_models_k-means.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: PA patterns in the Whitehall accelerometer sub-study

# > Main analyses: Cluster analysis using k-means 
#        - Predefined number of clusters: k = 5
#        - Using the selected set of PA metrics
#        - Metrics computed as weighted daily average (without distinguishing week days from weekend days)
#        - Skewed variables transformed as log(x+1)

# > Sensitivity analyses
#     1. Cluster analysis using k-means with different number of clusters (predefined number of clusters: k = 3 or 4)
#     2. Perform data dimension reduction through PCA,
#        - Compute principal components (PC), 
#        - Classify participants based on PC value (below or above the median) 
#     3. Using a different transformation of skewed variables (sqrt(x))
#     4. Using a different set of metrics (including number of bouts of different lengths)
#     5. Using metrics computed separately for week days and weekend days

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
# > Indicate the path to your data
path_to_data <- "E:/PC_FIXE/Analysis/02_ARTICLE_2"
load(file.path(path_to_data, "00_DATA", "00_data_PCA_k_means.rda"))        # non-standardized metrics, selected set of PA metrics 
load(file.path(path_to_data, "00_DATA", "00_z_data_PCA_k_means.rda"))      # standardized metrics, selected set of PA metrics 
load(file.path(path_to_data, "00_DATA", "00_data_full_PCA_k_means.rda"))   # non-standardized metrics, full set of PA metrics (including nb of bouts of different length)
load(file.path(path_to_data, "00_DATA", "00_z_data_full_PCA_k_means.rda")) # standardized metrics, full set of PA metrics 

# > Raw data
data_wei        <- data$wei
#z_data_wei     <- z_data$wei # skewed variables non-transformed non-transformed 

# > Data for main analyses: weighted daily average, skewed variables transformed (log(x+1))
data_wei_log    <- data$wei_log   # non-standardized
z_data_wei_log  <- z_data$wei_log # standardized

# > Data for sensitivity analyses: 
# >> analyses on transformed variables (sqrt(x))
data_wei_sqrt     <- data$wei_sqrt   # non-standardized
z_data_wei_sqrt   <- z_data$wei_sqrt # standardized

# >> analyses on PA metrics computed for week days and weekend days separately
# skewed variables non-transformed
data_WD_WE <- data$WD_WE   # non-standardized
z_data_WD_WE <- z_data$WD_WE # standardized
# skewed variables log-transformed
data_WD_WE_log <- data$WD_WE_log   # non-standardized
z_data_WD_WE_log <- z_data$WD_WE_log # standardized
# skewed variables sqrt-transformed
#data_WD_WE_sqrt <- data$WD_WE_sqrt   # non-standardized
#z_data_WD_WE_sqrt <- z_data$WD_WE_sqrt # standardized

# >> analyses including the number of bouts of different lengths
# Weighted daily average metrics
# skewed variables non-transformed
data_wei_full        <- data_full$wei   # non-standardized
z_data_wei_full        <- z_data_full$wei # standardized
# skewed variables log-transformed
data_wei_full_log    <- data_full$wei_log   # non-standardized
z_data_wei_full_log    <- z_data_full$wei_log # standardized
# skewed variables sqrt-transformed
#data_wei_full_sqrt   <- data_full$wei_sqrt   # non-standardized
#z_data_wei_full_sqrt   <- z_data_full$wei_sqrt # standardized

# Metrics computed separately in week days and weekend days
# skewed variables non-transformed
data_WD_WE_full      <- data_full$WD_WE   # non-standardized
z_data_WD_WE_full      <- z_data_full$WD_WE # standardized
# skewed variables log-transformed
data_WD_WE_full_log  <- data_full$WD_WE_log   # non-standardized
z_data_WD_WE_full_log  <- z_data_full$WD_WE_log # standardized
# skewed variables sqrt-transformed
#data_WD_WE_full_sqrt <- data_full$WD_WE_sqrt   # non-standardized
#z_data_WD_WE_full_sqrt <- z_data_full$WD_WE_sqrt # standardized

# -----------------------------
# Functions to run PCA and k-means
source("02_PCA-K-means\\00_functions.R")

# -----------------------------
# Main analyses: Cluster analysis using k-means 
#        - Predefined number of clusters: k = 5
#        - Using the selected set of PA metrics
#        - Metrics computed as weighted daily average (without distinguishing week days from weekend days)
#        - Skewed variables transformed as log(x+1)

# > Cluster analyses
set.seed(123)
KM.wei_log  <- kmeans(z_data_wei_log,  centers = 5, nstart = 25) # (log-transformed skewed variables)
data_wei_log$km <- KM.wei_log$cluster
data_wei_log$km <- as.factor(data_wei_log$km)

# -----------------------------
# Sensitivity analyses 1. Cluster analysis using k-means with different number of clusters (predefined number of clusters: k = 3 or 4)

# > Cluster analyses with
# >> k = 3
set.seed(123)
KM.wei_log_3  <- kmeans(z_data_wei_log,  centers = 3, nstart = 25) # (log-transformed skewed variables)

data_wei_log_k3 <- data_wei_log %>% dplyr::select(-km)
data_wei_log_k3$km <- KM.wei_log_3$cluster
data_wei_log_k3$km <- as.factor(data_wei_log_k3$km)

# >> k = 4
set.seed(123)
KM.wei_log_4  <- kmeans(z_data_wei_log,  centers = 4, nstart = 25) # (log-transformed skewed variables)

data_wei_log_k4 <- data_wei_log %>% dplyr::select(-km)
data_wei_log_k4$km <- KM.wei_log_4$cluster
data_wei_log_k4$km <- as.factor(data_wei_log_k4$km)

# > Difference in activity features among and between clusters
#   Note:   comp_PA_feat() function analyses the difference in PA features among and between clusters (see 00_functions.R script or type comp_PA_feat in console)
#   Note 2: We want the difference in *non-transformed* PA features: use data_wei instead of z_data_wei
wei_log_k_means_comp_3    <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log_k3 %>% dplyr::select(stno, km), by = "stno"))
wei_log_k_means_comp_4    <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log_k4 %>% dplyr::select(stno, km), by = "stno"))
wei_log_k_means_comp      <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log    %>% dplyr::select(stno, km), by = "stno"))

# > Comparison between the different cluster analyses

# >> Group kmeans objects and data in a list
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

# > Indicators for each analysis
# >> clusters indicators
map_dfr(list_for_comp, ~{ 
  
  clusters_info <- data.frame(model = .x$title.save, 
                              # Within-cluster sum of square
                              withinss = .x$kmeans.obj$withinss, 
                              # Cluster size
                              size = .x$kmeans.obj$size) %>% 
    gather(key = "Indicator", value = "Indicator_value", withinss, size) 
})
# >> clustering indicators
map_dfr(list_for_comp, ~{ 
  
  data.frame(model = .x$title.save, 
             # Total sum of square: measures the total variance in the data
             totss = .x$kmeans.obj$totss, 
             # Total within-cluster sum of squares, i.e. sum(within-cluster sum of squares): measures the compactness of the clustering --> lower is better
             tot.withinss = .x$kmeans.obj$tot.withinss,
             # Between-cluster sum of squares, i.e. Total sum of square - Total within-cluster sum of squares: measures the difference between clusters --> higher is better
             betweenss = .x$kmeans.obj$betweenss)
  
})

# > Tables with PA metrics characteristic of each cluster, for each clustering
# >> k = 3
wei_log_k_means_comp_3[[2]] %>% 
  mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
  dplyr::select(-x.mean, -x.sd, -letters) %>%
  spread(key = "Group.1", value = lab)%>% 
  View(.)
# >> k = 4
wei_log_k_means_comp_4[[2]] %>% 
  mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
  dplyr::select(-x.mean, -x.sd, -letters) %>%
  spread(key = "Group.1", value = lab) %>%
  View(.)
# >> k = 5
wei_log_k_means_comp[[2]] %>% 
  mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
  dplyr::select(-x.mean, -x.sd, -letters) %>%
  spread(key = "Group.1", value = lab) %>% 
  View(.)

# -----------------------------
# Sensitivity analyses 2. Perform data dimension reduction through PCA

# > Data dimension reduction using PCA 
PCA.wei_log  <- do.pca(z_data_wei_log)

# > Cluster analysis on PCA principal components (PC)
# >> with all 10 PC (PC from PCA of the log-transformed skewed variables)
set.seed(123)
KM.pca.wei_log <- kmeans(PCA.wei_log$pca$x, centers = 5, nstart = 25) 

data_wei_log.pca <- data_wei_log %>% dplyr::select(-km)
data_wei_log.pca$km <- KM.pca.wei_log$cluster
data_wei_log.pca$km <- as.factor(data_wei_log.pca$km)

# >> only 4 first PC (90% variance explained)
set.seed(123)
KM.pca.4.wei_log <- kmeans(PCA.wei_log$pca$x[,1:4], centers = 5, nstart = 25) # (4 first PC from PCA of the log-transformed skewed variables)

data_wei_log.pca.4 <- data_wei_log %>% dplyr::select(-km)
data_wei_log.pca.4$km <- KM.pca.4.wei_log$cluster
data_wei_log.pca.4$km <- as.factor(data_wei_log.pca.4$km)

# > Comparison between the different cluster analyses
# >> Group kmeans objects and data in a list
list_for_comp <- list(
  list(kmeans.obj = KM.wei_log, 
       raw.data = data_wei_log,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Weighted daily average (skewed variables are log(x+1))",
       title.save = "01_wei_log"),
  list(kmeans.obj = KM.pca.wei_log, 
       raw.data = data_wei_log,
       data.for.kmeans = z_data_wei_log,
       title.plot = "all PC from weighted daily average (skewed variables are log(x+1))",
       title.save = "01_wei_log_pca"),
  list(kmeans.obj = KM.pca.4.wei_log, 
       raw.data = data_wei_log,
       data.for.kmeans = z_data_wei_log,
       title.plot = "First 4 PC from weighted daily average (skewed variables are log(x+1))",
       title.save = "01_wei_log_pca_4")
)

# >> Plot clusters
fviz_cluster(object = KM.wei_log, 
             data = z_data_wei_log,
             main = "Analysis without PCA",
             geom = "point",
             palette = "Set1",
             ellipse = FALSE,
             star.plot = FALSE, 
             ggtheme = theme_minimal())

fviz_cluster(object = KM.pca.wei_log, 
             data = z_data_wei_log,
             main = "Analysis with PCA (all components)",
             geom = "point",
             palette = "Set1",
             ellipse = FALSE,
             star.plot = FALSE, 
             ggtheme = theme_minimal())

fviz_cluster(object = KM.pca.4.wei_log, 
             data = z_data_wei_log,
             main = "Analysis with PCA (4 first components)",
             geom = "point",
             palette = "Set1",
             ellipse = FALSE,
             star.plot = FALSE, 
             ggtheme = theme_minimal())

# > Indicators for each analysis
# >> clusters indicators
map_dfr(list_for_comp, ~{ 
  
  clusters_info <- data.frame(model = .x$title.save, 
                              Cluster = c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5"),
                              # Within-cluster sum of square
                              withinss = .x$kmeans.obj$withinss, 
                              # Cluster size
                              size = .x$kmeans.obj$size) %>% 
    gather(key = "Indicator", value = "Indicator_value", withinss, size) %>%
    spread(key = "Cluster", value = "Indicator_value") %>% 
    group_by(model, Indicator) %>%
    mutate(Total = Cluster1 + Cluster2 + Cluster3 + Cluster4 + Cluster5)
})

# >> clustering indicators
map_dfr(list_for_comp, ~{ 
  
  data.frame(model = .x$title.save, 
             # Total sum of square: measures the total variance in the data
             totss = .x$kmeans.obj$totss, 
             # Total within-cluster sum of squares, i.e. sum(within-cluster sum of squares): measures the compactness of the clustering --> lower is better
             tot.withinss = .x$kmeans.obj$tot.withinss,
             # Between-cluster sum of squares, i.e. Total sum of square - Total within-cluster sum of squares: measures the difference between clusters --> higher is better
             betweenss = .x$kmeans.obj$betweenss)
  
})

# > Difference in activity features among and between clusters
#   Note:   comp_PA_feat() function analyses the difference in PA features among and between clusters (see 00_functions.R script or type comp_PA_feat in console)
#   Note 2: We want the difference in *non-transformed* PA features: use data_wei instead of z_data_wei

#wei_log_k_means_comp  <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log       %>% dplyr::select(stno, km), by = "stno")) # already computed in previous sensitivity analysis
wei_log_pca_comp      <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log.pca   %>% dplyr::select(stno, km), by = "stno"))
wei_log_pca4_comp     <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log.pca.4 %>% dplyr::select(stno, km), by = "stno"))

# >> Plot - difference between clusters
rbind(wei_log_k_means_comp[[2]] %>% mutate(type = "Without PCA"),
      wei_log_pca_comp[[2]] %>% mutate(type = "10 first PC"),
      wei_log_pca4_comp[[2]] %>% mutate(type = "4 first PC")) %>% 
  ggplot(., aes(x = type, color = as.factor(Group.1)))+ 
  geom_point(aes(y = x.mean)) +
  facet_wrap(~ Feature, scales = "free_x", ncol = 2) + 
  coord_flip() + 
  scale_color_brewer(palette = "Set1", name = "Cluster") +
  theme_light() + 
  theme(legend.position = "bottom", 
        axis.title = element_blank())

# >> Difference among cluster 1,2,3,4,5 of each classification
# in wei_log_pca_comp <--> wei_log_pca4_comp:
# cluster 1 <--> cluster 2
# cluster 2 <--> cluster 3
# cluster 3 <--> cluster 4
# cluster 4 <--> cluster 1
# cluster 5 <--> cluster 5

fulltab <- rbind(data_wei %>% left_join(data_wei_log       %>% dplyr::select(stno, km), by = "stno") %>% mutate(type = "Without PCA"),
                 data_wei %>% left_join(data_wei_log.pca   %>% dplyr::select(stno, km), by = "stno") %>% mutate(type = "10 first PC") %>% mutate(km = recode(km, "1" = "2", "2" = "3", "3" = "4", "4" = "1", "5" = "5")), # change the name of the clusters to be consistent among classifications
                 data_wei %>% left_join(data_wei_log.pca.4 %>% dplyr::select(stno, km), by = "stno") %>% mutate(type = "4 first PC"))

fulltab %>% 
  split(.$km) %>% 
  map(., ~ {
  
    cat(paste0("\nResults for cluster ", unique(.x$km),"\n"))
    
    for(v in names(.x[,!names(.x) %in% c("stno", "km", "type")]))
    {
      # > One-way ANOVA: compare the difference among clusters, depending on classification
      anova.var <- aov(.x[,colnames(.x) == v] ~ type, data = .x)
      cat(paste0("p for ", v, " = ", round(summary(anova.var)[[1]][[5]][1], 5), "\n"))
    }
  })

# -----------------------------
# > Sensitivity analyses 3. Using a different transformation of skewed variables (sqrt(x))
set.seed(123)
KM.wei_sqrt <- kmeans(z_data_wei_sqrt, centers = 5, nstart = 25) # (sqrt-transformed skewed variables)
data_wei_sqrt$km.5 <- KM.wei_sqrt$cluster
data_wei_sqrt$km.5 <- as.factor(data_wei_sqrt$km.5)

# -----------------------------
# > Sensitivity analyses 4. Using a different set of metrics (including number of bouts of different lengths)
set.seed(123)
KM.wei_full_log  <- kmeans(z_data_wei_full_log,  centers = 5, nstart = 25) # (log-transformed skewed variables)
data_wei_full_log$km.5    <- KM.wei_full_log$cluster
data_wei_full_log$km.5    <- as.factor(data_wei_full_log$km.5)

# -----------------------------
# > Sensitivity analyses 5. Using metrics computed separately for week days and weekend days
set.seed(123)
KM.WDWE_log  <- kmeans(z_data_WD_WE_log,  centers = 5, nstart = 25) # (log-transformed skewed variables) 
data_WD_WE_log$km.5 <- KM.WDWE_log$cluster
data_WD_WE_log$km.5 <- as.factor(data_WD_WE_log$km.5)


stop()
# -----------------------------
# > Save analyses outputs

# >> Results of main analysis + sensitivity analysis 1 (different k values)
# Rename clustering membership variables
data_wei_log_k3 <- dplyr::rename(data_wei_log_k3, "km.3" = "km")
data_wei_log_k4 <- dplyr::rename(data_wei_log_k4, "km.4" = "km")
data_wei_log_k5 <- dplyr::rename(data_wei_log,    "km.5" = "km")

# Check names
names(data_wei_log_k3)
names(data_wei_log_k4)
names(data_wei_log_k5)

# Merge datasets, keep only the clustering membership variables
km_outputs <- left_join(data_wei_log_k3, data_wei_log_k4) %>% 
  left_join(., data_wei_log_k5) %>% 
  dplyr::select(stno, starts_with("km."))

# Check names and size of the different clusters
names(km_outputs)
summary(km_outputs)

# Check if names are ok for stata format
km_outputs <- janitor::clean_names(km_outputs)

# Save outputs 
# for stata
write_dta(km_outputs, file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/km_outputs.dta"))
# for R
save(km_outputs, file = file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/km_outputs.rda"))

# >> Results of main analysis + sensitivity analysis 2 (with or without PCA)
# Rename cluster membership variables
data_wei_log.pca   <- dplyr::rename(data_wei_log.pca, "km.pca" = "km")
data_wei_log.pca.4 <- dplyr::rename(data_wei_log.pca.4, "km.pca.4" = "km")

# Check names
names(data_wei_log_k5)
names(data_wei_log.pca)
names(data_wei_log.pca.4)

# Merge datasets, keep only the clustering membership variables
km_pca_outputs <- left_join(data_wei_log_k5, data_wei_log.pca) %>% 
  left_join(., data_wei_log.pca.4) %>% 
  dplyr::select(stno, starts_with("km."))

# Save outputs for R
save(km_pca_outputs, file = file.path("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/km_pca_outputs.rda"))

# >> Results of sensitivity analyses 3 to 5
save(data_wei_sqrt,        file = "E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/data_wei_sqrt_km.rda")
save(data_wei_full_log,    file = "E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/data_wei_full_log_km.rda")
save(data_WD_WE_log,       file = "E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/data_WD_WE_log_km.rda")
