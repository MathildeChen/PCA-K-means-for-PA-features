# Project: PCA-K-means-for-PA-features
# Script name: 01_kmeans_optimal_nb_clusters.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Performing some tests to determine the optimal number of clusters in k-means analyses
#        on several sets of features (selected or full set of features, skewed variables log-transformed or root-squared transformed, weighted daily average or week/weekend days average)

# Several tests are performed
# 1) Elbow method: total within-clusters sum of square (wss), indicates the dispersion of clustering --> lower is better
# 2) Silhouette method: indicates the average distance between clusters --> higher is better
# 3) Gap statistic: compares the wss for different values of k to its expected values under assumption of null distribution of the data --> ?
# 4) 30 indices see NbClust function from the NbClust package

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
library(NbClust)

# additional packages
library(cowplot)

# -----------------------------
# Data 

# > Indicate the path to your data
path_to_data <- "E:/PC_FIXE/Analysis/02_ARTICLE_2/00_DATA"
load(file.path(path_to_data, "00_data_PCA_k_means_july.rda"))        # non-standardized metrics, selected set of PA metrics 
load(file.path(path_to_data, "00_z_data_PCA_k_means_july.rda"))      # standardized metrics, selected set of PA metrics 
load(file.path(path_to_data, "00_data_full_PCA_k_means_july.rda"))   # non-standardized metrics, full set of PA metrics (including nb of bouts of different length)
load(file.path(path_to_data, "00_z_data_full_PCA_k_means_july.rda")) # standardized metrics, full set of PA metrics 

# > Selected set of metrics (without number of bouts of different lengths)
#   Standardized metrics
z_data_wei        <- z_data$wei
z_data_WD_WE      <- z_data$WD_WE
# with transformed variables (log(x+1))
z_data_wei_log    <- z_data$wei_log
z_data_WD_WE_log  <- z_data$WD_WE_log
# without M5TIME for sensitivity analyses
z_data_wei_log_no_M5    <- z_data_wei_log %>% dplyr::select(-z_M5TIME_num_wei)
z_data_WD_WE_log_no_M5  <- z_data$WD_WE_log %>% dplyr::select(-z_M5TIME_num_WE, -z_M5TIME_num_WD)
# with transformed variables (sqrt(x))
z_data_wei_sqrt   <- z_data$wei_sqrt
z_data_WD_WE_sqrt <- z_data$WD_WE_sqrt


# > Full set of metrics (with number of bouts of different lengths) - for Sensitivity analyses
#   Standardized variables
z_data_wei_full        <- z_data_full$wei
z_data_WD_WE_full      <- z_data_full$WD_WE
# with transformed variables (log(x+1))
z_data_wei_full_log    <- z_data_full$wei_log
z_data_WD_WE_full_log  <- z_data_full$WD_WE_log
# with transformed variables (sqrt(x))
z_data_wei_full_sqrt   <- z_data_full$wei_sqrt
z_data_WD_WE_full_sqrt <- z_data_full$WD_WE_sqrt

# -----------------------------
# Optimal number of clusters

# Performed tests:
# 1) Elbow method: total within-clusters sum of square (wss), indicates the dispersion of clustering --> lower is better
# 2) Silhouette method: indicates the average distance between clusters --> higher is better
# 3) Gap statistic: compares the wss for different values of k to its expected values under assumption of null distribution of the data --> ?

# > Weighted daily average
list(cbind(title = "Selected set (log-transformed skewed variables)",                z_data_wei_log),
     cbind(title = "Full set (log-transformed skewed variables)",                    z_data_wei_full_log),
     cbind(title = "Selected set without M5TIME (log-transformed skewed variables)", z_data_wei_log_no_M5),
     cbind(title = "Selected set (root-squared skewed variables)",                   z_data_wei_sqrt),
     cbind(title = "Full set (root-squared skewed variables)",                       z_data_wei_full_sqrt)) %>% 
  map(., ~ { 
    
    # > Remove first column for the tests
    data_for_plot <- .x[,c(-1)]
    # > Title of the plot
    title_plot <- unique(.x$title)
    
    # > Plot
    plot <- plot_grid(
      fviz_nbclust(data_for_plot, kmeans, method = "wss")                                                + ggtitle(label = paste0(title_plot), subtitle = "Elbow method"),
      fviz_nbclust(data_for_plot, kmeans, method = "silhouette")                                         + ggtitle(label = "",                 subtitle = "Silhouette method"),
      fviz_nbclust(data_for_plot, kmeans, method = "gap_stat", nstart = 25, nboot = 50, verbose = FALSE) + ggtitle(label = "",                 subtitle = "Gap statistic method"),
      nrow = 2)
    
    # > Save the plot
    ggsave(paste0("E://PC_FIXE//Analysis//02_ARTICLE_2//03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//wei_Elbow-Silhouette-Gap_",title_plot,"_july.png"), 
           width = 10, height = 6, 
           dpi = 300)
    
    })

# > Week/weekend days average
list(cbind(title = "Selected set (log-transformed skewed variables)",                z_data_WD_WE_log),
     cbind(title = "Full set (log-transformed skewed variables)",                    z_data_WD_WE_full_log),
     cbind(title = "Selected set without M5TIME (log-transformed skewed variables)", z_data_WD_WE_log_no_M5),
     cbind(title = "Selected set (root-squared skewed variables)",                   z_data_WD_WE_sqrt),
     cbind(title = "Full set (root-squared skewed variables)",                       z_data_WD_WE_full_sqrt)) %>% 
  map(., ~ { 
    
    # > Remove first column for the tests
    data_for_plot <- .x[,c(-1)]
    # > Title of the plot
    title_plot <- unique(.x$title)
    
    # > Plot
    plot <- plot_grid(
      fviz_nbclust(data_for_plot, kmeans, method = "wss", linecolor = "darkred")                                                + ggtitle(label = paste0(title_plot), subtitle = "Elbow method"),
      fviz_nbclust(data_for_plot, kmeans, method = "silhouette", linecolor = "darkred")                                         + ggtitle(label = "",                 subtitle = "Silhouette method"),
      fviz_nbclust(data_for_plot, kmeans, method = "gap_stat", nstart = 25, nboot = 50, verbose = FALSE, linecolor = "darkred") + ggtitle(label = "",                 subtitle = "Gap statistic method"),
      nrow = 2)
    
    # > Save the plot
    ggsave(paste0("D://PC_FIXE//Analysis//02_ARTICLE_2//03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//WD_WE-Elbow-Silhouette-Gap_",title_plot,"_july.png"), 
           width = 10, height = 6, 
           dpi = 300)
    
  })


# 30 indices for determining the number of clusters and proposes to user the best clustering scheme 
# from the different results obtained by varying all combinations of number of clusters, distance measures, 
# and clustering methods
# See the full list of indices in help section > Details by running: ?NbClust

# > Weighted daily average
nb_wei_log            <- NbClust(data = z_data_wei_log,            diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")
nb_wei_full_log       <- NbClust(data = z_data_wei_full_log,       diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")
nb_wei_log_no_M5      <- NbClust(data = z_data_wei_log_no_M5,      diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")
nb_wei_sqrt           <- NbClust(data = z_data_wei_sqrt,           diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")
nb_wei_full_sqrt      <- NbClust(data = z_data_wei_full_sqrt,      diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")

fviz_nbclust(nb_wei_log)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_wei_log_july.png", 
       width = 6, height = 4, 
       dpi = 300)

fviz_nbclust(nb_wei_full_log)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_wei_full_log_july.png", 
       width = 6, height = 4, 
       dpi = 300)

fviz_nbclust(nb_wei_log_no_M5)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_wei_log_no_M5_july.png", 
       width = 6, height = 4, 
       dpi = 300)

fviz_nbclust(nb_wei_sqrt)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_wei_sqrt_july.png", 
       width = 6, height = 4, 
       dpi = 300)

fviz_nbclust(nb_wei_full_sqrt)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_wei_full_sqrt_july.png", 
       width = 6, height = 4, 
       dpi = 300)


# Week/weekend day average

nb_WD_WE_log       <- NbClust(data = z_data_WD_WE_log,       diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")
nb_WD_WE_full_log  <- NbClust(data = z_data_WD_WE_full_log,  diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")
nb_WD_WE_log_no_M5 <- NbClust(data = z_data_WD_WE_log_no_M5, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")
nb_WD_WE_sqrt      <- NbClust(data = z_data_WD_WE_sqrt,      diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")
nb_WD_WE_full_sqrt <- NbClust(data = z_data_WD_WE_full_sqrt, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans")

fviz_nbclust(nb_WD_WE_log)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_WD_WE_log_july.png", 
       width = 6, height = 4, 
       dpi = 300)

fviz_nbclust(nb_WD_WE_full_log)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_WD_WE_full_log_july.png", 
       width = 6, height = 4, 
       dpi = 300)

fviz_nbclust(nb_WD_WE_log_no_M5)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_WD_WE_log_no_M5_july.png", 
       width = 6, height = 4, 
       dpi = 300)

fviz_nbclust(nb_WD_WE_sqrt)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_WD_WE_sqrt_july.png", 
       width = 6, height = 4, 
       dpi = 300)

fviz_nbclust(nb_WD_WE_full_sqrt)
ggsave("03_RESULTS//02_K-means//TEST_OPT_NB_CLUSTERS//NbClust_z_data_WD_WE_full_sqrt_july.png", 
       width = 6, height = 4, 
       dpi = 300)


# Silhouette method ++
set.seed(123)
z_data_wei_log
KM.wei_log  <- kmeans(z_data_wei_log,  centers = 5, nstart = 25)
ss <- silhouette(KM.wei_log$cluster, dist(z_data_wei_log))
ss_mean <- mean(ss[, 3])

ss_dat <- as.data.frame(matrix(ss, ncol = 3))
colnames(ss_dat) <- c("cluster", "neighbor", "sil")

ss_dat %>% group_by(cluster) %>% summarise(n = n(), mean = mean(sil))

ss_dat %>% 
  arrange(cluster, abs(sil)) %>% 
  mutate(n = 1:nrow(ss_dat)) %>% 
  ggplot() + 
  geom_col(aes(x = n, 
               y = abs(sil), 
               fill = as.factor(cluster),
               color = as.factor(cluster)), 
           position = position_dodge2(width=1)) +
  geom_hline(aes(yintercept = mean(sil)), lty = 2)

df <- z_data_wei_log
for(i in 2:5)
{
  set.seed(123)
  KM.wei_log_k  <- kmeans(df,  centers = i, nstart = 25)
  ss_i <- silhouette(KM.wei_log_k$cluster, dist(df))
  ss_i_mean <- mean(ss_i[, 3])
  
  ss_i_dat <- as.data.frame(matrix(ss_i, ncol = 3))
  colnames(ss_i_dat) <- c("cluster", "neighbor", "sil")
  
  p_i <- ss_i_dat %>% 
    arrange(cluster, abs(sil)) %>% 
    mutate(n = 1:nrow(ss_dat)) %>% 
    ggplot(aes(x = n, 
               y = abs(sil), ymin = 0, ymax = abs(sil), 
               color = as.factor(cluster))) + 
    geom_linerange() +
    #geom_boxplot(aes(fill = as.factor(cluster)), color = "black", alpha = 0.3, position = position_dodge2(width = 0.75, preserve = "single")) +
    #geom_point(aes(x = median(n), y = mean(abs(sil)), color = as.factor(cluster))) +
    geom_hline(aes(yintercept = mean(sil)), lty = 2) +
    ggtitle(paste0("k = ", i)) +
    labs(x = "Cluster", y = "Silhouette") +
    lims(y = c(0, 0.6)) +
    coord_flip() +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    theme(legend.position = "none", 
          axis.text.y = element_blank(),
          panel.grid = element_blank())
  
  ggsave(plot = p_i, 
         filename = paste0("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/TEST_OPT_NB_CLUSTERS/Silhouette_wei_log_", i, ".png"), 
         width = 3, height = 5)
  
}










