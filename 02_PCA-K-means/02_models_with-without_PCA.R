# Cluster analysis: with or without PCA

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
data_wei_log    <- data$wei_log

#   Standardized metrics
load("00_DATA\\00_z_data_PCA_k_means.rda")
z_data_wei        <- z_data$wei
# with transformed variables (log(x+1))
z_data_wei_log    <- z_data$wei_log

# -----------------------------
# Functions to run PCA and k-means
source("02_PCA-K-means\\00_functions.R")

# -----------------------------
# PCA 
PCA.wei_log  <- do.pca(z_data_wei_log)  # (log-transformed skewed variables)
# -----------------------------
# Cluster analyses

# > Analysis on scaled PA features
set.seed(123)
KM.wei_log  <- kmeans(z_data_wei_log,  centers = 5, nstart = 25) # (log-transformed skewed variables)
data_wei_log$km.5 <- KM.wei_log$cluster
data_wei_log$km.5 <- as.factor(data_wei_log$km.5)


# > Analysis on PCA principal components (PC)
# With all 10 PC
set.seed(123)
KM.pca.wei_log <- kmeans(PCA.wei_log$pca$x, centers = 5, nstart = 25) # (PC from PCA of the log-transformed skewed variables)
data_wei_log.pca <- data_wei_log %>% dplyr::select(-km.5)
data_wei_log.pca$km.5 <- KM.pca.wei_log$cluster
data_wei_log.pca$km.5 <- as.factor(data_wei_log.pca$km.5)

# Only 4 first PC (90% variance explained)
set.seed(123)
KM.pca.4.wei_log <- kmeans(PCA.wei_log$pca$x[,1:4], centers = 5, nstart = 25) # (PC from PCA of the log-transformed skewed variables)
data_wei_log.pca.4 <- data_wei_log %>% dplyr::select(-km.5)
data_wei_log.pca.4$km.5 <- KM.pca.4.wei_log$cluster
data_wei_log.pca.4$km.5 <- as.factor(data_wei_log.pca.4$km.5)

# > Comparison between the different cluster analyses

# Group kmeans objects and data in a list
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

# Plot clusters
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

# Kmeans indicators
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


map_dfr(list_for_comp, ~{ 
  
  data.frame(model = .x$title.save, 
             # Total sum of square: measures the total variance in the data
             totss = .x$kmeans.obj$totss, 
             # Total within-cluster sum of squares, i.e. sum(within-cluster sum of squares): measures the compactness of the clustering --> lower is better
             tot.withinss = .x$kmeans.obj$tot.withinss,
             # Between-cluster sum of squares, i.e. Total sum of square - Total within-cluster sum of squares: measures the difference between clusters --> higher is better
             betweenss = .x$kmeans.obj$betweenss)
  
})

# Compare PA features among the different classifications
wei_log_comp      <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log %>% dplyr::select(stno, km.5), by = "stno"))
wei_log_pca_comp  <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log.pca %>% dplyr::select(stno, km.5), by = "stno"))
wei_log_pca4_comp <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log.pca.4 %>% dplyr::select(stno, km.5), by = "stno"))

# Plot (mean only)
rbind(wei_log_comp[[2]] %>% mutate(type = "Without PCA"),
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

# Plot (mean + sd)
rbind(wei_log_comp[[2]] %>% mutate(type = "Without PCA"),
      wei_log_pca_comp[[2]] %>% mutate(type = "10 first PC"),
      wei_log_pca4_comp[[2]] %>% mutate(type = "4 first PC")) %>% 
  ggplot(., aes(x = type, color = as.factor(Group.1)))+ 
  geom_point(aes(y = x.mean)) +
  geom_linerange(aes(ymin = x.mean-x.sd, ymax = x.mean+x.sd)) +
  facet_wrap(~ Feature, scales = "free_x", ncol = 2) + 
  coord_flip() + 
  scale_color_brewer(palette = "Set1", name = "Cluster") +
  theme_light() + 
  theme(legend.position = "bottom", 
        axis.title = element_blank())


# in wei_log_pca_comp <--> wei_log_pca4_comp:
# cluster 1 <--> cluster 2
# cluster 2 <--> cluster 3
# cluster 3 <--> cluster 4
# cluster 4 <--> cluster 1
# cluster 5 <--> cluster 5



# difference among clusters for each classification
fulltab <- rbind(data_wei %>% left_join(data_wei_log %>% dplyr::select(stno, km.5), by = "stno") %>% mutate(type = "Without PCA"),
                 data_wei %>% left_join(data_wei_log.pca %>% dplyr::select(stno, km.5), by = "stno") %>% mutate(type = "10 first PC") %>% mutate(km.5 = recode(km.5, "1" = "2", "2" = "3", "3" = "4", "4" = "1", "5" = "5")),
                 data_wei %>% left_join(data_wei_log.pca.4 %>% dplyr::select(stno, km.5), by = "stno") %>% mutate(type = "10 first PC"))

fulltab %>% 
  split(.$km.5) %>% 
  map(., ~ {
    
    
    cat(paste0("\nResults for cluster ", unique(.x$km.5),"\n"))
    
    for(v in names(.x[,!names(.x) %in% c("stno", "km.5", "type")]))
    {
      
      # > One-way ANOVA: compare the difference among clusters, depending on classification
      anova.var <- aov(.x[,colnames(.x) == v] ~ type, data = .x)
      cat(paste0("p for ", v, " = ", round(summary(anova.var)[[1]][[5]][1], 5), "\n"))
      
      
    }
    
    
  })
