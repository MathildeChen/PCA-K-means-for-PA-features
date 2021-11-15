# Project: PCA-K-means-for-PA-features
# Script name: 03-2_graphs_tables_k-means_sensitivity_analyses.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Tables and plots for sensitivity analyses

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

# Note 2: the scripts of the analyses are provided in a separate R script 
# source("02_PCA-K-means\\02_models_k-means_2.R")

# -----------------------------
# Packages
library(corrplot)
library(wesanderson)
library(ggplot2)
library(cowplot)
library(xlsx)

# -----------------------------
# Palette colors 
pal <- wes_palette("Zissou1", 5, "discrete")

# -----------------------------
# Data + Functions to run k-means analyses
source("02_PCA-K-means\\02_models_k-means_2.R")

# -----------------------------
# > Group k-means objects and data in a list 
list_sensi <- list(
  
  # S1
  # > k = 5
  list(kmeans.obj = KM.wei_log_no_M5, 
       raw.data = data_wei_no_M5,
       data.for.kmeans = z_data_wei_log_no_M5,
       title.plot = "Excluding timing of 5 most active hours - k = 5",
       title.save = "Exclud. M5TIME - k = 5",
       sensi = "S1"),
  # > k = 4
  list(kmeans.obj = KM.wei_log_4_no_M5, 
       raw.data = data_wei_no_M5_k4,
       data.for.kmeans = z_data_wei_log_no_M5,
       title.plot = "Excluding timing of 5 most active hours - k = 4",
       title.save = "Exclud. M5TIME - k = 4",
       sensi = "S1"),
  # > k = 3
  list(kmeans.obj = KM.wei_log_3_no_M5, 
       raw.data = data_wei_no_M5_k3,
       data.for.kmeans = z_data_wei_log_no_M5,
       title.plot = "Excluding timing of 5 most active hours - k = 3",
       title.save = "Exclud. M5TIME - k = 3",
       sensi = "S1"),
  
  # S2
  # > k = 5 
  list(kmeans.obj = KM.pca.wei_log, 
       raw.data = data_wei.pca,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Based on all PC - k = 5",
       title.save = "10 PC - k = 5",
       sensi = "S2"),
  # > k = 4
  list(kmeans.obj = KM.pca.wei_log_k4, 
       raw.data = data_wei.pca_k4,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Based on all PC - k = 4",
       title.save = "10 PC - k = 4",
       sensi = "S2"),
  # > k = 3
  list(kmeans.obj = KM.pca.wei_log_k3, 
       raw.data = data_wei.pca.4_k3,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Based on all PC - k = 3",
       title.save = "10 PC - k = 3",
       sensi = "S2"),
  
  list(kmeans.obj = KM.pca.4.wei_log, 
       raw.data = data_wei.pca.4,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Based on 4 PC - k = 5",
       title.save = "4 PC - k = 5",
       sensi = "S2"),
  # > k = 4
  list(kmeans.obj = KM.pca.4.wei_log_k4, 
       raw.data = data_wei.pca.4_k4,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Based on 4 PC - k = 4",
       title.save = "4 PC - k = 4",
       sensi = "S2"),
  # > k = 3
  list(kmeans.obj = KM.pca.wei_log_k3, 
       raw.data = data_wei.pca.4_k3,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Based on all PC - k = 3",
       title.save = "4 PC - k = 3",
       sensi = "S2"),
  
  # S3
  # > k = 5
  list(kmeans.obj = KM.wei_sqrt, 
       raw.data = data_wei_sqrt,
       data.for.kmeans = z_data_wei_sqrt,
       title.plot = "Weighted daily average (skewed variables are sqrt(x)) - k = 5",
       title.save = "skewed sqrt(x) - k = 5",
       sensi = "S3"),
  # > k = 4
  list(kmeans.obj = KM.wei_sqrt_k4, 
       raw.data = data_wei_sqrt_k4,
       data.for.kmeans = z_data_wei_sqrt,
       title.plot = "Weighted daily average (skewed variables are sqrt(x)) - k = 4",
       title.save = "skewed sqrt(x) - k = 4",
       sensi = "S3"),
  # > k = 3
  list(kmeans.obj = KM.wei_sqrt_k3, 
       raw.data = data_wei_sqrt_k3,
       data.for.kmeans = z_data_wei_sqrt,
       title.plot = "Weighted daily average (skewed variables are sqrt(x)) - k = 3",
       title.save = "skewed sqrt(x) - k = 3",
       sensi = "S3"),
  
  # S4
  # > k = 5
  list(kmeans.obj = KM.wei_full_log,
       raw.data = data_wei_full,
       data.for.kmeans = z_data_wei_full_log,
       title.plot = "Full set of features - k = 5",
       title.save = "wei_full_log - k = 5",
       sensi = "S4"),
  # > k = 4
  list(kmeans.obj = KM.wei_full_log_k4,
       raw.data = data_wei_full_k4,
       data.for.kmeans = z_data_wei_full_log,
       title.plot = "Full set of features - k = 4",
       title.save = "wei_full_log - k = 4",
       sensi = "S4"),
  # > k = 3
  list(kmeans.obj = KM.wei_full_log_k3,
       raw.data = data_wei_full_k3,
       data.for.kmeans = z_data_wei_full_log,
       title.plot = "Full set of features - k = 3",
       title.save = "wei_full_log - k = 3",
       sensi = "S4"),
  
  # S5
  # > k = 5
  list(kmeans.obj = KM.WDWE_log, 
       raw.data = data_WD_WE,
       data.for.kmeans = z_data_WD_WE_log,
       title.plot = "Week and weekend variables - k = 5",
       title.save = "WD_WE - k = 5",
       sensi = "S5"),
  # > k = 4
  list(kmeans.obj = KM.WDWE_log_k4, 
       raw.data = data_WD_WE_k4,
       data.for.kmeans = z_data_WD_WE_log,
       title.plot = "Week and weekend variables - k = 4",
       title.save = "WD_WE - k = 4",
       sensi = "S5"),
  # > k = 3
  list(kmeans.obj = KM.WDWE_log_k3, 
       raw.data = data_WD_WE_k3,
       data.for.kmeans = z_data_WD_WE_log,
       title.plot = "Week and weekend variables - k = 3",
       title.save = "WD_WE - k = 3",
       sensi = "S5")
  
  
  )

# -----------------------------
# > Clustering indicators
#   - Total sum of square: measures the total variance in the data
#   - Total within-cluster sum of squares, i.e. sum(within-cluster sum of squares): 
#     measures the compactness of the clustering --> lower is better
#   - Between-cluster sum of squares, i.e. Total sum of square - Total within-cluster sum of squares: 
#     measures the difference between clusters --> higher is better

list_sensi %>% 
  map_dfr(., ~{ 
    
    data.frame(
      model = .x$title.save, 
      
      # Total sum of square
      totss = .x$kmeans.obj$totss, 
      
      # Total within-cluster sum of squares
      tot.withinss = .x$kmeans.obj$tot.withinss,
      
      # Between-cluster sum of squares
      betweenss = .x$kmeans.obj$betweenss)
    
 })

#                   model     totss tot.withinss betweenss
#  Exclud. M5TIME - k = 5  48084.00     18570.66  29513.34
#  Exclud. M5TIME - k = 4  48084.00     20908.82  27175.18
#  Exclud. M5TIME - k = 3  48084.00     24536.52  23547.48
#           10 PC - k = 5  52006.33     22480.55  29525.78
#           10 PC - k = 4  52006.33     24825.57  27180.76
#           10 PC - k = 3  52006.33     28455.30  23551.03
#            4 PC - k = 5  46591.69     17470.19  29121.50
#            4 PC - k = 4  46591.69     19686.00  26905.69
#            4 PC - k = 3  52006.33     28455.30  23551.03
#  skewed sqrt(x) - k = 5  52091.00     22840.09  29250.91
#  skewed sqrt(x) - k = 4  52091.00     25250.48  26840.52
#  skewed sqrt(x) - k = 3  52091.00     28889.49  23201.51
#    wei_full_log - k = 5  80140.00     35810.10  44329.90
#    wei_full_log - k = 4  80140.00     39331.99  40808.01
#    wei_full_log - k = 3  80140.00     44656.29  35483.71
#           WD_WE - k = 5 104182.00     52472.59  51709.41
#           WD_WE - k = 4 104182.00     56699.88  47482.12
#           WD_WE - k = 3 104182.00     62488.30  41693.70

# ------------------------------------------------------
# > Clusters indicators
#   - Within-cluster sum of square: measures the compactness of each cluster
#   - Clusters size

list_sensi %>% 
  map_dfr(., ~{ 
  
  data.frame(
    
    model = .x$title.save, 
    
    # Within-cluster sum of square
    withinss = .x$kmeans.obj$withinss, 
                              
    # Cluster size
    size = .x$kmeans.obj$size)
  
  })

#                   model  withinss size
#  Exclud. M5TIME - k = 5  3993.847  751
#  Exclud. M5TIME - k = 5  2436.391  301
#  Exclud. M5TIME - k = 5  4166.110  890
#  Exclud. M5TIME - k = 5  3726.523 1029
#  Exclud. M5TIME - k = 5  4247.791 1037

#  Exclud. M5TIME - k = 4  6417.116 1127
#  Exclud. M5TIME - k = 4  4876.584  640
#  Exclud. M5TIME - k = 4  4596.535  951
#  Exclud. M5TIME - k = 4  5018.579 1290

# Exclud. M5TIME - k = 3  9945.357 1907
# Exclud. M5TIME - k = 3  5491.045  697
# Exclud. M5TIME - k = 3  9100.119 1404

#                   model  withinss size
#           10 PC - k = 5  4640.844 1033
#           10 PC - k = 5  5052.956  884
#           10 PC - k = 5  5189.789 1044
#           10 PC - k = 5  4761.151  744
#           10 PC - k = 5  2835.811  303

#           10 PC - k = 4  5642.366  646
#           10 PC - k = 4  5627.358  966
#          10 PC - k = 4  6087.139 1286
#           10 PC - k = 4  7468.708 1110

#           10 PC - k = 3 11679.913 1906
#           10 PC - k = 3  6232.355  698
#           10 PC - k = 3 10543.036 1404

#                   model  withinss size
#            4 PC - k = 5  3632.686  734
#            4 PC - k = 5  4061.772 1032
#            4 PC - k = 5  2281.673  322
#            4 PC - k = 5  3660.795 1022
#            4 PC - k = 5  3833.265  898

#            4 PC - k = 4  4438.721  633
#            4 PC - k = 4  4275.320  948
#            4 PC - k = 4  6043.021 1134
#            4 PC - k = 4  4928.933 1293

#            4 PC - k = 3 11679.913 1906
#            4 PC - k = 3  6232.355  698
#            4 PC - k = 3 10543.036 1404

#                   model  withinss size
#  skewed sqrt(x) - k = 5  4928.861  695
#  skewed sqrt(x) - k = 5  2864.137  300
#  skewed sqrt(x) - k = 5  4960.258  848
#  skewed sqrt(x) - k = 5  4962.294 1066
#  skewed sqrt(x) - k = 5  5124.545 1099

#  skewed sqrt(x) - k = 4  6390.858 1364
#  skewed sqrt(x) - k = 4  7665.264 1076
#  skewed sqrt(x) - k = 4  5459.148  896
#  skewed sqrt(x) - k = 4  5735.213  672

#  skewed sqrt(x) - k = 3 10776.350 1329
# skewed sqrt(x) - k = 3 11657.170 1948
#  skewed sqrt(x) - k = 3  6455.972  731

#                   model  withinss size
#    wei_full_log - k = 5  8030.166 1073
#    wei_full_log - k = 5  5001.732  408
#    wei_full_log - k = 5  7743.307  991
#    wei_full_log - k = 5  7824.916  872
#    wei_full_log - k = 5  7209.975  664

#    wei_full_log - k = 4 10082.848 1285
#    wei_full_log - k = 4 11854.395 1045
#    wei_full_log - k = 4  8508.209  934
#    wei_full_log - k = 4  8886.534  744

#    wei_full_log - k = 3 16068.844 1308
#    wei_full_log - k = 3  9731.141  794
#    wei_full_log - k = 3 18856.309 1906

#                   model  withinss size
#           WD_WE - k = 5  7352.979  380
#           WD_WE - k = 5 10783.850  970
#           WD_WE - k = 5 13090.290 1125
#           WD_WE - k = 5 11742.374  886
#           WD_WE - k = 5  9503.098  647

#           WD_WE - k = 4 14181.536 1275
#           WD_WE - k = 4 17527.957 1152
#           WD_WE - k = 4 13267.622  955
#           WD_WE - k = 4 11722.767  626

#           WD_WE - k = 3 23745.032 1430
#           WD_WE - k = 3 12820.998  673
#           WD_WE - k = 3 25922.267 1905

# ------------------------------------------------------
# > Visualizing clustering from k-means, 
#   using principal components analysis 
#   (main axes are the 2 first PC)

list_sensi %>% 
  map(., ~ { 
    
    # > Plot
    plot <- fviz_cluster(object = .x$kmeans.obj, 
                         data = .x$data.for.kmeans,
                         main = .x$title.plot,
                         geom = "point",
                         #palette = "Set2",
                         ellipse = FALSE,
                         star.plot = FALSE, # Add segments from centroids to items
                         ggtheme = theme_minimal()) +
      scale_color_brewer("Cluster", palette = "Set1") +
      scale_shape_manual('Cluster', values=c(15, 16, 17, 18, 3))
    
    # > Save the plot
    ggsave(plot, 
           filename = paste0("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/", .x$sensi, "/Clusters - ", .x$title.save, ".png"), 
           device = png(),
           width = 10, height = 6, 
           dpi = 300)
    
  })


# ------------------------------------------------------
# > Difference in activity features among and between clusters
#   Note:   comp_PA_feat() function analyses the difference in PA features among and between clusters 
#           (see 00_functions.R script or type comp_PA_feat in the console)

# S1
tab_k_5_s1 <- comp_PA_feat(data = data_wei_no_M5) 
tab_k_4_s1 <- comp_PA_feat(data = data_wei_no_M5_k4)
tab_k_3_s1 <- comp_PA_feat(data = data_wei_no_M5_k3)

# S2
tab_k_5_s2.1 <- comp_PA_feat(data = data_wei.pca)
tab_k_4_s2.1 <- comp_PA_feat(data = data_wei.pca_k4)
tab_k_3_s2.1 <- comp_PA_feat(data = data_wei.pca_k3)

tab_k_5_s2.2 <- comp_PA_feat(data = data_wei.pca.4)
tab_k_4_s2.2 <- comp_PA_feat(data = data_wei.pca.4_k4)
tab_k_3_s2.2 <- comp_PA_feat(data = data_wei.pca.4_k3)

# S3
tab_k_5_s3 <- comp_PA_feat(data = data_wei_sqrt) 
tab_k_4_s3 <- comp_PA_feat(data = data_wei_sqrt_k4)
tab_k_3_s3 <- comp_PA_feat(data = data_wei_sqrt_k3)

# S4
tab_k_5_s4 <- comp_PA_feat(data = data_wei_full) 
tab_k_4_s4 <- comp_PA_feat(data = data_wei_full_k4)
tab_k_3_s4 <- comp_PA_feat(data = data_wei_full_k3)

# S5
tab_k_5_s5 <- comp_PA_feat(data = data_WD_WE) 
tab_k_4_s5 <- comp_PA_feat(data = data_WD_WE_k4)
tab_k_3_s5 <- comp_PA_feat(data = data_WD_WE_k3)

# > Save outputs
list(
  
  list(title.save = "Exclud. M5TIME - k = 5", results = tab_k_5_s1, sensi = "S1"),
  list(title.save = "Exclud. M5TIME - k = 4", results = tab_k_4_s1, sensi = "S1"),
  list(title.save = "Exclud. M5TIME - k = 3", results = tab_k_3_s1, sensi = "S1"),
  
  list(title.save = "10 PC - k = 5", results = tab_k_5_s2.1, sensi = "S2"),
  list(title.save = "10 PC - k = 4", results = tab_k_4_s2.1, sensi = "S2"),
  list(title.save = "10 PC - k = 3", results = tab_k_3_s2.1, sensi = "S2"),
  
  list(title.save = "4 PC - k = 5", results = tab_k_5_s2.2, sensi = "S2"),
  list(title.save = "4 PC - k = 4", results = tab_k_4_s2.2, sensi = "S2"),
  list(title.save = "4 PC - k = 3", results = tab_k_3_s2.2, sensi = "S2"),
  
  list(title.save = "skewed sqrt(x) - k = 5", results = tab_k_5_s3, sensi = "S3"),
  list(title.save = "skewed sqrt(x) - k = 4", results = tab_k_4_s3, sensi = "S3"),
  list(title.save = "skewed sqrt(x) - k = 3", results = tab_k_3_s3, sensi = "S3"),
  
  list(title.save = "wei_full_log - k = 5", results = tab_k_5_s4, sensi = "S4"),
  list(title.save = "wei_full_log - k = 4", results = tab_k_4_s4, sensi = "S4"),
  list(title.save = "wei_full_log - k = 3", results = tab_k_3_s4, sensi = "S4"),
  
  list(title.save = "WD_WE - k = 5", results = tab_k_5_s5, sensi = "S5"),
  list(title.save = "WD_WE - k = 4", results = tab_k_4_s5, sensi = "S5"),
  list(title.save = "WD_WE - k = 3", results = tab_k_3_s5, sensi = "S5")
  
) %>% 
  map(., ~{ 
    
    # Formatting tables
    tab.aov <- .x$results[[1]] %>% 
      left_join(tab.name, by = c("Feature" = "var") ) %>%
      dplyr::select(-Feature) %>% 
      dplyr::rename("Feature" = "varname") %>% 
      dplyr::select(Feature, starts_with("Cluster"), p.aov) %>% 
      arrange(Feature)
    
    tab.tuk <- .x$results[[2]] %>% 
      mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
      dplyr::select(-x.mean, -x.sd, -letters) %>%
      spread(key = "Group.1", value = lab)
    
    tab.comp <- tab.tuk %>% left_join(tab.aov %>% dplyr::select(Feature, p.aov), by = "Feature")
    
    # Save results in one Excel file
    xlsx::write.xlsx(x = tab.comp,
               file = paste0("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/SENSITIVITY_ANALYSES/", .x$sensi, "/CLUSTERS_DIFF_", .x$title.save, ".xlsx"))
    
  })

