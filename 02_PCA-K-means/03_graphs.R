# Project: PCA-K-means-for-PA-features
# Script name: 03_graphs.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 

# Note: to properly load data and function scripts, open the PCA-K-means-for-PA-features R.project or 
# set the working directory within the corresponding folder on your computer

# Note 2: the scripts of the analyses are provided in a separate R script 
# source("02_PCA-K-means\\02_models.R")

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
# Data + Functions to run PCA and k-means + PCA and k-means results
source("02_PCA-K-means\\02_models.R")

# -----------------------------
# Table with PA features name
source("00_DATA\\00_tab-name_PA_features.R")

# -----------------------------
# PCA 
# > Scree plot (explained variance per PC) and variable contribution in each PC
list(list(title.save = "01_wei_log", pca.object = PCA.wei_log),
     list(title.save = "01_wei_sqrt", pca.object = PCA.wei_sqrt),
     list(title.save = "02_WDWE_log", pca.object = PCA.WDWE_log),
     list(title.save = "02_WDWE_sqrt", pca.object = PCA.WDWE_sqrt),
     list(title.save = "03_wei_full_log", pca.object = PCA.wei_full_log),
     list(title.save = "03_wei_full_sqrt", pca.object = PCA.wei_full_sqrt),
     list(title.save = "04_WDWE_full_log", pca.object = PCA.WDWE_full_log),
     list(title.save = "04_WDWE_full_sqrt", pca.object = PCA.WDWE_full_sqrt)) %>% 
  map(., ~{
    
    
    # > Screw plot
    scree.plot <- fviz_eig(.x$pca.object$pca, addlabels = TRUE, ylim = c(0, 60))
    
    # > Contribution + correlation of each variable to the 5 first dimensions
    plot.contrib <- .x$pca.object$pca.var %>% 
      mutate(metric = rownames(.)) %>%
      gather(key = "pca_feature_full", value = "value", -metric) %>%
      separate(col = "pca_feature_full", into = c("pca_feature", "B", "Dim"), remove = T) %>% 
      unite("CP", B:Dim, sep = ".") %>%
      spread(key = "pca_feature", value = "value") %>%
      filter(CP %in% c("Dim.1", "Dim.2", "Dim.3", "Dim.4")) %>%
      # Add explained variance into the graph
      left_join(data.frame(CP = paste0("Dim.", 1:length(.x$pca.object$pca.eig[,2])), 
                           variance.percent = .x$pca.object$pca.eig[,2]), 
                by = "CP") %>%
      mutate(CP_lab = paste0(CP, " (", round(variance.percent, 1), "%)"),
             contrib_lab = round(contrib, 1)) %>%
      # Add good looking nales of the features
      left_join(tab.name %>% mutate(var = paste0("z_", var)), by = c("metric" = "var")) %>%
      # Plot
      ggplot(., aes(x = varname, y = contrib, color = cor, label = contrib_lab)) + 
      geom_linerange(aes(ymin = 0, ymax = contrib)) + 
      geom_point(size = 5.5) +
      geom_text(color = "black", size = 2) + 
      theme_bw() +
      theme(panel.grid = element_blank(),
            strip.background = element_blank(),
            legend.position = "bottom") +
      scale_color_gradientn(colours = c(pal[5], pal[3], "white", pal[2], "blue"),
                            breaks = c(-1, -0.5, 0, 0.5, 1),
                            name = "Correlation") +
      coord_flip() + 
      facet_grid(~ CP_lab, scales = "free") +
      labs(y = "Contribution of variables to dimensions (%)", x = "Fragmentation metrics")
    
    # > Save the plots
    ggsave(scree.plot, 
           filename = paste0("03_RESULTS//01_PCA//plots//SCREE_PLOT//", .x$title.save, ".png"), 
           device = png(),
           width = 8, height = 6,
           dpi = 300)
    
    ggsave(plot.contrib, 
           filename = paste0("03_RESULTS//01_PCA//plots//VAR_CONTRIB//", .x$title.save, ".png"), 
           device = png(),
           width = 10, height = 6,
           dpi = 300)
    
    
  })

# > Descriptive tables
list(list(title.save = "01_wei_log", pca.obj = PCA.wei_log$pca, data = data_wei %>% dplyr::select(-stno))) %>% 
  map(., ~{
    
    # Create table
    tab.desc <- desc.n.PC(pca.obj = .x$pca.obj,
                          n = 10, 
                          desc.data = .x$data) %>% 
      plyr::ldply(., data.frame, .id = "PC") %>% 
      left_join(., tab.name, by = c("metric" = "var")) %>% 
      dplyr::select(PC, varname, Below_median, Above_median, P.VALUE) %>% 
      arrange(varname) %>% 
      split(.$PC)
    
    # Save table      
    write.xlsx(x = tab.desc$PC1, 
               file = paste0("03_RESULTS//01_PCA//", .x$title.save, ".xlsx"),
               sheetName = "PC1")
    # Save results for each PC in separate sheets
    for(name in names(tab.desc)[-1])
    {
      
      write.xlsx(x = tab.desc[paste0(name)], 
                 file =  paste0("03_RESULTS//01_PCA//", .x$title.save, ".xlsx"),
                 sheetName = paste0(name),
                 append = T)
      
    }
  })


# K-means


# > Data

list_for_plot <- list(
  # Main analyses
  list(kmeans.obj = KM.wei_log, 
       raw.data = data_wei_log,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Weighted daily average (skewed variables are log(x+1))",
       title.save = "01_wei_log"),
  
  list(kmeans.obj = KM.wei_sqrt, 
       raw.data = data_wei_sqrt,
       data.for.kmeans = z_data_wei_sqrt,
       title.plot = "Weighted daily average (skewed variables are sqrt(x))",
       title.save = "01_wei_sqrt"),
  
  list(kmeans.obj = KM.WDWE_log, 
       raw.data = data_WD_WE_log,
       data.for.kmeans = z_data_WD_WE_log,
       title.plot = "Week and weekend variables (skewed variables are log(x+1))",
       title.save = "02_WD_WE_log"),
  
  list(kmeans.obj = KM.WDWE_sqrt, 
       raw.data = data_WD_WE_sqrt,
       data.for.kmeans = z_data_WD_WE_sqrt,
       title.plot = "Week and weekend variables (skewed variables are sqrt(x))",
       title.save = "02_WD_WE_sqrt"),
  
  # Sensitivity analyses
  list(kmeans.obj = KM.wei_full_log,
       raw.data = data_wei_full_log,
       data.for.kmeans = z_data_wei_full_log,
       title.plot = "Weighted daily average (skewed variables are log(x+1)) - Full set of features",
       title.save = "03_wei_full_log"),
  
  list(kmeans.obj = KM.wei_full_sqrt, 
       raw.data = data_wei_full_sqrt,
       data.for.kmeans = z_data_wei_full_sqrt,
       title.plot = "Weighted daily average (skewed variables are sqrt(x)) - Full set of features",
       title.save = "03_wei_full_sqrt"),
  
  list(kmeans.obj = KM.WDWE_full_log,
       raw.data = data_WD_WE_full_log,
       data.for.kmeans = z_data_WD_WE_full_log,
       title.plot = "Week and weekend variables (skewed variables are log(x+1)) - Full set of features",
       title.save = "04_WD_WE_full_log"),
 
   list(kmeans.obj = KM.WDWE_full_sqrt, 
        raw.data = z_data_WD_WE_full_sqrt,
        data.for.kmeans = z_data_WD_WE_full_sqrt,
        title.plot = "Week and weekend variables (skewed variables are sqrt(x)) - Full set of features",
        title.save = "04_WD_WE_full_sqrt"))

# > Vizualizing clustering from k-means, 
#   using principal components analysis 
#   (main axes are the 2 first PC)

list_for_plot %>% 
  map(., ~ { 
    
    # > Plot
    plot <- fviz_cluster(object = .x$kmeans.obj, 
                 data = .x$data.for.kmeans,
                 main = .x$title.plot,
                 geom = "point",
                 palette = "Set1",
                 ellipse = FALSE,
                 star.plot = FALSE, # Add segments from centroids to items
                 ggtheme = theme_minimal())
    
      # > Save the plot
      ggsave(plot, 
             filename = paste0("03_RESULTS//02_K-means//CLUSTERS//CLUSTERS_VIZ//", .x$title.save, ".png"), 
             device = png(),
             width = 10, height = 6, 
             dpi = 300)
    
    })

# > "Grand tour"
#   Plotting cluster for each pair of variables 

fviz_cluster(KM.wei_log, data = z_data_wei_log,
             geom = "point",
             palette = "Set1",
             ellipse = FALSE,
             star.plot = FALSE, # Add segments from centroids to items
             ggtheme = theme_minimal(),
             choose.vars = c("z_dur_day_total_IN_min_wei", "z_dur_day_total_LIG_min_wei")
)



# > Clustering indicators
list_for_plot %>% 
  map_dfr(., ~{ 
    
    data.frame(model = .x$title.save, 
               # Total sum of square: measures the total variance in the data
               totss = .x$kmeans.obj$totss, 
               # Total within-cluster sum of squares, i.e. sum(within-cluster sum of squares): measures the compactness of the clustering --> lower is better
               tot.withinss = .x$kmeans.obj$tot.withinss,
               # Between-cluster sum of squares, i.e. Total sum of square - Total within-cluster sum of squares: measures the difference between clusters --> higher is better
               betweenss = .x$kmeans.obj$betweenss)
    
    })

# > Clusters indicators
list_for_plot %>% 
  map_dfr(., ~{ 
    
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
    
  }) %>% 
  split(., .$Indicator)


# > Centers coordinates
list_for_plot %>% 
  map(., ~{ 
    
    centers_info <- cbind(model = .x$title.save, 
                          Cluster = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
                          data.frame(.x$kmeans.obj$centers)) %>% 
      gather(key = "var", value = "value", -Cluster) %>% 
      # Merge with variable names to allow for merging dataframe from different analyses
      left_join(tab.name %>% mutate(var = paste0("z_", var)), by = "var") %>% 
      dplyr::select(-var) 
    
  })

# > Difference in activity features among and between clusters
#   comp_PA_feat() function analyseq the difference in PA features among and between clusters
#   see 00_functions.R script

# We want the difference in *non-transformed* PA features 
wei_log_k_means_comp    <- comp_PA_feat(data = data_wei %>% left_join(data_wei_log %>% dplyr::select(stno, km.5), by = "stno"))
wei_sqrt_k_means_comp   <- comp_PA_feat(data = data_wei %>% left_join(data_wei_sqrt %>% dplyr::select(stno, km.5), by = "stno"))
WD_WE_log_k_means_comp  <- comp_PA_feat(data = data_WD_WE %>% left_join(data_WD_WE_log %>% dplyr::select(stno, km.5), by = "stno"))
WD_WE_sqrt_k_means_comp <- comp_PA_feat(data = data_WD_WE %>% left_join(data_WD_WE_sqrt %>% dplyr::select(stno, km.5), by = "stno"))

wei_full_log_k_means_comp    <- comp_PA_feat(data = data_wei_full %>% left_join(data_wei_full_log %>% dplyr::select(stno, km.5), by = "stno"))
wei_full_sqrt_k_means_comp   <- comp_PA_feat(data = data_wei_full %>% left_join(data_wei_full_sqrt %>% dplyr::select(stno, km.5), by = "stno"))
WD_WE_full_log_k_means_comp  <- comp_PA_feat(data = data_WD_WE_full %>% left_join(data_WD_WE_full_log %>% dplyr::select(stno, km.5), by = "stno"))
WD_WE_full_sqrt_k_means_comp <- comp_PA_feat(data = data_WD_WE_full %>% left_join(data_WD_WE_full_sqrt %>% dplyr::select(stno, km.5), by = "stno"))

# > Results from variance analyses (difference among clusters)
list(
  
  list(title.save = "01_wei_log",         results = wei_log_k_means_comp[[1]]),
  list(title.save = "01_wei_sqrt",        results = wei_sqrt_k_means_comp[[1]]),
  list(title.save = "02_WD_WE_log",       results = WD_WE_log_k_means_comp[[1]]),
  list(title.save = "02_WD_WE_sqrt",      results = WD_WE_sqrt_k_means_comp[[1]]),
  
  list(title.save = "03_wei_full_log",    results = wei_full_log_k_means_comp[[1]]),
  list(title.save = "03_wei_full_sqrt",   results = wei_full_sqrt_k_means_comp[[1]]),
  list(title.save = "04_WD_WE_full_log",  results = WD_WE_full_log_k_means_comp[[1]]),
  list(title.save = "04_WD_WE_full_sqrt", results = WD_WE_full_sqrt_k_means_comp[[1]])
  
) %>% 
  map(., ~{ 
    
    # Formatting tables
    tab.save <- .x$results %>% 
      left_join(tab.name, by = c("Feature" = "var") ) %>%
      dplyr::select(-Feature) %>% 
      dplyr::rename("Feature" = "varname") %>% 
      dplyr::select(Feature, starts_with("Cluster"), p.aov) %>% 
      arrange(Feature)
    
    # Save results in one Excel file
    write.xlsx(x = tab.save,
               file = "03_RESULTS//02_K-means//CLUSTERS//CLUSTERS_DIFF_AMONG_GROUPS.xlsx", 
               sheetName = paste0(.x$title.save),
               append = TRUE)
    
  })


# > Results from multiple groups comparison (difference between clusters)
list(
  
  list(title.save = "01_wei_log",         results = wei_log_k_means_comp[[2]],         raw.data = data_wei %>% left_join(data_wei_log %>% dplyr::select(stno, km.5), by = "stno")),
  list(title.save = "01_wei_sqrt",        results = wei_sqrt_k_means_comp[[2]],        raw.data = data_wei %>% left_join(data_wei_sqrt %>% dplyr::select(stno, km.5), by = "stno")),
  list(title.save = "02_WD_WE_log",       results = WD_WE_log_k_means_comp[[2]],       raw.data = data_WD_WE %>% left_join(data_WD_WE_log %>% dplyr::select(stno, km.5), by = "stno")),
  list(title.save = "02_WD_WE_sqrt",      results = WD_WE_sqrt_k_means_comp[[2]],      raw.data = data_WD_WE %>% left_join(data_WD_WE_sqrt %>% dplyr::select(stno, km.5), by = "stno")),
  
  list(title.save = "03_wei_full_log",    results = wei_full_log_k_means_comp[[2]],    raw.data = data_wei_full %>% left_join(data_wei_full_log %>% dplyr::select(stno, km.5), by = "stno")),
  list(title.save = "03_wei_full_sqrt",   results = wei_full_sqrt_k_means_comp[[2]],   raw.data = data_wei_full %>% left_join(data_wei_full_sqrt %>% dplyr::select(stno, km.5), by = "stno")),
  list(title.save = "04_WD_WE_full_log",  results = WD_WE_full_log_k_means_comp[[2]],  raw.data = data_WD_WE_full %>% left_join(data_WD_WE_full_log %>% dplyr::select(stno, km.5), by = "stno")),
  list(title.save = "04_WD_WE_full_sqrt", results = WD_WE_full_sqrt_k_means_comp[[2]], raw.data = data_WD_WE_full %>% left_join(data_WD_WE_full_sqrt %>% dplyr::select(stno, km.5), by = "stno"))
  
) %>%
map(., ~ { 
  
  # > Variable distribution between clusters
  plot <- .x$raw.data %>% 
    dplyr::select(-km.5) %>% 
    gather(key = "Feature", value = "value", -stno) %>% 
    
    left_join(tab.name, by = c("Feature" = "var") ) %>%
    dplyr::select(-Feature) %>% 
    dplyr::rename("Feature" = "varname") %>%
    
    group_by(Feature) %>% 
    summarise(med = median(value),
              IQR1 = quantile(value, 0.25),
              IQR3 = quantile(value, 0.75)) %>% 
    ggplot(.) + 
    geom_hline(aes(yintercept = med), linetype = 2, col = "black") +
    geom_hline(aes(yintercept = IQR1), linetype = 2, col = "darkgrey") +
    geom_hline(aes(yintercept = IQR3), linetype = 2, col = "darkgrey") +
    geom_jitter(data = .x$raw.data %>% 
                  dplyr::select(-stno) %>% 
                  gather(key = "Feature", value = "value", -km.5) %>% 
                  left_join(tab.name, by = c("Feature" = "var") ) %>% 
                  dplyr::select(-Feature) %>% 
                  dplyr::rename("Feature" = "varname"),
                aes(x = km.5, y = value, col = as.factor(km.5)),
                alpha = 0.5, size = 0.1, pch = 1, width = 0.25) + 
    geom_point(data = .x$results,
               aes(x = Group.1, y = x.mean)) +
    geom_linerange(data = .x$results,
                   aes(x = Group.1, ymin = x.mean-x.sd, ymax = x.mean+x.sd)) +
    geom_text(data = .x$results,
              aes(x = Group.1, y = x.mean+x.sd*2, label = letters),
              size = 3) + 
    facet_wrap(. ~ Feature, scales = "free") + 
    scale_color_manual(values = pal) + 
    scale_fill_manual(values = pal) +
    theme_bw() + 
    theme(legend.position = "none",
          strip.background = element_blank(), 
          axis.title = element_blank()) + 
    coord_flip()
  
  # > Save the plot
  if(.x$title.save %in% c("01_wei_log", "01_wei_sqrt"))
  {
    
    ggsave(plot, 
           filename = paste0("03_RESULTS//02_K-means//CLUSTERS//DIFF_BETWEEN_CLUSTERS//", .x$title.save, ".png"), 
           device = png(),
           width = 10, height = 6, 
           dpi = 300)
    
  }
  
  if(.x$title.save %in% c("02_WD_WE_log", "02_WD_WE_sqrt"))
  {
    
    ggsave(plot, 
           filename = paste0("03_RESULTS//02_K-means//CLUSTERS//DIFF_BETWEEN_CLUSTERS//", .x$title.save, ".png"), 
           device = png(),
           width = 14, height = 7, 
           dpi = 300)
    
  }
  
  if(.x$title.save %in% c("04_WD_WE_full_log", "04_WD_WE_full_sqrt"))
  {
    
    ggsave(plot, 
           filename = paste0("03_RESULTS//02_K-means//CLUSTERS//DIFF_BETWEEN_CLUSTERS//", .x$title.save, ".png"), 
           device = png(),
           width = 14, height = 14, 
           dpi = 300)
    
  }
  
  
  })

