# Project: PCA-K-means-for-PA-features
# Script name: 03_graphs_PCA.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Plots and tables related to PCA on movement behavior characteristics

# Note: to properly load data and function scripts, open the PCA-K-means-for-PA-features R.project or 
# set the working directory within the corresponding folder on your computer

# Note 2: the scripts of the analyses are provided in a separate R script 
# source("02_PCA-K-means\\02-1_models_PCA.R")

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
source("02_PCA-K-means\\02-1_models_PCA.R")

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
           filename = paste0("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/01_PCA/plots/SCREE_PLOT/", .x$title.save, ".png"), 
           device = png(),
           width = 8, height = 6,
           dpi = 300)
    
    ggsave(plot.contrib, 
           filename = paste0("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/01_PCA/plots/VAR_CONTRIB/", .x$title.save, ".png"), 
           device = png(),
           width = 10, height = 6,
           dpi = 300)
    
    
  })

# > Descriptive tables
list(list(title.save = "01_wei_log",        pca.obj = PCA.wei_log$pca,        data = data_wei %>% dplyr::select(-stno)),
     list(title.save = "01_wei_sqrt",       pca.obj = PCA.wei_sqrt$pca,       data = data_wei %>% dplyr::select(-stno)),
     list(title.save = "02_WDWE_log",       pca.obj = PCA.WDWE_log$pca,       data = data_wei %>% dplyr::select(-stno)),
     list(title.save = "02_WDWE_sqrt",      pca.obj = PCA.WDWE_sqrt$pca,      data = data_wei %>% dplyr::select(-stno)),
     list(title.save = "03_wei_full_log",   pca.obj = PCA.wei_full_log$pca,   data = data_wei %>% dplyr::select(-stno)),
     list(title.save = "03_wei_full_sqrt",  pca.obj = PCA.wei_full_sqrt$pca,  data = data_wei %>% dplyr::select(-stno)),
     list(title.save = "04_WDWE_full_log",  pca.obj = PCA.WDWE_full_log$pca,  data = data_wei %>% dplyr::select(-stno)),
     list(title.save = "04_WDWE_full_sqrt", pca.obj = PCA.WDWE_full_sqrt$pca, data = data_wei %>% dplyr::select(-stno))) %>% 
  
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
               file = paste0("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/01_PCA/tables/", .x$title.save, ".xlsx"),
               sheetName = "PC1")
    # Save results for each PC in separate sheets
    for(name in names(tab.desc)[-1])
    {
      
      write.xlsx(x = tab.desc[paste0(name)], 
                 file =  paste0("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/01_PCA/tables/", .x$title.save, ".xlsx"),
                 sheetName = paste0(name),
                 append = T)
      
    }
  })

