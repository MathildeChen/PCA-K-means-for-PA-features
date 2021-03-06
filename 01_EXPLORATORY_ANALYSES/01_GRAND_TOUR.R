# Script name: 01_GRAND_TOUR.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 

# -----------------------------
# Packages
library(tidyverse)
library(wesanderson)
library(ggplot2)
library(cowplot)
library(scatterplot3d)
library(tourr)
library(GGally)

# -----------------------------
# Palette colors 
pal <- wes_palette("Zissou1", 5, "discrete")

# -----------------------------
# Data 

# > All PA metrics (with number of bouts of different lengths)
path_to_data <- "E:/PC_FIXE/Analysis/02_ARTICLE_2/00_DATA"
#   Standardized metrics
load(file.path(path_to_data, "00_z_data_full_PCA_k_means_july.rda"))   # standardized metrics, full set of PA metrics (including nb of bouts of different length)

# -----------------------------
# 2D plots 

# Plotting pairs of variables
for(v in names(z_data_full$wei)){
  
  med_v <- median(z_data_full$wei[,grepl(v, names(z_data_full$wei))])
  q25_v <- round(quantile(z_data_full$wei[,grepl(v, names(z_data_full$wei))], 0.25), 2)
  q75_v <- round(quantile(z_data_full$wei[,grepl(v, names(z_data_full$wei))], 0.75), 2)
  
  var_v <- names(z_data_full$wei)[!grepl(v, names(z_data_full$wei))]
  
  # Compute correlation between the selected variable and other variables
  tab_cor <- z_data_full$wei %>% 
    gather(key = "variable", value = "value", -!!as.name(v)) %>% 
    group_by(variable) %>% 
    summarise(cor = round(cor(value, !!as.name(v)), 2),
              med = round(median(value), 2),
              q25 = round(quantile(value, 0.25), 2),
              q75 = round(quantile(value, 0.75), 2))
  
  # Scatterplots
  p <- ggplot() + 
    # Cor (X, X)
    geom_text(data = tab_cor,
              aes(x = Inf, y = Inf, label = paste0("Corr: ", cor)),
              hjust = 1.25, vjust = 1.25, size = 3) +
    # X: median, 25th and 75th percentiles (the select variable)
    geom_vline(xintercept = med_v,
               col = pal[5], lty = 2) + 
    geom_vline(xintercept = c(q25_v, q75_v),
               col = pal[3], lty = 2) +
    # Y: median, 25th and 75th percentiles
    geom_hline(data = tab_cor,
               aes(yintercept = med),
               col = pal[5], lty = 2) + 
    geom_hline(data = tab_cor,
               aes(yintercept = q25),
               col = pal[3], lty = 2) +
    geom_hline(data = tab_cor,
               aes(yintercept = q75),
               col = pal[3], lty = 2) +
    # Scatter plots Y ~ X
    geom_point(data = z_data_full$wei %>% 
                 gather(key = "variable", value = "value", -!!as.name(v)),
               aes(x = !!as.name(v), y = value),
               color = pal[1], size = 0.1) +
    theme_bw() + 
    theme(strip.background = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank()) + 
    facet_wrap(. ~ variable) + 
    ggtitle(label = paste0(v))
  
  # Save plot
  ggsave(filename = paste0("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\03_RESULTS\\00_DESCRIPTIVE_ANALYSES\\2D.plots\\2D.plots.", v, "_july.png"), 
         plot = p, width = 8, height = 8)

}

# -----------------------------
# 3D plots

# Grand tour
animate(data_s,
        tour_path = grand_tour(),
        display = display_xy()
)

