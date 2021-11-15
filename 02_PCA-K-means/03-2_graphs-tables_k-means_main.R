# Project: PCA-K-means-for-PA-features
# Script name: 03-2_graphs_tables_k-means.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Tables and plots for main analyses (clustering with k-means)

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
list_main <- list(
  # > k = 5
  list(kmeans.obj = KM.wei_log, 
       raw.data = data_wei,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Weighted daily average (skewed variables are log(x+1))",
       title.save = "Main - k = 5"),
  # > k = 4
  list(kmeans.obj = KM.wei_log_k4, 
       raw.data = data_wei_k4,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Weighted daily average (skewed variables are log(x+1))",
       title.save = "Main - k = 4"),
  # > k = 3
  list(kmeans.obj = KM.wei_log_k3, 
       raw.data = data_wei_k3,
       data.for.kmeans = z_data_wei_log,
       title.plot = "Weighted daily average (skewed variables are log(x+1))",
       title.save = "Main - k = 3"))

# > Clustering indicators
#   - Total sum of square: measures the total variance in the data
#   - Total within-cluster sum of squares, i.e. sum(within-cluster sum of squares): 
#     measures the compactness of the clustering --> lower is better
#   - Between-cluster sum of squares, i.e. Total sum of square - Total within-cluster sum of squares: 
#     measures the difference between clusters --> higher is better

list_main %>% 
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

#         model totss tot.withinss betweenss
# Main - k = 5  52091     22564.41  29526.59
# Main - k = 4  52091     24909.46  27181.54
# Main - k = 3  52091     28539.43  23551.57

# --> Lowest tot.withinss & highest betweenss for analysis with k=5

# ------------------------------------------------------
# > Clusters indicators
#   - Within-cluster sum of square: measures the compactness of each cluster
#   - Clusters size

list_main %>% 
  map_dfr(., ~{ 
  
  data.frame(
    
    model = .x$title.save, 
    
    # Within-cluster sum of square
    withinss = .x$kmeans.obj$withinss, 
                              
    # Cluster size
    size = .x$kmeans.obj$size)
  
  })

#           model  withinss size
#    Main - k = 5  4638.383  730
#    Main - k = 5  5214.728 1046
#    Main - k = 5  5197.478  893
#    Main - k = 5  2851.674  303
#    Main - k = 5  4662.147 1036

#           model  withinss size
#    Main - k = 4  6103.204 1283
#    Main - k = 4  7539.185 1116
#    Main - k = 4  5671.684  968
#    Main - k = 4  5595.384  641

#           model  withinss  size
#    Main - k = 3  10625.347 1408
#    Main - k = 3  6255.323  698
#    Main - k = 3  11658.763 1902

# ------------------------------------------------------
# > Visualizing clustering from k-means, 
#   using principal components analysis 
#   (main axes are the 2 first PC)

list_main %>% 
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
           filename = paste0("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/MAIN_ANALYSES/Clusters - ", .x$title.save, ".png"), 
           device = png(),
           width = 10, height = 6, 
           dpi = 300)
    
  })


# ------------------------------------------------------
# > Centers coordinates:
#   movement behavior characteristics of the center of each cluster
#   Note: not directly interpretable (standardized variables)
list_main %>% 
  map(., ~{ 
    
    centers <- data.frame(.x$kmeans.obj$centers)
    
    centers_info <- centers %>% 
      mutate(Cluster = paste("Cluster", 1:nrow(centers)))  %>% 
      gather(key = "var", value = "value", -Cluster) %>% 
      left_join(tab.name %>% mutate(var = paste0("z_", var)), by = "var") %>% 
      dplyr::select(-var) %>%
      mutate(varname = substr(varname, 1, nchar(varname)-11)) %>%
      spread(key = "varname", value = value)
    
  })

# k = 5
#   Cluster Acceleration* (mg) Duration LIPA (min/day) Duration MVPA* (min/day) Duration SB (min/day) Intensity gradient intercept Intensity gradient slope Mean duration of LIPA bouts Mean duration of MVPA bouts* Mean duration of SB bouts* Most active 5hrs timing Number of LIPA bouts Number of MVPA bouts  Number of SB bouts
# Cluster 1          1.2618921               1.2575605               0.96971844           -1.31885152                   -0.5855768                0.7950846                   0.6490346                    0.2784831                -1.05354223             0.079379415            1.2310110           1.38991138           0.7776720
# Cluster 2         -0.8017738              -0.6969307              -0.58718896            0.72649910                    0.4665838               -0.5665559                  -0.3335718                   -0.2809916                 0.59385573            -0.090482577           -0.7055181          -0.72012680          -0.5230682 
# Cluster 3          0.4243577              -0.2991394               0.66542599           -0.06078883                   -1.0098959                0.9167090                  -0.3581097                    0.9684747                 0.08078803             0.070525624           -0.1323797           0.24666160          -0.2404448  
# Cluster 4         -1.9388370              -1.6448284              -2.14929818            1.57059708                    1.5203234               -1.8964736                  -0.9795828                   -1.2971101                 2.06607230             0.007938675           -1.8311398          -1.38092608          -1.6954968
# Cluster 5          0.1216123               0.5564524              -0.03540817           -0.21116113                    0.3673779               -0.2237297                   0.4746394                   -0.3679526                -0.53113019            -0.027690153            0.4945795          -0.06103367           0.6832833

# k = 4
#   Cluster Acceleration* (mg) Duration LIPA (min/day) Duration MVPA* (min/day) Duration SB (min/day) Intensity gradient intercept Intensity gradient slope  Mean duration of LIPA bouts Mean duration of MVPA bouts* Mean duration of SB bouts* Most active 5hrs timing Number of LIPA bouts Number of MVPA bouts  Number of SB bouts
# Cluster 1         -0.2402737               0.1146944               -0.3256486             0.1627997                    0.5188152               -0.4456353                    0.2473422                   -0.4493241                 -0.1666811            -0.065235736            0.0687297         -0.381782110           0.3018837
# Cluster 2          1.0649286               1.0746011                0.8272513            -1.0695749                   -0.4892573                0.6762959                    0.5673951                    0.2337863                 -0.9236244             0.033190522            1.0615729          1.105198451           0.7423578
# Cluster 3          0.1119848              -0.5096459                0.4720911             0.1713737                   -0.8449951                0.7143181                   -0.4759954                    0.8881937                  0.2944385             0.049734759           -0.3627618         -0.005224509          -0.4113373
# Cluster 4         -1.5422627              -1.3308431               -1.5013876             1.2775098                    1.0894328               -1.3642059                   -0.7641021                   -0.8489769                  1.4970363            -0.002318906           -1.4379754         -1.152133696          -1.2755281

# k = 3
#   Cluster Acceleration* (mg) Duration LIPA (min/day) Duration MVPA* (min/day) Duration SB (min/day) Intensity gradient intercept Intensity gradient slope  Mean duration of LIPA bouts Mean duration of MVPA bouts* Mean duration of SB bouts* Most active 5hrs timing Number of LIPA bouts Number of MVPA bouts  Number of SB bouts
# Cluster 1          0.9851480               0.9283622               0.77266573            -0.9482431                  -0.50831574              0.664802018                    0.4925588                    0.2796653                -0.82408292             0.044458670            0.9239982            0.9573286           0.6612257
# Cluster 2         -1.4915202              -1.2638778              -1.48619667             1.2376016                   1.08410120             -1.344682908                   -0.7039078                   -0.8379558                 1.40616422             0.001040399           -1.3706744           -1.1521490          -1.1881033
# Cluster 3         -0.1819176              -0.2234213              -0.02657627             0.2477815                  -0.02155314              0.001339342                   -0.1063066                    0.1004860                 0.09400953            -0.033293379           -0.1809983           -0.2858668          -0.0534751

# ------------------------------------------------------
# > Difference in activity features among and between clusters
#   Note:   comp_PA_feat() function analyses the difference in PA features among and between clusters 
#           (see 00_functions.R script or type comp_PA_feat in the console)

tab_k_5 <- comp_PA_feat(data = data_wei)
tab_k_4 <- comp_PA_feat(data = data_wei_k4)
tab_k_3 <- comp_PA_feat(data = data_wei_k3)


# > Tables with p for ANOVA (difference among clusters)
tab_k_5[[1]] # all p < 0.05
tab_k_4[[1]] # all p < 0.05
tab_k_3[[1]] # all p < 0.05, except for timing of most 5 active hours (p=0.087)


# > Tables with PA metrics characteristic of each cluster (difference between clusters)
# k = 5
tab_k_5[[2]] %>% 
  mutate(Feature = substr(Feature, 1, nchar(Feature)-11)) %>%
  mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
  dplyr::select(-x.mean, -x.sd, -letters) %>%
  spread(key = "Group.1", value = lab)

#                      Feature                1                2                3                4                5
#             Acceleration (mg)   44.9 (7.6) (e)   35.2 (7.6) (d)   31.7 (3.1) (c)   24.1 (2.6) (b)   17.2 (2.8) (a)
#       Duration LIPA (min/day) 297.2 (49.3) (e) 189.6 (33.2) (c) 248.8 (35.4) (d) 162.1 (30.8) (b)    96.6 (33) (a)
#       Duration MVPA (min/day) 103.6 (35.8) (e)  79.7 (29.1) (d)  44.2 (16.5) (c)  28.4 (13.1) (b)    8.2 (6.6) (a)
#         Duration SB (min/day) 585.8 (60.7) (a)   711.8 (62) (c) 696.7 (54.1) (b) 790.7 (56.4) (d) 875.2 (69.2) (e)
#  Intensity gradient intercept     12 (0.4) (b)   11.7 (0.5) (a)   12.6 (0.3) (c)   12.7 (0.4) (d)   13.4 (0.5) (e)
#      Intensity gradient slope   -1.9 (0.1) (d)   -1.9 (0.1) (e)   -2.1 (0.1) (c)   -2.2 (0.1) (b)   -2.5 (0.2) (a)
#   Mean duration of LIPA bouts    2.6 (0.4) (d)    2.3 (0.3) (b)    2.6 (0.3) (c)    2.3 (0.3) (b)      2 (0.3) (a)
#   Mean duration of MVPA bouts    2.5 (0.6) (d)    3.2 (1.1) (e)    1.9 (0.4) (b)      2 (0.6) (c)    1.4 (0.9) (a)
#     Mean duration of SB bouts    7.3 (1.3) (a)   11.2 (2.2) (c)    8.9 (1.4) (b)   13.5 (2.8) (d)  24.4 (12.7) (e)
#       Most active 5hrs timing   10.4 (1.7) (b)   10.3 (1.7) (b)  10.2 (1.5) (ab)   10.1 (1.5) (a)  10.2 (1.9) (ab)
#          Number of LIPA bouts 112.6 (12.6) (e)    83.1 (11) (c)  96.7 (10.9) (d)  70.7 (10.4) (b)  46.4 (13.4) (a)
#          Number of MVPA bouts  42.1 (11.6) (e)     26.6 (9) (d)   22.4 (7.3) (c)   13.5 (5.5) (b)    4.5 (3.1) (a)
#            Number of SB bouts  84.3 (12.2) (e)     68 (9.6) (c)  82.8 (11.3) (d)  63.4 (10.2) (b)  44.7 (12.9) (a)
# Notes: 
#(a), (b), (c), (d), (e): Tukey tests for multiple means comparison: PA features are different in clusters with different letters. 
#                         The order of the letters indicate cluster position compared to other clusters (a < b < c < d < e).

# >> k = 4
tab_k_4[[2]] %>% 
  mutate(Feature = substr(Feature, 1, nchar(Feature)-11)) %>%
  mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
  dplyr::select(-x.mean, -x.sd, -letters) %>%
  spread(key = "Group.1", value = lab)

#                       Feature                1                2                3                4
#            Acceleration (mg)    42.5 (8.1) (d)   32.1 (6.6) (c)   28.5 (3.3) (b)   19.4 (3.1) (a)
#       Duration LIPA (min/day) 284.6 (48.9) (d) 175.1 (32.8) (b) 218.2 (37.4) (c) 118.3 (35.1) (a)
#      Duration MVPA (min/day)   93.2 (36.7) (d)  68.5 (28.1) (c)  34.9 (14.4) (b)    15 (10.7) (a)
#         Duration SB (min/day) 610.8 (65.7) (a) 735.1 (64.8) (b) 734.2 (58.7) (b) 845.8 (68.2) (c)
#  Intensity gradient intercept   12.1 (0.5) (b)   11.8 (0.5) (a)   12.7 (0.3) (c)   13.1 (0.6) (d)
#      Intensity gradient slope   -1.9 (0.1) (c)   -1.9 (0.1) (c)   -2.2 (0.1) (b)   -2.4 (0.2) (a)
#   Mean duration of LIPA bouts    2.6 (0.4) (d)    2.2 (0.3) (b)    2.5 (0.3) (c)    2.1 (0.3) (a)
#  Mean duration of MVPA bouts     2.4 (0.6) (c)    3.1 (1.1) (d)    1.9 (0.4) (b)    1.7 (0.8) (a)
#    Mean duration of SB bouts     7.7 (1.4) (a)   12.1 (2.5) (c)   10.2 (1.9) (b)    19.7 (10) (d)
#       Most active 5hrs timing  10.3 (1.7) (ab)   10.3 (1.6) (b)   10.1 (1.5) (a)  10.2 (1.7) (ab)
#          Number of LIPA bouts 108.9 (12.7) (d)  78.1 (11.3) (b)  87.5 (11.8) (c)  54.9 (13.7) (a)
#          Number of MVPA bouts  38.2 (11.7) (d)   23.2 (8.5) (c)   18.1 (6.7) (b)    7.6 (4.8) (a)
#            Number of SB bouts  83.7 (12.3) (d)   65.2 (9.7) (b)  76.7 (11.4) (c)  51.4 (12.8) (a)
# Notes: 
#(a), (b), (c), (d): Tukey tests for multiple means comparison: PA features are different in clusters with different letters. 
#                    The order of the letters indicate cluster position compared to other clusters (a < b < c < d).

# >> k = 3
tab_k_3[[2]] %>% 
  mutate(Feature = substr(Feature, 1, nchar(Feature)-11)) %>%
  mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
  dplyr::select(-x.mean, -x.sd, -letters) %>%
  spread(key = "Group.1", value = lab)

#                      Feature                1                2                3
#            Acceleration  (mg)     41.5 (8) (c)   29.1 (3.9) (b)   19.7 (3.1) (a)
#       Duration LIPA (min/day) 274.5 (52.1) (c) 194.9 (37.8) (b) 122.9 (38.5) (a)
#      Duration MVPA (min/day)   90.1 (37.7) (c)    46 (21.4) (b)  14.9 (10.4) (a)
#         Duration SB (min/day) 622.9 (66.4) (a) 742.7 (56.6) (b) 841.8 (68.9) (c)
#  Intensity gradient intercept     12 (0.6) (a)   12.4 (0.5) (b)   13.1 (0.6) (c)
#      Intensity gradient slope   -1.9 (0.2) (c)   -2.1 (0.2) (b)   -2.4 (0.2) (a)
#   Mean duration of LIPA bouts    2.6 (0.4) (c)    2.4 (0.3) (b)    2.2 (0.3) (a)
#  Mean duration of MVPA bouts     2.5 (0.8) (c)    2.4 (0.9) (b)    1.7 (0.9) (a)
#    Mean duration of SB bouts       8 (1.5) (a)   11.2 (2.3) (b)   19.1 (9.9) (c)
#       Most active 5hrs timing   10.3 (1.7) (a)   10.2 (1.6) (a)   10.2 (1.7) (a)
#          Number of LIPA bouts   106 (13.6) (c)  82.1 (11.7) (b)  56.3 (14.5) (a)
#          Number of MVPA bouts  36.2 (11.8) (c)   19.4 (6.9) (b)    7.6 (4.5) (a)
#            Number of SB bouts  82.4 (12.8) (c)    71 (11.6) (b)  52.8 (13.6) (a)
# Notes: 
#(a), (b), (c): Tukey tests for multiple means comparison: PA features are different in clusters with different letters. 
#               The order of the letters indicate cluster position compared to other clusters (a < b < c).


# > Save outputs
list(
  
  list(title.save = "Main - k = 5", results = tab_k_5),
  list(title.save = "Main - k = 4", results = tab_k_4),
  list(title.save = "Main - k = 3", results = tab_k_3)
  
) %>% 
  map(., ~{ 
    
    # Formatting tables
    tab.aov <- .x$results[[1]] %>% 
      left_join(tab.name, by = c("Feature" = "var") ) %>%
      dplyr::select(-Feature) %>% 
      dplyr::rename("Feature" = "varname") %>% 
      dplyr::select(Feature, starts_with("Cluster"), p.aov) %>% 
      mutate(Feature = substr(Feature, 1, nchar(Feature)-11)) %>%
      arrange(Feature)
    
    tab.tuk <- .x$results[[2]] %>% 
      mutate(Feature = substr(Feature, 1, nchar(Feature)-11)) %>%
      mutate(lab = paste0(round(x.mean, 1), " (", round(x.sd, 1), ") (", letters, ")")) %>% 
      dplyr::select(-x.mean, -x.sd, -letters) %>%
      spread(key = "Group.1", value = lab)
    
    tab.comp <- tab.tuk %>% left_join(tab.aov %>% dplyr::select(Feature, p.aov), by = "Feature")
    
    # Save results in one Excel file
    xlsx::write.xlsx(x = tab.comp,
               file = "E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/MAIN_ANALYSES/CLUSTERS_DIFF.xlsx", 
               sheetName = paste0(.x$title.save))
    
  })

# ------------------------------------------------------
# > Plots representing multiple groups comparison (difference between clusters)
list(
  list(title.save = "Main - k = 5", results = tab_k_5[[2]] %>% mutate(Feature = substr(Feature, 1, nchar(Feature)-11)), raw.data = data_wei),
  list(title.save = "Main - k = 4", results = tab_k_4[[2]] %>% mutate(Feature = substr(Feature, 1, nchar(Feature)-11)), raw.data = data_wei_k4),
  list(title.save = "Main - k = 3", results = tab_k_3[[2]] %>% mutate(Feature = substr(Feature, 1, nchar(Feature)-11)), raw.data = data_wei_k3)
  ) %>%
  
map(., ~ { 
  
  # > Variable distribution between clusters
  plot <- 
    .x$raw.data %>% 
    dplyr::select(-km) %>% 
    gather(key = "Feature", value = "value", -stno)  %>%  
    left_join(tab.name, by = c("Feature" = "var")) %>%
    dplyr::select(-Feature) %>% 
    dplyr::rename("Feature" = "varname") %>%
    mutate(Feature = substr(Feature, 1, nchar(Feature)-11)) %>%
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
                  gather(key = "Feature", value = "value", -km) %>% 
                  left_join(tab.name, by = c("Feature" = "var") ) %>% 
                  dplyr::select(-Feature) %>% 
                  dplyr::rename("Feature" = "varname") %>%
                  mutate(Feature = substr(Feature, 1, nchar(Feature)-11)),
                aes(x = km, y = value, col = as.factor(km)),
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
  ggsave(plot, 
           filename = paste0("E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/MAIN_ANALYSES/DIFF_BETWEEN_CLUSTERS - ", .x$title.save, ".png"), 
           device = png(),
           width = 10, height = 6, 
           dpi = 300)
  
  })


# ------------------------------------------------------
# > Mean z-scores in each clusters 

col <- c("#6fb96f",
        "#d85e78",
        "#4dc0b1",
        "#b65336",
        "#5ca8d9",
        "#d39446",
        "#547dc5",
        "#b4aa4e",
        "#a24a56",
        "#368359",
        "#e49276",
        "#637a2e",
        "#90672e")

plots <- data_wei %>%
  mutate(dur_day_total_IN_min_wei = -dur_day_total_IN_min_wei,
         FRAG_mean_dur_IN_day_wei = -FRAG_mean_dur_IN_day_wei,
         ig_intercept_wei = -ig_intercept_wei) %>%
  mutate_each(funs(scale), -stno, -km) %>%
  dplyr::rename("km_5" = "km") %>% 
  left_join(data_wei_k4 %>% dplyr::select(stno, "km_4" = "km"), by = "stno") %>% 
  left_join(data_wei_k3 %>% dplyr::select(stno, "km_3" = "km"), by = "stno") %>% 
  gather(key = "clustering", value = "k", starts_with("km_")) %>%
  gather(key = "feature", value = "value", -stno, -clustering, -k, -stno) %>% 
  left_join(tab.name, by = c("feature"= "var")) %>% 
  dplyr::select(-feature) %>% 
  dplyr::select(-stno) %>%
  group_by(clustering, k, varname) %>% 
  summarise(mean_value = mean(value)) %>% 
  mutate(varname = substr(varname, 1, nchar(varname)-11)) %>% 
  #mutate(k = paste0("Cluster ", k)) %>% 
  split(.$clustering) %>% 
  map(.f = ~{ 
    
    data_for_plot <- .x %>%
      mutate(varname = factor(varname, levels = rev(c('Acceleration (mg)', 
                                                      'Duration SB (min/day)', 'Duration LIPA (min/day)', 'Duration MVPA (min/day)',  
                                                      'Intensity gradient intercept', 'Intensity gradient slope', 
                                                      'Mean duration of SB bouts', 'Mean duration of LIPA bouts', 'Mean duration of MVPA bouts', 
                                                      'Number of SB bouts', 'Number of LIPA bouts', 'Number of MVPA bouts', 
                                                      'Most active 5hrs timing')))) %>% 
      mutate(lab = "NULL") %>% 
      mutate(lab = if_else(clustering == "km_3" & k == "1", "Best\n(N = 1408)", lab)) %>%
      mutate(lab = if_else(clustering == "km_3" & k == "2", "Intermediate\n(N = 1902)", lab),
           lab = if_else(clustering == "km_3" & k == "3", "Worst\n(N = 698)", lab),
           lab = if_else(clustering == "km_4" & k == "1", "Best\n(N = 1116)", lab),
           lab = if_else(clustering == "km_4" & k == "2", "Sedentary, high MVPA\n(N = 968)", lab),
           lab = if_else(clustering == "km_4" & k == "3", "Sedentary, high LIPA\n(N = 1283)", lab),
           lab = if_else(clustering == "km_4" & k == "4", "Worst\n(N = 641)", lab),
           lab = if_else(clustering == "km_5" & k == "1", "Most active\n(N = 730)", lab),
           lab = if_else(clustering == "km_5" & k == "2", "Sedentary active\n(N = 893)", lab),
           lab = if_else(clustering == "km_5" & k == "3", "Light movers\n(N = 1036)", lab),
           lab = if_else(clustering == "km_5" & k == "4", "Inactive\n(N = 1046)", lab),
           lab = if_else(clustering == "km_5" & k == "5", "Coach potato\n(N = 303)", lab)) %>% 
      mutate(lab = factor(lab, levels = c("Best\n(N = 1408)", "Intermediate\n(N = 1902)", "Worst\n(N = 698)", 
                                          "Best\n(N = 1116)", "Sedentary, high MVPA\n(N = 968)", "Sedentary, high LIPA\n(N = 1283)","Worst\n(N = 641)",
                                          "Most active\n(N = 730)", "Sedentary active\n(N = 893)", "Light movers\n(N = 1036)", "Inactive\n(N = 1046)", "Coach potato\n(N = 303)"))) 
    if(unique(data_for_plot$clustering) == "km_3")
    {
      plot_i <- ggplot(data = data_for_plot, 
             aes(y = varname, 
                 x = mean_value, 
                 fill = varname)) +
        geom_vline(xintercept = 0, color = "grey") +
        geom_vline(xintercept = c(-1, 1), color = "grey", lty = 2) +
        geom_col(position = position_dodge2()) +
        theme_bw() + 
        theme(panel.grid = element_blank(),
              legend.position = "none") + 
        labs(x = "Mean value of z-score", y = "") +  
        lims(x = c(-2,2)) +      
        facet_grid(. ~ lab) + 
        scale_fill_manual(values = col)
    }
    if(unique(data_for_plot$clustering) != "km_3")
    {
      plot_i <- ggplot(data = data_for_plot, 
                       aes(y = varname, 
                           x = mean_value, 
                           fill = varname)) +
        geom_vline(xintercept = 0, color = "grey") +
        geom_vline(xintercept = c(-1, 1), color = "grey", lty = 2) +
        geom_col(position = position_dodge2()) +
        theme_bw() + 
        theme(panel.grid = element_blank(),
              legend.position = "none",
              axis.title.x = element_blank()) + 
        labs(y = "") +  
        lims(x = c(-2,2)) +      
        facet_grid(. ~ lab) + 
        scale_fill_manual(values = col)
    }
    
    plot_i
    
    })

# > save
ggsave(plot = plots$km_5, filename = "E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/MAIN_ANALYSES/z-scores - Main - k = 5.png",
       height = 6,
       width = 30, units = "cm")

ggsave(plot = plots$km_4, filename = "E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/MAIN_ANALYSES/z-scores - Main - k = 4.png",
       height = 6,
       width = 25, units = "cm")

ggsave(plot = plots$km_3, filename = "E:/PC_FIXE/Analysis/02_ARTICLE_2/03_RESULTS/02_K-means/MAIN_ANALYSES/z-scores - Main - k = 3.png",
       height = 7,
       width = 20, units = "cm")

  




