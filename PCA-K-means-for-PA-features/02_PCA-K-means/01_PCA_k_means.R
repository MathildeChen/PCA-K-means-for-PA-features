# Script name: 01_PCA_k_means_1.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 

# -----------------------------
# Packages
#library(multcomp)
library(tidyverse)
library(haven)
library(corrr)
library(xlsx)
library(testthat)

# packages for PCA and clustering
library(cluster)
library(FactoMineR)
library(factoextra)

# Graphics
library(corrplot)
library(wesanderson)
library(ggplot2)
library(cowplot)

pal <- wes_palette("Zissou1", 5, "continuous")

# -----------------------------
# Table name
tab.name <- tibble::tribble(
  ~var, ~varname,
  
  "ACC_day_mg_wei", "Acceleration (mg)",
  "ACC_day_mg_WD", "Acceleration (mg) - Week days",
  "ACC_day_mg_WE", "Acceleration (mg)",
  
  # Time in SB, LIPA and MVPA
  "dur_day_total_IN_min_wei", "Duration IN (min/day)",
  "dur_day_total_LIG_min_wei", "Duration LIPA (min/day)",
  "dur_day_total_MVPA_min_wei", "Duration MVPA (min/day)",
  
  "dur_day_total_IN_min_WD", "Duration IN (min/day) - Week days",
  "dur_day_total_LIG_min_WD", "Duration LIPA (min/day) - Week days",
  "dur_day_total_MVPA_min_WD", "Duration MVPA (min/day) - Week days",
  
  "dur_day_total_IN_min_WE", "Duration IN (min/day) - Weekend days",
  "dur_day_total_LIG_min_WE", "Duration LIPA (min/day) - Weekend days",
  "dur_day_total_MVPA_min_WE", "Duration MVPA (min/day) - Weekend days",
  
  # Mean duration of fragments of SB, LIPA and MVPA
  "FRAG_mean_dur_IN_day_wei",  "Mean duration of IN bouts",
  "FRAG_mean_dur_LIPA_day_wei",  "Mean duration of LIPA bouts",
  "FRAG_mean_dur_MVPA_day_wei",  "Mean duration of MVPA bouts",
  
  "FRAG_mean_dur_IN_day_WD",  "Mean duration of IN bouts - Week days",
  "FRAG_mean_dur_LIPA_day_WD",  "Mean duration of LIPA bouts - Week days",
  "FRAG_mean_dur_MVPA_day_WD",  "Mean duration of MVPA bouts - Week days",
  
  "FRAG_mean_dur_IN_day_WE",  "Mean duration of IN bouts - Weekend days",
  "FRAG_mean_dur_LIPA_day_WE",  "Mean duration of LIPA bouts - Weekend days",
  "FRAG_mean_dur_MVPA_day_WE",  "Mean duration of MVPA bouts - Weekend days",
 
   # Number of fragents of SB, LIPA and MVPA
  "FRAG_Nfrag_IN_day_wei", "Number of IN bouts",
  "FRAG_Nfrag_LIPA_day_wei", "Number of LIPA bouts",
  "FRAG_Nfrag_MVPA_day_wei","Number of MVPA bouts",
  "Nblocks_day_IN_unbt_wei", "Number of < 10 min IN fragments (unbouted)",
  "Nblocks_day_IN_bts_10_30_wei", "Number of 10-30 min IN fragments",
  "Nblocks_day_IN_bts_30_wei", "Number of > 30 min IN fragments",
  "Nblocks_day_LIG_unbt_wei", "Number of < 10 min LIPA fragments (unbouted)",
  "Nblocks_day_LIG_bts_10_wei", "Number of > 10 min LIPA fragments",
  "Nblocks_day_MVPA_unbt_wei", "Number of < 10 min MVPA fragments (unbouted)",
  "Nblocks_day_MVPA_bts_10_wei", "Number of > 10 min MVPA fragments",
  
  "FRAG_Nfrag_IN_day_WD", "Number of IN bouts - Week days",
  "FRAG_Nfrag_LIPA_day_WD", "Number of LIPA bouts - Week days",
  "FRAG_Nfrag_MVPA_day_WD","Number of MVPA bouts - Week days",
  "Nblocks_day_IN_unbt_WD", "Number of < 10 min IN fragments (unbouted) - Week days",
  "Nblocks_day_IN_bts_10_30_WD", "Number of 10-30 min IN fragments - Week days",
  "Nblocks_day_IN_bts_30_WD", "Number of > 30 min IN fragments - Week days",
  "Nblocks_day_LIG_unbt_WD", "Number of < 10 min LIPA fragments (unbouted) - Week days",
  "Nblocks_day_LIG_bts_10_WD", "Number of > 10 min LIPA fragments - Week days",
  "Nblocks_day_MVPA_unbt_WD", "Number of < 10 min MVPA fragments (unbouted) - Week days",
  "Nblocks_day_MVPA_bts_10_WD", "Number of > 10 min MVPA fragments - Week days",
  
  "FRAG_Nfrag_IN_day_WE", "Number of IN bouts - Weekend days",
  "FRAG_Nfrag_LIPA_day_WE", "Number of LIPA bouts - Weekend days",
  "FRAG_Nfrag_MVPA_day_WE","Number of MVPA bouts - Weekend days",
  "Nblocks_day_IN_unbt_WE", "Number of < 10 min IN fragments (unbouted) - Weekend days",
  "Nblocks_day_IN_bts_10_30_WE", "Number of 10-30 min IN fragments - Weekend days",
  "Nblocks_day_IN_bts_30_WE", "Number of > 30 min IN fragments - Weekend days",
  "Nblocks_day_LIG_unbt_WE", "Number of < 10 min LIPA fragments (unbouted) - Weekend days",
  "Nblocks_day_LIG_bts_10_WE", "Number of > 10 min LIPA fragments - Weekend days",
  "Nblocks_day_MVPA_unbt_WE", "Number of < 10 min MVPA fragments (unbouted) - Weekend days",
  "Nblocks_day_MVPA_bts_10_WE", "Number of > 10 min MVPA fragments - Weekend days",
  
  # Mean acceleration and timing of the most 5 active hours
  "M5TIME_num_wei", "Most active 5hrs timing",
  "M5TIME_num_WD", "Most active 5hrs timing - Week days",
  "M5TIME_num_WE", "Most active 5hrs timing - Weekend days",
  
  # Intensity gradient
  "ig_gradient_wei", "Intensity gradient slope", 
  "ig_intercept_wei", "Intensity gradient intercept",
  
  "ig_gradient_WD", "Intensity gradient slope - Week days", 
  "ig_intercept_WD", "Intensity gradient intercept - Week days",
  
  "ig_gradient_WE", "Intensity gradient slope - Weekend days", 
  "ig_intercept_WE", "Intensity gradient intercept - Weekend days",
)

# -----------------------------
# Data 
# > Sample participants (n = 4008)
sample_stno <- read_dta("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2021-05-03\\data_03052021.dta")

# > Person level summary
data0 <- read.csv("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2021-05-07\\part5_personsummary_WW_L40M100V400_T5A5.csv") %>%
  rename("stno" = "ID") %>%
  filter(stno %in% unique(sample_stno$stno)) 

# > Data check
# Number of blocks = total number of fragments
expect_equal(data0$FRAG_Nfrag_IN_day_wei, data0$Nblocks_day_total_IN_wei)
expect_equal(data0$FRAG_Nfrag_LIPA_day_wei, data0$Nblocks_day_total_LIG_wei)
expect_equal(data0$FRAG_Nfrag_MVPA_day_wei, data0$Nblocks_day_total_MOD_wei + data0$Nblocks_day_total_VIG_wei) # --> average diff: 1.46
expect_equal(data0$FRAG_Nfrag_MVPA_day_wei, data0$Nblocks_day_MOD_unbt_wei + data0$Nblocks_day_VIG_unbt_wei + data0$Nblocks_day_MVPA_bts_10_wei) # --> average diff: 0.594 

# > All PA features
data <- data0 %>%
  # Derive total duration in MVPA (total duration MOD + total duration VIG)
  mutate(dur_day_total_MVPA_min_wei = dur_day_total_MOD_min_wei + dur_day_total_VIG_min_wei,
         dur_day_total_MVPA_min_WD  = dur_day_total_MOD_min_WD  + dur_day_total_VIG_min_WD,
         dur_day_total_MVPA_min_WE  = dur_day_total_MOD_min_WE  + dur_day_total_VIG_min_WE) %>%
  # Derive number of unbouted fragments of MVPA (unbouted MOD + VIG)
  mutate(Nblocks_day_MVPA_unbt_wei = Nblocks_day_total_MOD_wei + Nblocks_day_total_VIG_wei,
         Nblocks_day_MVPA_unbt_WD  = Nblocks_day_MOD_unbt_WD   + Nblocks_day_VIG_unbt_WD,
         Nblocks_day_MVPA_unbt_WE  = Nblocks_day_MOD_unbt_WE   + Nblocks_day_VIG_unbt_WE) %>% 
  # Correct m5 timing 
  mutate(M5TIME_num_wei = if_else(M5TIME_num_wei > 24, M5TIME_num_wei - 24, M5TIME_num_wei),
         M5TIME_num_WD  = if_else(M5TIME_num_WD  > 24, M5TIME_num_WD  - 24, M5TIME_num_WD),
         M5TIME_num_WE  = if_else(M5TIME_num_WE  > 24, M5TIME_num_WE  - 24, M5TIME_num_WE)) %>%
  # Select appropiate variables for all days (_wei, weighted estimates), weekd days (_WD), weekend days (_WE)
  select(stno, 
         # > Mean acceleration
         ACC_day_mg_wei,               ACC_day_mg_WD,               ACC_day_mg_WE,
         # > Time in SB, LIPA and MVPA
         dur_day_total_IN_min_wei,     dur_day_total_IN_min_WD,     dur_day_total_IN_min_WE, 
         dur_day_total_LIG_min_wei,    dur_day_total_LIG_min_WD,    dur_day_total_LIG_min_WE, 
         dur_day_total_MVPA_min_wei,   dur_day_total_MVPA_min_WD,   dur_day_total_MVPA_min_WE,
         # > Total number of bouts in SB, LIPA and MVPA
         FRAG_Nfrag_IN_day_wei,        FRAG_Nfrag_IN_day_WD,        FRAG_Nfrag_IN_day_WE,
         FRAG_Nfrag_LIPA_day_wei,      FRAG_Nfrag_LIPA_day_WD,      FRAG_Nfrag_LIPA_day_WE,
         FRAG_Nfrag_MVPA_day_wei,      FRAG_Nfrag_MVPA_day_WD,      FRAG_Nfrag_MVPA_day_WE,
         # > Number of fragments of different lengths in SB, LIPA and MVPA
         Nblocks_day_IN_unbt_wei,      Nblocks_day_IN_unbt_WD,      Nblocks_day_IN_unbt_WE,
         Nblocks_day_IN_bts_10_30_wei, Nblocks_day_IN_bts_10_30_WD, Nblocks_day_IN_bts_10_30_WE,
         Nblocks_day_IN_bts_30_wei,    Nblocks_day_IN_bts_30_WD,    Nblocks_day_IN_bts_30_WE, 
         Nblocks_day_LIG_unbt_wei,     Nblocks_day_LIG_unbt_WD,     Nblocks_day_LIG_unbt_WE,
         Nblocks_day_LIG_bts_10_wei,   Nblocks_day_LIG_bts_10_WD,   Nblocks_day_LIG_bts_10_WE,
         Nblocks_day_MVPA_unbt_wei,    Nblocks_day_MVPA_unbt_WD,    Nblocks_day_MVPA_unbt_WE,
         Nblocks_day_MVPA_bts_10_wei,  Nblocks_day_MVPA_bts_10_WD,  Nblocks_day_MVPA_bts_10_WE,
         # > Mean duration of bouts in SB, LIPA and MVPA
         FRAG_mean_dur_IN_day_wei,     FRAG_mean_dur_IN_day_WD,     FRAG_mean_dur_IN_day_WE, 
         FRAG_mean_dur_LIPA_day_wei,   FRAG_mean_dur_LIPA_day_WD,   FRAG_mean_dur_LIPA_day_WE,
         FRAG_mean_dur_MVPA_day_wei,   FRAG_mean_dur_MVPA_day_WD,   FRAG_mean_dur_MVPA_day_WE,
         # > Timing of the most 5 active hours
         M5TIME_num_wei,               M5TIME_num_WD,               M5TIME_num_WE,
         # > Activity gradient
         ig_gradient_wei,              ig_gradient_WD,              ig_gradient_WE, 
         ig_intercept_wei,             ig_intercept_WD,             ig_intercept_WE)

# > Partition data into wei, WD and WE variables
data_wei   <- data %>% select(stno, ends_with("_wei"))
data_WD    <- data %>% select(stno, ends_with("_WD"))
data_WE    <- data %>% select(stno, ends_with("_WE"))
data_WD_WE <- data %>% select(-ends_with("_wei"))

# > Scale the data (and rename columns with "z_" to know that they are scaled
z_data_wei <- rename_with(data_wei[,-1], 
                          .fn = ~paste0("z_", .), 
                          .cols = names(data_wei[,-1])) %>% scale(.)

z_data_WD <- rename_with(data_WD[,-1], 
                         .fn = ~paste0("z_", .), 
                         .cols = names(data_WD[,-1])) %>% scale(.)

z_data_WE <- rename_with(data_WE[,-1], 
                         .fn = ~paste0("z_", .), 
                         .cols = names(data_WE[,-1])) %>% scale(.)

z_data_WD_WE <- rename_with(data_WD_WE[,-1], 
                            .fn = ~paste0("z_", .), 
                            .cols = names(data_WD_WE[,-1])) %>% scale(.)

# > Check standardization (mean = 0, sd = 1)
z_data_wei %>% 
  as.data.frame(.) %>% 
  gather(key = "variable", value = "value") %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value),
            sd = sd(value))

z_data_WD %>% 
  as.data.frame(.) %>% 
  gather(key = "variable", value = "value") %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value),
            sd = sd(value))

z_data_WE %>% 
  as.data.frame(.) %>% 
  gather(key = "variable", value = "value") %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value),
            sd = sd(value))

z_data_WD_WE %>% 
  as.data.frame(.) %>% 
  gather(key = "variable", value = "value") %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% View(.)

# --> OK

# > Covariates (N = 4006)
load("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\00_DATA\\tab_full_cov_s11.rda")
# Drop variables with NA (N = 3893)
tab_11_fin <- tab_11 %>% 
  drop_na(sex, fage_s, ethnicity_i, fstatusx_i_2, edu_imp, flgrlump_i_ordinal,
          fesmoke_i, funitwk0_i_3, ffruitvg_i_3, 
          fbmi_i_3, hypertension, hyperlipidemia, prevalent_diabete,
          mmm_index) 



# Change name of the variables
tidyr::gather(key = "var", value = "value", -stno) %>% 
  left_join(tab.name, by = "var") %>% 
  select(-var) %>% 
  tidyr::spread(key = "varname", value = "value")

# -----------------------------
# PCA 

pca <- prcomp(x = z_data_wei, tol = 0.05)

# > Get results from PCA
# Eigen values
pca.eig <- get_eigenvalue(pca)

# Variables results
res.var <- get_pca_var(pca)
pca.var <- data.frame(
  coord = res.var$coord,        # Coordonnées
  contrib = res.var$contrib,    # Contributions aux axes
  cos2 = res.var$cos2,          # Qualité de représentation 
  cor = res.var$cor             # Correlation
) %>% 
  mutate(metric = rownames(.)) %>%
  gather(key = "pca_feature_full", value = "value", -metric) %>%
  separate(col = "pca_feature_full", into = c("pca_feature", "B", "Dim"), remove = T) %>% 
  unite("CP", B:Dim, sep = ".") %>%
  spread(key = "pca_feature", value = "value")

# Individuals results
res.ind <- get_pca_ind(pca)
pca.ind <- data.frame(
  ind.coord = res.ind$coord,        # Coordonnées
  ind.contrib = res.ind$contrib,    # Contributions aux axes
  ind.cos2 = res.ind$cos2           # Qualité de représentation 
)

# > Screw plot
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 60))
ggsave("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//plots//PCA_screeplot.png",
       height = 4, 
       width = 6)

# > Contribution + correlation of each variable to the 5 first dimensions
pca.var %>%
  filter(CP %in% c("Dim.1", "Dim.2", "Dim.3", "Dim.4")) %>%
  # Add explained variance into the graph
  left_join(data.frame(CP = paste0("Dim.", 1:length(pca.eig[,2])), 
                       variance.percent = pca.eig[,2]), 
            by = "CP") %>%
  mutate(CP_lab = paste0(CP, " (", round(variance.percent, 1), "%)"),
         contrib_lab = round(contrib, 1)) %>%
  # Plot
  ggplot(., aes(x = metric, y = contrib, color = cor, label = contrib_lab)) + 
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

ggsave("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//plots//PCA_varcontrib.png",
       height = 4, 
       width = 8)

# Another way to look a this
pca.var %>% 
  filter(CP %in% c("Dim.1", "Dim.2", "Dim.3", "Dim.4")) %>% 
  ggplot(., aes(x = CP, y = reorder(metric, cor, max), fill = cor, label = round(contrib, 1))) + 
  geom_tile() + 
  geom_text() + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom") + 
  scale_fill_gradientn(colours = c(pal[5], pal[3], "white", pal[2], "blue"),
                        breaks = c(-1, -0.5, 0, 0.5, 1),
                        name = "Correlation") + 
  scale_x_discrete(position = "top") +
  labs(x = " ", y = "Fragmentation metrics")

ggsave("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//plots//PCA_varcontrib_table.png",
       height = 6, 
       width = 8)

# > Describe PC in terms of PA metrics
tab_PC <- list()
n <- 3

for(i in 1:n)
{
  
  PC_n <- paste0("PC", i)
  
  indices_belowmedian = which(pca$x[,paste0(PC_n)] <  median(pca$x[,paste0(PC_n)]))
  indices_abovemedian = which(pca$x[,paste0(PC_n)] >= median(pca$x[,paste0(PC_n)]))
  
  data_belowmedian = data[indices_belowmedian,]
  data_abovemedian = data[indices_abovemedian,]
  
  comp.metrics <- list()
  
  for(m in 1:length(names(data[,-1])))
  {
    # Metric name
    m_names <- names(data[,-1])[m]
    # Compute mean, sd and p value for t test
    comp.metrics[[paste0(names(data[,-1])[m])]] <- data.frame(
      # > mean
      mean_below = format(round(mean(data_belowmedian[,paste0(m_names)]), 1), nsmall = 1),
      mean_above = format(round(mean(data_abovemedian[,paste0(m_names)]), 1), nsmall = 1),
      # > sd
      sd_below = format(round(sd(data_belowmedian[,paste0(m_names)]), 2), nsmall = 2),
      sd_above = format(round(sd(data_abovemedian[,paste0(m_names)]), 2), nsmall = 2),
      # > t test
      p.value = format(round(t.test(data_belowmedian[,paste0(m_names)], 
                                    data_abovemedian[,paste0(m_names)], 
                                    paired = TRUE)$p.value, 2), nsmall = 2)
    )
    
  }
  
  # Save results
  tab_PC[[paste0(PC_n)]] <- plyr::ldply(comp.metrics, 
                                        data.frame, 
                                        .id = "metric") %>% 
    # labels
    mutate(Below_median = paste0(mean_below, " (", sd_below, ")"),
           Above_median = paste0(mean_above, " (", sd_above, ")")) %>% 
    # p.value
    mutate(p.value = as.character(p.value),
           p.value = if_else(p.value == "0.00", "< 0.001", p.value)) %>% 
    select(metric, Below_median, Above_median, p.value)
  
  # Reset table
  comp.metrics <- NULL
  
}

# Tables
write.xlsx(x = tab_PC$PC1, 
           file = "E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//tables//tab_PCA.xlsx",
           sheetName = "PC1")
write.xlsx(x = tab_PC$PC2, 
           file = "E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//tables//tab_PCA.xlsx",
           sheetName = "PC2",
           append = T)
write.xlsx(x = tab_PC$PC3, 
           file = "E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//tables//tab_PCA.xlsx",
           sheetName = "PC3",
           append = T)

# -----------------------------
# k-means clustering 

# > Optimal number of clusters
# Elbow method
fviz_nbclust(data_s, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
ggsave("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//plots//KME_Elbowplot.png",
       height = 4, 
       width = 6)

# > k = 3
set.seed(123)
km.res.3 <- kmeans(data_s, centers = 3, nstart = 25) 
km.res.4 <- kmeans(data_s, centers = 4, nstart = 25) 
km.res.5 <- kmeans(data_s, centers = 5, nstart = 25) 

data$cluster.3 <- km.res.3$cluster
data$cluster.3 <- as.factor(data$cluster.3)
data$cluster.4 <- km.res.4$cluster
data$cluster.4 <- as.factor(data$cluster.4)
data$cluster.5 <- km.res.5$cluster
data$cluster.5 <- as.factor(data$cluster.5)