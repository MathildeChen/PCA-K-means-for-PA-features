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

# -----------------------------
# Palette colors 
pal <- wes_palette("Zissou1", 5, "discrete")

# -----------------------------
# Data + Functions to run PCA and k-means + PCA and k-means results
source("02_PCA-K-means\\02_models.R")

# -----------------------------
# Table with PA features name



# PCA 

# -----------------------------


# > Descriptive tables
tab.wei    <- desc.n.PC(pca.obj = PCA.res$wei$pca,   n = 10, desc.data = data$wei[-1])
tab.WD     <- desc.n.PC(pca.obj = PCA.res$WD$pca,    n = 10, desc.data = data$WD[-1])
tab.WE     <- desc.n.PC(pca.obj = PCA.res$WE$pca,    n = 10, desc.data = data$WE[-1])
#tab.WD_WE  <- desc.n.PC(pca.obj = PCA.res$WD_WE, n = 10, desc.data = data$WD_WE[-1])

# > Save descriptive tables
tab_path <- "E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//tables//"
# > wei
write.xlsx(x = tab.wei$PC1, 
           file = paste0(tab_path, "tab_PCA_wei.xlsx"),
           sheetName = "PC1")
for(name in names(tab.wei)[-1])
{
  
  write.xlsx(x = tab.wei[paste0(name)], 
             file = paste0(tab_path, "tab_PCA_wei.xlsx"),
             sheetName = paste0(name),
             append = T)
  
}

# > WD
write.xlsx(x = tab.WD$PC1, 
           file = paste0(tab_path, "tab_PCA_WD.xlsx"),
           sheetName = "PC1")
for(name in names(tab.WD)[-1])
{
  
  write.xlsx(x = tab.WD[paste0(name)], 
             file = paste0(tab_path, "tab_PCA_WD.xlsx"),
             sheetName = paste0(name),
             append = T)
  
}

# > WE
write.xlsx(x = tab.WE$PC1, 
           file = paste0(tab_path, "tab_PCA_WE.xlsx"),
           sheetName = "PC1")
for(name in names(tab.WE)[-1])
{
  
  write.xlsx(x = tab.WE[paste0(name)], 
             file = paste0(tab_path, "tab_PCA_WE.xlsx"),
             sheetName = paste0(name),
             append = T)
  
}







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








# K-means


# Data

# > Clustering indicators


# - the cluster means or centers: a matrix, which rows are cluster number (1 to 3) and columns are variables
KM.wei_log$centers
# - the clustering vector: a vector of integers indicating the cluster to which each point is allocated
KM.wei_log$cluster 
# - TSS (total variance in the data)
KM.wei_log$totss
# - Within-cluster sum of squares
KM.wei_log$withinss
# - Total within-cluster sum of squares
KM.wei_log$tot.withinss 
sum(KM.wei_log$withinss)
# - Between-cluster sum of squares 
KM.wei_log$betweenss
KM.wei_log$totss - KM.wei_log$tot.withinss
# - Size (nb of obs in each cluster): 
KM.wei_log$size


KM.wei_log$centers %>% 
  as.data.frame(.) %>%
  mutate(Cluster = rownames(.)) %>%
  gather(key = "feature", value = "value", -Cluster)


# Global
data.frame(totss = .x$totss, 
           tot.withinss = .x$tot.withinss,
           betweenss = .x$betweenss)

# Clusters
data.frame(withinss = .x$withinss, 
           size = .x$size,
           row.names = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"))


nn





data <- data_wei_log

# > compute means, sd, p for means comparison
full_table <- list()
descrip_table <- list()
for(v in names(data[,!names(data) %in% c("stno", "km.5")]))
{
  # Means
  tab.mean <- aggregate(data[,colnames(data) == v], 
                        by = list(data[,colnames(data) == "km.5"]), 
                        mean)
  # SD
  tab.sd <- aggregate(data[,colnames(data) == v], 
                      by = list(data[,colnames(data) == "km.5"]), 
                      sd)
  
  # One-way ANOVA
  # Option 1:
  #lm.var <- lm(data[,colnames(data) == v] ~ km.5, data = data)
  #anova(lm.var)
  # Option 2:
  anova.var <- aov(data[,colnames(data) == v] ~ km.5, data = data)
  p.val.anova.var <- summary(anova.var)[[1]][[5]][1]
  
  # Tukey test (multiple means comparison)
  # Option 1
  #tukey.var <- glht(lm.var, linfct = mcp(km.5 = "Tukey"))
  # Option 2
  tukey.var <- glht(anova.var, linfct = mcp(km.5 = "Tukey"))
  cld.var <- cld(tukey.var)
  tab.mult.comp <- data.frame(Group.1 = levels(data$km.5),
                              letters = cld.var$mcletters$Letters)
  
  # Summary table
  tab.var <- left_join(tab.mean, tab.sd, by = "Group.1") %>% 
    mutate(lab = paste0(round(x.x, digits = 1), " (", round(x.y, digits = 2), ")"),
           Group.1 = paste0("Cluster ", Group.1)) %>% 
    dplyr::select("Cluster" = Group.1 , "Mean (sd)" = lab) %>% 
    spread(key = "Cluster", value = "Mean (sd)")
  tab.var$p <- p.val.anova.var
  
  descrip_table[[paste0(v)]] <- tab.var
  full_table[[paste0(v)]] <- left_join(tab.mean, 
                                         tab.sd, 
                                         by = "Group.1", 
                                         suffix = c(".mean", ".sd")) %>% 
    left_join(tab.mult.comp, by = "Group.1")
  
}


wei_log_k_means_comp <- comp_PA_feat(data = data_wei_log)
wei_log_k_means_comp[[1]]
wei_log_k_means_comp[[2]]
# Variable distribution between clusters
data_wei_log %>% 
  dplyr::select(-km.5) %>% 
  gather(key = "variable", value = "value", -stno) %>% 
  group_by(variable) %>% 
  summarise(med = median(value),
            IQR1 = quantile(value, 0.25),
            IQR3 = quantile(value, 0.75)) %>% 
  ggplot(.) + 
  geom_hline(aes(yintercept = med), linetype = 2, col = "black") +
  geom_hline(aes(yintercept = IQR1), linetype = 2, col = "darkgrey") +
  geom_hline(aes(yintercept = IQR3), linetype = 2, col = "darkgrey") +
  geom_jitter(data = data_wei_log %>% 
                dplyr::select(-stno) %>% 
                gather(key = "variable", value = "value", -km.5),
              aes(x = km.5, y = value, col = as.factor(km.5)),
              alpha = 0.5, size = 0.1, pch = 1, width = 0.25) + 
  geom_point(data =  plyr::ldply(wei_log_k_means_comp[[2]], data.frame, .id = "Feature") %>% 
               dplyr::rename("variable" = "Feature"),
             aes(x = Group.1, y = x.mean)) +
  geom_linerange(data =  plyr::ldply(wei_log_k_means_comp[[2]], data.frame, .id = "Feature") %>% 
                   dplyr::rename("variable" = "Feature"),
                 aes(x = Group.1, ymin = x.mean-x.sd, ymax = x.mean+x.sd)) +
  geom_text(data = plyr::ldply(wei_log_k_means_comp[[2]], data.frame, .id = "Feature") %>% 
              dplyr::rename("variable" = "Feature"),
            aes(x = Group.1, y = x.mean+x.sd*2, label = letters),
            size = 3) + 
  facet_wrap(. ~ variable, scales = "free") + 
  scale_color_manual(values = pal) + 
  scale_fill_manual(values = pal) +
  theme_bw() + 
  theme(legend.position = "none",
        strip.background = element_blank(), 
        axis.title = element_blank()) + 
  coord_flip()













write.xlsx(x = plyr::ldply(descrip_table, data.frame, .id = "Feature") %>% 
             mutate(p.aov = as.character(round(p, 3)),
                    p.aov = if_else(p < 0.001, "< 0.001", p.aov)) %>% 
             dplyr::select(-p), 
           file = "E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//tables//tab_k_means_3.xlsx")

# Variable distribution between clusters
data %>% 
  dplyr::select(-cluster.3) %>% 
  gather(key = "variable", value = "value", -stno) %>% 
  group_by(variable) %>% 
  summarise(med = median(value),
            IQR1 = quantile(value, 0.25),
            IQR3 = quantile(value, 0.75)) %>% 
  ggplot(.) + 
  geom_hline(aes(yintercept = med), linetype = 2, col = "black") +
  geom_hline(aes(yintercept = IQR1), linetype = 2, col = "darkgrey") +
  geom_hline(aes(yintercept = IQR3), linetype = 2, col = "darkgrey") +
  geom_jitter(data = data %>% 
                dplyr::select(-stno) %>% 
                gather(key = "variable", value = "value", -cluster.3),
              aes(x = cluster.3, y = value, col = as.factor(cluster.3)),
              alpha = 0.5, size = 0.1, pch = 1, width = 0.25) + 
  geom_point(data =  plyr::ldply(full_table_3, data.frame, .id = "Feature") %>% 
               dplyr::rename("variable" = "Feature"),
             aes(x = Group.1, y = x.mean)) +
  geom_linerange(data =  plyr::ldply(full_table_3, data.frame, .id = "Feature") %>% 
                   dplyr::rename("variable" = "Feature"),
                 aes(x = Group.1, ymin = x.mean-x.sd, ymax = x.mean+x.sd)) +
  geom_text(data = plyr::ldply(full_table_3, data.frame, .id = "Feature") %>% 
              dplyr::rename("variable" = "Feature"),
            aes(x = Group.1, y = x.mean+x.sd*2, label = letters),
            size = 3) + 
  facet_wrap(. ~ variable, scales = "free") + 
  scale_color_manual(values = pal) + 
  scale_fill_manual(values = pal) +
  theme_bw() + 
  theme(legend.position = "none",
        strip.background = element_blank(), 
        axis.title = element_blank()) + 
  coord_flip()








































