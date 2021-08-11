# Project: PCA-K-means-for-PA-features
# Script name: 00_functions.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: 
#   Load functions to perform PCA and k-means
#   In addition: functions tests --> at the end of the script

# Note: run this script before running 
#       - 01_k_means_optimal_nb_cluster.R
#       - 02_models.R
#       - 03_graphs.R

# -----------------------------
# Packages
library(tidyverse)
library(haven)
library(testthat)
library(multcomp)

# packages for PCA and clustering
library(cluster)
library(FactoMineR)
library(factoextra)

# -----------------------------
# PCA

# > Function to run PCA and store results 
do.pca <- function(data_for_pca){
  
  # > Check if data are standardized
  #   Mean of means should be = 0, mean of sd should be = 1
  mean_data <- mean(round(apply(data_for_pca, MARGIN = 2, FUN = mean)))
  sd_data <- mean(round(apply(data_for_pca, MARGIN = 2, FUN = sd)))
  testthat::expect_equal(mean_data, 0)
  testthat::expect_equal(sd_data, 1)
  
  # > Check if any NA
  #   There should be no na
  na_data <- sum(apply(is.na(data_for_pca),2,sum))
  testthat::expect_equal(na_data, 0)
  
  # > Compute PCA
  pca <- prcomp(x = data_for_pca, tol = 0.05)
  
  # > Get results from PCA
  # Eigen values
  pca.eig <- get_eigenvalue(pca)
  
  # Variables results
  res.var <- suppressWarnings(get_pca_var(pca))
  pca.var <- data.frame(
    coord = res.var$coord,        # Coordinates
    contrib = res.var$contrib,    # Axis contribution
    cos2 = res.var$cos2,          # Representation quality 
    cor = res.var$cor             # Correlation
  )
  
  # Individuals results
  res.ind <- get_pca_ind(pca)
  pca.ind <- data.frame(
    ind.coord = res.ind$coord,        # Coordinates
    ind.contrib = res.ind$contrib,    # Axis contribution
    ind.cos2 = res.ind$cos2           # Representation quality
  )
  
  # > Store results
  pca.res <- list(pca = pca,
                  pca.eig = pca.eig,
                  pca.var = pca.var,
                  pca.ind = pca.ind)
  
  return(pca.res)
  
}

# > Classify participants based on principal component value (below or above the median)
#   for the n first principal components of a PCA object
#   Arguments:
#    - pca.obj: an object of class "prcomp", containing PC estimations for participants (pca.obj$x) 
#    - n: number of PC to take into account
#    - input.data: dataframe including non-standardized PA features for participants 
#   Value: 
#    - output.data: corresponds to input data, but with n new columns corresponding to 
#      participant classification relative to median PC value, for each of the n first PC
class.n.PC <- function(pca.obj, n, input.data){
  
  output.data <- input.data 
  
  for(i in 1:n)
    {
  
    # > Name of the PC
    PC_n <- paste0("PC", i)
   
     # > Median PC
    med_PC_n <- median(pca.obj$x[,paste0(PC_n)])
    
    # > Identify people below or above median PC
    indices_belowmedian = which(pca.obj$x[,paste0(PC_n)] <  med_PC_n)
    indices_abovemedian = which(pca.obj$x[,paste0(PC_n)] >= med_PC_n)
    
    # > Check
    expect_equal(min(pca.obj$x[indices_abovemedian,paste0(PC_n)]) >= med_PC_n, TRUE)
    expect_equal(max(pca.obj$x[indices_belowmedian,paste0(PC_n)]) < med_PC_n, TRUE)
    
    # > Assign a class corresponding to position relative to the PC median value
    output.data[, paste0("class", PC_n)] <- NA
    output.data[indices_belowmedian, paste0("class", PC_n)] <- 0
    output.data[indices_abovemedian, paste0("class", PC_n)] <- 1
    output.data[, paste0("class", PC_n)] <- as.factor(output.data[, paste0("class", PC_n)])
  
  }
  
  return(output.data)
  
}

# > PA features characteristic of the analytical population 
#   by the n first principal components of a PCA object
#   Arguments:
#    - pca.obj: an object of class "prcomp", containing PC estimations for participants (pca.obj$x) 
#    - n: number of PC to take into account
#    - desc.data: dataframe including non-standardized PA features for participants 
#   Value: 
#    - tab.PC: a list containing n descriptive tables of PA features (mean, sd)

desc.n.PC <- function(pca.obj, n, desc.data){
  
  # > Create the list to be filled
  tab.PC <- list()
  
  # > Identify people below or above median PC
  for(i in 1:n)
  {
    
    # > Name of the PC
    PC_n <- paste0("PC", i)
    # > Median PC
    med_PC_n <- median(pca.obj$x[,paste0(PC_n)])
    # > Identify people below or above median PC
    indices_belowmedian = which(pca.obj$x[,paste0(PC_n)] <  med_PC_n)
    indices_abovemedian = which(pca.obj$x[,paste0(PC_n)] >= med_PC_n)
    # > Check
    expect_equal(min(pca.obj$x[indices_abovemedian,paste0(PC_n)]) >= med_PC_n, TRUE)
    expect_equal(max(pca.obj$x[indices_belowmedian,paste0(PC_n)]) < med_PC_n, TRUE)
    
    # > Corresponding PA data for describing the PC
    data_belowmedian = desc.data[indices_belowmedian,]
    data_abovemedian = desc.data[indices_abovemedian,]
    
    comp.metrics <- list()
    
    # > For each PA feature, compute mean and sd in data_belowmedian and data_abovemedian
    for(m in 1:length(names(desc.data)))
    {
      
      # Name of the PA feature
      m_names <- names(desc.data)[m]
      
      # Compute mean, sd and p value for t test
      comp.metrics[[paste0(names(desc.data)[m])]] <- data.frame(
        # > mean
        mean_below = mean(data_belowmedian[,paste0(m_names)]),
        mean_above = mean(data_abovemedian[,paste0(m_names)]),
        # > sd
        sd_below = sd(data_belowmedian[,paste0(m_names)]),
        sd_above = sd(data_abovemedian[,paste0(m_names)]), 
        # > t test
        p.value = t.test(data_belowmedian[,paste0(m_names)], 
                         data_abovemedian[,paste0(m_names)], 
                         paired = TRUE)$p.value) %>% 
        # > Labels
        mutate(Below_median = paste0(format(round(mean_below, 1), nsmall = 1), " (", format(round(sd_below, 2), nsmall = 2), ")"),
               Above_median = paste0(format(round(mean_above, 1), nsmall = 1), " (", format(round(sd_above, 2), nsmall = 2), ")")) %>% 
        mutate(P.VALUE = format(round(p.value, 2), nsmall = 2), 
               P.VALUE = as.character(P.VALUE),
               P.VALUE = if_else(P.VALUE == "0.00", "< 0.001", P.VALUE)) %>% 
        select(Below_median, Above_median, P.VALUE)
      
    }
    
    # Save results
    tab.PC[[paste0(PC_n)]] <- plyr::ldply(comp.metrics, 
                                          data.frame, 
                                          .id = "metric")
    
    # Reset table
    comp.metrics <- NULL
    
  }
  
  return(tab.PC)
  
}


# > Analyse the difference in PA features among and between clusters
#   from kmeans fonction
#   Arguments: 
#   - data: a data providing PA features and cluster membership variable
comp_PA_feat <- function(data)
{
  
  full_table_temp <- list()
  descrip_table_temp <- list()
  
  for(v in names(data[,!names(data) %in% c("stno", "km.5")]))
  {
    
    # > Compute means, sd, p for means comparison
    # Means
    tab.mean <- aggregate(data[,colnames(data) == v], 
                          by = list(data[,colnames(data) == "km.5"]), 
                          mean)
    # SD
    tab.sd <- aggregate(data[,colnames(data) == v], 
                        by = list(data[,colnames(data) == "km.5"]), 
                        sd)
    
    # > One-way ANOVA: compare the difference among cluster
    # Option 1:
    #lm.var <- lm(data[,colnames(data) == v] ~ km.5, data = data)
    #anova(lm.var)
    # Option 2:
    anova.var <- aov(data[,colnames(data) == v] ~ km.5, data = data)
    p.val.anova.var <- summary(anova.var)[[1]][[5]][1]
    
    # > Tukey's HSD: pairwise comparison 
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
    
    descrip_table_temp[[paste0(v)]] <- tab.var
    full_table_temp[[paste0(v)]] <- left_join(tab.mean, 
                                              tab.sd, 
                                              by = "Group.1", 
                                              suffix = c(".mean", ".sd")) %>% 
      left_join(tab.mult.comp, by = "Group.1")
    
  }
  
  # > Table with mean and sd for each cluster, and p value for one-way ANOVA (difference among groups)
  descrip_table <- plyr::ldply(descrip_table_temp, data.frame, .id = "Feature") %>% 
    mutate(p.aov = as.character(round(p, 3)),
           p.aov = if_else(p < 0.001, "< 0.001", p.aov)) %>% 
    dplyr::select(-p)
  
  # > Table with mean, sd and letters corresponding Tukey's HSD comparison
  #plyr::ldply(full_table, data.frame, .id = "Feature")
  
  return(list(descrip_table,full_table_temp))
  
  
}










stop()
# > Checks 

# Data for checks
# > Standardized data
load("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//data//00_z_data_PCA_k_means.rda")

# > Checks for do.pca()
test.1 <- do.pca(data_for_pca = z_data$wei)

pca.1 <- prcomp(x = z_data$wei, tol = 0.05)
pca.eig.1 <- get_eigenvalue(pca.1)

res.var.1 <- get_pca_var(pca.1)
pca.var.1 <- data.frame(coord = res.var$coord,contrib = res.var$contrib, cos2 = res.var$cos2, cor = res.var$cor) 

res.ind.1 <- get_pca_ind(pca.1)
pca.ind.1 <- data.frame(ind.coord = res.ind$coord,ind.contrib = res.ind$contrib, ind.cos2 = res.ind$cos2) 

expect_equal(test.1$pca, pca.1)
expect_equal(test.1$pca.eig, pca.eig.1)
expect_equal(test.1$pca.var, pca.var.1)
expect_equal(test.1$pca.ind, pca.ind.1)

# > Checks the function for descriptive table
p <- pca.1$x
PC_n <- paste0("PC", 1)
med <- median(pca.1$x[,paste0(PC_n)])
indices_belowmedian = which(p[,paste0(PC_n)] < med)
indices_abovemedian = which(p[,paste0(PC_n)] >= med)
expect_equal(min(p[indices_abovemedian,paste0(PC_n)]) >= med, TRUE)
expect_equal(max(p[indices_belowmedian,paste0(PC_n)]) < med, TRUE)

d <- data$wei
d_belowmedian = d[indices_belowmedian,]
d_abovemedian = d[indices_abovemedian,]

m <- 3
m_names <- names(d)[m]

# Compute mean, sd and p value for t test
# > mean
mean_below = mean(d_belowmedian[,paste0(m_names)])
mean_above = mean(d_abovemedian[,paste0(m_names)])
# > sd
sd_below = sd(d_belowmedian[,paste0(m_names)])
sd_above = sd(d_abovemedian[,paste0(m_names)]) 
# > t test
p.value = t.test(d_belowmedian[,paste0(m_names)], 
                 d_abovemedian[,paste0(m_names)], 
                 paired = TRUE)$p.value

tab.1 <- desc.n.PC(pca.obj = pca.1, n = 3, desc.data = data$wei)
tab.1$PC1 %>% filter(metric == m_names)


