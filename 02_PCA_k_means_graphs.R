


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
source("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//01_PCA_k_means_models.R")

# -----------------------------
# Table with PA features name

tab.name <- tibble::tribble(
  ~var,                              ~varname,
  # Average acceleration
  "ACC_day_mg_wei",                  "Acceleration (mg) - All days",
  "ACC_day_mg_WD",                   "Acceleration (mg) - Week days",
  "ACC_day_mg_WE",                   "Acceleration (mg) - Weekend days",
  
  # Time in SB, LIPA and MVPA
  "dur_day_total_IN_min_wei",        "Duration IN (min/day) - All days",
  "dur_day_total_LIG_min_wei",       "Duration LIPA (min/day) - All days",
  "dur_day_total_MVPA_min_wei",      "Duration MVPA (min/day) - All days",
  
  "dur_day_total_IN_min_WD",         "Duration IN (min/day) - Week days",
  "dur_day_total_LIG_min_WD",        "Duration LIPA (min/day) - Week days",
  "dur_day_total_MVPA_min_WD",       "Duration MVPA (min/day) - Week days",
  
  "dur_day_total_IN_min_WE",         "Duration IN (min/day) - Weekend days",
  "dur_day_total_LIG_min_WE",        "Duration LIPA (min/day) - Weekend days",
  "dur_day_total_MVPA_min_WE",       "Duration MVPA (min/day) - Weekend days",
  
  # Mean duration of fragments of SB, LIPA and MVPA
  "FRAG_mean_dur_IN_day_wei",         "Mean duration of IN bouts - All days",
  "FRAG_mean_dur_LIPA_day_wei",       "Mean duration of LIPA bouts - All days",
  "FRAG_mean_dur_MVPA_day_wei",       "Mean duration of MVPA bouts - All days",
  
  "FRAG_mean_dur_IN_day_WD",          "Mean duration of IN bouts - Week days",
  "FRAG_mean_dur_LIPA_day_WD",        "Mean duration of LIPA bouts - Week days",
  "FRAG_mean_dur_MVPA_day_WD",        "Mean duration of MVPA bouts - Week days",
  "FRAG_mean_dur_IN_day_WE",          "Mean duration of IN bouts - Weekend days",
  "FRAG_mean_dur_LIPA_day_WE",        "Mean duration of LIPA bouts - Weekend days",
  "FRAG_mean_dur_MVPA_day_WE",        "Mean duration of MVPA bouts - Weekend days",
  
  
  # Number of fragents of SB, LIPA and MVPA
  "FRAG_Nfrag_IN_day_wei",            "Number of IN bouts - All days",
  "FRAG_Nfrag_LIPA_day_wei",          "Number of LIPA bouts - All days",
  "FRAG_Nfrag_MVPA_day_wei",          "Number of MVPA bouts - All days",
  "Nblocks_day_IN_unbt_wei",          "Number of < 10 min IN fragments (unbouted) - All days",
  "Nblocks_day_IN_bts_10_30_wei",     "Number of 10-30 min IN fragments - All days",
  "Nblocks_day_IN_bts_30_wei",        "Number of > 30 min IN fragments - All days",
  "Nblocks_day_LIG_unbt_wei",         "Number of < 10 min LIPA fragments (unbouted) - All days",
  "Nblocks_day_LIG_bts_10_wei",       "Number of > 10 min LIPA fragments - All days",
  "Nblocks_day_MVPA_unbt_wei",        "Number of < 10 min MVPA fragments (unbouted) - All days",
  "Nblocks_day_MVPA_bts_10_wei",      "Number of > 10 min MVPA fragments - All days",
  
  "FRAG_Nfrag_IN_day_WD",             "Number of IN bouts - Week days",
  "FRAG_Nfrag_LIPA_day_WD",           "Number of LIPA bouts - Week days",
  "FRAG_Nfrag_MVPA_day_WD",           "Number of MVPA bouts - Week days",
  "Nblocks_day_IN_unbt_WD",           "Number of < 10 min IN fragments (unbouted) - Week days",
  "Nblocks_day_IN_bts_10_30_WD",      "Number of 10-30 min IN fragments - Week days",
  "Nblocks_day_IN_bts_30_WD",         "Number of > 30 min IN fragments - Week days",
  "Nblocks_day_LIG_unbt_WD",          "Number of < 10 min LIPA fragments (unbouted) - Week days",
  "Nblocks_day_LIG_bts_10_WD",        "Number of > 10 min LIPA fragments - Week days",
  "Nblocks_day_MVPA_unbt_WD",         "Number of < 10 min MVPA fragments (unbouted) - Week days",
  "Nblocks_day_MVPA_bts_10_WD",       "Number of > 10 min MVPA fragments - Week days",
  
  "FRAG_Nfrag_IN_day_WE",             "Number of IN bouts - Weekend days",
  "FRAG_Nfrag_LIPA_day_WE",           "Number of LIPA bouts - Weekend days",
  "FRAG_Nfrag_MVPA_day_WE",           "Number of MVPA bouts - Weekend days",
  "Nblocks_day_IN_unbt_WE",           "Number of < 10 min IN fragments (unbouted) - Weekend days",
  "Nblocks_day_IN_bts_10_30_WE",      "Number of 10-30 min IN fragments - Weekend days",
  "Nblocks_day_IN_bts_30_WE",         "Number of > 30 min IN fragments - Weekend days",
  "Nblocks_day_LIG_unbt_WE",          "Number of < 10 min LIPA fragments (unbouted) - Weekend days",
  "Nblocks_day_LIG_bts_10_WE",        "Number of > 10 min LIPA fragments - Weekend days",
  "Nblocks_day_MVPA_unbt_WE",         "Number of < 10 min MVPA fragments (unbouted) - Weekend days",
  "Nblocks_day_MVPA_bts_10_WE",       "Number of > 10 min MVPA fragments - Weekend days",
  
  # Timing of the most 5 active hours
  "M5TIME_num_wei",                   "Most active 5hrs timing - All days",
  "M5TIME_num_WD",                    "Most active 5hrs timing - Week days",
  "M5TIME_num_WE",                    "Most active 5hrs timing - Weekend days",
  
  # Intensity gradient
  "ig_gradient_wei",                  "Intensity gradient slope - All days", 
  "ig_intercept_wei",                 "Intensity gradient intercept - All days",
  "ig_gradient_WD",                   "Intensity gradient slope - Week days", 
  "ig_intercept_WD",                  "Intensity gradient intercept - Week days",
  "ig_gradient_WE",                   "Intensity gradient slope - Weekend days", 
  "ig_intercept_WE",                  "Intensity gradient intercept - Weekend days",
  
  # Labels for transformed skewed variables
  "log_ACC_day_mg_wei",              "Acceleration* (mg) - All days",
  "log_ACC_day_mg_WD",               "Acceleration* (mg) - Week days",
  "log_ACC_day_mg_WE",               "Acceleration* (mg) - Weekend days",
  "sqrt_ACC_day_mg_wei",             "Acceleration* (mg) - All days",
  "sqrt_ACC_day_mg_WD",              "Acceleration* (mg) - Week days",
  "sqrt_ACC_day_mg_WE",              "Acceleration* (mg) - Weekend days",
  
  "log_dur_day_total_MVPA_min_wei",  "Duration MVPA* (min/day) - All days",
  "sqrt_dur_day_total_MVPA_min_wei", "Duration MVPA* (min/day) - All days",
  
  "log_dur_day_total_MVPA_min_WD",   "Duration MVPA* (min/day) - Week days",
  "sqrt_dur_day_total_MVPA_min_WD",  "Duration MVPA* (min/day) - Week days",
  
  "log_dur_day_total_MVPA_min_WE",   "Duration MVPA* (min/day) - Weekend days",
  "sqrt_dur_day_total_MVPA_min_WE",  "Duration MVPA* (min/day) - Weekend days",
  
  "log_FRAG_mean_dur_IN_day_wei",     "Mean duration of IN bouts* - All days",
  "sqrt_FRAG_mean_dur_IN_day_wei",    "Mean duration of IN bouts* - All days",
  "log_FRAG_mean_dur_MVPA_day_wei",   "Mean duration of MVPA bouts* - All days",
  "sqrt_FRAG_mean_dur_MVPA_day_wei",  "Mean duration of MVPA bouts* - All days",
  
  "log_FRAG_mean_dur_IN_day_WD",      "Mean duration of IN bouts* - Week days",
  "sqrt_FRAG_mean_dur_IN_day_WD",     "Mean duration of IN bouts* - Week days",
  "log_FRAG_mean_dur_MVPA_day_WD",    "Mean duration of MVPA bouts* - Week days",
  "sqrt_FRAG_mean_dur_MVPA_day_WD",   "Mean duration of MVPA bouts* - Week days",
  
  "log_FRAG_mean_dur_IN_day_WE",      "Mean duration of IN bouts* - Weekend days",
  "sqrt_FRAG_mean_dur_IN_day_WE",     "Mean duration of IN bouts* - Weekend days",
  "log_FRAG_mean_dur_MVPA_day_WE",    "Mean duration of MVPA bouts* - Weekend days",
  "sqrt_FRAG_mean_dur_MVPA_day_WE",   "Mean duration of MVPA bouts* - Weekend days",
  
  "log_Nblocks_day_LIG_bts_10_wei",   "Number of > 10 min LIPA fragments* - All days",
  "log_Nblocks_day_LIG_bts_10_wei",   "Number of > 10 min LIPA fragments* - All days",
  "sqrt_Nblocks_day_MVPA_bts_10_wei", "Number of > 10 min MVPA fragments* - All days",
  "sqrt_Nblocks_day_MVPA_bts_10_wei", "Number of > 10 min MVPA fragments* - All days",
  
  "log_M5TIME_num_wei",               "Most active 5hrs timing* - All days",
  "log_M5TIME_num_WD",                "Most active 5hrs timing* - Week days",
  "log_M5TIME_num_WE",                "Most active 5hrs timing* - Weekend days",
  "sqrt_M5TIME_num_wei",              "Most active 5hrs timing* - All days",
  "sqrt_M5TIME_num_WD",               "Most active 5hrs timing* - Week days",
  "sqrt_M5TIME_num_WE",               "Most active 5hrs timing* - Weekend days"
  
)

# > Join data and tab.name to have a clean version of variables
data_named <- data %>%
  map(., ~ { 
    .x %>%
      tidyr::gather(key = "var", value = "value", -stno) %>% 
      left_join(tab.name, by = "var") %>% 
      select(-var) %>% 
      tidyr::spread(key = "varname", value = "value") 
  })

# > Check variables names
data_named %>% 
  map_dfr(., ~{ data.frame(var = names(.x))}, .id = "dataset") %>%
  filter(var != "stno",
  !var %in% unique(tab.name$varname))
# 0 x 0 table --> OK

# -----------------------------
# PA features characteristics in the analytical population

# > Descriptive table of the (non-standardized) variables 
desc.tab <- data_named$wei %>% 
  left_join(data_named$WD_WE, by = "stno") %>% 
  gather(key = "feature", value = "value", -stno) %>% 
  separate(feature, sep = " - ", into = c("feature", "day_type")) %>% 
  group_by(feature, day_type) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            min = min(value),
            q25 = quantile(value, probs = c(0.25)),
            q50 = quantile(value, probs = c(0.5)),
            q75 = quantile(value, probs = c(0.75)),
            max = max(value)) %>% 
  arrange(day_type)

# > t-tests for difference between week days and weekend days

# Skewed variables: does t test and Wilcoxon test provide different conclusion? 
acc_wd <- data_named$WD$`Acceleration (mg) - Week days`
acc_we <- data_named$WE$`Acceleration (mg) - Weekend days`

dur_mvpa_wd <- data_named$WD$`Duration MVPA (min/day) - Week days`
dur_mvpa_we <- data_named$WE$`Duration MVPA (min/day) - Weekend days`

mean_mvpa_wd <- data_named$WD$`Mean duration of MVPA bouts - Week days`
mean_mvpa_we <- data_named$WE$`Mean duration of MVPA bouts - Weekend days`



t.test(acc_wd, acc_we, paired = T)$p.value      # p<2.2e-16 
wilcox.test(acc_wd, acc_we, paired = T)$p.value # p<2.2e-16

t.test(dur_mvpa_wd, dur_mvpa_we, paired = T)$p.value      # p<2.2e-16 
wilcox.test(dur_mvpa_wd, dur_mvpa_we, paired = T)$p.value # p<2.2e-16

t.test(mean_mvpa_wd, mean_mvpa_we, paired = T)$p.value      # p=0.0008216 
wilcox.test(mean_mvpa_wd, mean_mvpa_we, paired = T)$p.value # p<2.2e-16



t.test.num <- c(
  
  "Acceleration (mg)" = t.test(data_named$WD$`Acceleration (mg) - Week days`, data_named$WE$`Acceleration (mg) - Weekend days`, paired = T)$p.value, 
  "Acceleration* (mg)" = t.test(data_named$WD$`Acceleration (mg) - Week days`, data_named$WE$`Acceleration (mg) - Weekend days`, paired = T)$p.value, 
  "Duration IN (min/day)", 
  "Duration LIPA (min/day)", 
  "Duration MVPA (min/day)",
  "Duration MVPA* (min/day)",
  "Intensity gradient intercept", 
  "Intensity gradient slope", 
  "Mean duration of IN bouts", 
  "Mean duration of LIPA bouts", 
  "Mean duration of MVPA bouts", 
  "Most active 5hrs timing", 
  "Number of IN bouts", 
  "Number of LIPA bouts", 
  "Number of MVPA bouts",
  
  
  t.test(data_named$WD[,m], 
         data_named$WE[,m], 
         paired = TRUE)$p.value
  
  
  
)







t.test.p <- list()

for(m in 2:length(names(data_named$WD)))
{
  
  # > Check if we compare the same feature
  feature_wd = sub("\\ - .*", "", names(data_named$WD)[m])
  feature_we = sub("\\ - .*", "", names(data_named$WE)[m])
  expect_equal(feature_wd, feature_we)
  
  # > ttest
  t.test.p[[paste0(m)]] <- data.frame(
    # > feature
    feature = feature_wd,
    # > t test pvalue
    p.value = t.test(data_named$WD[,m], 
                     data_named$WE[,m], 
                     paired = TRUE)$p.value
    ) %>% 
  mutate(P.VALUE = format(round(p.value, 2), nsmall = 2), 
         P.VALUE = as.character(P.VALUE),
         P.VALUE = if_else(P.VALUE == "0.00", "< 0.001", P.VALUE))
  
}

# > Descriptive table (mean, sd) & p value for difference between week days and weekend days
desc.tab %>% 
  mutate(lab = paste0(format(round(mean, 1), nsmall = 1), " (", format(round(sd, 2), nsmall = 2), ")")) %>% 
  select(feature, day_type, lab) %>% 
  spread(key = day_type, value = lab) %>% 
  left_join(plyr::ldply(t.test.p, data.frame) %>% 
              select(-.id, -p.value), 
            by = "feature")

# > Plots
#   histogram
list(data_named$wei,
     data_named$WD,
     data_named$WE) %>% 
  map(., ~ {
    
    # > Histogram
    p.hist <- ggplot() + 
      geom_histogram(data = .x %>% 
                       gather(key = "variable", value = "value", -stno),
                     aes(value), fill = "grey") + 
      geom_vline(data = .x %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(med = median(value)), 
                 aes(xintercept = med), linetype = 2, col = pal[1]) +
      geom_boxplot(data = .x %>% 
                     gather(key = "variable", value = "value", -stno),
                   aes(x = value, y = -100), width = 150, col = pal[1], fill = pal[2],
                   outlier.shape = NA) +
      facet_wrap(~ variable, scales = "free") +
      theme_bw() + 
      theme(strip.background = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
    
    # > Density
    p.dens <- ggplot() + 
      geom_density(data = .x %>% 
                     gather(key = "variable", value = "value", -stno),
                   aes(value), ) + 
      geom_vline(data = .x %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(med = median(value)), 
                 aes(xintercept = med), linetype = 2, col = pal[5]) +
      geom_vline(data = .x %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(IQR1 = quantile(value, 0.25)), 
                 aes(xintercept = IQR1), linetype = 2, col = pal[2]) +
      geom_vline(data = .x %>% 
                   gather(key = "variable", value = "value", -stno) %>% 
                   group_by(variable) %>% 
                   summarise(IQR3 = quantile(value, 0.75)), 
                 aes(xintercept = IQR3), linetype = 2, col = pal[2]) +
      facet_wrap(~ variable, scales = "free") +
      theme_bw() + 
      theme(strip.background = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
    
    # > Save
    ggsave(plot = p.hist, 
           filename = paste0("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//plots//p.hist_", sub(".* - ", "", names(.x)[2]), ".png"),
           height = 6, 
           width = 12)
    ggsave(plot = p.dens, 
           filename = paste0("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//plots//p.dens_", sub(".* - ", "", names(.x)[2]), ".png"),
           height = 6, 
           width = 12)
    
  })

# > Plots for comparision between week days and weekend days
# Histogram
ggplot() + 
  # > histogram week days vs weekend days
  geom_histogram(data = data_named$WD_WE %>% 
                   gather(key = "feature", value = "value", -stno) %>% 
                   separate(feature, sep = " - ", into = c("feature", "day_type")),
                 aes(value, fill = day_type)) + 
  # > median and boxplot from all days
  geom_vline(data = data_named$wei %>% 
               gather(key = "feature", value = "value", -stno) %>% 
               separate(feature, sep = " - ", into = c("feature", "day_type")) %>% 
               group_by(feature) %>% 
               summarise(med = median(value)), 
             aes(xintercept = med), linetype = 2, col = pal[1]) +
  geom_boxplot(data = data_named$wei %>% 
                 gather(key = "feature", value = "value", -stno) %>%
                 separate(feature, sep = " - ", into = c("feature", "day_type")),
               aes(x = value, y = -100), width = 150, col = pal[1], fill = pal[2],
               outlier.shape = NA) +
  # > graphics settings
  scale_fill_manual(values = c("grey", pal[3]), 
                    name = "Day type") + 
  facet_wrap(~ feature, scales = "free") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") 

ggsave(filename = paste0("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//plots//p.hist_WD_WE.png"),
       height = 6, 
       width = 12)

# Density
ggplot() + 
  # > desnity week days vs weekend days
  geom_density(data = data_named$WD_WE %>% 
                   gather(key = "feature", value = "value", -stno) %>% 
                   separate(feature, sep = " - ", into = c("feature", "day_type")),
                 aes(value, col = day_type, fill = day_type), alpha = 0.3) + 
  # > median and boxplot from all days
  geom_vline(data = data_named$wei %>% 
               gather(key = "feature", value = "value", -stno) %>% 
               separate(feature, sep = " - ", into = c("feature", "day_type")) %>% 
               group_by(feature) %>% 
               summarise(med = median(value)), 
             aes(xintercept = med), linetype = 2, col = pal[1]) +
  geom_vline(data = data_named$wei %>% 
               gather(key = "feature", value = "value", -stno) %>%
               separate(feature, sep = " - ", into = c("feature", "day_type")) %>%
               group_by(feature) %>% 
               summarise(IQR1 = quantile(value, 0.25)), 
             aes(xintercept = IQR1), linetype = 2, col = pal[2]) +
  geom_vline(data = data_named$wei %>% 
               gather(key = "feature", value = "value", -stno) %>% 
               separate(feature, sep = " - ", into = c("feature", "day_type")) %>%
               group_by(feature) %>% 
               summarise(IQR3 = quantile(value, 0.75)), 
             aes(xintercept = IQR3), linetype = 2, col = pal[2]) +
  # > graphics settings
  scale_color_manual(values = c("grey", pal[3]), 
                    name = "Day type") + 
  scale_fill_manual(values = c("grey", pal[3]), 
                     name = "Day type") +
  facet_wrap(~ feature, scales = "free") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") 

ggsave(filename = paste0("E://PC_FIXE//Analysis//02_ARTICLE_2//02_PCA_and_K-MEANS//plots//p.dens_WD_WE.png"),
       height = 6, 
       width = 12)













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