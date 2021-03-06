# Script name: 00_EXPLORATORY_ANALYSES_1.R
# 
# Author: M.Chen, Inserm, 2021
#
# Doing: Exploratory analyses for the papers on 
# association between PA profiles and CVD/mortality
#
# Exploratory analyses plan: 
# (1) Description of the population (means and SD for descriptive variables for the full sample, then stratified by sex, by outcomes); 
# (2) Means and SD for PA metrics for the full sample, then stratified by sex, outcomes; 
# (3) Look at PA metrics differences between weekdays and weekend days for the full sample, then stratified by sex, by outcomes;
# (4) Explore redundancy of the PA metrics: Correlations between physical activity features
# (5) 2D and 3D plots: hypotheses of clusters;

# -----------------------------
# Packages
library(tidyverse)
library(haven)
library(corrr)
library(xlsx)
library(testthat)

library(corrplot)
library(wesanderson)
library(ggplot2)
library(cowplot)
library(scatterplot3d)
library(tourr)
library(GGally)

pal <- wes_palette("Zissou1", 5, "continuous")

# -----------------------------
# Table name
tab.name <- tibble::tribble(
  ~var, ~varname,
  "ACC_day_mg_wei", "Acceleration (mg)",
  # Time in SB, LIPA and MVPA
  "dur_day_total_IN_min_wei", "Duration IN (min/day)",
  "dur_day_total_LIG_min_wei", "Duration LIPA (min/day)",
  "dur_day_total_MVPA_min_wei", "Duration MVPA (min/day)",
  # Mean duration of bouts of SB, LIPA and MVPA
  "FRAG_mean_dur_1_day_wei",  "Mean duration of IN bouts",
  "FRAG_mean_dur_LIPA_day_wei",  "Mean duration of LIPA bouts",
  "FRAG_mean_dur_MVPA_day_wei",  "Mean duration of MVPA bouts",
  # Number of bouts of SB, LIPA and MVPA
  "FRAG_Nfragments_1_day_wei", "Number of IN bouts",
  "FRAG_Nfragments_LIPA_day_wei", "Number of LIPA bouts",
  "FRAG_Nfragments_MVPA_day_wei","Number of MVPA bouts",
  # Mean acceleration and timing of the most 5 active hours
  "M5VALUE_wei", "Most active 5hrs acceleration (g)",
  "M5TIME_num_wei", "Most active 5hrs timing",
  # Intensity gradient
  "ig_gradient_wei", "Intensity gradient slope", 
  "ig_intercept_wei", "Intensity gradient intercept",
)

# -----------------------------
# Data 
# Sample participants (n = 4006)
sample_stno <- read_dta("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2020-05-15\\WW_L40M100V400_update052020.dta") %>%
  filter(exclusion == 0) %>% 
  select(stno)

# Selected PA metrics
data <- read.csv("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2021-03-28\\part5_personsummary_WW_L40M100V400_T5A5.csv") %>%
  rename("stno" = "ID") %>%
  filter(stno %in% unique(sample_stno$stno)) %>% 
  # Derive total duration in MVPA (total duration MOD + total duration VIG)
  mutate(dur_day_total_MVPA_min_wei = dur_day_total_MOD_min_wei + dur_day_total_VIG_min_wei) %>%
  # Select appropiate variables
  select(stno, 
         # Mean acceleration
         ACC_day_mg_wei,
         # Time in SB, LIPA and MVPA
         dur_day_total_IN_min_wei, 
         dur_day_total_LIG_min_wei, 
         dur_day_total_MVPA_min_wei,
         # Mean duration of bouts of SB, LIPA and MVPA
         FRAG_mean_dur_1_day_wei, # mean duration of SB bouts
         FRAG_mean_dur_LIPA_day_wei,
         FRAG_mean_dur_MVPA_day_wei,
         # Total number of bouts of SB, LIPA and MVPA
         FRAG_Nfragments_1_day_wei,
         FRAG_Nfragments_LIPA_day_wei,
         FRAG_Nfragments_MVPA_day_wei,
         # Mean acceleration and timing of the most 5 active hours
         M5VALUE_wei, M5TIME_num_wei,
         # Activity gradient
         ig_gradient_wei, ig_intercept_wei) %>%
  # Change NAs to 0 in FRAG_mean_dur_MVPA_day_wei
  mutate(FRAG_mean_dur_MVPA_day_wei = if_else(is.na(FRAG_mean_dur_MVPA_day_wei) == T, 0, FRAG_mean_dur_MVPA_day_wei)) %>%
  # Correct m5 timing 
  mutate(M5TIME_num_wei = if_else(M5TIME_num_wei > 24, M5TIME_num_wei - 24, M5TIME_num_wei)) %>%
  # Change name of the variables
  tidyr::gather(key = "var", value = "value", -stno) %>% 
  left_join(tab.name, by = "var") %>% 
  select(-var) %>% 
  tidyr::spread(key = "varname", value = "value")

# Scale the data
data_s <- scale(data[,-1])
gather(data.frame(data_s), "variable", "value") %>% 
  group_by(variable) %>% 
  summarise(mean = round(mean(value)), # should be 0 for each variable
            sd = round(sd(value)))

# > Covariates (N = 4006)
load("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\00_DATA\\tab_full_cov_s11.rda")

# > Covariates, full data (N = 3893)
load("E:\\PC_FIXE\\Analysis\\02_ARTICLE_2\\00_DATA\\tab_cov_s11.rda")

# > Outcomes (n = 4006)
# CVD
data_cvd <- read_dta("E:\\PC_FIXE\\Data\\03_SCALAR_VAR\\data_L40M100_052020_forMathide.dta")

# > Merge PA metrics, covariates and outcomes
data2 <- data %>% 
  left_join(tab_11, by = "stno") %>% 
  left_join(data_cvd %>% select(stno, cvd2_incident), by = "stno")

dim(data2)  

# -----------------------------
# (1) 1D, 2D and 3D plots: hypotheses of clusters;

# 1D plots
# Variables distribution 
summary(data)
# histogram
ggplot() + 
  geom_histogram(data = data %>% 
                   gather(key = "variable", value = "value", -stno),
                 aes(value), 
                 fill = pal[1]) + 
  geom_vline(data = data %>% 
               gather(key = "variable", value = "value", -stno) %>% 
               group_by(variable) %>% 
               summarise(med = median(value)), 
             aes(xintercept = med), 
             linetype = 2, col = pal[5]) +
  geom_boxplot(data = data %>% 
                 gather(key = "variable", value = "value", -stno),
               aes(x = value, y = -100), 
               width = 150, col = pal[5], outlier.shape = NA) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.grid = element_blank())
# density
ggplot() + 
  geom_density(data = data %>% 
                 gather(key = "variable", value = "value", -stno),
               aes(value), 
               fill = pal[2], col = pal[1]) + 
  geom_vline(data = data %>% 
               gather(key = "variable", value = "value", -stno) %>% 
               group_by(variable) %>% 
               summarise(med = median(value)), 
             aes(xintercept = med), 
             linetype = 2, col = pal[5]) +
  geom_vline(data = data %>% 
               gather(key = "variable", value = "value", -stno) %>% 
               group_by(variable) %>% 
               summarise(IQR1 = quantile(value, 0.25)), 
             aes(xintercept = IQR1), 
             linetype = 2, col = pal[5]) +
  geom_vline(data = data %>% 
               gather(key = "variable", value = "value", -stno) %>% 
               group_by(variable) %>% 
               summarise(IQR3 = quantile(value, 0.75)), 
             aes(xintercept = IQR3), 
             linetype = 2, col = pal[5]) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.grid = element_blank())

# 2D plots 
data_s <- data.frame(data_s)
# Plotting pairs of variables
for(v in names(data_s)){
  
  med_v <- median(data_s[,grepl(v, names(data_s))])
  
  var_v <- names(data_s)[!grepl(v, names(data_s))]
  
  # Compute correlation between the selected variable and other variables
  tab_cor <- data_s %>% 
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
    # X median (the select variable)
    geom_vline(xintercept = med_v,
               col = pal[4], lty = 2) + 
    # Y median
    geom_hline(data = tab_cor,
               aes(yintercept = med),
               col = pal[4], lty = 2) + 
    # Scatter plots Y ~ X
    geom_point(data = data_s %>% 
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
  ggsave(filename = paste0("E://PC_FIXE//Analysis//02_ARTICLE_2//01_EXPLORATORY_ANALYSES//plots//", v, ".png"), 
         plot = p, width = 8, height = 8)

}


p <- ggpairs(iris[, -5])+ theme_bw()


# Grand tour
animate(data_s,
        tour_path = grand_tour(),
        display = display_xy()
)

# -----------------------------
# (2) Description of the population 
#     (means and SD for descriptive variables for the full sample, then stratified by sex, by outcomes); 

# Merge data
data3 <- tab_11_fin %>% 
  left_join(data, by = "stno") %>% 
  left_join(data_cvd %>% select(stno, cvd2_incident), by = "stno")

# Mise en forme
data3_m <- data3 %>% 
  mutate(sex = if_else(sex == 2, "Women", "Men"),
         ethnicity_i = if_else(ethnicity_i == 0, "White", "Non-white"),
         edu_imp = if_else(edu_imp == 1, "Primary school and lower", 
                   if_else(edu_imp == 2, "Lower secondary school", 
                   if_else(edu_imp == 3, "Higher secondary school", 
                   if_else(edu_imp == 4, "University", 
                                         "Higher degree")))),
         flgrlump_i_ordinal = if_else(flgrlump_i_ordinal == 0,   "Administrative",
                              if_else(flgrlump_i_ordinal == 0.5, "Prof/exec", 
                                                                 "Clerical/support")), 
         fstatusx_i_2 = if_else(fstatusx_i_2 == 0, "Married/cohabitating", "Not married/cohabitating"), 
         funitwk0_i_3 = if_else(funitwk0_i_3 == 0, "Alcohol intake: none",
                        if_else(funitwk0_i_3 == 1, "Alcohol intake: 1-14 units/week", 
                                                   "Alcohol intake: more than 14 units/week")), 
         fesmoke_i = if_else(fesmoke_i == 1, "Smoking status: never smokers", 
                     if_else(fesmoke_i == 2, "Smoking status: ex-smokers", 
                                             "Smoking status: current smokers")), 
         ffruitvg_i_3 = if_else(ffruitvg_i_3 == 0, "Fruits and vegetables intake: none", 
                        if_else(ffruitvg_i_3 == 1, "Fruits and vegetables intake: daily", 
                                                   "Fruits and vegetables intake: more than daily")),
         fbmi_i_3 = if_else(fbmi_i_3 == 0, "BMI: normal", 
                    if_else(fbmi_i_3 == 1, "BMI: overweight", 
                                           "BMI: obese")),
         hypertension = if_else(hypertension == 0, "No hypertension", "Hypertension"),
         hyperlipidemia = if_else(hyperlipidemia == 0, "No hyperlipidemia", "Hyperlipidemia"),
         prevalent_diabete = if_else(prevalent_diabete == 0, "No prevalent diabetes", "Prevalent diabetes")) %>%
  rename("A0. Age, mean (SD)"             = "fage_s",
         "A1. Sex"                        = "sex",
         "A2. Ethnicity"                  = "ethnicity_i", 
         "A3. Education"                  = "edu_imp",
         "A4. Occupational position"      = "flgrlump_i_ordinal", 
         "A5. Marital status"             = "fstatusx_i_2", 
         "B1. Smoking status"             = "fesmoke_i",
         "B2. Alcohol intake"             = "funitwk0_i_3", 
         "B3. Fruits & vegetables intake" = "ffruitvg_i_3",
         "C1. BMI"                        = "fbmi_i_3",
         "C2. Hypertension"               = "hypertension",
         "C3. Prevalent diabetes"         = "prevalent_diabete",
         "C4. Hyperlipidemia"             = "hyperlipidemia",
         "C5. Multimorbidity index"       = "mmm_index")


# Descriptive table for full sample
# > Categorical and binary variables
t1 <- data3_m %>% 
  select(stno, 
         "A1. Sex","A2. Ethnicity","A3. Education", "A4. Occupational position", "A5. Marital status", 
         "B1. Smoking status", "B2. Alcohol intake", "B3. Fruits & vegetables intake",
         "C1. BMI", "C2. Hypertension", "C3. Prevalent diabetes", "C4. Hyperlipidemia", "C5. Multimorbidity index") %>%
  gather(key = "Variables", value = "value", -stno) %>%
  group_by(Variables, value) %>% 
  count() %>%
  group_by(Variables) %>%
  mutate(freq = (n/sum(n))*100) %>% 
  mutate(lab = paste0(n, " (", format(round(freq, digits = 1), nsmall = 1), ")")) %>% 
  ungroup() %>% 
  select(-n, -freq) %>% 
  rename("Value" = "value") %>% 
  mutate(Value = factor(Value, levels = c("Men", "Women", 
                                          "White", "Non-white",
                                          "Administrative", "Prof/exec", "Clerical/support",
                                          "Primary school and lower", "Lower secondary school", "Higher secondary school", "University", "Higher degree",
                                          "Married/cohabitating", "Not married/cohabitating", 
                                          "Smoking status: never smokers", "Smoking status: ex-smokers", "Smoking status: current smokers", 
                                          "Alcohol intake: none", "Alcohol intake: 1-14 units/week", "Alcohol intake: more than 14 units/week", 
                                          "Fruits and vegetables intake: none", "Fruits and vegetables intake: daily", "Fruits and vegetables intake: more than daily",
                                          "BMI: normal", "BMI: overweight", "BMI: obese",
                                          "No hypertension", "Hypertension",
                                          "No prevalent diabetes", "Prevalent diabetes",
                                          "No hyperlipidemia", "Hyperlipidemia",
                                          "0", "1", "2", "3", "4"
                                          ))) %>% 
  arrange(Value) %>%
  mutate(Value = as.character(Value))
  

# > Variables quantitatives  
mean_age <- format(round(mean(data3$fage_s), 1), nsmall = 1)
sd_age <- format(round(sd(data3$fage_s), 1), nsmall = 1)

t2 <- data.frame(
  Variables = "A0. Age, mean (SD)",
  Value = NA,
  lab = paste0(mean_age, " (", sd_age, ")")
)

t2 <- data3_m %>% 
  select(stno, 
         "A0. Age, mean (SD)",
         "Acceleration (mg)",
         # Time in SB, LIPA and MVPA
         "Duration IN (min/day)", "Duration LIPA (min/day)", "Duration MVPA (min/day)",
         # Mean duration of bouts of SB, LIPA and MVPA
         "Mean duration of IN bouts", "Mean duration of LIPA bouts", "Mean duration of MVPA bouts",
         # Number of bouts of SB, LIPA and MVPA
         "Number of IN bouts", "Number of LIPA bouts", "Number of MVPA bouts",
         # Mean acceleration and timing of the most 5 active hours
         "Most active 5hrs acceleration (g)", "Most active 5hrs timing",
         # Intensity gradient
         "Intensity gradient slope", "Intensity gradient intercept") %>%
  gather("Variables", "value", -stno) %>% 
  group_by(Variables) %>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% 
  mutate(lab = paste0(format(round(mean, digits = 1), nsmall = 1), " (", format(round(sd, digits = 1), nsmall = 1), ")"),
         Value = NA) %>% 
  select(Variables, Value, lab)


# > Table
T_full <- rbind(t1, t2)



# Descriptive table, stratified on sex

data3_m %>%
  select(stno, sex_2,
         "A2. Ethnicity","A3. Education", "A4. Occupational position", "A5. Marital status", 
         "B1. Smoking status", "B2. Alcohol intake", "B3. Fruits & vegetables intake",
         "C1. BMI", "C2. Hypertension", "C3. Prevalent diabetes", "C4. Hyperlipidemia", "C5. Multimorbidity index") %>%
  mutate(sex_2 = if_else(sex_2 == 0, "Women", "Men")) %>%
  gather(key = "Variables", value = "value", -stno, -sex_2) %>%
  group_by(Variables, value, sex_2) %>% 
  count() %>%
  group_by(sex_2, Variables) %>%
  mutate(freq = (n/sum(n))*100) %>% 
  mutate(lab = paste0(n, " (", format(round(freq, digits = 1), nsmall = 1), ")")) %>%
  ungroup() %>% 
  select(-n, -freq) %>%
  spread(key = sex_2, value = "lab") %>% 
  rename("Value" = "value")




