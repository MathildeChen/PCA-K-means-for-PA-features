# Script check data from GGIR
# GGIR release from 20th April 2021 

# Packages
library(tidyverse)
library(haven)
library(ggplot2)
library(testthat)

# =========================================================
# Data
# > Sample participants (n = 4008)
sample_stno <- read_dta("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2021-05-03\\data_03052021.dta")

# > Person level summary
data <- read.csv("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2021-05-07\\part5_personsummary_WW_L40M100V400_T5A5.csv") %>%
  rename("stno" = "ID") %>%
  filter(stno %in% unique(sample_stno$stno))

# > Day level summary
data_day <- read.csv("E:\\PC_FIXE\\Data\\04_DAILY_ACT_SUM\\2021-05-07\\part5_daysummary_WW_L40M100V400_T5A5.csv") %>%
  rename("stno" = "ID") %>%
  filter(stno %in% unique(sample_stno$stno))

# > Number of day per person
data_day %>% 
  group_by(stno) %>% 
  summarise(n_day = n()) %>% 
  group_by(n_day) %>% 
  summarise(n_n_day = n())

19+24+53+3912 # 4008

# =========================================================
# Tests Nblocks vs Nbouts
# Compare Nblocks vs Nbouts
# As we decided to not include any grace period, 
# here blocks = bouts

# > Person-level summary
# ----------------------
# IN
summary(data$Nbouts_day_IN_bts_10_30_wei)
summary(data$Nblocks_day_IN_bts_10_30_wei)
expect_equal(data$Nbouts_day_IN_bts_10_30_wei, data$Nblocks_day_IN_bts_10_30_wei)

summary(data$Nbouts_day_IN_bts_30_wei)
summary(data$Nblocks_day_IN_bts_30_wei)
expect_equal(data$Nbouts_day_IN_bts_30_wei, data$Nblocks_day_IN_bts_30_wei)

# LIPA
summary(data$Nbouts_day_LIG_bts_10_wei)
summary(data$Nblocks_day_LIG_bts_10_wei)
expect_equal(data$Nbouts_day_LIG_bts_10_wei, data$Nblocks_day_LIG_bts_10_wei)

# MVPA
summary(data$Nbouts_day_MVPA_bts_10_wei)
summary(data$Nblocks_day_MVPA_bts_10_wei)
expect_equal(data$Nbouts_day_MVPA_bts_10_wei, data$Nblocks_day_MVPA_bts_10_wei)

# -> OK 
# ----------------------

# > Day-level summary
# ----------------------
# IN
summary(data_day$Nbouts_day_IN_bts_10_30)
summary(data_day$Nblocks_day_IN_bts_10_30)
expect_equal(data_day$Nbouts_day_IN_bts_10_30, data_day$Nblocks_day_IN_bts_10_30)

summary(data_day$Nbouts_day_IN_bts_30)
summary(data_day$Nblocks_day_IN_bts_30)
expect_equal(data_day$Nbouts_day_IN_bts_30, data_day$Nblocks_day_IN_bts_30)

# LIPA
summary(data_day$Nbouts_day_LIG_bts_10)
summary(data_day$Nblocks_day_LIG_bts_10)
expect_equal(data_day$Nbouts_day_LIG_bts_10, data_day$Nblocks_day_LIG_bts_10)

# MVPA
summary(data_day$Nbouts_day_MVPA_bts_10)
summary(data_day$Nblocks_day_MVPA_bts_10)
expect_equal(data_day$Nbouts_day_MVPA_bts_10, data_day$Nblocks_day_MVPA_bts_10)

# -> OK 

# =========================================================
# Total Nblocks calculations
# Check if the total number of bouts is the sum of bt and unbt blocks

# Compute total number of Nblocks
test <- data %>%
  mutate(
    # IN
    Nblocks_day_total_IN_wei_test = Nblocks_day_IN_unbt_wei + Nblocks_day_IN_bts_10_30_wei + Nblocks_day_IN_bts_30_wei,
    # LIPA
    Nblocks_day_total_LIG_wei_test = Nblocks_day_LIG_unbt_wei + Nblocks_day_LIG_bts_10_wei,
    # MVPA (2 different ways)
    Nblocks_day_total_MVPA_wei_1 = Nblocks_day_MOD_unbt_wei + Nblocks_day_VIG_unbt_wei + Nblocks_day_MVPA_bts_10_wei,
    Nblocks_day_total_MVPA_wei_2 = Nblocks_day_total_MOD_wei + Nblocks_day_total_VIG_wei) 

test_day <- data_day %>%
  mutate(
    # IN
    Nblocks_day_total_IN_test = Nblocks_day_IN_unbt + Nblocks_day_IN_bts_10_30 + Nblocks_day_IN_bts_30,
    # LIPA
    Nblocks_day_total_LIG_test = Nblocks_day_LIG_unbt + Nblocks_day_LIG_bts_10,
    # MVPA
    Nblocks_day_total_MVPA_test_1 = Nblocks_day_MOD_unbt + Nblocks_day_VIG_unbt + Nblocks_day_MVPA_bts_10,
    Nblocks_day_total_MVPA_test_2 = Nblocks_day_total_MOD + Nblocks_day_total_VIG)

# > Person-level summary
# ----------------------

# IN
summary(test$Nblocks_day_total_IN_wei_test)
summary(test$Nblocks_day_total_IN_wei)
expect_equal(test$Nblocks_day_total_IN_wei_test, test$Nblocks_day_total_IN_wei)
# --> OK 

# LIPA
summary(test$Nblocks_day_total_LIG_wei_test)
summary(test$Nblocks_day_total_LIG_wei)
expect_equal(test$Nblocks_day_total_LIG_wei_test, test$Nblocks_day_total_LIG_wei)
# --> OK 

# MVPA
summary(test$Nblocks_day_total_MVPA_wei_1) # Nblocks_day_total_MVPA_wei_1 = Nblocks_day_MOD_unbt_wei + Nblocks_day_VIG_unbt_wei + Nblocks_day_MVPA_bts_10_wei
summary(test$Nblocks_day_total_MVPA_wei_2) # Nblocks_day_total_MVPA_wei_2 = Nblocks_day_total_MOD_wei + Nblocks_day_total_VIG_wei
expect_equal(test$Nblocks_day_total_MVPA_wei_1, test$Nblocks_day_total_MVPA_wei_2)
# > average diff: 1.04
# > THIS IS BECAUSE A SINGLE MVPA BOUT CAN REPRESENT MULTIPLE MOD AND/OR VIG BLOCKS. 

# ----------------------
# > Day-level summary
# ----------------------

# IN 
summary(test_day$Nblocks_day_total_IN)
summary(test_day$Nblocks_day_total_IN_test)
expect_equal(test_day$Nblocks_day_total_IN, test_day$Nblocks_day_total_IN_test)
# --> OK 

# LIPA
summary(test_day$Nblocks_day_total_LIG_test)
summary(test_day$Nblocks_day_total_LIG)
expect_equal(test_day$Nblocks_day_total_LIG_test, test_day$Nblocks_day_total_LIG)
# --> OK 

# MVPA 
summary(test_day$Nblocks_day_total_MVPA_test_1)
summary(test_day$Nblocks_day_total_MVPA_test_2)
expect_equal(test_day$Nblocks_day_total_MVPA_test_1, test_day$Nblocks_day_total_MVPA_test_2)
# > average diff: 5.82
# > THIS IS BECAUSE A SINGLE MVPA BOUT CAN REPRESENT MULTIPLE MOD AND/OR VIG BLOCKS. 

# ----------------------

# Observation: 
# --> Nb of bouts corresponds to nb of blocks for IN, LIPA and MVPA (small difference due to the definition of MVPA bouts vs MOD and VIG bouts)
# --> Total nb of bouts corresponds to the nb of unbt and bts "blocks"

# =========================================================
# Comparaison with Nfragments variables
# Check if total nb of fragments is equal to total number of bouts

# > Person-level summary
# ----------------------

# Compare our computations vs Nblocks_day_total_.. variables
# IN
summary(test$Nblocks_day_total_IN_wei)
summary(test$FRAG_Nfrag_IN_day_wei)
expect_equal(test$Nblocks_day_total_IN_wei, test$FRAG_Nfrag_IN_day_wei)
# OK

# LIPA
summary(test$Nblocks_day_total_LIG_wei)
summary(test$FRAG_Nfrag_LIPA_day_wei)
expect_equal(test$Nblocks_day_total_LIG_wei, test$FRAG_Nfrag_LIPA_day_wei)
# OK

# MVPA 
summary(test$Nblocks_day_total_MVPA_wei_1) # Nblocks_day_total_MVPA_wei_1 = MOD_unbt + VIG_unbt + MVPA_bts_10
summary(test$Nblocks_day_total_MVPA_wei_2) # Nblocks_day_total_MVPA_wei_2 = total_MOD + total_VIG
summary(test$FRAG_Nfrag_MVPA_day_wei)
expect_equal(test$Nblocks_day_total_MVPA_wei_1, test$Nblocks_day_total_MVPA_wei_2)
# average diff: 1.04
expect_equal(test$Nblocks_day_total_MVPA_wei_1, test$FRAG_Nfrag_MVPA_day_wei)
# average diff: 0.594
expect_equal(test$FRAG_Nfrag_MVPA_day_wei, test$Nblocks_day_total_MVPA_wei_2)
# average diff: 1.46

# ----------------------

# > Day-level summary
# ----------------------

summary(test_day$Nblocks_day_total_IN)
summary(test_day$FRAG_Nfrag_IN_day)
expect_equal(test_day$Nblocks_day_total_IN, test_day$FRAG_Nfrag_IN_day)
# OK

summary(test_day$Nblocks_day_total_LIG)
summary(test_day$FRAG_Nfrag_LIPA_day)
expect_equal(test_day$Nblocks_day_total_LIG, test_day$FRAG_Nfrag_LIPA_day)
# average diff: OK

summary(test_day$Nblocks_day_total_MVPA_test_1)
summary(test_day$Nblocks_day_total_MVPA_test_2)
summary(test_day$FRAG_Nfrag_MVPA_day)
expect_equal(test_day$Nblocks_day_total_MVPA_test_1, test_day$FRAG_Nfrag_MVPA_day)
# average diff: 2.46
expect_equal(test_day$Nblocks_day_total_MVPA_test_2, test_day$FRAG_Nfrag_MVPA_day)
# average diff: 4.22

# Observation:
# --> nb of bouts/blocks and nb of fragments equal for IN and LIPA
# --> Some differences between number of bouts and number of fragments for MVPA, 
# certainly explained by the fact that 1 MVPA fragment can corresponds to a different
# number of MOD nd/or VIG blocks

# =========================================================
# Check nb of fragments value according to nb of bouts in a day (day level summary)

# > IN
# Check nb of fragments when nb of bouts < 10
summary(test_day[which(test_day$Nblocks_day_total_IN < 10), "FRAG_Nfrag_IN_day"])
test_day[which(test_day$Nblocks_day_total_IN < 10), c("calendar_date", "FRAG_Nfrag_IN_day", "Nblocks_day_total_IN")]
# --> all < 10 OK 

# Check nb of fragments when nb of bouts >= 10 
summary(test_day[which(test_day$Nblocks_day_total_IN >= 10), "FRAG_Nfrag_IN_day"])
# --> Min = 10 and no NA: OK 
expect_equal(test_day[which(test_day$Nblocks_day_total_IN >= 10),"Nblocks_day_total_IN"],
             test_day[which(test_day$Nblocks_day_total_IN >= 10),"FRAG_Nfrag_IN_day"])
# --> OK 

# > LIPA
# Check nb of fragments when nb of bouts < 10
summary(test_day[which(test_day$Nblocks_day_total_LIG < 10), "FRAG_Nfrag_LIPA_day"])
test_day[which(test_day$Nblocks_day_total_LIG < 10),c("calendar_date", "Nblocks_day_total_LIG", "FRAG_Nfrag_LIPA_day")]
# --> all < 10 OK

# Check nb of fragments when nb of bouts >= 10 
summary(test_day[which(test_day$Nblocks_day_total_LIG >= 10), "FRAG_Nfrag_LIPA_day"])
# --> OK
expect_equal(test_day[which(test_day$Nblocks_day_total_LIG >= 10),"Nblocks_day_total_LIG"], 
             test_day[which(test_day$Nblocks_day_total_LIG >= 10),"FRAG_Nfrag_LIPA_day"])
# --> OK

# > MVPA
# Check nb of fragments when nb of bouts < 10
summary(test_day[which(test_day$Nblocks_day_total_MVPA_test_1 < 10), "FRAG_Nfrag_MVPA_day"])
summary(test_day[which(test_day$Nblocks_day_total_MVPA_test_2 < 10), "FRAG_Nfrag_MVPA_day"])
# --> all < 10 OK

test_day[which(test_day$Nblocks_day_total_MVPA_test_1 < 10),c("calendar_date", "Nblocks_day_total_MVPA_test_1", "FRAG_Nfrag_MVPA_day")]
test_day[which(test_day$Nblocks_day_total_MVPA_test_2 < 10),c("calendar_date", "Nblocks_day_total_MVPA_test_2", "FRAG_Nfrag_MVPA_day")]

expect_equal(test_day[which(test_day$Nblocks_day_total_MVPA_test_1 >= 10),"Nblocks_day_total_MVPA_test_1"], 
             test_day[which(test_day$Nblocks_day_total_MVPA_test_1 >= 10),"FRAG_Nfrag_MVPA_day"])
# --> average diff: 2.48
expect_equal(test_day[which(test_day$Nblocks_day_total_MVPA_test_2 >= 10),"Nblocks_day_total_MVPA_test_2"], 
             test_day[which(test_day$Nblocks_day_total_MVPA_test_2 >= 10),"FRAG_Nfrag_MVPA_day"])
# --> average diff: 4.27

# Check nb of fragments when nb of bouts > 10 
summary(test_day[which(test_day$Nblocks_day_total_MVPA_test_1 >= 10), "FRAG_Nfrag_MVPA_day"])
# --> Min = 6 (<10) 
summary(test_day[which(test_day$Nblocks_day_total_MVPA_test_2 >= 10), "FRAG_Nfrag_MVPA_day"])
# --> Min = 6 (<10) 

expect_equal(test_day[which(test_day$Nblocks_day_total_MVPA_test_1 >= 10),"Nblocks_day_total_MVPA_test_1"], 
             test_day[which(test_day$Nblocks_day_total_MVPA_test_1 >= 10),"FRAG_Nfrag_MVPA_day"])
# --> average diff: 2.48
expect_equal(test_day[which(test_day$Nblocks_day_total_MVPA_test_2 >= 10),"Nblocks_day_total_MVPA_test_2"], 
             test_day[which(test_day$Nblocks_day_total_MVPA_test_2 >= 10),"FRAG_Nfrag_MVPA_day"])
# --> average diff: 4.27

# Days were Nblock >= 10 while Nfrag < 10
test_day %>% 
  filter(Nblocks_day_total_MVPA_test_1 >= 10,
         FRAG_Nfrag_MVPA_day < 10) %>% 
  select(stno, calendar_date, Nblocks_day_total_MVPA_test_1, FRAG_Nfrag_MVPA_day)

# --> For MVPA: Some days have Nblock >= 10 while Nfrag < 10

# > Check for bout criteria
# "Metrics related to Gini, CoV, or power spectrum now only calculated if the 
# standard deviation in fragment duration is not zero, and if there are at least 10 
# fragments (5 inactive and 5 active). If this condition is not met the values will be NA."
test_day %>% 
  filter(FRAG_Nfrag_IN_day < 5,
         FRAG_Nfrag_PA_day < 5) %>% 
  select(stno, calendar_date, Nblocks_day_total_IN, Nblocks_day_total_LIG, Nblocks_day_total_MOD, Nblocks_day_total_VIG)

test_day %>% 
  filter(FRAG_Nfrag_IN_day < 10,
         FRAG_Nfrag_PA_day < 10) %>% 
  select(stno, calendar_date, Nblocks_day_total_IN, Nblocks_day_total_LIG, Nblocks_day_total_MOD, Nblocks_day_total_VIG)


test_day %>% 
  filter(FRAG_Nfrag_IN_day < 5,
         FRAG_Nfrag_PA_day < 5) %>% 
  select(stno, calendar_date, FRAG_Gini_dur_IN_day, FRAG_Gini_dur_PA_day)
# Gini is NA when FRAG_Nfrag_IN_day < 5 and FRAG_Nfrag_PA_day < 5 --> OK 

test_day %>% 
  filter(is.na(FRAG_Gini_dur_PA_day) == T) %>% 
    select(stno, calendar_date, FRAG_Nfrag_IN_day, FRAG_Nfrag_PA_day, FRAG_Gini_dur_IN_day, FRAG_Gini_dur_PA_day, FRAG_CoV_dur_IN_day, FRAG_CoV_dur_PA_day, FRAG_alpha_dur_IN_day, FRAG_alpha_dur_PA_day, FRAG_x0.5_dur_IN_day, FRAG_x0.5_dur_PA_day, FRAG_W0.5_dur_IN_day, FRAG_W0.5_dur_PA_day, Nblocks_day_total_IN, Nblocks_day_total_LIG, Nblocks_day_total_MOD, Nblocks_day_total_VIG)
# FRAG_Nfrag_IN_day < 5 and FRAG_Nfrag_PA_day < 5 when Gini is NA --> OK 

summary(t$FRAG_Nfrag_IN_day)
summary(t$FRAG_Nfrag_PA_day)
# --> OK 


# "Other metrics related to binary fragmentation (mean_dur_PA, mean_dur_IN, Nfrag_PA, 
# Nfrag_IN), are calculated when there are at least 2 fragments (1 inactive, 1 active). 
# If this condition is not met the value will is set to zero."
test_day %>% 
  filter(FRAG_Nfrag_IN_day < 5,
         FRAG_Nfrag_PA_day < 5) %>% 
  select(stno, calendar_date, FRAG_Nfrag_IN_day, FRAG_Nfrag_PA_day, FRAG_mean_dur_IN_day, FRAG_mean_dur_PA_day)
# --> OK : 2 days, mean bouts duration is computed even if there is low nb of bouts

summary(data_day$FRAG_mean_dur_IN_day)
summary(data_day$FRAG_mean_dur_LIPA_day)
summary(data_day$FRAG_mean_dur_PA_day)
summary(data_day$FRAG_mean_dur_MVPA_day)
# Only mean bouts duration == 0 for MVPA 

t2 <- test_day %>% 
  filter(FRAG_mean_dur_MVPA_day == 0) %>% 
  select(stno, calendar_date, FRAG_Nfrag_MVPA_day, dur_day_total_MOD_min, dur_day_total_VIG_min, FRAG_mean_dur_MVPA_day)
summary(t2$FRAG_Nfrag_MVPA_day)
# a void data set --> OK 

# "Metrics related to TP now only calculated if there is at least 1 
# inactivity fragment AND (1 LIPA OR 1 MVPA). If this condition is not met the 
# TP metric value is set to zero."

t3 <- test_day %>% 
  filter(FRAG_Nfrag_PA_day < 5) %>% 
  select(stno, calendar_date, FRAG_Nfrag_IN_day, FRAG_Nfrag_LIPA_day, FRAG_Nfrag_MVPA_day, starts_with("FRAG_TP_"))

t3 %>% filter(FRAG_Nfrag_MVPA_day == 0) %>% summary(.)
# FRAG_TP_IN2MVPA_day = 0 when FRAG_Nfrag_MVPA_day = 0: OK

t3 %>% filter(FRAG_Nfrag_LIPA_day == 1) %>% summary(.)
# No days with less than 1 LIPA fragments

test_day %>% filter(FRAG_TP_PA2IN_day == 0) # 0
test_day %>% filter(FRAG_TP_IN2PA_day == 0) # 0
test_day %>% filter(FRAG_TP_IN2LIPA_day == 0) # 0
test_day %>% 
  filter(FRAG_TP_IN2MVPA_day == 0) %>% # if TP = 0
  filter(FRAG_Nfrag_IN_day < 2, FRAG_Nfrag_MVPA_day < 2) %>% # there should be no fragments
  select(stno, calendar_date, FRAG_Nfrag_IN_day, FRAG_Nfrag_MVPA_day, FRAG_TP_IN2MVPA_day)
# 0

# --> OK 
# =========================================================
# Check mean bouts duration computation 
# > At day level: mean bouts duration = total time in activity / total number of fragments in activity
# > At person level: mean bouts duration = mean of the mean bouts duration on all days (no critera of 10 fragments minimum for computation)

# > IN
test_day_IN <- test_day %>% 
  mutate(mean_IN_dur_day = if_else(FRAG_Nfrag_IN_day == 0, 0, dur_day_total_IN_min / FRAG_Nfrag_IN_day))

summary(test_day_IN$mean_IN_dur_day)
summary(test_day_IN$FRAG_mean_dur_IN_day)
# OK at day level

t <- test_day_IN %>% 
  group_by(stno) %>% 
  summarise(mean_IN_dur_test = mean(mean_IN_dur_day)) %>% 
  left_join(test %>% select(stno, FRAG_mean_dur_IN_day_pla), by = "stno") 

expect_equal(t$mean_IN_dur_test, t$FRAG_mean_dur_IN_day_pla)
summary(t$mean_IN_dur_test)
summary(t$FRAG_mean_dur_IN_day_pla)
# OK at person level

# > LIPA
# As difference between nb of bouts and nb of frgaments, check with both
test_day_LIPA <- test_day %>% 
  mutate(mean_IN_dur_day_1 = if_else(FRAG_Nfrag_LIPA_day == 0, 0, dur_day_total_LIG_min / FRAG_Nfrag_LIPA_day))

summary(test_day_LIPA$mean_IN_dur_day_1)
summary(test_day_LIPA$FRAG_mean_dur_LIPA_day)
expect_equal(test_day_LIPA$mean_IN_dur_day_1, test_day_LIPA$FRAG_mean_dur_LIPA_day)
# OK at day level

t2 <- test_day_LIPA %>% 
  group_by(stno) %>% 
  summarise(mean_LIPA_dur_test = mean(mean_IN_dur_day_1)) %>% 
  left_join(test %>% select(stno, FRAG_mean_dur_LIPA_day_pla), by = "stno") 

summary(t2$mean_LIPA_dur_test)
summary(t2$FRAG_mean_dur_LIPA_day_pla)
expect_equal(t2$mean_LIPA_dur_test, t2$FRAG_mean_dur_LIPA_day_pla)
# OK at person level

# > MVPA
test_day_MVPA <- test_day %>% 
  mutate(mean_MVPA_dur_day1 = if_else(FRAG_Nfrag_MVPA_day == 0, 0, (dur_day_total_MOD_min + dur_day_total_VIG_min) / FRAG_Nfrag_MVPA_day))

summary(test_day_MVPA$mean_MVPA_dur_day1)
summary(test_day_MVPA$FRAG_mean_dur_MVPA_day)
# no more NA ok
expect_equal(test_day_MVPA$mean_MVPA_dur_day1, test_day_MVPA$FRAG_mean_dur_MVPA_day)
# OK at day level 

test_day %>% filter(FRAG_Nfrag_MVPA_day == 0) %>% 
  select(stno, FRAG_mean_dur_MVPA_day) %>% 
  summary(.)
# OK

t3 <- test_day_MVPA %>% 
  group_by(stno) %>% 
  summarise(mean_MVPA_dur_test = mean(mean_MVPA_dur_day1)) %>% 
  left_join(test %>% select(stno, FRAG_mean_dur_MVPA_day_pla), by = "stno") 

summary(t3$mean_MVPA_dur_test)
summary(t3$FRAG_mean_dur_MVPA_day_pla)
expect_equal(t3$mean_MVPA_dur_test, t3$FRAG_mean_dur_MVPA_day_pla)
# OK at person level 

# =========================================================
# Nvaliddays computation
# Is Nvaliddays = to the nb of days with > 10 fragments of IN?

test_day_IN <- test_day %>% 
  group_by(stno) %>% 
  mutate(Ndays = n()) %>% 
  # Keep only days with nb bouts > 10 
  filter(Nblocks_day_total_IN >= 10) %>% 
  group_by(stno) %>% 
  mutate(Nvaliddays_test = n(), # N days
         FRAG_Nfrag_IN_day_test = sum(FRAG_Nfrag_IN_day)/Nvaliddays_test) %>% # Total number of fragments
  # Add person level estimates 
  left_join(data %>% 
              select(stno, Nvaliddays, FRAG_Nfrag_IN_day_pla), 
            by = "stno") %>% 
  select(stno, calendar_date, Ndays, Nvaliddays_test, Nvaliddays, FRAG_Nfrag_IN_day_test, FRAG_Nfrag_IN_day_pla)

test_day_IN %>% 
  filter(Nvaliddays_test != Nvaliddays) 
# look for participants who have some days with < 10 IN fragments
# As the number of frag computed only based on the days > 10 fragments
# is different from the Nfrag at person level I understand that 
# Nfrag variables at person level is computed on all days 
# including those with < 10 fragments

# Check n valid days
expect_equal(test_day_IN$Nvaliddays_test, test_day_IN$Nvaliddays)
# average diff: 1.46, 92 mismatches
# Nvaliddays: number of days with data? 

# Check Nfrag variables
expect_equal(test_day_IN$FRAG_Nfrag_IN_day_test, test_day_IN$FRAG_Nfrag_IN_day_pla)
# --> average diff: 0.0178
summary(test_day_IN$FRAG_Nfrag_IN_day_test - test_day_IN$FRAG_Nfrag_IN_day_pla)
# --> similar estimates except for some values

test_day_IN %>% 
  filter(FRAG_Nfrag_IN_day_test - FRAG_Nfrag_IN_day_pla > 5) %>% 
  View(.)
# participants with some days where nb fragments < 10?

# Observation
# --> As the number of frag computed only based on the days > 10 fragments
# is different from the Nfrag at person level I understand that 
# Nfrag variables at person level is computed on all days 
# including those with < 10 fragments

# =========================================================
# Nvalids days checks
# during week days and weekend days
# does the criteria of the 10 fragments discard some participants? 

p7_wd <- data %>% 
  filter(Nvaliddays_AL10F_WD < 5) %>% 
  select(stno, Nvaliddays_AL10F_WD, Nvaliddays, Nvaliddays_WD)

data_day %>% 
  filter(stno %in% unique(p7_wd$stno), daytype == "WD") %>% 
  select(stno, calendar_date, daytype, nonwear_perc_day, starts_with("Nblocks_day_total")) %>% 
  group_by(stno) %>% 
  mutate(n = n()) %>%
  left_join(p7_wd, by = "stno") %>%
  filter(n != Nvaliddays_AL10F_WD, Nblocks_day_total_LIG<10) %>%
  View(.)

p7_we <- data %>% 
  filter(Nvaliddays_AL10F_WE < 2) %>% 
  select(stno, Nvaliddays_AL10F_WE, Nvaliddays, Nvaliddays_WE, FRAG_Gini_dur_IN_day_wei)

data_day %>% 
  filter(stno %in% unique(p7_we$stno), daytype == "WE") %>% 
  select(stno, calendar_date, daytype, nonwear_perc_day, starts_with("Nblocks_day_total")) %>% 
  group_by(stno) %>% 
  mutate(n = n()) %>%
  left_join(p7_we, by = "stno") %>%
  filter(n != Nvaliddays_AL10F_WE, Nblocks_day_total_IN < 10) %>% View(.)

# > Would discard 9 participants based on the weekend days

stop()

# Plot signal for the participants with less than 10 fragments 
# of IN during the weekend and 
# > Path of 60sec epoch files
path.files <- "E:\\PC_FIXE\\Data\\01_TIME_SERIES\\2020_05_15\\ms5.outraw\\40_100_400\\"

# > Load raw data from participants with high M5 timing
acc_data_highM5 <- data.frame(files = list.files(path = path.files,
                                                 pattern = ".csv$", 
                                                 recursive = TRUE)) %>% 
  mutate(stno = str_extract(files, "[0-9]+")) %>% 
  filter(stno %in% unique(dens$stno)) %>%
  split(.$stno, drop = T) %>%
  map_dfr(~ {
    
    i_dat <- vroom::vroom(file = paste0(path.files, unique(.x$files)), 
                          delim = ",")
    
  }, .id = "stno") %>% 
  # Compute logarithme of acc. data in g (because some data are skewed)
  mutate(ACC_g = ACC/1000, # convert acc in mg in acc in g
         log_ACC_g = log(1 + ACC_g), # compute the logarithm of the acc in g
         log_ACC = log(1 + ACC), # compute the logarithm of the acc in mg
         timestamp = as.POSIXct(timenum, origin="1970-1-1", tz="Europe/London")) # compute the timestamp

acc_data_highM5 %>% 
  #mutate(calendar_day = lubridate::ymd(timestamp)) %>%
  #left_join(dens, by = c("stno", "calendar_day"))
  ggplot(.) +
  geom_hline(yintercept = 0.04, lty = 2, col = pal[1]) + 
  geom_hline(yintercept = 0.1, lty = 2, col = pal[5]) + 
  geom_line(aes(x = timestamp, y = ACC_g), size = 0.01) + 
  facet_wrap(. ~ stno, scale = "free_x") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.grid = element_blank())








