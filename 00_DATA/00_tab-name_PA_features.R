# Table with PA metrics' name

tab.name <- tibble::tribble(
  ~var,                              ~varname,
  # Average acceleration
  "ACC_day_mg_wei",                  "Acceleration (mg) - All days",
  "ACC_day_mg_WD",                   "Acceleration (mg) - Week days",
  "ACC_day_mg_WE",                   "Acceleration (mg) - Weekend days",
  
  # Time in SB, LIPA and MVPA
  "dur_day_total_IN_min_wei",        "Duration SB (min/day) - All days",
  "dur_day_total_LIG_min_wei",       "Duration LIPA (min/day) - All days",
  "dur_day_total_MVPA_min_wei",      "Duration MVPA (min/day) - All days",
  
  "dur_day_total_IN_min_WD",         "Duration SB (min/day) - Week days",
  "dur_day_total_LIG_min_WD",        "Duration LIPA (min/day) - Week days",
  "dur_day_total_MVPA_min_WD",       "Duration MVPA (min/day) - Week days",
  
  "dur_day_total_IN_min_WE",         "Duration SB (min/day) - Weekend days",
  "dur_day_total_LIG_min_WE",        "Duration LIPA (min/day) - Weekend days",
  "dur_day_total_MVPA_min_WE",       "Duration MVPA (min/day) - Weekend days",
  
  # Mean duration of fragments of SB, LIPA and MVPA
  "FRAG_mean_dur_IN_day_wei",         "Mean duration of SB bouts - All days",
  "FRAG_mean_dur_LIPA_day_wei",       "Mean duration of LIPA bouts - All days",
  "FRAG_mean_dur_MVPA_day_wei",       "Mean duration of MVPA bouts - All days",
  
  "FRAG_mean_dur_IN_day_WD",          "Mean duration of SB bouts - Week days",
  "FRAG_mean_dur_LIPA_day_WD",        "Mean duration of LIPA bouts - Week days",
  "FRAG_mean_dur_MVPA_day_WD",        "Mean duration of MVPA bouts - Week days",
  "FRAG_mean_dur_IN_day_WE",          "Mean duration of SB bouts - Weekend days",
  "FRAG_mean_dur_LIPA_day_WE",        "Mean duration of LIPA bouts - Weekend days",
  "FRAG_mean_dur_MVPA_day_WE",        "Mean duration of MVPA bouts - Weekend days",
  
  
  # Number of fragments of SB, LIPA and MVPA
  "FRAG_Nfrag_IN_day_wei",            "Number of SB bouts - All days",
  "FRAG_Nfrag_LIPA_day_wei",          "Number of LIPA bouts - All days",
  "FRAG_Nfrag_MVPA_day_wei",          "Number of MVPA bouts - All days",
  "Nblocks_day_IN_unbt_wei",          "Number of < 10 min SB fragments (unbouted) - All days",
  "Nblocks_day_IN_bts_10_30_wei",     "Number of 10-30 min SB fragments - All days",
  "Nblocks_day_IN_bts_30_wei",        "Number of > 30 min SB fragments - All days",
  "Nblocks_day_LIG_unbt_wei",         "Number of < 10 min LIPA fragments (unbouted) - All days",
  "Nblocks_day_LIG_bts_10_wei",       "Number of > 10 min LIPA fragments - All days",
  "Nblocks_day_MVPA_unbt_wei",        "Number of < 10 min MVPA fragments (unbouted) - All days",
  "Nblocks_day_MVPA_bts_10_wei",      "Number of > 10 min MVPA fragments - All days",
  
  "FRAG_Nfrag_IN_day_WD",             "Number of SB bouts - Week days",
  "FRAG_Nfrag_LIPA_day_WD",           "Number of LIPA bouts - Week days",
  "FRAG_Nfrag_MVPA_day_WD",           "Number of MVPA bouts - Week days",
  "Nblocks_day_IN_unbt_WD",           "Number of < 10 min SB fragments (unbouted) - Week days",
  "Nblocks_day_IN_bts_10_30_WD",      "Number of 10-30 min SB fragments - Week days",
  "Nblocks_day_IN_bts_30_WD",         "Number of > 30 min SB fragments - Week days",
  "Nblocks_day_LIG_unbt_WD",          "Number of < 10 min LIPA fragments (unbouted) - Week days",
  "Nblocks_day_LIG_bts_10_WD",        "Number of > 10 min LIPA fragments - Week days",
  "Nblocks_day_MVPA_unbt_WD",         "Number of < 10 min MVPA fragments (unbouted) - Week days",
  "Nblocks_day_MVPA_bts_10_WD",       "Number of > 10 min MVPA fragments - Week days",
  
  "FRAG_Nfrag_IN_day_WE",             "Number of SB bouts - Weekend days",
  "FRAG_Nfrag_LIPA_day_WE",           "Number of LIPA bouts - Weekend days",
  "FRAG_Nfrag_MVPA_day_WE",           "Number of MVPA bouts - Weekend days",
  "Nblocks_day_IN_unbt_WE",           "Number of < 10 min SB fragments (unbouted) - Weekend days",
  "Nblocks_day_IN_bts_10_30_WE",      "Number of 10-30 min SB fragments - Weekend days",
  "Nblocks_day_IN_bts_30_WE",         "Number of > 30 min SB fragments - Weekend days",
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
  
  "log_FRAG_mean_dur_IN_day_wei",     "Mean duration of SB bouts* - All days",
  "sqrt_FRAG_mean_dur_IN_day_wei",    "Mean duration of SB bouts* - All days",
  "log_FRAG_mean_dur_IN_day_WD",      "Mean duration of SB bouts* - Week days",
  "sqrt_FRAG_mean_dur_IN_day_WD",     "Mean duration of SB bouts* - Week days",
  "log_FRAG_mean_dur_IN_day_WE",      "Mean duration of SB bouts* - Weekend days",
  "sqrt_FRAG_mean_dur_IN_day_WE",     "Mean duration of SB bouts* - Weekend days",
  
  "log_FRAG_mean_dur_MVPA_day_wei",   "Mean duration of MVPA bouts* - All days",
  "sqrt_FRAG_mean_dur_MVPA_day_wei",  "Mean duration of MVPA bouts* - All days",
  "log_FRAG_mean_dur_MVPA_day_WD",    "Mean duration of MVPA bouts* - Week days",
  "sqrt_FRAG_mean_dur_MVPA_day_WD",   "Mean duration of MVPA bouts* - Week days",
  "log_FRAG_mean_dur_MVPA_day_WE",    "Mean duration of MVPA bouts* - Weekend days",
  "sqrt_FRAG_mean_dur_MVPA_day_WE",   "Mean duration of MVPA bouts* - Weekend days",
  
  "log_Nblocks_day_LIG_bts_10_wei",   "Number of > 10 min LIPA fragments* - All days",
  "sqrt_Nblocks_day_LIG_bts_10_wei",  "Number of > 10 min LIPA fragments* - All days",
  "log_Nblocks_day_LIG_bts_10_WD",    "Number of > 10 min LIPA fragments* - Week days",
  "sqrt_Nblocks_day_LIG_bts_10_WD",   "Number of > 10 min LIPA fragments* - Week days",
  "log_Nblocks_day_LIG_bts_10_WE",    "Number of > 10 min LIPA fragments* - Weekend days",
  "sqrt_Nblocks_day_LIG_bts_10_WE",   "Number of > 10 min LIPA fragments* - Weekend days",
  
  "log_Nblocks_day_MVPA_bts_10_wei",  "Number of > 10 min MVPA fragments* - All days",
  "sqrt_Nblocks_day_MVPA_bts_10_wei", "Number of > 10 min MVPA fragments* - All days",
  "log_Nblocks_day_MVPA_bts_10_WD",  "Number of > 10 min MVPA fragments* - Week days",
  "sqrt_Nblocks_day_MVPA_bts_10_WD", "Number of > 10 min MVPA fragments* - Week days",
  "log_Nblocks_day_MVPA_bts_10_WE",  "Number of > 10 min MVPA fragments* - Weekend days",
  "sqrt_Nblocks_day_MVPA_bts_10_WE", "Number of > 10 min MVPA fragments* - Weekend days"
  
)