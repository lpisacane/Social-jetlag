#Can SJL be a long term predictor of depression 
#Lilianna Pisacane April 2026

library(haven) #open up.dta files
library(tidyverse) #cleaning and transforming data
library(broom) #tidy model output 
library(ggplot2) #visualizations
library(janitor)#making MFQ answer summary table
library(lubridate)#dealing with time series data 
library(psych)#cronbach alpha, skewness and kurtosis
library(forestplot) #making forestplot

#Contents 
#1. Load in data 
#2. Data preparation 
#3. Calculating SJL 
#4. Calculating MFQ
#5. Confounds and covariates
#6. Cleaning data

library(haven) #open up.dta files
library(tidyverse) #cleaning and transforming data
library(broom) #tidy model output 
library(ggplot2) #visualizations
library(janitor)#making MFQ answer summary table
library(lubridate)#dealing with time series data 
library(psych)#chronback alphas, skewness and kurtosis
library(forestplot) #making forestplot


################################################################################
#1. loading in data
################################################################################

#cleaning environment 
rm(list=ls())

#loading in data
data <- read_dta('data/raw/lilianna_dataset.dta')

################################################################################
#2. Data preparation
################################################################################

# ------------------------------------------------------------------------------
# Creating unique participant ID
# ------------------------------------------------------------------------------
data <- data %>%
  mutate(ID = paste(cidB5236, qlet, sep = "")) %>%
  relocate(ID)

# ------------------------------------------------------------------------------
# Renaming variables
# ------------------------------------------------------------------------------
data <- data %>%
  rename(
    "alive@1" = kz011b,
    birthweight = kz030,
    sex = kz021,
    "bmi@15" = fh3019,
    
    "age@15_months" = fh0011a,
    "age@18_months" = cct9991a,
    "age@21_months" = YPA9020,
    "age@23_months" = YPC2650,
    
    attempt_hour_schoolday = fh5420,
    attempt_mins_schoolday = fh5421,
    sleep_latency_schoolday = fh5314,
    sleep_hours_schoolday = fh5440,
    sleep_mins_schoolday = fh5441,
    wakeup_hour_schoolday = fh5425,
    wakeup_mins_schoolday = fh5426,
    
    attempt_hour_weekend = fh5460,
    attempt_mins_weekend = fh5461,
    sleep_latency_weekend = fh5324,
    sleep_hours_weekend = fh5480,
    sleep_mins_weekend = fh5481,
    wakeup_hour_weekend = fh5465,
    wakeup_mins_weekend = fh5466,
    
    sleep_enough_freq = fh5341,
    "total_MFQ@18" = cct2715
  )

# ------------------------------------------------------------------------------
# Creating variable metadata table
# ------------------------------------------------------------------------------
vars <- data.frame(
  name = names(data),
  label = sapply(data, function(x) attr(x, "label")) %>% as.character(),
  labelled = sapply(data, is.labelled)
)

# ------------------------------------------------------------------------------
# Sleep variables vector
# ------------------------------------------------------------------------------
sleep_vars_base <- c(
  "attempt_hour_schoolday",
  "attempt_mins_schoolday",
  "sleep_hours_schoolday",
  "sleep_mins_schoolday",
  "wakeup_hour_schoolday",
  "wakeup_mins_schoolday",
  "attempt_hour_weekend",
  "attempt_mins_weekend",
  "sleep_hours_weekend",
  "sleep_mins_weekend",
  "wakeup_hour_weekend",
  "wakeup_mins_weekend",
  "sleep_enough_freq"
)

# ------------------------------------------------------------------------------
# Recode missing values as NA
# ------------------------------------------------------------------------------
data[data == -10] <- NA
data[data == -6]  <- NA
data[data == -2]  <- NA
data[data == -1]  <- NA


################################################################################
#3. Calculating SJL
################################################################################

# ------------------------------------------------------------------------------
# Flag: answered all sleep questions
# ------------------------------------------------------------------------------
data <- data %>%
  mutate(
    all_sleep_vars = if_else(
      rowSums(across(all_of(sleep_vars_base),
                     ~ .x %in% c(-10, -6, -1, -2) | is.na(.x))) == 0,
      "yes",
      "no"
    )
  ) %>%
  relocate(all_sleep_vars, .after = ID)


# ------------------------------------------------------------------------------
# SCHOOLDAY sleep variables
# ------------------------------------------------------------------------------

# Split latency into hours and minutes
data <- data %>%
  mutate(
    sleep_latency_hours_schoolday = sleep_latency_schoolday %/% 60,  # whole hours
    sleep_latency_mins_schoolday  = sleep_latency_schoolday %% 60    # remaining minutes
  )

# Calculate actual sleep onset time
data <- data %>%
  mutate(
    # Initial sleep onset on Jan 1
    sleep_onset_schoolday = as.POSIXct(
      paste(
        "2026-01-01",
        sprintf("%02d:%02d:00", attempt_hour_schoolday, attempt_mins_schoolday)
      ),
      format = "%Y-%m-%d %H:%M:%S"
    ) +
      hours(sleep_latency_hours_schoolday) +
      minutes(sleep_latency_mins_schoolday),
    
    # If sleep onset is after midnight and not already rolled over, shift to Jan 2
    sleep_onset_schoolday = if_else(
      hour(sleep_onset_schoolday) <= 8 &
        as.Date(sleep_onset_schoolday) != as.Date("2026-01-02"),
      sleep_onset_schoolday + days(1),
      sleep_onset_schoolday
    )
  )

# Sleep offset
data <- data %>%
  mutate(
    sleep_offset_schoolday = as.POSIXct(
      paste(
        "2026-01-02",
        sprintf("%02d:%02d:00", wakeup_hour_schoolday, wakeup_mins_schoolday)
      ),
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

# Calculate sleep duration (hours)
data <- data %>%
  mutate(
    sleep_hours_schoolday_calculated = difftime(
      sleep_offset_schoolday,
      sleep_onset_schoolday,
      units = "hours"
    )
  )

# Calculate mid-sleep time
data <- data %>%
  mutate(
    mid_sleep_schoolday =
      sleep_onset_schoolday +
      ((sleep_offset_schoolday - sleep_onset_schoolday) / 2)
  )

# ------------------------------------------------------------------------------
# WEEKEND sleep variables
# ------------------------------------------------------------------------------

# Split latency into hours and minutes
data <- data %>%
  mutate(
    sleep_latency_hours_weekend = sleep_latency_weekend %/% 60,
    sleep_latency_mins_weekend  = sleep_latency_weekend %% 60
  )

# Calculate actual sleep onset time
data <- data %>%
  mutate(
    # Initial sleep onset on Jan 1
    sleep_onset_weekend = as.POSIXct(
      paste(
        "2026-01-01",
        sprintf("%02d:%02d:00", attempt_hour_weekend, attempt_mins_weekend)
      ),
      format = "%Y-%m-%d %H:%M:%S"
    ) +
      hours(sleep_latency_hours_weekend) +
      minutes(sleep_latency_mins_weekend),
    
    # If sleep onset is after midnight, roll to Jan 2
    sleep_onset_weekend = if_else(
      hour(sleep_onset_weekend) <= 8 &
        as.Date(sleep_onset_weekend) != as.Date("2026-01-02"),
      sleep_onset_weekend + days(1),
      sleep_onset_weekend
    )
  )

# Sleep offset
data <- data %>%
  mutate(
    sleep_offset_weekend = as.POSIXct(
      paste(
        "2026-01-02",
        sprintf("%02d:%02d:00", wakeup_hour_weekend, wakeup_mins_weekend)
      ),
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

# Calculate sleep duration (hours)
data <- data %>%
  mutate(
    sleep_hours_weekend_calculated = difftime(
      sleep_offset_weekend,
      sleep_onset_weekend,
      units = "hours"
    )
  )

# Calculate mid-sleep time
data <- data %>%
  mutate(
    mid_sleep_weekend =
      sleep_onset_weekend +
      ((sleep_offset_weekend - sleep_onset_weekend) / 2)
  )

# ------------------------------------------------------------------------------
# Calculating absolute social jetlag (|SJL|)
# ------------------------------------------------------------------------------

data <- data %>%
  mutate(
    SJL = difftime(mid_sleep_weekend, mid_sleep_schoolday, units = "hours"),
    modulus_SJL = abs(SJL) #|SJL| to be used in models
  )

# ------------------------------------------------------------------------------
# Reverse coding: frequency of getting enough sleep
# ------------------------------------------------------------------------------

print_labels(data$sleep_enough_freq)
table(data$sleep_enough_freq)

data <- data %>%
  mutate(
    sleep_enough_freq = recode(
      as.numeric(sleep_enough_freq),
      `1` = 5,
      `2` = 4,
      `3` = 3,
      `4` = 2,
      `5` = 1
    )
  )


################################################################################
#4. Calculating total MFQ
################################################################################

MFQ14_items <- c(
  "fg7210", "fg7213", "fg7214", "fg7212", "fg7215",
  "fg7216", "fg7219", "fg7224", "fg7222", "fg7223",
  "fg7221", "fg7225", "fg7218")

MFQ18_items <- c(
  "cct2700", "cct2702", "cct2703", "cct2701", "cct2704",
  "cct2705", "cct2707", "cct2711", "cct2709", "cct2710",
  "cct2708", "cct2712", "cct2706")

MFQ21_items <- c(
  "YPA2000", "YPA2020", "YPA2030", "YPA2010", "YPA2040", "YPA2050",
  "YPA2070", "YPA2110", "YPA2090", "YPA2100", "YPA2080", "YPA2120",
  "YPA2060")

MFQ23_items <- c(
  "YPC1650", "YPC1653", "YPC1654", "YPC1651", "YPC1655",
  "YPC1656", "YPC1659", "YPC1665", "YPC1662", "YPC1663",
  "YPC1660", "YPC1667", "YPC1658")

# ------------------------------------------------------------------------------
# Count number of MFQ items answered at each age
# ------------------------------------------------------------------------------

#age 14
data <- data %>%
  mutate(
    MFQ14_answered = rowSums(
      across(all_of(MFQ14_items), ~ . %in% c(1, 2, 3)),
      na.rm = TRUE
    )
  ) %>%
  relocate(MFQ14_answered, .after = ID)

#age 18
data <- data %>%
  mutate(
    MFQ18_answered = rowSums(
      across(all_of(MFQ18_items), ~ . %in% c(1, 2, 3)),
      na.rm = TRUE
    )
  ) %>%
  relocate(MFQ18_answered, .after = ID)

#age 21
data <- data %>%
  mutate(
    MFQ21_answered = rowSums(
      across(all_of(MFQ21_items), ~ . %in% c(1, 2, 3)),
      na.rm = TRUE
    )
  ) %>%
  relocate(MFQ21_answered, .after = ID)

#age 23
data <- data %>%
  mutate(
    MFQ23_answered = rowSums(
      across(all_of(MFQ23_items), ~ . %in% c(1, 2, 3)),
      na.rm = TRUE
    )
  ) %>%
  relocate(MFQ23_answered, .after = ID)

# ------------------------------------------------------------------------------
# Flag: answered all MFQ items at all ages
# ------------------------------------------------------------------------------

data <- data %>%
  mutate(
    all_MFQ = if_else(
      MFQ14_answered == 13 &
        MFQ18_answered == 13 &
        MFQ21_answered == 13 &
        MFQ23_answered == 13,
      "yes",
      "no"
    )
  ) %>%
  relocate(all_MFQ, .after = all_sleep_vars)

# ------------------------------------------------------------------------------
# Calculating total MFQ scores
# ------------------------------------------------------------------------------

data <- data %>%
  # Reverse-coded items (ages 14, 18, 21)
  mutate(
    across(
      all_of(c(MFQ14_items, MFQ18_items, MFQ21_items)),
      ~ case_when(
        . == 1 ~ 2,
        . == 2 ~ 1,
        . == 3 ~ 0,
        TRUE ~ .
      )
    )
  ) %>%
  
  # Non-reversed items (age 23)
  mutate(
    across(
      all_of(MFQ23_items),
      ~ case_when(
        . == 1 ~ 0,
        . == 2 ~ 1,
        . == 3 ~ 2,
        TRUE ~ .
      )
    )
  ) %>%
  
  # Total scores
  mutate(
    MFQ_total_14 = if_else(
      all_MFQ == "yes",
      rowSums(across(all_of(MFQ14_items))),
      NA_real_
    ),
    MFQ_total_18 = if_else(
      all_MFQ == "yes",
      rowSums(across(all_of(MFQ18_items))),
      NA_real_
    ),
    MFQ_total_21 = if_else(
      all_MFQ == "yes",
      rowSums(across(all_of(MFQ21_items))),
      NA_real_
    ),
    MFQ_total_23 = if_else(
      all_MFQ == "yes",
      rowSums(across(all_of(MFQ23_items))),
      NA_real_
    )
  )

# ------------------------------------------------------------------------------
# Dichotomous depression variables
# ------------------------------------------------------------------------------

data <- data %>%
  mutate(
    `depressed@14` = if_else(MFQ_total_14 >= 12, 1, 0),
    `depressed@18` = if_else(MFQ_total_18 >= 12, 1, 0),
    `depressed@21` = if_else(MFQ_total_21 >= 12, 1, 0),
    `depressed@23` = if_else(MFQ_total_23 >= 12, 1, 0)
  )

################################################################################
#5. Confounds and covariates
################################################################################

confounds_and_covariates <- c(
  "MFQ_total_14",
  "sleep_enough_freq",
  "age@18_months",
  "age@21_months",
  "age@23_months",
  "bmi@15",
  "b_seg_m",
  "sex")

# ------------------------------------------------------------------------------
# Flag: complete demographic data
# ------------------------------------------------------------------------------

data <- data %>%
  mutate(
    all_demo_vars = if_else(
      rowSums(
        across(
          all_of(confounds_and_covariates),
          ~ .x %in% c(-10, -6, -1, -2) | is.na(.x)
        )
      ) == 0,
      "yes",
      "no"
    )
  ) %>%
  relocate(all_demo_vars, .after = ID)

################################################################################
#6. Cleaning data
################################################################################

# ------------------------------------------------------------------------------
# Complete data 
# ------------------------------------------------------------------------------

data_clean <- data %>%
  filter(all_sleep_vars == "yes") %>%
  filter(all_MFQ == "yes") %>%
  filter(all_demo_vars == "yes")

# ------------------------------------------------------------------------------
# Remove implausible sleep values (negative durations)
# ------------------------------------------------------------------------------

data_clean <- data_clean %>%
  filter(
    sleep_hours_schoolday_calculated > 0 &
      sleep_hours_weekend_calculated > 0)

# ------------------------------------------------------------------------------
# remove outliers using cut-off rules (3–15 hours)
# ------------------------------------------------------------------------------

data_clean <- data_clean %>%
  filter(
    sleep_hours_schoolday_calculated >= 3 &
      sleep_hours_schoolday_calculated <= 15 &
      sleep_hours_weekend_calculated >= 3 &
      sleep_hours_weekend_calculated <= 15 )

