#contents 
#1. Representativeness
#2. Validity of sMFQ
#3. Sleep descriptive
#4. Distribution of SJL 

################################################################################
#1. Representativeness
################################################################################

#--------------------------------------------------------------------------------
#point prevalence depression at each age
#--------------------------------------------------------------------------------

#age 14 
data_clean %>%
  summarise(
    count_14 = sum(`depressed@14` == 1, na.rm = TRUE),
    total_14 = sum(!is.na(`depressed@14`)),
    percent_14 = (count_14 / total_14) * 100)

#age 18
data_clean %>%
  summarise(
    count_18 = sum(`depressed@18` == 1, na.rm = TRUE),
    total_18 = sum(!is.na(`depressed@18`)),
    percent_18 = (count_18 / total_18) * 100
  )

#age 21
data_clean %>%
  summarise(
    count_21 = sum(`depressed@21` == 1, na.rm = TRUE),
    total_21 = sum(!is.na(`depressed@21`)),
    percent_21 = (count_21 / total_21) * 100
  )

#age 23
data_clean %>%
  summarise(
    count_23 = sum(`depressed@23` == 1, na.rm = TRUE),
    total_23 = sum(!is.na(`depressed@23`)),
    percent_23 = (count_23 / total_23) * 100
  )


#--------------------------------------------------------------------------------
#percentages 
#--------------------------------------------------------------------------------
  
data %>%
  count(sex) %>%
  mutate(percent = n / sum(n) * 100)

data%>%
  count(b_seg_m) %>%
  mutate(percent = n / sum(n) * 100)


data_clean %>%
  count(sex) %>%
  mutate(percent = n / sum(n) * 100)

data_clean %>%
  count(b_seg_m) %>%
  mutate(percent = n / sum(n) * 100)

#--------------------------------------------------------------------------------
#chi-squared
#--------------------------------------------------------------------------------

  sex_summary <- bind_rows(
  data %>% 
    count(sex) %>% 
    mutate(dataset = "data"),
  
  data_clean %>% 
    count(sex) %>% 
    mutate(dataset = "data_no_outliers"))


sex_matrix <- sex_summary %>%
  pivot_wider(names_from = sex, values_from = n, values_fill = 0) %>%
  column_to_rownames("dataset")%>%
  select(-`NA`)# makes datasets the row names


chi_sex <- chisq.test(as.matrix(sex_matrix))

#--------------------------------------------------------------------------------
# t-test
#--------------------------------------------------------------------------------
  
mean_SEO_whole_cohort <- mean(data$b_seg_m, na.rm = TRUE) 
sd_SEO_analytic_sample <- sd(data$b_seg_m, na.rm = TRUE) 

mean_SEO_analytic_sample <- mean(data_clean$b_seg_m, na.rm = TRUE) 
sd_SEO_analytic_sample <- sd(data_clean$b_seg_m, na.rm = TRUE) 


t.test(data_clean$b_seg_m , mu = mean_SEO_whole_cohort, na.rm = TRUE)


################################################################################
#2. Validity of sMFQ
################################################################################

alpha(data_clean[ , MFQ14_items]) 
alpha(data_clean[ , MFQ18_items]) 
alpha(data_clean[ , MFQ21_items]) 
alpha(data_clean[ , MFQ21_items])

--------------------------------------------------------------------------------
# sMFQ plots
--------------------------------------------------------------------------------

#age 14
sMFQ_14 <- data_clean %>%
  ggplot(aes(x = MFQ_total_14)) +
  geom_histogram(
    bins = 24,
    color = "black",
    fill = "grey70",
    boundary = 0
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "sMFQ Total Score At Age 14",
    y = "Density"
  ) +
  theme_classic(base_size = 12)

#age18
sMFQ_18 <-data_clean %>%
  ggplot(aes(x = MFQ_total_18)) +
  geom_histogram(
    bins = 24,
    color = "black",
    fill = "grey70",
    boundary = 0
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "sMFQ Total Score At Age 18",
    y = "Density"
  ) +
  theme_classic(base_size = 12)

#age 21
sMFQ_21 <-data_clean %>%
  ggplot(aes(x = MFQ_total_21)) +
  geom_histogram(
    bins = 24,
    color = "black",
    fill = "grey70",
    boundary = 0
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "sMFQ Total Score At Age 21",
    y = "Density"
  ) +
  theme_classic(base_size = 12)

#age23
sMFQ_23 <-data_clean %>%
  ggplot(aes(x = MFQ_total_23)) +
  geom_histogram(
    bins = 24,
    color = "black",
    fill = "grey70",
    boundary = 0
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "sMFQ Total Score At Age 23",
    y = "Density"
  ) +
  theme_classic(base_size = 12)

################################################################################
#3. Sleep descriptive
################################################################################

#--------------------------------------------------------------------------------
# Function for repeated summary + t-test
#--------------------------------------------------------------------------------

#runs means, sd and t-test

  get_sleep_stats <- function(x, y) {      #x = schoolday y= weekend
    list(
      mean_x = mean(x, na.rm = TRUE),     
      sd_x   = sd(x, na.rm = TRUE),
      mean_y = mean(y, na.rm = TRUE),
      sd_y   = sd(y, na.rm = TRUE),
      t_test = t.test(x, y, paired = TRUE)
    )
  }

#--------------------------------------------------------------------------------
  # Sleep duration
#--------------------------------------------------------------------------------
  
# Convert 
schoolday_duration <- as.numeric(data_clean$sleep_hours_schoolday_calculated, units = "hours")
weekend_duration   <- as.numeric(data_clean$sleep_hours_weekend_calculated, units = "hours")

# Apply stats function
duration_stats <- get_sleep_stats(schoolday_duration, weekend_duration)


#--------------------------------------------------------------------------------
# Sleep latency
#--------------------------------------------------------------------------------
  
# Convert 
schoolday_latency <- as.numeric(data_clean$sleep_latency_schoolday, units = "minutes")
weekend_latency   <- as.numeric(data_clean$sleep_latency_hours_weekend, units = "minutes")

# Apply stats function
latency_stats <- get_sleep_stats(schoolday_latency, weekend_latency)

#-------------------------------------------------------------------------------
# converting HH:MM into continuous variable
#-------------------------------------------------------------------------------
  
#creating conversion function
convert_time_to_hours <- function(time_var) {
  hrs <- hour(time_var) + minute(time_var) / 60 #converting HH:MM into decimal hours since midnight
  ifelse(hrs >= 12, hrs - 12, hrs + 12) # re-centering so midnight is in middle of scale (i.e. 0 -> 12)
}

#applying conversion function to HH:MM variables 
data_sleep_hours <- data_clean %>%
  mutate(
    onset_schoolday_hr  = convert_time_to_hours(sleep_onset_schoolday),
    offset_schoolday_hr = convert_time_to_hours(sleep_offset_schoolday),
    onset_weekend_hr    = convert_time_to_hours(sleep_onset_weekend),
    offset_weekend_hr   = convert_time_to_hours(sleep_offset_weekend),
    
    mid_sleep_schoolday_hr = onset_schoolday_hr + (offset_schoolday_hr - onset_schoolday_hr) / 2,
    mid_sleep_weekend_hr   = onset_weekend_hr + (offset_weekend_hr - onset_weekend_hr) / 2
  )


#--------------------------------------------------------------------------------
# Onset, offset, mid-sleep
#--------------------------------------------------------------------------------
  
onset_stats <- get_sleep_stats(
  data_sleep_hours$onset_schoolday_hr,
  data_sleep_hours$onset_weekend_hr)

offset_stats <- get_sleep_stats(
  data_sleep_hours$offset_schoolday_hr,
  data_sleep_hours$offset_weekend_hr)

mid_stats <- get_sleep_stats(
  data_sleep_hours$mid_sleep_schoolday_hr,
  data_sleep_hours$mid_sleep_weekend_hr)

#--------------------------------------------------------------------------------
# Summary table (clean output)
#--------------------------------------------------------------------------------
  

results <- tibble(
  variable = c(
    "Onset Schoolday", "Onset Weekend",
    "Offset Schoolday", "Offset Weekend",
    "Mid-Sleep Schoolday", "Mid-Sleep Weekend"
  ),
  mean_hr = c(
    onset_stats$mean_x, onset_stats$mean_y,
    offset_stats$mean_x, offset_stats$mean_y,
    mid_stats$mean_x, mid_stats$mean_y
  ),
  sd_hr = c(
    onset_stats$sd_x, onset_stats$sd_y,
    offset_stats$sd_x, offset_stats$sd_y,
    mid_stats$sd_x, mid_stats$sd_y
  )
)

print(results)


################################################################################
#4. Distribution of SJL
################################################################################

#-------------------------------------------------------------------------------
# Prepare SJL variable
#-------------------------------------------------------------------------------

data_clean <- data_clean %>%
  mutate(
    SJL_hours = as.numeric(SJL, units = "hours"),
    SJL_category = case_when(
      SJL < 0 ~ "<0 hour (negative)",
      SJL >= 0 & SJL < 1 ~ "0 to <1 hour",
      SJL >= 1 & SJL < 2 ~ "1 to <2 hours",
      SJL >= 2 ~ "≥2 hours")
    )

#-------------------------------------------------------------------------------
# Descriptive statistics
#-------------------------------------------------------------------------------

SJL_stats <- list(
  skewness = skew(data_clean$SJL_hours),
  kurtosis = kurtosi(data_clean$SJL_hours),
  mean     = mean(data_clean$SJL_hours, na.rm = TRUE),
  median   = median(data_clean$SJL_hours, na.rm = TRUE),
  sd       = sd(data_clean$SJL_hours, na.rm = TRUE),
  min      = min(data_clean$SJL_hours, na.rm = TRUE),
  max      = max(data_clean$SJL_hours, na.rm = TRUE)
)

summary(data_clean$SJL_hours)

#-------------------------------------------------------------------------------
# Category distribution
#-------------------------------------------------------------------------------

sjl_percentages <- data_clean %>%
  count(SJL_category, name = "count") %>%
  mutate(
    percentage = round(100 * count / sum(count), 2)
  ) %>%
  arrange(desc(percentage))

#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------

# Colour plot
SJL_colour <- data_clean %>%
  ggplot(aes(x = SJL, fill = SJL_category)) +
  geom_histogram(bins = 40, colour = "black") +
  scale_fill_manual(values = c(
    "<0 hour (negative)" = "#FBB4AE",
    "0 to <1 hour" = "#B3CDE3",
    "1 to <2 hours" = "#CCEBC5",
    "≥2 hours" = "#DECBE4"
  )) +
  labs(
    x = "SJL (hours)",
    fill = "SJL Category"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(breaks = c(-1, 0, 1, 2, 3, 4, 5, 6)) +
  theme(legend.position = c(0.8, 0.7))+
  theme_classic() 

