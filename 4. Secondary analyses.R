#contents 
#1.Running logistic regression (SJL categorical)
#2.Pulling results for forest plot
#3.Preparing model output for forest plots
#4.Creating forest plot
#5.Full fully adjusted model output
#6.Point prevalence at each outcome time point per SJL level

################################################################################
#1.Running logistic regression (SJL categorical)
################################################################################

# ------------------------------------------------------------------------------
# Transforming SJL into categorical variable
# ------------------------------------------------------------------------------

data_clean <- data_clean %>%
  mutate(
    modulus_SJL_cat = case_when(
      modulus_SJL < 1 ~ "0 to <1 hour",
      modulus_SJL >= 1 & modulus_SJL < 2 ~ "1 to <2 hours",
      modulus_SJL >= 2 ~ "≥2 hours"
    )
  )

data_clean$modulus_SJL_cat <- factor(
  data_clean$modulus_SJL_cat,
  levels = c("0 to <1 hour", "1 to <2 hours", "≥2 hours")
)


# ------------------------------------------------------------------------------
# Models @ 18
# ------------------------------------------------------------------------------

gl_model_base18_cat <- glm(`depressed@18` ~ modulus_SJL_cat,
                           data_clean = data_clean,
                           family = binomial)

gl_model1_18_cat <- glm(`depressed@18` ~ modulus_SJL_cat + MFQ_total_14,
                        data_clean = data_clean,
                        family = binomial)

gl_model2_18_cat <- glm(`depressed@18` ~ modulus_SJL_cat + MFQ_total_14 + sex +
                          `age@18_months` + `bmi@15` + b_seg_m,
                        data_clean = data_clean,
                        family = binomial)

gl_model3_18_cat <- glm(`depressed@18` ~ modulus_SJL_cat + MFQ_total_14 + sex +
                          `age@18_months` + `bmi@15` + b_seg_m + sleep_enough_freq,
                        data_clean = data_clean,
                        family = binomial)

# ------------------------------------------------------------------------------
# Models @ 21
# ------------------------------------------------------------------------------

gl_model_base21_cat <- glm(`depressed@21` ~ modulus_SJL_cat,
                           data_clean = data_clean,
                           family = binomial)

gl_model1_21_cat <- glm(`depressed@21` ~ modulus_SJL_cat + MFQ_total_14,
                        data_clean = data_clean,
                        family = binomial)

gl_model2_21_cat <- glm(`depressed@21` ~ modulus_SJL_cat + MFQ_total_14 + sex +
                          `age@21_months` + `bmi@15` + b_seg_m,
                        data_clean = data_clean,
                        family = binomial)

gl_model3_21_cat <- glm(`depressed@21` ~ modulus_SJL_cat + MFQ_total_14 + sex +
                          `age@21_months` + `bmi@15` + b_seg_m + sleep_enough_freq,
                        data_clean = data_clean,
                        family = binomial)

# ------------------------------------------------------------------------------
# Models @ 23
# ------------------------------------------------------------------------------

gl_model_base23_cat <- glm(`depressed@23` ~ modulus_SJL_cat,
                           data_clean = data_clean,
                           family = binomial)

gl_model1_23_cat <- glm(`depressed@23` ~ modulus_SJL_cat + MFQ_total_14,
                        data_clean = data_clean,
                        family = binomial)

gl_model2_23_cat <- glm(`depressed@23` ~ modulus_SJL_cat + MFQ_total_14 + sex +
                          `age@23_months` + `bmi@15` + b_seg_m,
                        data_clean = data_clean,
                        family = binomial)

gl_model3_23_cat <- glm(`depressed@23` ~ modulus_SJL_cat + MFQ_total_14 + sex +
                          `age@23_months` + `bmi@15` + b_seg_m + sleep_enough_freq,
                        data_clean = data_clean,
                        family = binomial)

################################################################################
#2.Pulling results for forest plot
################################################################################

# ------------------------------------------------------------------------------
# Results @ 18
# ------------------------------------------------------------------------------

glm_base18_cat <- tidy(gl_model_base18_cat)
ci_base18_cat  <- confint(gl_model_base18_cat)

OR_base18_cat  <- exp(coef(gl_model_base18_cat))
lci_base18_cat <- exp(ci_base18_cat[, 1])
uci_base18_cat <- exp(ci_base18_cat[, 2])
p_base18_cat   <- glm_base18_cat$p.value

results_glm_base18_cat <- data_clean.frame(
  Model = "glm_base18_cat",
  Category = names(OR_base18_cat)[2:3],
  OR = OR_base18_cat[2:3],
  LCI = lci_base18_cat[2:3],
  UCI = uci_base18_cat[2:3],
  pvalue = p_base18_cat[2:3]
)

glm1_18_cat <- tidy(gl_model1_18_cat)
ci1_18_cat  <- confint(gl_model1_18_cat)

OR1_18_cat  <- exp(coef(gl_model1_18_cat))
lci1_18_cat <- exp(ci1_18_cat[, 1])
uci1_18_cat <- exp(ci1_18_cat[, 2])
p1_18_cat   <- glm1_18_cat$p.value

results_glm1_18_cat <- data_clean.frame(
  Model = "glm1_18_cat",
  Category = names(OR1_18_cat)[2:3],
  OR = OR1_18_cat[2:3],
  LCI = lci1_18_cat[2:3],
  UCI = uci1_18_cat[2:3],
  pvalue = p1_18_cat[2:3]
)

glm2_18_cat <- tidy(gl_model2_18_cat)
ci2_18_cat  <- confint(gl_model2_18_cat)

OR2_18_cat  <- exp(coef(gl_model2_18_cat))
lci2_18_cat <- exp(ci2_18_cat[, 1])
uci2_18_cat <- exp(ci2_18_cat[, 2])
p2_18_cat   <- glm2_18_cat$p.value

results_glm2_18_cat <- data_clean.frame(
  Model = "glm2_18_cat",
  Category = names(OR2_18_cat)[2:3],
  OR = OR2_18_cat[2:3],
  LCI = lci2_18_cat[2:3],
  UCI = uci2_18_cat[2:3],
  pvalue = p2_18_cat[2:3]
)

glm3_18_cat <- tidy(gl_model3_18_cat)
ci3_18_cat  <- confint(gl_model3_18_cat)

OR3_18_cat  <- exp(coef(gl_model3_18_cat))
lci3_18_cat <- exp(ci3_18_cat[, 1])
uci3_18_cat <- exp(ci3_18_cat[, 2])
p3_18_cat   <- glm3_18_cat$p.value

results_glm3_18_cat <- data_clean.frame(
  Model = "glm3_18_cat",
  Category = names(OR3_18_cat)[2:3],
  OR = OR3_18_cat[2:3],
  LCI = lci3_18_cat[2:3],
  UCI = uci3_18_cat[2:3],
  pvalue = p3_18_cat[2:3]
)

# ------------------------------------------------------------------------------
# Results @ 21
# ------------------------------------------------------------------------------

glm_base21_cat <- tidy(gl_model_base21_cat)
ci_base21_cat  <- confint(gl_model_base21_cat)

OR_base21_cat  <- exp(coef(gl_model_base21_cat))
lci_base21_cat <- exp(ci_base21_cat[, 1])
uci_base21_cat <- exp(ci_base21_cat[, 2])
p_base21_cat   <- glm_base21_cat$p.value

results_glm_base21_cat <- data_clean.frame(
  Model = "glm_base21_cat",
  Category = names(OR_base21_cat)[2:3],
  OR = OR_base21_cat[2:3],
  LCI = lci_base21_cat[2:3],
  UCI = uci_base21_cat[2:3],
  pvalue = p_base21_cat[2:3]
)

glm1_21_cat <- tidy(gl_model1_21_cat)
ci1_21_cat  <- confint(gl_model1_21_cat)

OR1_21_cat  <- exp(coef(gl_model1_21_cat))
lci1_21_cat <- exp(ci1_21_cat[, 1])
uci1_21_cat <- exp(ci1_21_cat[, 2])
p1_21_cat   <- glm1_21_cat$p.value

results_glm1_21_cat <- data_clean.frame(
  Model = "glm1_21_cat",
  Category = names(OR1_21_cat)[2:3],
  OR = OR1_21_cat[2:3],
  LCI = lci1_21_cat[2:3],
  UCI = uci1_21_cat[2:3],
  pvalue = p1_21_cat[2:3]
)

glm2_21_cat <- tidy(gl_model2_21_cat)
ci2_21_cat  <- confint(gl_model2_21_cat)

OR2_21_cat  <- exp(coef(gl_model2_21_cat))
lci2_21_cat <- exp(ci2_21_cat[, 1])
uci2_21_cat <- exp(ci2_21_cat[, 2])
p2_21_cat   <- glm2_21_cat$p.value

results_glm2_21_cat <- data_clean.frame(
  Model = "glm2_21_cat",
  Category = names(OR2_21_cat)[2:3],
  OR = OR2_21_cat[2:3],
  LCI = lci2_21_cat[2:3],
  UCI = uci2_21_cat[2:3],
  pvalue = p2_21_cat[2:3]
)

glm3_21_cat <- tidy(gl_model3_21_cat)
ci3_21_cat  <- confint(gl_model3_21_cat)

OR3_21_cat  <- exp(coef(gl_model3_21_cat))
lci3_21_cat <- exp(ci3_21_cat[, 1])
uci3_21_cat <- exp(ci3_21_cat[, 2])
p3_21_cat   <- glm3_21_cat$p.value

results_glm3_21_cat <- data_clean.frame(
  Model = "glm3_21_cat",
  Category = names(OR3_21_cat)[2:3],
  OR = OR3_21_cat[2:3],
  LCI = lci3_21_cat[2:3],
  UCI = uci3_21_cat[2:3],
  pvalue = p3_21_cat[2:3]
)

# ------------------------------------------------------------------------------
# Results @ 23
# ------------------------------------------------------------------------------

glm_base23_cat <- tidy(gl_model_base23_cat)
ci_base23_cat  <- confint(gl_model_base23_cat)

OR_base23_cat  <- exp(coef(gl_model_base23_cat))
lci_base23_cat <- exp(ci_base23_cat[, 1])
uci_base23_cat <- exp(ci_base23_cat[, 2])
p_base23_cat   <- glm_base23_cat$p.value

results_glm_base23_cat <- data_clean.frame(
  Model = "glm_base23_cat",
  Category = names(OR_base23_cat)[2:3],
  OR = OR_base23_cat[2:3],
  LCI = lci_base23_cat[2:3],
  UCI = uci_base23_cat[2:3],
  pvalue = p_base23_cat[2:3]
)

glm1_23_cat <- tidy(gl_model1_23_cat)
ci1_23_cat  <- confint(gl_model1_23_cat)

OR1_23_cat  <- exp(coef(gl_model1_23_cat))
lci1_23_cat <- exp(ci1_23_cat[, 1])
uci1_23_cat <- exp(ci1_23_cat[, 2])
p1_23_cat   <- glm1_23_cat$p.value

results_glm1_23_cat <- data_clean.frame(
  Model = "glm1_23_cat",
  Category = names(OR1_23_cat)[2:3],
  OR = OR1_23_cat[2:3],
  LCI = lci1_23_cat[2:3],
  UCI = uci1_23_cat[2:3],
  pvalue = p1_23_cat[2:3]
)

glm2_23_cat <- tidy(gl_model2_23_cat)
ci2_23_cat  <- confint(gl_model2_23_cat)

OR2_23_cat  <- exp(coef(gl_model2_23_cat))
lci2_23_cat <- exp(ci2_23_cat[, 1])
uci2_23_cat <- exp(ci2_23_cat[, 2])
p2_23_cat   <- glm2_23_cat$p.value

results_glm2_23_cat <- data_clean.frame(
  Model = "glm2_23_cat",
  Category = names(OR2_23_cat)[2:3],
  OR = OR2_23_cat[2:3],
  LCI = lci2_23_cat[2:3],
  UCI = uci2_23_cat[2:3],
  pvalue = p2_23_cat[2:3]
)

glm3_23_cat <- tidy(gl_model3_23_cat)
ci3_23_cat  <- confint(gl_model3_23_cat)

OR3_23_cat  <- exp(coef(gl_model3_23_cat))
lci3_23_cat <- exp(ci3_23_cat[, 1])
uci3_23_cat <- exp(ci3_23_cat[, 2])
p3_23_cat   <- glm3_23_cat$p.value

results_glm3_23_cat <- data_clean.frame(
  Model = "glm3_23_cat",
  Category = names(OR3_23_cat)[2:3],
  OR = OR3_23_cat[2:3],
  LCI = lci3_23_cat[2:3],
  UCI = uci3_23_cat[2:3],
  pvalue = p3_23_cat[2:3]
)

################################################################################
#3.Preparing model output for forest plots
################################################################################

##########################################################################
# Preparing model output for forest plot
##########################################################################

#combine for results for each age
glm_results_18_cat <- rbind(
  results_glm_base18_cat, results_glm1_18_cat,
  results_glm2_18_cat, results_glm3_18_cat)

glm_results_21_cat <- rbind(
  results_glm_base21_cat, results_glm1_21_cat,
  results_glm2_21_cat, results_glm3_21_cat)

glm_results_23_cat <- rbind(
  results_glm_base23_cat, results_glm1_23_cat,
  results_glm2_23_cat, results_glm3_23_cat)


#combine results for all models at each age
results_all_glm_cat <- rbind(
  results_glm_base18_cat, results_glm1_18_cat, results_glm2_18_cat, results_glm3_18_cat,
  results_glm_base21_cat, results_glm1_21_cat, results_glm2_21_cat, results_glm3_21_cat,
  results_glm_base23_cat, results_glm1_23_cat, results_glm2_23_cat, results_glm3_23_cat
) %>%
  
  mutate(
    Model = factor(Model, levels = c(
      "glm_base18_cat","glm1_18_cat","glm2_18_cat","glm3_18_cat",
      "glm_base21_cat","glm1_21_cat","glm2_21_cat","glm3_21_cat",
      "glm_base23_cat","glm1_23_cat","glm2_23_cat","glm3_23_cat"
    )),
    
    Age = case_when(
      grepl("18", Model) ~ "18",
      grepl("21", Model) ~ "21",
      grepl("23", Model) ~ "23"
    )
  ) %>%
  
  rename(model_code = Model) %>%
  
  mutate(
    Model = recode(model_code,
                   "glm_base18_cat" = "Unadjusted",
                   "glm1_18_cat"    = "Baseline Adjusted",
                   "glm2_18_cat"    = "Demographic Adjusted",
                   "glm3_18_cat"    = "Fully Adjusted",
                   
                   "glm_base21_cat" = "Unadjusted",
                   "glm1_21_cat"    = "Baseline Adjusted",
                   "glm2_21_cat"    = "Demographic Adjusted",
                   "glm3_21_cat"    = "Fully Adjusted",
                   
                   "glm_base23_cat" = "Unadjusted",
                   "glm1_23_cat"    = "Baseline Adjusted",
                   "glm2_23_cat"    = "Demographic Adjusted",
                   "glm3_23_cat"    = "Fully Adjusted"
    ),
    
    Category = recode(Category,
                      "modulus_SJL_cat1 to <2 hours" = "Moderate",
                      "modulus_SJL_cat≥2 hours"      = "High"
    )
  ) %>%
  
  relocate(Age, .after = model_code) %>%
  relocate(Model, .after = Age) %>%
  
  arrange(Age, Category, Model)

# ------------------------------------------------------------------------------
# Spacing for forest plot
# ------------------------------------------------------------------------------

blank_row <- results_all_glm_cat[1, ]
blank_row[,] <- NA

results_all_glm_cat_spaced <- rbind(
  blank_row,
  
  # Age 18
  results_all_glm_cat[results_all_glm_cat$Age == "18" & results_all_glm_cat$Category == "Moderate", ],
  blank_row,
  results_all_glm_cat[results_all_glm_cat$Age == "18" & results_all_glm_cat$Category == "High", ],
  blank_row,
  
  # Age 21
  results_all_glm_cat[results_all_glm_cat$Age == "21" & results_all_glm_cat$Category == "Moderate", ],
  blank_row,
  results_all_glm_cat[results_all_glm_cat$Age == "21" & results_all_glm_cat$Category == "High", ],
  blank_row,
  
  # Age 23
  results_all_glm_cat[results_all_glm_cat$Age == "23" & results_all_glm_cat$Category == "Moderate", ],
  blank_row,
  results_all_glm_cat[results_all_glm_cat$Age == "23" & results_all_glm_cat$Category == "High", ]
)

n_rows_cat <- nrow(results_all_glm_cat_spaced)


################################################################################
#4.Creating forest plot
################################################################################

# ------------------------------------------------------------------------------
# Text to go next to plot
# ------------------------------------------------------------------------------

forestplot_glm_cat_tabletext <- cbind(
  # Age column
  c("Age", 
    as.character(
      ifelse(is.na(results_all_glm_cat_spaced$Age), "", 
             ifelse(duplicated(results_all_glm_cat_spaced$Age), "", 
                    as.character(results_all_glm_cat_spaced$Age))))),
  
  c("SJL Category", as.character(
    ave(results_all_glm_cat_spaced$Category, results_all_glm_cat_spaced$Age,
        FUN = function(x) ifelse(duplicated(x), "", x)))),
  
  # Model column
  c("Model",
    as.character(
      ifelse(is.na(results_all_glm_cat_spaced$Model), "", 
             as.character(results_all_glm_cat_spaced$Model)))),
  # OR column
  c("OR",
    as.character(
      ifelse(is.na(results_all_glm_cat_spaced$OR), "", 
             sprintf("%.2f", results_all_glm_cat_spaced$OR)))),
  # 95% CI column
  c("95% CI",
    as.character(
      ifelse(is.na(results_all_glm_cat_spaced$LCI), "", 
             paste0(sprintf("%.2f", results_all_glm_cat_spaced$LCI), " - ",
                    sprintf("%.2f", results_all_glm_cat_spaced$UCI))))),
  #p value column
  c("p value",
    as.character(
      ifelse(is.na(results_all_glm_cat_spaced$pvalue), "", 
             sprintf("%.2f", results_all_glm_cat_spaced$pvalue)))))


# ------------------------------------------------------------------------------
# Actually creating plot
# ------------------------------------------------------------------------------


forestplot_glm_cat <- data_clean.frame(
  labeltext = forestplot_glm_cat_tabletext,
  mean = c(NA,results_all_glm_cat_spaced$OR),
  lower = c(NA, results_all_glm_cat_spaced$LCI),
  upper = c(NA, results_all_glm_cat_spaced$UCI))  

forestplot(
  labeltext = forestplot_glm_cat_tabletext,
  mean      = forestplot_glm_cat$mean,
  lower     = forestplot_glm_cat$lower,
  upper     = forestplot_glm_cat$upper,
  zero = 1,
  lwd.zero = 2,
  colgap = unit(10, "mm"),
  boxsize = 0.4,
  lwd.ci = 2,
  align = c('l','l','l','c','c','c'),
  is.summary = c(TRUE, rep(FALSE, n_rows_cat)),  # bold only header
  txt_gp = fpTxtGp(label = gpar(fontface = 2)),
  xticks = c(0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.25,2.50),
  col = fpColors(
    box = 'black',
    lines  = 'black',
    summary = 'black'
  ),
  default = gpar(vertices = TRUE),
  xlab = 'OR (95% CIs)')

grid.text(
  "Reference category = 0 to <1 hr SJL;  Moderate SJL = 1 to <2 hrs;  High SJL = ≥2 hrs",
  x = 0.46, y = unit(0.05, "npc"),  # adjust y to move below plot
  just = "right",
  gp = gpar(fontsize = 9, fontface = "italic")
)

################################################################################
#6. point prevalence at each outcome time point per SJL level
################################################################################

depressed_levelSJL <- data_clean %>%
  pivot_longer(
    cols = starts_with("depressed"),
    names_to = "age",
    values_to = "depression"
  ) %>%
  mutate(
    age = gsub("depressed_", "", age)
  )

# ------------------------------------------------------------------------------
# Creating count and percentage
# ------------------------------------------------------------------------------

SJL_depression_percent <- depressed_levelSJL %>%
  filter(!is.na(depression)) %>%
  group_by(modulus_SJL_cat, age) %>%
  summarise(
    total_n = n(),
    depressed_n = sum(depression == 1, na.rm = TRUE),
    percent_depressed = (depressed_n / total_n)*100,
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Creating full point prevelenace table
# ------------------------------------------------------------------------------

SJL_depression_percent_wide <- SJL_depression_percent %>%
  select(modulus_SJL_cat, age, percent_depressed) %>%
  pivot_wider(
    names_from = age,
    values_from = percent_depressed) %>%
  rename('SJL category' = modulus_SJL_cat ,'Age 14 %' = `depressed@14`, 'Age 18 %' = `depressed@18` ,
         'Age 21 %' = `depressed@21` , 'Age 23 %' = `depressed@23`)



