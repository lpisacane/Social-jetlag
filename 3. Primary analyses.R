#contents 
#1.Running logistic regression (SJL continuous)
#2.Pulling results for forest plot
#3.Preparing model output for forest plots
#4.Creating forest plot
#5.Full fully adjusted model output

##########################################################################
#1. Running logistic regression models 
##########################################################################

# ------------------------------------------------------------------------------
# Models @ 18
# ------------------------------------------------------------------------------

gl_model_base18 <- glm(`depressed@18` ~ modulus_SJL, data_clean = data_clean, family = binomial)
glm_base18 <- tidy(gl_model_base18)
summary(gl_model_base18)
ci_glmbase18 <- confint(gl_model_base18)

gl_model1_18 <- glm(`depressed@18` ~ modulus_SJL + MFQ_total_14, data_clean = data_clean, family = binomial)
glm1_18 <- tidy(gl_model1_18)
summary(gl_model1_18)
ci_glm1_18 <- confint(gl_model1_18)

gl_model2_18 <- glm(`depressed@18` ~ modulus_SJL + MFQ_total_14 + sex + `age@18_months` + `bmi@15` + b_seg_m,
                    data_clean = data_clean, family = binomial)
glm2_18 <- tidy(gl_model2_18)
summary(gl_model2_18)
ci_glm2_18 <- confint(gl_model2_18)

gl_model3_18 <- glm(`depressed@18`  ~ modulus_SJL + MFQ_total_14 + sex + `age@18_months` + `bmi@15` + b_seg_m + sleep_enough_freq,
                    data_clean = data_clean, family = binomial)
glm3_18 <- tidy(gl_model3_18)
summary(gl_model3_18)
ci_glm3_18 <- confint(gl_model3_18)


# ------------------------------------------------------------------------------
# Models @ 21
# ------------------------------------------------------------------------------

gl_model_base21 <- glm(`depressed@21` ~ modulus_SJL, data_clean = data_clean, family = binomial)
glm_base21 <- tidy(gl_model_base21)
summary(gl_model_base21)
ci_glmbase21 <- confint(gl_model_base21)

gl_model1_21 <- glm(`depressed@21` ~ modulus_SJL + MFQ_total_14, data_clean = data_clean, family = binomial)
glm1_21 <- tidy(gl_model1_21)
summary(gl_model1_21)
ci_glm1_21 <- confint(gl_model1_21)

gl_model2_21 <- glm(`depressed@21` ~ modulus_SJL + MFQ_total_14 + sex + `age@21_months` + `bmi@15` + b_seg_m,
                    data_clean = data_clean, family = binomial)
glm2_21 <- tidy(gl_model2_21)
summary(gl_model2_21)
ci_glm2_21 <- confint(gl_model2_21)

gl_model3_21 <- glm(`depressed@21`  ~ modulus_SJL + MFQ_total_14 + sex + `age@21_months` + `bmi@15` + b_seg_m + sleep_enough_freq,
                    data_clean = data_clean, family = binomial)
glm3_21 <- tidy(gl_model3_21)
summary(gl_model3_21)
ci_glm3_21 <- confint(gl_model3_21)


# ------------------------------------------------------------------------------
# Models @ 23
# ------------------------------------------------------------------------------

gl_model_base23 <- glm(`depressed@23` ~ modulus_SJL, data_clean = data_clean, family = binomial)
glm_base23 <- tidy(gl_model_base23)
summary(gl_model_base23)
ci_glmbase23 <- confint(gl_model_base23)

gl_model1_23 <- glm(`depressed@23` ~ modulus_SJL + MFQ_total_14, data_clean = data_clean, family = binomial)
glm1_23 <- tidy(gl_model1_23)
summary(gl_model1_23)
ci_glm1_23 <- confint(gl_model1_23)

gl_model2_23 <- glm(`depressed@23` ~ modulus_SJL + MFQ_total_14 + sex + `age@23_months` + `bmi@15` + b_seg_m,
                    data_clean = data_clean, family = binomial)
glm2_23 <- tidy(gl_model2_23)
summary(gl_model2_23)
ci_glm2_23 <- confint(gl_model2_23)

gl_model3_23 <- glm(`depressed@23`  ~ modulus_SJL + MFQ_total_14 + sex + `age@23_months` + `bmi@15` + b_seg_m + sleep_enough_freq,
                    data_clean = data_clean, family = binomial)
glm3_23 <- tidy(gl_model3_23)
summary(gl_model3_23)
ci_glm3_23 <- confint(gl_model3_23)

##########################################################################
#2. Pulling results for forest plots 
##########################################################################

# ------------------------------------------------------------------------------
# Results @ 18
# ------------------------------------------------------------------------------

# Unadjusted
OR_glm_base18 <- exp(coef(gl_model_base18))[2] #OR for SJL
lci_glmbase18 <- exp(ci_glmbase18[2,1])
uci_glmbase18 <- exp(ci_glmbase18[2,2])
p_glmbase18 <- glm_base18$p.value[2]


results_glmbase18 <- data_clean.frame(
  Model = "glmbase18",
  OR = OR_glm_base18,
  LCI = lci_glmbase18,
  UCI = uci_glmbase18,
  pvalue = p_glmbase18
)

# Baseline adjusted
OR_glm1_18 <- exp(coef(gl_model1_18))[2]
lci_glm1_18 <- exp(ci_glm1_18[2,1])
uci_glm1_18 <- exp(ci_glm1_18[2,2])
p_glm1_18 <- glm1_18$p.value[2]


results_glm1_18 <- data_clean.frame(
  Model = "glm1_18",
  OR = OR_glm1_18,
  LCI = lci_glm1_18,
  UCI = uci_glm1_18,
  pvalue = p_glm1_18
)

# Demographic adjusted
OR_glm2_18 <- exp(coef(gl_model2_18))[2]
lci_glm2_18 <- exp(ci_glm2_18[2,1])
uci_glm2_18 <- exp(ci_glm2_18[2,2])
p_glm2_18 <- glm2_18$p.value[2]


results_glm2_18 <- data_clean.frame(
  Model = "glm2_18",
  OR = OR_glm2_18,
  LCI = lci_glm2_18,
  UCI = uci_glm2_18,
  pvalue = p_glm2_18
)

# Fully adjusted 
OR_glm3_18 <- exp(coef(gl_model3_18))[2]
lci_glm3_18 <- exp(ci_glm3_18[2,1])
uci_glm3_18 <- exp(ci_glm3_18[2,2])
p_glm3_18 <- glm3_18$p.value[2]


results_glm3_18 <- data_clean.frame(
  Model = "glm3_18",
  OR = OR_glm3_18,
  LCI = lci_glm3_18,
  UCI = uci_glm3_18,
  pvalue = p_glm3_18
)

# ------------------------------------------------------------------------------
# Results @ 21
# ------------------------------------------------------------------------------


# Unadjusted
OR_glm_base21 <- exp(coef(gl_model_base21))[2]
lci_glmbase21 <- exp(ci_glmbase21[2,1])
uci_glmbase21 <- exp(ci_glmbase21[2,2])
p_glmbase21 <- glm_base21$p.value[2]


results_glmbase21 <- data_clean.frame(
  Model = "glmbase21",
  OR = OR_glm_base21,
  LCI = lci_glmbase21,
  UCI = uci_glmbase21,
  pvalue = p_glmbase21
)

# Baseline adjusted
OR_glm1_21 <- exp(coef(gl_model1_21))[2]
lci_glm1_21 <- exp(ci_glm1_21[2,1])
uci_glm1_21 <- exp(ci_glm1_21[2,2])
p_glm1_21 <- glm1_21$p.value[2]


results_glm1_21 <- data_clean.frame(
  Model = "glm1_21",
  OR = OR_glm1_21,
  LCI = lci_glm1_21,
  UCI = uci_glm1_21,
  pvalue = p_glm1_21
)

# Demographic adjusted
OR_glm2_21 <- exp(coef(gl_model2_21))[2]
lci_glm2_21 <- exp(ci_glm2_21[2,1])
uci_glm2_21 <- exp(ci_glm2_21[2,2])
p_glm2_21 <- glm2_21$p.value[2]


results_glm2_21 <- data_clean.frame(
  Model = "glm2_21",
  OR = OR_glm2_21,
  LCI = lci_glm2_21,
  UCI = uci_glm2_21,
  pvalue = p_glm2_21
)

# Fully adjusted 
OR_glm3_21 <- exp(coef(gl_model3_21))[2]
lci_glm3_21 <- exp(ci_glm3_21[2,1])
uci_glm3_21 <- exp(ci_glm3_21[2,2])
p_glm3_21 <- glm3_21$p.value[2]


results_glm3_21 <- data_clean.frame(
  Model = "glm3_21",
  OR = OR_glm3_21,
  LCI = lci_glm3_21,
  UCI = uci_glm3_21,
  pvalue = p_glm3_21
)

# ------------------------------------------------------------------------------
# Results @ 23
# ------------------------------------------------------------------------------

# Unadjusted
OR_glm_base23 <- exp(coef(gl_model_base23))[2] #odds ratio for SJL
lci_glmbase23 <- exp(ci_glmbase23[2,1])
uci_glmbase23 <- exp(ci_glmbase23[2,2])
p_glmbase23 <- glm_base23$p.value[2]


results_glmbase23 <- data_clean.frame(
  Model = "glmbase23",
  OR = OR_glm_base23,
  LCI = lci_glmbase23,
  UCI = uci_glmbase23,
  pvalue = p_glmbase23
)

# Baseline adjusted
OR_glm1_23 <- exp(coef(gl_model1_23))[2]
lci_glm1_23 <- exp(ci_glm1_23[2,1])
uci_glm1_23 <- exp(ci_glm1_23[2,2])
p_glm1_23 <- glm1_23$p.value[2]


results_glm1_23 <- data_clean.frame(
  Model = "glm1_23",
  OR = OR_glm1_23,
  LCI = lci_glm1_23,
  UCI = uci_glm1_23,
  pvalue = p_glm1_23
)

# Demographic adjusted
OR_glm2_23 <- exp(coef(gl_model2_23))[2]
lci_glm2_23 <- exp(ci_glm2_23[2,1])
uci_glm2_23 <- exp(ci_glm2_23[2,2])
p_glm2_23 <- glm2_23$p.value[2]


results_glm2_23 <- data_clean.frame(
  Model = "glm2_23",
  OR = OR_glm2_23,
  LCI = lci_glm2_23,
  UCI = uci_glm2_23,
  pvalue = p_glm2_23
)

# Fully adjusted
OR_glm3_23 <- exp(coef(gl_model3_23))[2]
lci_glm3_23 <- exp(ci_glm3_23[2,1])
uci_glm3_23 <- exp(ci_glm3_23[2,2])
p_glm3_23 <- glm3_23$p.value[2]


results_glm3_23 <- data_clean.frame(
  Model = "glm3_23",
  OR = OR_glm3_23,
  LCI = lci_glm3_23,
  UCI = uci_glm3_23,
  pvalue = p_glm3_23
)

##########################################################################
#3. Preparing model output for forest plot
##########################################################################

# Combine results for each age
glm_results_18 <- rbind(results_glmbase18, results_glm1_18, results_glm2_18, results_glm3_18)
glm_results_21 <- rbind(results_glmbase21, results_glm1_21, results_glm2_21, results_glm3_21)
glm_results_23 <- rbind(results_glmbase23, results_glm1_23, results_glm2_23, results_glm3_23)

# Combine results for all models and all ages
results_all_glm <- rbind(
  results_glmbase18, results_glm1_18, results_glm2_18, results_glm3_18,
  results_glmbase21, results_glm1_21, results_glm2_21, results_glm3_21,
  results_glmbase23, results_glm1_23, results_glm2_23, results_glm3_23
) %>%
  mutate(Age = case_when(
    grepl("18$", Model) ~ "18",
    grepl("21$", Model) ~ "21",
    grepl("23$", Model) ~ "23"
  ))%>%
  mutate(Model = factor(Model, levels = c(
    "glmbase18","glm1_18","glm2_18","glm3_18",
    "glmbase21","glm1_21","glm2_21","glm3_21",
    "glmbase23","glm1_23","glm2_23","glm3_23"
  )))%>%
  rename(model_code= Model)%>%
  mutate(Model = recode(model_code,
                        "glmbase18"= 'Unadjusted' , 
                        "glm1_18" = 'Baseline Adjusted', 
                        "glm2_18" = 'Demographic Adjusted' ,
                        "glm3_18" = 'Fully Adjusted', 
                        "glmbase21"= 'Unadjusted' , 
                        "glm1_21" = 'Baseline Adjusted', 
                        "glm2_21" = 'Demographic Adjusted' ,
                        "glm3_21" = 'Fully Adjusted', 
                        "glmbase23"= 'Unadjusted' , 
                        "glm1_23" = 'Baseline Adjusted', 
                        "glm2_23" = 'Demographic Adjusted' ,
                        "glm3_23" = 'Fully Adjusted'))%>%
  relocate(Age , .after = model_code)%>%
  relocate(Model , .after = Age)


# ------------------------------------------------------------------------------
# Spacing for forest plot
# ------------------------------------------------------------------------------

blank_row <- results_all_glm[1, ] 
blank_row[,] <- NA                   

#Inserting blank row into results table to add spacing
results_all_glm_spaced <- rbind(
  results_all_glm[results_all_glm$Age == "18", ],
  blank_row,
  results_all_glm[results_all_glm$Age == "21", ],
  blank_row,
  results_all_glm[results_all_glm$Age == "23", ]
)

n_rows <- nrow(results_all_glm_spaced) #used later to bold only headers

##########################################################################
#4.Creating forest plot
##########################################################################

# ------------------------------------------------------------------------------
# Text to go next to plot
# ------------------------------------------------------------------------------

#creating df for forest plot
forestplot_glm <- data_clean.frame(
  mean = c(NA,results_all_glm_spaced$OR),
  lower = c(NA, results_all_glm_spaced$LCI),
  upper = c(NA, results_all_glm_spaced$UCI))

#creating text to sit next to forest plot
forestplot_glm_tabletext <- cbind(
  # Age column
  c("Age", 
    as.character(
      ifelse(is.na(results_all_glm_spaced$Age), "", #if Age is NA replace with blank string
             ifelse(duplicated(results_all_glm_spaced$Age), "", #if age has already appeared above replace with blank string
                    as.character(results_all_glm_spaced$Age))))), #if not display the age value
  # Model column
  c("Model",
    as.character(
      ifelse(is.na(results_all_glm_spaced$Model), "", 
             as.character(results_all_glm_spaced$Model)))),
  # OR column
  c("OR",
    as.character(
      ifelse(is.na(results_all_glm_spaced$OR), "", 
             sprintf("%.2f", results_all_glm_spaced$OR)))),
  # 95% CI column
  c("95% CI",
    as.character(
      ifelse(is.na(results_all_glm_spaced$LCI), "", 
             paste0(sprintf("%.2f", results_all_glm_spaced$LCI), " - ",
                    sprintf("%.2f", results_all_glm_spaced$UCI))))),
  #p value column
  c("p value",
    as.character(
      ifelse(is.na(results_all_glm_spaced$pvalue), "", 
             sprintf("%.2f", results_all_glm_spaced$pvalue)))))

# ------------------------------------------------------------------------------
# Actually creating plot
# ------------------------------------------------------------------------------

forestplot(
  forestplot_glm_tabletext,
  forestplot_glm,
  zero = 1,
  lwd.zero = 2,
  colgap = unit(10, "mm"),
  boxsize = 0.3,
  lwd.ci = 2,
  col=fpColors(line='black'),
  align = c('l','l','l','c','c'),
  is.summary = c(TRUE, rep(FALSE, n_rows)),  # only make header bold
  txt_gp = fpTxtGp(
    label = gpar(fontface = 2)
  ),
  xticks = c(0.7,0.8,0.9,1,1.1,1.2,1.3,1.4),
  xlab = 'OR (95% CIs)'
)

##########################################################################
#5.Creating forest plot
##########################################################################

summary(gl_model3_18)
summary(gl_model3_21)
summary(gl_model3_23)

