# Load libraries
library(MatchIt)
library(tableone)

# Load data
my_data <- read.csv("Y:/Rachel Wolansky/Intrahepatic Cholangio/Data Out/Step 4 Adj vs Surgery pstage node positive 2006-2017.csv")

# Define covariates
all_covariates <- c("AGE", "RACE", "SEX", "INSURANCE_STATUS", "GRADE_COMBINED", "CDS", "margins", "xrt")

# Helper functions
get_smd <- function(data, vars) {
  table_temp <- CreateTableOne(vars=vars, strata="adjchemo", data=data, test=FALSE)
  smd_matrix <- ExtractSmd(table_temp)
  smd_vector <- as.numeric(smd_matrix[,1])
  names(smd_vector) <- rownames(smd_matrix)
  return(smd_vector)
}

get_imbalanced <- function(smd_values, threshold = 0.2) {
  names(smd_values)[abs(smd_values) > threshold]
}

# Treatment distribution and pre-matching table
print("=== TREATMENT DISTRIBUTION ===")
print(table(my_data$adjchemo))

print("\n=== PRE-MATCHING ===")
table1 <- CreateTableOne(vars=all_covariates, strata="adjchemo", data=my_data, test=FALSE)
print(table1, smd=TRUE)

# ===== PRE-MATCHING SURVIVAL ANALYSIS =====
print("\n=== PRE-MATCHING SURVIVAL ANALYSIS ===")
print("Prognostic factor analysis in the entire unmatched node-positive cohort")

# Load survival libraries if not already loaded
library(survival)
library(survminer)

# Create 5-year restricted dataset for pre-matching analysis
my_data_5yr <- my_data
my_data_5yr$lastcontact_5yr <- pmin(my_data$lastcontact, 60)
my_data_5yr$mortality_5yr <- my_data$mortality
my_data_5yr$mortality_5yr[my_data$lastcontact > 60 & my_data$mortality == 0] <- 0

# Create survival object for pre-matching analysis
surv_obj_prematch <- Surv(time = my_data_5yr$lastcontact_5yr, event = my_data_5yr$mortality_5yr)

# Univariate Cox models for each variable
print("\n=== UNIVARIATE COX MODELS (PRE-MATCHING) ===")
univariate_results <- data.frame()

for(var in c("adjchemo", all_covariates)) {
  if(var %in% c("AGE", "CDS")) {
    # Continuous variables
    formula_str <- paste("surv_obj_prematch ~", var)
  } else {
    # Categorical variables
    formula_str <- paste("surv_obj_prematch ~ factor(", var, ")")
  }
  
  tryCatch({
    cox_uni <- coxph(as.formula(formula_str), data = my_data_5yr)
    cox_summary <- summary(cox_uni)
    
    # Extract results for each coefficient
    for(i in 1:length(coef(cox_uni))) {
      var_name <- names(coef(cox_uni))[i]
      hr <- exp(coef(cox_uni)[i])
      hr_lower <- exp(confint(cox_uni))[i,1]
      hr_upper <- exp(confint(cox_uni))[i,2]
      p_value <- cox_summary$coefficients[i,5]
      
      univariate_results <- rbind(univariate_results, data.frame(
        Variable = var_name,
        HR = hr,
        HR_CI_Lower = hr_lower,
        HR_CI_Upper = hr_upper,
        P_value = p_value
      ))
    }
  }, error = function(e) {
    print(paste("Error with variable", var, ":", e$message))
  })
}

# Print univariate results
print("Univariate Cox Regression Results (5-year survival):")
print(knitr::kable(univariate_results, 
                   col.names = c("Variable", "Hazard Ratio", "95% CI Lower", "95% CI Upper", "P-value"),
                   digits = 3,
                   format = "pipe"))

# Multivariable Cox model (pre-matching)
print("\n=== MULTIVARIABLE COX MODEL (PRE-MATCHING) ===")
cox_model_prematch <- coxph(
  Surv(time = lastcontact_5yr, event = mortality_5yr) ~ 
    adjchemo + AGE + factor(RACE) + factor(SEX) + 
    factor(INSURANCE_STATUS) + factor(GRADE_COMBINED) + CDS + 
    factor(margins) + factor(xrt),
  data = my_data_5yr
)

# Get the summary with hazard ratios and confidence intervals
cox_summary_prematch <- summary(cox_model_prematch)
print(cox_summary_prematch)

# Create a formatted table with hazard ratios and confidence intervals
cox_results_prematch <- data.frame(
  Variable = names(coef(cox_model_prematch)),
  HR = exp(coef(cox_model_prematch)),
  HR_CI_Lower = exp(confint(cox_model_prematch))[,1],
  HR_CI_Upper = exp(confint(cox_model_prematch))[,2],
  P_value = cox_summary_prematch$coefficients[,5]
)

# Print formatted results
print("Multivariable Cox Regression Results (Pre-matching):")
print(knitr::kable(cox_results_prematch, 
                   col.names = c("Variable", "Hazard Ratio", "95% CI Lower", "95% CI Upper", "P-value"),
                   digits = 3,
                   format = "pipe"))

# ===== PROPENSITY SCORE MATCHING =====
print("\n\n=== PROPENSITY SCORE MATCHING ===")

# Step 1: Initial matching with caliper only
print("\n=== STEP 1: INITIAL CALIPER MATCHING ===")
formula <- as.formula(paste("adjchemo ~", paste(all_covariates, collapse = " + ")))
psmatch <- matchit(formula, data=my_data, method="nearest", estimand="ATT", ratio=1, caliper=0.1)
matched_data <- match.data(psmatch)
smd_values <- get_smd(matched_data, all_covariates)

print("SMDs after initial caliper matching:")
print(round(smd_values, 3))
print(paste("Sample size:", nrow(matched_data)))

# Initialize tracking variables
exact_vars <- c()
step_num <- 2

# Iterative exact matching - one variable at a time
while(length(get_imbalanced(smd_values)) > 0 && step_num <= 10) {
  imbalanced_vars <- get_imbalanced(smd_values)
  
  print(paste("\n=== STEP", step_num, ": EXACT MATCHING ==="))
  print(paste("Currently imbalanced (>0.2):", paste(imbalanced_vars, collapse=", ")))
  
  # Find the variable with highest SMD that can use exact matching (≤10 unique values)
  best_var <- NULL
  highest_smd <- 0
  
  for(var in imbalanced_vars) {
    unique_vals <- length(unique(my_data[[var]], na.rm=TRUE))
    if(unique_vals <= 10 && abs(smd_values[var]) > highest_smd) {
      best_var <- var
      highest_smd <- abs(smd_values[var])
    }
  }
  
  if(is.null(best_var)) {
    print("No suitable variables for exact matching (all have >10 unique values)")
    break
  }
  
  # Add this variable to exact matching
  exact_vars <- c(exact_vars, best_var)
  print(paste("Adding exact match on:", best_var, "(SMD =", round(smd_values[best_var], 3), ")"))
  print(paste("All exact variables:", paste(exact_vars, collapse=", ")))
  
  # Perform matching with current exact variables
  tryCatch({
    psmatch_exact <- matchit(formula, data=my_data, method="nearest", estimand="ATT", 
                             ratio=1, caliper=0.3, exact=exact_vars)
    matched_data_new <- match.data(psmatch_exact)
    smd_values_new <- get_smd(matched_data_new, all_covariates)
    
    print("SMDs after adding exact match:")
    print(round(smd_values_new, 3))
    print(paste("Sample size:", nrow(matched_data_new)))
    
    # Check if we lost too much sample size (>60% loss)
    original_n <- nrow(my_data)
    current_n <- nrow(matched_data_new)
    loss_pct <- (original_n - current_n) / original_n * 100
    
    if(loss_pct > 60) {
      print(paste("WARNING: Lost", round(loss_pct, 1), "% of sample. Consider stopping."))
    }
    
    # Update our working data and SMDs
    matched_data <- matched_data_new
    smd_values <- smd_values_new
    
  }, error = function(e) {
    print(paste("Error with exact matching on", paste(exact_vars, collapse=", "), ":", e$message))
    # Remove the problematic variable and continue
    exact_vars <- exact_vars[-length(exact_vars)]
    break
  })
  
  step_num <- step_num + 1
}

# Final results
print("\n=== FINAL RESULTS ===")
final_imbalanced <- get_imbalanced(smd_values)

if(length(final_imbalanced) == 0) {
  print("✓ SUCCESS: All SMDs < 0.2")
} else {
  print(paste("⚠ Still imbalanced:", paste(final_imbalanced, collapse=", ")))
  print("Final SMDs:")
  print(round(smd_values, 3))
}

print(paste("Final sample size:", nrow(matched_data)))
print(paste("Treatment/Control:", sum(matched_data$adjchemo == 1), "/", sum(matched_data$adjchemo == 0)))
print(paste("Sample size reduction:", round((nrow(my_data) - nrow(matched_data))/nrow(my_data)*100, 1), "%"))

if(length(exact_vars) > 0) {
  print(paste("Variables used for exact matching:", paste(exact_vars, collapse=", ")))
}

# Final table
print("\n=== FINAL MATCHED TABLE ===")
table_final <- CreateTableOne(vars=all_covariates, strata="adjchemo", data=matched_data, test=FALSE)
print(table_final, smd=TRUE)

# Check variable diversity in final sample
print("\n=== SAMPLE DIVERSITY CHECK ===")
for(var in all_covariates) {
  unique_vals <- length(unique(matched_data[[var]], na.rm=TRUE))
  print(paste(var, "- Unique values in final sample:", unique_vals))
}

# ===== POST-MATCHING SURVIVAL ANALYSIS =====
print("\n\n=== POST-MATCHING SURVIVAL ANALYSIS ===")

# IMPORTANT: Create pair IDs for matched data to enable stratified analysis
matched_data$pair_id <- rep(1:(nrow(matched_data)/2), each = 2)

# Create a survival object
surv_obj <- Surv(time = matched_data$lastcontact, event = matched_data$mortality)

# Fit Kaplan-Meier curves by treatment group
km_fit <- survfit(surv_obj ~ adjchemo, data = matched_data)

# Log-rank test
log_rank <- survdiff(surv_obj ~ adjchemo, data = matched_data)
print(log_rank)

# Calculate p-value from log-rank test
log_rank_p <- 1 - pchisq(log_rank$chisq, df = 1)
print(paste("Log-rank test p-value:", format(log_rank_p, digits = 5)))

# STRATIFIED Cox proportional hazards model (accounting for matched pairs)
print("\n=== STRATIFIED COX MODEL (accounting for matched pairs) ===")
cox_model_stratified <- coxph(surv_obj ~ adjchemo + strata(pair_id), data = matched_data)
print("Stratified Cox model results:")
summary(cox_model_stratified)

# Plot Kaplan-Meier curves with custom x-axis breaks and no grid lines
km_plot <- ggsurvplot(
  km_fit,
  data = matched_data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  xlab = "Months",
  ylab = "Overall Survival Probability",
  legend.labs = c("No Adjuvant Chemo", "Adjuvant Chemo"),
  palette = c("blue", "red"),
  title = "Overall Survival by Adjuvant Chemotherapy Status (Node-Positive)",
  xlim = c(0, 70),
  break.time.by = 12,     # Create breaks every 12 months
  axes.offset = FALSE,    # Remove space between axis and first tick
  ggtheme = theme_bw() +  # Use a clean theme
    theme(
      panel.grid.major = element_blank(), # Remove major grid lines
      panel.grid.minor = element_blank(), # Remove minor grid lines
      axis.text.x = element_text(angle = 0, hjust = 0.5)  # Horizontal x-axis labels
    )
)

print(km_plot)

# Calculate survival rates at specific time points
surv_rates <- summary(km_fit, times = c(12, 36, 60))

# Create a data frame for survival rates
survival_rates <- data.frame(
  Group = rep(c("No Adjuvant Chemo", "Adjuvant Chemo"), each = 3),
  `Time Point` = rep(c("1-year", "3-year", "5-year"), 2),
  `Survival Rate (%)` = c(
    surv_rates$surv[1:3] * 100,
    surv_rates$surv[4:6] * 100
  ),
  `95% CI Lower` = c(
    surv_rates$lower[1:3] * 100,
    surv_rates$lower[4:6] * 100
  ),
  `95% CI Upper` = c(
    surv_rates$upper[1:3] * 100,
    surv_rates$upper[4:6] * 100
  )
)

print(knitr::kable(survival_rates, format = "pipe"))

# Restrict follow-up time to 5 years (60 months)
# Create a modified dataset with censoring at 5 years
matched_data_5yr <- matched_data
matched_data_5yr$lastcontact_5yr <- pmin(matched_data$lastcontact, 60)
matched_data_5yr$mortality_5yr <- matched_data$mortality

# For patients with follow-up beyond 5 years who didn't die, censor them at 5 years
matched_data_5yr$mortality_5yr[matched_data$lastcontact > 60 & matched_data$mortality == 0] <- 0

# STRATIFIED Cox model for 5-year restricted data (PRIMARY ANALYSIS)
print("\n=== STRATIFIED COX MODEL - 5 YEAR RESTRICTED (PRIMARY ANALYSIS) ===")
cox_model_5yr_stratified <- coxph(
  Surv(time = lastcontact_5yr, event = mortality_5yr) ~ 
    adjchemo + strata(pair_id),
  data = matched_data_5yr
)

print("5-year stratified Cox model results:")
summary(cox_model_5yr_stratified)

# Optional: Cox model with robust standard errors clustered by pair
print("\n=== COX MODEL WITH ROBUST STANDARD ERRORS ===")
cox_model_robust <- coxph(
  Surv(time = lastcontact_5yr, event = mortality_5yr) ~ 
    adjchemo + cluster(pair_id),
  data = matched_data_5yr
)

print("Cox model with robust standard errors:")
summary(cox_model_robust)

# MULTIVARIABLE STRATIFIED Cox model (if needed for sensitivity analysis)
print("\n=== MULTIVARIABLE STRATIFIED COX MODEL (SENSITIVITY ANALYSIS) ===")
cox_model_mv_stratified <- coxph(
  Surv(time = lastcontact_5yr, event = mortality_5yr) ~ 
    adjchemo + AGE + factor(RACE) + factor(SEX) + 
    factor(INSURANCE_STATUS) + factor(GRADE_COMBINED) + CDS + 
    factor(margins) + factor(xrt) + strata(pair_id),
  data = matched_data_5yr
)

print("Multivariable stratified Cox model results:")
cox_summary_mv <- summary(cox_model_mv_stratified)
print(cox_summary_mv)

# Create a formatted table for multivariable stratified results
cox_results_mv_stratified <- data.frame(
  Variable = names(coef(cox_model_mv_stratified)),
  HR = exp(coef(cox_model_mv_stratified)),
  HR_CI_Lower = exp(confint(cox_model_mv_stratified))[,1],
  HR_CI_Upper = exp(confint(cox_model_mv_stratified))[,2],
  P_value = cox_summary_mv$coefficients[,5]
)

print("Multivariable Stratified Cox Regression Results (Post-matching):")
print(knitr::kable(cox_results_mv_stratified, 
                   col.names = c("Variable", "Hazard Ratio", "95% CI Lower", "95% CI Upper", "P-value"),
                   digits = 3,
                   format = "pipe"))

# Create survival object for 5-year data
surv_obj_5yr <- Surv(time = matched_data_5yr$lastcontact_5yr, event = matched_data_5yr$mortality_5yr)

# Fit Kaplan-Meier curves by treatment group
km_fit_5yr <- survfit(surv_obj_5yr ~ adjchemo, data = matched_data_5yr)

# Calculate exact 5-year survival rates with confidence intervals
surv_rates_5yr <- summary(km_fit_5yr, times = 60)

# Create a data frame for 5-year survival rates
survival_rates_5yr <- data.frame(
  Group = c("No Adjuvant Chemo", "Adjuvant Chemo"),
  `5-year Survival (%)` = surv_rates_5yr$surv * 100,
  `95% CI Lower` = surv_rates_5yr$lower * 100,
  `95% CI Upper` = surv_rates_5yr$upper * 100
)

# Print formatted 5-year survival table
print(knitr::kable(survival_rates_5yr, 
                   digits = 1,
                   format = "pipe"))

# Log-rank test for comparison
log_rank_5yr <- survdiff(surv_obj_5yr ~ adjchemo, data = matched_data_5yr)
log_rank_p_5yr <- 1 - pchisq(log_rank_5yr$chisq, df = 1)

# Print p-value
print(paste("P-value for 5-year survival difference:", format(log_rank_p_5yr, digits = 3)))

# ===== COMPARISON OF DIFFERENT ANALYTICAL APPROACHES =====
print("\n\n=== COMPARISON: POST-MATCHING TREATMENT EFFECTS ===")
print("Log-rank test (primary analysis for matched data):")
print(paste("P =", round(log_rank_p, 3)))

print("Stratified Cox model (accounting for matched pairs):")
stratified_hr <- exp(coef(cox_model_5yr_stratified)["adjchemo"])
stratified_ci <- exp(confint(cox_model_5yr_stratified)["adjchemo",])
stratified_p <- summary(cox_model_5yr_stratified)$coefficients["adjchemo", "Pr(>|z|)"]
print(paste("HR:", round(stratified_hr, 3), 
            "95% CI:", round(stratified_ci[1], 3), "-", round(stratified_ci[2], 3),
            "P =", round(stratified_p, 3)))

print("Cox model with robust standard errors:")
robust_hr <- exp(coef(cox_model_robust)["adjchemo"])
robust_p <- summary(cox_model_robust)$coefficients["adjchemo", "Pr(>|z|)"]
print(paste("HR:", round(robust_hr, 3), 
            "P =", round(robust_p, 3)))

# ===== COMPARISON OF PRE vs POST-MATCHING RESULTS =====
print("\n\n=== COMPARISON: PRE vs POST-MATCHING TREATMENT EFFECTS ===")
print("Pre-matching adjuvant chemo effect:")
adjchemo_prematch <- cox_results_prematch[cox_results_prematch$Variable == "adjchemo", ]
print(paste("HR:", round(adjchemo_prematch$HR, 3), 
            "95% CI:", round(adjchemo_prematch$HR_CI_Lower, 3), "-", round(adjchemo_prematch$HR_CI_Upper, 3),
            "P =", round(adjchemo_prematch$P_value, 3)))

print("Post-matching adjuvant chemo effect (STRATIFIED):")
print(paste("HR:", round(stratified_hr, 3), 
            "95% CI:", round(stratified_ci[1], 3), "-", round(stratified_ci[2], 3),
            "P =", round(stratified_p, 3)))