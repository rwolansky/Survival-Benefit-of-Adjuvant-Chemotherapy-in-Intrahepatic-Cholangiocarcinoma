# Load libraries
library(MatchIt)
library(tableone)

# Load data
my_data <- read.csv("Y:/Rachel Wolansky/Intrahepatic Cholangio/Data Out/Step 4 Adj vs Surgery margin positive.csv")

# Define covariates
all_covariates <- c("AGE", "RACE", "SEX", "INSURANCE_STATUS", "CDS", "pStage", "xrt")

# Helper functions
get_smd <- function(data, vars) {
  table_temp <- CreateTableOne(vars=vars, strata="adjchemo", data=data, test=FALSE)
  smd_matrix <- ExtractSmd(table_temp)
  smd_vector <- as.numeric(smd_matrix[,1])
  names(smd_vector) <- rownames(smd_matrix)
  return(smd_vector)
}

# Treatment distribution and pre-matching table
print("=== TREATMENT DISTRIBUTION ===")
print(table(my_data$adjchemo))

print("\n=== PRE-MATCHING ===")
table1 <- CreateTableOne(vars=all_covariates, strata="adjchemo", data=my_data, test=FALSE)
print(table1, smd=TRUE)

# Initial matching with caliper only (sufficient for this dataset)
print("\n=== MATCHING WITH CALIPER ===")
formula <- as.formula(paste("adjchemo ~", paste(all_covariates, collapse = " + ")))
psmatch <- matchit(formula, data=my_data, method="nearest", estimand="ATT", caliper = 0.25, ratio=1)
matched_data <- match.data(psmatch)
smd_values <- get_smd(matched_data, all_covariates)

print("SMDs after matching:")
print(round(smd_values, 3))

print(paste("Final sample size:", nrow(matched_data)))
print(paste("Treatment/Control:", sum(matched_data$adjchemo == 1), "/", sum(matched_data$adjchemo == 0)))

# Final table
print("\n=== FINAL MATCHED TABLE ===")
table_final <- CreateTableOne(vars=all_covariates, strata="adjchemo", data=matched_data, test=FALSE)
print(table_final, smd=TRUE)

# Load necessary libraries
library(survival)
library(survminer)

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

# Cox proportional hazards model
cox_model <- coxph(surv_obj ~ adjchemo, data = matched_data)
summary(cox_model)

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
  title = "Overall Survival by Adjuvant Chemotherapy Status",
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

#Restrict follow-up time to 5 years (60 months)
# Create a modified dataset with censoring at 5 years
matched_data_5yr <- matched_data
matched_data_5yr$lastcontact_5yr <- pmin(matched_data$lastcontact, 60)
matched_data_5yr$mortality_5yr <- matched_data$mortality

# For patients with follow-up beyond 5 years who didn't die, censor them at 5 years
matched_data_5yr$mortality_5yr[matched_data$lastcontact > 60 & matched_data$mortality == 0] <- 0

# Run the full Cox model restricted to 5-year outcomes
cox_model_5yr <- coxph(
  Surv(time = lastcontact_5yr, event = mortality_5yr) ~ 
    adjchemo + AGE + factor(RACE) + factor(SEX) + 
    factor(INSURANCE_STATUS) + factor(xrt),
  data = matched_data_5yr
)