# Load libraries
library(MatchIt)
library(tableone)

# Load data
my_data <- read.csv("Y:/Rachel Wolansky/Intrahepatic Cholangio/Data Out/Step 4 Adj vs Surgery clinical stage 3 and 4.csv")

# Define covariates
all_covariates <- c("AGE", "RACE", "SEX", "INSURANCE_STATUS", "CDS", "margins", "xrt", "LAD")

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

# Step 1: Initial KNN matching
print("\n=== STEP 1: INITIAL MATCHING ===")
formula <- as.formula(paste("adjchemo ~", paste(all_covariates, collapse = " + ")))
psmatch <- matchit(formula, data=my_data, method="nearest", estimand="ATT", ratio=1, caliper=0.1)
matched_data <- match.data(psmatch)
smd_values <- get_smd(matched_data, all_covariates)

print("SMDs after initial matching:")
print(round(smd_values, 3))
imbalanced_vars <- get_imbalanced(smd_values)
print(paste("Imbalanced (>0.2):", paste(imbalanced_vars, collapse=", ")))

# Step 2: Exact matching on imbalanced categorical variables
if(length(imbalanced_vars) > 0) {
  print("\n=== STEP 2: EXACT MATCHING ===")
  
  # Check which variables can use exact matching (≤10 unique values)
  exact_candidates <- c()
  for(var in imbalanced_vars) {
    unique_vals <- length(unique(my_data[[var]], na.rm=TRUE))
    if(unique_vals <= 10) {
      exact_candidates <- c(exact_candidates, var)
    }
  }
  
  if(length(exact_candidates) > 0) {
    print(paste("Exact matching on:", paste(exact_candidates, collapse=", ")))
    psmatch_exact <- matchit(formula, data=my_data, method="nearest", estimand="ATT", 
                             ratio=1, caliper=0.1, exact=exact_candidates)
    matched_data <- match.data(psmatch_exact)
    smd_values <- get_smd(matched_data, all_covariates)
    
    print("SMDs after exact matching:")
    print(round(smd_values, 3))
    imbalanced_vars <- get_imbalanced(smd_values)
  }
}

# Step 3: Second round if needed
if(length(imbalanced_vars) > 0) {
  print("\n=== STEP 3: SECOND ROUND EXACT MATCHING ===")
  
  exact_candidates_2 <- c()
  for(var in imbalanced_vars) {
    unique_vals <- length(unique(my_data[[var]], na.rm=TRUE))
    if(unique_vals <= 10) {
      exact_candidates_2 <- c(exact_candidates_2, var)
    }
  }
  
  if(length(exact_candidates_2) > 0) {
    all_exact_vars <- unique(c(exact_candidates, exact_candidates_2))
    print(paste("Exact matching on:", paste(all_exact_vars, collapse=", ")))
    
    psmatch_exact2 <- matchit(formula, data=my_data, method="nearest", estimand="ATT", 
                              ratio=1, caliper=0.2, exact=all_exact_vars)
    matched_data <- match.data(psmatch_exact2)
    smd_values <- get_smd(matched_data, all_covariates)
    
    print("SMDs after second round:")
    print(round(smd_values, 3))
    imbalanced_vars <- get_imbalanced(smd_values)
  }
}

# Step 4: Third round if needed
if(length(imbalanced_vars) > 0) {
  print("\n=== STEP 4: THIRD ROUND EXACT MATCHING ===")
  
  exact_candidates_3 <- c()
  for(var in imbalanced_vars) {
    unique_vals <- length(unique(my_data[[var]], na.rm=TRUE))
    if(unique_vals <= 10) {
      exact_candidates_3 <- c(exact_candidates_3, var)
    }
  }
  
  if(length(exact_candidates_3) > 0) {
    all_exact_vars_final <- unique(c(exact_candidates, exact_candidates_2, exact_candidates_3))
    print(paste("Exact matching on:", paste(all_exact_vars_final, collapse=", ")))
    
    psmatch_exact3 <- matchit(formula, data=my_data, method="nearest", estimand="ATT", 
                              ratio=1, caliper=0.2, exact=all_exact_vars_final)
    matched_data <- match.data(psmatch_exact3)
    smd_values <- get_smd(matched_data, all_covariates)
    
    print("SMDs after third round:")
    print(round(smd_values, 3))
    
    print(paste("Sample size after third round: n =", nrow(matched_data)))
  }
}

# Final results
print("\n=== FINAL RESULTS ===")
final_imbalanced <- get_imbalanced(smd_values)

if(length(final_imbalanced) == 0) {
  print("✓ SUCCESS: All SMDs < 0.2")
} else {
  print(paste("⚠ Still imbalanced:", paste(final_imbalanced, collapse=", ")))
}

print(paste("Final sample size:", nrow(matched_data)))
print(paste("Treatment/Control:", sum(matched_data$adjchemo == 1), "/", sum(matched_data$adjchemo == 0)))

# Final table
print("\n=== FINAL MATCHED TABLE ===")
table_final <- CreateTableOne(vars=all_covariates, strata="adjchemo", data=matched_data, test=FALSE)
print(table_final, smd=TRUE)



# Load necessary libraries
library(survival)
library(survminer)

# For survival analysis, you need two variables:
# 1. Time-to-event (lastcontact)
# 2. Event indicator (mortality)

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