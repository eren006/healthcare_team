# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(car)  # For model diagnostics
library(caret)  # For data partitioning
library(MASS)  # For Box-Cox transformation

getwd()
setwd("/Users/jiwonchoi/Desktop/github")
# Load dataset
file_path <- "healthcare_team/pricing/data_cleaned.xlsx"  # Update with the correct path
hospital_data <- read_excel(file_path)  
# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(car)  # For model diagnostics
library(caret)  # For data partitioning
library(MASS)  # For Box-Cox transformation

# Clear workspace
rm(list=ls())

# Set working directory
setwd("/Users/jiwonchoi/Desktop/github")

# Load dataset
file_path <- "healthcare_team/pricing/data_cleaned.xlsx"  # Update with the correct path
hospital_data <- read_excel(file_path)  

# View dataset structure
str(hospital_data)
summary(hospital_data)

# Select numeric columns
numeric_data <- hospital_data %>% select_if(is.numeric)

# Compute correlation matrix
corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Identify highly correlated variables (>0.3 or <-0.3)
high_corr_indices <- which(abs(corr_matrix) > 0.3 & abs(corr_matrix) < 1, arr.ind = TRUE)

high_corr_df <- data.frame(
  Variable1 = rownames(corr_matrix)[high_corr_indices[,1]],
  Variable2 = colnames(corr_matrix)[high_corr_indices[,2]],
  Correlation = corr_matrix[high_corr_indices]
)

# Remove duplicate correlations
high_corr_df <- high_corr_df %>% distinct()
print(high_corr_df)

# Check for multicollinearity using VIF
model_for_vif <- lm(total_cost_to_hospital ~ ., data = numeric_data)

# Compute VIF values
vif_values <- vif(model_for_vif)
print(vif_values)

# Identify variables with VIF > 5
high_vif <- vif_values[vif_values > 5]
print(high_vif)

# Identify aliased (perfectly collinear) variables
alias_info <- alias(lm(total_cost_to_hospital ~ ., data = numeric_data))
print(alias_info)

# Remove aliased variables
aliased_vars <- c("cad_svd", "none_19", "none_31", "alert", "elective")
numeric_data_cleaned <- numeric_data[, !(names(numeric_data) %in% aliased_vars)]

# Verify cleaned dataset
str(numeric_data_cleaned)

# Recalculate VIF
model_for_vif_cleaned <- lm(total_cost_to_hospital ~ ., data = numeric_data_cleaned)
vif_values_cleaned <- vif(model_for_vif_cleaned)
print(vif_values_cleaned)

# Identify high VIF variables
high_vif <- vif_values_cleaned[vif_values_cleaned > 5]
print(high_vif)

# Check correlation between body_weight and body_height
cor(numeric_data_cleaned$body_weight, numeric_data_cleaned$body_height, use = "complete.obs")

# Remove high multicollinearity variables
vars_to_remove <- c("total_length_of_stay", "length_of_stay_icu", "length_of_stay_ward", "body_height")
numeric_data_final <- numeric_data_cleaned[, !(names(numeric_data_cleaned) %in% vars_to_remove)]

# Verify final cleaned dataset
str(numeric_data_final)

# Recalculate VIF
model_final <- lm(total_cost_to_hospital ~ ., data = numeric_data_final)
vif_final <- vif(model_final)
print(vif_final)

# Identify remaining high VIF variables
high_vif_final <- vif_final[vif_final > 5]
print(high_vif_final)

# Simple Linear Regression (a)
model_linear <- lm(total_cost_to_hospital ~ body_weight, data = numeric_data_final)
summary(model_linear)

# Interpretation of regression coefficient
cat("For each additional kg increase in body weight, the total cost changes by INR",
    round(coef(model_linear)[2], 2), "on average.\n")

# Diagnostic plots
par(mfrow=c(2,2))
plot(model_linear)
par(mfrow=c(1,1))

# Test for heteroscedasticity
ncvTest(model_linear)

# Log-Linear Model (b)
model_loglinear <- lm(log(total_cost_to_hospital) ~ body_weight, data = numeric_data_final)
summary(model_loglinear)

# Compare models
cat("Linear Model R-squared:", summary(model_linear)$r.squared, "\n")
cat("Log-Linear Model R-squared:", summary(model_loglinear)$r.squared, "\n")

cat("Linear Model AIC:", AIC(model_linear), "\n")
cat("Log-Linear Model AIC:", AIC(model_loglinear), "\n")

# Diagnostic plots for log-linear model
par(mfrow=c(2,2))
plot(model_loglinear)
par(mfrow=c(1,1))

# Question (c)
intercept <- coef(model_loglinear)[1]
slope <- coef(model_loglinear)[2]

log_cost_50 <- intercept + slope * 50
log_cost_51 <- intercept + slope * 51

cost_50 <- exp(log_cost_50)
cost_51 <- exp(log_cost_51)

cost_difference <- cost_51 - cost_50
cat("Predicted cost difference:", round(cost_difference, 2), "INR\n")

se_slope <- summary(model_loglinear)$coefficients[2,2]
se_diff <- sqrt((se_slope^2) * cost_50^2)  # Corrected error

lower_bound <- cost_difference - 1.96 * se_diff
upper_bound <- cost_difference + 1.96 * se_diff

cat("95% Confidence Interval for Cost Difference: (", round(lower_bound, 2), ",", round(upper_bound, 2), ")\n")

if (lower_bound > 1000) {
  cat("Yes, a 51kg patient is likely to spend at least INR 1,000 more than a 50kg patient at 5% significance level.\n")
} else {
  cat("No, we cannot infer this with 95% confidence.\n")
}

# Question (d)
patient_weight <- 50
predicted_log_cost <- predict(model_loglinear, newdata = data.frame(body_weight = patient_weight), se.fit = TRUE)

mean_log_cost <- predicted_log_cost$fit
se_log_cost <- predicted_log_cost$se.fit

log_package_price <- log(300000)

prob_exceed <- plnorm(300000, meanlog = mean_log_cost, sdlog = se_log_cost, lower.tail = FALSE)

cat("Probability that the treatment cost exceeds INR 300,000:", prob_exceed, "\n")
