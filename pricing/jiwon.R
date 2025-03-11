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
<<<<<<< HEAD
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
=======

# View dataset structure
str(df)
summary(df)

# correlation
# Clear workspace
rm(list=ls())

# Select numeric columns explicitly
numeric_data <- hospital_data %>% select_if(is.numeric)

# Correlation matrix
corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
print(corr_matrix)
>>>>>>> ccd9864 (Question e up to date)

# Identify highly correlated variables (>0.3 or <-0.3)
high_corr_indices <- which(abs(corr_matrix) > 0.3 & abs(corr_matrix) < 1, arr.ind = TRUE)

high_corr_df <- data.frame(
  Variable1 = rownames(corr_matrix)[high_corr_indices[,1]],
  Variable2 = colnames(corr_matrix)[high_corr_indices[,2]],
  Correlation = corr_matrix[high_corr_indices]
)

<<<<<<< HEAD
# Remove duplicate correlations
high_corr_df <- high_corr_df %>% distinct()
print(high_corr_df)

# Check for multicollinearity using VIF
model_for_vif <- lm(total_cost_to_hospital ~ ., data = numeric_data)

# Compute VIF values
vif_values <- vif(model_for_vif)
print(vif_values)

# Identify variables with VIF > 5
=======
# Removing duplicate pairs
high_corr_df <- high_corr_df %>% filter(as.numeric(Variable1) < as.numeric(Variable2))

print(high_corr_df)

# Check for multicollinearity using VIF (Variance Inflation Factor)
# Replace 'Total_Cost' with your actual dependent variable's column name
model_for_vif <- lm(total_cost_to_hospital ~ ., data = numeric_data)

# Calculate VIF values
vif_values <- vif(model_for_vif)
print(vif_values)

# Identify variables with VIF greater than 5
>>>>>>> ccd9864 (Question e up to date)
high_vif <- vif_values[vif_values > 5]
print(high_vif)

# Identify aliased (perfectly collinear) variables
alias_info <- alias(lm(total_cost_to_hospital ~ ., data = numeric_data))
print(alias_info)

<<<<<<< HEAD
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
=======
# List of aliased variables to remove
aliased_vars <- c("cad_svd", "none_19", "none_31", "alert", "elective")

# Remove the aliased variables from the dataset
numeric_data_cleaned <- numeric_data[, !(names(numeric_data) %in% aliased_vars)]

# Verify the structure of the cleaned dataset
str(numeric_data_cleaned)

# Fit a new linear regression model after removing aliased variables
model_for_vif_cleaned <- lm(total_cost_to_hospital ~ ., data = numeric_data_cleaned)

# Compute VIF values for remaining variables
vif_values_cleaned <- vif(model_for_vif_cleaned)

# Print VIF values
print(vif_values_cleaned)

# Identify variables with high VIF (>5)
>>>>>>> ccd9864 (Question e up to date)
high_vif <- vif_values_cleaned[vif_values_cleaned > 5]
print(high_vif)

# Check correlation between body_weight and body_height
cor(numeric_data_cleaned$body_weight, numeric_data_cleaned$body_height, use = "complete.obs")

<<<<<<< HEAD
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
=======
# List of variables to remove
vars_to_remove <- c("total_length_of_stay", "length_of_stay_icu", "length_of_stay_ward", "body_height")

# Remove the selected variables
numeric_data_final <- numeric_data_cleaned[, !(names(numeric_data_cleaned) %in% vars_to_remove)]

# Check the structure of the cleaned dataset
str(numeric_data_final)

# Remove the selected variables
numeric_data_final <- numeric_data_cleaned[, !(names(numeric_data_cleaned) %in% vars_to_remove)]

# Recalculate VIF
model_final <- lm(total_cost_to_hospital ~ ., data = numeric_data_final)
vif_final <- vif(model_final)

# Print new VIF values
print(vif_final)

# Identify if any variables still have high VIF
high_vif_final <- vif_final[vif_final > 5]
print(high_vif_final)


# a. linear regression between total cost and body weight
# Fit a simple linear regression model
model_linear <- lm(total_cost_to_hospital ~ body_weight, data = numeric_data_final)

# Model summary
summary(model_linear)

# Interpretation of the regression coefficient
>>>>>>> ccd9864 (Question e up to date)
cat("For each additional kg increase in body weight, the total cost changes by INR",
    round(coef(model_linear)[2], 2), "on average.\n")

# Diagnostic plots
par(mfrow=c(2,2))
plot(model_linear)
par(mfrow=c(1,1))

<<<<<<< HEAD
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
=======
# Check for heteroscedasticity (non-constant variance)
ncvTest(model_linear)

# b. log linear model

# Fit a log-linear regression model
model_loglinear <- lm(log(total_cost_to_hospital) ~ body_weight, data = numeric_data_final)

# Model summary
summary(model_loglinear)

# Compare R-squared and residual standard error
cat("Linear Model R-squared:", summary(model_linear)$r.squared, "\n")
cat("Log-Linear Model R-squared:", summary(model_loglinear)$r.squared, "\n")

# Compare AIC values
cat("Linear Model AIC:", AIC(model_linear), "\n")
cat("Log-Linear Model AIC:", AIC(model_loglinear), "\n")

# Plot diagnostic plots for log-linear model
>>>>>>> ccd9864 (Question e up to date)
par(mfrow=c(2,2))
plot(model_loglinear)
par(mfrow=c(1,1))

<<<<<<< HEAD
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

=======
# c
# Define the regression coefficients
intercept <- coef(model_loglinear)[1]
slope <- coef(model_loglinear)[2]

# Compute predicted log-costs
log_cost_50 <- intercept + slope * 50
log_cost_51 <- intercept + slope * 51

# Convert log-costs to actual costs
cost_50 <- exp(log_cost_50)
cost_51 <- exp(log_cost_51)

# Compute cost difference
cost_difference <- cost_51 - cost_50
cat("Predicted cost difference:", round(cost_difference, 2), "INR\n")

# Extract standard error of the slope
se_slope <- summary(model_loglinear)$coefficients[2,2]

# Compute standard error of the difference
se_diff <- se_slope * exp(log_cost_50)

# Compute 95% confidence interval for the difference
>>>>>>> ccd9864 (Question e up to date)
lower_bound <- cost_difference - 1.96 * se_diff
upper_bound <- cost_difference + 1.96 * se_diff

cat("95% Confidence Interval for Cost Difference: (", round(lower_bound, 2), ",", round(upper_bound, 2), ")\n")

<<<<<<< HEAD
=======
# Check if INR 1,000 is within the confidence interval
>>>>>>> ccd9864 (Question e up to date)
if (lower_bound > 1000) {
  cat("Yes, a 51kg patient is likely to spend at least INR 1,000 more than a 50kg patient at 5% significance level.\n")
} else {
  cat("No, we cannot infer this with 95% confidence.\n")
}

<<<<<<< HEAD
# Question (d)
patient_weight <- 50
predicted_log_cost <- predict(model_loglinear, newdata = data.frame(body_weight = patient_weight), se.fit = TRUE)

mean_log_cost <- predicted_log_cost$fit
se_log_cost <- predicted_log_cost$se.fit

log_package_price <- log(300000)

prob_exceed <- plnorm(300000, meanlog = mean_log_cost, sdlog = se_log_cost, lower.tail = FALSE)

cat("Probability that the treatment cost exceeds INR 300,000:", prob_exceed, "\n")
=======
# d
# Define weight of the patient
patient_weight <- 50

# Predict log(cost) for 50 kg
predicted_log_cost <- predict(model_loglinear, newdata = data.frame(body_weight = patient_weight), se.fit = TRUE)

# Extract mean and standard error
mean_log_cost <- predicted_log_cost$fit
se_log_cost <- predicted_log_cost$se.fit

# Convert package price (INR 300,000) to log scale
log_package_price <- log(300000)

# Compute probability that the log cost exceeds log(300000)
prob_exceed <- 1 - pnorm(log_package_price, mean = mean_log_cost, sd = se_log_cost)

# Print the probability
cat("Probability that the treatment cost exceeds INR 300,000:", prob_exceed, "\n")


>>>>>>> ccd9864 (Question e up to date)
