# -------------------------------
# Combined R Script for Regression Analysis on Healthcare Data
# -------------------------------

# Load necessary libraries (without janitor or writexl)
library(tidyverse)
library(readxl)
library(car)
library(ggplot2)
library(MASS)
library(corrplot)

# Set seed for reproducibility
set.seed(42)

# -------------------------------
# 1. Data Loading and Cleaning
# -------------------------------
getwd()
setwd("/Users/jiwonchoi/Desktop/github")
# Read in the data (using the modified data sheet as in Erin.R)
data <- read_excel("healthcare_team/pricing/data_cleaned.xlsx")

# Define selected columns for analysis
selected_cols <- c("age", "body_weight", "body_height", "hr_pulse", "hb", 
                   "total_cost_to_hospital", "total_length_of_stay", 
                   "length_of_stay_icu", "length_of_stay_ward")

# Subset data using base R instead of select(one_of(...))
data_subset <- data[, selected_cols]

# Convert selected columns to long format for outlier detection
data_long <- pivot_longer(data_subset, cols = everything(), 
                          names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  mutate(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower_bound = q1 - 1.5 * iqr,
    upper_bound = q3 + 1.5 * iqr,
    outlier = (value < lower_bound | value > upper_bound)
  )

# Helper function to flag outliers in a vector
identify_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr_val
  upper_bound <- q3 + 1.5 * iqr_val
  return(x < lower_bound | x > upper_bound)
}

# Create a logical data frame for outliers in the selected columns
outlier_flags <- as.data.frame(lapply(data_subset, identify_outliers))

# Flag rows that are outliers in any of the selected columns
data$outlier_flag <- apply(outlier_flags, 1, any)

# Remove rows with any outliers using base R
data_cleaned <- data[!data$outlier_flag, ]
data_cleaned$outlier_flag <- NULL

# -------------------------------
# Remove Aliased / Highly Collinear Variables
# -------------------------------
# Remove variables that are aliased or causing multicollinearity.
aliased_vars <- c("cad_svd", "none_19", "none_31", "alert", "elective")
data_cleaned <- data_cleaned[, !(names(data_cleaned) %in% aliased_vars)]

# Also remove variables highly correlated with others (e.g., lengths of stay and body_height)
vars_to_remove <- c("total_length_of_stay", "length_of_stay_icu", "length_of_stay_ward", "body_height")
data_final <- data_cleaned[, !(names(data_cleaned) %in% vars_to_remove)]

# Check the structure of the final cleaned dataset
str(data_final)

# -------------------------------
# 2. Regression Analysis
# -------------------------------

# (a) Simple Linear Regression Model:
#     Check the association between total cost and body weight.
model_linear <- lm(total_cost_to_hospital ~ body_weight, data = data_final)
summary(model_linear)

# Interpretation:
# The coefficient for body_weight represents the average change in total_cost_to_hospital (in INR)
# for every additional kg of body weight.

# Diagnostic Plots for the linear model
par(mfrow = c(2, 2))
plot(model_linear)
par(mfrow = c(1, 1))

# Test for heteroscedasticity
ncvTest(model_linear)

# (b) Log-Linear Model:
#     Model the logarithm of total cost versus body weight.
model_loglinear <- lm(log(total_cost_to_hospital) ~ body_weight, data = data_final)
summary(model_loglinear)


# Compare models using R-squared and AIC
cat("Linear Model R-squared:", summary(model_linear)$r.squared, "\n")
cat("Log-Linear Model R-squared:", summary(model_loglinear)$r.squared, "\n")
cat("Linear Model AIC:", AIC(model_linear), "\n")
cat("Log-Linear Model AIC:", AIC(model_loglinear), "\n")

# Diagnostic Plots for the log-linear model
par(mfrow = c(2, 2))
plot(model_loglinear)
par(mfrow = c(1, 1))

# (c) Testing the INR 1,000 Difference using the Log-Linear Model:
# For patients weighing 50kg vs. 51kg, calculate the predicted cost difference.

# Extract coefficients from the log-linear model
intercept <- coef(model_loglinear)[1]
slope <- coef(model_loglinear)[2]

# Calculate predicted log costs for 50kg and 51kg
log_cost_50 <- intercept + slope * 50
log_cost_51 <- intercept + slope * 51

# Convert predictions back to original scale
cost_50 <- exp(log_cost_50)
cost_51 <- exp(log_cost_51)

# Predicted cost difference
predicted_diff <- cost_51 - cost_50
cat("Predicted cost difference between 51kg and 50kg:", round(predicted_diff, 2), "INR\n")

# Approximate the standard error for the difference using the delta method:
se_slope <- summary(model_loglinear)$coefficients[2, 2]
# Approximate standard error of cost at 50kg as cost_50 * se_slope.
se_diff <- cost_50 * se_slope

# 95% Confidence Interval for the cost difference
lower_bound <- predicted_diff - 1.96 * se_diff
upper_bound <- predicted_diff + 1.96 * se_diff
cat("95% CI for cost difference: (", round(lower_bound, 2), ",", round(upper_bound, 2), ")\n")

if (lower_bound > 1000) {
  cat("Conclusion (c): At 5% significance, a 51kg patient is likely to spend at least INR 1,000 more than a 50kg patient.\n")
} else {
  cat("Conclusion (c): At 5% significance, we cannot infer that a 51kg patient will spend at least INR 1,000 more than a 50kg patient.\n")
}

# (d) Probability that Treatment Cost Exceeds INR 300,000 for a 50kg Patient:
# Predict the log cost and its standard error for body_weight = 50.
predicted <- predict(model_loglinear, newdata = data.frame(body_weight = 50), se.fit = TRUE)
mean_log_cost <- predicted$fit
se_log_cost <- predicted$se.fit

# Under the assumption that log(total_cost_to_hospital) is normally distributed,
# the treatment cost follows a log-normal distribution.
# Calculate the probability that cost > 300,000 INR.
prob_exceed <- plnorm(300000, meanlog = mean_log_cost, sdlog = se_log_cost, lower.tail = FALSE)
cat("Probability that treatment cost exceeds INR 300,000 for a 50kg patient:", round(prob_exceed, 4), "\n")