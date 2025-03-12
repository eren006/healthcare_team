
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(stats)
library(tidyr)
library(writexl)
library(janitor)

# Load dataset
df <- read_excel("pricing/data_cleaned.xlsx", sheet = 1)
print(colnames(df))

#create dummy variable
df <- df %>% 
  mutate(is_Female = ifelse(trimws(gender) == "F", 1, 0)) %>%  
  select(-gender) 
df <- df %>%
  mutate(is_Unmarried = ifelse(trimws(marital_status) == "UNMARRIED", 1, 0)) %>%
  select(-marital_status)  
df <- df %>%
  select(-key_complaints_code)  
df <- df %>%
  select(-past_medical_history_code)  
df <- df %>%
  select(-mode_of_arrival)  
df <- df %>% select(-state_at_the_time_of_arrival)  
df <- df %>%
  select(-type_of_admsn) 
df <- df %>%
  select(-implant_used_y_n)

print(str(df))

#delete variable with only one feature
df <- df %>% select(-cad_svd)
df <- df %>% select(-alert)
df <- df %>% select(-sl)

view(df)

# --- EDA ---------------------------------------------
numeric <- c("age", "body_weight", "body_height", "hr_pulse", "rr", "hb",
             "total_cost_to_hospital", "total_length_of_stay", 
             "length_of_stay_icu", "length_of_stay_ward", "cost_of_implant")

# Separate the numeric columns from the categorical ones
numeric_data <- df %>% select(all_of(numeric))
numeric_data

# Identify categorical columns (all columns except numeric ones)
categorical_data <- df %>% select(-all_of(numeric))
categorical_data


# ------ For Numerical Variables:
# Corr Matrix -----
# Calculate the correlation matrix for numeric variables
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "spearman")

# Visualize the correlation matrix with a heatmap
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45, 
         diag = FALSE)

# Extract high correlation pairs (Spearman > 0.7)
high_cor_pairs <- which(cor_matrix > 0.7 & upper.tri(cor_matrix), arr.ind = TRUE)

high_cor <- data.frame(
  Variable1 = rownames(cor_matrix)[high_cor_pairs[, 1]],
  Variable2 = colnames(cor_matrix)[high_cor_pairs[, 2]],
  Correlation = cor_matrix[high_cor_pairs]
) %>% 
  arrange(desc(Correlation))

# Show results
print(high_cor)

# VIF test -----
lm_model <- lm(total_cost_to_hospital ~ age+ body_weight + body_height + hr_pulse + rr + hb +
                 total_length_of_stay + 
                 length_of_stay_icu + length_of_stay_ward, data = numeric_data)

# Calculate VIF and format results
vif_results <- vif(lm_model) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  rename(VIF = ".") %>%
  filter(VIF > 5) %>%
  arrange(desc(VIF))

# Display results
if(nrow(vif_results) > 0) {
  cat("Variables with VIF >5:\n")
  print(vif_results)
} else {
  cat("No variables with VIF >5 detected")
}

# ------ For Categorical Variables:
# Function to compute Cramér's V ----- 
calculate_cramers_v <- function(x, y) {
  tbl <- table(x, y)
  chisq_test <- chisq.test(tbl)
  cramer_v <- sqrt(chisq_test$statistic / (sum(tbl) * (min(dim(tbl)) - 1)))
  return(cramer_v)
}

# Compute Cramér's V for all categorical pairs
categorical_cols <- names(categorical_data)
categorical_cols

cramers_v_matrix <- outer(categorical_cols, categorical_cols, 
                          Vectorize(function(x, y) calculate_cramers_v(categorical_data[[x]], categorical_data[[y]])))

# Convert to data frame for visualization
cramers_v_df <- as.data.frame(cramers_v_matrix)
rownames(cramers_v_df) <- categorical_cols
colnames(cramers_v_df) <- categorical_cols

# Print the matrix
print(cramers_v_df)

# Set correlation threshold (adjust as needed)
threshold <- 0.7

# Get upper triangle indices of pairs meeting threshold
strong_pairs <- which(cramers_v_matrix >= threshold & 
                        upper.tri(cramers_v_matrix), 
                      arr.ind = TRUE)

# Create formatted results table
strong_associations <- data.frame(
  Variable1 = categorical_cols[strong_pairs[, 1]],
  Variable2 = categorical_cols[strong_pairs[, 2]],
  Cramers_V = cramers_v_matrix[strong_pairs]
) %>% 
  arrange(desc(Cramers_V)) %>%
  distinct()  # Remove duplicates if any

# Display results
if(nrow(strong_associations) > 0) {
  cat("Categorical variable pairs with Cramér's V ≥", threshold, "\n")
  print(strong_associations)
} else {
  cat("No categorical variable pairs with Cramér's V ≥", threshold)
}

print(colnames(df))

#--- Preprocessing: remove highly-collineared variables from results above
# Remove specified columns
df <- df %>%
  select(-body_weight, 
         -length_of_stay_ward,
         -age,
         -length_of_stay_icu,
         -implant)

# Verify removal
print(colnames(df))


# -------------------------------------------------
# Question f: MLR & feature selections

aliased_vars <- c("elective", "none_19", "none_31")
df <- df %>% select(-all_of(aliased_vars))

y <- df$total_cost_to_hospital

X <- df %>% select(-total_cost_to_hospital)
df <- cbind(y, X)

#full model
full_model <- lm(y ~ ., data = df)
summary(full_model)

# Reduced Model: Remove Non-Significant Variables
# Define variables that must to remove

# Identify significant variables (p-value < 0.1), excluding intercept
significant_vars <- names(which(summary(full_model)$coefficients[,4] < 0.1))
significant_vars <- setdiff(significant_vars, "(Intercept)")  # Remove intercept
significant_vars

# Ensure 'y' is included in the dataset
df_reduced <- df %>% select(y, all_of(significant_vars))
print(colnames(df_reduced))

# Fit reduced model
reduced_model <- lm(y ~ ., data = df_reduced)
summary(reduced_model)

# -------------------------------------------------
# Question g: Train-Test Split & Model Evaluation

# Set seed for reproducibility
set.seed(2005)

# Split into 70% training and 30% test set
trainIndex <- createDataPartition(df$y, p = 0.7, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# Train Full Model
lm_train_full <- lm(y ~ ., data = train_data)
cat("\nFull Model Summary (Training Data):\n")
print(summary(lm_train_full))

# Identify significant variables (p-value < 0.1), excluding intercept
significant_vars <- names(which(summary(lm_train_full)$coefficients[,4] < 0.1))
significant_vars <- setdiff(significant_vars, "(Intercept)")  # Remove intercept

significant_vars_filtered <- setdiff(significant_vars, aliased_vars)
significant_vars_filtered

train_reduced_data <- train_data %>% select(y, all_of(significant_vars_filtered))
print(colnames(train_reduced_data))

# Train Reduced Model
lm_train_reduced <- lm(y ~ ., data = train_reduced_data)
cat("\nReduced Model Summary (Training Data):\n")
print(summary(lm_train_reduced))

# Predict on Test Data
predictions_full <- predict(lm_train_full, newdata = test_data)
predictions_reduced <- predict(lm_train_reduced, newdata = test_data %>% select(all_of(significant_vars)))

# Compute RMSE for both models
rmse_full <- sqrt(mean((predictions_full - test_data$y)^2))
rmse_reduced <- sqrt(mean((predictions_reduced - test_data$y)^2))

# Performance Interpretation
cat("\nTraining Data Performance:\n")
cat("Full Model R-squared:", summary(lm_train_full)$r.squared, 
    "Adj R-squared:", summary(lm_train_full)$adj.r.squared, "\n")
cat("Reduced Model R-squared:", summary(lm_train_reduced)$r.squared,
    "Adj R-squared:", summary(lm_train_reduced)$adj.r.squared, "\n\n")

# Test Performance
cat("Test Data Performance:\n")
print(paste("Full Model RMSE: ", round(rmse_full, 2)))
print(paste("Reduced Model RMSE: ", round(rmse_reduced, 2)))

# Performance Evaluation
cat("\nPerformance Evaluation:\n")
if(abs(rmse_full - rmse_reduced) < 0.1*rmse_full) {
  cat("Both models show comparable performance. The reduced model maintains",
      "similar predictive accuracy with fewer variables, which is acceptable.")
} else if(rmse_reduced < rmse_full) {
  cat("The reduced model demonstrates BETTER performance with fewer variables,",
      "which is highly acceptable.")
} else {
  cat("The reduced model shows WORSE performance. The difference in RMSE (", 
      round(rmse_reduced - rmse_full, 2), ") might be acceptable depending",
      "on operational requirements for model simplicity.")
}

# Additional Model Evaluation Metrics

# Mean Absolute Error (MAE)
mae_full <- mean(abs(predictions_full - test_data$y))
mae_reduced <- mean(abs(predictions_reduced - test_data$y))
# Print additional performance metrics
cat("\nModel Performance Metrics:\n")
cat("Full Model: RMSE =", round(rmse_full, 2), "| MAE =", round(mae_full, 2))
cat("Reduced Model: RMSE =", round(rmse_reduced, 2), "| MAE =", round(mae_reduced, 2))

# Residual Analysis (Distribution of Errors)
# Compute residuals
residuals_full <- test_data$y - predictions_full
residuals_reduced <- test_data$y - predictions_reduced
# Plot histogram of residuals
par(mfrow = c(1, 2))  # Create side-by-side plots

hist(residuals_full, breaks = 30, col = "blue", main = "Residuals: Full Model", xlab = "Residuals")
hist(residuals_reduced, breaks = 30, col = "red", main = "Residuals: Reduced Model", xlab = "Residuals")

# Predicted vs. Actual Scatter Plot
par(mfrow = c(1, 2))  # Create side-by-side plots
# Full Model Plot
plot(test_data$y, predictions_full, col = "blue", pch = 16, main = "Full Model: Predicted vs Actual",
     xlab = "Actual Cost", ylab = "Predicted Cost")
abline(0, 1, col = "black", lwd = 2)  # Ideal fit line
# Reduced Model Plot
plot(test_data$y, predictions_reduced, col = "red", pch = 16, main = "Reduced Model: Predicted vs Actual",
     xlab = "Actual Cost", ylab = "Predicted Cost")
abline(0, 1, col = "black", lwd = 2)  # Ideal fit line
# Reset plot layout
par(mfrow = c(1, 1))

