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
df_scaled <- df_scaled %>%
  select(-cad_svd)
df_scaled <- df_scaled %>%
  select(-alert)



# -------------------------------------------------
# Question f: MLR & feature selections

## Full Model

y <- df$total_cost_to_hospital

X <- df %>% select(-total_cost_to_hospital, -sl)

dummy_vars <- X %>% select(where(~ n_distinct(.) == 2)) 
numeric_vars <- X %>% select(-all_of(names(dummy_vars)))  

numeric_vars_scaled <- scale(numeric_vars) %>% as.data.frame()

df_scaled <- cbind(y, numeric_vars_scaled, dummy_vars)

str(df_scaled)

#full model
full_model <- lm(y ~ ., data = df_scaled)
summary(full_model)


# Reduced Model: Remove Non-Significant Variables

# Define variables that must to remove
aliased_vars <- c("cad_svd", "none_19", "none_31", "alert", "elective")
df_reduced <- df_scaled %>% select(-all_of(aliased_vars))

# Identify significant variables (p-value < 0.1), excluding intercept
significant_vars <- names(which(summary(full_model)$coefficients[,4] < 0.1))
significant_vars <- setdiff(significant_vars, "(Intercept)")  # Remove intercept

# Ensure 'y' is included in the dataset
df_reduced <- df_reduced %>% select(y, all_of(significant_vars))
print(colnames(df_reduced))

# Fit reduced model
reduced_model <- lm(y ~ ., data = df_reduced)
summary(reduced_model)

# -------------------------------------------------
# Question g: Train-Test Split & Model Evaluation

# Set seed for reproducibility
set.seed(2005)

# Split into 70% training and 30% test set
trainIndex <- createDataPartition(df_scaled$y, p = 0.7, list = FALSE)
train_data <- df_scaled[trainIndex, ]
test_data <- df_scaled[-trainIndex, ]

# Train Full Model
lm_train_full <- lm(y ~ ., data = train_data)
cat("\nFull Model Summary (Training Data):\n")
print(summary(lm_train_full))

# Define variables that must to remove
aliased_vars <- c("cad_svd", "none_19", "none_31", "alert", "elective")

# Identify significant variables (p-value < 0.1), excluding intercept
significant_vars <- names(which(summary(lm_train_full)$coefficients[,4] < 0.1))
significant_vars <- setdiff(significant_vars, "(Intercept)")  # Remove intercept

significant_vars_filtered <- setdiff(significant_vars, aliased_vars)

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


















