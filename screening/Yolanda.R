rm(list=ls())

# Load necessary libraries
library(stats)
library(caret)

# Read training and validation datasets
clean_train <- read.csv("screening/clean_train.csv")
clean_val <- read.csv("screening/clean_val.csv")

# Remove "activity" variable if it exists in both datasets
clean_train <- clean_train[, !colnames(clean_train) %in% "activity"]
clean_val <- clean_val[, !colnames(clean_val) %in% "activity"]

# Define predictors (exclude the target variable 'ckd')
predictors <- setdiff(colnames(clean_train), "ckd")

# Fit the LPM model using OLS (Ordinary Least Squares)
lpm_model <- lm(ckd ~ ., data = clean_train)

# Display model summary
summary(lpm_model)

# Predict probabilities on training data
clean_train$pred_prob <- predict(lpm_model, clean_train)

# Ensure probabilities are within [0,1] range
clean_train$pred_prob <- pmax(pmin(clean_train$pred_prob, 1), 0)

# Initialize best threshold search
best_threshold <- 0.5
best_score <- -Inf

# Iterate over multiple thresholds (0.1 to 0.9)
for (threshold in seq(0.1, 0.9, by = 0.01)) {
  clean_train$pred_ckd <- ifelse(clean_train$pred_prob > threshold, 1, 0)
  
  correct_CKD <- sum(clean_train$pred_ckd == 1 & clean_train$ckd == 1)
  false_positives <- sum(clean_train$pred_ckd == 1 & clean_train$ckd == 0)
  
  # Compute scoring metric
  score <- (1300 * correct_CKD) - (100 * false_positives)
  
  # Update best threshold
  if (score > best_score) {
    best_score <- score
    best_threshold <- threshold
  }
}

# Print the best threshold and score
print(paste("Best Threshold:", best_threshold))
print(paste("Best Score (Train):", best_score))

# Predict probabilities on validation data
clean_val$pred_prob <- predict(lpm_model, clean_val)

# Ensure probabilities are within [0,1] range
clean_val$pred_prob <- pmax(pmin(clean_val$pred_prob, 1), 0)

# Apply the best threshold found on training data
clean_val$final_pred <- ifelse(clean_val$pred_prob > best_threshold, 1, 0)

# Compute confusion matrix
conf_matrix <- confusionMatrix(factor(clean_val$final_pred), factor(clean_val$ckd), positive = "1")

# Compute the validation score
correct_CKD_val <- sum(clean_val$final_pred == 1 & clean_val$ckd == 1)
false_positives_val <- sum(clean_val$final_pred == 1 & clean_val$ckd == 0)

val_score <- (1300 * correct_CKD_val) - (100 * false_positives_val)

# Print performance metrics
print(conf_matrix)
print(paste("Validation Score:", val_score))

# Predict probabilities using the trained LPM model
clean_val$pred_prob <- predict(lpm_model, clean_val)

# Ensure probabilities are within [0,1] range
clean_val$pred_prob <- pmax(pmin(clean_val$pred_prob, 1), 0)

# Classify CKD using the default threshold (0.5)
clean_val$default_pred <- ifelse(clean_val$pred_prob > 0.5, 1, 0)

# Compute confusion matrix BEFORE threshold adjustment
conf_matrix_default <- confusionMatrix(factor(clean_val$default_pred), factor(clean_val$ckd), positive = "1")

# Print confusion matrix
print(conf_matrix_default)



