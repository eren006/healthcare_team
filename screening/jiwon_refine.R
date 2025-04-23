getwd()
setwd("/Users/jiwonchoi/Desktop/github/healthcare_team/screening")
# -------------------------------
# 0. Load Necessary Libraries
# -------------------------------
library(readxl)
library(dplyr)
library(caret)
library(rpart)       # For classification trees (if needed)
library(glmnet)      # For logistic regression
library(pROC)        # For ROC analysis
library(corrplot)    # For correlation heatmap
library(naniar)      # For visualizing missingness
library(fastDummies) # For creating dummy variables
library(ggplot2)     # For visualization

# -------------------------------
# 1. Load the Dataset & Initial Split
# -------------------------------
data <- read_excel("screening_goodnames.xlsx")
head(data)

# Print missing values per column
print(colSums(is.na(data)))

# Split the data into training and testing sets
# Training: Observations with CKD value; Testing: Observations without CKD
training_data <- data %>% filter(!is.na(ckd))
testing_data  <- data %>% filter(is.na(ckd))

# Check dimensions
cat("Training data dimensions:", dim(training_data), "\n")
cat("Testing data dimensions:", dim(testing_data), "\n")

# Visualize missingness
vis_miss(data)

# Save the raw splits as CSV files
write.csv(training_data, "train.csv", row.names = FALSE)
write.csv(testing_data, "test.csv", row.names = FALSE)


# -------------------------------
# 2. Define Functions for Missing Value Imputation
# -------------------------------
# Function to calculate mode (for binary columns)
get_binary_mode <- function(v) {
  uniqv <- unique(na.omit(v))  # Ignore NA values
  if (length(uniqv) == 0) return(NA)  # In case of all NA
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to fill NA with mode for binary columns
fill_na_with_mode <- function(df, cols) {
  for (col in cols) {
    mode_val <- get_binary_mode(df[[col]])
    df[[col]][is.na(df[[col]])] <- mode_val
  }
  return(df)
}

# Function to fill NA with majority (mode) for categorical columns
fill_na_with_majority <- function(df, cols) {
  df <- df %>%
    mutate(across(all_of(cols), ~ {
      mode_val <- names(sort(table(.), decreasing = TRUE))[1]
      ifelse(is.na(.), mode_val, .)
    }))
  return(df)
}

# -------------------------------
# 3. Handle Missing Values on Training Data
# -------------------------------
# Define variable groups
binary_cols <- c("female", "educ", "unmarried", "income", "insured", "obese", "dyslipidemia", "pvd", 
                 "poor_vision", "smoker", "hypertension", "diabetes", "fam_hypertension", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd")
numerical_cols <- c("age", "weight", "height", "bmi", "waist", "sbp", "dbp", "hdl", "ldl", "total_chol")
categorical_cols <- c("racegrp", "care_source", "activity")

# Remove rows with >70% missing in numerical columns
missing_threshold <- 0.7 * length(numerical_cols)
removed_records <- training_data[rowSums(is.na(training_data[, numerical_cols])) > missing_threshold, ]
training_data <- training_data[rowSums(is.na(training_data[, numerical_cols])) <= missing_threshold, ]

# Impute missing values in training data:
training_data <- fill_na_with_mode(training_data, binary_cols)

for (col in numerical_cols) {
  training_data[[col]][is.na(training_data[[col]])] <- mean(training_data[[col]], na.rm = TRUE)
}

training_data <- fill_na_with_majority(training_data, categorical_cols)

# Verify missingness after processing
cat("Missing values after processing (training):\n")
print(colSums(is.na(training_data)))
cat("Number of removed records due to missingness threshold:", nrow(removed_records), "\n")


# -------------------------------
# 4. Create Dummy Variables & Convert to Factors
# -------------------------------
# Create dummy variables for categorical features (one-hot encoding, remove first dummy)
training_data <- dummy_cols(training_data, select_columns = "racegrp", remove_first_dummy = TRUE)
training_data <- dummy_cols(training_data, select_columns = "care_source", remove_first_dummy = TRUE)
training_data <- dummy_cols(training_data, select_columns = "activity", remove_first_dummy = TRUE)

# Remove original categorical columns and any non-model ID columns (e.g., id)
training_data <- training_data %>% select(-racegrp, -care_source, -activity, -id)

# Optionally, define which variables should be factors (binary variables and created dummies)
binary_vars <- c("female", "educ", "unmarried", "income", "insured", "obese", "dyslipidemia", "pvd", 
                 "poor_vision", "smoker", "hypertension", "diabetes", "fam_hypertension", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd", 
                 "racegrp_hispa", "racegrp_other", "racegrp_white",
                 "care_source_DrHMO", "care_source_noplace", "care_source_other",
                 "activity_2", "activity_3", "activity_4")
training_data[binary_vars] <- lapply(training_data[binary_vars], as.factor)


# -------------------------------
# 5. Outlier Treatment on Continuous Variables
# -------------------------------
continuous_vars <- c("hdl", "ldl", "sbp", "dbp", "weight", "bmi", "height", "waist", "age", "total_chol")

# Function to cap outliers using 1.5*IQR rule
handle_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  pmax(pmin(x, upper_bound), lower_bound)
}

# Apply outlier capping
training_data <- training_data %>% 
  mutate(across(all_of(continuous_vars), handle_outliers))

cat("Summary after outlier treatment:\n")
print(summary(training_data %>% select(all_of(continuous_vars))))


# -------------------------------
# 6. Normalization (Min-Max Scaling)
# -------------------------------
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
training_data <- training_data %>% 
  mutate(across(all_of(continuous_vars), normalize))

cat("Summary of normalized variables (0-1 range):\n")
print(summary(training_data %>% select(all_of(continuous_vars))))


# -------------------------------
# 7. Check Correlations and Drop High-Correlation Variables
# -------------------------------
# Compute correlation matrix on numerical (continuous) variables
corr_matrix <- cor(training_data %>% select(where(is.numeric)), use = "complete.obs")
corrplot(corr_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)

# Find highly correlated pairs (|r| >= 0.7)
corr_long <- as.data.frame(as.table(corr_matrix)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>%
  filter(Variable1 != Variable2) %>%
  mutate(AbsCorrelation = abs(Correlation)) %>%
  arrange(desc(AbsCorrelation))
highly_correlated <- corr_long %>% filter(AbsCorrelation >= 0.7)
cat("Highly correlated variable pairs (|r| >= 0.7):\n")
print(highly_correlated)

# Based on domain knowledge, drop the following:
# 1. Drop 'total_chol' because LDL is a better indicator (high correlation ~0.93)
# 2. Drop 'waist' because it is highly correlated with Weight and BMI
# 3. Drop 'weight' (when BMI is available)
# 4. Drop 'obese' (redundant given continuous BMI measure)
training_data <- training_data %>% 
  select(-total_chol, -waist, -weight, -obese)

# 5. Consolidate 'fam_cvd' and 'fam_hypertension' into one binary variable 'fam_cardio'
training_data <- training_data %>%
  mutate(
    fam_cvd = as.integer(as.character(fam_cvd)),
    fam_hypertension = as.integer(as.character(fam_hypertension)),
    fam_cardio = factor(if_else(fam_cvd == 1 | fam_hypertension == 1, 1, 0))
  ) %>%
  select(-fam_cvd, -fam_hypertension)

cat("Variables after dropping high-correlated ones:\n")
print(names(training_data))


# -------------------------------
# 8. Check Class Balance & Visualize
# -------------------------------
# Ensure CKD is a factor
training_data$ckd <- as.factor(training_data$ckd)

class_dist <- table(training_data$ckd)
prop_dist <- prop.table(class_dist)
cat("\nClass Distribution:\n")
print(class_dist)
cat("\nClass Proportions:\n")
print(round(prop_dist, 3))

# Visualize class distribution
class_plot <- ggplot(training_data, aes(x = ckd, fill = ckd)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(title = "Class Distribution of CKD",
       subtitle = paste("Imbalance ratio:", round(max(prop_dist)/min(prop_dist), 1)),
       x = "CKD Diagnosis",
       y = "Count") +
  theme_minimal()
print(class_plot)


# -------------------------------
# 9. Create Train-Validation Split
# -------------------------------
set.seed(42)
trainIndex <- createDataPartition(training_data$ckd, p = 0.8, list = FALSE)
validation_data <- training_data[-trainIndex, ]
training_data <- training_data[trainIndex, ]

# Save cleaned train/validation sets
write.csv(training_data, "clean_train.csv", row.names = FALSE)
write.csv(validation_data, "clean_val.csv", row.names = FALSE)


# -------------------------------
# 10. (Optional) Apply the Same Pre-Processing Pipeline to Test Data
# -------------------------------
# Since the test set lacks CKD values, apply similar cleaning (imputation, dummy creation, outlier capping, normalization, etc.)
# Note: You may need to use the parameters (e.g., means, min/max) computed from training_data for consistency.
testing_data <- fill_na_with_mode(testing_data, binary_cols)
for (col in numerical_cols) {
  testing_data[[col]][is.na(testing_data[[col]])] <- mean(testing_data[[col]], na.rm = TRUE)
}
testing_data <- fill_na_with_majority(testing_data, categorical_cols)

# Create dummies (ensure same dummy levels as training)
testing_data <- dummy_cols(testing_data, select_columns = "racegrp", remove_first_dummy = TRUE)
testing_data <- dummy_cols(testing_data, select_columns = "care_source", remove_first_dummy = TRUE)
testing_data <- dummy_cols(testing_data, select_columns = "activity", remove_first_dummy = TRUE)
testing_data <- testing_data %>% select(-racegrp, -care_source, -activity, -id)

# Convert binary variables to factors
testing_data[binary_vars] <- lapply(testing_data[binary_vars], as.factor)

# Apply outlier capping and normalization using the same functions
testing_data <- testing_data %>% 
  mutate(across(all_of(continuous_vars), handle_outliers)) %>%
  mutate(across(all_of(continuous_vars), normalize))

# Drop variables (total_chol, waist, weight, obese, fam_cvd, fam_hypertension) as in training
testing_data <- testing_data %>% 
  select(-total_chol, -waist, -weight, -obese) %>%
  mutate(
    fam_cvd = as.integer(as.character(fam_cvd)),
    fam_hypertension = as.integer(as.character(fam_hypertension)),
    fam_cardio = factor(if_else(fam_cvd == 1 | fam_hypertension == 1, 1, 0))
  ) %>%
  select(-fam_cvd, -fam_hypertension)

# Save cleaned test set
write.csv(testing_data, "clean_test.csv", row.names = FALSE)

cat("\nPre-processing complete. Cleaned train, validation, and test datasets are saved.\n")

# -------------------------------
# Modeling Part: LASSO Logistic Regression
# -------------------------------
library(glmnet)
library(pROC)
library(caret)

# Load the cleaned training and validation datasets
train_data <- read.csv("clean_train.csv", stringsAsFactors = TRUE)
val_data   <- read.csv("clean_val.csv", stringsAsFactors = TRUE)

# Check class distribution in training data
cat("Training set CKD distribution:\n")
print(prop.table(table(train_data$ckd)))

# Prepare model matrices:
# Create a model matrix without an intercept (using -1) for predictors
x_train <- model.matrix(ckd ~ . - 1, data = train_data)
y_train <- as.numeric(as.character(train_data$ckd))  # assuming factor levels "0" and "1"

x_val   <- model.matrix(ckd ~ . - 1, data = val_data)
y_val   <- as.numeric(as.character(val_data$ckd))

# -------------------------------
# 1. Train LASSO Model using Cross-Validation
# -------------------------------
set.seed(42)
cv_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
best_lambda <- cv_model$lambda.min

# Fit the final LASSO model with the best lambda
final_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)

# -------------------------------
# 2. Model Evaluation on Validation Data
# -------------------------------
# Predict probabilities on the validation set
val_probs <- predict(final_model, newx = x_val, type = "response")
val_probs <- as.vector(val_probs)

# Set a classification threshold (adjustable based on your sensitivity/specificity goals)
threshold <- 0.3
val_pred <- ifelse(val_probs > threshold, 1, 0)

# Create confusion matrix
conf_mat <- table(Predicted = val_pred, Actual = y_val)
cat("Confusion Matrix:\n")
print(conf_mat)

# Calculate Sensitivity and Specificity
TP <- sum(val_pred == 1 & y_val == 1)
TN <- sum(val_pred == 0 & y_val == 0)
FP <- sum(val_pred == 1 & y_val == 0)
FN <- sum(val_pred == 0 & y_val == 1)

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

cat("\nModel Performance (Threshold =", threshold, "):\n")
cat("Sensitivity:", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")

# Compute ROC curve and AUC
roc_obj <- roc(y_val, val_probs)
auc_val <- auc(roc_obj)
cat("AUC:", round(auc_val, 3), "\n")
plot(roc_obj, col = "red", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# -------------------------------
# 3. (Optional) Threshold Tuning Example
# -------------------------------
# Explore multiple thresholds and evaluate performance metrics.
thresholds <- seq(0.1, 0.5, by = 0.05)
tuning_results <- data.frame(threshold = thresholds, sensitivity = NA, specificity = NA)

for (i in 1:length(thresholds)) {
  temp_pred <- ifelse(val_probs > thresholds[i], 1, 0)
  TP_temp <- sum(temp_pred == 1 & y_val == 1)
  TN_temp <- sum(temp_pred == 0 & y_val == 0)
  FP_temp <- sum(temp_pred == 1 & y_val == 0)
  FN_temp <- sum(temp_pred == 0 & y_val == 1)
  
  tuning_results$sensitivity[i] <- if ((TP_temp + FN_temp) > 0) TP_temp / (TP_temp + FN_temp) else NA
  tuning_results$specificity[i] <- if ((TN_temp + FP_temp) > 0) TN_temp / (TN_temp + FP_temp) else NA
}

cat("\nThreshold Tuning Results:\n")
print(tuning_results)

# Check class distribution in training set
train_class_dist <- table(train_data$ckd)
train_prop <- prop.table(train_class_dist)
cat("Training Set Class Proportions:\n")
print(round(train_prop, 3))

# Check class distribution in validation set
val_class_dist <- table(val_data$ckd)
val_prop <- prop.table(val_class_dist)
cat("Validation Set Class Proportions:\n")
print(round(val_prop, 3))

# -------------------------------
# Save Test Data Predictions
# -------------------------------
# --- Debug: Check the cleaned test set ---
cat("Number of rows in test_clean:", nrow(test_clean), "\n")
print(head(test_clean))
print("Column names in test_clean:")
print(colnames(test_clean))

# --- Build the test model matrix ---
x_test <- model.matrix(~ . - 1, data = test_clean)
cat("Dimensions of raw x_test:", dim(x_test), "\n")

# Align x_test with the training predictors (x_train)
train_cols <- colnames(x_train)
cat("Number of predictors in training model matrix:", length(train_cols), "\n")
test_cols  <- colnames(x_test)

# Identify missing columns in x_test and add them as zero columns
missing_cols <- setdiff(train_cols, test_cols)
if(length(missing_cols) > 0){
  for(col in missing_cols){
    x_test <- cbind(x_test, rep(0, nrow(x_test)))
    colnames(x_test)[ncol(x_test)] <- col
  }
}

# Remove extra columns in x_test that are not in x_train
extra_cols <- setdiff(colnames(x_test), train_cols)
if(length(extra_cols) > 0){
  x_test <- x_test[, !(colnames(x_test) %in% extra_cols)]
}

# Order the columns to match training
x_test <- x_test[, train_cols, drop = FALSE]
cat("Dimensions of aligned x_test:", dim(x_test), "\n")

# --- Predict using the final logistic regression model ---
if(nrow(x_test) > 0){
  test_probs <- predict(final_model, newx = x_test, type = "response")
  test_probs <- as.vector(test_probs)
  cat("Length of test_probs:", length(test_probs), "\n")
  
  # Apply threshold
  threshold <- 0.3
  test_pred <- ifelse(test_probs > threshold, 1, 0)
  cat("Length of test_pred:", length(test_pred), "\n")
  
  # Combine with test IDs
  submission <- data.frame(ID = test_clean$id, CKD_Prediction = test_pred)
  write.csv(submission, "CKD_test_predictions_logistic.csv", row.names = FALSE)
  cat("Test predictions saved to CKD_test_predictions_logistic.csv\n")
} else {
  cat("Error: x_test has 0 rows. Please check the cleaned test data.\n")
}

# Load the original test set (with IDs) and the cleaned test set
test_original <- read.csv("test.csv", stringsAsFactors = TRUE)
test_clean <- read.csv("clean_test.csv", stringsAsFactors = TRUE)

# Since the cleaning process dropped the 'id' column, reattach it using the original order
test_clean$id <- test_original$id

# Build the test model matrix using the same formula as training (without intercept)
x_test <- model.matrix(~ . - 1, data = test_clean)

# Check dimensions of training matrix
train_cols <- colnames(x_train)
cat("Number of predictors in training model matrix: ", length(train_cols), "\n")

# Align test matrix with training columns
missing_cols <- setdiff(train_cols, colnames(x_test))
if(length(missing_cols) > 0){
  for(col in missing_cols){
    x_test <- cbind(x_test, rep(0, nrow(x_test)))
    colnames(x_test)[ncol(x_test)] <- col
  }
}

# Remove any extra columns in x_test not present in x_train
extra_cols <- setdiff(colnames(x_test), train_cols)
if(length(extra_cols) > 0){
  x_test <- x_test[, !(colnames(x_test) %in% extra_cols)]
}

# Ensure the order of columns matches that of x_train
x_test <- x_test[, train_cols, drop = FALSE]
cat("Dimensions of aligned x_test:", dim(x_test), "\n")

# Predict probabilities using the final logistic regression model
test_probs <- predict(final_model, newx = x_test, type = "response")
test_probs <- as.vector(test_probs)
cat("Length of test_probs: ", length(test_probs), "\n")

# Apply the chosen threshold to obtain binary predictions
threshold <- 0.3
test_pred <- ifelse(test_probs > threshold, 1, 0)
cat("Length of test_pred: ", length(test_pred), "\n")

# Combine the test IDs and predictions into a data frame.
submission <- data.frame(ID = test_clean$id, CKD_Prediction = test_pred)

# Save the prediction result to a CSV file.
write.csv(submission, "CKD_test_predictions_logistic.csv", row.names = FALSE)
cat("Test predictions saved to CKD_test_predictions_logistic.csv\n")



# -------------------------------
# Load the Dataset & Initial Split
# -------------------------------
data <- read_excel("screening_goodnames.xlsx")

# Split the data into training (with CKD) and test (without CKD)
training_data <- data %>% filter(!is.na(ckd))
testing_data  <- data %>% filter(is.na(ckd))

# Save original test IDs for final submission
original_test_ids <- testing_data$id

# Remove the 'ckd' column from the test set since it is missing for all observations
testing_data <- testing_data %>% select(-ckd)

# -------------------------------
# Define Functions for Preprocessing
# -------------------------------
get_binary_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  if(length(uniqv) == 0) return(NA)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fill_na_with_mode <- function(df, cols) {
  for(col in cols) {
    mode_val <- get_binary_mode(df[[col]])
    df[[col]][is.na(df[[col]])] <- mode_val
  }
  return(df)
}

fill_na_with_majority <- function(df, cols) {
  df <- df %>% mutate(across(all_of(cols), ~ {
    mode_val <- names(sort(table(.), decreasing = TRUE))[1]
    ifelse(is.na(.), mode_val, .)
  }))
  return(df)
}

handle_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  pmax(pmin(x, upper_bound), lower_bound)
}

normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# -------------------------------
# Preprocessing Steps (for both training and test)
# -------------------------------
# Define variable groups
binary_cols <- c("female", "educ", "unmarried", "income", "insured", "obese",
                 "dyslipidemia", "pvd", "poor_vision", "smoker", "hypertension",
                 "diabetes", "fam_hypertension", "fam_diabetes", "stroke", "cvd",
                 "fam_cvd", "chf", "anemia", "ckd")
numerical_cols <- c("age", "weight", "height", "bmi", "waist", "sbp", "dbp",
                    "hdl", "ldl", "total_chol")
categorical_cols <- c("racegrp", "care_source", "activity")

# --- Process Training Data (which has CKD) ---
missing_threshold <- 0.7 * length(numerical_cols)
training_data <- training_data[rowSums(is.na(training_data[, numerical_cols])) <= missing_threshold, ]

training_data <- fill_na_with_mode(training_data, binary_cols)
for(col in numerical_cols){
  training_data[[col]][is.na(training_data[[col]])] <- mean(training_data[[col]], na.rm = TRUE)
}
training_data <- fill_na_with_majority(training_data, categorical_cols)

# Create dummy variables for categorical features
training_data <- dummy_cols(training_data, select_columns = "racegrp", remove_first_dummy = TRUE)
training_data <- dummy_cols(training_data, select_columns = "care_source", remove_first_dummy = TRUE)
training_data <- dummy_cols(training_data, select_columns = "activity", remove_first_dummy = TRUE)
training_data <- training_data %>% select(-racegrp, -care_source, -activity, -id)

# Convert selected binary variables to factors
binary_vars <- c("female", "educ", "unmarried", "income", "insured", "obese",
                 "dyslipidemia", "pvd", "poor_vision", "smoker", "hypertension",
                 "diabetes", "fam_hypertension", "fam_diabetes", "stroke", "cvd",
                 "fam_cvd", "chf", "anemia", "ckd",
                 "racegrp_hispa", "racegrp_other", "racegrp_white",
                 "care_source_DrHMO", "care_source_noplace", "care_source_other",
                 "activity_2", "activity_3", "activity_4")
training_data[binary_vars] <- lapply(training_data[binary_vars], as.factor)

# Outlier treatment and normalization on continuous variables
continuous_vars <- c("hdl", "ldl", "sbp", "dbp", "weight", "bmi", "height", "waist", "age", "total_chol")
training_data <- training_data %>% mutate(across(all_of(continuous_vars), handle_outliers))
training_data <- training_data %>% mutate(across(all_of(continuous_vars), normalize))

# Drop highly correlated variables per domain knowledge
training_data <- training_data %>% select(-total_chol, -waist, -weight, -obese)
training_data <- training_data %>% 
  mutate(
    fam_cvd = as.integer(as.character(fam_cvd)),
    fam_hypertension = as.integer(as.character(fam_hypertension)),
    fam_cardio = factor(if_else(fam_cvd == 1 | fam_hypertension == 1, 1, 0))
  ) %>% select(-fam_cvd, -fam_hypertension)

# Ensure CKD is a factor
training_data$ckd <- as.factor(training_data$ckd)

# --- Process Test Data (which does not have CKD) ---
# Since test data originally has a 'ckd' column with all NAs, we already removed it earlier.
# For the test set, use the same functions but exclude "ckd" from binary imputation.
binary_cols_test <- setdiff(binary_cols, "ckd")

testing_data <- fill_na_with_mode(testing_data, binary_cols_test)
for(col in numerical_cols){
  testing_data[[col]][is.na(testing_data[[col]])] <- mean(testing_data[[col]], na.rm = TRUE)
}
testing_data <- fill_na_with_majority(testing_data, categorical_cols)

# Create dummy variables for test data
testing_data <- dummy_cols(testing_data, select_columns = "racegrp", remove_first_dummy = TRUE)
testing_data <- dummy_cols(testing_data, select_columns = "care_source", remove_first_dummy = TRUE)
testing_data <- dummy_cols(testing_data, select_columns = "activity", remove_first_dummy = TRUE)
testing_data <- testing_data %>% select(-racegrp, -care_source, -activity, -id)

# Convert binary variables in test data to factors
testing_data[binary_cols_test] <- lapply(testing_data[binary_cols_test], as.factor)

# Apply outlier treatment and normalization on test data
testing_data <- testing_data %>% 
  mutate(across(all_of(continuous_vars), handle_outliers)) %>%
  mutate(across(all_of(continuous_vars), normalize))

# Drop the same variables as in training
testing_data <- testing_data %>% select(-total_chol, -waist, -weight, -obese) %>%
  mutate(
    fam_cvd = as.integer(as.character(fam_cvd)),
    fam_hypertension = as.integer(as.character(fam_hypertension)),
    fam_cardio = factor(if_else(fam_cvd == 1 | fam_hypertension == 1, 1, 0))
  ) %>% select(-fam_cvd, -fam_hypertension)

# -------------------------------
# Train-Validation Split on Training Data
# -------------------------------
set.seed(42)
trainIndex <- createDataPartition(training_data$ckd, p = 0.8, list = FALSE)
train_data <- training_data[trainIndex, ]
val_data <- training_data[-trainIndex, ]

# -------------------------------
# Modeling: Train LASSO Logistic Regression
# -------------------------------
x_train <- model.matrix(ckd ~ . - 1, data = train_data)
y_train <- as.numeric(as.character(train_data$ckd))
x_val <- model.matrix(ckd ~ . - 1, data = val_data)
y_val <- as.numeric(as.character(val_data$ckd))

set.seed(42)
cv_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
best_lambda <- cv_model$lambda.min
final_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)

# Evaluate Model on Validation Set
val_probs <- predict(final_model, newx = x_val, type = "response")
val_probs <- as.vector(val_probs)
threshold <- 0.3
val_pred <- ifelse(val_probs > threshold, 1, 0)
conf_mat <- table(Predicted = val_pred, Actual = y_val)
cat("Confusion Matrix on Validation Set:\n")
print(conf_mat)
TP <- sum(val_pred == 1 & y_val == 1)
TN <- sum(val_pred == 0 & y_val == 0)
FP <- sum(val_pred == 1 & y_val == 0)
FN <- sum(val_pred == 0 & y_val == 1)
cat("Sensitivity:", round(TP / (TP + FN), 3), "\n")
cat("Specificity:", round(TN / (TN + FP), 3), "\n")
roc_obj <- roc(y_val, val_probs)
auc_val <- auc(roc_obj)
cat("AUC:", round(auc_val, 3), "\n")
plot(roc_obj, col = "red", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# -------------------------------
# Create Model Matrix for Test Data and Predict
# -------------------------------
# Build test model matrix using the same predictors as in training
x_test <- model.matrix(~ . - 1, data = testing_data)

# Align x_test with the training predictors
train_cols <- colnames(x_train)
missing_cols <- setdiff(train_cols, colnames(x_test))
if(length(missing_cols) > 0){
  for(col in missing_cols){
    x_test <- cbind(x_test, rep(0, nrow(x_test)))
    colnames(x_test)[ncol(x_test)] <- col
  }
}
extra_cols <- setdiff(colnames(x_test), train_cols)
if(length(extra_cols) > 0){
  x_test <- x_test[, !(colnames(x_test) %in% extra_cols)]
}
x_test <- x_test[, train_cols, drop = FALSE]

# Predict probabilities on the test set
test_probs <- predict(final_model, newx = x_test, type = "response")
test_probs <- as.vector(test_probs)
test_pred <- ifelse(test_probs > threshold, 1, 0)

# -------------------------------
# Save Test Data Predictions
# -------------------------------
submission <- data.frame(ID = original_test_ids, CKD_Prediction = test_pred)
write.csv(submission, "CKD_test_predictions_logistic.csv", row.names = FALSE)
cat("Test predictions saved to CKD_test_predictions_logistic.csv\n")



