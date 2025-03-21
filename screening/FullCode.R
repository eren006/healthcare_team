# Load necessary libraries
library(readxl)
library(dplyr)
library(caret)
library(rpart) # For classification trees
library(glmnet) # For logistic regression
library(pROC) # For ROC analysis
library(corrplot) # For correlation heatmap
library(naniar)
library(fastDummies)

# -------------------------------
# Load the dataset
# -------------------------------
data <- read_excel("screening/screening_goodnames.xlsx")
head(data)

# ---------------------------------------------------
# Split the dataset into training, validation and testing sets
# ---------------------------------------------------
# Training set: First 6,000 observations with CKD values
# Testing set: Remaining 2,819 observations without CKD values
print(colSums(is.na(data)))
training_data <- data %>% filter(!is.na(ckd))
testing_data  <- data %>% filter(is.na(ckd))

# Validation Split from Train Data (80% Train, 20% Validation)
set.seed(42)
trainIndex <- createDataPartition(training_data$ckd, p = 0.8, list = FALSE)
validation_data <- training_data[-trainIndex, ]
training_data <- training_data[trainIndex, ]

# Verify the dimensions (should be around 6000 for training and 2819 for test)
dim(training_data)
dim(validation_data)
dim(testing_data)

library(janitor)
training_data <- training_data %>% clean_names()
validation_data <- validation_data %>% clean_names()
testing_data <- testing_data %>% clean_names()

# Save the datasets as CSV files
getwd()
write.csv(training_data, "screening/train.csv", row.names = FALSE)
write.csv(testing_data, "screening/test.csv", row.names = FALSE)
write.csv(validation_data, "screening/validation.csv", row.names = FALSE)


# -----------------------------------------------
# Data Preprocessing starts here
# -----------------------------------------------

# -------------------------------
# Function to Calculate Mode (for binary columns)
# -------------------------------
get_binary_mode <- function(v) {
  uniqv <- unique(na.omit(v))  # Ignore NA values
  if (length(uniqv) == 0) return(NA)  # Handle all-NA case
  uniqv[which.max(tabulate(match(v, uniqv)))]  # Return the most frequent value
}

# -------------------------------
# Function to fill NA with Majority Category (for categorical columns)
# -------------------------------
fill_na_with_majority <- function(df, cols) {
  df <- df %>%
    mutate(across(all_of(cols), ~ {
      mode_val <- names(sort(table(.), decreasing = TRUE))[1]
      ifelse(is.na(.), mode_val, .)
    }))
  return(df)
}

# -------------------------------
# Function to fill NA with the Mode (for binary columns)
# -------------------------------
fill_na_with_mode <- function(df, cols) {
  for (col in cols) {
    mode_val <- get_binary_mode(df[[col]])
    df[[col]][is.na(df[[col]])] <- mode_val
  }
  return(df)
}

# -------------------------------
# Handle Missing Values - Training Data
# -------------------------------

# Original code for training data (unchanged)
binary_cols <- c("female", "educ", "unmarried", "income", "insured", "obese", "dyslipidemia", "pvd", 
                 "poor_vision", "smoker", "hypertension", "diabetes", "fam_hypertension", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd")
numerical_cols <- c("age", "weight", "height", "bmi", "waist", "sbp", "dbp", "hdl", "ldl", "total_chol")
categorical_cols <- c("racegrp", "care_source", "activity")

missing_threshold <- 0.7 * length(numerical_cols)
removed_records_train <- training_data[rowSums(is.na(training_data[, numerical_cols])) > missing_threshold, ]
training_data <- training_data[rowSums(is.na(training_data[, numerical_cols])) <= missing_threshold, ]

training_data <- fill_na_with_mode(training_data, binary_cols)
for (col in numerical_cols) {
  training_data[[col]][is.na(training_data[[col]])] <- mean(training_data[[col]], na.rm = TRUE)
}
training_data <- fill_na_with_majority(training_data, categorical_cols)

# Store parameters from training data
train_num_means <- sapply(training_data[numerical_cols], mean, na.rm = TRUE)
train_bin_modes <- sapply(training_data[binary_cols], get_binary_mode)
train_cat_modes <- sapply(training_data[categorical_cols], function(x) names(sort(table(x), decreasing = TRUE)[1]))
                          
# -------------------------------
# Handle Missing Values - Validation Data
# -------------------------------

# Use parameters from training data
handle_missing_values_validation <- function(df) {
  # Remove rows with >70% missing in numerical columns
  df <- df[rowSums(is.na(df[, numerical_cols])) <= missing_threshold, ]
  
  # Fill binary columns with training modes
  for (col in binary_cols) {
    df[[col]][is.na(df[[col]])] <- train_bin_modes[[col]]
  }
  
  # Fill numerical columns with training means
  for (col in numerical_cols) {
    df[[col]][is.na(df[[col]])] <- train_num_means[[col]]
  }
  
  # Fill categorical columns with training modes
  for (col in categorical_cols) {
    mode_val <- train_cat_modes[[col]]
    df[[col]][is.na(df[[col]])] <- mode_val
  }
  
  return(df)
}

# Apply to validation data
removed_records_val <- validation_data[rowSums(is.na(validation_data[, numerical_cols])) > missing_threshold, ]
validation_data <- handle_missing_values_validation(validation_data)

# -------------------------------
# Handle Missing Values - Test Data
# -------------------------------

# Use parameters from training data
handle_missing_values_test <- function(df) {
  # Remove rows with >70% missing in numerical columns
  df <- df[rowSums(is.na(df[, numerical_cols])) <= missing_threshold, ]
  
  # Fill binary columns with training modes (excluding ckd if needed)
  bin_cols_test <- if("ckd" %in% names(df)) binary_cols else binary_cols[binary_cols != "ckd"]
  
  for (col in bin_cols_test) {
    df[[col]][is.na(df[[col]])] <- train_bin_modes[[col]]
  }
  
  # Fill numerical columns with training means
  for (col in numerical_cols) {
    df[[col]][is.na(df[[col]])] <- train_num_means[[col]]
  }
  
  # Fill categorical columns with training modes
  for (col in categorical_cols) {
    mode_val <- train_cat_modes[[col]]
    df[[col]][is.na(df[[col]])] <- mode_val
  }
  
  return(df)
}

# Apply to test data
removed_records_test <- testing_data[rowSums(is.na(testing_data[, numerical_cols])) > missing_threshold, ]
testing_data <- handle_missing_values_test(testing_data)

# -------------------------------
# Verification
# -------------------------------
cat("\nTraining Data Missing Values:\n")
print(colSums(is.na(training_data)))

cat("\nValidation Data Missing Values:\n")
print(colSums(is.na(validation_data)))

cat("\nTest Data Missing Values:\n")
print(colSums(is.na(testing_data)))

cat("\nRecords Removed:")
cat("\nTraining:", nrow(removed_records_train))
cat("\nValidation:", nrow(removed_records_val))
cat("\nTest:", nrow(removed_records_test))

# ---------------------------------------------
# Change to dummy variables & change to factor
# ---------------------------------------------
binary_vars <- c("female", "educ", "unmarried", "income","insured", "obese", "dyslipidemia", "pvd", "poor_vision", 
                 "smoker", "hypertension", "diabetes",  "fam_hypertension", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd", "racegrp_hispa", "racegrp_other", "racegrp_white",
                 "care_source_DrHMO", "care_source_noplace", "care_source_other", "activity_2","activity_3","activity_4")

training_data <- dummy_cols(training_data, select_columns = "racegrp", remove_first_dummy = TRUE)
training_data <- dummy_cols(training_data, select_columns = "care_source", remove_first_dummy = TRUE)
training_data <- dummy_cols(training_data, select_columns = "activity", remove_first_dummy = TRUE)
training_data <- training_data %>% select(-racegrp)
training_data <- training_data %>% select(-care_source)
training_data <- training_data %>% select(-id)
training_data <- training_data %>% select(-activity)
dim(training_data)
training_data[binary_vars] <- lapply(training_data[binary_vars], as.factor)

validation_data <- dummy_cols(validation_data, select_columns = "racegrp", remove_first_dummy = TRUE)
validation_data <- dummy_cols(validation_data, select_columns = "care_source", remove_first_dummy = TRUE)
validation_data <- dummy_cols(validation_data, select_columns = "activity", remove_first_dummy = TRUE)
validation_data <- validation_data %>% select(-racegrp)
validation_data <- validation_data %>% select(-care_source)
validation_data <- validation_data %>% select(-id)
validation_data <- validation_data %>% select(-activity)
dim(validation_data)
validation_data[binary_vars] <- lapply(validation_data[binary_vars], as.factor)

testing_data <- dummy_cols(testing_data, select_columns = "racegrp", remove_first_dummy = TRUE)
testing_data <- dummy_cols(testing_data, select_columns = "care_source", remove_first_dummy = TRUE)
testing_data <- dummy_cols(testing_data, select_columns = "activity", remove_first_dummy = TRUE)
testing_data <- testing_data %>% select(-racegrp)
testing_data <- testing_data %>% select(-care_source)
testing_data <- testing_data %>% select(-id)
testing_data <- testing_data %>% select(-activity)
dim(testing_data)
testing_data[binary_vars] <- lapply(testing_data[binary_vars], as.factor)

# -----------------------------------------------------------------------------------
# Handle Outliers (Continuous Variables) - THIS ONLY ON TRAINING SET AS EXPECTED
# -----------------------------------------------------------------------------------
continuous_vars <- c("hdl", "ldl", "sbp", "dbp", "weight", "bmi", "height", "waist", "age", "total_chol")

# Function to cap outliers using 1.5*IQR rule
handle_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x <- pmax(pmin(x, upper_bound), lower_bound)
  return(x)
}

# Apply outlier capping to selected variables
training_data <- training_data %>% 
  mutate(across(all_of(continuous_vars), handle_outliers))

# Verify outlier handling
print("Summary after outlier treatment:")
print(summary(training_data %>% select(all_of(continuous_vars))))

# -------------------------------
# Normalization (Min-Max Scaling)
# -------------------------------

# Define continuous variables (adjust based on your remaining columns)
continuous_vars <- c("hdl", "ldl", "sbp", "dbp", "weight", "bmi", "height", "waist", "age", "total_chol")

# 1. Store parameters from training data
train_mins <- sapply(training_data[continuous_vars], min, na.rm = TRUE)
train_maxs <- sapply(training_data[continuous_vars], max, na.rm = TRUE)

# 2. Create normalization function using training parameters
normalize_dataset <- function(df) {
  df %>%
    mutate(across(
      all_of(continuous_vars),
      ~ (.x - train_mins[cur_column()]) / (train_maxs[cur_column()] - train_mins[cur_column()])
    ))
}

# 3. Apply to all datasets
training_data <- normalize_dataset(training_data)
validation_data <- normalize_dataset(validation_data)
testing_data <- normalize_dataset(testing_data)

# 4. Verification
cat("\nTraining Data Normalization Summary:\n")
print(summary(training_data[continuous_vars]))

cat("\nValidation Data Normalization Summary:\n")
print(summary(validation_data[continuous_vars]))

cat("\nTest Data Normalization Summary:\n")
print(summary(testing_data[continuous_vars]))

### ------------------------------------
### EDA starts HERE
### ------------------------------------

# ----------------------------------------------------------------------------------
# Compute Highly Correlated Variables (Before Variable Selection) - only for training
# ----------------------------------------------------------------------------------
# Compute correlation matrix on numerical variables
corr_matrix <- cor(training_data %>% select(where(is.numeric)), use = "complete.obs")

# Convert to long format and clean up
corr_long <- as.data.frame(as.table(corr_matrix)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>%
  filter(Variable1 != Variable2) %>%          # Remove self-correlations
  mutate(AbsCorrelation = abs(Correlation)) %>%  # Absolute values
  arrange(desc(AbsCorrelation))               # Sort by highest correlation

# Filter and print highly correlated pairs (threshold = 0.7)
highly_correlated <- corr_long %>% 
  filter(AbsCorrelation >= 0.7)

print("Highly correlated variable pairs (|r| >= 0.7):")
print(highly_correlated)

# Correlation heatmap
corrplot(corr_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)


# ------------------------------------------------------------------
# Variable Selection (Drop Columns solve multicollinearity) - ALL 3 SETS
# ------------------------------------------------------------------
# 1. Drop "Total Chol" because LDL is a better indicator in the context of CKD.
#    (Total Chol is highly correlated with LDL at 0.93)
training_data <- training_data %>% select(-`total_chol`)
validation_data <- validation_data %>% select(-`total_chol`)
testing_data <- testing_data %>% select(-`total_chol`)

# 2. Drop "Waist" because it is highly correlated with both Weight and BMI (around 0.87),
#    and you may prefer Weight and BMI to better capture the clinical aspect of body composition.
training_data <- training_data %>% select(-waist)
validation_data <- validation_data %>% select(-waist)
testing_data <- testing_data %>% select(-waist)

# 3. Drop "Weight"
training_data <- training_data %>% select(-weight)
validation_data <- validation_data %>% select(-weight)
testing_data <- testing_data %>% select(-weight)

# 4. Drop "Obese" because BMI (and Weight) provides a more informative, continuous measure.
#    Obese is highly correlated with BMI and Weight.
training_data <- training_data %>% select(-obese)
validation_data <- validation_data %>% select(-obese)
testing_data <- testing_data %>% select(-obese)

# 5. Consolidate "Fam CVD" and "Fam Hypertension" into a single variable.
#    These two variables are highly correlated (0.79) and represent overlapping family history.
#    Here, we create a new binary variable 'FamCardio' which is 1 if either condition is present.
training_data <- training_data %>%
  mutate(
    fam_cvd = as.integer(fam_cvd), 
    fam_hypertension = as.integer(fam_hypertension),
    fam_cardio = factor(if_else(fam_cvd == 1 | fam_hypertension == 1, 1, 0))
  ) %>%
  select(-fam_cvd, -fam_hypertension)

validation_data <- validation_data %>%
  mutate(
    fam_cvd = as.integer(fam_cvd), 
    fam_hypertension = as.integer(fam_hypertension),
    fam_cardio = factor(if_else(fam_cvd == 1 | fam_hypertension == 1, 1, 0))
  ) %>%
  select(-fam_cvd, -fam_hypertension)

testing_data <- testing_data %>%
  mutate(
    fam_cvd = as.integer(fam_cvd), 
    fam_hypertension = as.integer(fam_hypertension),
    fam_cardio = factor(if_else(fam_cvd == 1 | fam_hypertension == 1, 1, 0))
  ) %>%
  select(-fam_cvd, -fam_hypertension)


# Check the remaining variable names
print(names(training_data))
print(names(validation_data))
print(names(testing_data))

# ----------------------------------------------------------
# Compute Highly Correlated Variables (AFTER Variable Selection)
# ----------------------------------------------------------
# Compute correlation matrix on numerical variables
corr_matrix <- cor(training_data %>% select(where(is.numeric)), use = "complete.obs")

# Convert to long format and clean up
corr_long <- as.data.frame(as.table(corr_matrix)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>%
  filter(Variable1 != Variable2) %>%          # Remove self-correlations
  mutate(AbsCorrelation = abs(Correlation)) %>%  # Absolute values
  arrange(desc(AbsCorrelation))               # Sort by highest correlation

# Filter and print highly correlated pairs (threshold = 0.7)
highly_correlated <- corr_long %>% 
  filter(AbsCorrelation >= 0.7)

print("Highly correlated variable pairs (|r| >= 0.7):")
print(highly_correlated)

# Correlation heatmap
corrplot(corr_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)

View(training_data)

# --------------------------------------
# Check Class Balance (ckd) & Skewness
# --------------------------------------
# Convert ckd to factor for proper handling
training_data$ckd <- as.factor(training_data$ckd)

# Class distribution analysis
class_dist <- table(training_data$ckd)
prop_dist <- prop.table(class_dist)

cat("\nClass Distribution:\n")
print(class_dist)
cat("\nClass Proportions:\n")
print(round(prop_dist, 3))

# Visualize class balance
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


# Check class distribution in training set
train_class_dist <- table(training_data$ckd)
train_prop <- prop.table(train_class_dist)
cat("Training Set Class Proportions:\n")
print(round(train_prop, 3))

# Check class distribution in validation set
val_class_dist <- table(validation_data$ckd)
val_prop <- prop.table(val_class_dist)
cat("Validation Set Class Proportions:\n")
print(round(val_prop, 3))

###### SAVE ALL 3 FILES READY FOR MODELLING ############
write.csv(training_data, "screening/clean_train.csv", row.names = FALSE)
write.csv(validation_data, "screening/clean_val.csv", row.names = FALSE)
write.csv(testing_data, "screening/clean_test.csv", row.names = FALSE)

cat("\nPre-processing complete. Cleaned train, validation, and test datasets are saved.\n")


##### 3 Models starts here (please don't check things up unless something's wrong - notice in chat plz ~)

## ------------------------ LINEAR PROBABILITY MODEL ------------------------

# Load necessary libraries
library(stats)
library(caret)

# Read training and validation datasets
clean_train <- read.csv("screening/clean_train.csv")
clean_val <- read.csv("screening/clean_val.csv")

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






## ------------- LOGISTIC REGRESSION MODEL ------------------------

getwd()
# Load the cleaned training and validation datasets
train_data <- read.csv("screening/clean_train.csv", stringsAsFactors = TRUE)
val_data   <- read.csv("screening/clean_val.csv", stringsAsFactors = TRUE)

# Load the original test set (with IDs) and the cleaned test set
test_original <- read.csv("screening/test.csv", stringsAsFactors = TRUE)
test_clean <- read.csv("screening/clean_test.csv", stringsAsFactors = TRUE)

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
  threshold <- 0.1
  test_pred <- ifelse(test_probs > threshold, 1, 0)
  cat("Length of test_pred:", length(test_pred), "\n")
  
  # Combine with test IDs
  submission <- data.frame(ID = test_clean$id, CKD_Prediction = test_pred)
  write.csv(submission, "CKD_test_predictions_logistic.csv", row.names = FALSE)
  cat("Test predictions saved to CKD_test_predictions_logistic.csv\n")
} else {
  cat("Error: x_test has 0 rows. Please check the cleaned test data.\n")
}

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
threshold <- 0.1
test_pred <- ifelse(test_probs > threshold, 1, 0)
cat("Length of test_pred: ", length(test_pred), "\n")

# Combine the test IDs and predictions into a data frame.
submission <- data.frame(ID = test_clean$id, CKD_Prediction = test_pred)

# Save the prediction result to a CSV file.
write.csv(submission, "CKD_test_predictions_logistic.csv", row.names = FALSE)
cat("Test predictions saved to CKD_test_predictions_logistic.csv\n")



## ------------------------ RANDOM FOREST MODEL ------------------------


# Load necessary libraries
library(randomForest)
library(caret)

# Step 1: Define the cleaning function (if not already defined)
clean_factor_levels <- function(data) {
  # Example: Clean factor levels by removing unused levels
  data <- droplevels(data)
  return(data)
}

# Step 2: Apply the cleaning function to training and validation datasets
training_data <- clean_factor_levels(training_data)
validation_data <- clean_factor_levels(validation_data)

# Step 3: Ensure the target variable 'ckd' is a factor with valid levels
training_data$ckd <- factor(training_data$ckd, levels = c("1", "0"))
validation_data$ckd <- factor(validation_data$ckd, levels = c("1", "0"))

# Step 4: Define the outcome variable and predictors
outcome_var <- "ckd"
predictors <- setdiff(names(training_data), outcome_var) # All columns except 'ckd'

# Step 1: Calculate class weights
#class_weights <- table(training_data$ckd)
#class_weights <- 1 / class_weights # Inverse of class frequencies
#class_weights <- class_weights / sum(class_weights) # Normalize weights

# Step 2: Train the Random Forest model with class weights
rf_model <- randomForest(
  x = training_data[, predictors],  # Select predictors
  y = training_data$ckd,  # Target variable
  ntree = 100, 
  importance = TRUE
)


#classwt = class_weights

# Step 3: Evaluate the model on the validation dataset
validation_predictions <- predict(rf_model, validation_data)

# Confusion matrix and evaluation metrics
conf_matrix <- confusionMatrix(validation_predictions, validation_data$ckd)
print(conf_matrix)

# Feature importance
importance(rf_model)








