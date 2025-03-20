getwd()
setwd("/Users/jiwonchoi/Desktop/github/healthcare_team/screening")
# ----------------------------------------
# Improved LASSO Logistic Regression Code
# ----------------------------------------
# Load Necessary Libraries
library(readxl)
library(dplyr)
library(caret)
library(glmnet)
library(pROC)
library(corrplot)
library(naniar)
library(fastDummies)

# ----------------------------------------
# 1. Load & Split Dataset
# ----------------------------------------
data <- read_excel("screening_goodnames.xlsx")

# Split the data into training (labeled) and testing (unlabeled)
training_data <- data %>% filter(!is.na(ckd))
testing_data  <- data %>% filter(is.na(ckd))

write.csv(training_data, "train.csv", row.names = FALSE)
write.csv(testing_data, "test.csv", row.names = FALSE)

# ----------------------------------------
# 2. Check Balance in Original Datasets
# ----------------------------------------
cat("Initial CKD Distribution in Training Data:\n")
print(prop.table(table(training_data$ckd)))
cat("\nNumber of Rows in Testing Data (No Labels):", nrow(testing_data), "\n\n")

# ----------------------------------------
# 3. Handling Missing Values
# ----------------------------------------

# Function to calculate mode
get_binary_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

binary_cols <- c("female", "educ", "unmarried", "dyslipidemia", "pvd", "poor_vision", 
                 "smoker", "hypertension", "fam_hypertension", "diabetes", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd")

numerical_cols <- c("age", "weight", "height", "bmi", "waist", "sbp", "dbp", "hdl", "ldl")

categorical_cols <- c("racegrp", "care_source", "activity")

# Fill binary columns with mode
for (col in binary_cols) {
  training_data[[col]][is.na(training_data[[col]])] <- get_binary_mode(training_data[[col]])
}

# Fill numerical columns with mean
for (col in numerical_cols) {
  training_data[[col]][is.na(training_data[[col]])] <- mean(training_data[[col]], na.rm = TRUE)
}

# Fill categorical columns with most common value
fill_na_with_majority <- function(df, cols) {
  df <- df %>% mutate(across(all_of(cols), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
  return(df)
}

training_data <- fill_na_with_majority(training_data, categorical_cols)

# Remove rows with >70% missing numerical data
missing_threshold <- 0.7 * length(numerical_cols)
training_data <- training_data[rowSums(is.na(training_data[numerical_cols])) <= missing_threshold, ]

# ----------------------------------------
# 4. Handle Outliers
# ----------------------------------------
handle_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x <- pmax(pmin(x, upper_bound), lower_bound)
  return(x)
}

training_data <- training_data %>% mutate(across(all_of(numerical_cols), handle_outliers))

# ----------------------------------------
# 5. Normalize Numerical Variables
# ----------------------------------------
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

training_data <- training_data %>% mutate(across(all_of(numerical_cols), normalize))

# ----------------------------------------
# 6. Create Dummy Variables
# ----------------------------------------
training_data <- dummy_cols(training_data, select_columns = c("racegrp", "care_source", "activity"), 
                            remove_first_dummy = TRUE) %>% 
  select(-racegrp, -care_source, -activity)

# ----------------------------------------
# 7. Remove Single-Level Factors
# ----------------------------------------
single_level_vars <- names(training_data)[sapply(training_data, function(col) is.factor(col) && length(unique(col)) == 1)]
training_data <- training_data %>% select(-all_of(single_level_vars))

# ----------------------------------------
# 8. Train-Validation Split
# ----------------------------------------
set.seed(42)
trainIndex <- createDataPartition(training_data$ckd, p = 0.8, list = FALSE)
validation_data <- training_data[-trainIndex, ]
training_data <- training_data[trainIndex, ]

write.csv(training_data, "clean_train.csv", row.names = FALSE)
write.csv(validation_data, "clean_val.csv", row.names = FALSE)

# Check balance after splitting
cat("\nCKD Distribution in Training Data (After Split):\n")
print(prop.table(table(training_data$ckd)))
cat("\nCKD Distribution in Validation Data (After Split):\n")
print(prop.table(table(validation_data$ckd)))
cat("\n")

# ----------------------------------------
# 9. Train LASSO Logistic Regression Model (Without Weights)
# ----------------------------------------
# Remove incomplete cases BEFORE creating model matrices
training_data <- training_data[complete.cases(training_data), ]

# Prepare x_train and y_train AFTER removing incomplete cases
x_train <- model.matrix(ckd ~ . - 1, data = training_data)
y_train <- training_data$ckd

# Fit LASSO Model
lasso_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
best_lambda <- lasso_model$lambda.min
final_lasso <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)

# ----------------------------------------
# 10. Model Evaluation on Validation Data
# ----------------------------------------
# Filter out incomplete rows from validation_data
validation_data_complete <- validation_data[complete.cases(validation_data), ]

# Prepare x_val and y_val AFTER filtering incomplete cases
x_val <- model.matrix(ckd ~ . - 1, data = validation_data_complete)
y_val <- validation_data_complete$ckd

# Predict probabilities on validation set
val_prob_lasso <- as.vector(predict(final_lasso, newx = x_val, type = "response"))

# Adjust threshold to 0.3 to favor higher sensitivity
threshold <- 0.3
val_pred <- ifelse(val_prob_lasso > threshold, 1, 0)

# Calculate Sensitivity & Specificity
TP <- sum(val_pred == 1 & y_val == 1)
TN <- sum(val_pred == 0 & y_val == 0)
FP <- sum(val_pred == 1 & y_val == 0)
FN <- sum(val_pred == 0 & y_val == 1)

Sensitivity <- TP / (TP + FN)
Specificity <- TN / (TN + FP)

cat("Sensitivity (Recall):", round(Sensitivity, 3), "\n")
cat("Specificity:", round(Specificity, 3), "\n")

conf_matrix <- table(Predicted = val_pred, Actual = as.numeric(as.character(y_val)))
print(conf_matrix)

roc_curve <- roc(y_val, val_prob_lasso)
auc_value <- auc(roc_curve)
cat("AUC for Validation Set (LASSO):", auc_value, "\n")
plot(roc_curve, col = "red", main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))
