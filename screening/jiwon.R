# -------------------------------
# 1. Load Necessary Libraries
# -------------------------------
library(readxl)
library(dplyr)
library(caret)
library(glmnet)
library(pROC)
library(corrplot)
library(naniar)
library(fastDummies)

# ----------------------------------------
# 2. Load the Dataset & Split into Training and Testing Sets
# ----------------------------------------
getwd()
setwd("/Users/jiwonchoi/Desktop/github/healthcare_team/screening")
data <- read_excel("screening_goodnames.xlsx")
head(data)

# Training set: First 6,000 observations with CKD values
# Testing set: Remaining 2,819 observations without CKD values
training_data <- data %>% filter(!is.na(ckd))
testing_data  <- data %>% filter(is.na(ckd))

# Save the initial training and testing datasets
write.csv(training_data, "train.csv", row.names = FALSE)
write.csv(testing_data, "test.csv", row.names = FALSE)

# ----------------------------------------
# 3. Handle Missing Values for Training Data Only
# ----------------------------------------

# Identify columns
binary_cols <- c("female", "educ", "unmarried", "income", "insured", "obese", "dyslipidemia", 
                 "pvd", "poor_vision", "smoker", "hypertension", "diabetes", "fam_hypertension", 
                 "fam_diabetes", "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd")
numerical_cols <- c("age", "weight", "height", "bmi", "waist", "sbp", "dbp", "hdl", "ldl")
categorical_cols <- c("racegrp", "care_source", "activity")

# Fill binary columns with mode
get_binary_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  if (length(uniqv) == 0) return(NA)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
for (col in binary_cols) {
  training_data[[col]][is.na(training_data[[col]])] <- get_binary_mode(training_data[[col]])
}

# Fill numerical columns with mean
for (col in numerical_cols) {
  training_data[[col]][is.na(training_data[[col]])] <- mean(training_data[[col]], na.rm = TRUE)
}

# Fill categorical columns with majority value
fill_na_with_majority <- function(df, cols) {
  df <- df %>% mutate(across(all_of(cols), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
  return(df)
}
training_data <- fill_na_with_majority(training_data, categorical_cols)

# Remove unnecessary columns and convert to dummy variables
training_data <- dummy_cols(training_data, select_columns = c("racegrp", "care_source", "activity"), 
                            remove_first_dummy = TRUE) %>% 
  select(-racegrp, -care_source, -activity, -id)

# Save the cleaned training data before splitting
write.csv(training_data, "clean_train.csv", row.names = FALSE)

# ----------------------------------------
# 4. Validation Split from Training Data (No Data Leakage)
# ----------------------------------------
set.seed(42)
trainIndex <- createDataPartition(training_data$ckd, p = 0.8, list = FALSE)
validation_data <- training_data[-trainIndex, ]
training_data <- training_data[trainIndex, ]

# Save the split training and validation datasets
write.csv(training_data, "clean_train.csv", row.names = FALSE)
write.csv(validation_data, "clean_val.csv", row.names = FALSE)

# ----------------------------------------
# 5. Imputation & Normalization - ONLY using Training Data
# ----------------------------------------

# Compute means, min, and max values from training data
train_means <- sapply(training_data[numerical_cols], mean, na.rm = TRUE)
train_min <- sapply(training_data[numerical_cols], min, na.rm = TRUE)
train_max <- sapply(training_data[numerical_cols], max, na.rm = TRUE)

# Normalization function
normalize <- function(x, min_val, max_val) {
  (x - min_val) / (max_val - min_val)
}

# Apply preprocessing to training data
for (col in numerical_cols) {
  training_data[[col]] <- normalize(training_data[[col]], train_min[col], train_max[col])
}

# Apply the same preprocessing to validation and testing data
for (col in numerical_cols) {
  validation_data[[col]] <- normalize(validation_data[[col]], train_min[col], train_max[col])
  testing_data[[col]] <- normalize(testing_data[[col]], train_min[col], train_max[col])
}

# ----------------------------------------
# 6. Train Logistic Regression Model
# ----------------------------------------
training_data$ckd <- as.factor(training_data$ckd)
validation_data$ckd <- as.factor(validation_data$ckd)

logistic_model <- glm(ckd ~ ., data = training_data, family = binomial)
summary(logistic_model)

# ----------------------------------------
# 7. Model Evaluation on Validation Set
# ----------------------------------------

val_prob <- predict(logistic_model, newdata = validation_data, type = "response")
profits <- data.frame()
thresholds <- seq(0.01, 0.99, by = 0.01)

for (threshold in thresholds) {
  val_pred <- ifelse(val_prob > threshold, 1, 0)
  TP <- sum(val_pred == 1 & validation_data$ckd == 1)
  FP <- sum(val_pred == 1 & validation_data$ckd == 0)
  profit <- TP * 1300 - FP * 100
  profits <- rbind(profits, data.frame(Threshold = threshold, Profit = profit))
}

optimal_threshold <- profits$Threshold[which.max(profits$Profit)]
cat("✅ Optimal Threshold for Maximum Profit: ", optimal_threshold, "\n")

plot(profits$Threshold, profits$Profit, type = "l", col = "blue", 
     xlab = "Threshold", ylab = "Profit", main = "Profit vs. Threshold")

# ----------------------------------------
# 8. Predict on Test Data
# ----------------------------------------

test_prob <- predict(logistic_model, newdata = testing_data, type = "response")
test_pred <- ifelse(test_prob > optimal_threshold, 1, 0)

output <- data.frame(ID = 1:length(test_pred), CKD_Prediction = test_pred)
write.csv(output, "CKD_predictions.csv", row.names = FALSE)

# ----------------------------------------
# 9. ROC Curve & AUC Calculation (Optional)
# ----------------------------------------

roc_curve <- roc(validation_data$ckd, val_prob)
auc_value <- auc(roc_curve)
cat("📊 AUC for Validation Set: ", auc_value, "\n")

plot(roc_curve, col = "red", main = paste("ROC Curve (AUC =", round(auc_value, 3), ")"))
