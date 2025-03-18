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
# Split the dataset into training and testing sets
# Training set: First 6,000 observations with CKD values
# Testing set: Remaining 2,819 observations without CKD values
print(colSums(is.na(data)))
training_data <- data %>% filter(!is.na(ckd))
testing_data  <- data %>% filter(is.na(ckd))

# Verify the dimensions (should be around 6000 for training and 2819 for test)
dim(training_data)
dim(testing_data)

vis_miss(data)

# Save the datasets as CSV files
getwd()
write.csv(training_data, "/Users/judyfu/Desktop/HC/healthcare_team/screening/train.csv", row.names = FALSE)
write.csv(testing_data, "/Users/judyfu/Desktop/HC/healthcare_team/screening/test.csv", row.names = FALSE)

data <- training_data
dim(data)

test <- testing_data
dim(test)

# -------------------------------
# Change to dummy variables
# -------------------------------
data <- dummy_cols(data, select_columns = "racegrp", remove_first_dummy = TRUE)
data <- dummy_cols(data, select_columns = "care_source", remove_first_dummy = TRUE)
data <- data %>% select(-racegrp)
data <- data %>% select(-care_source)
dim(data)
View(data)

# -------------------------------
# Handling missing values
# -------------------------------
# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Handle missing values only on the training set
handle_missing_values <- function(data) {
  # Identify binary and numerical columns
  binary_cols <- sapply(data, function(x) length(unique(x)) == 2)
  numerical_cols <- sapply(data, is.numeric)
  
  # Fill binary variables with mode
  for (col in names(data)[binary_cols]) {
    data[[col]][is.na(data[[col]])] <- get_mode(data[[col]])
  }
  
  # Fill numerical variables with mean
  for (col in names(data)[numerical_cols]) {
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  }
  
  # Identify records missing more than 70% of numerical columns
  missing_threshold <- 0.7 * sum(numerical_cols)
  removed_records <- data[rowSums(is.na(data[, numerical_cols])) > missing_threshold, ]
  
  # Remove records missing more than 70% of numerical columns
  data <- data[rowSums(is.na(data[, numerical_cols])) <= missing_threshold, ]
  
  # Return cleaned data and removed records
  return(list(cleaned_data = data, removed_records = removed_records))
}

# Apply the function to handle missing values only on the training set
result <- handle_missing_values(data)
data <- result$cleaned_data
removed_records <- result$removed_records

# Print the removed records
print("Records removed due to more than 70% missing numerical values:")
print(removed_records)

# Verify the dimensions after handling missing values
dim(data)

head(data)

View(data)

# -------------------------------
# Handle Outliers (Using IQR Method)
# -------------------------------
handle_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

data <- data %>% mutate(across(where(is.numeric), handle_outliers))

dim(data)

View(data)

# -------------------------------
# Normalization (Min-Max Scaling)
# -------------------------------
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
data <- data %>% mutate(across(where(is.numeric), normalize))
dim(data)

View(data)

# -------------------------------
# Compute Highly Correlated Variables (Train Data Only)
# -------------------------------
# Compute correlation matrix on numerical variables
corr_matrix <- cor(data %>% select(where(is.numeric)), use = "complete.obs")

# Convert correlation matrix to long format
corr_long <- as.data.frame(as.table(corr_matrix))

# Rename columns
colnames(corr_long) <- c("Variable1", "Variable2", "Correlation")

# Remove self-correlations
corr_long <- corr_long %>% filter(Variable1 != Variable2)

# Convert to absolute correlation values
corr_long$AbsCorrelation <- abs(corr_long$Correlation)

# Sort by highest absolute correlation
corr_long <- corr_long %>% arrange(desc(AbsCorrelation))

# Filter for highly correlated variables (Threshold: 0.8)
highly_correlated <- corr_long %>% filter(AbsCorrelation >= 0.8)

# Print highly correlated variable pairs
print(highly_correlated)


# -------------------------------
# Model Skewness - Weighting Method (Only on Training Data)
# -------------------------------
table(data$ckd) 

# Compute class weights for imbalance handling
class_weights <- table(data$ckd)
class_weights <- max(class_weights) / class_weights

# Apply weights to training data
data$weights <- ifelse(data$ckd == "1", class_weights[1], class_weights[2])

# -------------------------------
# Feature Selection (Only on Training Set)
# -------------------------------
# Drop "total_chol" (Highly correlated with LDL)
data <- data %>% select(-total_chol)

# Drop "waist" (Highly correlated with BMI & weight)
data <- data %>% select(-waist)

# Consolidate Family History Variables
data <- data %>%
  mutate(fam_cardiovascular = ifelse(fam_cvd > 0 | fam_hypertension > 0, 1, 0)) %>%
  select(-fam_cvd, -fam_hypertension)

# Drop "obese" (Redundant with BMI & weight)
data <- data %>% select(-obese)