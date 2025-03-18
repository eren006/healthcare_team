# install.packages("naniar")
install.packages("fastDummies") 

getwd()
setwd("/Users/jiwonchoi/Desktop/github/healthcare_team/screening")

# -------------------------------
# 1. Load Libraries & Data
# -------------------------------
library(tidyverse)    
library(caret)        
library(corrplot)     
library(ggplot2)      
library(readxl)
library(naniar)
library(reshape2)
library(fastDummies)    

# Load dataset
data <- read_excel("screening_goodnames.xlsx")

# -------------------------------
# 2. One-Hot Encoding for Categorical Variables
# -------------------------------
data <- dummy_cols(data, select_columns = "racegrp", remove_first_dummy = TRUE)
data <- dummy_cols(data, select_columns = "care_source", remove_first_dummy = TRUE)
data <- data %>% select(-racegrp, -care_source)

# -------------------------------
# 3. Convert Binary Variables to Factors (Before Splitting)
# -------------------------------
binary_vars <- c("female", "educ", "unmarried", "dyslipidemia", "pvd", "poor_vision", 
                 "smoker", "hypertension", "fam_hypertension", "diabetes", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd")

data[binary_vars] <- lapply(data[binary_vars], as.factor)

# -------------------------------
# 4. Missing Value Treatment (Incorporating Friend’s Logic)
# -------------------------------
# Check missing values
print(colSums(is.na(data)))

# Replace missing values in numeric columns with median
data <- data %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# -------------------------------
# 5. Handle Outliers (Using IQR Method)
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

# -------------------------------
# 6. Split Data into Train, Validation, and Test (No Data Leakage)
# -------------------------------
train_data <- data %>% filter(!is.na(ckd))  # Train Data (Labeled)
test_data  <- data %>% filter(is.na(ckd))   # Test Data (Unlabeled)

# Validation Split from Train Data (80% Train, 20% Validation)
set.seed(42)  
trainIndex <- createDataPartition(train_data$ckd, p = 0.8, list = FALSE)
validation_data <- train_data[-trainIndex, ]
train_data <- train_data[trainIndex, ]

# Verify dataset dimensions
dim(train_data)      
dim(validation_data) 
dim(test_data)       

# -------------------------------
# 7. Compute & Print Highly Correlated Variables (Train Data Only)
# -------------------------------
# Compute correlation matrix on numerical variables
corr_matrix <- cor(train_data %>% select(where(is.numeric)), use = "complete.obs")

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
# 8. Feature Selection (Only on Training Set)
# -------------------------------
# Drop "total_chol" (Highly correlated with LDL)
train_data <- train_data %>% select(-total_chol)
validation_data <- validation_data %>% select(-total_chol)
test_data  <- test_data %>% select(-total_chol)

# Drop "waist" (Highly correlated with BMI & weight)
train_data <- train_data %>% select(-waist)
validation_data <- validation_data %>% select(-waist)
test_data  <- test_data %>% select(-waist)

# Consolidate Family History Variables
train_data <- train_data %>%
  mutate(fam_cardiovascular = ifelse(fam_cvd > 0 | fam_hypertension > 0, 1, 0)) %>%
  select(-fam_cvd, -fam_hypertension)

validation_data <- validation_data %>%
  mutate(fam_cardiovascular = ifelse(fam_cvd > 0 | fam_hypertension > 0, 1, 0)) %>%
  select(-fam_cvd, -fam_hypertension)

test_data <- test_data %>%
  mutate(fam_cardiovascular = ifelse(fam_cvd > 0 | fam_hypertension > 0, 1, 0)) %>%
  select(-fam_cvd, -fam_hypertension)

# Drop "obese" (Redundant with BMI & weight)
train_data <- train_data %>% select(-obese)
validation_data  <- validation_data %>% select(-obese)
test_data  <- test_data %>% select(-obese)

# -------------------------------
# 9. Normalization (Min-Max Scaling)
# -------------------------------
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

train_data <- train_data %>% mutate(across(where(is.numeric), normalize))
validation_data <- validation_data %>% mutate(across(where(is.numeric), normalize))
test_data <- test_data %>% mutate(across(where(is.numeric), normalize))

# -------------------------------
# 10. Model Skewness - Weighting Method (Only on Training Data)
# -------------------------------
table(train_data$ckd) 

# Compute class weights for imbalance handling
class_weights <- table(train_data$ckd)
class_weights <- max(class_weights) / class_weights

# Apply weights to training data
train_data$weights <- ifelse(train_data$ckd == "1", class_weights[1], class_weights[2])

# -------------------------------
# 11. Model Building & Feature Selection
# -------------------------------

# Identify categorical variables with only one level
single_level_vars <- names(train_data)[sapply(train_data, function(x) is.factor(x) && length(unique(x)) == 1)]

# Print the problematic variables
print(single_level_vars)

# Remove single-level categorical variables
train_data <- train_data %>% select(-all_of(single_level_vars))

# Fit a weighted logistic regression model (Excluding weight variable)
model <- glm(ckd ~ ., data = train_data %>% select(-weight), 
             family = binomial, weights = train_data$weights)

summary(model)

# -------------------------------
# 12. Prediction & Adjusting Classification Threshold
# -------------------------------
# Get predicted probabilities on the test set
pred_probs <- predict(model_step, newdata = test_data, type = "response")

# Adjust threshold for positive classification
threshold <- 0.3  
pred_class <- ifelse(pred_probs > threshold, 1, 0)

# Evaluate model performance
conf_mat <- caret::confusionMatrix(factor(pred_class), factor(test_data$ckd))
print(conf_mat)
