# install.packages("naniar")
# install.packages("fastDummies") 

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
# 3. Convert Binary Variables to Factors
# -------------------------------
binary_vars <- c("female", "educ", "unmarried", "dyslipidemia", "pvd", "poor_vision", 
                 "smoker", "hypertension", "fam_hypertension", "diabetes", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd")
data[binary_vars] <- lapply(data[binary_vars], as.factor)

# -------------------------------
# 4. Split Data into Train, Validation, and Test (to avoid data leakage)
# -------------------------------
train_data <- data %>% filter(!is.na(ckd))  # Labeled data for training/validation
test_data  <- data %>% filter(is.na(ckd))   # Unlabeled data for testing

# Create validation split from train_data (80% training, 20% validation)
set.seed(42)  
trainIndex <- createDataPartition(train_data$ckd, p = 0.8, list = FALSE)
validation_data <- train_data[-trainIndex, ]
train_data <- train_data[trainIndex, ]

# -------------------------------
# 5. Missing Value Treatment (using training set statistics)
# -------------------------------
numeric_vars <- names(train_data)[sapply(train_data, is.numeric)]
train_medians <- sapply(train_data[numeric_vars], median, na.rm = TRUE)

for (var in numeric_vars) {
  train_data[[var]][is.na(train_data[[var]])] <- train_medians[var]
  validation_data[[var]][is.na(validation_data[[var]])] <- train_medians[var]
  test_data[[var]][is.na(test_data[[var]])] <- train_medians[var]
}

# -------------------------------
# 6. Outlier Handling (using training set thresholds)
# -------------------------------
compute_thresholds <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(c(lower = lower_bound, upper = upper_bound))
}

thresholds <- lapply(train_data[numeric_vars], compute_thresholds)

for (var in numeric_vars) {
  lower <- thresholds[[var]]["lower"]
  upper <- thresholds[[var]]["upper"]
  
  train_data[[var]] <- pmax(pmin(train_data[[var]], upper), lower)
  validation_data[[var]] <- pmax(pmin(validation_data[[var]], upper), lower)
  test_data[[var]] <- pmax(pmin(test_data[[var]], upper), lower)
}

# -------------------------------
# 7. EDA: Missing Data Visualization & Correlation Heatmap
# -------------------------------
# Check missing data
vis_miss(train_data)

# Compute correlation matrix (Only on Training Data)
corr_matrix <- cor(train_data %>% select(where(is.numeric)), use = "complete.obs")

# Reorder correlation matrix using hierarchical clustering
var_order <- corrMatOrder(corr_matrix, order = "hclust")
reordered_corr_matrix <- corr_matrix[var_order, var_order]

# Plot the reordered correlation matrix
corrplot(reordered_corr_matrix,
         method = "color",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7)

# -------------------------------
# 8. Print Highly Correlated Variables (Train Data Only)
# -------------------------------
corr_long <- as.data.frame(as.table(corr_matrix))
colnames(corr_long) <- c("Variable1", "Variable2", "Correlation")

corr_long <- corr_long %>% filter(Variable1 != Variable2)
corr_long$AbsCorrelation <- abs(corr_long$Correlation)
corr_long <- corr_long %>% arrange(desc(AbsCorrelation))

highly_correlated <- corr_long %>% filter(AbsCorrelation >= 0.8)
print(highly_correlated)

# -------------------------------
# 9. Feature Selection (Only on Training Set)
# -------------------------------
train_data <- train_data %>% select(-total_chol, -waist, -obese)
validation_data <- validation_data %>% select(-total_chol, -waist, -obese)
test_data  <- test_data %>% select(-total_chol, -waist, -obese)

train_data <- train_data %>% 
  mutate(fam_cardiovascular = ifelse(fam_cvd > 0 | fam_hypertension > 0, 1, 0)) %>%
  select(-fam_cvd, -fam_hypertension)

validation_data <- validation_data %>% 
  mutate(fam_cardiovascular = ifelse(fam_cvd > 0 | fam_hypertension > 0, 1, 0)) %>%
  select(-fam_cvd, -fam_hypertension)

test_data <- test_data %>% 
  mutate(fam_cardiovascular = ifelse(fam_cvd > 0 | fam_hypertension > 0, 1, 0)) %>%
  select(-fam_cvd, -fam_hypertension)

# -------------------------------
# 10. Normalization (Min-Max Scaling Using Train Data)
# -------------------------------
min_vals <- sapply(train_data %>% select(where(is.numeric)), min, na.rm = TRUE)
max_vals <- sapply(train_data %>% select(where(is.numeric)), max, na.rm = TRUE)

normalize <- function(x, min_val, max_val) {
  (x - min_val) / (max_val - min_val)
}

train_data <- train_data %>% mutate(across(where(is.numeric), ~ normalize(., min_vals[cur_column()], max_vals[cur_column()])))
validation_data <- validation_data %>% mutate(across(where(is.numeric), ~ normalize(., min_vals[cur_column()], max_vals[cur_column()])))
test_data <- test_data %>% mutate(across(where(is.numeric), ~ normalize(., min_vals[cur_column()], max_vals[cur_column()])))

# -------------------------------
# 11. Model Skewness - Weighting Method (Only on Training Data)
# -------------------------------
class_weights <- table(train_data$ckd)
class_weights <- max(class_weights) / class_weights

train_data$weights <- ifelse(train_data$ckd == "1", class_weights[1], class_weights[2])

# -------------------------------
# 12. Model Building & Feature Selection
# -------------------------------
single_level_vars <- names(train_data)[sapply(train_data, function(x) is.factor(x) && length(unique(x)) == 1)]
train_data <- train_data %>% select(-all_of(single_level_vars))

selected_features <- names(train_data %>% select(-ckd, -weights, -weight))

model <- glm(as.formula(paste("ckd ~", paste(selected_features, collapse = " + "))), 
             data = train_data, 
             family = binomial, 
             weights = train_data$weights)

summary(model)

# -------------------------------
# 13. Prediction & Adjusting Classification Threshold
# -------------------------------
pred_probs <- predict(model, newdata = test_data, type = "response")
threshold <- 0.3  
pred_class <- ifelse(pred_probs > threshold, 1, 0)

conf_mat <- caret::confusionMatrix(factor(pred_class), factor(test_data$ckd))
print(conf_mat)
