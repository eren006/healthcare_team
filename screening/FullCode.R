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
# Split the dataset into training and testing sets
# ---------------------------------------------------
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
write.csv(training_data, "screening/train.csv", row.names = FALSE)
write.csv(testing_data, "screening/test.csv", row.names = FALSE)


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
# Handle Missing Values
# -------------------------------

# 1. Identify binary columns (0/1 with or without NA)
binary_cols <- c("female", "educ", "unmarried", "income", "insured", "obese", "dyslipidemia", "pvd", 
                 "poor_vision", "smoker", "hypertension", "diabetes", "fam_hypertension", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd")

# 2. Identify numerical columns
numerical_cols <- c("age", "weight", "height", "bmi", "waist", "sbp", "dbp", "hdl", "ldl", "total_chol")

# 3. Identify categorical columns
categorical_cols <- c("racegrp", "care_source", "activity")

# 4. Remove rows with >70% missing in numerical columns
missing_threshold <- 0.7 * length(numerical_cols)
removed_records <- training_data[rowSums(is.na(training_data[, numerical_cols])) > missing_threshold, ]
training_data <- training_data[rowSums(is.na(training_data[, numerical_cols])) <= missing_threshold, ]

# 5. Fill binary columns with mode (0/1 only)
training_data <- fill_na_with_mode(training_data, binary_cols)

# 6. Fill numerical columns with mean (NA excluded)
for (col in numerical_cols) {
  training_data[[col]][is.na(training_data[[col]])] <- mean(training_data[[col]], na.rm = TRUE)
}

# 7. Fill categorical columns with majority (mode) value
training_data <- fill_na_with_majority(training_data, categorical_cols)

# Verification
print("Missing values after processing:")
print(colSums(is.na(training_data)))

# Apply to training_data (wrapped in function)
handle_missing_values <- function(df) {
  binary_cols <- c("female", "educ", "unmarried", "income", "insured", "obese", "dyslipidemia", "pvd", 
                   "poor_vision", "smoker", "hypertension", "diabetes", "fam_hypertension", "fam_diabetes", 
                   "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd")
  numerical_cols <- c("age", "weight", "height", "bmi", "waist", "sbp", "dbp", "hdl", "ldl", "total_chol")
  categorical_cols <- c("racegrp", "care_source", "activity")
  
  df <- fill_na_with_mode(df, binary_cols)
  for (col in numerical_cols) {
    df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
  }
  df <- fill_na_with_majority(df, categorical_cols)
  
  return(list(cleaned_data = df, removed_records = removed_records))
}

# Apply function
result <- handle_missing_values(training_data)
training_data <- result$cleaned_data
removed <- result$removed_records

# Print results
cat("Removed records:", nrow(removed), "\n")
cat("Missing values detected:", colSums(is.na(training_data)), "\n")

# ---------------------------------------------
# Change to dummy variables & change to factor
# ---------------------------------------------
training_data <- dummy_cols(training_data, select_columns = "racegrp", remove_first_dummy = TRUE)
training_data <- dummy_cols(training_data, select_columns = "care_source", remove_first_dummy = TRUE)
training_data <- dummy_cols(training_data, select_columns = "activity", remove_first_dummy = TRUE)
training_data <- training_data %>% select(-racegrp)
training_data <- training_data %>% select(-care_source)
training_data <- training_data %>% select(-id)
dim(training_data)

binary_vars <- c("female", "educ", "unmarried", "income","insured", "obese", "dyslipidemia", "pvd", "poor_vision", 
                 "smoker", "hypertension", "diabetes",  "fam_hypertension", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd", "racegrp_hispa", "racegrp_other", "racegrp_white",
                 "care_source_DrHMO", "care_source_noplace", "care_source_other", "activity_2","activity_3","activity_4")

training_data[binary_vars] <- lapply(training_data[binary_vars], as.factor)


# --------------------------------------
# Handle Outliers (Continuous Variables)
# --------------------------------------
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
# Define normalization function
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply normalization
training_data <- training_data %>% 
  mutate(across(all_of(continuous_vars), normalize))

# Verify normalization
print("Normalized variables (0-1 range):")
print(summary(training_data %>% select(all_of(continuous_vars))))

# ----------------------------------------------------------
# Compute Highly Correlated Variables (Before Variable Selection)
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

# ------------------------------------------------------------------
# Variable Selection (Drop Columns solve multicollinearity)
# ------------------------------------------------------------------
# 1. Drop "Total Chol" because LDL is a better indicator in the context of CKD.
#    (Total Chol is highly correlated with LDL at 0.93)
training_data <- training_data %>% select(-`total_chol`)

# 2. Drop "Waist" because it is highly correlated with both Weight and BMI (around 0.87),
#    and you may prefer Weight and BMI to better capture the clinical aspect of body composition.
training_data <- training_data %>% select(-waist)

# 3. Drop "Weight"
training_data <- training_data %>% select(-weight)

# 4. Drop "Obese" because BMI (and Weight) provides a more informative, continuous measure.
#    Obese is highly correlated with BMI and Weight.
training_data <- training_data %>% select(-obese)

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


# Check the remaining variable names
print(names(training_data))

# ----------------------------------------------------------
# Compute Highly Correlated Variables (Before Variable Selection)
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

# ----------------------------------------
# Prepare Training Set for Modeling use
# ----------------------------------------
write.csv(training_data, "screening/clean_train.csv", row.names = FALSE)







