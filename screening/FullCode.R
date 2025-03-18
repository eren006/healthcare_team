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

# ---------------------------------------------
# Change to dummy variables & change to factor
# ---------------------------------------------
data <- dummy_cols(data, select_columns = "racegrp", remove_first_dummy = TRUE)
data <- dummy_cols(data, select_columns = "care_source", remove_first_dummy = TRUE)
data <- data %>% select(-racegrp)
data <- data %>% select(-care_source)
dim(data)

binary_vars <- c("female", "educ", "unmarried", "dyslipidemia", "pvd", "poor_vision", 
                 "smoker", "hypertension", "fam_hypertension", "diabetes", "fam_diabetes", 
                 "stroke", "cvd", "fam_cvd", "chf", "anemia", "ckd")

data[binary_vars] <- lapply(data[binary_vars], as.factor)

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
write.csv(training_data, "/Users/judyfu/Desktop/HC/healthcare_team/screening/train.csv", row.names = FALSE)
write.csv(testing_data, "/Users/judyfu/Desktop/HC/healthcare_team/screening/test.csv", row.names = FALSE)

data <- training_data
dim(data)

test <- testing_data
dim(test)

# -------------------------------
# Function to Calculate Mode (for binary columns)
# -------------------------------
get_binary_mode <- function(v) {
  uniqv <- unique(na.omit(v))  # Ignore NA values
  if(length(uniqv) == 0) return(NA)  # Handle all-NA case
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# -------------------------------
# Detect Binary Columns
# -------------------------------
is_binary_column <- function(x) {
  clean_x <- na.omit(x)
  all(clean_x %in% c(0, 1)) && length(unique(clean_x)) <= 2
}

# -------------------------------
# Handle Missing Values
# -------------------------------
handle_missing_values <- function(data) {
  # 1. Identify binary columns (0/1 with or without NA)
  binary_cols <- sapply(data, is_binary_column)
  
  # 2. Identify numerical columns
  numerical_cols <- sapply(data, is.numeric)
  
  # 3. Remove rows with >70% missing in numerical columns
  missing_threshold <- 0.7 * sum(numerical_cols)
  removed_records <- data[rowSums(is.na(data[, numerical_cols])) > missing_threshold, ]
  data <- data[rowSums(is.na(data[, numerical_cols])) <= missing_threshold, ]
  
  # 4. Fill binary columns with mode (0/1 only)
  for(col in names(data)[binary_cols]) {
    data[[col]][is.na(data[[col]])] <- get_binary_mode(data[[col]])
  }
  
  # 5. Fill numerical columns with mean (NA excluded)
  for(col in names(data)[numerical_cols]) {
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  }
  
  # Verification
  print("Missing values after processing:")
  print(colSums(is.na(data)))
  
  return(list(cleaned_data = data, 
              removed_records = removed_records,
              binary_columns = names(data)[binary_cols]))
}

result <- handle_missing_values(data)
data <- result$cleaned_data
removed <- result$removed_records

# Print results
cat("Removed records:", nrow(removed), "\n")
cat("Binary columns detected:", result$binary_columns, "\n")

# -------------------------------
# Handle Outliers (Continuous Variables)
# -------------------------------

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
data <- data %>% 
  mutate(across(all_of(continuous_vars), handle_outliers))

# Verify outlier handling
print("Summary after outlier treatment:")
print(summary(data %>% select(all_of(continuous_vars))))

# -------------------------------
# Normalization (Min-Max Scaling)
# -------------------------------

# Define normalization function
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Apply to the same continuous variables used for outlier handling
data <- data %>% 
  mutate(across(all_of(continuous_vars), normalize))

# Verify normalization
print("Normalized variables (0-1 range):")
print(summary(data %>% select(all_of(continuous_vars))))

# ----------------------------------------------------------
# Compute Highly Correlated Variables (After Preprocessing)
# ----------------------------------------------------------
# Compute correlation matrix on numerical variables
corr_matrix <- cor(data %>% select(where(is.numeric)), use = "complete.obs")

# Convert to long format and clean up
corr_long <- as.data.frame(as.table(corr_matrix)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>%
  filter(Variable1 != Variable2) %>%          # Remove self-correlations
  mutate(AbsCorrelation = abs(Correlation)) %>%  # Absolute values
  arrange(desc(AbsCorrelation))               # Sort by highest correlation

# Filter and print highly correlated pairs (threshold = 0.8)
highly_correlated <- corr_long %>% 
  filter(AbsCorrelation >= 0.8)

print("Highly correlated variable pairs (|r| >= 0.8):")
print(highly_correlated)

corrplot(corr_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)

# -------------------------------
# Check Class Balance (ckd) & Skewness
# -------------------------------
# Convert ckd to factor for proper handling
data$ckd <- as.factor(data$ckd)

# Class distribution analysis
class_dist <- table(data$ckd)
prop_dist <- prop.table(class_dist)

cat("\nClass Distribution:\n")
print(class_dist)
cat("\nClass Proportions:\n")
print(round(prop_dist, 3))

# Visualize class balance
class_plot <- ggplot(data, aes(x = ckd, fill = ckd)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(title = "Class Distribution of CKD",
       subtitle = paste("Imbalance ratio:", round(max(prop_dist)/min(prop_dist), 1)),
       x = "CKD Diagnosis",
       y = "Count") +
  theme_minimal()

print(class_plot)





