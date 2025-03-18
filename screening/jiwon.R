install.packages("naniar")

getwd()
setwd("/Users/jiwonchoi/Desktop/github/healthcare_team/screening")

# -------------------------------
# 1. Load Libraries & Data
# -------------------------------
library(tidyverse)    # For data manipulation
library(caret)        # For preprocessing and model partitioning
library(corrplot)     # For plotting correlation heatmaps
library(ggplot2)      # For general plotting
library(readxl)
library(naniar)
library(reshape2)

# Load your dataset (adjust the file path and filename as needed)
data <- read_excel("screening_data.xls", sheet = "All Data")

# Check the data
head(data)

# -------------------------------
# 2. Missing Value Treatment
# -------------------------------
# Check for missing values
print(colSums(is.na(data)))

train_data <- data %>% filter(!is.na(CKD))
test_data  <- data %>% filter(is.na(CKD))

# Verify the dimensions (should be around 6000 for training and 2819 for test)
dim(train_data)
dim(test_data)


# -------------------------------
# 3. EDA 
# -------------------------------
# Check missing data
vis_miss(data)

# Suppose corr_matrix is your correlation matrix
# 1. Determine the reordering of variables via hierarchical clustering
var_order <- corrMatOrder(corr_matrix, order = "hclust")

# 2. Reorder your correlation matrix
reordered_corr_matrix <- corr_matrix[var_order, var_order]

# 3. Print the variable order to the console
print(colnames(reordered_corr_matrix))

# 4. Plot the reordered correlation matrix
corrplot(reordered_corr_matrix,
         method = "color",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7)

# Exclude self-correlations by setting the diagonal to NA
diag(corr_matrix) <- NA

# Optionally, set the upper triangle to NA to avoid duplicate pairs
corr_matrix[upper.tri(corr_matrix)] <- NA

# Convert the matrix into a long-format data frame, excluding NAs
corr_long <- melt(corr_matrix, na.rm = TRUE)

# Sort the data frame by the absolute value of the correlation coefficient, in descending order
corr_long_sorted <- corr_long[order(-abs(corr_long$value)), ]

# Print the top 10 highest absolute correlations (excluding self-correlations)
print(head(corr_long_sorted, 10))


# Data 
# 1. Drop "Total Chol" because LDL is a better indicator in the context of CKD.
#    (Total Chol is highly correlated with LDL at 0.93)
data <- data %>% select(-`Total Chol`)

# 2. Drop "Waist" because it is highly correlated with both Weight and BMI (around 0.87),
#    and you may prefer Weight and BMI to better capture the clinical aspect of body composition.
data <- data %>% select(-Waist)

# 3. Consolidate "Fam CVD" and "Fam Hypertension" into a single variable.
#    These two variables are highly correlated (0.79) and represent overlapping family history.
#    Here, we create a new binary variable 'FamCardio' which is 1 if either condition is present.
data <- data %>%
  mutate(FamCardio = ifelse(`Fam CVD` > 0 | `Fam Hypertension` > 0, 1, 0)) %>%
  select(-`Fam CVD`, -`Fam Hypertension`)

# 4. Drop "Obese" because BMI (and Weight) provides a more informative, continuous measure.
#    Obese is highly correlated with BMI and Weight.
data <- data %>% select(-Obese)

# Check the remaining variable names
print(names(data))





# Impute missing numeric values using median imputation
preProcValues <- preProcess(data, method = c("medianImpute"))
data <- predict(preProcValues, data)

# -------------------------------
# 3. Outlier Detection & Treatment
# -------------------------------
# Define a function to replace outliers with NA (using 1.5*IQR rule)
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[ x < (qnt[1] - H) | x > (qnt[2] + H) ] <- NA
  return(x)
}

# Apply the function to all numeric variables
data <- data %>%
  mutate(across(where(is.numeric), remove_outliers))

# Re-impute any new NAs from outlier removal (using the same preProcess object)
data <- predict(preProcValues, data)

# -------------------------------
# 4. Normalization
# -------------------------------
# Normalize (scale) numeric variables so they have mean 0 and SD 1
num_vars <- sapply(data, is.numeric)
data[num_vars] <- scale(data[num_vars])

# -------------------------------
# 5. Correlation Heat Map
# -------------------------------
# Compute the correlation matrix for numeric variables
corr_matrix <- cor(data[, num_vars], use = "pairwise.complete.obs")
# Plot the heat map of correlations
corrplot(corr_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)

# -------------------------------
# 6. Create BMI from Weight & Height
# -------------------------------
# Assuming 'Weight' is in kilograms and 'Height' is in centimeters:
data$BMI <- data$Weight / ((data$Height/100)^2)
# If your data uses different units (e.g., lbs and inches), adjust the formula accordingly.

# -------------------------------
# 7. Commonsense Variable Removal
# -------------------------------
# Remove variables that are not useful for prediction (e.g., IDs or variables with little variance)
# Adjust the variable names based on your dataset.
data <- data %>% select(-ID)  # Remove an ID column if present

# -------------------------------
# 8. Weighted Modeling for Skewness
# -------------------------------
# The goal here is to focus on correctly identifying the positives (CKD cases) even if that means a 
# higher cost for false negatives. For example, if your outcome variable is named 'CKD' (1 = at risk, 0 = not)
# and you want to penalize missing a positive case more heavily, create a weight vector.
# In this example, assign a weight of 1300 for positive cases and 100 for negatives.
data$weight <- ifelse(data$CKD == 1, 1300, 100)

# -------------------------------
# 9. Split Data (for training/validation)
# -------------------------------
# For demonstration, we split the data into training (80%) and testing (20%) sets.
set.seed(123)
trainIndex <- createDataPartition(data$CKD, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData  <- data[-trainIndex, ]

# -------------------------------
# 10. Model Building & Feature Selection
# -------------------------------
# Fit a weighted logistic regression model. (Other models such as classification trees could also be tried.)
# Exclude the weight variable from the predictors.
model <- glm(CKD ~ ., data = trainData %>% select(-weight), 
             family = binomial, weights = trainData$weight)
summary(model)

# Optional: Stepwise feature selection to refine the model
model_step <- step(model, direction = "both")
summary(model_step)

# -------------------------------
# 11. Prediction & Adjusting Classification Threshold
# -------------------------------
# Get predicted probabilities on the test set using the refined model.
pred_probs <- predict(model_step, newdata = testData, type = "response")

# To focus on identifying positives, adjust the classification threshold below 0.5 (e.g., 0.3).
threshold <- 0.3
pred_class <- ifelse(pred_probs > threshold, 1, 0)

# View a confusion matrix to see performance (you may need the caret package's confusionMatrix function)
conf_mat <- caret::confusionMatrix(factor(pred_class), factor(testData$CKD))
print(conf_mat)

# -------------------------------
# 12. (Optional) Feature Selection Finalization
# -------------------------------
# Additional feature selection methods (e.g., using recursive feature elimination) can be applied here if desired.
