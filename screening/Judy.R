# Load necessary libraries
library(readxl)
library(dplyr)
library(caret)
library(rpart) # For classification trees
library(glmnet) # For logistic regression
library(pROC) # For ROC analysis
library(corrplot) # For correlation heatmap

# Load the dataset
data <- read_excel("screening/screening_goodnames.xlsx")

# Split the dataset into training and testing sets
# Training set: First 6,000 observations with CKD values
# Testing set: Remaining 2,819 observations without CKD values
training_data <- data[1:6000, ]
testing_data <- data[6001:nrow(data), ]

# Save the datasets as CSV files
write.csv(training_data, "train.csv", row.names = FALSE)
write.csv(testing_data, "test.csv", row.names = FALSE)

# Data Preprocessing
# Handle missing values in the training set
training_data <- training_data %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Handle outliers using the IQR method
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

training_data <- training_data %>% mutate(across(where(is.numeric), handle_outliers))

# Normalize the data using Min-Max normalization
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

training_data <- training_data %>% mutate(across(where(is.numeric), normalize))

# Create a heatmap for correlation
correlation_matrix <- cor(training_data %>% select(where(is.numeric)))
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, diag = FALSE)

# Model Training
# Define the target variable and features
target <- "CKD"
features <- setdiff(names(training_data), c("ID", "CKD"))

# Split the training data into training and validation sets
set.seed(123)
train_index <- createDataPartition(training_data[[target]], p = 0.8, list = FALSE)
train_set <- training_data[train_index, ]
validation_set <- training_data[-train_index, ]

# Train a logistic regression model
logistic_model <- glm(as.formula(paste(target, "~ .")), 
                      data = train_set %>% select(all_of(c(target, features))), 
                      family = binomial())

# Train a classification tree model
tree_model <- rpart(as.formula(paste(target, "~ .")), 
                    data = train_set %>% select(all_of(c(target, features))), 
                    method = "class")

# Evaluate models on the validation set
# Logistic Regression
logistic_pred <- predict(logistic_model, validation_set, type = "response")
logistic_roc <- roc(validation_set[[target]], logistic_pred)
cat("Logistic Regression AUC:", auc(logistic_roc), "\n")

# Classification Tree
tree_pred <- predict(tree_model, validation_set, type = "prob")[, 2]
tree_roc <- roc(validation_set[[target]], tree_pred)
cat("Classification Tree AUC:", auc(tree_roc), "\n")

# Choose the best model based on AUC
if (auc(logistic_roc) > auc(tree_roc)) {
  final_model <- logistic_model
  cat("Selected Model: Logistic Regression\n")
} else {
  final_model <- tree_model
  cat("Selected Model: Classification Tree\n")
}

# Make predictions on the test set
test_predictions <- predict(final_model, testing_data, type = "response")

# Adjust predictions based on the cost-benefit analysis
# Threshold optimization to maximize the score
# Score = $1,300 * True Positives - $100 * False Positives
optimize_threshold <- function(probabilities, true_labels) {
  thresholds <- seq(0, 1, by = 0.01)
  best_score <- -Inf
  best_threshold <- 0.5
  
  for (threshold in thresholds) {
    predicted_labels <- ifelse(probabilities >= threshold, 1, 0)
    true_positives <- sum(predicted_labels == 1 & true_labels == 1)
    false_positives <- sum(predicted_labels == 1 & true_labels == 0)
    score <- 1300 * true_positives - 100 * false_positives
    
    if (score > best_score) {
      best_score <- score
      best_threshold <- threshold
    }
  }
  
  return(best_threshold)
}

# Use the validation set to find the optimal threshold
optimal_threshold <- optimize_threshold(predict(final_model, validation_set, type = "response"), 
                                        validation_set[[target]])

# Apply the optimal threshold to the test set predictions
test_predictions_binary <- ifelse(test_predictions >= optimal_threshold, 1, 0)

# Save the predictions to a CSV file
predictions_df <- data.frame(ID = testing_data$ID, Prediction = test_predictions_binary)
write.csv(predictions_df, "test_predictions.csv", row.names = FALSE)

# Print the optimal threshold and predictions
cat("Optimal Threshold:", optimal_threshold, "\n")
print(head(predictions_df))
