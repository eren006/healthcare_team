library(tidyverse)
library(readxl)
library(data.table) 
library(janitor)
library(writexl)

data <- read_excel("../healthcare_team/screening/screening_data.xls", sheet ='All Data')

clean_data <- data %>% clean_names()
write_xlsx(clean_data, "screening_goodnames.xlsx")

numerical_cols <- c('age', 'income', 'weight', 'height', 'bmi', 'waist', 'sbp', 'dbp', 'hdl', 'ldl', 'total_chol')

remove_outliers <- function(df, columns) {
  for (col in columns) {
    # Calculate Q1 (25th percentile) and Q3 (75th percentile)
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    
    # Calculate IQR
    IQR <- Q3 - Q1
    
    # Define lower and upper bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Filter out outliers
    df <- df[df[[col]] >= lower_bound & df[[col]] <= upper_bound, ]
  }
  
  return(df)
}

# Remove outliers
data_clean <- remove_outliers(data, numerical_cols)




# 加载必要的库
library(tidyverse)
library(randomForest)
library(caret)

# 确保分类变量是因子
training_data <- training_data %>% mutate_if(is.character, as.factor)
validation_data <- validation_data %>% mutate_if(is.character, as.factor)

training_data$ckd <- as.factor(training_data$ckd)
validation_data$ckd <- as.factor(validation_data$ckd)

# 定义因变量和预测变量
outcome_var <- "ckd"
predictors <- setdiff(names(training_data), outcome_var)

# 设定交叉验证
set.seed(42)
cv_folds <- trainControl(method = "cv", number = 5)  # 5折交叉验证

# 初始化变量
selected_features <- c()
remaining_features <- predictors
best_model <- NULL
best_score <- 0

# 前向特征选择 (Forward Selection)
for (i in 1:length(predictors)) {
  best_candidate <- NULL
  best_candidate_score <- 0
  
  for (feature in remaining_features) {
    current_features <- c(selected_features, feature)
    
    # 训练随机森林模型
    rf_model <- train(
      as.formula(paste(outcome_var, "~", paste(current_features, collapse = "+"))),
      data = training_data,
      method = "rf",
      trControl = cv_folds,
      metric = "Accuracy"
    )
    
    # 获取交叉验证分数
    mean_score <- max(rf_model$results$Accuracy)
    
    # 选择提升最大的特征
    if (mean_score > best_candidate_score) {
      best_candidate <- feature
      best_candidate_score <- mean_score
    }
  }
  
  # 如果找到提升的特征，加入最终集合
  if (!is.null(best_candidate) && best_candidate_score > best_score) {
    selected_features <- c(selected_features, best_candidate)
    remaining_features <- setdiff(remaining_features, best_candidate)
    best_score <- best_candidate_score
    best_model <- rf_model
  } else {
    break  # 如果没有特征能提升模型性能，则停止
  }
}

# 输出最终选择的特征
print("Selected Features:")
print(selected_features)

# 输出最佳模型的准确率
print("Best Model Accuracy:")
print(best_score)

# Load necessary libraries
library(tidyverse)
library(randomForest)
library(caret)

# Ensure categorical variables are factors
training_data <- training_data %>% mutate_if(is.character, as.factor)
validation_data <- validation_data %>% mutate_if(is.character, as.factor)

# Ensure target variable is a factor (for classification)
training_data$ckd <- as.factor(training_data$ckd)
validation_data$ckd <- as.factor(validation_data$ckd)

# Define the outcome variable and predictors
outcome_var <- "ckd"
predictors <- setdiff(names(training_data), outcome_var)

# Set up 10-fold cross-validation
set.seed(42)  # Set seed for reproducibility
cv_folds <- trainControl(method = "cv", number = 10, classProbs = TRUE)  # 10-fold CV

# Train the Random Forest model with 10-fold CV
rf_model <- train(
  as.formula(paste(outcome_var, "~", paste(predictors, collapse = "+"))),
  data = training_data,
  method = "rf",
  trControl = cv_folds,
  metric = "Accuracy",  # Classification task, use Accuracy
  tuneLength = 5  # Tune hyperparameters if necessary (e.g., mtry)
)

# Output the model results
print("Random Forest Model with 10-fold Cross Validation:")
print(rf_model)

# Output the accuracy of the model from cross-validation
print("Accuracy from 10-fold Cross Validation:")
print(rf_model$results$Accuracy)

# You can also visualize the results (feature importance)
print("Feature Importance:")
print(importance(rf_model$finalModel))

# Visualize variable importance
varImpPlot(rf_model$finalModel)














