
install.packages("tidyverse")
install.packages("janitor")
install.packages("corrplot")
install.packages("vcd")
install.packages("car")
install.packages("e1071")
install.packages("writexl") 

library(tidyverse)
library(corrplot)
library(janitor)
library(vcd)
library(readxl)
library(car)
library(e1071)
library(ggplot2)
library(writexl)


data <- read_excel("../healthcare_team/pricing/data.xlsx", sheet ='MH-Modified Data')

data <- data %>% clean_names()

set.seed(42)

#outliers

selected_cols <- c("age", "body_weight", "body_height", "hr_pulse", "rr", "hb",
                   "total_cost_to_hospital", "total_length_of_stay", 
                   "length_of_stay_icu", "length_of_stay_ward")

selected_cols_1 <- c("age", "body_weight", "body_height", "hr_pulse", "hb",
                   "total_cost_to_hospital", "total_length_of_stay", 
                   "length_of_stay_icu", "length_of_stay_ward")

# Convert selected numeric columns to long format
data_long <- data %>%
  select(all_of(selected_cols_1)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Compute IQR and determine outliers
data_long <- data_long %>%
  group_by(Variable) %>%
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    Outlier = ifelse(Value < lower_bound | Value > upper_bound, "Yes", "No")
  )

# Plot using ggplot2 (Only for selected columns)
ggplot(data_long, aes(x = Variable, y = Value, color = Outlier)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +  # Hide default outliers
  geom_jitter(width = 0.2, size = 2) +  # Add points
  scale_color_manual(values = c("No" = "black", "Yes" = "red")) +  # Color outliers red
  theme_minimal() +
  labs(title = "Outlier Detection for Selected Variables", color = "Outlier Status") +
  facet_wrap(~Variable, scales = "free")  # Separate plots for each variable




identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Apply the outlier detection to each column and create a logical matrix of outliers
outlier_flags <- data %>%
  select(all_of(selected_cols_1)) %>%
  map_dfr(~identify_outliers(.x)) %>%
  as.data.frame()

# Mark rows that have outliers in any of the selected columns
data$outlier_flag <- apply(outlier_flags, 1, function(x) any(x))

# Remove rows with outliers
data_cleaned <- data %>%
  filter(!outlier_flag) %>%
  select(-outlier_flag)  # Remove the outlier_flag column

# Print the cleaned dataset (without outliers)
print(data_cleaned)

# Plot boxplots after removing outliers
data_long_cleaned <- data_cleaned %>%
  select(all_of(selected_cols)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(data_long_cleaned, aes(x = Variable, y = Value)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.2, size = 2, color = "black") +
  theme_minimal() +
  labs(title = "Boxplots After Removing Outliers") +
  facet_wrap(~Variable, scales = "free")  # Separate plots for each variable

data_cleaned <- subset(data_cleaned, select=-c(implant_used_y_n))

#heat-map

# Separate the numeric columns from the categorical ones
numeric_data <- data %>%
  select(all_of(selected_cols))

# Identify categorical columns (all columns except numeric ones)
categorical_data <- data %>%
  select(-all_of(selected_cols))

# Calculate the correlation matrix for numeric variables
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs", method = "spearman")

# Visualize the correlation matrix with a heatmap
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45, 
         diag = FALSE)

# Fit a linear model using the numeric columns
lm_model <- lm(age ~ body_weight + body_height + hr_pulse + rr + hb + 
                 total_cost_to_hospital + total_length_of_stay + 
                 length_of_stay_icu + length_of_stay_ward, data = numeric_data)

# Check the VIF for each variable
vif(lm_model)

#if based on vif, remove total_length_of_stay,length_of_stay_icu, length_of_stay_ward


# Function to compute Cramér's V
calculate_cramers_v <- function(x, y) {
  tbl <- table(x, y)
  chisq_test <- chisq.test(tbl)
  cramer_v <- sqrt(chisq_test$statistic / (sum(tbl) * (min(dim(tbl)) - 1)))
  return(cramer_v)
}

# Compute Cramér's V for all categorical pairs
categorical_cols <- names(categorical_data)
cramers_v_matrix <- outer(categorical_cols, categorical_cols, 
                          Vectorize(function(x, y) calculate_cramers_v(categorical_data[[x]], categorical_data[[y]])))

# Convert to data frame for visualization
cramers_v_df <- as.data.frame(cramers_v_matrix)
rownames(cramers_v_df) <- categorical_cols
colnames(cramers_v_df) <- categorical_cols

# Print the matrix
print(cramers_v_df)

#skewness

# Compute skewness only for selected numeric columns
skewness_results <- sapply(data[selected_cols], skewness, na.rm = TRUE)

# Print results
print(skewness_results)


# Convert data to long format for ggplot
data_long <- data %>%
  select(all_of(selected_cols)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms with density overlay
ggplot(data_long, aes(x = Value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~Variable, scales = "free") +  # Separate plots per variable
  theme_minimal() +
  labs(title = "Histogram & Density Plots for Selected Variables")

write_xlsx(data_cleaned, "data_cleaned.xlsx")

# List of aliased variables to remove
aliased_vars <- c("cad_svd", "none_19", "none_31", "alert", "elective")

# Remove the aliased variables from the dataset
numeric_data_cleaned <- numeric_data[, !(names(numeric_data) %in% aliased_vars)]

data_q1 <- data_cleaned[, !(names(data_cleaned) %in% c("cad_svd", "none_19", "none_31", "alert", "elective"))]

#q1











