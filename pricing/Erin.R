
install.packages("tidyverse")
install.packages("janitor")
install.packages("corrplot")

library(tidyverse)
library(corrplot)
library(janitor)

library(readxl)


data <- read_excel("../healthcare_team/pricing/data.xlsx", sheet ='MH-Modified Data')

data <- data %>% clean_names()

set.seed(42)

#outliers

selected_cols <- c("age", "body_weight", "body_height", "hr_pulse", "rr", "hb",
                   "total_cost_to_hospital", "total_length_of_stay", 
                   "length_of_stay_icu", "length_of_stay_ward")

# Convert selected numeric columns to long format
data_long <- data %>%
  select(all_of(selected_cols)) %>%
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


# Select only the relevant columns for outlier detection
selected_cols <- c("age", "body_weight", "body_height", "hr_pulse", "rr", "hb",
                   "total_cost_to_hospital", "total_length_of_stay", 
                   "length_of_stay__icu", "length_of_stay__ward")


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
  select(all_of(selected_cols)) %>%
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
