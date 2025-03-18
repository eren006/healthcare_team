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
