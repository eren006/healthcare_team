# load in the data file into data.frame (you can use import as well)
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(stats)
library(dplyr)
library(tidyr)

# load in the data file into data.frame
df <- read_excel("/Users/judyfu/Desktop/A2/folder/pricing/data_cleaned.xlsx", sheet = 1)

# Inspect the data (additional)
str(df)

#take a look at the summary statistics
summary(df)

head(df)

rm(list=ls())

# **去除列名中的所有空格**
colnames(df) <- gsub(" ", "", colnames(df))  
print(colnames(df))

df <- df %>%  # 确保更新 df
  mutate(is_Female = ifelse(trimws(gender) == "F", 1, 0)) %>%  
  select(-gender) 

df <- df %>%
  mutate(is_Unmarried = ifelse(trimws(marital_status) == "UNMARRIED", 1, 0)) %>%
  select(-marital_status)  

df <- df %>%
  mutate(is_ACHD = ifelse(key_complaints_code == "ACHD", 1, 0),
         is_CAD_DVD = ifelse(key_complaints_code == "CAD-DVD", 1, 0),
         is_CAD_TVD = ifelse(key_complaints_code == "CAD-TVD", 1, 0),
         is_CAD_VSD = ifelse(key_complaints_code == "CAD-VSD", 1, 0),
         is_OS_ASD = ifelse(key_complaints_code == "OS-ASD", 1, 0),
         is_other_heart = ifelse(key_complaints_code == "other- heart", 1, 0),
         is_other_respiratory = ifelse(key_complaints_code == "other- respiratory", 1, 0),
         is_other_general = ifelse(key_complaints_code == "other-general", 1, 0),
         is_other_nervous = ifelse(key_complaints_code == "other-nervous", 1, 0),
         is_other_tertalogy = ifelse(key_complaints_code == "other-tertalogy", 1, 0),
         is_PM_VSD = ifelse(key_complaints_code == "PM-VSD", 1, 0),
         is_RHD = ifelse(key_complaints_code == "RHD", 1, 0)) %>%
  select(-key_complaints_code)  

df <- df %>%
  mutate(is_Diabetes1 = ifelse(past_medical_history_code == "Diabetes1", 1, 0),
         is_Diabetes2 = ifelse(past_medical_history_code == "Diabetes2", 1, 0),
         is_Hypertension1 = ifelse(past_medical_history_code == "hypertension1", 1, 0),
         is_Hypertension2 = ifelse(past_medical_history_code == "hypertension2", 1, 0),
         is_Hypertension3 = ifelse(past_medical_history_code == "hypertension3", 1, 0),
         is_Other = ifelse(past_medical_history_code == "other", 1, 0)) %>% 
  select(-past_medical_history_code)  # 删除原始变量

df <- df %>%
  mutate(is_Ambulance = ifelse(mode_of_arrival == "AMBULANCE", 1, 0),
         is_Transferred = ifelse(mode_of_arrival == "TRANSFERRED", 1, 0)) %>% 
  select(-mode_of_arrival)  # 删除原始变量

#delete "state_at_the_time_of_arrival" 
df <- df %>% select(-state_at_the_time_of_arrival)  

df <- df %>%
  mutate(is_Emergency = ifelse(trimws(type_of_admsn) == "EMERGENCY", 1, 0)) %>%
  select(-type_of_admsn) 

df <- df %>%
  mutate(is_implant = ifelse(trimws(implant_used_y_n) == "Y", 1, 0)) %>%
  select(-implant_used_y_n)
