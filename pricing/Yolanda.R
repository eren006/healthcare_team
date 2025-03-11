rm(list=ls())

library(readxl)
df <- read_excel("pricing/data.xlsx", sheet = "MH-Modified Data")

df <- df %>% clean_names()

#create dummy variable
df <- df %>% 
  mutate(is_Female = ifelse(trimws(gender) == "F", 1, 0)) %>%  
  select(-gender) 

df <- df %>%
  mutate(is_Unmarried = ifelse(trimws(marital_status) == "UNMARRIED", 1, 0)) %>%
  select(-marital_status)  

df <- df %>%
  select(-key_complaints_code)  

df <- df %>%
  select(-past_medical_history_code)  

df <- df %>%
  select(-mode_of_arrival)  

df <- df %>% select(-state_at_the_time_of_arrival)  

df <- df %>%
  select(-type_of_admsn) 

df <- df %>%
  select(-implant_used_y_n)

#question f
y <- df$total_cost_to_hospital
X <- df %>% select(-total_cost_to_hospital, -sl) 
X <- as.data.frame(scale(X))  
df_scaled <- cbind(y, X)    

#full model
full_model <- lm(y ~ ., data = df_scaled)
summary(full_model)

#refined regression model
significant_vars <- c("total_cost_to_hospital","cad_dvd", "other_tertalogy", "body_height", "diabetes2", "cost_of_implant")

df_significant <- df %>% select(all_of(significant_vars))

model_refined <- lm(total_cost_to_hospital ~ cad_dvd + other_tertalogy + body_height + diabetes2 + cost_of_implant, data = df_significant)

summary(model_refined)


