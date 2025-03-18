rm(list=ls())

library(readxl)
df <- read.csv("screening/train.csv")

#transform categorical vairable into dummy variable
install.packages("fastDummies")
library(fastDummies)
df <- dummy_cols(df, select_columns = "racegrp", remove_first_dummy = TRUE)
df <- dummy_cols(df, select_columns = "care_source", remove_first_dummy = TRUE)
df <- df %>% select(-racegrp)
df <- df %>% select(-care_source)


