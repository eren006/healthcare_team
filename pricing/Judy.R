# load in the data file into data.frame (you can use import as well)
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(stats)
library(dplyr)
library(tidyr)

# load in the data file into data.frame
data <- read_excel("/Users/judyfu/Desktop/A2/healthcare_team/pricing/data.xlsx", sheet = "MH-Modified Data")

# Inspect the data (additional)
str(data)

#take a look at the summary statistics
summary(data)

head(data)

