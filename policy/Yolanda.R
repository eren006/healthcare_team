#question f

install.packages(c("readxl", "fixest", "dplyr"))

library(readxl)
library(dplyr)
library(fixest)  

df <- read_excel("C:/Users/25766/Desktop/多伦多/health care/assignment 4/overtime_data-4.xlsx")

# missing value
colSums(is.na(df))

df <- df %>% filter(!is.na(beds), !is.na(ownership))

# treated states
treated_states <- c("AK", "NH", "NY", "PA")
df <- df %>%
  mutate(treated = ifelse(state %in% treated_states, 1, 0))

# post 
law_years <- c("AK"=2010, "NH"=2008, "NY"=2009, "PA"=2009)
df <- df %>%
  mutate(
    post = case_when(
      state == "AK" & year >= 2010 ~ 1,
      state == "NH" & year >= 2008 ~ 1,
      state == "NY" & year >= 2009 ~ 1,
      state == "PA" & year >= 2009 ~ 1,
      TRUE ~ 0
    ),
    treated_post = treated * post
  )


# interaction
df <- df %>%
  mutate(treated_post = treated * post)

# ownership
df <- df %>%
  mutate(
    ownership_NFP = ifelse(ownership == "NFP", 1, 0),
    ownership_GOV = ifelse(ownership == "GOV", 1, 0)
  )

# panel regression
model <- feols(
  curhdef ~ treated_post + beds + chain + hospital_aff + ownership_NFP + ownership_GOV | provnum + year,
  data = df,
  cluster = ~provnum 
)

summary(model)


