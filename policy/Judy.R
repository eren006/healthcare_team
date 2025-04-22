
# -----------------------------
#question f

install.packages(c("readxl", "fixest", "dplyr"))

library(readxl)
library(dplyr)
library(fixest)  

df <- read_excel("policy/overtime_data-4.xlsx")

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

# -----------------------------
#question g

library(dplyr)
library(ggplot2)

#treated and controlled states
treated_states <- c("AK", "NH", "NY", "PA")

df <- df %>%
  mutate(group = ifelse(state %in% treated_states, "Treated", "Control"))

# calculate mean curhdef 
avg_def <- df %>%
  group_by(group, year) %>%
  summarise(mean_def = mean(curhdef, na.rm = TRUE), .groups = "drop")

avg_treatment_year <- 2009

ggplot(avg_def, aes(x = year, y = mean_def, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = avg_treatment_year, linetype = "dashed", color = "gray") +
  labs(
    title = "Mean Deficiency Citations: Treated vs. Control States",
    x = "Year",
    y = "Mean Deficiency Citations (curhdef)",
    color = "Group"
  ) +
  scale_color_manual(values = c("Treated" = "red", "Control" = "black")) +
  theme_minimal()

# -----------------------------
# Question h
# Difference-in-Differences with two-way fixed effects
# Outcome: Permanent RN Staffing (rn_permanent_hprd)

# Ensure the dataset is filtered for non-missing staffing outcome
df_h <- df %>%
  filter(!is.na(rn_permanent_hprd))

# Run the DiD regression on staffing outcome
staffing_model <- feols(
  rn_permanent_hprd ~ treated_post + beds + chain + hospital_aff + ownership_NFP + ownership_GOV | provnum + year,
  data = df_h,
  cluster = ~provnum
)

# Output regression summary
summary(staffing_model)

# Visualize average RN staffing trends over time by treatment group
avg_staffing <- df %>%
  group_by(group, year) %>%
  summarise(mean_staff = mean(rn_permanent_hprd, na.rm = TRUE), .groups = "drop")

ggplot(avg_staffing, aes(x = year, y = mean_staff, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "gray") +
  labs(
    title = "Mean RN Permanent Staffing: Treated vs. Control States",
    x = "Year",
    y = "Permanent RN Hours per Resident Day",
    color = "Group"
  ) +
  scale_color_manual(values = c("Treated" = "blue", "Control" = "black")) +
  theme_minimal()









