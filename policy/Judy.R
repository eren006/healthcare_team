# -----------------------------
# Load libraries
# -----------------------------
library(readxl)
library(dplyr)
library(fixest)
library(ggplot2)

df <- read_excel("policy/overtime_data-4.xlsx")
names(df)

# Define treatment and policy year variables

# Define treatment policy years by state
law_years <- c(AK = 2010, MO = 2006, NH = 2008, NY = 2009, PA = 2009)

# Add three separate indicators
df <- df %>%
  mutate(
    treated_state = ifelse(state %in% names(law_years), 1, 0),
    policy_year = ifelse(state %in% names(law_years), law_years[state], NA),
    treated_year = ifelse(!is.na(policy_year) & year >= policy_year, 1, 0),
    treated_post = treated_state * treated_year
  )

View(df)

# Filter for non-missing staffing outcome
df_h <- df %>%
  filter(!is.na(rn_permanent_hprd))

# Ensure ownership is a factor
df_h <- df_h %>%
  mutate(ownership = factor(ownership))

View(df_h)

# Run DiD model with two-way fixed effects
staffing_model <- feols(
  rn_permanent_hprd ~ treated_post + beds + chain + hospital_aff + ownership |
    provnum + year,
  data = df_h,
  cluster = ~provnum
)

# Print regression results
summary(staffing_model)

# -----------------------------
# Visualize average staffing trends
df_h <- df_h %>%
  mutate(group = ifelse(treated_state == 1, "Treated", "Control"))

avg_staffing <- df_h %>%
  group_by(group, year) %>%
  summarise(mean_staff = mean(rn_permanent_hprd, na.rm = TRUE), .groups = "drop")

ggplot(avg_staffing, aes(x = year, y = mean_staff, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Average Permanent RN Staffing (HPRD) Over Time",
    x = "Year",
    y = "Permanent RN Hours per Resident Day",
    color = "Group"
  ) +
  theme_minimal()

