# Poisson GLM - data preparation

library(dplyr)

# 1) Paths
path_rds <- "data_processed/heapo/heapo_modelling.rds"

# 2) Load
dat <- readRDS(path_rds)

# 3) Quick check
str(dat)
dim(dat)
names(dat)
head(dat)

# Interpretation:
# n_high_days = number of days with high electricity consumption
# within a given month for each household.
# high_consumption is a daily indicator (1 = above median, 0 = otherwise).
# These daily indicators are summed within each household-month.
# The offset log(n_days) can later be used in the Poisson GLM
# to account for different month lengths and model a daily rate.

# 4) Create period (month)
dat <- dat %>%
  mutate(year_month = format(date, "%Y-%m"))

# 5) Aggregate to count per household-month
dat_count <- dat %>%
  group_by(Household_ID, year_month) %>%
  summarise(
    n_high_days = sum(high_consumption, na.rm = TRUE),
    n_days = n(),
    heating_degree_days = mean(heating_degree_days, na.rm = TRUE),
    temp_avg = mean(temp_avg, na.rm = TRUE),
    living_area = first(living_area),
    n_residents = first(n_residents),
    building_type = first(building_type),
    heatpump_type = first(heatpump_type),
    .groups = "drop"
  )

# 6) Check
head(dat_count)
summary(dat_count$n_high_days)

# 7) Poisson GLM
mod_pois <- glm(
  n_high_days ~ heating_degree_days + temp_avg + living_area +
    n_residents + building_type + heatpump_type,
  family = poisson(link = "log"),
  offset = log(n_days),
  data = dat_count
)

summary(mod_pois)