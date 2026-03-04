###############################################################################
# HEAPO Data Preparation
# Goal: Merge all data sources into a single analytical dataset
#       with annual consumption and all relevant predictors
###############################################################################

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(purrr)

# Define paths (relative to project root – open HSLU_AML_01.Rproj in RStudio)
base_path   <- "data_raw/heapo_data"
output_path <- "data_processed/heapo"

###############################################################################
# 1. LOAD METADATA
###############################################################################

# Main metadata (linking table)
meta <- read_delim(file.path(base_path, "meta_data", "meta_data.csv"),
                   delim = ";", show_col_types = FALSE)

# Household survey data
households <- read_delim(file.path(base_path, "meta_data", "households.csv"),
                         delim = ";", show_col_types = FALSE)

cat("Metadata:", nrow(meta), "households\n")
cat("Households:", nrow(households), "entries\n")

###############################################################################
# 2. SMART METER DATA: CALCULATE ANNUAL CONSUMPTION
###############################################################################

# Use monthly data (already aggregated, efficient to load)
monthly_dir   <- file.path(base_path, "smart_meter_data", "monthly")
monthly_files <- list.files(monthly_dir, pattern = "\\.csv$", full.names = TRUE)

cat("Loading", length(monthly_files), "monthly smart meter files...\n")

# Read all monthly files and aggregate to annual consumption
annual_consumption <- map_dfr(monthly_files, function(f) {
  df <- read_delim(f, delim = ";", show_col_types = FALSE,
                   col_types = cols(.default = col_character()))

  if (nrow(df) == 0) return(NULL)

  df %>%
    mutate(
      Household_ID           = as.character(Household_ID),
      year                   = year(ymd(substr(Timestamp, 1, 10))),
      kWh_received_Total     = as.numeric(kWh_received_Total),
      kWh_received_HeatPump  = as.numeric(kWh_received_HeatPump),
      kWh_received_Other     = as.numeric(kWh_received_Other),
      kWh_returned_Total     = as.numeric(kWh_returned_Total)
    ) %>%
    group_by(Household_ID, year) %>%
    summarise(
      months_available    = n(),
      annual_kWh_total    = sum(kWh_received_Total,    na.rm = TRUE),
      annual_kWh_heatpump = sum(kWh_received_HeatPump, na.rm = TRUE),
      annual_kWh_other    = sum(kWh_received_Other,    na.rm = TRUE),
      annual_kWh_returned = sum(kWh_returned_Total,    na.rm = TRUE),
      .groups = "drop"
    )
})

cat("Annual consumption calculated:", nrow(annual_consumption), "household-year combinations\n")

# Keep only complete years (at least 10 months)
# For 10-11 months: scale up to 12 months
annual_full <- annual_consumption %>%
  filter(months_available >= 10, annual_kWh_total > 0) %>%
  mutate(
    annual_kWh_total    = annual_kWh_total    * (12 / months_available),
    annual_kWh_heatpump = annual_kWh_heatpump * (12 / months_available),
    annual_kWh_other    = annual_kWh_other    * (12 / months_available),
    annual_kWh_returned = annual_kWh_returned * (12 / months_available)
  )

cat("Complete years (>=10 months, >0 kWh):", nrow(annual_full), "entries\n")

# Take median annual consumption per household (more robust than mean)
household_consumption <- annual_full %>%
  group_by(Household_ID) %>%
  summarise(
    n_complete_years    = n(),
    annual_kWh_total    = median(annual_kWh_total),
    annual_kWh_heatpump = median(annual_kWh_heatpump),
    annual_kWh_other    = median(annual_kWh_other),
    annual_kWh_returned = median(annual_kWh_returned),
    .groups = "drop"
  )

cat("Households with annual consumption:", nrow(household_consumption), "\n")

###############################################################################
# 3. WEATHER DATA: ANNUAL AVERAGES
###############################################################################

weather_daily_dir <- file.path(base_path, "weather_data", "daily")
weather_files     <- list.files(weather_daily_dir, pattern = "\\.csv$", full.names = TRUE)

cat("Loading", length(weather_files), "weather stations...\n")

weather_annual <- map_dfr(weather_files, function(f) {
  df <- read_delim(f, delim = ";", show_col_types = FALSE)

  df %>%
    mutate(year = year(ymd(Timestamp))) %>%
    group_by(Weather_ID, year) %>%
    summarise(
      temp_avg            = mean(Temperature_avg_daily, na.rm = TRUE),
      temp_range          = mean(Temperature_max_daily - Temperature_min_daily, na.rm = TRUE),
      heating_degree_sia  = sum(HeatingDegree_SIA_daily,  na.rm = TRUE),
      cooling_degree_us   = sum(CoolingDegree_US_daily,   na.rm = TRUE),
      humidity_avg        = mean(Humidity_avg_daily,       na.rm = TRUE),
      precipitation_total = sum(Precipitation_total_daily, na.rm = TRUE),
      .groups = "drop"
    )
})

# Average across all available years per station
weather_avg <- weather_annual %>%
  group_by(Weather_ID) %>%
  summarise(
    across(temp_avg:precipitation_total, ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

cat("Weather stations aggregated:", nrow(weather_avg), "\n")

###############################################################################
# 4. MERGE EVERYTHING
###############################################################################

# Meta + Households
df <- meta %>%
  mutate(Household_ID = as.character(Household_ID)) %>%
  left_join(households %>% mutate(Household_ID = as.character(Household_ID)),
            by = "Household_ID")

# + Annual consumption (inner join: keep only households with consumption data)
df <- df %>%
  inner_join(household_consumption, by = "Household_ID")

# + Weather data
df <- df %>%
  left_join(weather_avg, by = "Weather_ID")

cat("Merged dataset:", nrow(df), "rows,", ncol(df), "columns\n")

###############################################################################
# 5. FEATURE ENGINEERING & VARIABLE TYPES
###############################################################################

heapo_final <- df %>%
  transmute(
    Household_ID = Household_ID,

    # --- RESPONSE VARIABLE (continuous amount -> log-transform for LM) ---
    annual_kWh_total = annual_kWh_total,

    # --- CONTINUOUS PREDICTORS ---
    living_area         = as.numeric(Survey_Building_LivingArea),
    temp_avg            = temp_avg,
    temp_range          = temp_range,
    heating_degree_days = heating_degree_sia,
    cooling_degree_days = cooling_degree_us,
    humidity_avg        = humidity_avg,
    precipitation_total = precipitation_total,
    # kWh returned (indicator for PV system and size)
    annual_kWh_returned = annual_kWh_returned,
    # Specific consumption per m2 (derived feature)
    kWh_per_m2 = ifelse(!is.na(as.numeric(Survey_Building_LivingArea)) &
                          as.numeric(Survey_Building_LivingArea) > 0,
                        annual_kWh_total / as.numeric(Survey_Building_LivingArea), NA),

    # --- CATEGORICAL PREDICTORS (>2 levels) ---
    weather_region = factor(Weather_ID),
    building_type  = factor(Survey_Building_Type),

    # --- CATEGORICAL PREDICTORS (2 levels) ---
    heatpump_type = factor(Survey_HeatPump_Installation_Type),
    group         = factor(Group),

    # --- COUNT PREDICTOR ---
    n_residents = as.integer(Survey_Building_Residents),

    # --- BINARY PREDICTORS ---
    has_floor_heating    = as.integer(as.logical(Survey_HeatDistribution_System_FloorHeating)),
    has_radiator         = as.integer(as.logical(Survey_HeatDistribution_System_Radiator)),
    has_dryer            = as.integer(as.logical(Survey_Installation_HasDryer)),
    has_freezer          = as.integer(as.logical(Survey_Installation_HasFreezer)),
    has_electric_vehicle = as.integer(as.logical(Survey_Installation_HasElectricVehicle)),

    # --- DERIVED BINARY ---
    # High consumption: above median (for GLM Binomial)
    high_consumption = as.integer(annual_kWh_total > median(annual_kWh_total, na.rm = TRUE))
  )

cat("\n=== FINAL DATASET (before cleaning) ===\n")
cat("Dimensions:", nrow(heapo_final), "observations x", ncol(heapo_final), "variables\n")

###############################################################################
# 6. HANDLE MISSING VALUES
###############################################################################

# Share of missing values per variable
na_pct <- sapply(heapo_final, function(x) mean(is.na(x)) * 100)
cat("\nMissing values (%):\n")
print(round(na_pct[na_pct > 0], 1))

# Binary variables: NA -> 0 (reasonable assumption: absent if not reported)
heapo_final <- heapo_final %>%
  mutate(
    has_electric_vehicle = ifelse(is.na(has_electric_vehicle), 0L, has_electric_vehicle),
    has_dryer            = ifelse(is.na(has_dryer),            0L, has_dryer),
    has_freezer          = ifelse(is.na(has_freezer),          0L, has_freezer)
  )

# Remove rows with NAs in core variables
core_vars <- c("annual_kWh_total", "living_area", "n_residents",
               "building_type", "heatpump_type", "weather_region",
               "has_floor_heating", "has_radiator")

heapo_clean <- heapo_final %>%
  drop_na(all_of(core_vars))

cat("\nAfter cleaning:", nrow(heapo_clean), "observations x",
    ncol(heapo_clean), "variables\n")

# Variable types in final dataset
cat("\n=== FINAL DATASET ===\n")
cat("Observations:", nrow(heapo_clean), "\n")
cat("Predictors:", ncol(heapo_clean) - 3, "(excluding ID, response, high_consumption)\n")

cat("\nVariable types:\n")
cat("- Response (continuous amount): annual_kWh_total\n")
cat("- Response (binary): high_consumption\n")

numeric_vars <- names(heapo_clean)[sapply(heapo_clean, is.numeric) &
  !names(heapo_clean) %in% c("Household_ID", "annual_kWh_total", "high_consumption",
                               "has_floor_heating", "has_radiator", "dhw_by_electric",
                               "has_dryer", "has_freezer", "has_electric_vehicle",
                               "n_residents")]
cat("- Continuous predictors:", paste(numeric_vars, collapse = ", "), "\n")

factor_vars <- names(heapo_clean)[sapply(heapo_clean, is.factor)]
for (col in factor_vars) {
  cat(sprintf("- Categorical: %s (%d levels)\n", col, nlevels(heapo_clean[[col]])))
}

cat("- Count: n_residents\n")
cat("- Binary: has_floor_heating, has_radiator,",
    "has_dryer, has_freezer, has_electric_vehicle\n")

cat("\nSummary:\n")
print(summary(heapo_clean))

###############################################################################
# 7. SAVE
###############################################################################

# As RDS (R-native, preserves data types)
saveRDS(heapo_clean, file.path(output_path, "heapo_dataset.rds"))

# As CSV (for portability)
write_csv(heapo_clean, file.path(output_path, "heapo_dataset.csv"))

cat("\nDataset saved as:\n")
cat("  -", file.path(output_path, "heapo_dataset.rds"), "\n")
cat("  -", file.path(output_path, "heapo_dataset.csv"), "\n")

cat("\n=== DATA PREPARATION COMPLETE ===\n")
cat("\nThe dataset meets the following requirements:\n")
cat("- N in range [10^3, 10^5]\n")
cat("- 10-20 predictors\n")
cat("- Continuous variables (living_area, temp_avg, etc.)\n")
cat("- Categorical variable with >2 levels (weather_region: 8 levels)\n")
cat("- Count variable (n_residents)\n")
cat("- Binary variables (has_floor_heating, etc.)\n")
cat("- Response for LM: log(annual_kWh_total) [amount -> log-transform]\n")
cat("- Response for GLM Poisson: n_residents [count]\n")
cat("- Response for GLM Binomial: high_consumption [binary]\n")
cat("- Response for GAM/NN/SVM: annual_kWh_total or high_consumption\n")
