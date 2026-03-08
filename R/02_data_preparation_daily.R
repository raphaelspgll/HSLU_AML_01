###############################################################################
# HEAPO Data Preparation – Daily Level
# Goal: Merge all data sources into a single analytical dataset
#       with DAILY consumption and all relevant predictors
#       Output: one row per household x day
###############################################################################

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(purrr)
library(here)

# Define paths (relative to project root – open HSLU_AML_01.Rproj in RStudio)
base_path <- here("data_raw", "heapo_data")
output_path <- here("data_processed", "heapo")

###############################################################################
# 1. LOAD METADATA
###############################################################################

meta <- read_delim(
  file.path(base_path, "meta_data", "meta_data.csv"),
  delim = ";",
  show_col_types = FALSE
) %>%
  mutate(Household_ID = as.character(Household_ID))

households <- read_delim(
  file.path(base_path, "meta_data", "households.csv"),
  delim = ";",
  show_col_types = FALSE
) %>%
  mutate(Household_ID = as.character(Household_ID))

# Merge static household features
household_meta <- meta %>%
  left_join(households, by = "Household_ID") %>%
  transmute(
    Household_ID,
    weather_region = Weather_ID,
    group = factor(Group),
    building_type = factor(Survey_Building_Type),
    heatpump_type = factor(Survey_HeatPump_Installation_Type),
    living_area = as.numeric(Survey_Building_LivingArea),
    n_residents = as.integer(Survey_Building_Residents),
    has_floor_heating = as.integer(as.logical(
      Survey_HeatDistribution_System_FloorHeating
    )),
    has_radiator = as.integer(as.logical(
      Survey_HeatDistribution_System_Radiator
    )),
    has_dryer = as.integer(as.logical(Survey_Installation_HasDryer)),
    has_freezer = as.integer(as.logical(Survey_Installation_HasFreezer)),
    has_electric_vehicle = as.integer(as.logical(
      Survey_Installation_HasElectricVehicle
    ))
  ) %>%
  # Binary NAs -> 0
  mutate(
    has_electric_vehicle = ifelse(
      is.na(has_electric_vehicle),
      0L,
      has_electric_vehicle
    ),
    has_dryer = ifelse(is.na(has_dryer), 0L, has_dryer),
    has_freezer = ifelse(is.na(has_freezer), 0L, has_freezer)
  )

cat("Households with metadata:", nrow(household_meta), "\n")

###############################################################################
# 2. LOAD DAILY SMART METER DATA
###############################################################################

daily_dir <- file.path(base_path, "smart_meter_data", "daily")
daily_files <- list.files(daily_dir, pattern = "\\.csv$", full.names = TRUE)

cat("Loading", length(daily_files), "daily smart meter files...\n")

daily_consumption <- map_dfr(daily_files, function(f) {
  df <- read_delim(
    f,
    delim = ";",
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )

  if (nrow(df) == 0) {
    return(NULL)
  }

  df %>%
    transmute(
      Household_ID = as.character(Household_ID),
      date = as.Date(substr(Timestamp, 1, 10)),
      kWh_total = as.numeric(kWh_received_Total),
      kWh_heatpump = as.numeric(kWh_received_HeatPump),
      kWh_other = as.numeric(kWh_received_Other),
      kWh_returned = as.numeric(kWh_returned_Total)
    ) %>%
    filter(!is.na(date), !is.na(kWh_total), kWh_total >= 0)
})

cat("Daily entries loaded:", nrow(daily_consumption), "\n")
cat("Households:", n_distinct(daily_consumption$Household_ID), "\n")
cat(
  "Time period:",
  as.character(min(daily_consumption$date)),
  "to",
  as.character(max(daily_consumption$date)),
  "\n"
)

###############################################################################
# 3. LOAD DAILY WEATHER DATA
###############################################################################

weather_daily_dir <- file.path(base_path, "weather_data", "daily")
weather_files <- list.files(
  weather_daily_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

cat("\nLoading", length(weather_files), "weather stations...\n")

weather_daily <- map_dfr(weather_files, function(f) {
  read_delim(f, delim = ";", show_col_types = FALSE) %>%
    transmute(
      weather_region = Weather_ID,
      date = as.Date(Timestamp),
      temp_max = Temperature_max_daily,
      temp_min = Temperature_min_daily,
      temp_avg = Temperature_avg_daily,
      temp_range = Temperature_max_daily - Temperature_min_daily,
      heating_degree_days = HeatingDegree_SIA_daily,
      cooling_degree_days = CoolingDegree_US_daily,
      humidity_avg = Humidity_avg_daily,
      precipitation = Precipitation_total_daily,
      sunshine_hours = Sunshine_duration_daily
    )
})

cat("Weather entries loaded:", nrow(weather_daily), "\n")

###############################################################################
# 4. MERGE EVERYTHING
###############################################################################

# Step 1: Smart meter + household metadata
df <- daily_consumption %>%
  inner_join(household_meta, by = "Household_ID")

cat("\nAfter join with metadata:", nrow(df), "rows\n")

# Step 2: + weather data (by region and date)
df <- df %>%
  left_join(weather_daily, by = c("weather_region", "date"))

cat("After join with weather data:", nrow(df), "rows\n")

###############################################################################
# 5. FEATURE ENGINEERING
###############################################################################

# Derive time variables
df <- df %>%
  mutate(
    year = year(date),
    month = month(date),
    weekday = wday(date, label = FALSE, week_start = 1),
    is_weekend = as.integer(weekday >= 6),
    # kWh per m2 (daily)
    kWh_per_m2 = ifelse(
      !is.na(living_area) & living_area > 0,
      kWh_total / living_area,
      NA
    )
  )

###############################################################################
# 6. FILTER CORE VARIABLES & CLEAN
###############################################################################

core_vars <- c(
  "kWh_total",
  "living_area",
  "n_residents",
  "building_type",
  "heatpump_type",
  "weather_region",
  "has_floor_heating",
  "has_radiator",
  "temp_avg"
)

heapo_daily <- df %>%
  drop_na(all_of(core_vars)) %>%
  select(
    Household_ID,
    date,
    year,
    month,
    weekday,
    is_weekend,
    # Response
    kWh_total,
    kWh_heatpump,
    kWh_other,
    kWh_returned,
    kWh_per_m2,
    # Weather (daily)
    temp_avg,
    temp_min,
    temp_max,
    temp_range,
    heating_degree_days,
    cooling_degree_days,
    humidity_avg,
    precipitation,
    sunshine_hours,
    # Household (static)
    weather_region,
    group,
    building_type,
    heatpump_type,
    living_area,
    n_residents,
    has_floor_heating,
    has_radiator,
    has_dryer,
    has_freezer,
    has_electric_vehicle
  )

cat("\n=== FINAL DATASET ===\n")
cat("Observations:", nrow(heapo_daily), "\n")
cat("Variables:", ncol(heapo_daily), "\n")
cat("Households:", n_distinct(heapo_daily$Household_ID), "\n")
cat(
  "Time period:",
  as.character(min(heapo_daily$date)),
  "to",
  as.character(max(heapo_daily$date)),
  "\n"
)

###############################################################################
# 7. SAVE
###############################################################################

if (!dir.exists(output_path)) {
  dir.create(output_path)
}

saveRDS(heapo_daily, file.path(output_path, "heapo_dataset_daily.rds"))
write_csv(heapo_daily, file.path(output_path, "heapo_dataset_daily.csv"))

cat("\nDataset saved as:\n")
cat("  -", file.path(output_path, "heapo_dataset_daily.rds"), "\n")
cat("  -", file.path(output_path, "heapo_dataset_daily.csv"), "\n")
cat("\n=== DATA PREPARATION COMPLETE ===\n")
