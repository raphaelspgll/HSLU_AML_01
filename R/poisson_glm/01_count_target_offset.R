# (1) Create count target + offset dataset for Poisson GLM
# Input : data_processed/heapo/heapo_modelling.rds
# Output: data_processed/poisson/dat_count.rds

suppressPackageStartupMessages({
  library(dplyr)
})

# -----------------------------
# Paths
# -----------------------------
path_in  <- "data_processed/heapo/heapo_modelling.rds"
path_out <- "data_processed/poisson/dat_count.rds"

# Create output directory if needed
dir_out <- dirname(path_out)
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# -----------------------------
# Load
# -----------------------------
if (!file.exists(path_in)) stop("Input file not found: ", path_in)
dat <- readRDS(path_in)

# -----------------------------
# Minimal validation
# -----------------------------
required_cols <- c(
  "Household_ID", "date", "high_consumption",
  "heating_degree_days", "temp_avg",
  "living_area", "n_residents",
  "building_type", "heatpump_type"
)
missing_cols <- setdiff(required_cols, names(dat))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

if (!inherits(dat$date, "Date")) {
  stop("Column `date` must be class Date. Current class: ", paste(class(dat$date), collapse = ", "))
}

# high_consumption should be 0/1 (allow NA)
bad_vals <- !is.na(dat$high_consumption) & !dat$high_consumption %in% c(0L, 1L)
if (any(bad_vals)) stop("`high_consumption` contains values other than 0/1/NA.")

# -----------------------------
# Create year-month key
# -----------------------------
dat <- dat %>%
  mutate(year_month = format(date, "%Y-%m"))

# -----------------------------
# Aggregate to household-month
# -----------------------------
dat_count <- dat %>%
  group_by(Household_ID, year_month) %>%
  summarise(
    # Count outcome: number of high-consumption days in month
    n_high_days = sum(high_consumption, na.rm = TRUE),
    
    # Exposure: number of observed days in that month
    n_days = n(),
    
    # Monthly weather summaries
    heating_degree_days = mean(heating_degree_days, na.rm = TRUE),
    temp_avg            = mean(temp_avg, na.rm = TRUE),
    
    # Household/static features (should be constant within household)
    living_area   = first(living_area),
    n_residents   = first(n_residents),
    building_type = first(building_type),
    heatpump_type = first(heatpump_type),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Offset for rate model (per day)
    log_n_days = log(n_days)
  )

# -----------------------------
# Sanity checks
# -----------------------------
if (any(dat_count$n_days <= 0)) stop("Found n_days <= 0 (impossible).")
if (any(dat_count$n_high_days < 0)) stop("Found n_high_days < 0 (impossible).")
if (any(dat_count$n_high_days > dat_count$n_days)) {
  stop("Found n_high_days > n_days. Check `high_consumption` coding and aggregation.")
}

# -----------------------------
# Save
# -----------------------------
saveRDS(dat_count, path_out)

# -----------------------------
# Minimal console output
# -----------------------------
cat("\n[01] Saved:", path_out, "\n")
cat("[01] Rows:", nrow(dat_count), "| Cols:", ncol(dat_count), "\n")
cat("[01] n_high_days summary:\n")
print(summary(dat_count$n_high_days))
cat("[01] share of zeros:", mean(dat_count$n_high_days == 0), "\n\n")