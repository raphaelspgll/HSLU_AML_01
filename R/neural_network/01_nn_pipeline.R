# ----------------------------------------
# (1) Load + basic preprocessing
# ----------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(caret)
})

# -----------------------------
# Paths
# -----------------------------
path_in <- "data_processed/heapo/heapo_modelling.rds"

# -----------------------------
# Load
# -----------------------------
if (!file.exists(path_in)) {
  stop("Input file not found: ", path_in)
}

dat <- readRDS(path_in)

cat("\n[01] Data loaded\n")
cat("[01] Rows:", nrow(dat), "| Cols:", ncol(dat), "\n")

# -----------------------------
# Keep only variables needed for NN
# -----------------------------
dat_nn <- dat %>%
  select(
    high_consumption,
    heating_degree_days, temp_avg,
    living_area, building_type, heatpump_type,
    has_floor_heating, n_residents, is_weekend
  ) %>%
  mutate(
    high_consumption   = as.factor(high_consumption),
    building_type      = as.factor(building_type),
    heatpump_type      = as.factor(heatpump_type),
    has_floor_heating  = as.factor(has_floor_heating),
    is_weekend         = as.factor(is_weekend)
  )

# Drop unused factor levels just in case
dat_nn$high_consumption <- droplevels(dat_nn$high_consumption)

cat("[01] Rows in NN dataset:", nrow(dat_nn), "\n")
cat("[01] Class balance:\n")
print(table(dat_nn$high_consumption))

# ----------------------------------------
# (2) Train / test split
# ----------------------------------------
# Goal:
# - Create a reproducible 80/20 split
# - Preserve class balance (stratified split)

if (nrow(dat_nn) == 0) {
  stop("NN dataset is empty.")
}

if (nlevels(dat_nn$high_consumption) < 2) {
  stop("high_consumption has fewer than 2 classes.")
}

set.seed(42)

idx_train <- createDataPartition(dat_nn$high_consumption, p = 0.8, list = FALSE)

train <- dat_nn[idx_train, ]
test  <- dat_nn[-idx_train, ]

cat("\n[02] Train/test split completed\n")
cat("[02] Train rows:", nrow(train), "\n")
cat("[02] Test rows :", nrow(test), "\n")

cat("\n[02] Class balance in full data:\n")
print(prop.table(table(dat_nn$high_consumption)))

cat("\n[02] Class balance in train:\n")
print(prop.table(table(train$high_consumption)))

cat("\n[02] Class balance in test:\n")
print(prop.table(table(test$high_consumption)))




# 3. Scaling
# 4. Fit NN
# 5. Predict
# 6. Evaluate (metrics + plots)
# 7. Save outputs (figures + results)