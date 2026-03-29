# R/poisson_glm/03_fit_poisson.R
# (3) Fit Poisson GLM with offset
# Input : data_processed/poisson/dat_count.rds
# Output: models/poisson_glm/mod_pois.rds
#         models/poisson_glm/poisson_summary.txt

suppressPackageStartupMessages({
  library(dplyr)
})

# -----------------------------
# Paths
# -----------------------------
path_in   <- "data_processed/poisson/dat_count.rds"
model_dir <- "models/poisson_glm"
path_mod  <- file.path(model_dir, "mod_pois.rds")
path_txt  <- file.path(model_dir, "poisson_summary.txt")

if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)

# -----------------------------
# Load
# -----------------------------
if (!file.exists(path_in)) stop("Input file not found: ", path_in)
dat_count <- readRDS(path_in)

# -----------------------------
# Minimal checks
# -----------------------------
required_cols <- c(
  "n_high_days", "log_n_days",
  "heating_degree_days", "temp_avg", "living_area",
  "building_type", "heatpump_type",
  "has_floor_heating", "n_residents", "share_weekend"
)
missing_cols <- setdiff(required_cols, names(dat_count))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Ensure factors are factors (important for GLM interpretation)
dat_count <- dat_count %>%
  mutate(
    building_type = as.factor(building_type),
    heatpump_type = as.factor(heatpump_type),
    has_floor_heating = as.factor(has_floor_heating)  # treat as categorical 0/1
  )

# -----------------------------
# Model formula (all required predictors)
# -----------------------------
form <- n_high_days ~ heating_degree_days + temp_avg + living_area +
  building_type + heatpump_type + has_floor_heating +
  n_residents + share_weekend

# -----------------------------
# Fit Poisson GLM
# -----------------------------
mod_pois <- glm(
  formula = form,
  family  = poisson(link = "log"),
  offset  = log_n_days,
  data    = dat_count
)

# -----------------------------
# Save model
# -----------------------------
saveRDS(mod_pois, path_mod)

# -----------------------------
# Save summary text (readable output)
# -----------------------------
summ <- summary(mod_pois)

# Capture output for a clean text file
txt <- c(
  "Poisson GLM summary",
  "===================",
  "",
  paste0("Formula: ", deparse(form)),
  "Offset: log_n_days",
  "",
  "Call:",
  paste(capture.output(mod_pois$call), collapse = "\n"),
  "",
  "Coefficients (summary):",
  paste(capture.output(summ$coefficients), collapse = "\n"),
  "",
  paste0("Null deviance: ", round(summ$null.deviance, 3), " on ", summ$df.null, " df"),
  paste0("Residual deviance: ", round(summ$deviance, 3), " on ", summ$df.residual, " df"),
  paste0("AIC: ", round(summ$aic, 3)),
  ""
)

writeLines(txt, con = path_txt)

cat("\n[03] Saved model:", path_mod, "\n")
cat("[03] Saved summary:", path_txt, "\n\n")
print(summ)