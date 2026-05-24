# ============================================================
# Poisson GLM Pipeline — HSLU AML
# ============================================================
# Input:  data_processed/heapo/heapo_modelling.rds
# Output: data_processed/poisson/dat_count.rds
#         models/poisson_glm/mod_pois.rds
#         models/poisson_glm/mod_quasi.rds
#         models/poisson_glm/coeff_rate_ratios.csv
#         models/poisson_glm/dispersion_val.rds
#         models/poisson_glm/quasi_vs_pois_se_pvals.csv
# ============================================================

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(here)
})

# ------------------------------------------------------------
# Paths  (all defined once here — edit only this section)
# ------------------------------------------------------------
path_in       <- here("data_processed/heapo/heapo_modelling.rds")
path_count    <- here("data_processed/poisson/dat_count.rds")
path_mod_pois <- here("models/poisson_glm/mod_pois.rds")
path_mod_quas <- here("models/poisson_glm/mod_quasi.rds")
path_rr       <- here("models/poisson_glm/coeff_rate_ratios.csv")
path_disp     <- here("models/poisson_glm/dispersion_val.rds")
path_comp     <- here("models/poisson_glm/quasi_vs_pois_se_pvals.csv")

# Create output directories if missing
dir.create(dirname(path_count),    recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(path_mod_pois), recursive = TRUE, showWarnings = FALSE)

# ============================================================
# (1) Load + validate raw data
# ============================================================
if (!file.exists(path_in)) stop("Input file not found: ", path_in)

dat <- readRDS(path_in)
cat("\n[01] Data loaded —", nrow(dat), "rows,", ncol(dat), "cols\n")

required_cols <- c(
  "Household_ID", "date", "high_consumption",
  "heating_degree_days", "temp_avg",
  "living_area", "building_type", "heatpump_type",
  "has_floor_heating", "n_residents", "is_weekend"
)
missing_cols <- setdiff(required_cols, names(dat))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}
if (!inherits(dat$date, "Date")) {
  stop("`date` must be class Date. Found: ", paste(class(dat$date), collapse = ", "))
}

cat("[01] Validation passed\n")

# ============================================================
# (2) Aggregate to household-month count data
# ============================================================
dat_count <- dat %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  group_by(Household_ID, year_month) %>%
  summarise(
    n_high_days         = sum(high_consumption, na.rm = TRUE),
    n_days              = n(),
    heating_degree_days = mean(heating_degree_days, na.rm = TRUE),
    temp_avg            = mean(temp_avg, na.rm = TRUE),
    share_weekend       = mean(is_weekend, na.rm = TRUE),
    living_area         = first(living_area),
    building_type       = first(building_type),
    heatpump_type       = first(heatpump_type),
    has_floor_heating   = first(has_floor_heating),
    n_residents         = first(n_residents),
    .groups = "drop"
  ) %>%
  mutate(
    log_n_days        = log(n_days),
    # Factor conversion done once here for all downstream steps
    building_type     = as.factor(building_type),
    heatpump_type     = as.factor(heatpump_type),
    has_floor_heating = as.factor(has_floor_heating)
  )

# Sanity checks
if (any(dat_count$n_days <= 0))                       stop("Found n_days <= 0.")
if (any(dat_count$n_high_days < 0))                   stop("Found n_high_days < 0.")
if (any(dat_count$n_high_days > dat_count$n_days))    stop("n_high_days > n_days.")
if (any(is.na(dat_count$share_weekend)))              stop("share_weekend has NAs.")

saveRDS(dat_count, path_count)
cat("\n[02] Aggregated to", nrow(dat_count), "household-month rows\n")
cat("[02] Share of zero counts:", round(mean(dat_count$n_high_days == 0), 4), "\n")
cat("[02] Saved dat_count to:", path_count, "\n")

# ============================================================
# (3) Fit Poisson GLM  (skip if model already saved)
# ============================================================
form <- n_high_days ~ heating_degree_days + temp_avg + living_area +
  building_type + heatpump_type + has_floor_heating +
  n_residents + share_weekend

if (file.exists(path_mod_pois)) {
  
  mod_pois <- readRDS(path_mod_pois)
  cat("\n[03] Loaded existing Poisson model from:", path_mod_pois, "\n")
  
} else {
  
  mod_pois <- glm(
    formula = form,
    family  = poisson(link = "log"),
    offset  = log_n_days,
    data    = dat_count
  )
  
  saveRDS(mod_pois, path_mod_pois)
  cat("\n[03] Poisson GLM fitted and saved to:", path_mod_pois, "\n")
}

summ_p <- summary(mod_pois)
cat("[03] Residual deviance:", round(summ_p$deviance, 2),
    "on", summ_p$df.residual, "df | AIC:", round(summ_p$aic, 2), "\n")

# ============================================================
# (4) Coefficient table: rate ratios + 95% CI
# ============================================================
coef_mat <- summ_p$coefficients
est      <- coef_mat[, "Estimate"]
se       <- coef_mat[, "Std. Error"]
zcrit    <- qnorm(0.975)

rr_table <- data.frame(
  term       = rownames(coef_mat),
  estimate   = round(as.numeric(est), 6),
  std_error  = round(as.numeric(se), 6),
  z_value    = round(as.numeric(coef_mat[, "z value"]), 3),
  p_value    = signif(as.numeric(coef_mat[, "Pr(>|z|)"]), 4),
  rate_ratio = round(exp(est), 4),
  rr_ci_low  = round(exp(est - zcrit * se), 4),
  rr_ci_high = round(exp(est + zcrit * se), 4),
  row.names  = NULL
)

write.csv(rr_table, path_rr, row.names = FALSE)
cat("\n[04] Coefficient rate-ratio table saved to:", path_rr, "\n")

# ============================================================
# (5) Overdispersion check
# ============================================================
dispersion <- summ_p$deviance / summ_p$df.residual
flag <- if (dispersion > 1.5) "OVERDISPERSION likely — quasi-Poisson recommended." else "Dispersion acceptable."

saveRDS(dispersion, path_disp)
cat("\n[05] Dispersion (deviance / df):", round(dispersion, 4), "\n")
cat("[05]", flag, "\n")
cat("[05] Saved dispersion value to:", path_disp, "\n")

# ============================================================
# (6) Fit quasi-Poisson + compare SE / p-values
# ============================================================
mod_quasi <- glm(
  formula = form,
  family  = quasipoisson(link = "log"),
  offset  = log_n_days,
  data    = dat_count
)

saveRDS(mod_quasi, path_mod_quas)
cat("\n[06] Quasi-Poisson fitted and saved to:", path_mod_quas, "\n")

summ_q    <- summary(mod_quasi)
coef_q    <- summ_q$coefficients
terms_all <- union(rownames(coef_mat), rownames(coef_q))

get_col <- function(mat, term, colname) {
  if (!term %in% rownames(mat)) return(NA_real_)
  as.numeric(mat[term, colname])
}

comp <- data.frame(
  term           = terms_all,
  estimate_pois  = round(vapply(terms_all, \(t) get_col(coef_mat, t, "Estimate"),   numeric(1)), 6),
  se_pois        = round(vapply(terms_all, \(t) get_col(coef_mat, t, "Std. Error"), numeric(1)), 6),
  p_pois         = signif(vapply(terms_all, \(t) get_col(coef_mat, t, "Pr(>|z|)"), numeric(1)), 4),
  estimate_quasi = round(vapply(terms_all, \(t) get_col(coef_q,   t, "Estimate"),   numeric(1)), 6),
  se_quasi       = round(vapply(terms_all, \(t) get_col(coef_q,   t, "Std. Error"), numeric(1)), 6),
  p_quasi        = signif(vapply(terms_all, \(t) get_col(coef_q,   t, "Pr(>|t|)"), numeric(1)), 4),
  row.names = NULL
)

write.csv(comp, path_comp, row.names = FALSE)
cat("[06] Quasi dispersion (phi):", round(summ_q$dispersion, 4), "\n")
cat("[06] SE / p-value comparison saved to:", path_comp, "\n")

# ============================================================
cat("\n[07] Pipeline complete.\n")
# ============================================================