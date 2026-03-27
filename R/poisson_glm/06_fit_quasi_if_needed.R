# (6) Fit quasi-Poisson (same predictors + offset) and compare SE/p-values vs Poisson
# Input : data_processed/poisson/dat_count.rds
#         models/poisson_glm/mod_pois.rds
# Output: models/poisson_glm/mod_quasi.rds
#         models/poisson_glm/quasi_vs_pois_se_pvals.csv
#         models/poisson_glm/quasi_summary.txt

suppressPackageStartupMessages({
  library(dplyr)
})

# -----------------------------
# Paths
# -----------------------------
path_data <- "data_processed/poisson/dat_count.rds"
model_dir <- "models/poisson_glm"
path_pois <- file.path(model_dir, "mod_pois.rds")
path_quas <- file.path(model_dir, "mod_quasi.rds")
path_csv  <- file.path(model_dir, "quasi_vs_pois_se_pvals.csv")
path_txt  <- file.path(model_dir, "quasi_summary.txt")

if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)

if (!file.exists(path_data)) stop("Data file not found: ", path_data)
if (!file.exists(path_pois)) stop("Poisson model file not found: ", path_pois)

# -----------------------------
# Load
# -----------------------------
dat_count <- readRDS(path_data)
mod_pois  <- readRDS(path_pois)

# Ensure factor types match (important)
dat_count <- dat_count %>%
  mutate(
    building_type = as.factor(building_type),
    heatpump_type = as.factor(heatpump_type),
    has_floor_heating = as.factor(has_floor_heating)
  )

# Same formula as Poisson model
form <- formula(mod_pois)

# -----------------------------
# Fit quasi-Poisson
# -----------------------------
mod_quasi <- glm(
  formula = form,
  family  = quasipoisson(link = "log"),
  offset  = log_n_days,
  data    = dat_count
)

saveRDS(mod_quasi, path_quas)

summ_p <- summary(mod_pois)
summ_q <- summary(mod_quasi)

# -----------------------------
# Compare SE and p-values
# -----------------------------
coef_p <- summ_p$coefficients
coef_q <- summ_q$coefficients

terms_all <- union(rownames(coef_p), rownames(coef_q))

get_col <- function(mat, term, colname) {
  if (!term %in% rownames(mat)) return(NA_real_)
  as.numeric(mat[term, colname])
}

comp <- data.frame(
  term = terms_all,
  estimate_pois = vapply(terms_all, \(t) get_col(coef_p, t, "Estimate"), numeric(1)),
  se_pois       = vapply(terms_all, \(t) get_col(coef_p, t, "Std. Error"), numeric(1)),
  p_pois        = vapply(terms_all, \(t) get_col(coef_p, t, "Pr(>|z|)"), numeric(1)),
  estimate_quasi = vapply(terms_all, \(t) get_col(coef_q, t, "Estimate"), numeric(1)),
  se_quasi       = vapply(terms_all, \(t) get_col(coef_q, t, "Std. Error"), numeric(1)),
  p_quasi        = vapply(terms_all, \(t) get_col(coef_q, t, "Pr(>|t|)"), numeric(1)),
  row.names = NULL
) %>%
  mutate(
    # tidy rounding for readability
    estimate_pois  = round(estimate_pois, 6),
    se_pois        = round(se_pois, 6),
    p_pois         = signif(p_pois, 4),
    estimate_quasi = round(estimate_quasi, 6),
    se_quasi       = round(se_quasi, 6),
    p_quasi        = signif(p_quasi, 4)
  )

write.csv(comp, path_csv, row.names = FALSE)

# -----------------------------
# Save quasi summary text
# -----------------------------
disp_q <- summ_q$dispersion

txt <- c(
  "Quasi-Poisson GLM summary",
  "=========================",
  "",
  paste0("Formula: ", deparse(form)),
  "Offset: log_n_days",
  "",
  paste0("Estimated dispersion (phi): ", round(disp_q, 4)),
  "",
  "Coefficients (quasi):",
  paste(capture.output(summ_q$coefficients), collapse = "\n"),
  ""
)

writeLines(txt, con = path_txt)

cat("\n[06] Saved quasi model:", path_quas, "\n")
cat("[06] Saved quasi summary:", path_txt, "\n")
cat("[06] Saved comparison CSV:", path_csv, "\n")
cat("[06] Quasi dispersion (phi):", round(disp_q, 4), "\n\n")

print(head(comp, 12))