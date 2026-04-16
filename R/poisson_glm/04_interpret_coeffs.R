# R/poisson_glm/04_interpret_coeffs.R
# (4) Interpret coefficients as rate ratios (exp(beta)) + 95% CI
# Input : models/poisson_glm/mod_pois.rds
# Output: models/poisson_glm/coeff_rate_ratios.csv

suppressPackageStartupMessages({
  library(dplyr)
})

# -----------------------------
# Paths
# -----------------------------
model_dir <- "models/poisson_glm"
path_mod  <- file.path(model_dir, "mod_pois.rds")
path_out  <- file.path(model_dir, "coeff_rate_ratios.csv")

if (!file.exists(path_mod)) stop("Model file not found: ", path_mod)

# -----------------------------
# Load model
# -----------------------------
mod_pois <- readRDS(path_mod)
summ <- summary(mod_pois)

# -----------------------------
# Extract coefficients + compute rate ratios
# -----------------------------
coef_mat <- summ$coefficients
# columns: Estimate, Std. Error, z value, Pr(>|z|)
est <- coef_mat[, "Estimate"]
se  <- coef_mat[, "Std. Error"]

# Wald 95% CI on log scale
alpha <- 0.05
zcrit <- qnorm(1 - alpha/2)

ci_low  <- est - zcrit * se
ci_high <- est + zcrit * se

out <- data.frame(
  term         = rownames(coef_mat),
  estimate     = as.numeric(est),
  std_error    = as.numeric(se),
  z_value      = as.numeric(coef_mat[, "z value"]),
  p_value      = as.numeric(coef_mat[, "Pr(>|z|)"]),
  rate_ratio   = exp(est),
  rr_ci_low    = exp(ci_low),
  rr_ci_high   = exp(ci_high),
  row.names = NULL
)

# Optional: nicer rounding for readability in CSV
out <- out %>%
  mutate(
    estimate   = round(estimate, 6),
    std_error  = round(std_error, 6),
    z_value    = round(z_value, 3),
    p_value    = signif(p_value, 4),
    rate_ratio = round(rate_ratio, 4),
    rr_ci_low  = round(rr_ci_low, 4),
    rr_ci_high = round(rr_ci_high, 4)
  )

# -----------------------------
# Save
# -----------------------------
write.csv(out, path_out, row.names = FALSE)

cat("\n[04] Saved coefficient interpretation table:", path_out, "\n\n")
print(out)