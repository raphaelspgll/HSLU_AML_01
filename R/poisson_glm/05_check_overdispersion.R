# (5) Check overdispersion for Poisson GLM
# Input : models/poisson_glm/mod_pois.rds
# Output: models/poisson_glm/overdispersion.txt

# -----------------------------
# Paths
# -----------------------------
model_dir <- "models/poisson_glm"
path_mod  <- file.path(model_dir, "mod_pois.rds")
path_out  <- file.path(model_dir, "overdispersion.txt")

if (!file.exists(path_mod)) stop("Model file not found: ", path_mod)

# -----------------------------
# Load model + compute dispersion
# -----------------------------
mod_pois <- readRDS(path_mod)
summ <- summary(mod_pois)

res_dev <- summ$deviance
df_res  <- summ$df.residual
dispersion <- res_dev / df_res

# Rule-of-thumb interpretation
flag <- if (dispersion > 1.5) "OVERDISPERSION likely (consider quasi-Poisson / NB)." else "Dispersion looks acceptable."

txt <- c(
  "Overdispersion check (Poisson GLM)",
  "==================================",
  paste0("Residual deviance: ", round(res_dev, 3)),
  paste0("Residual df      : ", df_res),
  paste0("Dispersion (dev/df): ", round(dispersion, 4)),
  "",
  paste0("Interpretation: ", flag),
  ""
)

writeLines(txt, con = path_out)

cat("\n[05] Saved overdispersion check:", path_out, "\n")
cat("[05] Dispersion (dev/df):", round(dispersion, 4), "\n")
cat("[05] ", flag, "\n\n", sep = "")