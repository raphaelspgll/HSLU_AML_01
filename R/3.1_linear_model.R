# ============================================================
# Linear Model Pipeline — HSLU AML
# ============================================================
# Input:  data_processed/heapo/heapo_modelling.rds
# Output: models/linear_model/heapo_mlm_simple.rds
#         models/linear_model/heapo_lm.rds
# ============================================================

# ------------------------------------------------------------
# Libraries
# caret is loaded later (before CV only) so it does not pollute the search
# path when the model objects are serialised — caret loads lubridate as a
# dependency, and any lm object saved with lubridate on the search path
# will fail to load in an R session where lubridate is not available.
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tibble)
  library(broom)
  library(car)
  library(ggplot2)
})

set.seed(42)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
heapo_modelling <- readRDS(here("data_processed", "heapo", "heapo_modelling.rds"))

# ------------------------------------------------------------
# Univariate models
# ------------------------------------------------------------
predictors <- c(
  "heating_degree_days",
  "sunshine_hours",
  "humidity_avg",
  "living_area",
  "n_residents",
  "building_type",
  "heatpump_type"
)

univariate <- lapply(predictors, function(p) {
  lm(as.formula(paste("log_kWh_total ~", p)), data = heapo_modelling) |>
    broom::glance() |>
    mutate(predictor = p)
}) |>
  bind_rows() |>
  select(predictor, r.squared, adj.r.squared, p.value) |>
  arrange(desc(r.squared))

print(univariate)

# ------------------------------------------------------------
# Simple MLR (main effects only — baseline)
# ------------------------------------------------------------
heapo_mlm_simple <- lm(
  log_kWh_total ~ heating_degree_days +
    sunshine_hours +
    humidity_avg +
    living_area +
    n_residents +
    building_type +
    heatpump_type,
  data = heapo_modelling
)

summary(heapo_mlm_simple)

# ------------------------------------------------------------
# Extended MLR (+ interaction terms)
# ------------------------------------------------------------
heapo_lm <- lm(
  log_kWh_total ~ heating_degree_days * building_type +
    heating_degree_days * heatpump_type +
    sunshine_hours +
    humidity_avg +
    living_area +
    n_residents,
  data = heapo_modelling
)

summary(heapo_lm)

# ------------------------------------------------------------
# Model diagnostics
# ------------------------------------------------------------
par(mfrow = c(2, 2))
plot(heapo_lm, main = "Extended MLR Diagnostics")
par(mfrow = c(1, 1))

vif_vals <- car::vif(heapo_lm)
print(data.frame(Predictor = names(vif_vals), VIF = round(as.numeric(vif_vals), 2)))

# ------------------------------------------------------------
# Coefficient plot
# ------------------------------------------------------------
cis <- confint(heapo_lm)

coef_df <- tibble(
  term     = names(coef(heapo_lm)),
  estimate = coef(heapo_lm),
  lower    = cis[, 1],
  upper    = cis[, 2]
) |>
  filter(term != "(Intercept)") |>
  mutate(significant = !(lower < 0 & upper > 0))

ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate), color = significant)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Linear Model – Coefficient Estimates with 95% CIs",
    subtitle = "Coefficients on log scale — positive = higher consumption",
    x        = "Coefficient estimate",
    y        = NULL
  )

# ------------------------------------------------------------
# Save models before loading caret so the serialised objects do not
# capture caret/lubridate on the search path.
# ------------------------------------------------------------
saveRDS(heapo_mlm_simple, here("models", "linear_model", "heapo_mlm_simple.rds"))
saveRDS(heapo_lm,         here("models", "linear_model", "heapo_lm.rds"))
cat("Models saved to models/linear_model/\n")

# ------------------------------------------------------------
# 5-fold CV model comparison (MAE on kWh scale)
# ------------------------------------------------------------
suppressPackageStartupMessages(library(caret))

df_cv <- heapo_modelling |>
  select(
    log_kWh_total, heating_degree_days, sunshine_hours, humidity_avg,
    living_area, n_residents, building_type, heatpump_type
  ) |>
  na.omit() |>
  as.data.frame()

set.seed(42)
folds <- createFolds(df_cv$log_kWh_total, k = 5, list = TRUE)

cv_results <- lapply(folds, function(test_idx) {
  train <- df_cv[-test_idx, ]
  test  <- df_cv[test_idx, ]

  simple_fit <- lm(
    log_kWh_total ~ heating_degree_days + sunshine_hours + humidity_avg +
      living_area + n_residents + building_type + heatpump_type,
    data = train
  )
  ext_fit <- lm(
    log_kWh_total ~ heating_degree_days * building_type +
      heating_degree_days * heatpump_type +
      sunshine_hours + humidity_avg + living_area + n_residents,
    data = train
  )

  actual_kwh <- exp(test$log_kWh_total)
  list(
    simple_mae = mean(abs(actual_kwh - exp(predict(simple_fit, newdata = test)))),
    ext_mae    = mean(abs(actual_kwh - exp(predict(ext_fit,    newdata = test))))
  )
})

simple_cv_mae <- mean(sapply(cv_results, `[[`, "simple_mae"))
ext_cv_mae    <- mean(sapply(cv_results, `[[`, "ext_mae"))

print(data.frame(
  Model      = c("Simple MLR (main effects)", "Extended MLR (+ interactions)"),
  Adj_R2     = c(
    round(summary(heapo_mlm_simple)$adj.r.squared, 4),
    round(summary(heapo_lm)$adj.r.squared, 4)
  ),
  CV_MAE_kWh = c(round(simple_cv_mae, 2), round(ext_cv_mae, 2))
))

