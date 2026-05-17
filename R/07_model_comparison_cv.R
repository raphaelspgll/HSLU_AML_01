# 5-fold cross-validation: LM vs GAM on log_kWh_total
# Run this script once from RStudio with HSLU_AML_01.Rproj open.
# Output: models/cv_reg_results.rds

library(dplyr)
library(mgcv)
library(caret)

heapo <- readRDS("data_processed/heapo/heapo_modelling.rds")

df_reg <- heapo |>
  select(
    log_kWh_total, heating_degree_days, sunshine_hours,
    humidity_avg, living_area, n_residents, building_type, heatpump_type
  ) |>
  as.data.frame() |>
  na.omit()

set.seed(42)
folds <- createFolds(df_reg$log_kWh_total, k = 5, list = TRUE)

cv_results <- lapply(folds, function(test_idx) {
  train <- df_reg[-test_idx, ]
  test  <- df_reg[test_idx, ]

  lm_cv <- lm(
    log_kWh_total ~
      heating_degree_days * building_type +
      heating_degree_days * heatpump_type +
      sunshine_hours + humidity_avg + living_area + n_residents,
    data = train
  )

  gam_cv <- mgcv::gam(
    log_kWh_total ~
      s(heating_degree_days, by = building_type) +
      s(heating_degree_days, by = heatpump_type) +
      building_type + heatpump_type +
      s(sunshine_hours) + s(humidity_avg) +
      s(living_area) + n_residents,
    family = gaussian,
    data = train
  )

  pred_lm  <- predict(lm_cv,  newdata = test)
  pred_gam <- predict(gam_cv, newdata = test)

  actual_log <- test$log_kWh_total
  actual_kwh <- exp(actual_log)

  list(
    lm_mae  = mean(abs(actual_kwh - exp(pred_lm))),
    lm_r2   = 1 - sum((actual_log - pred_lm)^2) /
                  sum((actual_log - mean(actual_log))^2),
    gam_mae = mean(abs(actual_kwh - exp(pred_gam))),
    gam_r2  = 1 - sum((actual_log - pred_gam)^2) /
                  sum((actual_log - mean(actual_log))^2)
  )
})

mae_lm  <- mean(sapply(cv_results, `[[`, "lm_mae"))
r2_lm   <- mean(sapply(cv_results, `[[`, "lm_r2"))
mae_gam <- mean(sapply(cv_results, `[[`, "gam_mae"))
r2_gam  <- mean(sapply(cv_results, `[[`, "gam_r2"))

reg_summary <- data.frame(
  Model             = c("Linear Model (extended)", "GAM (same predictors)"),
  `CV MAE (kWh/day)` = round(c(mae_lm, mae_gam), 2),
  `CV R2`           = round(c(r2_lm,  r2_gam),  3),
  check.names = FALSE
)

saveRDS(reg_summary, "models/cv_reg_results.rds")
cat("Saved models/cv_reg_results.rds\n")
print(reg_summary)
