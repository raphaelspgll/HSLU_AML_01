# ============================================================
# GAM – log(kWh_total)
# Response:   log_kWh_total (daily total electricity consumption, log scale)
# Predictors: heating_degree_days, temp_avg, living_area (smooth terms)
#             building_type, heatpump_type (parametric terms)
# ============================================================

library(dplyr)
library(ggplot2)
library(mgcv)

set.seed(42)

# ------------------------------------------------------------
# 1. Load & prepare data
# ------------------------------------------------------------
df <- readRDS("data_processed/heapo/heapo_modelling.rds")

df_gam <- df |>
  select(log_kWh_total, heating_degree_days, temp_avg, living_area,
         building_type, heatpump_type) |>
  drop_na()

train_idx <- sample(nrow(df_gam), 0.8 * nrow(df_gam))
gam_train <- df_gam[train_idx, ]
gam_test  <- df_gam[-train_idx, ]

cat("Training rows:", nrow(gam_train), "| Test rows:", nrow(gam_test), "\n")

# ------------------------------------------------------------
# 2. Fit GAM
# ------------------------------------------------------------
model_gam <- gam(
  log_kWh_total ~ s(heating_degree_days) + s(temp_avg) + s(living_area) +
    building_type + heatpump_type,
  family = gaussian,
  data   = gam_train
)

saveRDS(model_gam, "models/gam_model.rds")

# ------------------------------------------------------------
# 3. Model summary
# ------------------------------------------------------------
summary(model_gam)

# edf > 1 for smooth terms indicates non-linearity is present
# R-sq (adj) and deviance explained show overall fit quality

# ------------------------------------------------------------
# 4. Performance on test set
# ------------------------------------------------------------
pred_gam <- predict(model_gam, newdata = gam_test)
rmse_gam <- sqrt(mean((gam_test$log_kWh_total - pred_gam)^2))
mae_gam  <- mean(abs(gam_test$log_kWh_total - pred_gam))

cat(sprintf(
  "\nTest-set performance (log scale):\n  RMSE: %.4f\n  MAE:  %.4f\n",
  rmse_gam, mae_gam
))

# RMSE on original kWh scale (back-transformed)
rmse_kwh <- sqrt(mean((exp(gam_test$log_kWh_total) - exp(pred_gam))^2))
cat(sprintf("  RMSE (kWh scale): %.2f kWh/day\n", rmse_kwh))

# ------------------------------------------------------------
# 5. Smooth term plots (base R)
# ------------------------------------------------------------
par(mfrow = c(1, 3))
plot(model_gam, shade = TRUE, seWithMean = TRUE,
     ylab = "Effect on log(kWh_total)", residuals = FALSE)
par(mfrow = c(1, 1))

# ------------------------------------------------------------
# 6. Prediction grid: heating_degree_days x building_type
# ------------------------------------------------------------
hdd_grid <- expand.grid(
  heating_degree_days = seq(min(gam_train$heating_degree_days),
                            max(gam_train$heating_degree_days), length.out = 200),
  temp_avg      = median(gam_train$temp_avg),
  living_area   = median(gam_train$living_area),
  building_type = levels(gam_train$building_type),
  heatpump_type = levels(gam_train$heatpump_type)[1]
)

hdd_pred        <- predict(model_gam, newdata = hdd_grid, se.fit = TRUE)
hdd_grid$fit    <- hdd_pred$fit
hdd_grid$se     <- hdd_pred$se.fit

ggplot(hdd_grid, aes(x = heating_degree_days, y = exp(fit),
                     color = building_type, fill = building_type)) +
  geom_ribbon(aes(ymin = exp(fit - 1.96 * se), ymax = exp(fit + 1.96 * se)),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  labs(title    = "GAM – Predicted Daily Consumption by Heating Demand",
       subtitle = "Temperature and living area held at median; air-source heat pump",
       x     = "Heating Degree Days",
       y     = "Predicted kWh/day",
       color = "Building type", fill = "Building type") +
  theme_minimal()

# ------------------------------------------------------------
# 7. Prediction grid: temp_avg x building_type
# ------------------------------------------------------------
temp_grid <- expand.grid(
  temp_avg            = seq(min(gam_train$temp_avg),
                            max(gam_train$temp_avg), length.out = 200),
  heating_degree_days = median(gam_train$heating_degree_days),
  living_area         = median(gam_train$living_area),
  building_type       = levels(gam_train$building_type),
  heatpump_type       = levels(gam_train$heatpump_type)[1]
)

temp_pred         <- predict(model_gam, newdata = temp_grid, se.fit = TRUE)
temp_grid$fit     <- temp_pred$fit
temp_grid$se      <- temp_pred$se.fit

ggplot(temp_grid, aes(x = temp_avg, y = exp(fit),
                      color = building_type, fill = building_type)) +
  geom_ribbon(aes(ymin = exp(fit - 1.96 * se), ymax = exp(fit + 1.96 * se)),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  labs(title    = "GAM – Predicted Daily Consumption by Average Temperature",
       subtitle = "Other predictors held at median / reference level",
       x     = "Average Temperature (°C)",
       y     = "Predicted kWh/day",
       color = "Building type", fill = "Building type") +
  theme_minimal()

# ------------------------------------------------------------
# 8. Observed vs. fitted
# ------------------------------------------------------------
fit_df <- data.frame(
  observed = gam_train$log_kWh_total,
  fitted   = fitted(model_gam)
)

ggplot(fit_df, aes(x = fitted, y = observed)) +
  geom_point(alpha = 0.1, size = 0.6, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "tomato", linetype = "dashed") +
  labs(title = "GAM – Observed vs. Fitted",
       x = "Fitted log(kWh_total)", y = "Observed log(kWh_total)") +
  theme_minimal()

# ------------------------------------------------------------
# 9. Residuals vs. fitted
# ------------------------------------------------------------
resid_df <- data.frame(
  fitted    = fitted(model_gam),
  residuals = residuals(model_gam)
)

ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.1, size = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, color = "tomato", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 0.8) +
  labs(title = "GAM – Residuals vs. Fitted",
       x = "Fitted log(kWh_total)", y = "Residuals") +
  theme_minimal()
