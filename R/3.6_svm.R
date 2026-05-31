# ============================================================
# SVM Classification Pipeline — HSLU AML
# ============================================================
# Input:  data_processed/heapo/heapo_modelling.rds
# Output: models/svm/heapo_svm_baseline_hh_q75.rds
#         models/svm/heapo_svm_hh_q75.rds
# ============================================================

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(kernlab)
  library(caret)
  library(doParallel)
})

set.seed(42)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
heapo_modelling <- readRDS(here("data_processed", "heapo", "heapo_modelling.rds"))

# ------------------------------------------------------------
# Create binary target (per-household 75th percentile)
# ------------------------------------------------------------
hh_q75 <- heapo_modelling |>
  group_by(Household_ID) |>
  summarise(hh_q75 = quantile(log_kWh_total, 0.75, na.rm = TRUE), .groups = "drop")

heapo_svm_data <- heapo_modelling |>
  left_join(hh_q75, by = "Household_ID") |>
  mutate(
    high_consumption = as.factor(ifelse(log_kWh_total >= hh_q75, "high", "normal"))
  ) |>
  select(
    high_consumption,
    heating_degree_days,
    sunshine_hours,
    humidity_avg,
    living_area,
    n_residents,
    building_type,
    heatpump_type
  ) |>
  na.omit()

cat("Class balance:\n")
print(prop.table(table(heapo_svm_data$high_consumption)))

# ------------------------------------------------------------
# Train / test split (80/20 stratified)
# ------------------------------------------------------------
set.seed(42)
train_idx  <- createDataPartition(heapo_svm_data$high_consumption, p = 0.8, list = FALSE)
train_data <- heapo_svm_data[train_idx, ]
test_data  <- heapo_svm_data[-train_idx, ]
test_x     <- test_data |> select(-high_consumption)
test_y     <- test_data$high_consumption

cat("Training rows:", nrow(train_data), "\n")
cat("Test rows:    ", nrow(test_data), "\n")

# ------------------------------------------------------------
# Baseline SVM (RBF kernel, C=1, sigma estimated from data)
# ------------------------------------------------------------
svm_baseline <- kernlab::ksvm(
  high_consumption ~ .,
  data    = train_data,
  kernel  = "rbfdot",
  C       = 1,
  kpar    = "automatic",
  scaled  = TRUE
)

pred_baseline <- kernlab::predict(svm_baseline, test_x)
cm_baseline   <- confusionMatrix(pred_baseline, test_y, positive = "high")
print(cm_baseline)

# ------------------------------------------------------------
# Tuned SVM (5-fold CV grid search)
# ------------------------------------------------------------
cl <- makePSOCKcluster(max(1, parallel::detectCores() - 1))
registerDoParallel(cl)

ctrl <- trainControl(
  method        = "cv",
  number        = 5,
  allowParallel = TRUE
)

tune_grid <- expand.grid(
  C     = c(1, 10, 100),
  sigma = c(0.01, 0.1, 1 / 7)
)

set.seed(42)
svm_caret <- train(
  high_consumption ~ .,
  data       = train_data,
  method     = "svmRadial",
  trControl  = ctrl,
  tuneGrid   = tune_grid,
  preProcess = c("center", "scale")
)

stopCluster(cl)
registerDoSEQ()

cat("Best tuning parameters:\n")
print(svm_caret$bestTune)

print(svm_caret$results |> arrange(desc(Accuracy)) |> select(C, sigma, Accuracy, Kappa))

# ------------------------------------------------------------
# Evaluate tuned model
# ------------------------------------------------------------
pred_best <- predict(svm_caret, test_x)
cm_best   <- confusionMatrix(pred_best, test_y, positive = "high")
print(cm_best)

extract_metrics <- function(cm, model_name) {
  data.frame(
    Model       = model_name,
    Accuracy    = round(cm$overall["Accuracy"],    4),
    Sensitivity = round(cm$byClass["Sensitivity"], 4),
    Specificity = round(cm$byClass["Specificity"], 4),
    F1_Score    = round(cm$byClass["F1"],           4),
    row.names   = NULL
  )
}

print(rbind(
  extract_metrics(cm_baseline, "Baseline SVM (C=1, default sigma)"),
  extract_metrics(
    cm_best,
    paste0("Tuned SVM (C=", svm_caret$bestTune$C, ", sigma=", round(svm_caret$bestTune$sigma, 4), ")")
  )
))

# ------------------------------------------------------------
# Save models
# ------------------------------------------------------------
saveRDS(svm_baseline, here("models", "svm", "heapo_svm_baseline_hh_q75.rds"))
saveRDS(svm_caret,    here("models", "svm", "heapo_svm_hh_q75.rds"))
cat("Models saved to models/svm/\n")
