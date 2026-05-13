# 5-fold CV for classification models: GLM Binomial and SVM
# Response: high_consumption defined as the global 75th percentile of log_kWh_total
# (top 25% of daily usage), consistent with the SVM section of the report.
# SVM hyperparameters are fixed to the tuned values (C=100, sigma=0.1429) so that
# each fold evaluates predictive performance rather than re-running hyperparameter search.
# Output: models/cv_class_results.rds
#
# NOTE: Each SVM fold takes ~5-10 min on this dataset. Allow 30-60 min total.
# Run from RStudio with HSLU_AML_01.Rproj open.

library(dplyr)
library(caret)
library(kernlab)

heapo <- readRDS("data_processed/heapo/heapo_modelling.rds")

# Derive high_consumption using the global 75th percentile (top 25%), matching
# the threshold used in the SVM section of the report.
q75 <- quantile(heapo$log_kWh_total, 0.75, na.rm = TRUE)

# Union of predictors used by either model — drop NAs across all of them once
# so both models operate on exactly the same rows and folds are shared.
df_all <- heapo |>
  select(
    log_kWh_total,
    heating_degree_days,
    temp_avg,
    sunshine_hours,
    humidity_avg,
    living_area,
    building_type,
    heatpump_type,
    has_floor_heating,
    n_residents,
    is_weekend,
    month
  ) |>
  mutate(
    month = as.factor(month),
    high_consumption_int = as.integer(log_kWh_total >= q75),
    high_consumption_fac = factor(
      ifelse(log_kWh_total >= q75, "high", "normal"),
      levels = c("high", "normal")
    )
  ) |>
  select(-log_kWh_total) |>
  na.omit()

cat(sprintf("Rows after NA removal: %d\n", nrow(df_all)))

set.seed(42)
folds <- createFolds(df_all$high_consumption_int, k = 5, list = TRUE)

compute_auc <- function(actual, prob) {
  ord <- order(prob, decreasing = TRUE)
  act <- actual[ord]
  n1 <- sum(act == 1)
  n0 <- sum(act == 0)
  tpr <- c(0, cumsum(act == 1) / n1)
  fpr <- c(0, cumsum(act == 0) / n0)
  sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)
}

glm_cols <- c(
  "high_consumption_int",
  "heating_degree_days",
  "temp_avg",
  "living_area",
  "building_type",
  "heatpump_type",
  "has_floor_heating",
  "n_residents",
  "is_weekend",
  "month"
)
svm_cols <- c(
  "high_consumption_fac",
  "heating_degree_days",
  "sunshine_hours",
  "humidity_avg",
  "living_area",
  "n_residents",
  "building_type",
  "heatpump_type"
)

cv_class <- lapply(seq_along(folds), function(i) {
  cat(sprintf("  Fold %d / %d ... ", i, length(folds)))
  t0 <- proc.time()["elapsed"]

  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(df_all)), test_idx)

  # GLM Binomial
  glm_train <- df_all[train_idx, glm_cols]
  glm_test <- df_all[test_idx, glm_cols]
  names(glm_train)[1] <- "y"
  names(glm_test)[1] <- "y"

  glm_fit <- glm(y ~ ., data = glm_train, family = binomial)
  glm_prob <- predict(glm_fit, newdata = glm_test, type = "response")
  glm_pred <- as.integer(glm_prob >= 0.5)

  # SVM with fixed tuned hyperparameters
  svm_train <- df_all[train_idx, svm_cols]
  svm_test <- df_all[test_idx, svm_cols]

  svm_fit <- kernlab::ksvm(
    high_consumption_fac ~ .,
    data = svm_train,
    kernel = "rbfdot",
    kpar = list(sigma = 0.1429),
    C = 100,
    prob.model = TRUE,
    scaled = TRUE
  )
  svm_prob <- kernlab::predict(
    svm_fit,
    newdata = svm_test,
    type = "probabilities"
  )[, "high"]
  svm_pred <- kernlab::predict(svm_fit, newdata = svm_test)

  actual_int <- df_all$high_consumption_int[test_idx]
  actual_fac <- df_all$high_consumption_fac[test_idx]

  elapsed <- proc.time()["elapsed"] - t0
  cat(sprintf("done (%.0f s)\n", elapsed))

  list(
    glm_auc = compute_auc(actual_int, glm_prob),
    glm_acc = mean(glm_pred == actual_int),
    svm_auc = compute_auc(actual_int, svm_prob),
    svm_acc = mean(svm_pred == actual_fac)
  )
})

glm_auc <- mean(sapply(cv_class, `[[`, "glm_auc"))
glm_acc <- mean(sapply(cv_class, `[[`, "glm_acc"))
svm_auc <- mean(sapply(cv_class, `[[`, "svm_auc"))
svm_acc <- mean(sapply(cv_class, `[[`, "svm_acc"))

class_summary <- data.frame(
  Model = c("GLM Binomial", "SVM (C=100, σ=0.1429)"),
  `CV AUC` = round(c(glm_auc, svm_auc), 3),
  `CV Accuracy` = round(c(glm_acc, svm_acc), 3),
  check.names = FALSE
)

saveRDS(class_summary, "models/cv_class_results.rds")
cat("\nSaved models/cv_class_results.rds\n")
print(class_summary)
