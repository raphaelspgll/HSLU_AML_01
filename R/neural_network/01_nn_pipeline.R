# ============================================================
# Neural Network Pipeline — HSLU AML 01
# ============================================================
# Input:  data_processed/heapo/heapo_modelling.rds
# Output: models/neural_network/mod_nn_nnet.rds
#         data_processed/splits/nn/test_nn.rds
#         data_processed/splits/nn/nn_pred_prob.rds
#         models/neural_network/nn_cv_results.csv
#         models/cv_class_results.rds  (NN row appended)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(caret)
  library(nnet)
  library(ROCR)
  library(here)
})

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
path_in       <- here("data_processed/heapo/heapo_modelling.rds")
path_test     <- here("models/neural_network/splits/test_nn.rds")
path_pred     <- here("models/neural_network/splits/nn_pred_prob.rds")
path_mod      <- here("models/neural_network/mod_nn_nnet.rds")
path_cv       <- here("models/neural_network/nn_cv_results.csv")
path_cv_class <- here("models/cv_class_results.rds")

dir.create(dirname(path_test), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(path_mod),  recursive = TRUE, showWarnings = FALSE)

# ============================================================
# (1) Load + preprocess
# ============================================================
if (!file.exists(path_in)) stop("Input file not found: ", path_in)

dat <- readRDS(path_in)
cat("\n[01] Data loaded —", nrow(dat), "rows,", ncol(dat), "cols\n")

# Define high_consumption using per-household 75th percentile
hh_q75 <- dat %>%
  group_by(Household_ID) %>%
  summarise(hh_q75 = quantile(log_kWh_total, 0.75, na.rm = TRUE), .groups = "drop")

dat <- dat %>%
  left_join(hh_q75, by = "Household_ID") %>%
  mutate(high_consumption = as.integer(log_kWh_total >= hh_q75)) %>%
  select(-hh_q75)
cat(sprintf("[01] Per-household q75 | High: %d (%.1f%%) | Normal: %d (%.1f%%)\n",
            sum(dat$high_consumption == 1), mean(dat$high_consumption == 1) * 100,
            sum(dat$high_consumption == 0), mean(dat$high_consumption == 0) * 100))

dat_nn <- dat %>%
  select(
    high_consumption,
    heating_degree_days, temp_avg,
    living_area, building_type, heatpump_type,
    has_floor_heating, n_residents, is_weekend
  ) %>%
  mutate(
    across(c(high_consumption, building_type, heatpump_type,
             has_floor_heating, is_weekend), as.factor)
  )

dat_nn$high_consumption <- droplevels(dat_nn$high_consumption)

cat("[01] NN dataset:", nrow(dat_nn), "rows | Class balance:\n")
print(prop.table(table(dat_nn$high_consumption)))

# ============================================================
# (2) 5-fold stratified CV setup
# ============================================================
if (nrow(dat_nn) == 0)                    stop("NN dataset is empty.")
if (nlevels(dat_nn$high_consumption) < 2) stop("Need at least 2 classes.")

set.seed(42)
n_folds  <- 5
fold_idx <- createFolds(dat_nn$high_consumption, k = n_folds,
                        list = TRUE, returnTrain = FALSE)

cat("\n[02] 5-fold stratified CV setup done\n")

# ============================================================
# (3) Cross-validation loop
# ============================================================

to_be_run <- FALSE  # Set to FALSE to skip training and load saved results

if (to_be_run) {
  
  cv_metrics <- data.frame()
  
  for (fold in seq_len(n_folds)) {
    
    cat(sprintf("\n[03] Fold %d/%d ...", fold, n_folds))
    
    idx_test  <- fold_idx[[fold]]
    train_raw <- dat_nn[-idx_test, ]
    test_raw  <- dat_nn[ idx_test, ]
    
    y_train <- train_raw$high_consumption
    y_test  <- test_raw$high_consumption
    
    x_train_mm <- model.matrix(~ ., data = train_raw %>% select(-high_consumption))[, -1]
    x_test_mm  <- model.matrix(~ ., data = test_raw  %>% select(-high_consumption))[, -1]
    
    train_means <- colMeans(x_train_mm)
    train_sds   <- apply(x_train_mm, 2, sd)
    train_sds[train_sds == 0] <- 1
    
    train_fold <- as.data.frame(scale(x_train_mm, center = train_means, scale = train_sds))
    test_fold  <- as.data.frame(scale(x_test_mm,  center = train_means, scale = train_sds))
    
    colnames(train_fold) <- make.names(colnames(train_fold))
    colnames(test_fold)  <- make.names(colnames(test_fold))
    
    train_fold$high_consumption <- as.factor(y_train)
    test_fold$high_consumption  <- as.factor(y_test)
    
    set.seed(42)
    mod_fold <- nnet(
      high_consumption ~ .,
      data  = train_fold,
      size  = 25,
      decay = 0.05,
      maxit = 1000,
      trace = FALSE
    )
    
    pred_raw  <- predict(mod_fold, newdata = test_fold, type = "raw")
    pred_prob <- if (is.matrix(pred_raw)) {
      if ("1" %in% colnames(pred_raw)) pred_raw[, "1"] else pred_raw[, ncol(pred_raw)]
    } else { pred_raw }
    
    truth      <- test_fold$high_consumption
    pred_class <- factor(ifelse(pred_prob >= 0.5, "1", "0"), levels = levels(truth))
    cm         <- table(Predicted = pred_class, Actual = truth)
    acc        <- mean(pred_class == truth)
    tp <- cm["1","1"]; tn <- cm["0","0"]
    fp <- cm["1","0"]; fn <- cm["0","1"]
    sens    <- tp / (tp + fn)
    spec    <- tn / (tn + fp)
    pred_roc <- ROCR::prediction(pred_prob, truth)
    auc_val  <- as.numeric(ROCR::performance(pred_roc, "auc")@y.values[[1]])
    
    cv_metrics <- rbind(cv_metrics, data.frame(
      fold        = fold,
      accuracy    = round(acc,     4),
      sensitivity = round(sens,    4),
      specificity = round(spec,    4),
      auc         = round(auc_val, 4)
    ))
    
    cat(sprintf("  AUC=%.4f  Acc=%.4f", auc_val, acc))
  }
  
  cat("\n\n[03] CV Results per fold:\n")
  print(cv_metrics)
  cat(sprintf("\n[03] Mean AUC=%.4f | Mean Acc=%.4f | Mean Sens=%.4f | Mean Spec=%.4f\n",
              mean(cv_metrics$auc),
              mean(cv_metrics$accuracy),
              mean(cv_metrics$sensitivity),
              mean(cv_metrics$specificity)))
  
  write.csv(cv_metrics, path_cv, row.names = FALSE)
  cat("[03] CV results saved to:", path_cv, "\n")
  
} else {
  
  # Load previously saved CV results
  if (!file.exists(path_cv)) stop("to_be_run=FALSE but no saved CV results found at: ", path_cv)
  cv_metrics <- read.csv(path_cv)
  cat("\n[03] Skipping training — loaded saved CV results from:", path_cv, "\n")
  print(cv_metrics)
  cat(sprintf("\n[03] Mean AUC=%.4f | Mean Acc=%.4f | Mean Sens=%.4f | Mean Spec=%.4f\n",
              mean(cv_metrics$auc),
              mean(cv_metrics$accuracy),
              mean(cv_metrics$sensitivity),
              mean(cv_metrics$specificity)))
}
# ============================================================
# (4) Refit final model on full dataset + save outputs
# ============================================================

if (to_be_run) {
  
  cat("\n[04] Refitting final model on full dataset...\n")
  
  # Encode + scale on full data
  x_full_mm  <- model.matrix(~ ., data = dat_nn %>% select(-high_consumption))[, -1]
  full_means <- colMeans(x_full_mm)
  full_sds   <- apply(x_full_mm, 2, sd)
  full_sds[full_sds == 0] <- 1
  
  train_nn <- as.data.frame(scale(x_full_mm, center = full_means, scale = full_sds))
  colnames(train_nn) <- make.names(colnames(train_nn))
  train_nn$high_consumption <- as.factor(dat_nn$high_consumption)
  
  # Train final model
  set.seed(42)
  mod_nn <- nnet(
    high_consumption ~ .,
    data  = train_nn,
    size  = 25,
    decay = 0.05,
    maxit = 1000,
    trace = FALSE
  )
  saveRDS(mod_nn, path_mod)
  cat("[04] Final model saved to:", path_mod, "\n")
  
  # Prepare representative test set (last fold)
  idx_test_final <- fold_idx[[n_folds]]
  test_raw_final <- dat_nn[idx_test_final, ]
  y_test_final   <- test_raw_final$high_consumption
  
  x_test_final <- model.matrix(~ ., data = test_raw_final %>% select(-high_consumption))[, -1]
  test_nn      <- as.data.frame(scale(x_test_final, center = full_means, scale = full_sds))
  colnames(test_nn) <- make.names(colnames(test_nn))
  test_nn$high_consumption <- as.factor(y_test_final)
  
  saveRDS(test_nn, path_test)
  cat("[04] test_nn saved to:", path_test, "\n")
  
  # Predict probabilities on test set
  pred_prob_raw <- predict(mod_nn, newdata = test_nn, type = "raw")
  pred_prob_1   <- if (is.matrix(pred_prob_raw)) {
    if ("1" %in% colnames(pred_prob_raw)) pred_prob_raw[, "1"] else pred_prob_raw[, ncol(pred_prob_raw)]
  } else { pred_prob_raw }
  
  saveRDS(pred_prob_1, path_pred)
  cat("[04] pred_prob_1 saved to:", path_pred, "\n")
  
} else {
  
  # Load previously saved model and outputs
  if (!file.exists(path_mod))  stop("to_be_run=FALSE but no saved model found at: ", path_mod)
  if (!file.exists(path_test)) stop("to_be_run=FALSE but no saved test set found at: ", path_test)
  if (!file.exists(path_pred)) stop("to_be_run=FALSE but no saved predictions found at: ", path_pred)
  
  mod_nn      <- readRDS(path_mod)
  test_nn     <- readRDS(path_test)
  pred_prob_1 <- readRDS(path_pred)
  
  cat("\n[04] Skipping refit — loaded saved model from:", path_mod, "\n")
  cat("[04] Loaded test set from:", path_test, "\n")
  cat("[04] Loaded predictions from:", path_pred, "\n")
}

# ============================================================
# (5) Append NN row to shared cv_class_results.rds
# ============================================================
nn_cv_row <- data.frame(
  Model         = "Neural Network",
  `CV AUC`      = round(mean(cv_metrics$auc), 3),
  `CV Accuracy` = round(mean(cv_metrics$accuracy), 3),
  check.names   = FALSE
)

if (file.exists(path_cv_class)) {
  existing <- readRDS(path_cv_class)
  existing <- existing[!grepl("Neural Network", existing$Model), ]
  saveRDS(rbind(existing, nn_cv_row), path_cv_class)
  cat("[05] NN row appended to cv_class_results.rds\n")
} else {
  warning("[05] cv_class_results.rds not found — run GLM/SVM pipeline first.")
}

cat("\n[05] Pipeline complete.\n")
