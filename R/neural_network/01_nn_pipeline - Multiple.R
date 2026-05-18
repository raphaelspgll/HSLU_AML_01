# ============================================================
# Neural Network Pipeline — HSLU AML 01
# ============================================================
# Input:  data_processed/heapo/heapo_modelling.rds
# Output: models/neural_network/mod_nn_nnet.rds
#         data_processed/splits/nn/test_nn.rds
#         data_processed/splits/nn/nn_pred_prob.rds
#         models/neural_network/nn_test_metrics.csv
#         models/neural_network/nn_confusion_matrix.csv
# ============================================================

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(caret)
  library(nnet)
  library(ROCR)
})

# ------------------------------------------------------------
# Paths  (all defined once here — edit only this section)
# ------------------------------------------------------------
path_in   <- "../../data_processed/heapo/heapo_modelling.rds"
path_test <- "../../data_processed/splits/nn/test_nn.rds"
path_pred <- "../../data_processed/splits/nn/nn_pred_prob.rds"
path_mod  <- "../../models/neural_network/mod_nn_nnet.rds"
path_metr <- "../../models/neural_network/nn_test_metrics.csv"
path_cm   <- "../../models/neural_network/nn_confusion_matrix.csv"

# Create output directories if missing
dir.create(dirname(path_test), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(path_mod),  recursive = TRUE, showWarnings = FALSE)

# ============================================================
# (1) Load + preprocess
# ============================================================
if (!file.exists(path_in)) stop("Input file not found: ", path_in)

dat <- readRDS(path_in)
cat("\n[01] Data loaded —", nrow(dat), "rows,", ncol(dat), "cols\n")

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
# (2) Train / test split  (stratified 80/20)
# ============================================================
if (nrow(dat_nn) == 0)           stop("NN dataset is empty.")
if (nlevels(dat_nn$high_consumption) < 2) stop("Need at least 2 classes.")

set.seed(42)
idx_train <- createDataPartition(dat_nn$high_consumption, p = 0.8, list = FALSE)
train <- dat_nn[ idx_train, ]
test  <- dat_nn[-idx_train, ]

cat("\n[02] Split — train:", nrow(train), "| test:", nrow(test), "\n")

# ============================================================
# (3) Encode + scale
# ============================================================
y_train <- train$high_consumption
y_test  <- test$high_consumption

x_train_mm <- model.matrix(~ ., data = train %>% select(-high_consumption))[, -1]
x_test_mm  <- model.matrix(~ ., data = test  %>% select(-high_consumption))[, -1]

# Fit scaling on train only, apply to both
train_means <- colMeans(x_train_mm)
train_sds   <- apply(x_train_mm, 2, sd)
train_sds[train_sds == 0] <- 1   # avoid division by zero

train_nn <- as.data.frame(scale(x_train_mm, center = train_means, scale = train_sds))
test_nn  <- as.data.frame(scale(x_test_mm,  center = train_means, scale = train_sds))

# Safe column names + response
colnames(train_nn) <- make.names(colnames(train_nn))
colnames(test_nn)  <- make.names(colnames(test_nn))

train_nn$high_consumption <- as.factor(y_train)
test_nn$high_consumption  <- as.factor(y_test)

cat("[03] Encoding + scaling done —", ncol(train_nn) - 1, "predictors\n")

# ============================================================
# (4B) Fit neural network  // Multiple models with comparison
# ============================================================

# --- Define 10 parameter combinations ---
param_grid <- data.frame(
  model_id = sprintf("nn_%02d", 1:10),
  size      = c( 3,  5,  8, 10, 12,  15,  15,  20,  20,  25),
  decay     = c(0.0001, 0.001, 0.001, 0.005, 0.01, 0.01, 0.05, 0.01, 0.1, 0.05),
  maxit     = c(300,  300,  500,  500,  500,  700,  700, 1000, 1000, 1000)
)

cat("\n[04B] Starting loop over", nrow(param_grid), "NN configurations...\n")

# --- Storage for results ---
results_list <- vector("list", nrow(param_grid))

for (i in seq_len(nrow(param_grid))) {
  
  p        <- param_grid[i, ]
  mod_path <- file.path("../../models/neural_network",
                        paste0(p$model_id, ".rds"))
  
  cat(sprintf("\n  [%02d/10] size=%-3d | decay=%-5s | maxit=%-3d",
              i, p$size, p$decay, p$maxit))
  
  # --- Train or load from cache ---
  if (file.exists(mod_path)) {
    mod <- readRDS(mod_path)
    cat("  → loaded from cache")
  } else {
    set.seed(42)
    mod <- nnet(
      high_consumption ~ .,
      data  = train_nn,
      size  = p$size,
      decay = p$decay,
      maxit = p$maxit,
      trace = FALSE
    )
    saveRDS(mod, mod_path)
    cat("  → trained & saved")
  }
  
  # --- Predict on test set ---
  pred_raw <- predict(mod, newdata = test_nn, type = "raw")
  
  pred_prob <- if (is.matrix(pred_raw)) {
    if ("1" %in% colnames(pred_raw)) pred_raw[, "1"] else pred_raw[, ncol(pred_raw)]
  } else {
    pred_raw
  }
  
  # --- Metrics ---
  truth      <- test_nn$high_consumption
  pred_class <- factor(ifelse(pred_prob >= 0.5, "1", "0"), levels = levels(truth))
  
  cm  <- table(Predicted = pred_class, Actual = truth)
  acc <- mean(pred_class == truth)
  tp  <- cm["1", "1"]; tn <- cm["0", "0"]
  fp  <- cm["1", "0"]; fn <- cm["0", "1"]
  sens <- tp / (tp + fn)
  spec <- tn / (tn + fp)
  
  pred_roc <- ROCR::prediction(pred_prob, truth)
  auc_val  <- as.numeric(ROCR::performance(pred_roc, "auc")@y.values[[1]])
  results_list[[i]] <- data.frame(
    model_id    = p$model_id,
    size        = p$size,
    decay       = p$decay,
    maxit       = p$maxit,
    accuracy    = round(acc,  4),
    sensitivity = round(sens, 4),
    specificity = round(spec, 4),
    auc         = round(auc_val, 4),
    pred_prob   = I(list(pred_prob))   # store probs for ROC plot later
  )
  
  cat(sprintf("  | AUC=%.4f  Acc=%.4f", auc_val, acc))
}

# --- Combine all results ---
results_df <- do.call(rbind, lapply(results_list, function(x) x[, -ncol(x)]))

cat("\n\n[04B] ── Comparison Table ──────────────────────────────\n")
print(results_df[order(-results_df$auc), ])

# --- Save comparison table ---
write.csv(results_df[order(-results_df$auc), ],
          "../../models/neural_network/nn_comparison.csv",
          row.names = FALSE)
cat("\n[04B] Comparison table saved to models/neural_network/nn_comparison.csv\n")

# --- Pick best model (by AUC) ---
best_idx  <- which.max(results_df$auc)
best_info <- results_df[best_idx, ]

cat(sprintf("\n[04B] ✓ Best model: %s | size=%d | decay=%s | maxit=%d | AUC=%.4f\n",
            best_info$model_id, best_info$size,
            best_info$decay, best_info$maxit, best_info$auc))

# Load best model and save as the main model
best_mod_path <- file.path("../../models/neural_network",
                           paste0(best_info$model_id, ".rds"))
mod_nn <- readRDS(best_mod_path)
saveRDS(mod_nn, path_mod)   # overwrites mod_nn_nnet.rds
cat("[04B] Best model saved as:", path_mod, "\n")

# Keep best probs for section (5) onward
pred_prob_1 <- results_list[[best_idx]]$pred_prob[[1]]

# --- ROC overlay plot (all 10 models) ---
par(mfrow = c(1, 1))
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     main = "ROC Curves — All 10 NN Configurations")
abline(a = 0, b = 1, lty = 2, col = "grey60")

colors <- colorRampPalette(c("steelblue", "tomato"))(10)

for (i in seq_len(nrow(param_grid))) {
  pp       <- results_list[[i]]$pred_prob[[1]]
  truth    <- test_nn$high_consumption
  pred_roc <- ROCR::prediction(pp, truth)
  perf_roc <- ROCR::performance(pred_roc, "tpr", "fpr")
  plot(perf_roc, add = TRUE, col = colors[i], lwd = 1.5)
}

# Highlight best in bold
pp_best  <- results_list[[best_idx]]$pred_prob[[1]]
pred_best <- ROCR::prediction(pp_best, test_nn$high_consumption)
perf_best <- ROCR::performance(pred_best, "tpr", "fpr")
plot(perf_best, add = TRUE, col = "black", lwd = 3)

legend("bottomright",
       legend = c(sprintf("%s (AUC=%.3f)", results_df$model_id, results_df$auc),
                  paste0("BEST: ", best_info$model_id)),
       col    = c(colors, "black"),
       lwd    = c(rep(1.5, 10), 3),
       cex    = 0.65)

# ============================================================
# (5) Predict + evaluate  (best model only)
# ============================================================

# pred_prob_1 and mod_nn are already set by section (4B)
# No need to re-predict — just save and evaluate

# Save probabilities for report
saveRDS(pred_prob_1, path_pred)
cat("[05] Saved pred_prob_1 to:", path_pred, "\n")

# Save test set for report
saveRDS(test_nn, path_test)
cat("[05] Saved test_nn to:", path_test, "\n")

# Class predictions at 0.5 threshold
truth      <- test_nn$high_consumption
pred_class <- factor(ifelse(pred_prob_1 >= 0.5, "1", "0"), levels = levels(truth))

# Metrics
cm  <- table(Predicted = pred_class, Actual = truth)
acc <- mean(pred_class == truth)
tp  <- cm["1", "1"]; tn <- cm["0", "0"]
fp  <- cm["1", "0"]; fn <- cm["0", "1"]
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)

cat("\n[05] Best model confusion matrix:\n"); print(cm)
cat(sprintf("\n[05] Accuracy: %.4f | Sensitivity: %.4f | Specificity: %.4f\n",
            acc, sensitivity, specificity))

# ============================================================
# (6) Save metrics + confusion matrix  (best model)
# ============================================================
pred_roc <- ROCR::prediction(pred_prob_1, truth)
auc_val  <- as.numeric(ROCR::performance(pred_roc, "auc")@y.values[[1]])

write.csv(
  data.frame(
    model       = paste0("Neural Network (", best_info$model_id, ")"),
    size        = best_info$size,
    decay       = best_info$decay,
    maxit       = best_info$maxit,
    accuracy    = acc,
    sensitivity = sensitivity,
    specificity = specificity,
    auc         = auc_val
  ),
  path_metr, row.names = FALSE
)

write.csv(as.data.frame(cm), path_cm, row.names = FALSE)
cat("\n[06] Metrics saved to:", path_metr, "\n")
# ============================================================
# (7) Evaluation plots  (console preview only — report uses ggplot)
# ============================================================
perf_roc <- ROCR::performance(pred_roc, "tpr", "fpr")

par(mfrow = c(1, 2))

plot(pred_prob_1,
     col  = ifelse(truth == "1", "tomato", "steelblue"),
     pch  = 16, cex = 0.4,
     main = "Predicted Probabilities (NN)",
     xlab = "Observation", ylab = "P(high consumption)")
legend("topright", legend = c("Actual 1", "Actual 0"),
       col = c("tomato", "steelblue"), pch = 16)

plot(perf_roc, lwd = 2, main = "ROC Curve (NN)",
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(a = 0, b = 1, lty = 2, col = "grey60")

par(mfrow = c(1, 1))

cat("\n[07] Pipeline complete.\n")