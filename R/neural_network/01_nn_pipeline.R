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
# (4) Fit neural network  (load if already saved)
# ============================================================
if (file.exists(path_mod)) {
  
  mod_nn <- readRDS(path_mod)
  cat("\n[04] Loaded existing model from:", path_mod, "\n")
  
} else {
  
  set.seed(42)
  mod_nn <- nnet(
    high_consumption ~ .,
    data  = train_nn,
    size  = 3,
    decay = 0.001,
    maxit = 300,
    trace = FALSE
  )
  
  saveRDS(mod_nn, path_mod)
  cat("\n[04] Model fitted and saved to:", path_mod, "\n")
}

# Save test set for report
saveRDS(test_nn, path_test)
cat("[04] Saved test_nn to:", path_test, "\n")

# ============================================================
# (5) Predict + evaluate
# ============================================================
pred_prob_raw <- predict(mod_nn, newdata = test_nn, type = "raw")

# Extract P(class = 1) — handles both matrix and vector output
pred_prob_1 <- if (is.matrix(pred_prob_raw)) {
  if ("1" %in% colnames(pred_prob_raw)) pred_prob_raw[, "1"] else pred_prob_raw[, ncol(pred_prob_raw)]
} else {
  pred_prob_raw
}

# Save probabilities for report
saveRDS(pred_prob_1, path_pred)
cat("[05] Saved pred_prob_1 to:", path_pred, "\n")

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

cat("\n[05] Confusion matrix:\n"); print(cm)
cat(sprintf("\n[05] Accuracy: %.4f | Sensitivity: %.4f | Specificity: %.4f\n",
            acc, sensitivity, specificity))

# ============================================================
# (6) Save metrics + confusion matrix
# ============================================================
pred_roc <- ROCR::prediction(pred_prob_1, truth)
auc_val <- as.numeric(ROCR::performance(pred_roc, "auc")@y.values[[1]])
write.csv(
  data.frame(model = "Neural Network", accuracy = acc,
             sensitivity = sensitivity, specificity = specificity,
             auc = auc_val),
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