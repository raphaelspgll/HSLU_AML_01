# ----------------------------------------
# (1) Load + basic preprocessing
# ----------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(caret)
})

# -----------------------------
# Paths
# -----------------------------
path_in <- "data_processed/heapo/heapo_modelling.rds"

# -----------------------------
# Load
# -----------------------------
if (!file.exists(path_in)) {
  stop("Input file not found: ", path_in)
}

dat <- readRDS(path_in)

cat("\n[01] Data loaded\n")
cat("[01] Rows:", nrow(dat), "| Cols:", ncol(dat), "\n")

# -----------------------------
# Keep only variables needed for NN
# -----------------------------
dat_nn <- dat %>%
  select(
    high_consumption,
    heating_degree_days, temp_avg,
    living_area, building_type, heatpump_type,
    has_floor_heating, n_residents, is_weekend
  ) %>%
  mutate(
    high_consumption   = as.factor(high_consumption),
    building_type      = as.factor(building_type),
    heatpump_type      = as.factor(heatpump_type),
    has_floor_heating  = as.factor(has_floor_heating),
    is_weekend         = as.factor(is_weekend)
  )

# Drop unused factor levels just in case
dat_nn$high_consumption <- droplevels(dat_nn$high_consumption)

cat("[01] Rows in NN dataset:", nrow(dat_nn), "\n")
cat("[01] Class balance:\n")
print(table(dat_nn$high_consumption))

# ----------------------------------------
# (2) Train / test split
# ----------------------------------------
# Goal:
# - Create a reproducible 80/20 split
# - Preserve class balance (stratified split)

if (nrow(dat_nn) == 0) {
  stop("NN dataset is empty.")
}

if (nlevels(dat_nn$high_consumption) < 2) {
  stop("high_consumption has fewer than 2 classes.")
}

set.seed(42)

idx_train <- createDataPartition(dat_nn$high_consumption, p = 0.8, list = FALSE)

train <- dat_nn[idx_train, ]
test  <- dat_nn[-idx_train, ]

cat("\n[02] Train/test split completed\n")
cat("[02] Train rows:", nrow(train), "\n")
cat("[02] Test rows :", nrow(test), "\n")

cat("\n[02] Class balance in full data:\n")
print(prop.table(table(dat_nn$high_consumption)))

cat("\n[02] Class balance in train:\n")
print(prop.table(table(train$high_consumption)))

cat("\n[02] Class balance in test:\n")
print(prop.table(table(test$high_consumption)))

# ----------------------------------------
# (3) Encode categorical variables + scale numeric predictors
# ----------------------------------------
# Goal:
# - Convert categorical predictors into numeric dummy variables
# - Scale numeric predictors using training data only
# - Apply the same transformation to the test set

suppressPackageStartupMessages({
  library(dplyr)
})

# -----------------------------
# Separate response and predictors
# -----------------------------
y_train <- train$high_consumption
y_test  <- test$high_consumption

x_train_raw <- train %>% select(-high_consumption)
x_test_raw  <- test  %>% select(-high_consumption)

# -----------------------------
# One-hot encode categorical predictors
# -----------------------------
# model.matrix() converts factors into dummy variables
# We remove the intercept column afterwards

x_train_mm <- model.matrix(~ ., data = x_train_raw)[, -1]
x_test_mm  <- model.matrix(~ ., data = x_test_raw)[, -1]

cat("\n[03] Encoded predictor dimensions\n")
cat("[03] Train matrix:", nrow(x_train_mm), "rows x", ncol(x_train_mm), "cols\n")
cat("[03] Test matrix :", nrow(x_test_mm), "rows x", ncol(x_test_mm), "cols\n")

# -----------------------------
# Scale predictors using TRAIN only
# -----------------------------
# Important:
# - fit scaling parameters on training set
# - reuse same center/scale for test set

train_means <- apply(x_train_mm, 2, mean)
train_sds   <- apply(x_train_mm, 2, sd)

# Safety fix: if a column has sd = 0, replace with 1
train_sds[train_sds == 0] <- 1

x_train_scaled <- scale(x_train_mm, center = train_means, scale = train_sds)
x_test_scaled  <- scale(x_test_mm, center = train_means, scale = train_sds)

# Convert back to data frames
train_nn <- as.data.frame(x_train_scaled)
test_nn  <- as.data.frame(x_test_scaled)

# Add response back
train_nn$high_consumption <- y_train
test_nn$high_consumption  <- y_test

cat("\n[03] Scaling completed\n")
cat("[03] Final train_nn rows:", nrow(train_nn), "| cols:", ncol(train_nn), "\n")
cat("[03] Final test_nn rows :", nrow(test_nn),  "| cols:", ncol(test_nn), "\n")

# ----------------------------------------
# (3b) Make encoded column names safe
# ----------------------------------------

colnames(train_nn) <- make.names(colnames(train_nn))
colnames(test_nn)  <- make.names(colnames(test_nn))

cat("\n[03b] Safe column names applied\n")
print(names(train_nn))

# ----------------------------------------
# (4) Fit Neural Network with nnet
# ----------------------------------------

suppressPackageStartupMessages({
  library(nnet)
})

# Response must be factor for classification
train_nn$high_consumption <- as.factor(train_nn$high_consumption)
test_nn$high_consumption  <- as.factor(test_nn$high_consumption)

# Paths
model_dir <- "models/neural_network"
path_mod  <- file.path(model_dir, "mod_nn_nnet.rds")

if (!dir.exists(model_dir)) {
  dir.create(model_dir, recursive = TRUE)
}

# Fit one simple model
set.seed(42)

mod_nn <- nnet(
  high_consumption ~ .,
  data = train_nn,
  size = 3,
  decay = 0.001,
  maxit = 300,
  trace = FALSE
)

cat("\n[04] Neural network fitted successfully with nnet\n")

# Save model
saveRDS(mod_nn, path_mod)

cat("[04] Saved neural network model:", path_mod, "\n")

# ----------------------------------------
# (5) Predict + evaluate
# ----------------------------------------
# Goal:
# - Predict class probabilities on the test set
# - Convert probabilities into class predictions
# - Evaluate the model with a confusion matrix and accuracy

pred_prob <- predict(mod_nn, newdata = test_nn, type = "raw")

# For binary classification, extract probability of class "1"
if (is.matrix(pred_prob)) {
  if ("1" %in% colnames(pred_prob)) {
    pred_prob_1 <- pred_prob[, "1"]
  } else {
    pred_prob_1 <- pred_prob[, ncol(pred_prob)]
  }
} else {
  pred_prob_1 <- pred_prob
}

# Convert probabilities to class predictions using 0.5 cutoff
pred_class <- ifelse(pred_prob_1 >= 0.5, "1", "0")
pred_class <- factor(pred_class, levels = levels(test_nn$high_consumption))

# True labels
truth <- test_nn$high_consumption

# Confusion matrix
cm <- table(Predicted = pred_class, Actual = truth)

# Accuracy
acc <- mean(pred_class == truth)

cat("\n[05] Confusion matrix:\n")
print(cm)

cat("\n[05] Accuracy:", round(acc, 4), "\n")

tp <- cm["1", "1"]
tn <- cm["0", "0"]
fp <- cm["1", "0"]
fn <- cm["0", "1"]

sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)

cat("[05] Sensitivity:", round(sensitivity, 4), "\n")
cat("[05] Specificity:", round(specificity, 4), "\n")

# 6. Evaluate (metrics + plots)
# 7. Save outputs (figures + results)