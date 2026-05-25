# ============================================================
# GLM Binomial – high_consumption
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

set.seed(42)

# ------------------------------------------------------------
# 1. Daten laden & vorbereiten
# ------------------------------------------------------------
df <- readRDS("data_processed/heapo/heapo_modelling.rds")

df_glm <- df |>
  select(high_consumption, heating_degree_days, temp_avg, living_area,
         building_type, heatpump_type, has_floor_heating,
         n_residents, is_weekend, month) |>
  mutate(
    high_consumption = as.integer(high_consumption),
    month            = as.factor(month)
  ) |>
  drop_na()

# ------------------------------------------------------------
# 2. Train/Test Split (80/20)
# ------------------------------------------------------------
train_idx <- sample(nrow(df_glm), 0.8 * nrow(df_glm))
train_df  <- df_glm[train_idx, ]
test_df   <- df_glm[-train_idx, ]

# ------------------------------------------------------------
# 3. Modell fitten
# ------------------------------------------------------------
model_glm <- glm(high_consumption ~ ., data = train_df, family = binomial)

summary(model_glm)

# ------------------------------------------------------------
# 4. Vorhersage & Klassifikation (Schwelle 0.5)
# ------------------------------------------------------------
pred_prob  <- predict(model_glm, newdata = test_df, type = "response")
pred_class <- ifelse(pred_prob >= 0.5, 1L, 0L)
actual     <- test_df$high_consumption

# ------------------------------------------------------------
# 5. Modellgüte
# ------------------------------------------------------------
conf_mat <- table(Predicted = pred_class, Actual = actual)
accuracy <- mean(pred_class == actual)

# AUC via Trapezregel (ohne externe Packages)
compute_auc <- function(actual, prob) {
  ord  <- order(prob, decreasing = TRUE)
  act  <- actual[ord]
  n1   <- sum(act == 1)
  n0   <- sum(act == 0)
  tpr  <- cumsum(act == 1) / n1
  fpr  <- cumsum(act == 0) / n0
  tpr  <- c(0, tpr)
  fpr  <- c(0, fpr)
  sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)
}

auc_val <- compute_auc(actual, pred_prob)

tp <- conf_mat["1", "1"]
tn <- conf_mat["0", "0"]
fp <- conf_mat["1", "0"]
fn <- conf_mat["0", "1"]

precision <- tp / (tp + fp)
recall    <- tp / (tp + fn)
f1        <- 2 * precision * recall / (precision + recall)

cat("Confusion Matrix:\n")
print(conf_mat)
cat("\n")
cat("Accuracy: ", round(accuracy,  4), "– Anteil korrekt klassifizierter Beobachtungen\n")
cat("AUC:      ", round(auc_val,   4), "– Trennschärfe (1 = perfekt, 0.5 = zufällig)\n")
cat("Precision:", round(precision, 4), "– Anteil echte High-Consumers unter allen als 'high' klassifizierten\n")
cat("Recall:   ", round(recall,    4), "– Anteil erkannter High-Consumers (Sensitivität)\n")
cat("F1-Score: ", round(f1,        4), "– Harmonisches Mittel von Precision und Recall\n")

# ------------------------------------------------------------
# 6. Visualisierungen mit Interpretation
# ------------------------------------------------------------

# -- 6a. Confusion Matrix ------------------------------------
conf_df <- data.frame(
  Predicted = factor(c("0", "0", "1", "1"), levels = c("1", "0")),
  Actual    = factor(c("0", "1", "0", "1"), levels = c("0", "1")),
  Count     = c(tn, fn, fp, tp),
  Label     = c("TN", "FN", "FP", "TP")
)

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = paste0(Label, "\n", Count)),
            size = 5, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "steelblue", high = "tomato") +
  scale_x_discrete(labels = c("0" = "Low (0)", "1" = "High (1)")) +
  scale_y_discrete(labels = c("0" = "Low (0)", "1" = "High (1)")) +
  labs(title    = "GLM Binomial – Confusion Matrix",
       subtitle = paste0("Accuracy = ", round(accuracy, 3),
                         "  |  F1 = ", round(f1, 3),
                         "  |  AUC = ", round(auc_val, 3)),
       x        = "Tatsächliche Klasse",
       y        = "Vorhergesagte Klasse",
       fill     = "Anzahl") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", panel.grid = element_blank())

specificity <- tn / (tn + fp)
cat(sprintf(paste0(
  "\nInterpretation Confusion Matrix:\n",
  "  Das Modell trifft bei %.1f%% aller Tage die richtige Vorhersage.\n",
  "  Von %d Tagen mit tatsaechlich hohem Stromverbrauch wurden %d%% korrekt\n",
  "  erkannt – %d Tage wurden faelschlicherweise als 'normal' eingestuft.\n",
  "  Von %d Tagen mit tatsaechlich normalem Verbrauch wurden %d%% korrekt\n",
  "  erkannt – %d Tage wurden faelschlicherweise als 'hoch' eingestuft.\n",
  "  Fazit: Das Modell erkennt beide Klassen gleich gut (%.0f%% vs %.0f%%).\n",
  "  Die ~%.0f%% Fehler auf beiden Seiten entstehen hauptsaechlich bei Haushalten\n",
  "  mit mittlerem Verbrauch, die nahe an der Entscheidungsgrenze liegen.\n"),
  accuracy * 100,
  tp + fn, round(recall * 100), fn,
  tn + fp, round(specificity * 100), fp,
  round(recall * 100), round(specificity * 100),
  round((1 - accuracy) * 100)))

# -- 6b. Odds-Ratio-Plot -------------------------------------
coef_summary <- coef(summary(model_glm))
coef_df <- data.frame(
  term    = rownames(coef_summary),
  estimate = coef_summary[, "Estimate"],
  se       = coef_summary[, "Std. Error"],
  p_value  = coef_summary[, "Pr(>|z|)"]
) |>
  filter(term != "(Intercept)") |>
  mutate(
    odds_ratio  = exp(estimate),
    ci_low      = exp(estimate - 1.96 * se),
    ci_high     = exp(estimate + 1.96 * se),
    significant = p_value < 0.05
  ) |>
  arrange(odds_ratio)

ggplot(coef_df, aes(x = odds_ratio, y = reorder(term, odds_ratio),
                    color = significant)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = "steelblue"),
                     labels = c("n.s.", "p < 0.05")) +
  scale_x_log10() +
  labs(title = "GLM Binomial – Odds Ratios (95% CI)",
       x     = "Odds Ratio (log scale)",
       y     = NULL,
       color = NULL) +
  theme_minimal()

# strukturelle Faktoren fuer Interpretation (Monatsvariablen ausblenden)
struct_pos <- coef_df |> filter(significant, odds_ratio > 1, !grepl("^month", term)) |>
  slice_max(odds_ratio, n = 1)
struct_neg <- coef_df |> filter(significant, odds_ratio < 1, !grepl("^month", term)) |>
  slice_min(odds_ratio, n = 1)
floor_heat <- coef_df |> filter(term == "has_floor_heating")
hp_ground  <- coef_df |> filter(term == "heatpump_typeground-source")

cat(sprintf(paste0(
  "\nInterpretation Odds-Ratio-Plot:\n",
  "  Dieser Plot zeigt, welche Faktoren einen Haushalt eher zu einem\n",
  "  Vielverbraucher machen. Punkte rechts der Mittellinie (OR > 1) erhoehen\n",
  "  das Risiko, Punkte links davon senken es. Blaue Punkte sind statistisch\n",
  "  zuverlaessig, graue koennen Zufall sein.\n\n",
  "  Groesster Einflussfaktor: Gebaeudetyp 'Haus' (OR = %.1f).\n",
  "  Einfamilienhaeuser haben eine %.1f-fach hoehere Wahrscheinlichkeit\n",
  "  fuer hohen Verbrauch als Mehrfamilienhaeuser – plausibel, da sie\n",
  "  groesser sind und mehr Flaeche zu heizen haben.\n\n",
  "  Erdwaermepumpe (OR = %.2f): Haushalte mit Erdwaermepumpe haben\n",
  "  ein um %.0f%% geringeres Risiko fuer hohen Verbrauch als Luftwaermepumpen –\n",
  "  Erdwaermepumpen sind effizienter, besonders bei kaltem Wetter.\n\n",
  "  Fussbodenheizung (OR = %.2f): erhoehte Wahrscheinlichkeit fuer hohen\n",
  "  Verbrauch. Dies wirkt kontraintuitiv, erklaert sich aber dadurch, dass\n",
  "  Fussbodenheizung oft in groesseren Haeusern verbaut ist (Konfundierung\n",
  "  mit Wohnflaeche).\n\n",
  "  Monatsvariablen (nicht beschriftet): Sommer hat deutlich tiefere OR als\n",
  "  Winter – rein saisonal bedingt durch den Wegfall des Heizbedarfs.\n"),
  struct_pos$odds_ratio, struct_pos$odds_ratio,
  hp_ground$odds_ratio, round((1 - hp_ground$odds_ratio) * 100),
  floor_heat$odds_ratio))

# -- 6c. ROC-Kurve -------------------------------------------
roc_df <- (function(actual, prob) {
  ord <- order(prob, decreasing = TRUE)
  act <- actual[ord]
  n1  <- sum(act == 1)
  n0  <- sum(act == 0)
  data.frame(
    fpr = c(0, cumsum(act == 0) / n0),
    tpr = c(0, cumsum(act == 1) / n1)
  )
})(actual, pred_prob)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  labs(title    = "GLM Binomial – ROC-Kurve",
       subtitle = paste0("AUC = ", round(auc_val, 3)),
       x        = "False Positive Rate",
       y        = "True Positive Rate") +
  theme_minimal()

cat(sprintf(paste0(
  "\nInterpretation ROC-Kurve:\n",
  "  Diese Kurve zeigt, wie gut das Modell Viel- und Wenigverbraucher\n",
  "  unterscheiden kann – unabhaengig davon, wo man die Entscheidungsgrenze zieht.\n",
  "  Die gestrichelte Linie waere ein Modell, das nur rät (Muenzwurf).\n",
  "  Je weiter die blaue Kurve links oben liegt, desto besser das Modell.\n",
  "  Mit einem AUC-Wert von %.3f liegt die Treffsicherheit bei %.1f%%:\n",
  "  Nimmt man zwei zufaellige Haushalte – einen Viel- und einen Wenigverbraucher –\n",
  "  sagt das Modell in %.1f%% der Faelle korrekt voraus, welcher der Vielverbraucher ist.\n"),
  auc_val, auc_val * 100, auc_val * 100))

# -- 6d. Vorhergesagte Wahrscheinlichkeiten nach Klasse ------
prob_df <- data.frame(prob = pred_prob, actual = factor(actual))
low_q   <- quantile(pred_prob[actual == 0], c(0.25, 0.5, 0.75))
high_q  <- quantile(pred_prob[actual == 1], c(0.25, 0.5, 0.75))

ggplot(prob_df, aes(x = prob, fill = actual)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity", color = "white") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato"),
                    labels = c("Low", "High")) +
  labs(title = "Vorhergesagte Wahrscheinlichkeiten nach Klasse",
       x     = "P(high_consumption = 1)",
       y     = "Count",
       fill  = "Tatsächliche Klasse") +
  theme_minimal()

cat(sprintf(paste0(
  "\nInterpretation Wahrscheinlichkeitsverteilung:\n",
  "  Das Modell weist jedem Tag eine Wahrscheinlichkeit zwischen 0 und 1 zu,\n",
  "  wie stark es einen Vielverbraucher-Tag einschaetzt.\n",
  "  Rot = Tage, die tatsaechlich hohen Verbrauch hatten.\n",
  "  Blau = Tage mit tatsaechlich niedrigem Verbrauch.\n",
  "  Ein gutes Modell wuerde zwei klar getrennte Hügel zeigen.\n",
  "  Hier liegen die Durchschnittswerte bei %.2f (niedrig) und %.2f (hoch).\n",
  "  Die Ueberlappung in der Mitte zeigt: bei Tagen mit mittlerem Verbrauch\n",
  "  ist das Modell unsicher – das ist erwartbar, da die Grenze zwischen\n",
  "  'hoch' und 'niedrig' genau am Durchschnitt aller Haushalte gezogen wurde.\n"),
  low_q[2], high_q[2]))
