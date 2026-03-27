# (2) Quick EDA for monthly count outcome (minimal requirements)
# Input : data_processed/poisson/dat_count.rds
# Output: report/figures/poisson_glm/*.png
#         models/poisson_glm/eda_summary.txt

suppressPackageStartupMessages({
  library(dplyr)
})

# -----------------------------
# Paths
# -----------------------------
path_in  <- "data_processed/poisson/dat_count.rds"
fig_dir  <- "report/figures/poisson_glm"
out_txt  <- "models/poisson_glm/eda_summary.txt"

# Create output directories if needed
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(dirname(out_txt))) dir.create(dirname(out_txt), recursive = TRUE)

# -----------------------------
# Load
# -----------------------------
if (!file.exists(path_in)) stop("Input file not found: ", path_in)
dat_count <- readRDS(path_in)

# -----------------------------
# Minimal checks
# -----------------------------
required_cols <- c(
  "n_high_days", "n_days",
  "heating_degree_days", "temp_avg", "living_area",
  "building_type", "heatpump_type",
  "has_floor_heating", "n_residents", "share_weekend"
)
missing_cols <- setdiff(required_cols, names(dat_count))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# -----------------------------
# Text summary (minimal EDA numbers)
# -----------------------------
share_zeros <- mean(dat_count$n_high_days == 0)
summ_y <- summary(dat_count$n_high_days)
summ_exp <- summary(dat_count$n_days)

txt <- c(
  "Quick EDA summary (household-month level)",
  "=======================================",
  paste0("Rows: ", nrow(dat_count)),
  "",
  "Outcome: n_high_days",
  paste0("Share of zeros: ", round(share_zeros, 4)),
  paste0("Summary: ", paste(names(summ_y), as.numeric(summ_y), sep="=", collapse=", ")),
  "",
  "Exposure: n_days",
  paste0("Summary: ", paste(names(summ_exp), as.numeric(summ_exp), sep="=", collapse=", ")),
  ""
)

writeLines(txt, con = out_txt)
cat("\n[02] Saved EDA text summary:", out_txt, "\n")

# -----------------------------
# Plots (base R for portability)
# -----------------------------

# 1) Histogram of n_high_days
png(file.path(fig_dir, "01_hist_n_high_days.png"), width = 1200, height = 800, res = 150)
hist(dat_count$n_high_days,
     breaks = 32,
     main = "Histogram: n_high_days (count per household-month)",
     xlab = "n_high_days",
     ylab = "Frequency")
dev.off()

# 2) Barplot of discrete counts (helps because counts are integers)
count_tab <- table(dat_count$n_high_days)
png(file.path(fig_dir, "02_bar_n_high_days.png"), width = 1200, height = 800, res = 150)
barplot(count_tab,
        main = "Barplot: n_high_days (discrete counts)",
        xlab = "n_high_days",
        ylab = "Number of household-months")
dev.off()

# 3) Boxplot by building_type
png(file.path(fig_dir, "03_box_by_building_type.png"), width = 1200, height = 800, res = 150)
boxplot(n_high_days ~ building_type, data = dat_count,
        main = "n_high_days by building_type",
        xlab = "building_type",
        ylab = "n_high_days")
dev.off()

# 4) Boxplot by heatpump_type
png(file.path(fig_dir, "04_box_by_heatpump_type.png"), width = 1200, height = 800, res = 150)
boxplot(n_high_days ~ heatpump_type, data = dat_count,
        main = "n_high_days by heatpump_type",
        xlab = "heatpump_type",
        ylab = "n_high_days")
dev.off()

# 5) Boxplot by has_floor_heating
png(file.path(fig_dir, "05_box_by_has_floor_heating.png"), width = 1200, height = 800, res = 150)
boxplot(n_high_days ~ has_floor_heating, data = dat_count,
        main = "n_high_days by has_floor_heating",
        xlab = "has_floor_heating (0/1)",
        ylab = "n_high_days")
dev.off()

# 6) Scatter: n_high_days vs temp_avg
png(file.path(fig_dir, "06_scatter_n_high_days_vs_temp_avg.png"), width = 1200, height = 800, res = 150)
plot(dat_count$temp_avg, dat_count$n_high_days,
     main = "n_high_days vs temp_avg",
     xlab = "temp_avg",
     ylab = "n_high_days")
dev.off()

cat("[02] Saved plots to:", fig_dir, "\n")
cat("[02] Files:\n")
cat(" - 01_hist_n_high_days.png\n")
cat(" - 02_bar_n_high_days.png\n")
cat(" - 03_box_by_building_type.png\n")
cat(" - 04_box_by_heatpump_type.png\n\n")
cat(" - 05_box_by_has_floor_heating.png\n")
cat(" - 06_scatter_n_high_days_vs_temp_avg.png\n\n")