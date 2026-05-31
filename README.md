# HSLU AML 01 — Household Energy Consumption Analysis

**Applied Machine Learning and Predictive Modelling 1 · FS 2026 · Hochschule Luzern**

https://github.com/raphaelspgll/HSLU_AML_01

---

## Description

This repository contains the full analysis pipeline for a university group project at HSLU. The project applies statistical and machine learning models to predict daily electricity consumption of Swiss households equipped with heat pumps.

**Dataset:** [HEAPO – Household Energy and Smart Meter Data](https://zenodo.org/records/15056919) (Zenodo, 2025)  
A Swiss research dataset covering daily smart meter readings from heat pump households paired with weather observations and household survey data.

**ML problem:**
- **Regression** — predict `log(daily kWh total)` from weather and building covariates
- **Classification** — classify each household-day as *high consumption* (top 25% per household) or *normal*

Six models are implemented and compared: Linear Model, GLM Poisson, GLM Binomial, Generalised Additive Model (GAM), Neural Network, and Support Vector Machine (SVM).

---

## Table of Contents

1. [Project Structure](#project-structure)
2. [Prerequisites](#prerequisites)
3. [Setup & Installation](#setup--installation)
4. [Usage](#usage)
5. [Models](#models)
6. [Results](#results)
7. [Authors](#authors)

---

## Project Structure

```
HSLU_AML_01/
├── HSLU_AML_01.Rproj               # Open this in RStudio (sets working directory)
├── README.md
├── SETUP.md                         # Detailed setup and troubleshooting guide
│
├── R/
│   ├── setup.R                      # One-time: initialise renv + install packages + download data
│   ├── download_data.R              # Standalone: download HEAPO ZIP from Zenodo
│   │
│   ├── 00_data_preparation.R        # Build annual dataset (956 obs × 22 vars)
│   ├── 00_data_preparation_daily.R  # Build daily dataset (~1.4 M obs × 25 vars)
│   ├── 00_data_preparation_daily.Rmd  # Documented version of the daily preparation
│   │
│   ├── 01_data_quality_report.Rmd   # Data quality and suitability report (HTML)
│   ├── 02_data_filtration_EDA.Rmd   # Filter to modelling subset + full EDA (HTML)
│   ├── 02_EDA_short.Rmd             # Condensed 2-page EDA summary (HTML)
│   │
│   ├── 3.1_linear_model.R           # Linear Model on log(kWh_total)
│   ├── 3.2_poisson_pipeline.R       # GLM Poisson — count of high-consumption days/month
│   ├── 3.3_glm_binomial.R           # GLM Binomial — binary high-consumption classification
│   ├── 3.4_gam_model.R              # Generalised Additive Model
│   ├── 3.5_neural_network.R         # Neural Network (nnet, 5-fold CV)
│   ├── 3.6_svm.R                    # Support Vector Machine (RBF kernel, tuned)
│   │
│   ├── 04_model_comparison_cv.R     # 5-fold CV comparison: LM vs GAM (regression)
│   └── 04_model_comparison_cv_classification.R  # 5-fold CV: GLM Binomial vs SVM
│
├── data_raw/
│   └── heapo_data/                  # Raw HEAPO data — NOT tracked by Git (download manually)
│       ├── meta_data/
│       ├── smart_meter_data/
│       ├── weather_data/
│       └── reports/
│
├── data_processed/
│   └── heapo/                       # Processed datasets
│       ├── heapo_dataset.rds/.csv   # Annual dataset — NOT tracked (generated locally)
│       ├── heapo_dataset_daily.rds  # Full daily dataset — NOT tracked (generated locally)
│       └── heapo_modelling.rds/.csv # Filtered modelling subset — TRACKED (ready to use)
│   └── poisson/
│       └── dat_count.rds            # Poisson aggregated counts — TRACKED
│
├── models/                          # Saved model objects (.rds)
│   ├── linear_model/
│   ├── poisson_glm/
│   ├── gam/
│   ├── neural_network/
│   └── svm/
│
├── report/
│   └── report.Rmd                   # Final course report (HTML + PDF)
│
└── renv/                            # renv project library (auto-generated, committed)
```

> **What is tracked by Git:** `data_processed/heapo/heapo_modelling.rds/.csv` and `data_processed/poisson/dat_count.rds` are committed — **model scripts work immediately after cloning** without any data preparation. Raw data and large intermediate datasets (`heapo_dataset_daily.rds`, `heapo_dataset.rds`) are excluded and must be generated locally if needed.

---

## Prerequisites

| Requirement | Version |
|---|---|
| [R](https://www.r-project.org/) | ≥ 4.1 |
| [RStudio](https://posit.co/download/rstudio-desktop/) | Recommended |
| RAM | ≥ 8 GB (daily dataset loads ~1 400 CSV files) |
| Disk space | ~6 GB (raw data ~5 GB, processed ~1 GB) |

Key R packages (managed via `renv`):

| Category | Packages |
|---|---|
| Data wrangling | `dplyr`, `tidyr`, `readr`, `lubridate`, `purrr` |
| Visualisation | `ggplot2`, `scales` |
| Modelling | `mgcv` (GAM), `nnet` (NN), `e1071` / `kernlab` (SVM), `caret` |
| Diagnostics | `broom`, `car`, `ROCR` |
| Reporting | `knitr`, `kableExtra`, `rmarkdown` |
| Paths | `here` |

---

## Setup & Installation

### 1. Clone the repository

```bash
git clone <repo-url>
cd HSLU_AML_01
```

The modelling dataset (`heapo_modelling.rds/.csv`) and Poisson counts (`dat_count.rds`) are included in the repository — **you can run all model scripts immediately after cloning**, without downloading or preparing any data.

### 2. Open the project in RStudio

Double-click `HSLU_AML_01.Rproj`. RStudio sets the working directory to the project root automatically — **all relative paths depend on this**.

### 3. Restore the R environment

If a `renv.lock` file is present (committed by a teammate), run once in the RStudio console:

```r
renv::restore()
```

RStudio may prompt this automatically on project open — click **Restore**.

> **First-time setup (one team member only):** If `renv.lock` does not yet exist, run `source("R/setup.R")` to initialise renv, install packages, download data, then commit the generated `renv.lock` and `renv/` files.

### 4. Download the raw data *(optional — only needed to re-run data preparation)*

The raw data (~438 MB ZIP, ~5 GB extracted) is not tracked by Git. Skip this step if you only need to run models. Choose one option:

**Option A — automatic (via `download_data.R`):**
```r
source("R/download_data.R")
```

**Option B — manual:**
1. Download from: `https://zenodo.org/records/15056919/files/heapo_data.zip?download=1`
2. Unzip into `data_raw/` so the structure matches:
   ```
   data_raw/
   └── heapo_data/
       ├── meta_data/
       ├── smart_meter_data/
       ├── weather_data/
       └── reports/
   ```

---

## Usage

Run all scripts from RStudio with `HSLU_AML_01.Rproj` open. Execute steps in order — each step depends on the output of the previous one.

### Step 1 — Data Preparation *(optional)*

> **Skip this step** if you just want to run models — `heapo_modelling.rds` is already in the repository.
> Only needed if you want to regenerate datasets from the raw HEAPO data (requires Step 4 of Setup).

| Order | Script | Output | Notes |
|---|---|---|---|
| 1a | `R/00_data_preparation.R` | `data_processed/heapo/heapo_dataset.rds/.csv` | Annual dataset, 956 obs. |
| 1b | `R/00_data_preparation_daily.R` | `data_processed/heapo/heapo_dataset_daily.rds` | Daily dataset, ~1.4 M obs. May take several minutes. |

```r
source("R/00_data_preparation.R")
source("R/00_data_preparation_daily.R")
```

### Step 2 — Data Quality & EDA *(optional)*

> **Skip this step** if you are not reproducing the data pipeline from scratch.
> `02_data_filtration_EDA.Rmd` regenerates `heapo_modelling.rds` — skip if the committed version is sufficient.

Knit the following `.Rmd` files via RStudio (**Knit** button or `rmarkdown::render()`):

| Order | Script | Output |
|---|---|---|
| 2a | `R/01_data_quality_report.Rmd` | HTML quality report |
| 2b | `R/02_data_filtration_EDA.Rmd` | `data_processed/heapo/heapo_modelling.rds/.csv` + HTML EDA |
| 2c | `R/02_EDA_short.Rmd` | Condensed HTML EDA summary |

### Step 3 — Model Fitting

Run scripts or knit notebooks in any order (all read from `heapo_modelling.rds`):

| Script | Task | Method |
|---|---|---|
| `R/3.1_linear_model.R` | `source()` | Linear Model |
| `R/3.2_poisson_pipeline.R` | `source()` | GLM Poisson |
| `R/3.3_glm_binomial.R` | `source()` | GLM Binomial |
| `R/3.4_gam_model.R` | `source()` | GAM |
| `R/3.5_neural_network.R` | `source()` | Neural Network |
| `R/3.6_svm.R` | `source()` | SVM |

> **Neural Network:** Set `to_be_run <- TRUE` on line 88 of `3.5_neural_network.R` to retrain from scratch (slow). The default loads pre-saved CV results.

### Step 4 — Model Comparison

```r
source("R/04_model_comparison_cv.R")              # LM vs GAM (regression)
source("R/04_model_comparison_cv_classification.R") # GLM Binomial vs SVM (classification, ~30–60 min)
```

### Step 5 — Final Report

Knit `report/report.Rmd` in RStudio to produce the full HTML and PDF report.

---

## Models

All models use the filtered modelling dataset: households observed between 2022-01-01 and 2024-02-29 with ≥95% daily coverage (~100 000 household-days across ~400 households).

### Regression task — predict `log(daily kWh total)`

| Model | Script | Description |
|---|---|---|
| **Linear Model (LM)** | `3.1_linear_model.R` | OLS on log-transformed daily consumption. Includes interaction terms `heating_degree_days × building_type` and `heating_degree_days × heatpump_type`. Interpretable baseline. |
| **GAM** | `3.4_gam_model.R` | Generalised Additive Model with smooth spline terms `s(heating_degree_days)`, `s(temp_avg)`, `s(living_area)`. Captures non-linear weather effects without requiring interaction specification. |

### Classification task — predict `high_consumption` (top 25% per household)

| Model | Script | Description |
|---|---|---|
| **GLM Binomial** | `3.3_glm_binomial.R` | Logistic regression. 80/20 train-test split. Reports accuracy, AUC, precision, recall, F1. |
| **GLM Poisson** | `3.2_poisson_pipeline.R` | Poisson GLM on aggregated household-month counts of high-consumption days, with log(n_days) offset. Includes overdispersion check and quasi-Poisson comparison. |
| **Neural Network** | `3.5_neural_network.R` | Single hidden layer (25 units, `nnet`), weight decay 0.05, feature scaling via model matrix. 5-fold stratified CV. |
| **SVM** | `3.6_svm.R` | RBF kernel (`kernlab`). Baseline at C=1 with sigma estimated from training data. Tuned by 5-fold CV over C ∈ {1, 10, 100} and sigma ∈ {0.01, 0.1, 1/7}. Best parameters: C=100, σ=1/7≈0.1429. Per-household 75th percentile threshold, same as GLM Binomial. |

**Key predictors across all models:**

| Predictor | Type | Role |
|---|---|---|
| `heating_degree_days` | Continuous | Primary weather driver (cold-weather demand) |
| `temp_avg` | Continuous | Daily average temperature |
| `living_area` | Continuous | Heated floor area (m²) |
| `building_type` | Categorical | Apartment vs. house |
| `heatpump_type` | Categorical | Air-source vs. ground-source |
| `n_residents` | Count | Number of occupants |
| `is_weekend` | Binary | Weekend effect |

---

## Results

Model comparison is performed via 5-fold cross-validation on the shared modelling dataset. Final results are saved to:

- `models/cv_reg_results.rds` — MAE (kWh/day) and R² for LM vs GAM
- `models/cv_class_results.rds` — AUC and accuracy for GLM Binomial, SVM, and Neural Network

**Regression (LM vs GAM):** The GAM captures non-linear temperature effects and is expected to outperform the extended LM on out-of-fold MAE, at the cost of interpretability.

**Classification (GLM Binomial, SVM, Neural Network):** The SVM with tuned RBF kernel and the Neural Network are expected to achieve higher AUC than the logistic regression. Sensitivity (recall of high-consumption days) is the primary business-relevant metric.

> Full results with coefficient tables, diagnostic plots, ROC curves, and cross-validation summaries are in `report/report.Rmd`.

---

## Authors

- **Emanuel Lemma**
- **Raphaël Spagolla**
- **Tharrmeehan Krishnathasan**

HSLU — Hochschule Luzern · Applied Machine Learning and Predictive Modelling 1 · FS 2026
