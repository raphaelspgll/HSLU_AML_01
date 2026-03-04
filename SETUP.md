# Project Setup Guide

## First-time setup (run once per machine)

### Prerequisites
- [R](https://www.r-project.org/) >= 4.1
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)

### Steps

1. **Clone the repository**
   ```bash
   git clone <repo-url>
   cd HSLU_AML_01
   ```

2. **Open the project in RStudio**
   Double-click `HSLU_AML_01.Rproj`.
   RStudio automatically sets the working directory to the project root — this is required for relative paths to work correctly.

3. **First person only: run the setup script**
   In the RStudio console:
   ```r
   source("R/setup.R")
   ```
   This will:
   - Initialise `renv` for the project
   - Install all required R packages into a project-local library
   - Create `renv.lock` (the reproducibility snapshot)

   Then commit the generated files:
   ```bash
   git add renv.lock renv/
   git commit -m "Add renv lockfile"
   git push
   ```

4. **All other team members: restore the environment**
   After `renv.lock` is committed, teammates only need to run once:
   ```r
   renv::restore()
   ```
   RStudio may prompt this automatically when opening the project — just click **Restore**.

---

## Downloading the raw data

The raw data is **not tracked by Git** (5 GB). Download it once manually:

1. Download the ZIP (~438 MB):
   ```
   https://zenodo.org/records/15056919/files/heapo_data.zip?download=1
   ```
2. Unzip into `data_raw/` so the structure looks like:
   ```
   data_raw/
   └── heapo_data/
       ├── meta_data/
       ├── smart_meter_data/
       ├── weather_data/
       └── reports/
   ```

---

## Running the data preparation

Make sure `data_raw/heapo_data/` is present before running (raw data is not tracked by Git).

Run scripts in this order from RStudio (with the `.Rproj` open):

| Step | Script | Output | Notes |
|------|--------|--------|-------|
| 1 | `R/01_data_preparation.R` | `data_processed/heapo/heapo_dataset.rds/.csv` | Annual dataset, 956 obs. Optional if only daily data is needed. |
| 2 | `R/02_data_preparation_daily.R` | `data_processed/heapo/heapo_dataset_daily.rds/.csv` | Daily dataset, ~1.4M obs. Main dataset for modelling. |
| 3 | Knit `R/03_data_quality_report.Rmd` | HTML or PDF report | Requires both datasets from steps 1 & 2. |

> **Note:** Step 2 reads ~1,400 individual CSV files and may take several minutes to run.

---

## Project structure

```
HSLU_AML_01/
├── HSLU_AML_01.Rproj           # Open this in RStudio
├── SETUP.md                    # This guide
├── R/
│   ├── setup.R                 # Run once: initialise renv + install packages
│   ├── 01_data_preparation.R   # Annual dataset (956 obs x 22 vars)
│   ├── 02_data_preparation_daily.R    # Daily dataset (~1.4M obs x 25 vars)
│   ├── 02_data_preparation_daily.Rmd  # Daily dataset – documented version
│   └── 03_data_quality_report.Rmd     # Data quality & suitability report
├── data_raw/
│   └── heapo_data/             # Raw HEAPO data (not tracked by Git)
├── data_processed/
│   └── heapo/                  # Processed datasets (not tracked by Git)
├── models/                     # Saved model objects
├── report/                     # Final course report
└── renv/                       # renv project library (auto-generated)
```

---

## Troubleshooting

**`renv::restore()` fails with package errors**
Try updating renv itself first: `renv::upgrade()`, then retry `renv::restore()`.

**Paths not found when running scripts**
Make sure you opened the project via `HSLU_AML_01.Rproj` — not by navigating to a file directly. The `.Rproj` file sets the working directory to the project root automatically.

**Step 2 (daily prep) runs out of memory**
The daily dataset loads ~1,400 files using `map_dfr()`. On machines with less than 8 GB RAM, close other applications before running.
