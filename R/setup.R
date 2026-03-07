###############################################################################
# HEAPO Project Setup
# Run this script ONCE after cloning the repository.
# It initialises renv and installs all required R packages.
# After completion, your environment is fully reproducible via renv.lock.
#
# Usage: source("R/setup.R")  (from RStudio with HSLU_AML_01.Rproj open)
###############################################################################

message("=== HEAPO Project Setup ===\n")

# 1. Install renv if not already available
#    Uses Posit Package Manager as fallback (more reliable for newer R versions)
if (!requireNamespace("renv", quietly = TRUE)) {
  message("Installing renv...")
  install.packages("renv",
                   repos = "https://packagemanager.posit.co/cran/latest")
}

# 2. Initialise renv for this project
#    bare = TRUE: only sets up infrastructure, does not auto-scan for packages
message("Initialising renv...")
renv::init(bare = TRUE)

# 3. All packages required by the R scripts and Rmd reports
packages <- c(
  # Data wrangling
  "dplyr",
  "readr",
  "tidyr",
  "lubridate",
  "purrr",
  # Visualisation
  "ggplot2",
  "scales",
  # Reporting
  "knitr",
  "kableExtra",
  "rmarkdown"
)

# 4. Install all packages into the project-local renv library
message("Installing packages: ", paste(packages, collapse = ", "), "\n")
renv::install(packages)

# 5. Snapshot the installed versions into renv.lock
message("Creating renv.lock snapshot...")
renv::snapshot()

message("\n=== Setup complete! ===")
message("Next steps:")
message("  1. Commit renv.lock and renv/ to Git.")
message("  2. Teammates clone the repo, open HSLU_AML_01.Rproj,")
message("     and run renv::restore() to reproduce this environment.")
message("\nSee SETUP.md for the full workflow.")
