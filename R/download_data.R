# 6. Download data
library("here")
options(timeout = 600)

zip_url <- "https://zenodo.org/records/15056919/files/heapo_data.zip?download=1"
dest_dir <- here("data_raw")
zip_file <- file.path(dest_dir, "heapo_data.zip")

message("Downloading data from Zenodo...")
download.file(url = zip_url, destfile = zip_file, mode = "wb")

message("Unzipping files...")
unzip(zipfile = zip_file, exdir = dest_dir)

file.remove(zip_file)

message("Done! Data is ready in: ", dest_dir)
