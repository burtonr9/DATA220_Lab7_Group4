# scripts/01_extract.R -- P5: read every source file and keep an untouched copy.
# Reads: data/raw/        Writes: data/interim/<source>_raw.csv
source("config.R")
source("R/load_functions.R")
make_folders()
log_msg("01_extract: start")

# =============================================================================
# TASK P5 -- for every source: read its file, then save an untouched copy
# -----------------------------------------------------------------------------
# Pick the reading function by what the source is (your profile told you):
#   one CSV file              read_raw_csv(SOURCES$<name>$file)
#   a folder of CSV files     read_many_csv(SOURCES$<name>$file)
#   Excel, JSON (an API sample) or TSV     read_any_file(SOURCES$<name>$file)
# then save it:               save_csv(<data>, "data/interim/<name>_raw.csv")
# Example (a folder of monthly CSV files):
#   sales_raw <- read_many_csv(SOURCES$sales$file)
#   save_csv(sales_raw, "data/interim/sales_raw.csv")
# =============================================================================

# your calls here


log_msg("01_extract: done")
