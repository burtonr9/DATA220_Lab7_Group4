# scripts/01_extract.R -- read every raw file and keep an untouched copy.
# Reads: data/raw/        Writes: data/interim/*_raw.csv
# Nothing is cleaned here: every column stays text, exactly as delivered.
source("config.R")
source("R/load_functions.R")
make_folders()
log_msg("01_extract: start")

stations_raw <- read_raw_csv("data/raw/stations.csv")
counters_raw <- read_raw_csv("data/raw/counters.csv")
counts_raw   <- read_raw_csv("data/raw/trail_counts.csv")

# The weather comes as one export per month. The March export calls the snow column
# "Snow (cm)" instead of "Total Snow (cm)": rename it while reading, so the months stack.
weather_raw <- read_many_csv("data/raw/weather", rename = c("Snow (cm)" = "Total Snow (cm)"))

save_csv(stations_raw, "data/interim/stations_raw.csv")
save_csv(counters_raw, "data/interim/counters_raw.csv")
save_csv(counts_raw,   "data/interim/trail_counts_raw.csv")
save_csv(weather_raw,  "data/interim/weather_raw.csv")
log_msg("01_extract: done")
