# scripts/02_clean.R -- fix values, one decision at a time.
# Reads: data/interim/*_raw.csv        Writes: data/interim/*_clean.csv
# Every step has the same three parts -- use this pattern in your own project:
#   - FOUND:    what an inspect function showed
#   - DECISION: which toolkit function fixes it, and why that is the right fix
#   - CHECK:    how we know it worked
source("config.R")
source("R/load_functions.R")
log_msg("02_clean: start")

# =============================== weather ===============================================
weather <- read_raw_csv("data/interim/weather_raw.csv") |>
  clean_names()             # "Max Temp (°C)" -> max_temp_c: names that are easy to type

# FOUND:    show_duplicate_keys(weather, c("climate_id", "date_time")) shows January 31
#           twice for each station -- the February export overlaps January.
# DECISION: remove_duplicates(), because the two copies are identical. source_file is
#           the only column that differs, so drop it first.
weather <- weather |>
  select(-source_file) |>
  remove_duplicates("weather")

# FOUND:    temperatures, rain and snow are text; stations are named by climate_id.
# DECISION: to_number() for the measurements, recode_values() for the station codes
#           (STATION_CODES in config.R), as.Date() for the date (one format only).
weather <- weather |>
  mutate(station_code = recode_values(climate_id, STATION_CODES),
         date         = as.Date(date_time),
         max_temp_c   = to_number(max_temp_c),
         min_temp_c   = to_number(min_temp_c),
         mean_temp_c  = to_number(mean_temp_c),
         rain_mm      = to_number(total_rain_mm),
         snow_cm      = to_number(total_snow_cm)) |>
  select(station_code, date, max_temp_c, min_temp_c, mean_temp_c, rain_mm, snow_cm)

# FOUND:    show_out_of_range(weather, "max_temp_c", -50, 45) shows one day at 99.9 °C.
# DECISION: set_out_of_range_to_na(): Edmonton is never above 45 °C, so it is a sensor
#           error -- NA, never 0 or a guess.
weather$max_temp_c <- set_out_of_range_to_na(weather$max_temp_c, -50, 45, "max_temp_c")

# CHECK:    one row per station per day.
check_key(weather, c("station_code", "date"), "weather")
save_csv(weather, "data/interim/weather_clean.csv")

# =============================== trail counts ==========================================
counts <- read_raw_csv("data/interim/trail_counts_raw.csv")

# FOUND:    find_text_variants(counts, "counter_id") shows "rv-02" next to "RV-02" and
#           " KF-04" next to "KF-04".
# DECISION: standardize_text(), because the differences are only case and spaces.
# FOUND:    trips is text, and big counts are written with a comma ("1,110").
# DECISION: to_number(), which removes the comma before converting.
counts <- counts |>
  mutate(counter_id = standardize_text(counter_id),
         date       = as.Date(date),
         trips      = to_number(trips))

# FOUND:    show_out_of_range(counts, "trips", 0, 20000) shows one day with 99999 trips.
# DECISION: set_out_of_range_to_na(): no counter records 20,000 trips a day; it is a glitch.
counts$trips <- set_out_of_range_to_na(counts$trips, 0, 20000, "trips")

# CHECK:    one row per counter per day.
check_key(counts, c("counter_id", "date"), "trail_counts")
save_csv(counts, "data/interim/trail_counts_clean.csv")

# =============================== counters and stations =================================
# FOUND:    profile_raw_file("data/raw/counters.csv") says installed is a "date in 2 formats".
# DECISION: parse_dates_mixed(), which reads both 2019-05-14 and 06/02/2020.
counters <- read_raw_csv("data/interim/counters_raw.csv") |>
  mutate(installed = parse_dates_mixed(installed))
check_key(counters, "counter_id", "counters")
save_csv(counters, "data/interim/counters_clean.csv")

stations <- read_raw_csv("data/interim/stations_raw.csv") |>
  mutate(elevation_m = to_number(elevation_m))
check_key(stations, "station_code", "stations")
save_csv(stations, "data/interim/stations_clean.csv")

log_msg("02_clean: done")
