# scripts/03_transform.R -- the analysis table: counts + counter details + weather.
# Reads: data/interim/*_clean.csv      Writes: data/processed/*.csv
source("config.R")
source("R/load_functions.R")
log_msg("03_transform: start")

# read.csv() reads dates as text, so as.Date() again
weather  <- read.csv("data/interim/weather_clean.csv")      |> mutate(date = as.Date(date))
stations <- read.csv("data/interim/stations_clean.csv")
counters <- read.csv("data/interim/counters_clean.csv")
counts   <- read.csv("data/interim/trail_counts_clean.csv") |> mutate(date = as.Date(date))

# FOUND:    find_unmatched(counts, counters, "counter_id") shows 20 rows from KF-09, a
#           counter that is not in the counter list.
# DECISION: set them aside in output/unknown_counter_rows.csv (evidence, not lost) and
#           keep only known counters -- in the database a count must point to a counter.
unknown <- find_unmatched(counts, counters, "counter_id")
save_csv(unknown, "output/unknown_counter_rows.csv")
counts <- filter(counts, counter_id %in% counters$counter_id)

# One weather row per day: the analysis station only (config.R), plus the snow-day label.
# Joining both stations by date would double every count (safe_left_join() would stop).
weather_one <- weather |>
  filter(station_code == ANALYSIS_STATION) |>
  select(date, mean_temp_c, snow_cm) |>
  mutate(snow_day = make_bands(snow_cm, cutoffs = SNOW_DAY_CM, labels = c("no snow", "snow day")))
check_key(weather_one, "date", "weather_one")

trail_weather <- counts |>
  add_date_parts("date") |>
  safe_left_join(select(counters, counter_id, location, trail_type), "counter_id", "counts + counters") |>
  safe_left_join(weather_one, "date", "counts + weather")
check_key(trail_weather, c("counter_id", "date"), "trail_weather")

save_csv(stations,      "data/processed/stations.csv")
save_csv(weather,       "data/processed/weather_daily.csv")
save_csv(counters,      "data/processed/counters.csv")
save_csv(counts,        "data/processed/trail_counts.csv")
save_csv(trail_weather, "data/processed/trail_weather.csv")
log_msg("03_transform: done")
