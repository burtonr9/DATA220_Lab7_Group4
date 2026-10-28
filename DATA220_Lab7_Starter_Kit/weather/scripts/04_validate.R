# scripts/04_validate.R -- checks that STOP if something is wrong. If this script
# reaches its last line, every check passed.  Writes: output/quality_report.csv
source("config.R")
source("R/load_functions.R")
log_msg("04_validate: start")

stations      <- read.csv("data/processed/stations.csv")
weather_daily <- read.csv("data/processed/weather_daily.csv")
counters      <- read.csv("data/processed/counters.csv")
trail_counts  <- read.csv("data/processed/trail_counts.csv")
trail_weather <- read.csv("data/processed/trail_weather.csv")

check_key(stations,      "station_code",            "stations")
check_key(weather_daily, c("station_code", "date"), "weather_daily")
check_key(counters,      "counter_id",              "counters")
check_key(trail_counts,  c("counter_id", "date"),   "trail_counts")
check_key(trail_weather, c("counter_id", "date"),   "trail_weather")

check_range(weather_daily$max_temp_c, -50, 45, "max_temp_c")
check_range(trail_counts$trips, 0, 20000, "trips")
check_allowed(trail_weather$snow_day, c("no snow", "snow day"), "snow_day")
check_same_total(trail_counts$trips, trail_weather$trips, "trips")   # nothing lost in the joins

save_quality_report(
  report_row("02_clean",     "weather_daily", 182, nrow(weather_daily), "Jan 31 copy removed; 99.9 max temp -> NA"),
  report_row("02_clean",     "counters",      4,   nrow(counters),      "installed dates in one format"),
  report_row("03_transform", "trail_counts",  375, nrow(trail_counts),  "KF-09 rows set aside (unknown counter)"),
  report_row("03_transform", "trail_weather", nrow(trail_counts), nrow(trail_weather), "counter details + EDM-A weather")
)
log_msg("04_validate: all checks passed")
