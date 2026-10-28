# scripts/05_load_db.R -- store the five tables in SQLite with their keys and links.
# Reads: data/processed/*.csv        Writes: DB_PATH (config.R)
# The database is built by this script: never edit it by hand -- change a script and rerun.
source("config.R")
source("R/load_functions.R")
log_msg("05_load_db: start")

stations      <- read.csv("data/processed/stations.csv")
weather_daily <- read.csv("data/processed/weather_daily.csv")
counters      <- read.csv("data/processed/counters.csv")
trail_counts  <- read.csv("data/processed/trail_counts.csv")
trail_weather <- read.csv("data/processed/trail_weather.csv")

con <- db_connect_fresh(DB_PATH)
# parent tables first, then the tables that point to them (FOREIGN KEY)
db_write(con, "stations",      stations,      key = "station_code")
db_write(con, "counters",      counters,      key = "counter_id")
db_write(con, "weather_daily", weather_daily, key = c("station_code", "date"),
         references = c(station_code = "stations"))
db_write(con, "trail_counts",  trail_counts,  key = c("counter_id", "date"),
         references = c(counter_id = "counters"))
db_write(con, "trail_weather", trail_weather, key = c("counter_id", "date"),
         references = c(counter_id = "counters"))
print(db_tables(con))
dbDisconnect(con)
log_msg("05_load_db: done -- ", DB_PATH)
