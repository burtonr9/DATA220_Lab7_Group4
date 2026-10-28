# scripts/06_queries.R -- run every query in sql/ against the database we built and
# save each result (sql/q1_coldest_days.sql -> output/q1_coldest_days.csv).
source("config.R")
source("R/load_functions.R")
log_msg("06_queries: start")

con <- db_connect(DB_PATH)
run_sql_folder(con, "sql", "output")
dbDisconnect(con)
log_msg("06_queries: done")
