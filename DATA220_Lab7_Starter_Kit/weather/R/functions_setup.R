# R/functions_setup.R -- folders, the log, saving files.
# Functions: make_folders(), log_msg(), save_csv()

# Date formats the toolkit tries, in this order: 2026-01-31, 01/31/2026, 31-Jan-2026, 2026/01/31.
DATE_FORMATS <- c("%Y-%m-%d", "%m/%d/%Y", "%d-%b-%Y", "%Y/%m/%d")


# ============================================================================
# make_folders()
# LEVEL     project
# PURPOSE   Create the standard project folders if they are not there yet.
# USE WHEN  at the top of the first script, so every later save has a place to go.
# ARGUMENTS (none)
# RETURNS   the folder names, invisibly (nothing is printed)
# EXAMPLE
#   make_folders()
#   dir.exists("data/interim")
#   #> [1] TRUE
# HOW THE CODE WORKS
#   1. A vector holds the folder names (Week 5: c()).
#   2. A for loop calls dir.create() once per folder (Week 5 loops, Week 8 dir.create()).
#      recursive = TRUE also creates the parent folder; showWarnings = FALSE stays quiet
#      when a folder already exists, so the function is safe to run again and again.
#   3. invisible() returns a value without printing it (Week 8).
# ============================================================================
make_folders <- function() {
  folders <- c("data/raw", "data/interim", "data/processed", "data/db", "logs", "output")
  for (f in folders) {
    dir.create(f, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(folders)
}


# ============================================================================
# log_msg(...)
# LEVEL     project
# PURPOSE   Print a time-stamped message AND add it to logs/run_log.txt, so every
#           run leaves a record of what happened.
# USE WHEN  at the start and end of each script, and after each important step.
# ARGUMENTS ... : pieces of text and numbers, pasted together
# RETURNS   nothing (it is called for its message)
# EXAMPLE
#   log_msg("read ", 180, " weather rows")
#   #> read 180 weather rows
# HOW THE CODE WORKS
#   1. paste0() glues the pieces together; format(Sys.time(), ...) adds date and time (Week 8).
#   2. message() prints the line (Week 5).
#   3. cat(..., append = TRUE) adds the same line to the end of the log file (Week 8).
# ============================================================================
log_msg <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(...))
  message(line)
  dir.create("logs", showWarnings = FALSE)
  cat(line, "\n", file = "logs/run_log.txt", append = TRUE)
}


# ============================================================================
# save_csv(df, path)
# LEVEL     core
# PURPOSE   Save a table as a CSV file and log how many rows were written.
# USE WHEN  at the end of every step: the next script reads the file you saved.
# ARGUMENTS df   : the data frame to save
#           path : where to save it, e.g. "data/interim/sales_clean.csv"
# RETURNS   the path, invisibly
# EXAMPLE
#   small <- data.frame(day = c("Mon", "Tue"), trips = c(120, 95))
#   save_csv(small, "output/demo_small.csv")
#   #> saved 2 rows -> output/demo_small.csv
# HOW THE CODE WORKS
#   1. dirname() is the folder part of the path (the partner of basename(), Week 8);
#      dir.create() makes sure that folder exists.
#   2. write.csv(..., row.names = FALSE) writes the file (Week 7).
#   3. log_msg() records the number of rows (nrow(), Week 5).
# ============================================================================
save_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(df, path, row.names = FALSE)
  log_msg("saved ", nrow(df), " rows -> ", path)
  invisible(path)
}
