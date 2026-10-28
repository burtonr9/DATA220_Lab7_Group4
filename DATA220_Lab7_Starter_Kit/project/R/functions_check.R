# R/functions_check.R -- checks that STOP the script when something is wrong.
# A check that passes is silent or says OK; a check that fails stops with a message.
# Functions: check_no_missing(), check_range(), check_allowed(), check_row_count(),
#            check_same_total(), report_row(), save_quality_report()


# ============================================================================
# check_no_missing(df, cols, name = "table")
# LEVEL     project
# PURPOSE   Stop if any of the named columns has a missing value.
# USE WHEN  columns that must always be filled: IDs, dates, keys.
# ARGUMENTS df   : a data frame
#           cols : the columns that must be complete
#           name : a name for the message
# RETURNS   TRUE invisibly; stops otherwise
# EXAMPLE
#   d <- data.frame(id = c("A", "B"), date = c("2026-01-01", NA))
#   try(check_no_missing(d, c("id", "date"), "d"))
#   #> Error in check_no_missing(d, c("id", "date"), "d") : 
#   #>   d: 1 missing value(s) in date
# HOW THE CODE WORKS
#   1. A for loop goes through cols; sum(is.na(df[[col]])) counts missing values (Weeks 5, 7, 8).
#   2. stop() names the first column with missing values (Week 5).
# ============================================================================
check_no_missing <- function(df, cols, name = "table") {
  for (col in cols) {
    n <- sum(is.na(df[[col]]))
    if (n > 0) stop(name, ": ", n, " missing value(s) in ", col)
  }
  invisible(TRUE)
}


# ============================================================================
# check_range(x, min, max, name = "value")
# LEVEL     project
# PURPOSE   Stop if any value is below min or above max (NA is allowed).
# USE WHEN  after cleaning numbers: temperatures, counts, prices.
# ARGUMENTS x        : a number column
#           min, max : the allowed range
#           name     : a name for the message
# RETURNS   TRUE invisibly; stops otherwise
# EXAMPLE
#   check_range(c(5, 20, NA), 0, 100, "trips")
#   try(check_range(c(5, 200), 0, 100, "trips"))
#   #> Error in check_range(c(5, 200), 0, 100, "trips") : 
#   #>   trips: 1 value(s) outside [0, 100]
# HOW THE CODE WORKS
#   1. sum(x < min | x > max, na.rm = TRUE) counts values outside the range (Week 5).
#   2. stop() if any (Week 5).
# ============================================================================
check_range <- function(x, min, max, name = "value") {
  n <- sum(x < min | x > max, na.rm = TRUE)
  if (n > 0) stop(name, ": ", n, " value(s) outside [", min, ", ", max, "]")
  invisible(TRUE)
}


# ============================================================================
# check_allowed(x, allowed, name = "value")
# LEVEL     project
# PURPOSE   Stop if a column has a value that is not on the allowed list
#           (Week 8's check_in_set(); NA is allowed).
# USE WHEN  after standardising a category column.
# ARGUMENTS x       : a column
#           allowed : the values that may appear
#           name    : a name for the message
# RETURNS   TRUE invisibly; stops and lists the unexpected values otherwise
# EXAMPLE
#   check_allowed(c("paved", "gravel"), c("paved", "gravel", "bridge"), "trail_type")
#   try(check_allowed(c("paved", "Gravel"), c("paved", "gravel"), "trail_type"))
#   #> Error in check_allowed(c("paved", "Gravel"), c("paved", "gravel"), "trail_type") : 
#   #>   trail_type has unexpected value(s): Gravel
# HOW THE CODE WORKS
#   1. setdiff(unique(x[!is.na(x)]), allowed): the values that are not allowed (Weeks 7-8).
#   2. stop() lists them, joined with paste(collapse =) (Weeks 5, 8).
# ============================================================================
check_allowed <- function(x, allowed, name = "value") {
  bad <- setdiff(unique(x[!is.na(x)]), allowed)
  if (length(bad) > 0) stop(name, " has unexpected value(s): ", paste(bad, collapse = ", "))
  invisible(TRUE)
}


# ============================================================================
# check_row_count(df, min, max = Inf, name = "table")
# LEVEL     project
# PURPOSE   Stop if a table has fewer than min or more than max rows.
# USE WHEN  you know how many rows to expect: "4 counters x 90 days", "at least 1".
# ARGUMENTS df       : a data frame
#           min, max : the expected number of rows (max = Inf means "no upper limit")
#           name     : a name for the message
# RETURNS   TRUE invisibly; stops otherwise
# EXAMPLE
#   check_row_count(data.frame(x = 1:3), min = 3, max = 3, name = "small")
#   try(check_row_count(data.frame(x = 1:3), min = 4, name = "small"))
#   #> Error in check_row_count(data.frame(x = 1:3), min = 4, name = "small") : 
#   #>   small: 3 rows, expected between 4 and Inf
# HOW THE CODE WORKS
#   1. nrow(df) compared with min and max; stop() if outside (Week 5).
# ============================================================================
check_row_count <- function(df, min, max = Inf, name = "table") {
  n <- nrow(df)
  if (n < min || n > max) stop(name, ": ", n, " rows, expected between ", min, " and ", max)
  invisible(TRUE)
}


# ============================================================================
# check_same_total(before, after, name = "total")
# LEVEL     project
# PURPOSE   Stop if a total changed between two steps -- nothing lost or invented.
# USE WHEN  after a join or a reshape that should keep every trip, sale or minute.
# ARGUMENTS before, after : the same number column before and after the step
#           name          : a name for the message
# RETURNS   TRUE invisibly; stops with both totals otherwise
# EXAMPLE
#   check_same_total(c(5, 6, NA), c(6, 5), "trips")
#   #> trips: total unchanged (11)
# HOW THE CODE WORKS
#   1. sum(..., na.rm = TRUE) of each (Week 5).
#   2. stop() if they differ; message() says OK (Week 5).
# ============================================================================
check_same_total <- function(before, after, name = "total") {
  a <- sum(before, na.rm = TRUE)
  b <- sum(after, na.rm = TRUE)
  if (a != b) stop(name, ": total changed from ", a, " to ", b)
  message(name, ": total unchanged (", a, ")")
  invisible(TRUE)
}


# ============================================================================
# report_row(stage, table, rows_in, rows_out, note = "")
# LEVEL     project
# PURPOSE   One row of evidence for the quality report (as in Week 8).
# USE WHEN  at the end of each stage: what came in, what went out, what changed.
# ARGUMENTS stage, table : where you are; rows_in, rows_out : the counts; note : what changed
# RETURNS   a one-row data frame
# EXAMPLE
#   report_row("02_clean", "sales", 264, 262, "2 rows without a date dropped")
#   #>      stage table rows_in rows_out                          note
#   #> 1 02_clean sales     264      262 2 rows without a date dropped
# HOW THE CODE WORKS
#   1. data.frame() with one value per column (Week 5).
# ============================================================================
report_row <- function(stage, table, rows_in, rows_out, note = "") {
  data.frame(stage = stage, table = table, rows_in = rows_in, rows_out = rows_out, note = note)
}


# ============================================================================
# save_quality_report(..., path = "output/quality_report.csv")
# LEVEL     project
# PURPOSE   Stack report rows into one table and save it.
# USE WHEN  at the end of your checks script.
# ARGUMENTS ... : report rows (from report_row()); path : where to save
# RETURNS   the report, invisibly
# EXAMPLE
#   save_quality_report(report_row("02_clean", "a", 10, 9), report_row("02_clean", "b", 5, 5),
#                       path = "output/demo_report.csv")
#   #> saved 2 rows -> output/demo_report.csv
# HOW THE CODE WORKS
#   1. bind_rows() stacks the rows (Week 8); save_csv() writes the file.
# ============================================================================
save_quality_report <- function(..., path = "output/quality_report.csv") {
  report <- bind_rows(...)
  save_csv(report, path)
  invisible(report)
}
