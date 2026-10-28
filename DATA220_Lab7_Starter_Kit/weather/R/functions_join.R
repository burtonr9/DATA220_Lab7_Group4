# R/functions_join.R -- keys and joins you can trust.
# Functions: check_key(), safe_left_join(), safe_inner_join()


# ============================================================================
# check_key(df, key, name = "table")
# LEVEL     core
# PURPOSE   Is `key` really a key? It must never repeat and never be missing.
#           Stops with a clear message if not; says OK if it is.
# USE WHEN  after every cleaning step, and before every join and every database load.
# ARGUMENTS df   : a data frame
#           key  : the key column(s): one, e.g. "counter_id", or several, e.g. c("counter_id", "date")
#           name : a name for the message
# RETURNS   TRUE invisibly; stops otherwise
# EXAMPLE
#   d <- data.frame(id = c("A", "B", "B"), day = c(1, 1, 2))
#   check_key(d, c("id", "day"), "d")
#   #> d: key (id + day) OK -- 3 rows, all different
#   try(check_key(d, "id", "d"))
#   #> Error in check_key(d, "id", "d") : 
#   #>   d: (id) is not a key -- 1 repeated, 0 missing. show_duplicate_keys() shows the rows.
# HOW THE CODE WORKS
#   1. check_columns() stops if a key column does not exist.
#   2. df[key] keeps only the key columns (Week 5); sum(is.na()) counts missing parts (Week 7).
#   3. sum(duplicated()) counts repeated key values -- the Week 8 check_key_unique(),
#      for one or several columns.
#   4. stop() if either count is above 0 (Week 5); otherwise message() says OK.
# ============================================================================
check_key <- function(df, key, name = "table") {
  check_columns(df, key, name)
  keys <- df[key]
  n_missing  <- sum(is.na(keys))
  n_repeated <- sum(duplicated(keys))
  if (n_missing > 0 || n_repeated > 0) {
    stop(name, ": (", paste(key, collapse = " + "), ") is not a key -- ",
         n_repeated, " repeated, ", n_missing, " missing. show_duplicate_keys() shows the rows.")
  }
  message(name, ": key (", paste(key, collapse = " + "), ") OK -- ", nrow(df), " rows, all different")
  invisible(TRUE)
}


# ============================================================================
# safe_left_join(x, y, by, name = "join")
# LEVEL     core
# PURPOSE   A left join that checks itself: every row of x is kept, no row is
#           multiplied, and you are told how many rows found no match.
# USE WHEN  adding details from a lookup table (y) to a main table (x).
# ARGUMENTS x    : the main table -- all its rows stay
#           y    : the lookup table -- `by` must be a key in it
#           by   : the shared column(s)
#           name : a name for the message
# RETURNS   x with y's columns added; rows without a match get NA there
# YOU DECIDE  left join (keep unmatched rows, NA) or safe_inner_join() (drop them).
# EXAMPLE
#   counts   <- data.frame(counter_id = c("RV-01", "RV-01", "KF-09"), trips = c(5, 6, 2))
#   counters <- data.frame(counter_id = c("RV-01", "MC-03"), location = c("Hawrelak", "Mill Creek"))
#   safe_left_join(counts, counters, "counter_id", "counts + counters")
#   #> counts + counters: 3 rows kept; 1 without a match (NA)
#   #>   counter_id trips location
#   #> 1      RV-01     5 Hawrelak
#   #> 2      RV-01     6 Hawrelak
#   #> 3      KF-09     2     <NA>
# HOW THE CODE WORKS
#   1. left_join(x, y, by =, relationship = "many-to-one") (Week 8): the relationship
#      argument makes R stop if a row of x would match more than one row of y.
#   2. stopifnot() confirms the row count did not change (Week 5).
#   3. anti_join() counts the rows of x with no match (Weeks 7-8).
# ============================================================================
safe_left_join <- function(x, y, by, name = "join") {
  out <- left_join(x, y, by = by, relationship = "many-to-one")
  stopifnot(nrow(out) == nrow(x))
  unmatched <- nrow(anti_join(x, y, by = by))
  message(name, ": ", nrow(x), " rows kept; ", unmatched, " without a match (NA)")
  out
}


# ============================================================================
# safe_inner_join(x, y, by, name = "join")
# LEVEL     project
# PURPOSE   An inner join that tells you how many rows it dropped, and never multiplies rows.
# USE WHEN  you only want rows that match -- and you have decided the others may go.
# ARGUMENTS x, y, by, name : as for safe_left_join()
# RETURNS   only the rows of x that match y, with y's columns added
# YOU DECIDE  dropping rows changes every count after this step: record how many.
# EXAMPLE
#   counts   <- data.frame(counter_id = c("RV-01", "RV-01", "KF-09"), trips = c(5, 6, 2))
#   counters <- data.frame(counter_id = c("RV-01", "MC-03"), location = c("Hawrelak", "Mill Creek"))
#   safe_inner_join(counts, counters, "counter_id", "counts + counters")
#   #> counts + counters: 2 rows kept; 1 dropped (no match)
#   #>   counter_id trips location
#   #> 1      RV-01     5 Hawrelak
#   #> 2      RV-01     6 Hawrelak
# HOW THE CODE WORKS
#   1. inner_join(..., relationship = "many-to-one") keeps matched rows only (Weeks 7-8).
#   2. nrow(x) - nrow(out) is the number of rows dropped; message() reports it.
# ============================================================================
safe_inner_join <- function(x, y, by, name = "join") {
  out <- inner_join(x, y, by = by, relationship = "many-to-one")
  message(name, ": ", nrow(out), " rows kept; ", nrow(x) - nrow(out), " dropped (no match)")
  out
}
