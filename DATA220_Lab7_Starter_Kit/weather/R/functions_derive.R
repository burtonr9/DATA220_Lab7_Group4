# R/functions_derive.R -- new columns and summaries.
# Functions: add_date_parts(), make_bands(), add_row_id(), complete_dates(),
#            summarise_by_group(), top_n_by_group()


# ============================================================================
# add_date_parts(df, date_col = "date")
# LEVEL     project
# PURPOSE   Add year, month (2026-01), weekday and is_weekend columns from a date column.
# USE WHEN  you will group or compare by month, weekday or weekend.
# ARGUMENTS df       : a data frame
#           date_col : the name of a Date column (use parse_dates_mixed() or as.Date() first)
# RETURNS   df with four new columns
# EXAMPLE
#   d <- data.frame(date = as.Date(c("2026-01-30", "2026-01-31")))
#   add_date_parts(d)
#   #>         date year   month  weekday is_weekend
#   #> 1 2026-01-30 2026 2026-01   Friday      FALSE
#   #> 2 2026-01-31 2026 2026-01 Saturday       TRUE
# HOW THE CODE WORKS
#   1. class() + stop() refuse a column that is still text (Week 5).
#   2. format(date, "%Y") and format(date, "%Y-%m") give the year and month (Week 8);
#      as.integer() makes the year a number.
#   3. weekdays() gives the day name (Week 8); format(date, "%u") is the day number
#      1-7 (Monday = 1), so %in% c("6", "7") is the weekend.
# ============================================================================
add_date_parts <- function(df, date_col = "date") {
  d <- df[[date_col]]
  if (class(d) != "Date") stop(date_col, " is not a Date column -- use parse_dates_mixed() or as.Date() first")
  df$year       <- as.integer(format(d, "%Y"))
  df$month      <- format(d, "%Y-%m")
  df$weekday    <- weekdays(d)
  df$is_weekend <- format(d, "%u") %in% c("6", "7")
  df
}


# ============================================================================
# make_bands(x, cutoffs, labels)
# LEVEL     project
# PURPOSE   Turn numbers into labelled groups ("bands"). A value equal to or above a
#           cutoff goes into the next band.
# USE WHEN  you want categories from a number: snow day yes/no, age groups, price bands.
# ARGUMENTS x       : a number column
#           cutoffs : where each new band starts, smallest first -- you choose them
#           labels  : one more label than cutoffs
# RETURNS   a text column of labels (NA stays NA)
# YOU DECIDE  the cutoffs. Write them as a DECISION.
# EXAMPLE
#   make_bands(c(0, 1.5, 2, 7.4, NA), cutoffs = 2, labels = c("no snow", "snow day"))
#   #> [1] "no snow"  "no snow"  "snow day" "snow day" NA        
#   make_bands(c(-25, -12, -3, 4), cutoffs = c(-20, -10, 0), labels = c("below -20", "-20 to -10", "-10 to 0", "above 0"))
#   #> [1] "below -20"  "-20 to -10" "-10 to 0"   "above 0"   
# HOW THE CODE WORKS
#   1. if + stop() checks there is one more label than cutoffs (Week 5).
#   2. Every value starts in the first band: if_else(is.na(x), NA, labels[1]) (Week 8).
#   3. A for loop over the cutoffs (seq(), Week 5) moves values that are >= that cutoff
#      into the next label.
# ============================================================================
make_bands <- function(x, cutoffs, labels) {
  if (length(labels) != length(cutoffs) + 1) stop("labels must have one more value than cutoffs")
  band <- if_else(is.na(x), NA, labels[1])
  for (i in seq(1, length(cutoffs))) {
    band <- if_else(!is.na(x) & x >= cutoffs[i], labels[i + 1], band)
  }
  band
}


# ============================================================================
# add_row_id(df, prefix = "R")
# LEVEL     project
# PURPOSE   Add an ID column row_id (R1, R2, ...) when no column identifies a row.
# USE WHEN  the table has no natural key -- e.g. one row per event with no event number.
#           (Week 9 did the same for views with view_id.)
# ARGUMENTS df     : a data frame
#           prefix : text in front of the number
# RETURNS   df with row_id as the first column
# YOU DECIDE  only if no real key exists: an added ID cannot catch duplicate rows.
# EXAMPLE
#   add_row_id(data.frame(event = c("open", "close")), prefix = "E")
#   #>   row_id event
#   #> 1     E1  open
#   #> 2     E2 close
# HOW THE CODE WORKS
#   1. seq(1, nrow(df)) gives 1, 2, 3, ... (Week 5); paste0() puts the prefix in front (Week 8).
#   2. select(row_id, everything()) puts the new column first (Week 7 select();
#      everything() means "all the other columns").
# ============================================================================
add_row_id <- function(df, prefix = "R") {
  df$row_id <- paste0(prefix, seq(1, nrow(df)))
  select(df, row_id, everything())
}


# ============================================================================
# complete_dates(df, date_col, group_col = NULL, from = NULL, to = NULL)
# LEVEL     project
# PURPOSE   Make sure every day from `from` to `to` has a row (for every group).
#           Days with no data become rows with NA, so a gap is visible, not hidden.
# USE WHEN  daily data where a missing day matters: a sensor offline, a closed store.
# ARGUMENTS df        : a data frame with a Date column
#           date_col  : the Date column's name
#           group_col : optional: one row per group per day (e.g. "counter_id")
#           from, to  : the period (default: the first and last date in the data)
# RETURNS   df with the missing days added as NA rows
# EXAMPLE
#   d <- data.frame(id = c("A", "A", "B"), date = as.Date(c("2026-01-01", "2026-01-03", "2026-01-02")), n = c(5, 7, 2))
#   complete_dates(d, "date", "id")
#   #> complete_dates(): 3 missing day(s) added as NA rows
#   #>   id       date  n
#   #> 1  A 2026-01-01  5
#   #> 2  A 2026-01-02 NA
#   #> 3  A 2026-01-03  7
#   #> 4  B 2026-01-01 NA
#   #> 5  B 2026-01-02  2
#   #> 6  B 2026-01-03 NA
# HOW THE CODE WORKS
#   1. seq(from, to, by = "day") lists every day (Week 5 seq(), here with dates).
#   2. With a group: a for loop builds one block of (group, every day) per group;
#      bind_rows() stacks the blocks (Weeks 5, 8).
#   3. left_join(all days, df) keeps every day; days with no data get NA (Week 7).
# ============================================================================
complete_dates <- function(df, date_col, group_col = NULL, from = NULL, to = NULL) {
  if (is.null(from)) from <- min(df[[date_col]], na.rm = TRUE)
  if (is.null(to))   to   <- max(df[[date_col]], na.rm = TRUE)
  days <- seq(as.Date(from), as.Date(to), by = "day")
  if (is.null(group_col)) {
    all_days <- data.frame(days)
    names(all_days) <- date_col
    by <- date_col
  } else {
    blocks <- list()
    for (g in unique(df[[group_col]])) {
      block <- data.frame(g, days)
      names(block) <- c(group_col, date_col)
      blocks[[g]] <- block
    }
    all_days <- bind_rows(blocks)
    by <- c(group_col, date_col)
  }
  out <- left_join(all_days, df, by = by)
  message("complete_dates(): ", nrow(out) - nrow(df), " missing day(s) added as NA rows")
  out
}


# ============================================================================
# summarise_by_group(df, group_col, value_col)
# LEVEL     project
# PURPOSE   One row per group with the usual summaries of one number column: rows,
#           missing, total, mean, min, max. Missing values are counted, not hidden.
# USE WHEN  a quick per-group summary (per month, per category) before you write your own.
# ARGUMENTS df        : a data frame
#           group_col : the column to group by
#           value_col : a number column
# RETURNS   one row per group
# EXAMPLE
#   d <- data.frame(day = c("Mon", "Mon", "Tue", "Tue"), trips = c(10, 14, 8, NA))
#   summarise_by_group(d, "day", "trips")
#   #> # A tibble: 2 × 7
#   #>   day    rows missing total  mean   min   max
#   #>   <chr> <int>   <int> <dbl> <dbl> <dbl> <dbl>
#   #> 1 Mon       2       0    24    12    10    14
#   #> 2 Tue       2       1     8     8     8     8
# HOW THE CODE WORKS
#   1. data.frame(group = df[[group_col]], value = df[[value_col]]) copies the two columns
#      under fixed names, so the code below works for any column (Week 8 df[[ ]]).
#   2. group_by() + summarise() with n(), sum(is.na()), sum(), mean(), min(), max() (Week 8).
#   3. names(out)[1] <- group_col puts the real group name back (Week 5).
# ============================================================================
summarise_by_group <- function(df, group_col, value_col) {
  out <- data.frame(group = df[[group_col]], value = df[[value_col]]) |>
    group_by(group) |>
    summarise(rows    = n(),
              missing = sum(is.na(value)),
              total   = sum(value, na.rm = TRUE),
              mean    = round(mean(value, na.rm = TRUE), 1),
              min     = min(value, na.rm = TRUE),
              max     = max(value, na.rm = TRUE))
  names(out)[1] <- group_col
  out
}


# ============================================================================
# top_n_by_group(df, group_col, value_col, top = 3)
# LEVEL     project
# PURPOSE   The `top` rows with the largest value in each group (ties broken by row order).
# USE WHEN  "the 3 busiest days per counter", "top 5 items per month".
# ARGUMENTS df        : a data frame
#           group_col : the column to group by
#           value_col : the number column to rank by
#           top       : how many rows per group
# RETURNS   top rows per group, all columns kept
# EXAMPLE
#   d <- data.frame(month = c("Jan", "Jan", "Jan", "Feb", "Feb"), item = c("A", "B", "C", "A", "B"), sold = c(5, 9, 7, 3, 4))
#   top_n_by_group(d, "month", "sold", top = 2)
#   #> # A tibble: 4 × 3
#   #>   month item   sold
#   #>   <chr> <chr> <dbl>
#   #> 1 Feb   B         4
#   #> 2 Feb   A         3
#   #> 3 Jan   B         9
#   #> 4 Jan   C         7
# HOW THE CODE WORKS
#   1. Two helper columns (group_, value_) copy the chosen columns (Week 8 df[[ ]]).
#   2. group_by() + slice_max(n = top, with_ties = FALSE) keeps the largest rows per group;
#      ungroup() ends the grouping (Week 8).
#   3. select(-group_, -value_) removes the helper columns again (Week 7).
# ============================================================================
top_n_by_group <- function(df, group_col, value_col, top = 3) {
  df$group_ <- df[[group_col]]
  df$value_ <- df[[value_col]]
  df |>
    group_by(group_) |>
    slice_max(value_, n = top, with_ties = FALSE) |>
    ungroup() |>
    select(-group_, -value_)
}
