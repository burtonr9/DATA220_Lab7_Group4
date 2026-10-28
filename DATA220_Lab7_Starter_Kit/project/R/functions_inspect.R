# R/functions_inspect.R -- SHOW a problem. These functions never change your data:
# they tell you what is there, so YOU can decide what (if anything) to fix.
# Functions: profile_raw_file(), count_missing(), count_values(), show_duplicate_keys(),
#            find_text_variants(), show_date_formats(), show_out_of_range(), find_unmatched()


# ============================================================================
# profile_raw_file(path, sheet = 1)
# LEVEL     core
# PURPOSE   "What is in this file?" One row per column: what the values look like,
#           how many are missing, how many different values, and examples.
# USE WHEN  first thing, on every new source -- before you write any cleaning code.
# ARGUMENTS path  : a .csv, .xlsx, .json or .tsv file (read with read_any_file())
#           sheet : Excel only -- which sheet
# RETURNS   a data frame: column, looks_like, missing, distinct, examples
#           looks_like warns you: "date in 2 formats", "number with , $ or %".
#           examples shows every value when there are 12 or fewer; a value with a
#           stray space is shown in quotes, e.g. ' KF-04'.
# EXAMPLE
#   profile_raw_file("data/raw/counters.csv")
#   #> read 4 rows x 4 columns from data/raw/counters.csv
#   #> profile: 4 rows x 4 columns
#   #>       column        looks_like missing distinct
#   #> 1 counter_id              text       0        4
#   #> 2   location              text       0        4
#   #> 3 trail_type              text       0        4
#   #> 4  installed date in 2 formats       0        4
#   #>                                                                                 examples
#   #> 1                                                          RV-01 | RV-02 | MC-03 | KF-04
#   #> 2 Hawrelak Park Trail | Saskatchewan Drive Path | Mill Creek Ravine | Kinsmen Footbridge
#   #> 3                                                    multi-use | paved | gravel | bridge
#   #> 4                                      2019-05-14 | 06/02/2020 | 2021-09-30 | 04/18/2022
# HOW THE CODE WORKS
#   1. read_any_file() reads the file as text.
#   2. A for loop goes through names(df) (Week 5); df[[col]] takes one column by its name (Week 8).
#   3. For each column, two small helpers below decide looks_like and examples;
#      missing counts NA or blank values; distinct is length(unique()) (Week 7).
#   4. Each column's summary is a one-row data.frame(); bind_rows() stacks them (Week 8).
# ============================================================================
profile_raw_file <- function(path, sheet = 1) {
  df <- read_any_file(path, sheet = sheet)
  rows <- list()
  for (col in names(df)) {
    v <- df[[col]]
    filled <- v[!is.na(v) & str_squish(v) != ""]
    rows[[col]] <- data.frame(column     = col,
                              looks_like = guess_type(filled),
                              missing    = length(v) - length(filled),
                              distinct   = length(unique(filled)),
                              examples   = show_examples(filled))
  }
  message("profile: ", nrow(df), " rows x ", ncol(df), " columns")
  bind_rows(rows)
}

# Helper for profile_raw_file(): what do these (non-missing) text values look like?
#   number : as.numeric() works on all of them once , $ % are removed (Week 5 as.numeric();
#            str_remove_all() is new: it deletes every , $ or %; suppressWarnings() hides
#            the warning as.numeric() gives for text that is not a number).
#   date   : every value is a date in one of DATE_FORMATS (as.Date(x, format =), Week 8);
#            the loop counts how many different formats are in use.
#   text   : anything else.
guess_type <- function(v) {
  if (length(v) == 0) return("empty")
  as_num <- suppressWarnings(as.numeric(str_remove_all(v, "[,$%]")))
  if (all(!is.na(as_num))) {
    if (any(v != str_remove_all(v, "[,$%]"))) return("number with , $ or %")
    return("number")
  }
  read_ok <- is.na(v)                         # start: no value read yet (all FALSE)
  formats_used <- 0
  for (f in DATE_FORMATS) {
    this_format <- !is.na(as.Date(v, format = f)) & !read_ok
    if (any(this_format)) formats_used <- formats_used + 1
    read_ok <- read_ok | this_format
  }
  if (all(read_ok)) {
    if (formats_used == 1) return("date")
    return(paste0("date in ", formats_used, " formats"))
  }
  "text"
}

# Helper for profile_raw_file(): a short list of example values.
# Values whose spaces change under str_squish() (Week 7) are shown in quotes.
show_examples <- function(v) {
  u <- unique(v)
  u <- if_else(u != str_squish(u), paste0("'", u, "'"), u)
  if (length(u) <= 12) return(paste(u, collapse = " | "))
  paste(c(head(u, 3), "..."), collapse = " | ")
}


# ============================================================================
# count_missing(df)
# LEVEL     core
# PURPOSE   How many values are missing in each column?
# USE WHEN  before cleaning, and again after, to see what your cleaning changed.
# ARGUMENTS df : a data frame
# RETURNS   a data frame: column, missing, percent
# EXAMPLE
#   d <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA), c = 1:3)
#   count_missing(d)
#   #>   column missing percent
#   #> 1      a       1    33.3
#   #> 2      b       1    33.3
#   #> 3      c       0     0.0
# HOW THE CODE WORKS
#   1. colSums(is.na(df)) counts the NA values in every column (Week 7).
#   2. data.frame() puts the counts in a table with a percent column; round() (Week 5).
# ============================================================================
count_missing <- function(df) {
  missing <- colSums(is.na(df))
  data.frame(column  = names(missing),
             missing = missing,
             percent = round(100 * missing / nrow(df), 1),
             row.names = NULL)
}


# ============================================================================
# count_values(df, col, top = 20)
# LEVEL     core
# PURPOSE   Which values occur, and how often? The most frequent first.
# USE WHEN  checking a category column: spellings, codes, unexpected values.
# ARGUMENTS df  : a data frame
#           col : the column name, as text
#           top : how many rows to show
# RETURNS   a data frame: the value, n, percent
# EXAMPLE
#   d <- data.frame(device = c("iPhone", "iphone", "Android", "iPhone", NA))
#   count_values(d, "device")
#   #> device: 4 different value(s), 1 missing
#   #>    device n percent
#   #> 1  iPhone 2      40
#   #> 2 Android 1      20
#   #> 3  iphone 1      20
#   #> 4    <NA> 1      20
# HOW THE CODE WORKS
#   1. df[[col]] takes the column by its name (Week 8); data.frame() makes it a one-column table.
#   2. count() counts each value; arrange(desc(n)) puts the most frequent first (Weeks 7-8).
#   3. A percent column, the original column name back with names(), head() for the top rows.
# ============================================================================
count_values <- function(df, col, top = 20) {
  out <- data.frame(value = df[[col]]) |>
    count(value) |>
    arrange(desc(n))
  out$percent <- round(100 * out$n / sum(out$n), 1)
  names(out)[1] <- col
  message(col, ": ", nrow(out), " different value(s), ", sum(is.na(df[[col]])), " missing")
  head(out, top)
}


# ============================================================================
# show_duplicate_keys(df, key)
# LEVEL     core
# PURPOSE   Show every row whose key value appears more than once -- so you can see
#           WHY before deciding what to do. It does not remove anything.
# USE WHEN  check_key() said a key repeats.
# ARGUMENTS df  : a data frame
#           key : the key column(s), e.g. "member_id" or c("member_id", "loan_date")
# RETURNS   the rows that share a key value, grouped together
# YOU DECIDE  exact copies -> remove_duplicates(); real different rows -> the key is
#           wrong (add a column to it) or a value needs fixing first.
# EXAMPLE
#   loans <- data.frame(member = c("M1", "M2", "M2", "M3"), day = c(1, 1, 1, 2), books = c(2, 4, 3, 1))
#   show_duplicate_keys(loans, c("member", "day"))
#   #> 1 key value(s) repeat, in 2 row(s)
#   #>   member day books
#   #> 1     M2   1     4
#   #> 2     M2   1     3
# HOW THE CODE WORKS
#   1. df[key] keeps only the key columns; duplicated() marks repeats (Week 7).
#   2. filter() keeps the repeated key values; distinct() lists each once (Week 7).
#   3. inner_join() of that list with df brings back every row with those keys (Week 7).
# ============================================================================
show_duplicate_keys <- function(df, key) {
  keys <- df[key]
  repeated <- filter(keys, duplicated(keys)) |> distinct()
  out <- inner_join(repeated, df, by = key)
  message(nrow(repeated), " key value(s) repeat, in ", nrow(out), " row(s)")
  out
}


# ============================================================================
# find_text_variants(df, col)
# LEVEL     core
# PURPOSE   Find values that are probably the same thing written differently:
#           they become identical once spaces and capitals are ignored.
# USE WHEN  a category or ID column might have spelling problems ("rv-02", "RV-02 ").
# ARGUMENTS df  : a data frame
#           col : the column name, as text
# RETURNS   one row per spelling, grouped by its simplified form, with how many rows use it
# YOU DECIDE  same thing -> standardize_text() or recode_values(); different things -> leave them.
# EXAMPLE
#   d <- data.frame(item = c("Latte", "latte ", "LATTE", "Mocha", "Mocha"))
#   find_text_variants(d, "item")
#   #> 1 value(s) written in more than one way
#   #> # A tibble: 3 × 3
#   #>   same_as spelling  rows
#   #>   <chr>   <chr>    <int>
#   #> 1 latte   "LATTE"      1
#   #> 2 latte   "Latte"      1
#   #> 3 latte   "latte "     1
# HOW THE CODE WORKS
#   1. count() gives each spelling and its rows (Week 7).
#   2. simple = str_to_lower(str_squish(value)): lower case, no extra spaces (Weeks 7-8).
#   3. group_by(simple) + summarise(n()) counts spellings per simple form (Week 8);
#      filter() keeps forms with more than one spelling.
#   4. inner_join() brings back the spellings of those forms (Week 7).
# ============================================================================
find_text_variants <- function(df, col) {
  spellings <- data.frame(value = df[[col]]) |>
    filter(!is.na(value)) |>
    count(value, name = "rows")
  spellings$simple <- str_to_lower(str_squish(spellings$value))
  groups <- spellings |>
    group_by(simple) |>
    summarise(spellings = n()) |>
    filter(spellings > 1)
  out <- inner_join(groups, spellings, by = "simple") |>
    select(same_as = simple, spelling = value, rows)
  message(nrow(groups), " value(s) written in more than one way")
  out
}


# ============================================================================
# show_date_formats(df, col)
# LEVEL     project
# PURPOSE   Which date formats does this column use? Shows each format with a count
#           and an example, plus values that are not a date at all.
# USE WHEN  profile_raw_file() says "date in 2 formats", or dates come out NA.
# ARGUMENTS df  : a data frame
#           col : the column name, as text
# RETURNS   a data frame: format, values, example
# YOU DECIDE  parse_dates_mixed() reads all the listed formats; "none of these" values
#           need a look first (typos? another format?).
# EXAMPLE
#   d <- data.frame(day = c("2026-10-01", "10/15/2026", "2026-10-02", "15 Oct"))
#   show_date_formats(d, "day")
#   #>          format values    example
#   #> 1      %Y-%m-%d      2 2026-10-01
#   #> 2      %m/%d/%Y      1 10/15/2026
#   #> 3 none of these      1     15 Oct
# HOW THE CODE WORKS
#   1. A for loop tries each format in DATE_FORMATS with as.Date(x, format = ) (Weeks 5, 8).
#   2. A value counts for the first format that reads it; a running TRUE/FALSE vector
#      remembers which values are already read.
#   3. One row per format in use; bind_rows() stacks them (Week 8).
# ============================================================================
show_date_formats <- function(df, col) {
  v <- df[[col]]
  v <- v[!is.na(v)]
  read_ok <- is.na(v)                          # all FALSE to start
  rows <- list()
  for (f in DATE_FORMATS) {
    this_format <- !is.na(as.Date(v, format = f)) & !read_ok
    if (any(this_format)) {
      rows[[f]] <- data.frame(format = f, values = sum(this_format), example = v[this_format][1])
    }
    read_ok <- read_ok | this_format
  }
  if (any(!read_ok)) {
    rows[["none"]] <- data.frame(format = "none of these", values = sum(!read_ok), example = v[!read_ok][1])
  }
  bind_rows(rows)
}


# ============================================================================
# show_out_of_range(df, col, min, max)
# LEVEL     project
# PURPOSE   Show the rows whose value is below min or above max.
# USE WHEN  before deciding whether extreme values are errors (a 99999 count) or real.
# ARGUMENTS df       : a data frame
#           col      : a NUMBER column (use to_number() first if it is text)
#           min, max : the range you expect
# RETURNS   the rows outside the range
# YOU DECIDE  error -> set_out_of_range_to_na(); real -> keep it, and write why.
# EXAMPLE
#   d <- data.frame(day = 1:4, trips = c(120, 99999, 85, -3))
#   show_out_of_range(d, "trips", 0, 20000)
#   #> 2 row(s) with trips outside [0, 20000]
#   #>   day trips
#   #> 1   2 99999
#   #> 2   4    -3
# HOW THE CODE WORKS
#   1. is.numeric() + stop() refuses a text column (Week 5).
#   2. filter() keeps rows where the value is < min or > max (Week 7; | means "or").
# ============================================================================
show_out_of_range <- function(df, col, min, max) {
  if (!is.numeric(df[[col]])) stop(col, " is not a number column -- use to_number() first")
  out <- filter(df, df[[col]] < min | df[[col]] > max)
  message(nrow(out), " row(s) with ", col, " outside [", min, ", ", max, "]")
  out
}


# ============================================================================
# find_unmatched(x, y, by)
# LEVEL     core
# PURPOSE   Which rows of x have NO matching row in y? Run it before a join so you
#           know what the join will drop (inner join) or leave empty (left join).
# USE WHEN  before every join, and before storing a table that points to another.
# ARGUMENTS x, y : data frames
#           by   : the column(s) they share
# RETURNS   the rows of x without a match
# YOU DECIDE  fix the key, keep the rows with NA (left join), set them aside, or drop them.
# EXAMPLE
#   counts   <- data.frame(counter_id = c("RV-01", "RV-01", "KF-09"), trips = c(5, 6, 2))
#   counters <- data.frame(counter_id = c("RV-01", "MC-03"))
#   find_unmatched(counts, counters, "counter_id")
#   #> 1 row(s) of x have no match (1 key value(s))
#   #>   counter_id trips
#   #> 1      KF-09     2
# HOW THE CODE WORKS
#   1. anti_join(x, y, by =) keeps the rows of x that have no match in y (Weeks 7-8).
#   2. distinct() on the key columns counts the different unmatched key values (Week 7).
# ============================================================================
find_unmatched <- function(x, y, by) {
  unmatched <- anti_join(x, y, by = by)
  message(nrow(unmatched), " row(s) of x have no match (", nrow(distinct(unmatched[by])), " key value(s))")
  unmatched
}
