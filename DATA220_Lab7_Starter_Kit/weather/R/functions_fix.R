# R/functions_fix.R -- CHANGE values. Each function is one cleaning decision:
# use it because an inspect function showed you a problem, and write a
# "# DECISION:" comment that says what you saw, which function you used, and why.
# Functions: clean_names(), rename_columns(), standardize_text(), recode_values(),
#            lump_rare(), to_number(), parse_dates_mixed(), set_out_of_range_to_na(),
#            replace_missing(), drop_missing_rows(), remove_duplicates()


# ============================================================================
# clean_names(df)
# LEVEL     core
# PURPOSE   Give columns simple names that are easy to type: lower case, words joined
#           by _, no spaces, brackets or units.  "Max Temp (°C)" -> max_temp_c
# USE WHEN  right after reading a file whose column names have spaces or symbols.
# ARGUMENTS df : a data frame
# RETURNS   the same data frame with new column names
# EXAMPLE
#   messy <- data.frame(`Max Temp (°C)` = 1, `Date/Time` = 2, `Station Name` = 3, check.names = FALSE)
#   names(clean_names(messy))
#   #> [1] "max_temp_c"   "date_time"    "station_name"
# HOW THE CODE WORKS
#   1. str_to_lower() makes every name lower case (Week 8).
#   2. str_replace_all(x, "[^a-z0-9]+", "_") turns every run of characters that is not a
#      letter or digit into one _ (new: a pattern in [ ] means "any of these characters").
#   3. str_remove_all(x, "^_|_$") removes a _ at the start (^) or end ($) (new).
#   4. names(df) <- ... puts the new names on the table (Week 5).
# ============================================================================
clean_names <- function(df) {
  new <- str_to_lower(names(df))
  new <- str_replace_all(new, "[^a-z0-9]+", "_")
  new <- str_remove_all(new, "^_|_$")
  names(df) <- new
  df
}


# ============================================================================
# rename_columns(df, mapping)
# LEVEL     project
# PURPOSE   Rename columns, given as c(old = "new"). Names that are not in the table
#           are skipped, so one mapping can be used on every file of a source.
# USE WHEN  one file (or one month) calls a column something else.
# ARGUMENTS df      : a data frame
#           mapping : c("old name" = "new name", ...)
# RETURNS   the data frame with the columns renamed
# EXAMPLE
#   d <- data.frame(`Snow (cm)` = 1, Day = 2, check.names = FALSE)
#   names(rename_columns(d, c("Snow (cm)" = "Total Snow (cm)")))
#   #>   renamed column 'Snow (cm)' -> 'Total Snow (cm)'
#   #> [1] "Total Snow (cm)" "Day"            
# HOW THE CODE WORKS
#   1. A for loop goes through the old names in names(mapping) (Week 5).
#   2. if (old %in% names(df)): only rename what is there (Week 7 %in%).
#   3. names(df)[names(df) == old] <- new changes that one name (Week 5); message() says so.
#      (rename() from Week 7 does the same when you type the names yourself.)
# ============================================================================
rename_columns <- function(df, mapping) {
  for (old in names(mapping)) {
    if (old %in% names(df)) {
      names(df)[names(df) == old] <- mapping[[old]]
      message("  renamed column '", old, "' -> '", mapping[[old]], "'")
    }
  }
  df
}


# ============================================================================
# standardize_text(x, case = "upper")
# LEVEL     core
# PURPOSE   Make one value always look the same: no stray spaces, one letter case.
# USE WHEN  find_text_variants() shows the same thing written with different spaces
#           or capitals: " KF-04", "kf-04", "KF-04 ".
# ARGUMENTS x    : a text column (vector)
#           case : "upper", "lower", "title" or "none"
# RETURNS   the cleaned text, same length as x
# YOU DECIDE  which case: IDs and codes usually "upper"; names "title".
# EXAMPLE
#   standardize_text(c(" kf-04", "RV-02 ", "rv-02"))
#   #> [1] "KF-04" "RV-02" "RV-02"
#   standardize_text(c("  hot chocolate", "LATTE"), case = "title")
#   #> [1] "Hot Chocolate" "Latte"        
# HOW THE CODE WORKS
#   1. str_squish() removes spaces at both ends and doubled spaces inside (Week 7).
#   2. if / else if picks str_to_upper(), str_to_lower() or str_to_title() (Weeks 5, 7, 8).
# ============================================================================
standardize_text <- function(x, case = "upper") {
  x <- str_squish(x)
  if (case == "upper") {
    x <- str_to_upper(x)
  } else if (case == "lower") {
    x <- str_to_lower(x)
  } else if (case == "title") {
    x <- str_to_title(x)
  }
  x
}


# ============================================================================
# recode_values(x, mapping)
# LEVEL     core
# PURPOSE   Replace values with the ones you agreed on: codes, abbreviations,
#           spellings that standardize_text() cannot fix.  c("Sci Fi" = "Science Fiction")
# USE WHEN  count_values() or find_text_variants() shows several names for one thing.
# ARGUMENTS x       : a text column (vector)
#           mapping : c("old value" = "new value", ...)
# RETURNS   x with the old values replaced; everything else unchanged
# EXAMPLE
#   recode_values(c("CLASS-A01", "CLASS-B02", "CLASS-A01"), c("CLASS-A01" = "EDM-A", "CLASS-B02" = "EDM-B"))
#   #>   'CLASS-A01' -> 'EDM-A': 2 value(s)
#   #>   'CLASS-B02' -> 'EDM-B': 1 value(s)
#   #> [1] "EDM-A" "EDM-B" "EDM-A"
# HOW THE CODE WORKS
#   1. A for loop goes through the old values (Week 5).
#   2. if_else(x == old, new, x) replaces only the matching values (Week 8); NA stays NA.
#      case_when() (Week 7) does the same when you type each rule yourself.
#   3. message() says how many values each rule changed.
# ============================================================================
recode_values <- function(x, mapping) {
  for (old in names(mapping)) {
    n <- sum(x == old, na.rm = TRUE)
    x <- if_else(x == old, mapping[[old]], x)
    message("  '", old, "' -> '", mapping[[old]], "': ", n, " value(s)")
  }
  x
}


# ============================================================================
# lump_rare(x, min_count, other = "Other")
# LEVEL     project
# PURPOSE   Put categories with fewer than min_count rows into one "Other" group.
# USE WHEN  a chart or summary would have dozens of tiny categories.
# ARGUMENTS x         : a text column (vector)
#           min_count : categories with fewer rows than this become `other` -- you choose it
#           other     : the label for the combined group
# RETURNS   x with the rare categories replaced
# YOU DECIDE  min_count. Write down why that number; never lump the values you study.
# EXAMPLE
#   x <- c("Pothole", "Pothole", "Graffiti", "Pothole", "Snow", "Graffiti", "Noise")
#   lump_rare(x, min_count = 2)
#   #> 2 value(s) with fewer than 2 rows -> 'Other'
#   #> [1] "Pothole"  "Pothole"  "Graffiti" "Pothole"  "Other"    "Graffiti" "Other"   
# HOW THE CODE WORKS
#   1. table(x) counts each value (Week 7).
#   2. names(counts)[counts < min_count] lists the rare ones (Week 5).
#   3. if_else(x %in% rare, other, x) replaces them (Weeks 7-8).
# ============================================================================
lump_rare <- function(x, min_count, other = "Other") {
  counts <- table(x)
  rare <- names(counts)[counts < min_count]
  message(length(rare), " value(s) with fewer than ", min_count, " rows -> '", other, "'")
  if_else(x %in% rare, other, x)
}


# ============================================================================
# to_number(x)
# LEVEL     core
# PURPOSE   Turn text like "1,110", "$4.50" or "12%" into numbers.
# USE WHEN  profile_raw_file() says "number" or "number with , $ or %".
# ARGUMENTS x : a text column (vector)
# RETURNS   numbers; text that is not a number becomes NA, and a message says how many
# EXAMPLE
#   to_number(c("1,110", "$4.50", "12%", "n/a", NA))
#   #> to_number(): 1 value(s) could not be read as numbers -> NA
#   #> [1] 1110.0    4.5   12.0     NA     NA
# HOW THE CODE WORKS
#   1. str_remove_all(x, "[,$% ]") deletes commas, dollar signs, percent signs and
#      spaces (new: the characters in [ ] are the ones removed).
#   2. as.numeric() converts the text (Week 5); suppressWarnings() hides R's warning
#      for text that is not a number -- the message below reports it instead.
#   3. sum(is.na(out) & !is.na(x)) counts values that were there but could not be read.
# ============================================================================
to_number <- function(x) {
  out <- suppressWarnings(as.numeric(str_remove_all(x, "[,$% ]")))
  bad <- sum(is.na(out) & !is.na(x))
  if (bad > 0) message("to_number(): ", bad, " value(s) could not be read as numbers -> NA")
  out
}


# ============================================================================
# parse_dates_mixed(x, formats = DATE_FORMATS)
# LEVEL     core
# PURPOSE   Read dates written in several formats in one column (the Week 8 parse_mixed(),
#           for any number of formats). Tries 2026-01-31, 01/31/2026, 31-Jan-2026, 2026/01/31.
# USE WHEN  show_date_formats() or profile_raw_file() shows more than one format.
# ARGUMENTS x       : a text column of dates
#           formats : the formats to try, in order
# RETURNS   a Date column; text that matches no format becomes NA, and a message says how many
# YOU DECIDE  ambiguous days like 06/02/2020 are read in the FIRST format that fits
#           (here: month/day). Check an example you know.
# EXAMPLE
#   parse_dates_mixed(c("2019-05-14", "06/02/2020", "18-Apr-2022", "soon"))
#   #> parse_dates_mixed(): 1 value(s) match no format -> NA
#   #> [1] "2019-05-14" "2020-06-02" "2022-04-18" NA          
# HOW THE CODE WORKS
#   1. as.Date(x, format = ) reads one format; anything else becomes NA (Week 8).
#   2. A for loop tries each format; if_else(is.na(out), new try, out) only fills values
#      that are still NA -- exactly Week 8's parse_mixed(), repeated for each format.
# ============================================================================
parse_dates_mixed <- function(x, formats = DATE_FORMATS) {
  out <- as.Date(x, format = formats[1])
  for (f in formats) {
    out <- if_else(is.na(out), as.Date(x, format = f), out)
  }
  bad <- sum(is.na(out) & !is.na(x))
  if (bad > 0) message("parse_dates_mixed(): ", bad, " value(s) match no format -> NA")
  out
}


# ============================================================================
# set_out_of_range_to_na(x, min, max, name = "value")
# LEVEL     project
# PURPOSE   Replace values outside [min, max] with NA -- never with 0 or a guess.
# USE WHEN  show_out_of_range() showed values that cannot be true (a 99.9 °C day).
# ARGUMENTS x        : a number column
#           min, max : the possible range -- you choose it
#           name     : a name for the message
# RETURNS   x with impossible values as NA
# YOU DECIDE  the range. Write it as a DECISION with the reason.
# EXAMPLE
#   set_out_of_range_to_na(c(12, -8, 99.9, 3), -50, 45, "max_temp_c")
#   #> max_temp_c: 1 value(s) outside [-50, 45] -> NA
#   #> [1] 12 -8 NA  3
# HOW THE CODE WORKS
#   1. x < min | x > max marks the impossible values (Week 5 comparisons, | means "or").
#   2. if_else(..., NA, x) puts NA there and keeps every other value (Week 8).
# ============================================================================
set_out_of_range_to_na <- function(x, min, max, name = "value") {
  bad <- !is.na(x) & (x < min | x > max)
  message(name, ": ", sum(bad), " value(s) outside [", min, ", ", max, "] -> NA")
  if_else(bad, NA, x)
}


# ============================================================================
# replace_missing(x, value)
# LEVEL     project
# PURPOSE   Replace NA with a value YOU choose (there is no default on purpose).
# USE WHEN  missing has a known meaning: no rain recorded = 0 mm, no category = "Unknown".
# ARGUMENTS x     : a column (vector)
#           value : the replacement -- you must give it
# RETURNS   x without NA
# YOU DECIDE  whether missing really means that value. If you are not sure, keep NA.
# EXAMPLE
#   replace_missing(c("Web", NA, "iPhone"), "Unknown")
#   #> replace_missing(): 1 missing value(s) -> Unknown
#   #> [1] "Web"     "Unknown" "iPhone" 
# HOW THE CODE WORKS
#   1. sum(is.na(x)) counts what will change (Week 7).
#   2. replace_na(x, value) does the replacement (Week 7).
# ============================================================================
replace_missing <- function(x, value) {
  message("replace_missing(): ", sum(is.na(x)), " missing value(s) -> ", value)
  replace_na(x, value)
}


# ============================================================================
# drop_missing_rows(df, cols, name = "table")
# LEVEL     project
# PURPOSE   Drop the rows that have NA in any of the columns you name.
# USE WHEN  a row is useless without that value (no date, no ID) -- not just "has an NA".
# ARGUMENTS df   : a data frame
#           cols : the column(s) that must be filled
#           name : a name for the message
# RETURNS   df without those rows; a message says how many were dropped
# YOU DECIDE  which columns. Dropping rows changes every count after it: report it.
# EXAMPLE
#   d <- data.frame(id = c("A", NA, "C"), trips = c(5, 6, NA))
#   drop_missing_rows(d, "id", "d")
#   #> d: dropped 1 row(s) with missing id
#   #>   id trips
#   #> 1  A     5
#   #> 2  C    NA
# HOW THE CODE WORKS
#   1. A for loop goes through cols (Week 5).
#   2. filter(df, !is.na(df[[col]])) keeps rows where that column is filled (Weeks 7-8).
#      drop_na() from Week 7 does the same when you type the column names yourself.
# ============================================================================
drop_missing_rows <- function(df, cols, name = "table") {
  before <- nrow(df)
  for (col in cols) {
    df <- filter(df, !is.na(df[[col]]))
  }
  message(name, ": dropped ", before - nrow(df), " row(s) with missing ", paste(cols, collapse = "/"))
  df
}


# ============================================================================
# remove_duplicates(df, name = "table")
# LEVEL     core
# PURPOSE   Drop rows that are EXACT copies of another row (every column the same).
# USE WHEN  show_duplicate_keys() shows identical rows, e.g. two exports that overlap.
# ARGUMENTS df   : a data frame
#           name : a name for the message
# RETURNS   df without the copies; a message says how many were removed
# YOU DECIDE  only for true copies. Rows that differ in any column are not removed --
#           a helper column like source_file can make copies look different: drop it first.
# EXAMPLE
#   d <- data.frame(id = c("A", "B", "B"), day = c(1, 2, 2))
#   remove_duplicates(d, "d")
#   #> d: removed 1 exact duplicate row(s); 2 left
#   #>   id day
#   #> 1  A   1
#   #> 2  B   2
# HOW THE CODE WORKS
#   1. distinct() keeps one of each identical row (Week 7).
#   2. nrow() before and after gives the number removed (Week 5).
# ============================================================================
remove_duplicates <- function(df, name = "table") {
  before <- nrow(df)
  df <- distinct(df)
  message(name, ": removed ", before - nrow(df), " exact duplicate row(s); ", nrow(df), " left")
  df
}
