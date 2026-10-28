# check_lab7.R -- DATA 220 Lab 7 self-check. It gives an ESTIMATED score: the instructor
# reviews your work (your decisions, your sources, your plan) and gives the final mark.
#
# Run it from your team repository folder (where SUBMISSION.md is):
#   in a terminal:   Rscript check_lab7.R
#   in RStudio:      source("../check_lab7.R")     (from inside weather/ or project/)
# It copies weather/ and project/ to a temporary folder, runs every script there in a
# fresh R session (your files are never changed), and looks at the results.
# A line with 0 points means something is missing or unfinished -- not that your approach is wrong.
# Parts: A Warm-up 30 · B Your project 60 · C Team 10.

suppressPackageStartupMessages({ library(DBI) })
root <- if (file.exists("SUBMISSION.md")) "." else if (file.exists("../SUBMISSION.md")) ".." else
  stop("Run this from your team repository folder -- the one with SUBMISSION.md in it.")
root <- normalizePath(root, winslash = "/")
rscript <- file.path(R.home("bin"), "Rscript")

results <- data.frame(part = character(), check = character(), points = numeric(),
                      max = numeric(), note = character(), stringsAsFactors = FALSE)
award <- function(part, check, points, max, note = "") {
  results[nrow(results) + 1, ] <<- list(part, check, round(points, 1), max, note)
}
is_todo <- function(x) length(x) == 0 || any(is.na(x)) || any(grepl("TODO", x, fixed = TRUE)) || any(trimws(x) == "")
read_out <- function(dir, path) {
  f <- file.path(dir, path)
  if (!file.exists(f)) return(NULL)
  tryCatch(read.csv(f, check.names = FALSE, na.strings = c("", "NA"), stringsAsFactors = FALSE), error = function(e) NULL)
}
code_lines <- function(f) if (file.exists(f)) { l <- readLines(f, warn = FALSE); l[!grepl("^\\s*#", l)] } else character(0)
split_names <- function(x) trimws(unlist(strsplit(as.character(x), ",")))

run_copy <- function(folder) {
  tmp <- file.path(tempdir(), paste0("lab7_", basename(folder), "_", format(Sys.time(), "%H%M%OS3")))
  dir.create(tmp, recursive = TRUE)
  top <- setdiff(list.files(folder, all.files = TRUE, no.. = TRUE), c(".Rproj.user", ".git"))
  invisible(file.copy(file.path(folder, top), tmp, recursive = TRUE))
  for (d in c("data/interim", "data/processed", "data/db", "output", "logs")) unlink(file.path(tmp, d, "*"), recursive = TRUE)
  scripts <- sort(list.files(file.path(tmp, "scripts"), pattern = "^[0-9][0-9]_.*\\.R$"))
  ok <- list(); log <- list()
  old <- setwd(tmp)
  for (s in scripts) {
    out <- suppressWarnings(system2(rscript, c("--vanilla", file.path("scripts", s)), stdout = TRUE, stderr = TRUE))
    st <- attr(out, "status")
    ok[[s]] <- is.null(st) || st == 0
    log[[s]] <- out
  }
  setwd(old)
  list(dir = tmp, ok = ok, log = log)
}
stage_ok <- function(run, prefix) {
  s <- names(run$ok)[startsWith(names(run$ok), prefix)]
  length(s) == 1 && isTRUE(run$ok[[s]])
}

# ---------------------------------------------------------------- team names (used below)
sub_lines <- readLines(file.path(root, "SUBMISSION.md"), warn = FALSE)
field <- function(label) {
  l <- grep(paste0("\\*\\*", label, ":\\*\\*"), sub_lines, value = TRUE)
  if (length(l) == 0) "" else trimws(sub(paste0(".*\\*\\*", label, ":\\*\\*"), "", l[1]))
}
rows <- grep("^\\s*\\|", sub_lines, value = TRUE)
rows <- rows[!grepl("^\\s*\\|\\s*-", rows) & !grepl("^\\s*\\|\\s*Name\\s*\\|", rows)]
cells <- lapply(strsplit(rows, "\\|"), function(x) trimws(x[-1]))
members <- Filter(function(x) length(x) >= 4 && !is_todo(x[1]), cells)
team <- tolower(vapply(members, `[`, "", 1))
is_member <- function(names) length(names) > 0 && all(tolower(names) %in% team)

# ======================================================================== A. Warm-up (30)
W <- run_copy(file.path(root, "weather"))
wd <- W$dir
all_ran <- length(W$ok) == 6 && all(unlist(W$ok))
award("A Warm-up", "W1 the weather pipeline runs", 5 * all_ran, 5,
      if (all_ran) "6 scripts, no errors" else paste("stopped in", names(W$ok)[!unlist(W$ok)][1]))

cfg_w <- new.env()
w_ok <- tryCatch({ sys.source(file.path(wd, "config.R"), envir = cfg_w); TRUE }, error = function(e) FALSE)
cm <- if (w_ok && exists("SNOW_DAY_CM", cfg_w)) cfg_w$SNOW_DAY_CM else NA
snow_rows <- NA
db_team <- file.path(wd, "data/db/weather.sqlite")
if (file.exists(db_team)) {
  con <- dbConnect(RSQLite::SQLite(), db_team)
  snow_rows <- tryCatch(dbGetQuery(con, "SELECT COUNT(*) AS n FROM trail_weather WHERE snow_day = 'snow day'")$n, error = function(e) NA)
  dbDisconnect(con)
}
w2 <- 2 * isTRUE(cm == 5) + 3 * isTRUE(snow_rows == 26)
award("A Warm-up", "W2 snow-day decision changed to 5 cm", w2, 5,
      paste0("SNOW_DAY_CM = ", cm, "; snow-day rows = ", snow_rows, if (!isTRUE(snow_rows == 26)) " (expected 26)" else ""))

# W3: two queries, graded on the finished database (so they never wait for the pipeline)
fin <- file.path(root, "weather/data/finished/weather.sqlite")
if (!file.exists(fin)) fin <- db_team
expected <- list(
  q1 = list(df = data.frame(date = c("2026-01-19", "2026-01-23", "2026-01-20", "2026-01-21", "2026-01-22"),
                            min_temp_c = c(-35.6, -33, -31.6, -31.6, -30.8)), ordered = "min_temp_c"),
  q2 = list(df = data.frame(counter_id = c("RV-01", "RV-02", "KF-04", "MC-03"), days = c(90, 90, 90, 85),
                            total = c(53092, 28595, 24314, 13458), avg = c(589.9, 321.3, 270.2, 158.3)), ordered = "total"))
norm <- function(x) {
  n <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.na(x) | !is.na(n))) ifelse(is.na(n), "NA", sprintf("%.1f", n)) else ifelse(is.na(x), "NA", as.character(x))
}
has_col <- function(res, ev, ordered) any(vapply(res, function(rv) {
  rv <- norm(rv); if (ordered) identical(rv, ev) else identical(sort(rv), sort(ev)) }, TRUE))
con <- if (file.exists(fin)) dbConnect(RSQLite::SQLite(), fin) else NULL
for (q in names(expected)) {
  label <- paste0("W3 ", q, if (q == "q1") " five coldest days" else " trips per counter")
  f <- list.files(file.path(root, "weather/sql"), pattern = paste0("^", q, "_.*\\.sql$"), full.names = TRUE)
  if (length(f) != 1) { award("A Warm-up", label, 0, 8, paste0("sql/", q, "_*.sql not found")); next }
  sql <- paste(readLines(f, warn = FALSE), collapse = "\n")
  if (grepl("TODO", gsub("--[^\n]*", "", sql), fixed = TRUE)) { award("A Warm-up", label, 0, 8, "still the TODO placeholder"); next }
  res <- if (is.null(con)) simpleError("no database") else tryCatch(dbGetQuery(con, sql), error = function(e) e)
  if (inherits(res, "error")) { award("A Warm-up", label, 0, 8, paste("SQL error:", substr(conditionMessage(res), 1, 40))); next }
  e <- expected[[q]]
  rows_ok <- nrow(res) == nrow(e$df)
  vals <- rows_ok && all(vapply(names(e$df), function(n) has_col(res, norm(e$df[[n]]), FALSE), TRUE))
  in_order <- vals && all(vapply(e$ordered, function(n) has_col(res, norm(e$df[[n]]), TRUE), TRUE))
  award("A Warm-up", label, 2 + 2 * rows_ok + 3 * vals + in_order, 8,
        paste0(nrow(res), " row(s)", if (!rows_ok) paste0(", expected ", nrow(e$df)) else if (!vals) "; values differ"
               else if (!in_order) "; check the order" else "; right answer"))
}
if (!is.null(con)) dbDisconnect(con)
q3 <- list.files(file.path(root, "weather/sql"), pattern = "^q3_.*\\.sql$", full.names = TRUE)
ans <- if (length(q3) == 1) grep("--\\s*ANSWER:", readLines(q3, warn = FALSE), value = TRUE) else ""
award("A Warm-up", "W4 join read: ANSWER line", 4 * any(grepl("hawrelak", tolower(ans))), 4,
      if (any(grepl("hawrelak", tolower(ans)))) "right" else "fill in the ANSWER line in sql/q3_...sql")

# ======================================================================== B. Your project (60)
P <- run_copy(file.path(root, "project"))
pd <- P$dir
cfg <- new.env()
cfg_ok <- tryCatch({ sys.source(file.path(pd, "config.R"), envir = cfg); TRUE }, error = function(e) FALSE)
sources <- if (cfg_ok && exists("SOURCES", cfg)) cfg$SOURCES else list()
sources <- Filter(function(s) !is_todo(s$what), sources)
n_src <- length(sources)

fields_ok <- n_src >= 1 && all(vapply(sources, function(s) all(c("what", "format", "get", "link", "file", "owner") %in% names(s)) &&
  !is_todo(s$format) && !is_todo(s$get) && !is_todo(s$link) && !is_todo(s$file) && !is_todo(s$owner), TRUE))
formats_ok <- n_src >= 1 && all(vapply(sources, function(s)
  isTRUE(s$format %in% c("csv", "several csv", "excel", "json", "api", "other")) && isTRUE(s$get %in% c("download", "api")), TRUE))
owners_ok <- n_src >= 1 && all(vapply(sources, function(s) is_member(split_names(s$owner)), TRUE))
title_ok <- cfg_ok && exists("PROJECT", cfg) && !is_todo(cfg$PROJECT)
award("B Project", "P1 sources: what, format, link, owner", 2 * title_ok + 4 * fields_ok + 2 * formats_ok + 2 * owners_ok, 10,
      if (!cfg_ok) "config.R has an error" else paste0(n_src, " source(s)", if (n_src > 0 && !owners_ok) "; owner not in SUBMISSION.md" else ""))

has_file <- vapply(sources, function(s) !is_todo(s$file) && file.exists(file.path(pd, s$file)), TRUE)
raw_files <- list.files(file.path(pd, "data/raw"), recursive = TRUE)
raw_files <- raw_files[!grepl("^README|\\.gitkeep$", basename(raw_files))]
award("B Project", "P2 a sample file for every source", if (n_src == 0) 0 else 8 * mean(has_file) + 2 * (length(raw_files) > 0), 10,
      paste0(sum(has_file), " of ", n_src, " sources have a file in data/raw"))

profiles <- list.files(file.path(pd, "output"), pattern = "^profile_.*\\.csv$")
with_file <- names(sources)[has_file]
prof_each <- length(with_file) > 0 && all(vapply(with_file, function(n) any(startsWith(profiles, paste0("profile_", n))), TRUE))
found <- sum(grepl("^\\s*#\\s*FOUND:", readLines(file.path(pd, "config.R"), warn = FALSE)))
award("B Project", "P3 sources inspected; 2+ FOUND notes",
      3 * (stage_ok(P, "00_") && length(with_file) > 0) + 3 * prof_each + 2 * min(2, found), 10,
      paste0(length(profiles), " profile(s); ", found, " FOUND line(s)"))

tables <- if (cfg_ok && exists("TABLES", cfg)) cfg$TABLES else list()
tables <- Filter(function(t) !is_todo(t$grain), tables)
tables_ok <- length(tables) >= 2 && all(vapply(tables, function(t) !is_todo(t$key) && !is_todo(t$owner), TRUE))
links <- if (cfg_ok && exists("LINKS", cfg)) Filter(function(l) !is_todo(l$child), cfg$LINKS) else list()
links_ok <- length(links) >= 1 && all(vapply(links, function(l) isTRUE(l$child %in% names(tables)) &&
  isTRUE(l$parent %in% names(tables)) && !is_todo(l$key), TRUE))
t_owners_ok <- length(tables) >= 1 && all(vapply(tables, function(t) is_member(split_names(t$owner)), TRUE))
award("B Project", "P4 table plan: grain, key, owner, link", 6 * tables_ok + 2 * links_ok + 2 * t_owners_ok, 10,
      paste0(length(tables), " table(s), ", length(links), " link(s)"))

interim_raw <- list.files(file.path(pd, "data/interim"), pattern = "_raw\\.csv$")
ex_share <- if (length(with_file) == 0) 0 else min(1, length(interim_raw) / length(with_file))
award("B Project", "P5 01_extract.R reads every file", 4 * (stage_ok(P, "01_") && length(with_file) > 0) + 6 * ex_share, 10,
      paste0(length(interim_raw), " raw copies for ", length(with_file), " source file(s)"))

toolkit <- c("clean_names", "rename_columns", "standardize_text", "recode_values", "lump_rare", "to_number",
             "parse_dates_mixed", "set_out_of_range_to_na", "replace_missing", "drop_missing_rows", "remove_duplicates",
             "add_date_parts", "make_bands", "add_row_id", "complete_dates", "summarise_by_group", "top_n_by_group",
             "check_key", "safe_left_join", "safe_inner_join", "check_no_missing", "check_range", "check_allowed",
             "check_row_count", "check_same_total", "find_text_variants", "show_duplicate_keys", "count_values",
             "show_date_formats", "show_out_of_range", "find_unmatched", "count_missing")
clean_code <- paste(code_lines(file.path(root, "project/scripts/02_clean.R")), collapse = "\n")
used <- toolkit[vapply(toolkit, function(f) grepl(paste0("\\b", f, "\\("), clean_code), TRUE)]
clean_files <- list.files(file.path(pd, "data/interim"), pattern = "_clean\\.csv$")
decisions <- sum(grepl("^\\s*#\\s*DECISION:", readLines(file.path(root, "project/scripts/02_clean.R"), warn = FALSE)))
p6 <- 2 * (stage_ok(P, "02_") && length(used) > 0) + 2 * (length(clean_files) > 0) + 2 * (length(used) >= 3) +
  2 * ("check_key" %in% used) + 2 * (decisions >= 2)
award("B Project", "P6 first cleaning step, with reasons", p6, 10,
      paste0(length(clean_files), " clean file(s); ", length(used), " toolkit function(s); ", decisions, " DECISION(s)"))

# ======================================================================== C. Team (10)
sub_ok <- !is_todo(field("Team number")) && !is_todo(field("Project title")) && length(members) >= 2 &&
  all(vapply(members, function(x) !is_todo(x[2]) && tolower(x[3]) %in% c("yes", "no"), TRUE))
did <- vapply(members, function(x) !is_todo(x[4]), TRUE)
present <- sum(vapply(members, function(x) tolower(x[3]) == "yes", TRUE))
award("C Team", "SUBMISSION.md: names, IDs, present", 5 * sub_ok, 5,
      paste0(length(members), " member(s); ", present, " present at the lab"))
award("C Team", "what each member did", if (length(did)) 5 * mean(did) else 0, 5,
      paste(sum(did), "of", length(members), "filled in"))

# ======================================================================== report
cat("\nDATA 220 Lab 7 self-check -- team", if (is_todo(field("Team number"))) "(number not set)" else field("Team number"), "\n\n")
w <- max(nchar(results$check))
for (i in seq_len(nrow(results))) {
  cat(sprintf("%-10s %-*s %4.1f / %-2d %s\n", results$part[i], w, results$check[i],
              results$points[i], results$max[i], results$note[i]))
}
total <- sum(results$points)
cat(sprintf("\nESTIMATED SCORE: %.0f / 100   (the instructor reviews your work and gives the mark)\n", total))
show_error <- function(run, label) {
  bad <- names(run$ok)[!unlist(run$ok)]
  if (length(bad) > 0) {
    cat("\nFirst script that stopped in ", label, ": ", bad[1], "\n", sep = "")
    cat(tail(run$log[[bad[1]]], 6), sep = "\n")
  }
}
show_error(W, "weather/")
show_error(P, "project/")
if (dir.exists(file.path(root, ".git"))) {
  authors <- tryCatch(system2("git", c("-C", shQuote(root), "shortlog", "-sn", "HEAD"), stdout = TRUE, stderr = FALSE),
                      error = function(e) character(0))
  if (length(authors) > 0) { cat("\nCommits per author (for information):\n"); cat(authors, sep = "\n") }
}
writeLines(c(sprintf("%s | %s | %.1f / %d | %s", results$part, results$check, results$points, results$max, results$note),
             sprintf("ESTIMATED SCORE: %.0f / 100", total)), file.path(root, "check_lab7_result.txt"))
unlink(c(W$dir, P$dir), recursive = TRUE)
invisible(total)
