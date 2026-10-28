# R/functions_database.R -- SQLite: store tables with their keys and links, then ask
# questions in SQL. Everything here is the Week 9 code, wrapped in functions.
# Functions: db_connect_fresh(), db_connect(), db_write(), db_tables(), db_query(),
#            db_unmatched(), db_check_links(), run_sql_file(), run_sql_folder()


# ============================================================================
# db_connect_fresh(path)
# LEVEL     core
# PURPOSE   Start a NEW, empty database file (the old one is deleted) and switch on
#           foreign-key checking. The database is built by your scripts -- never by hand.
# USE WHEN  at the top of the script that loads your tables.
# ARGUMENTS path : the database file, e.g. "data/db/project.sqlite"
# RETURNS   an open connection (close it with dbDisconnect(con) at the end)
# EXAMPLE
#   con <- db_connect_fresh("output/demo.sqlite")
#   #> new empty database: output/demo.sqlite
#   dbListTables(con)
#   #> character(0)
#   dbDisconnect(con)
# HOW THE CODE WORKS
#   1. file.exists() and file.remove(): delete the old file; stop() if it cannot be deleted --
#      usually because a connection is still open (Week 9).
#   2. dbConnect(RSQLite::SQLite(), path) creates and opens the file (Week 9).
#   3. dbExecute(con, "PRAGMA foreign_keys = ON"): SQLite checks foreign keys only when asked (Week 9).
# ============================================================================
db_connect_fresh <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path) && !file.remove(path)) {
    stop("Cannot delete ", path, ". Close open connections (dbDisconnect(con)) or restart R.")
  }
  con <- dbConnect(RSQLite::SQLite(), path)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  log_msg("new empty database: ", path)
  con
}


# ============================================================================
# db_connect(path)
# LEVEL     project
# PURPOSE   Open a database that already exists (to query it, not to rebuild it).
# USE WHEN  running queries, or looking at a database in the console.
# ARGUMENTS path : the database file
# RETURNS   an open connection
# EXAMPLE
#   con <- db_connect("output/demo.sqlite")
#   dbDisconnect(con)
#   try(db_connect("output/no_such_file.sqlite"))
#   #> Error in db_connect("output/no_such_file.sqlite") : 
#   #>   No database at output/no_such_file.sqlite -- run the script that builds it first
# HOW THE CODE WORKS
#   1. file.exists() + stop(): connecting to a missing file would silently create an
#      empty one, so the function refuses (Week 9).
#   2. dbConnect() and PRAGMA foreign_keys = ON, as above (Week 9).
# ============================================================================
db_connect <- function(path) {
  if (!file.exists(path)) stop("No database at ", path, " -- run the script that builds it first")
  con <- dbConnect(RSQLite::SQLite(), path)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}


# ============================================================================
# db_write(con, name, df, key, references = NULL)
# LEVEL     core
# PURPOSE   Store a table in the database WITH its rules: the key becomes the
#           PRIMARY KEY (never repeated, never missing), and each column in
#           `references` becomes a FOREIGN KEY to a parent table.
# USE WHEN  loading each processed table -- parent tables first, then the tables
#           that point to them.
# ARGUMENTS con        : an open connection (from db_connect_fresh())
#           name       : the table name
#           df         : the data frame (simple column names: use clean_names())
#           key        : the key column(s)
#           references : optional c(column = "parent_table"); the parent's key must be
#                        that same single column
# RETURNS   the number of rows stored, invisibly
# YOU DECIDE  child rows whose parent is missing are NOT stored silently: the function
#           stops and lists them. Set them aside (find_unmatched()) or store the table
#           without `references` -- and write which you chose.
# EXAMPLE
#   con <- db_connect_fresh("output/demo.sqlite")
#   #> new empty database: output/demo.sqlite
#   counters <- data.frame(counter_id = c("RV-01", "MC-03"), location = c("Hawrelak", "Mill Creek"))
#   counts   <- data.frame(counter_id = c("RV-01", "RV-01", "MC-03"), day = c(1, 2, 1), trips = c(5, 6, 2))
#   db_write(con, "counters", counters, key = "counter_id")
#   #> counters: key (counter_id) OK -- 2 rows, all different
#   #> table counters: 2 rows stored; PRIMARY KEY (counter_id)
#   db_write(con, "counts", counts, key = c("counter_id", "day"), references = c(counter_id = "counters"))
#   #> counts: key (counter_id + day) OK -- 3 rows, all different
#   #> table counts: 3 rows stored; PRIMARY KEY (counter_id, day); FOREIGN KEY -> counters
#   more <- data.frame(counter_id = c("RV-01", "KF-09"), day = c(3, 3), trips = c(4, 1))
#   try(db_write(con, "more_counts", more, key = c("counter_id", "day"), references = c(counter_id = "counters")))
#   #> more_counts: key (counter_id + day) OK -- 2 rows, all different
#   #> Error in db_write(con, "more_counts", more, key = c("counter_id", "day"),  : 
#   #>   more_counts: 1 row(s) have a counter_id that is not in counters (KF-09). Set them aside with find_unmatched(), or store more_counts without references.
#   dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'counts'")$sql |> cat()
#   #> CREATE TABLE counts (
#   #>   counter_id TEXT NOT NULL,
#   #>   day REAL NOT NULL,
#   #>   trips REAL,
#   #>   PRIMARY KEY (counter_id, day),
#   #>   FOREIGN KEY (counter_id) REFERENCES counters (counter_id)
#   #> )
#   dbDisconnect(con)
# HOW THE CODE WORKS
#   1. check_key() stops if the key repeats or is missing -- before anything is stored.
#   2. For each reference, dbGetQuery() reads the parent's keys and filter(!... %in% ...)
#      finds child rows without a parent (Weeks 7, 9); stop() lists them.
#   3. A for loop writes one line per column: its name and type (REAL for numbers,
#      INTEGER for TRUE/FALSE, TEXT for the rest); key columns get NOT NULL (Week 9).
#   4. paste() adds PRIMARY KEY (...) and FOREIGN KEY (...) REFERENCES parent (...) lines,
#      and dbExecute() runs the CREATE TABLE statement -- the same SQL as Week 9.
#   5. Dates become text (2026-01-31) with as.character(); dbAppendTable() adds the rows (Week 9).
# ============================================================================
db_write <- function(con, name, df, key, references = NULL) {
  check_key(df, key, name)
  if (name %in% dbListTables(con)) stop("table ", name, " already exists -- start from db_connect_fresh()")

  # 1. child rows must have a parent
  for (col in names(references)) {
    parent <- references[[col]]
    parent_keys <- dbGetQuery(con, paste0("SELECT ", col, " FROM ", parent))[[col]]
    orphans <- filter(df, !df[[col]] %in% parent_keys)
    if (nrow(orphans) > 0) {
      stop(name, ": ", nrow(orphans), " row(s) have a ", col, " that is not in ", parent, " (",
           paste(head(unique(orphans[[col]]), 5), collapse = ", "),
           "). Set them aside with find_unmatched(), or store ", name, " without references.")
    }
  }

  # 2. one line per column, then the key and the links
  lines <- c()
  for (col in names(df)) {
    type <- "TEXT"
    if (is.numeric(df[[col]])) type <- "REAL"
    if (is.logical(df[[col]])) type <- "INTEGER"
    if (col %in% key) type <- paste(type, "NOT NULL")
    lines <- c(lines, paste(col, type))
  }
  lines <- c(lines, paste0("PRIMARY KEY (", paste(key, collapse = ", "), ")"))
  for (col in names(references)) {
    lines <- c(lines, paste0("FOREIGN KEY (", col, ") REFERENCES ", references[[col]], " (", col, ")"))
  }
  sql <- paste0("CREATE TABLE ", name, " (\n  ", paste(lines, collapse = ",\n  "), "\n)")
  dbExecute(con, sql)

  # 3. the rows
  for (col in names(df)) {
    if (class(df[[col]]) == "Date") df[[col]] <- as.character(df[[col]])
  }
  dbAppendTable(con, name, df)
  log_msg("table ", name, ": ", nrow(df), " rows stored; PRIMARY KEY (", paste(key, collapse = ", "), ")",
          if (length(references) > 0) paste0("; FOREIGN KEY -> ", paste(references, collapse = ", ")) else "")
  invisible(nrow(df))
}


# ============================================================================
# db_tables(con)
# LEVEL     project
# PURPOSE   What is in the database? One row per table: rows and columns.
# USE WHEN  after loading, and whenever a query says "no such table".
# ARGUMENTS con : an open connection
# RETURNS   a data frame: table, rows, columns
# EXAMPLE
#   con <- db_connect("output/demo.sqlite")       # the database made in the db_write() example
#   db_tables(con)
#   #>      table rows columns
#   #> 1 counters    2       2
#   #> 2   counts    3       3
#   dbDisconnect(con)
# HOW THE CODE WORKS
#   1. dbListTables() lists the tables (Week 9).
#   2. A for loop runs SELECT COUNT(*) on each with dbGetQuery() and counts its columns
#      with dbListFields() (Week 9); bind_rows() stacks the rows (Week 8).
# ============================================================================
db_tables <- function(con) {
  rows <- list()
  for (t in dbListTables(con)) {
    n <- dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", t))$n
    rows[[t]] <- data.frame(table = t, rows = n, columns = length(dbListFields(con, t)))
  }
  bind_rows(rows)
}


# ============================================================================
# db_query(con, sql)
# LEVEL     core
# PURPOSE   Run one SQL query and get the result as a data frame.
# USE WHEN  trying a query in the console before saving it in sql/.
# ARGUMENTS con : an open connection; sql : the query, as text
# RETURNS   the result as a data frame
# EXAMPLE
#   con <- db_connect("output/demo.sqlite")
#   db_query(con, "SELECT counter_id, SUM(trips) AS trips FROM counts GROUP BY counter_id")
#   #>   counter_id trips
#   #> 1      MC-03     2
#   #> 2      RV-01    11
#   dbDisconnect(con)
# HOW THE CODE WORKS
#   1. It is dbGetQuery() (Week 9) under a shorter name.
# ============================================================================
db_query <- function(con, sql) {
  dbGetQuery(con, sql)
}


# ============================================================================
# db_unmatched(con, child, parent, key)
# LEVEL     project
# PURPOSE   Inside the database: which rows of the child table have no parent?
# USE WHEN  checking links for tables stored without `references`.
# ARGUMENTS con : an open connection; child, parent : table names; key : the shared column
# RETURNS   the unmatched child rows
# EXAMPLE
#   con <- db_connect_fresh("output/demo2.sqlite")
#   #> new empty database: output/demo2.sqlite
#   db_write(con, "counters", data.frame(counter_id = c("RV-01", "MC-03")), key = "counter_id")
#   #> counters: key (counter_id) OK -- 2 rows, all different
#   #> table counters: 2 rows stored; PRIMARY KEY (counter_id)
#   db_write(con, "counts", data.frame(counter_id = c("RV-01", "KF-09"), trips = c(5, 2)), key = "counter_id")
#   #> counts: key (counter_id) OK -- 2 rows, all different
#   #> table counts: 2 rows stored; PRIMARY KEY (counter_id)
#   db_unmatched(con, "counts", "counters", "counter_id")
#   #>   counter_id trips
#   #> 1      KF-09     2
#   dbDisconnect(con)
# HOW THE CODE WORKS
#   1. The SQL is a LEFT JOIN from child to parent, keeping rows where the parent side
#      IS NULL (Week 9): SELECT c.* FROM child c LEFT JOIN parent p ON ... WHERE p.key IS NULL.
#   2. paste0() builds that text from the names; dbGetQuery() runs it (Weeks 8-9).
# ============================================================================
db_unmatched <- function(con, child, parent, key) {
  sql <- paste0("SELECT c.* FROM ", child, " AS c LEFT JOIN ", parent, " AS p ON c.", key,
                " = p.", key, " WHERE p.", key, " IS NULL")
  dbGetQuery(con, sql)
}


# ============================================================================
# db_check_links(con, links, path = "output/link_check.csv")
# LEVEL     project
# PURPOSE   Check several child -> parent links at once and save the result.
# USE WHEN  after loading, to report every link in one table.
# ARGUMENTS con   : an open connection
#           links : list(list(child = "...", parent = "...", key = "..."), ...)
#           path  : where to save the report
# RETURNS   a data frame: child, parent, key, child_rows, unmatched_rows
# EXAMPLE
#   con <- db_connect("output/demo2.sqlite")      # made in the db_unmatched() example
#   db_check_links(con, list(list(child = "counts", parent = "counters", key = "counter_id")),
#                  path = "output/demo_links.csv")
#   #> saved 1 rows -> output/demo_links.csv
#   #>    child   parent        key child_rows unmatched_rows
#   #> 1 counts counters counter_id          2              1
#   dbDisconnect(con)
# HOW THE CODE WORKS
#   1. A for loop over the links; db_unmatched() counts the child rows without a parent;
#      SELECT COUNT(*) counts all child rows (Weeks 5, 9).
#   2. bind_rows() makes one table (Week 8); save_csv() saves it.
# ============================================================================
db_check_links <- function(con, links, path = "output/link_check.csv") {
  rows <- list()
  for (l in links) {
    total <- dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", l$child))$n
    bad <- nrow(db_unmatched(con, l$child, l$parent, l$key))
    rows[[l$child]] <- data.frame(child = l$child, parent = l$parent, key = l$key,
                                  child_rows = total, unmatched_rows = bad)
  }
  out <- bind_rows(rows)
  save_csv(out, path)
  out
}


# ============================================================================
# run_sql_file(con, file, out_dir = "output")
# LEVEL     core
# PURPOSE   Run the query in one .sql file and save the result as a CSV with the same name.
# USE WHEN  your questions live in sql/ files (comments with -- are fine).
# ARGUMENTS con     : an open connection
#           file    : e.g. "sql/q1_top5.sql"
#           out_dir : where the result goes (sql/q1_top5.sql -> output/q1_top5.csv)
# RETURNS   the result as a data frame
# EXAMPLE
#   writeLines("-- trips per counter\nSELECT counter_id, SUM(trips) AS trips FROM counts GROUP BY counter_id",
#              "output/demo_q.sql")
#   con <- db_connect("output/demo.sqlite")
#   run_sql_file(con, "output/demo_q.sql", out_dir = "output")
#   #> saved 2 rows -> output/demo_q.csv
#   #>   counter_id trips
#   #> 1      MC-03     2
#   #> 2      RV-01    11
#   dbDisconnect(con)
# HOW THE CODE WORKS
#   1. readLines() reads the file's lines (new); paste(collapse = "\n") joins them (Week 8).
#   2. dbGetQuery() runs the query (Week 9).
#   3. basename() and str_replace() turn q1_top5.sql into q1_top5.csv (Week 8; str_replace()
#      is new: it replaces the first match); save_csv() saves it.
# ============================================================================
run_sql_file <- function(con, file, out_dir = "output") {
  sql <- paste(readLines(file, warn = FALSE), collapse = "\n")
  result <- dbGetQuery(con, sql)
  save_csv(result, paste0(out_dir, "/", str_replace(basename(file), "\\.sql$", ".csv")))
  result
}


# ============================================================================
# run_sql_folder(con, folder = "sql", out_dir = "output")
# LEVEL     project
# PURPOSE   Run every .sql file in a folder, in name order, save each result and show
#           the first rows.
# USE WHEN  in your queries script: one call runs all your questions.
# ARGUMENTS con : an open connection; folder : the .sql files; out_dir : the results
# RETURNS   a list of the results, invisibly
# EXAMPLE
#   # run_sql_folder(con, "sql")
# HOW THE CODE WORKS
#   1. list.files(pattern = "\\.sql$") finds the files (Week 8).
#   2. A for loop calls run_sql_file() on each and prints the first rows with head() (Weeks 5, 7).
# ============================================================================
run_sql_folder <- function(con, folder = "sql", out_dir = "output") {
  files <- list.files(folder, pattern = "\\.sql$", full.names = TRUE)
  results <- list()
  for (f in files) {
    cat("\n==== ", basename(f), " ====\n", sep = "")
    results[[basename(f)]] <- run_sql_file(con, f, out_dir)
    print(head(results[[basename(f)]], 6))
  }
  invisible(results)
}
