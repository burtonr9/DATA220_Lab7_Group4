# config.R -- YOUR project's data: the sources, what you found in them, and the tables
# you plan. Lab 7 starts it; Lab 8 builds on it. Write names exactly as in SUBMISSION.md.

PROJECT <- "TODO: your project title"

# =============================================================================
# P1 -- Sources: one entry per data source
# -----------------------------------------------------------------------------
#   what   : one line -- what the data are
#   format : "csv", "several csv", "excel", "json", "api" or "other"
#   get    : "download" (a file from a portal or a person) or "api" (Lab 8 calls it)
#   link   : where it comes from: a web page, a portal, an API address
#   file   : the file or folder in data/raw/ -- for an API source, a saved SAMPLE
#            response (P2)
#   owner  : who looks after this source; several people: "Alex Example, Sam Example"
# Example (delete it and write your own):
#   sales = list(what = "Daily sales from the cart's till, one export per month",
#                format = "several csv", get = "download",
#                link = "emailed by Campus Food Services",
#                file = "data/raw/sales", owner = "Alex Example"),
# =============================================================================
SOURCES <- list(
  source_1 = list(what = "TODO", format = "TODO", get = "TODO",
                  link = "TODO", file = "TODO", owner = "TODO")
)

# =============================================================================
# P3 -- What you found (run scripts/00_inspect_sources.R first)
# -----------------------------------------------------------------------------
# One "# FOUND:" line per problem you will have to clean or decide -- at least two:
#   FOUND (example): sales Date is in 2 formats (2026-10-01, 10/15/2026) -> parse_dates_mixed()
#
# =============================================================================

# =============================================================================
# P4 -- The tables you plan (preliminary: they may change once you know your data)
# -----------------------------------------------------------------------------
#   grain : what ONE row is ("one row per item per day")
#   key   : the column(s) that identify one row -- never repeated, never missing
#   owner : who builds it; a table can have several owners: "Alex Example, Sam Example"
# Example:
#   sales = list(grain = "one row per item per day", key = c("item", "date"), owner = "Alex Example"),
#   items = list(grain = "one row per menu item",    key = "item",            owner = "Sam Example"),
# =============================================================================
TABLES <- list(
  table_1 = list(grain = "TODO", key = "TODO", owner = "TODO"),
  table_2 = list(grain = "TODO", key = "TODO", owner = "TODO")
)

# How the tables link: child -> parent, through a shared key column. E.g.
#   list(child = "sales", parent = "items", key = "item")
LINKS <- list(
  list(child = "TODO", parent = "TODO", key = "TODO")
)
