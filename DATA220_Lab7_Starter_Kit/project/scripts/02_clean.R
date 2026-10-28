# scripts/02_clean.R -- P6: your first cleaning steps, one decision at a time.
# Reads: data/interim/<source>_raw.csv      Writes: data/interim/<source>_clean.csv
# Lab 8 continues from here (more cleaning, joins, checks, the database, queries).
source("config.R")
source("R/load_functions.R")
log_msg("02_clean: start")

# =============================================================================
# TASK P6 -- clean ONE table (more if you have time), step by step
# -----------------------------------------------------------------------------
# For every step, write the three lines, then the code (see weather/scripts/02_clean.R):
#   - FOUND:    what an inspect function showed you          (the problem)
#   - DECISION: which toolkit function you use, and WHY it is the right fix
#   - CHECK:    how you know it worked (check_key(), find_text_variants() again, ...)
# End with check_key() on the key you planned in config.R, and save_csv().
# =============================================================================
# Example step (coffee-cart sales):
#   sales <- read_raw_csv("data/interim/sales_raw.csv") |> clean_names()
#   # FOUND:    find_text_variants(sales, "item") shows "Latte", "latte " and "LATTE".
#   # DECISION: standardize_text(case = "title"), because only spaces and capitals differ.
#   sales <- mutate(sales, item = standardize_text(item, case = "title"))
#   # CHECK:    find_text_variants(sales, "item") now finds nothing.

# your steps here


log_msg("02_clean: done")
