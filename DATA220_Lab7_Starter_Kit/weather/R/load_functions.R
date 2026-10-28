# R/load_functions.R -- the DATA 220 project toolkit.
# Every script starts with   source("R/load_functions.R")
# It loads the four packages from Weeks 7-9 and every toolkit function.
#
# The toolkit has one file per job. Each function has a comment block that says
# what it is for, when to use it, what it needs, what it gives back, a worked
# example with its real output, and how its code works (with the week it was taught).
#
#   functions_setup.R      folders, the log, saving files
#   functions_read.R       getting raw files into R, unchanged
#   functions_inspect.R    SHOW a problem -- these never change your data
#   functions_fix.R        CHANGE values -- each one is a decision you make
#   functions_derive.R     new columns and summaries
#   functions_join.R       keys and joins you can trust
#   functions_check.R      checks that stop the script when something is wrong
#   functions_database.R   SQLite: store tables with keys, ask questions in SQL

suppressPackageStartupMessages({
  library(dplyr)      # Weeks 7-8
  library(tidyr)      # Week 7
  library(stringr)    # Week 7
  library(DBI)        # Week 9
})

for (part in c("setup", "read", "inspect", "fix", "derive", "join", "check", "database")) {
  source(paste0("R/functions_", part, ".R"))
}
