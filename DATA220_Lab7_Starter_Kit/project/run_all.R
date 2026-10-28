# run_all.R -- run every numbered script in scripts/, in order, in one go.
# In RStudio: Session > Restart R, then source("run_all.R")
# In a terminal (from this folder): Rscript run_all.R
stages <- sort(list.files("scripts", pattern = "^[0-9][0-9]_.*\\.R$", full.names = TRUE))
t0 <- Sys.time()
for (s in stages) {
  message("==== ", s, " ====")
  source(s, echo = FALSE)
}
message("Finished ", length(stages), " scripts in ",
        round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), " s")
