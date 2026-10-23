# =============================================================================
# 01_build_monitoring_baseline.R
# Deployment
#
#
# Run once from the project root:   source("deployment/01_build_monitoring_baseline.R")
# =============================================================================
source("deployment/R/scoring_functions.R")

art <- load_artefacts()
prep <- prepare_input(art$test_raw, art$ref, max_rows = Inf)
stopifnot(prep$ok)

scores <- score_data(prep$data, art)

baseline <- list(
  built_on  = Sys.time(),
  n         = nrow(prep$data),
  threshold = art$threshold,
  scores    = scores$prob_unemployed,
  features  = prep$data,
  flag_rate = mean(scores$flagged)
)
saveRDS(baseline, "deployment/monitoring_baseline.rds")

cat("Baseline saved: ", baseline$n, "rows | share flagged at", art$threshold, "=",
    round(baseline$flag_rate, 3), "\n")
