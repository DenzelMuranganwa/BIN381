# =============================================================================
# smoke_test.R - quick checks that the deployment code behaves
# Run from the project root:  source("deployment/tests/smoke_test.R")
# Stops with an error if any check fails.
# =============================================================================
source("deployment/R/scoring_functions.R")
source("deployment/R/monitoring_functions.R")

ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAILED: ", msg, call. = FALSE); cat("  pass -", msg, "\n") }

art <- load_artefacts()
ref <- art$ref
cat("Model expects", length(ref$cols), "raw predictors:\n  ", paste(ref$cols, collapse = ", "), "\n\n")

cat("1. Scoring real held-out rows\n")
set.seed(1)
samp <- art$test_raw[sample(nrow(art$test_raw), 300), ]
prep <- prepare_input(samp, ref)
ok(prep$ok, "valid rows pass validation")
sc <- score_data(prep$data, art)
ok(nrow(sc) == 300, "rows in = rows out (300)")
ok(all(sc$prob_unemployed >= 0 & sc$prob_unemployed <= 1), "all probabilities between 0 and 1")
ok(all(sc$flagged == (sc$prob_unemployed >= art$threshold)), "flag matches the threshold rule")
cat("   share flagged:", round(mean(sc$flagged), 3), "\n\n")

cat("2. Deployment pipeline reproduces the evaluation baked test set\n")
tb <- as.data.frame(readRDS(file.path(art$root, "data", "Cleaned", "test_baked_fixed.rds")))
tb$Unemployed <- NULL
full <- prepare_input(art$test_raw[1:2000, ], ref)
mine <- as.data.frame(recipes::bake(art$recipe, new_data = full$data))
mine$Unemployed <- NULL
ok(identical(names(mine), names(tb)), "same baked column names and order as test_baked_fixed.rds")
ok(isTRUE(all.equal(mine, tb[1:2000, ], check.attributes = FALSE, tolerance = 1e-8)),
   "first 2000 baked rows are identical to the ones used for evaluation")
cat("\n3. Validation catches bad input\n")
bad <- samp[1:5, ]; bad$Age <- NULL
ok(!prepare_input(bad, ref)$ok, "missing Age is rejected")
bad <- samp[1:5, ]; bad$Age <- c(30, 200, 25, 40, 50)
ok(!prepare_input(bad, ref)$ok, "Age = 200 is rejected")
bad <- samp[1:5, ]; bad$Age <- c("abc", "20", "30", "40", "50")
ok(!prepare_input(bad, ref)$ok, "text in Age is rejected")
bad <- samp[1:5, ]; bad$Province <- "Atlantis"
p <- prepare_input(bad, ref)
ok(p$ok && length(p$warnings) > 0, "unseen Province gives a warning, not a crash")
ok(nrow(score_data(p$data, art)) == 5, "...and still scores")
ok(!prepare_input(samp[0, ], ref)$ok, "empty file is rejected")

cat("\n4. Casing/whitespace clean-up\n")
messy <- samp[1:3, ]; messy$Province <- paste0("  ", toupper(messy$Province), " ")
pm <- prepare_input(messy, ref)
ok(pm$ok && all(pm$data$Province %in% ref$info$Province$levels), "messy Province text is standardised")

cat("\n5. Monitoring maths\n")
ok(abs(psi(sc$prob_unemployed, sc$prob_unemployed)) < 1e-6, "PSI of a sample against itself is ~0")
shifted <- pmin(1, sc$prob_unemployed + 0.25)
ok(psi(sc$prob_unemployed, shifted) > PSI_LIMIT, "a big shift in scores trips PSI > 0.20")
dr <- check_drift(prep$data, prep$data, sc$prob_unemployed, sc$prob_unemployed, ref)
ok(!any(dr$Status == "ACTION"), "no drift flagged when comparing a batch with itself")
ok(identical(parse_truth(c("1", "Unemployed", "0", "Not_Unemployed", "x")), c(1L, 1L, 0L, 0L, NA_integer_)), "label parsing")

cat("\nAll smoke tests passed.\n")
