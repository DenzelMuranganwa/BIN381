# =============================================================================
# 02b_fix_baked.R
# Re-bakes train/test with consistent Province handling.
# Fixes inconsistent Province casing before dummy encoding.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(recipes)
})

DATA_DIR <- "data/Cleaned"

# ---- 1. Load raw train/test ------------------------------------------------
train_raw <- readRDS(file.path(DATA_DIR, "train_raw.rds"))
test_raw  <- readRDS(file.path(DATA_DIR, "test_raw.rds"))

cat("Raw train rows:", nrow(train_raw), "\n")
cat("Raw test rows: ", nrow(test_raw),  "\n")

# ---- 2. Standardise Province -----------------------------------------------
# The raw data already has a 'Province' column. We standardise it to Title Case
# with trimmed whitespace. This collapses all casing variants into one level
# per province BEFORE dummy encoding.

standardise_province <- function(df) {
  if ("Province" %in% names(df)) {
    df <- df %>%
      mutate(Province = str_to_title(str_trim(as.character(Province))))
  }
  df
}

train_raw <- standardise_province(train_raw)
test_raw  <- standardise_province(test_raw)

cat("\nUnique Provinces in train after cleaning:\n")
print(sort(unique(train_raw$Province)))

# ---- 3. Also standardise any other character columns used as factors --------
# (same treatment as Province — prevents similar explosion in other vars)
standardise_all_chars <- function(df) {
  df %>%
    mutate(across(
      where(is.character) & !any_of("Province"),
      ~ str_trim(.x)
    ))
}
train_raw <- standardise_all_chars(train_raw)
test_raw  <- standardise_all_chars(test_raw)

# ---- 4. Re-build the recipe (same steps, but on cleaned data) ---------------
set.seed(2026)

rec_fixed <- recipe(Unemployed ~ ., data = train_raw) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors())  %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_nzv(all_predictors())            %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_zv(all_predictors())             %>%
  step_lincomb(all_numeric_predictors())

rec_fixed_prepped <- prep(rec_fixed, training = train_raw)

train_baked_fixed <- bake(rec_fixed_prepped, new_data = train_raw)
test_baked_fixed  <- bake(rec_fixed_prepped, new_data = test_raw)

# ---- 5. Check the fix ------------------------------------------------------
cat("\n=== After fix ===\n")
cat("Train baked dims:", dim(train_baked_fixed), "\n")
cat("Test baked dims: ", dim(test_baked_fixed),  "\n")

province_cols <- grep("^Province_", names(train_baked_fixed), value = TRUE)
cat("\nProvince dummy columns now:", length(province_cols), "\n")
print(province_cols)

# ---- 6. Save ---------------------------------------------------------------
saveRDS(train_baked_fixed, file.path(DATA_DIR, "train_baked_fixed.rds"))
saveRDS(test_baked_fixed,  file.path(DATA_DIR, "test_baked_fixed.rds"))
saveRDS(rec_fixed_prepped, file.path(DATA_DIR, "recipe_prepped_fixed.rds"))

cat("\n✔ Saved: train_baked_fixed.rds, test_baked_fixed.rds, recipe_prepped_fixed.rds\n")