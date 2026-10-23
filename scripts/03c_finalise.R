# =============================================================================
# 03c_finalise.R
# Finalise selected-model evaluation and artefacts.
#
# Decision: RF-improved operating at threshold = 0.20 (instead of default 0.5)
# Rationale: ROC-AUC = 0.787 (highest of all models), and at th = 0.20 gives
#            80% recall — same order as Naive Bayes — with better precision
#            and much better specificity.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(caret); library(pROC); library(ranger); library(scales)
})

set.seed(2026)
DATA_DIR   <- "data/Cleaned"
OUTPUT_DIR <- "outputs"
POSITIVE   <- "Unemployed"
NEGATIVE   <- "Not_Unemployed"
CHOSEN_THRESHOLD <- 0.20

# ---- 1. Reload ------------------------------------------------------------
train_baked <- readRDS(file.path(DATA_DIR, "train_baked_fixed.rds"))
test_baked  <- readRDS(file.path(DATA_DIR, "test_baked_fixed.rds"))
test_raw    <- readRDS(file.path(DATA_DIR, "test_raw.rds"))
metadata    <- readRDS(file.path(DATA_DIR, "final_model_metadata.rds"))

train_baked$Unemployed <- factor(train_baked$Unemployed, c(0, 1),
                                 c(NEGATIVE, POSITIVE))
test_baked$Unemployed  <- factor(test_baked$Unemployed, c(0, 1),
                                 c(NEGATIVE, POSITIVE))

impute_proc <- metadata$impute_proc
train_baked <- predict(impute_proc, train_baked)
test_baked  <- predict(impute_proc, test_baked)

# ---- 2. Load RF-improved (or refit if not saved) -------------------------
# We need the RF-improved caret object. It was inside 03_evaluation.R's session
# and not saved. Refit here quickly using the same seed and grid.
cat("Refitting RF-improved with identical settings to 03_evaluation.R...\n")

set.seed(2026)
rf_grid <- expand.grid(
  mtry = c(floor(sqrt(ncol(train_baked) - 1)),
           floor((ncol(train_baked) - 1) / 3),
           floor((ncol(train_baked) - 1) / 2)),
  splitrule = "gini",
  min.node.size = c(5, 20)
)

rf_improved <- train(
  Unemployed ~ ., data = train_baked, method = "ranger",
  trControl = trainControl(method = "cv", number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary),
  tuneGrid = rf_grid, num.trees = 500,
  importance = "impurity", metric = "ROC",
  num.threads = parallel::detectCores()
)

cat("RF refit. Best tune:\n"); print(rf_improved$bestTune)

# ---- 3. Predict + apply chosen threshold ----------------------------------
prob_pos <- predict(rf_improved, test_baked, type = "prob")[, POSITIVE]
estimate <- factor(ifelse(prob_pos >= CHOSEN_THRESHOLD, POSITIVE, NEGATIVE),
                   levels = c(NEGATIVE, POSITIVE))

cm <- confusionMatrix(estimate, test_baked$Unemployed, positive = POSITIVE)
cat("\n=== RF-improved @ threshold", CHOSEN_THRESHOLD, "===\n")
print(cm$table)
cat("Recall:",       round(cm$byClass["Sensitivity"], 4),
    "| Precision:",  round(cm$byClass["Precision"],   4),
    "| Specificity:",round(cm$byClass["Specificity"], 4),
    "| F1:",         round(cm$byClass["F1"],          4),
    "| Balanced Acc:",round(cm$byClass["Balanced Accuracy"], 4), "\n")

r <- pROC::roc(test_baked$Unemployed, prob_pos,
               levels = c(NEGATIVE, POSITIVE), direction = "<", quiet = TRUE)
cat("ROC-AUC:", round(as.numeric(pROC::auc(r)), 4), "\n")

# ---- 4. Save the final selected-model record ------------------------------
final_row <- tibble(
  model         = "RF-improved @ th=0.20",
  Accuracy      = round(cm$overall["Accuracy"], 4),
  Precision     = round(cm$byClass["Precision"], 4),
  Recall        = round(cm$byClass["Sensitivity"], 4),
  F1            = round(cm$byClass["F1"], 4),
  Specificity   = round(cm$byClass["Specificity"], 4),
  Balanced_Acc  = round(cm$byClass["Balanced Accuracy"], 4),
  ROC_AUC       = round(as.numeric(pROC::auc(r)), 4)
)

updated <- bind_rows(metadata$comparison, final_row) %>% arrange(desc(ROC_AUC))
cat("\n================ FINAL COMPARISON MATRIX ================\n")
print(updated)
write_csv(updated, file.path(OUTPUT_DIR, "model_comparison_matrix.csv"))

# ---- 5. Fairness at chosen threshold -------------------------------------
subgroup_cols <- intersect(
  c("Province", "gender", "population_group", "geo_type",
    "youth_flag", "quarter_label"),
  names(test_raw)
)
test_meta <- test_raw %>% select(all_of(subgroup_cols))

preds_final <- tibble(truth = test_baked$Unemployed,
                      estimate = estimate, prob_pos = prob_pos) %>%
  bind_cols(test_meta)

for (g in subgroup_cols) {
  tbl <- preds_final %>% filter(!is.na(.data[[g]])) %>%
    group_by(Group = .data[[g]]) %>%
    group_modify(~ {
      n <- nrow(.x)
      if (n < 30) return(tibble(n = n, Recall = NA_real_,
                                Precision = NA_real_, F1 = NA_real_,
                                Accuracy = NA_real_))
      c <- confusionMatrix(.x$estimate, .x$truth, positive = POSITIVE)
      tibble(n = n,
             Recall    = round(c$byClass["Sensitivity"], 3),
             Precision = round(c$byClass["Precision"], 3),
             F1        = round(c$byClass["F1"], 3),
             Accuracy  = round(c$overall["Accuracy"], 3))
    }) %>% ungroup()
  cat("\n=== Final model fairness by", g, "===\n")
  print(as.data.frame(tbl))
  write_csv(tbl, file.path(OUTPUT_DIR, paste0("fairness_final_", g, ".csv")))
}

# ---- 6. Save final artefacts for deployment ------------------------------
saveRDS(rf_improved, file.path(DATA_DIR, "final_model.rds"))
saveRDS(list(
  final_model_name  = "RF-improved",
  chosen_threshold  = CHOSEN_THRESHOLD,
  comparison        = updated,
  boot_CIs          = metadata$boot_CIs,
  impute_proc       = impute_proc,
  best_tune         = rf_improved$bestTune,
  decision_note     = paste0(
    "RF-improved selected on ROC-AUC (0.787). Operating threshold adjusted ",
    "to ", CHOSEN_THRESHOLD, " to achieve 80% recall on the minority class, ",
    "mitigating the class-imbalance failure observed at the default threshold."
  )
), file.path(DATA_DIR, "final_model_metadata.rds"))

cat("\n✔ Final model saved to data/Cleaned/final_model.rds\n")
cat("✔ Metadata saved to data/Cleaned/final_model_metadata.rds\n")
cat("✔ Chosen threshold:", CHOSEN_THRESHOLD, "\n")
cat("✔ Hand-off ready for deployment.\n")