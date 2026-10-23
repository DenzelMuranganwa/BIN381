# =============================================================================
# 03b_rf_balanced.R
# Patch: Fit a properly class-balanced Random Forest using ranger directly.
# Reason: caret::train() did not propagate class.weights correctly, so all
#         RF variants collapsed to majority-class prediction.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(caret)
  library(pROC)
  library(ranger)
  library(e1071)
  library(scales)
})

set.seed(2026)
DATA_DIR   <- "data/Cleaned"
OUTPUT_DIR <- "outputs"
POSITIVE   <- "Unemployed"
NEGATIVE   <- "Not_Unemployed"

# ---- 1. Reload the fixed baked data ----------------------------------------
train_baked <- readRDS(file.path(DATA_DIR, "train_baked_fixed.rds"))
test_baked  <- readRDS(file.path(DATA_DIR, "test_baked_fixed.rds"))

train_baked$Unemployed <- factor(train_baked$Unemployed,
                                 levels = c(0, 1),
                                 labels = c(NEGATIVE, POSITIVE))
test_baked$Unemployed  <- factor(test_baked$Unemployed,
                                 levels = c(0, 1),
                                 labels = c(NEGATIVE, POSITIVE))

impute_proc <- preProcess(train_baked, method = c("medianImpute", "zv"))
train_baked <- predict(impute_proc, train_baked)
test_baked  <- predict(impute_proc, test_baked)

# ---- 2. Compute class weights (inverse-frequency, normalized) -------------
freq <- prop.table(table(train_baked$Unemployed))
class_w <- 1 / freq
class_w <- class_w / sum(class_w) * 2
names(class_w) <- c(NEGATIVE, POSITIVE)
cat("Class weights:\n"); print(class_w)

# ---- 3. Fit balanced RF using ranger DIRECTLY -----------------------------
set.seed(2026)
rf_balanced <- ranger(
  dependent.variable.name = "Unemployed",
  data                    = train_baked,
  num.trees               = 500,
  mtry                    = floor(sqrt(ncol(train_baked) - 1)),
  min.node.size           = 10,
  class.weights           = class_w,
  probability             = TRUE,
  importance              = "impurity",
  num.threads             = parallel::detectCores()
)

cat("\nranger model fitted. OOB prediction error:", rf_balanced$prediction.error, "\n")

# ---- 4. Predict on test set ----------------------------------------------
prob_pos <- predict(rf_balanced, data = test_baked)$predictions[, POSITIVE]
estimate <- factor(ifelse(prob_pos >= 0.5, POSITIVE, NEGATIVE),
                   levels = c(NEGATIVE, POSITIVE))

# ---- 5. Evaluate -----------------------------------------------------------
cm <- confusionMatrix(estimate, test_baked$Unemployed, positive = POSITIVE)
cat("\n--- RF-balanced confusion matrix ---\n"); print(cm$table)
cat("Recall:",    round(cm$byClass["Sensitivity"], 4),
    "| Precision:", round(cm$byClass["Precision"],   4),
    "| Specificity:", round(cm$byClass["Specificity"], 4), "\n")

r <- pROC::roc(test_baked$Unemployed, prob_pos,
               levels = c(NEGATIVE, POSITIVE),
               direction = "<", quiet = TRUE)
cat("ROC-AUC:", round(as.numeric(pROC::auc(r)), 4), "\n")

# ---- 6. Compare to previously evaluated models ---------------------------
prev <- read.csv(file.path(OUTPUT_DIR, "model_comparison_matrix.csv"))
new_row <- tibble(
  model        = "RF-balanced",
  Accuracy     = round(cm$overall["Accuracy"], 4),
  Precision    = round(cm$byClass["Precision"], 4),
  Recall       = round(cm$byClass["Sensitivity"], 4),
  F1           = round(cm$byClass["F1"], 4),
  Specificity  = round(cm$byClass["Specificity"], 4),
  Balanced_Acc = round(cm$byClass["Balanced Accuracy"], 4),
  ROC_AUC      = round(as.numeric(pROC::auc(r)), 4)
)

final_matrix <- bind_rows(prev, new_row) %>% arrange(desc(ROC_AUC))
cat("\n================ UPDATED MODEL COMPARISON MATRIX ================\n")
print(final_matrix)
write_csv(final_matrix, file.path(OUTPUT_DIR, "model_comparison_matrix.csv"))

# ---- 7. Bootstrap CIs for the new model ----------------------------------
boot_metric <- function(prob, truth, fun, R = 1000) {
  n <- length(prob); out <- numeric(R)
  for (i in seq_len(R)) {
    idx <- sample.int(n, n, replace = TRUE)
    out[i] <- tryCatch(fun(prob[idx], truth[idx]), error = function(e) NA_real_)
  }
  quantile(out, c(0.025, 0.5, 0.975), na.rm = TRUE)
}

est_boot <- function(p, t) {
  e <- factor(ifelse(p >= 0.5, POSITIVE, NEGATIVE), levels = c(NEGATIVE, POSITIVE))
  confusionMatrix(e, t, positive = POSITIVE)$byClass["Sensitivity"]
}
auc_boot <- function(p, t) {
  rr <- pROC::roc(t, p, levels = c(NEGATIVE, POSITIVE),
                  direction = "<", quiet = TRUE)
  as.numeric(pROC::auc(rr))
}

auc_ci <- boot_metric(prob_pos, test_baked$Unemployed, auc_boot)
rec_ci <- boot_metric(prob_pos, test_baked$Unemployed, est_boot)

boot_row <- tibble(
  model         = "RF-balanced",
  AUC_median    = round(auc_ci[2], 4),
  AUC_lo        = round(auc_ci[1], 4),
  AUC_hi        = round(auc_ci[3], 4),
  Recall_median = round(rec_ci[2], 4),
  Recall_lo     = round(rec_ci[1], 4),
  Recall_hi     = round(rec_ci[3], 4)
)

prev_ci <- read.csv(file.path(OUTPUT_DIR, "bootstrap_CIs.csv"))
final_ci <- bind_rows(prev_ci, boot_row)
cat("\n================ UPDATED BOOTSTRAP CIs ================\n")
print(final_ci)
write_csv(final_ci, file.path(OUTPUT_DIR, "bootstrap_CIs.csv"))

# ---- 8. Fairness for RF-balanced -----------------------------------------
test_raw <- readRDS(file.path(DATA_DIR, "test_raw.rds"))
subgroup_cols <- intersect(
  c("Province", "gender", "population_group",
    "geo_type", "youth_flag", "quarter_label"),
  names(test_raw)
)
test_meta <- test_raw %>% select(all_of(subgroup_cols))

preds_bal <- tibble(
  truth    = test_baked$Unemployed,
  estimate = estimate,
  prob_pos = prob_pos,
  model    = "RF-balanced"
) %>% bind_cols(test_meta)

for (g in subgroup_cols) {
  tbl <- preds_bal %>%
    filter(!is.na(.data[[g]])) %>%
    group_by(Group = .data[[g]]) %>%
    group_modify(~ {
      n <- nrow(.x)
      if (n < 30) return(tibble(n = n, Recall = NA_real_, Precision = NA_real_,
                                F1 = NA_real_, Accuracy = NA_real_))
      c <- confusionMatrix(.x$estimate, .x$truth, positive = POSITIVE)
      tibble(n = n,
             Recall    = round(c$byClass["Sensitivity"], 3),
             Precision = round(c$byClass["Precision"], 3),
             F1        = round(c$byClass["F1"], 3),
             Accuracy  = round(c$overall["Accuracy"], 3))
    }) %>% ungroup()
  
  cat("\n=== RF-balanced fairness by", g, "===\n")
  print(as.data.frame(tbl))
  write_csv(tbl, file.path(OUTPUT_DIR, paste0("fairness_RFbalanced_", g, ".csv")))
}

# ---- 9. Save the balanced RF + updated metadata --------------------------
saveRDS(rf_balanced, file.path(DATA_DIR, "final_model_balanced.rds"))
saveRDS(list(
  final_model_name = "RF-balanced",
  comparison       = final_matrix,
  boot_CIs         = final_ci,
  impute_proc      = impute_proc,
  class_weights    = class_w
), file.path(DATA_DIR, "final_model_metadata.rds"))

cat("\n✔ Patch complete.\n")
cat("✔ Balanced RF saved to data/Cleaned/final_model_balanced.rds\n")
cat("✔ Updated metadata saved to data/Cleaned/final_model_metadata.rds\n")