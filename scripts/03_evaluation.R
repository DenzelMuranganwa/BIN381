# =============================================================================
# 03_evaluation.R
# Candidate model evaluation on the fixed, baked train/test data.
#
# Evaluates:
#   - GLM (baseline)
#   - Naive Bayes (baseline)
#   - RF-original (50 trees)
#   - RF-improved (500 trees, class-weighted, wider tuning grid)
#
# Uses the FIXED baked data (single Province dummy per province).
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(caret)
  library(pROC)
  library(ranger)
  library(e1071)
  
  #library(vip)
  library(scales)
})

set.seed(2026)

DATA_DIR   <- "data/Cleaned"
OUTPUT_DIR <- "outputs"
dir.create(OUTPUT_DIR, showWarnings = FALSE)

POSITIVE <- "Unemployed"
NEGATIVE <- "Not_Unemployed"

# =============================================================================
# 1. LOAD FIXED DATA
# =============================================================================
train_baked <- readRDS(file.path(DATA_DIR, "train_baked_fixed.rds"))
test_baked  <- readRDS(file.path(DATA_DIR, "test_baked_fixed.rds"))
test_raw    <- readRDS(file.path(DATA_DIR, "test_raw.rds"))

train_baked$Unemployed <- factor(train_baked$Unemployed,
                                 levels = c(0, 1),
                                 labels = c(NEGATIVE, POSITIVE))
test_baked$Unemployed  <- factor(test_baked$Unemployed,
                                 levels = c(0, 1),
                                 labels = c(NEGATIVE, POSITIVE))

impute_proc <- preProcess(train_baked, method = c("medianImpute", "zv"))
train_baked <- predict(impute_proc, train_baked)
test_baked  <- predict(impute_proc, test_baked)

cat("Train rows:", nrow(train_baked), "| Test rows:", nrow(test_baked), "\n")
cat("Test class distribution:\n"); print(prop.table(table(test_baked$Unemployed)))

# =============================================================================
# 2. FIT BASELINE MODELS
# =============================================================================
set.seed(2026)
glm_model <- train(
  Unemployed ~ .,
  data = train_baked,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary),
  metric = "ROC"
)

set.seed(2026)
nb_model <- naiveBayes(Unemployed ~ ., data = train_baked)

set.seed(2026)
rf_grid_original <- expand.grid(
  mtry          = floor(sqrt(ncol(train_baked) - 1)),
  splitrule     = "gini",
  min.node.size = 10
)
rf_original <- train(
  Unemployed ~ .,
  data = train_baked,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary),
  tuneGrid = rf_grid_original,
  num.trees = 50,
  importance = "impurity",
  metric = "ROC",
  num.threads = parallel::detectCores()
)

# =============================================================================
# 3. FIT IMPROVED RANDOM FOREST (class-weighted, 500 trees, tuned)
# =============================================================================
class_w <- 1 / as.numeric(prop.table(table(train_baked$Unemployed)))
class_w <- class_w / sum(class_w) * 2
names(class_w) <- c(NEGATIVE, POSITIVE)
cat("\nClass weights for RF-improved:\n"); print(class_w)

set.seed(2026)
rf_grid_improved <- expand.grid(
  mtry          = c(floor(sqrt(ncol(train_baked) - 1)),
                    floor((ncol(train_baked) - 1) / 3),
                    floor((ncol(train_baked) - 1) / 2)),
  splitrule     = "gini",
  min.node.size = c(5, 20)
)
rf_improved <- train(
  Unemployed ~ .,
  data = train_baked,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary),
  tuneGrid      = rf_grid_improved,
  num.trees     = 500,
  class.weights = class_w,
  importance    = "impurity",
  metric        = "ROC",
  num.threads   = parallel::detectCores()
)
cat("\nRF-improved best tuning parameters:\n"); print(rf_improved$bestTune)

# =============================================================================
# 4. PREDICTIONS
# =============================================================================
get_preds <- function(model, data, name) {
  if (inherits(model, "naiveBayes")) {
    cls  <- predict(model, newdata = data)
    prob <- predict(model, newdata = data, type = "raw")[, POSITIVE]
  } else {
    cls  <- predict(model, newdata = data)
    prob <- predict(model, newdata = data, type = "prob")[, POSITIVE]
  }
  tibble(
    truth    = data$Unemployed,
    estimate = factor(cls, levels = c(NEGATIVE, POSITIVE)),
    prob_pos = as.numeric(prob),
    model    = name
  )
}

preds <- bind_rows(
  get_preds(glm_model,   test_baked, "GLM"),
  get_preds(nb_model,    test_baked, "Naive Bayes"),
  get_preds(rf_original, test_baked, "RF-original"),
  get_preds(rf_improved, test_baked, "RF-improved")
)

# =============================================================================
# 5. COMPARISON MATRIX
# =============================================================================
cls_metrics <- function(d) {
  cm <- confusionMatrix(d$estimate, d$truth, positive = POSITIVE)
  r  <- pROC::roc(d$truth, d$prob_pos, levels = c(NEGATIVE, POSITIVE),
                  direction = "<", quiet = TRUE)
  tibble(
    Accuracy     = round(cm$overall["Accuracy"], 4),
    Precision    = round(cm$byClass["Precision"], 4),
    Recall       = round(cm$byClass["Sensitivity"], 4),
    F1           = round(cm$byClass["F1"], 4),
    Specificity  = round(cm$byClass["Specificity"], 4),
    Balanced_Acc = round(cm$byClass["Balanced Accuracy"], 4),
    ROC_AUC      = round(as.numeric(pROC::auc(r)), 4)
  )
}
metric_tbl <- preds %>%
  group_by(model) %>%
  group_modify(~ cls_metrics(.x)) %>%
  ungroup() %>%
  arrange(desc(ROC_AUC))

cat("\n================ MODEL COMPARISON MATRIX ================\n")
print(as.data.frame(metric_tbl))
write_csv(metric_tbl, file.path(OUTPUT_DIR, "model_comparison_matrix.csv"))

# =============================================================================
# 6. CONFUSION MATRICES
# =============================================================================
for (m in unique(preds$model)) {
  d  <- preds %>% filter(model == m)
  cm <- confusionMatrix(d$estimate, d$truth, positive = POSITIVE)
  cat("\n---", m, "---\n"); print(cm$table)
  cat("Recall:", round(cm$byClass["Sensitivity"], 4),
      "| Precision:", round(cm$byClass["Precision"], 4), "\n")
}
cm_df <- preds %>%
  group_by(model) %>%
  summarise(cm = list(as.data.frame(table(Predicted = estimate, Actual = truth)))) %>%
  unnest(cm)

ggplot(cm_df, aes(Actual, Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 4.5) +
  facet_wrap(~ model) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Confusion Matrices — Test Set", fill = "Count") +
  theme_minimal(base_size = 12)
ggsave(file.path(OUTPUT_DIR, "confusion_matrices.png"),
       width = 13, height = 5, dpi = 300)

# =============================================================================
# 7. ROC CURVES
# =============================================================================
roc_df <- preds %>%
  group_by(model) %>%
  group_modify(~ {
    r <- pROC::roc(.x$truth, .x$prob_pos, levels = c(NEGATIVE, POSITIVE),
                   direction = "<", quiet = TRUE)
    tibble(sens = r$sensitivities, spec = r$specificities)
  }) %>%
  mutate(fpr = 1 - spec)

ggplot(roc_df, aes(fpr, sens, color = model)) +
  geom_path(size = 1.1) +
  geom_abline(linetype = "dashed", color = "grey50") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "ROC Curves — Test Set",
       x = "False Positive Rate", y = "True Positive Rate", color = "Model") +
  theme_minimal(base_size = 12)
ggsave(file.path(OUTPUT_DIR, "roc_curves.png"), width = 9, height = 6, dpi = 300)

# =============================================================================
# 8. PRECISION–RECALL CURVES
# =============================================================================
pr_df <- preds %>%
  group_by(model) %>%
  group_modify(~ {
    map_dfr(seq(0.05, 0.95, 0.05), function(th) {
      pred_th <- factor(ifelse(.x$prob_pos >= th, POSITIVE, NEGATIVE),
                        levels = c(NEGATIVE, POSITIVE))
      cm <- confusionMatrix(pred_th, .x$truth, positive = POSITIVE)
      tibble(threshold = th,
             recall    = cm$byClass["Sensitivity"],
             precision = cm$byClass["Precision"])
    })
  })

ggplot(pr_df, aes(recall, precision, color = model)) +
  geom_path(size = 1.1) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Precision–Recall Curves — Test Set",
       x = "Recall", y = "Precision", color = "Model") +
  theme_minimal(base_size = 12)
ggsave(file.path(OUTPUT_DIR, "pr_curves.png"), width = 9, height = 6, dpi = 300)

# =============================================================================
# 9. THRESHOLD ANALYSIS
# =============================================================================
pr_df %>%
  pivot_longer(c(recall, precision)) %>%
  ggplot(aes(threshold, value, color = name)) +
  geom_line(size = 1) +
  facet_wrap(~ model) +
  labs(title = "Metrics vs Decision Threshold",
       y = "Metric value", x = "Threshold", color = "Metric") +
  theme_minimal(base_size = 11)
ggsave(file.path(OUTPUT_DIR, "threshold_analysis.png"),
       width = 13, height = 5, dpi = 300)
write_csv(pr_df, file.path(OUTPUT_DIR, "threshold_results.csv"))

# =============================================================================
# 10. BOOTSTRAP 95% CIs
# =============================================================================
boot_metric <- function(d, fun, R = 1000) {
  n <- nrow(d); out <- numeric(R)
  for (i in seq_len(R)) {
    idx <- sample.int(n, n, replace = TRUE)
    out[i] <- tryCatch(fun(d[idx, ]), error = function(e) NA_real_)
  }
  quantile(out, c(0.025, 0.5, 0.975), na.rm = TRUE)
}
boot_tbl <- preds %>%
  group_by(model) %>%
  group_modify(~ {
    auc_fun <- function(d) {
      r <- pROC::roc(d$truth, d$prob_pos, levels = c(NEGATIVE, POSITIVE),
                     direction = "<", quiet = TRUE)
      as.numeric(pROC::auc(r))
    }
    rec_fun <- function(d) {
      cm <- confusionMatrix(d$estimate, d$truth, positive = POSITIVE)
      cm$byClass["Sensitivity"]
    }
    a <- boot_metric(.x, auc_fun)
    r <- boot_metric(.x, rec_fun)
    tibble(
      AUC_median = round(a[2], 4), AUC_lo = round(a[1], 4), AUC_hi = round(a[3], 4),
      Recall_median = round(r[2], 4), Recall_lo = round(r[1], 4), Recall_hi = round(r[3], 4)
    )
  })

cat("\n================ BOOTSTRAP 95% CIs ================\n")
print(as.data.frame(boot_tbl))
write_csv(boot_tbl, file.path(OUTPUT_DIR, "bootstrap_CIs.csv"))

# =============================================================================
# 11. FEATURE IMPORTANCE (RF-improved) — caret version, no vip needed
# =============================================================================
imp <- varImp(rf_improved, scale = TRUE)
imp_df <- as.data.frame(imp$importance) %>%
  tibble::rownames_to_column("Feature") %>%
  arrange(desc(Overall)) %>%
  slice_head(n = 15)

ggplot(imp_df, aes(reorder(Feature, Overall), Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "RF-improved — Top 15 Predictors of Unemployment Risk",
       x = NULL, y = "Importance (scaled)") +
  theme_minimal(base_size = 11)

ggsave(file.path(OUTPUT_DIR, "rf_feature_importance.png"),
       width = 8, height = 6, dpi = 300)

# =============================================================================
# 12. FAIRNESS ANALYSIS
# =============================================================================
stopifnot(nrow(test_baked) == nrow(test_raw))
subgroup_cols <- c("Province", "gender", "population_group",
                   "geo_type", "youth_flag", "quarter_label")
subgroup_cols <- intersect(subgroup_cols, names(test_raw))
test_meta <- test_raw %>% select(all_of(subgroup_cols))

fairness_report <- function(model_name) {
  preds_m <- preds %>% filter(model == model_name)
  for (g in subgroup_cols) {
    df <- preds_m %>% bind_cols(test_meta) %>% filter(!is.na(.data[[g]]))
    tbl <- df %>%
      group_by(Group = .data[[g]]) %>%
      group_modify(~ {
        n <- nrow(.x)
        if (n < 30) return(tibble(n = n, Recall = NA_real_,
                                  Precision = NA_real_, F1 = NA_real_,
                                  Accuracy = NA_real_))
        cm <- confusionMatrix(.x$estimate, .x$truth, positive = POSITIVE)
        tibble(
          n         = n,
          Recall    = round(cm$byClass["Sensitivity"], 3),
          Precision = round(cm$byClass["Precision"], 3),
          F1        = round(cm$byClass["F1"], 3),
          Accuracy  = round(cm$overall["Accuracy"], 3)
        )
      }) %>% ungroup()
    cat("\n=== Fairness —", model_name, "by", g, "===\n")
    print(as.data.frame(tbl))
    write_csv(tbl, file.path(OUTPUT_DIR,
                             paste0("fairness_", gsub("[^A-Za-z]", "", model_name), "_", g, ".csv")))
  }
}
for (m in unique(preds$model)) fairness_report(m)

# =============================================================================
# 13. SELECT FINAL MODEL
# =============================================================================
final_name <- metric_tbl %>% slice_max(ROC_AUC, n = 1) %>% pull(model)
cat("\n>>> FINAL MODEL SELECTED:", final_name, "\n")

final_model <- switch(final_name,
                      "GLM"         = glm_model,
                      "Naive Bayes" = nb_model,
                      "RF-original" = rf_original,
                      "RF-improved" = rf_improved
)
saveRDS(final_model, file.path(DATA_DIR, "final_model.rds"))
saveRDS(list(
  final_model_name = final_name,
  comparison       = metric_tbl,
  boot_CIs         = boot_tbl,
  impute_proc      = impute_proc
), file.path(DATA_DIR, "final_model_metadata.rds"))

cat("\n✔ Evaluation complete. Outputs in ./outputs/\n")
cat("✔ Final model:", final_name, "\n")
cat("✔ Saved to: data/Cleaned/final_model.rds\n")