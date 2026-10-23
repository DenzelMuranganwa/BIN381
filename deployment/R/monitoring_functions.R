
# =============================================================================
# monitoring_functions.R
# Deployment
#
# Monitoring checks:
#   PSI on predicted probability      > 0.20       -> investigate
#   KS / chi-square feature drift     p < 0.01     -> investigate
#   Missing rate on key predictors   > 5%          -> alert
#   Recall vs 0.80 baseline           drop > 10 pp  -> retrain
#   Population-group recall gap       widens > 10 pp -> retrain with reweighting
#   Retire if recall < 0.60 for two consecutive windows
# =============================================================================

PSI_LIMIT      <- 0.20
KS_ALPHA       <- 0.01
MISSING_LIMIT  <- 0.05
BASE_RECALL    <- 0.80
RECALL_DROP    <- 0.10
RETIRE_RECALL  <- 0.60
GAP_WIDEN      <- 0.10
MIN_ROWS_DRIFT <- 500

# -----------------------------------------------------------------------------
# Population Stability Index
# Bin edges are based on the reference (expected) scores.
# -----------------------------------------------------------------------------

psi <- function(expected, actual, bins = 10) {
  expected <- expected[!is.na(expected)]
  actual   <- actual[!is.na(actual)]
  
  if (length(expected) < 50 || length(actual) < 50) {
    return(NA_real_)
  }
  
  br <- unique(
    quantile(
      expected,
      probs = seq(0, 1, length.out = bins + 1),
      na.rm = TRUE
    )
  )
  
  if (length(br) < 3) {
    return(NA_real_)
  }
  
  br[1] <- -Inf
  br[length(br)] <- Inf
  
  e <- as.numeric(table(cut(expected, br))) / length(expected)
  a <- as.numeric(table(cut(actual, br))) / length(actual)
  
  e <- pmax(e, 1e-4)
  a <- pmax(a, 1e-4)
  
  sum((a - e) * log(a / e))
}

# -----------------------------------------------------------------------------
# Compare one predictor between reference and new data.
# Numeric predictors: Kolmogorov-Smirnov test.
# Categorical predictors: chi-square goodness-of-fit test.
# -----------------------------------------------------------------------------

feature_drift <- function(ref_x, new_x) {
  
  if (is.numeric(ref_x)) {
    
    r <- ref_x[!is.na(ref_x)]
    n <- as.numeric(new_x)
    n <- n[!is.na(n)]
    
    if (length(r) < 30 || length(n) < 30) {
      return(c(stat = NA_real_, p = NA_real_))
    }
    
    k <- suppressWarnings(ks.test(r, n))
    
    return(c(
      stat = unname(k$statistic),
      p = k$p.value
    ))
  }
  
  r <- as.character(ref_x)
  n <- as.character(new_x)
  
  lv <- union(
    unique(r[!is.na(r)]),
    unique(n[!is.na(n)])
  )
  
  if (length(lv) < 2) {
    return(c(stat = NA_real_, p = NA_real_))
  }
  
  rc <- table(factor(r, levels = lv))
  nc <- table(factor(n, levels = lv))
  
  if (sum(nc) < 30 || sum(rc) == 0) {
    return(c(stat = NA_real_, p = NA_real_))
  }
  
  pr <- as.numeric(rc) / sum(rc)
  pr <- pmax(pr, 1e-6)
  pr <- pr / sum(pr)
  
  ch <- suppressWarnings(
    chisq.test(
      as.numeric(nc),
      p = pr,
      rescale.p = TRUE
    )
  )
  
  c(
    stat = unname(ch$statistic),
    p = ch$p.value
  )
}

# -----------------------------------------------------------------------------
# Convert a check result into a monitoring status.
# -----------------------------------------------------------------------------

traffic <- function(bad, warn = FALSE) {
  if (is.na(bad)) {
    "n/a"
  } else if (bad) {
    "ACTION"
  } else if (isTRUE(warn)) {
    "Watch"
  } else {
    "OK"
  }
}

# -----------------------------------------------------------------------------
# Main data-drift and data-quality checks.
#
# ref_df and new_df should contain prepared predictor data.
# PSI is checked separately and has its own 50-score minimum.
# Feature-drift tests run only when new_df has at least 500 rows.
# Missing-rate checks still run for smaller batches.
# -----------------------------------------------------------------------------

check_drift <- function(ref_df, new_df, ref_scores, new_scores, ref) {
  
  rows <- list()
  small_batch <- nrow(new_df) < MIN_ROWS_DRIFT
  
  add <- function(measure, detail, value, rule, status, action) {
    rows[[length(rows) + 1]] <<- data.frame(
      Measure = measure,
      Detail = detail,
      Value = value,
      Rule = rule,
      Status = status,
      If_breached = action,
      stringsAsFactors = FALSE
    )
  }
  
  # 1. Prediction-score drift
  p <- psi(ref_scores, new_scores)
  
  add(
    "Prediction drift (PSI)",
    "prob_unemployed",
    if (is.na(p)) NA_real_ else round(p, 3),
    paste0("PSI > ", PSI_LIMIT),
    traffic(p > PSI_LIMIT, p > 0.10),
    "Investigate"
  )
  
  # 2. Feature drift
  if (!small_batch) {
    
    for (cn in intersect(ref$cols, intersect(names(ref_df), names(new_df)))) {
      
      d <- feature_drift(ref_df[[cn]], new_df[[cn]])
      p_value <- unname(d["p"])
      stat_value <- unname(d["stat"])
      
      flag <- if (is.na(p_value)) {
        NA
      } else {
        p_value < KS_ALPHA
      }
      
      # For numeric predictors, require a meaningful KS effect size too.
      if (is.numeric(ref_df[[cn]]) && !is.na(stat_value) &&
          !is.na(flag)) {
        flag <- flag && stat_value >= 0.10
      }
      
      rule_text <- if (is.numeric(ref_df[[cn]])) {
        "KS p < 0.01 and D >= 0.10"
      } else {
        "Chi-square p < 0.01"
      }
      
      add(
        "Feature drift",
        cn,
        if (is.na(p_value)) NA_real_ else signif(p_value, 3),
        rule_text,
        traffic(flag, !is.na(p_value) && p_value < KS_ALPHA),
        "Investigate"
      )
    }
    
  } else {
    
    # Explicitly record why feature drift was not assessed.
    add(
      "Feature drift",
      "Batch too small",
      NA_real_,
      paste0("At least ", MIN_ROWS_DRIFT, " rows required"),
      "n/a",
      "Collect more data before assessing feature drift"
    )
  }
  
  # 3. Missing-data checks for core predictors
  for (cn in intersect(CORE_COLS, names(new_df))) {
    
    missing_rate <- mean(is.na(new_df[[cn]]))
    
    add(
      "Missing rate",
      cn,
      round(missing_rate, 4),
      paste0("Missing rate > ", MISSING_LIMIT),
      traffic(missing_rate > MISSING_LIMIT),
      "Alert data owner"
    )
  }
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------------
# Convert unemployment labels to 1/0.
# -----------------------------------------------------------------------------

parse_truth <- function(x) {
  
  x <- trimws(as.character(x))
  
  dplyr::case_when(
    x %in% c("1", "TRUE", "True", "true",
             "Unemployed", "unemployed") ~ 1L,
    
    x %in% c("0", "FALSE", "False", "false",
             "Not_Unemployed", "Not Unemployed") ~ 0L,
    
    TRUE ~ NA_integer_
  )
}

# -----------------------------------------------------------------------------
# Calculate recall, requiring at least 30 known positive cases.
# -----------------------------------------------------------------------------

recall_of <- function(truth, flagged) {
  
  pos <- truth == 1
  
  if (sum(pos, na.rm = TRUE) < 30) {
    return(NA_real_)
  }
  
  mean(flagged[pos], na.rm = TRUE)
}

# -----------------------------------------------------------------------------
# Performance monitoring when the uploaded data contains real outcomes.
# -----------------------------------------------------------------------------

check_performance <- function(
    truth,
    flagged,
    groups = NULL,
    baseline_gap = NA_real_,
    prior_log = NULL) {
  
  out <- list()
  rec <- recall_of(truth, flagged)
  
  out[[1]] <- data.frame(
    Measure = "Recall at chosen threshold",
    Detail = "all rows",
    Value = round(rec, 3),
    Rule = "drop > 10 pp from 0.80",
    Status = traffic(
      rec < BASE_RECALL - RECALL_DROP,
      rec < BASE_RECALL - 0.05
    ),
    If_breached = "Retrain",
    stringsAsFactors = FALSE
  )
  
  # Check whether the previous monitoring window also had low recall.
  prev_low <- FALSE
  
  if (!is.null(prior_log) &&
      nrow(prior_log) > 0 &&
      "recall" %in% names(prior_log)) {
    
    lr <- suppressWarnings(
      as.numeric(tail(prior_log$recall, 1))
    )
    
    prev_low <- !is.na(lr) && lr < RETIRE_RECALL
  }
  
  retire <- !is.na(rec) &&
    rec < RETIRE_RECALL &&
    prev_low
  
  retirement_status <- if (is.na(rec)) {
    "n/a"
  } else if (retire) {
    "ACTION"
  } else if (rec < RETIRE_RECALL) {
    "Watch"
  } else {
    "OK"
  }
  
  out[[2]] <- data.frame(
    Measure = "Retirement check",
    Detail = "recall < 0.60 two windows running",
    Value = round(rec, 3),
    Rule = "2 consecutive windows < 0.60",
    Status = retirement_status,
    If_breached = "Retire and rebuild",
    stringsAsFactors = FALSE
  )
  
  # Compare recall across population groups when possible.
  if (!is.null(groups)) {
    
    g <- tapply(
      seq_along(truth),
      groups,
      function(i) recall_of(truth[i], flagged[i])
    )
    
    g <- g[!is.na(g)]
    
    if (length(g) >= 2) {
      
      gap <- max(g) - min(g)
      
      limit <- if (is.na(baseline_gap)) {
        NA_real_
      } else {
        baseline_gap + GAP_WIDEN
      }
      
      group_detail <- paste(
        names(which.min(g)),
        "vs",
        names(which.max(g))
      )
      
      group_rule <- if (is.na(limit)) {
        "gap widens > 10 pp"
      } else {
        paste0("gap > ", round(limit, 3))
      }
      
      group_status <- if (is.na(limit)) {
        "n/a"
      } else {
        traffic(gap > limit)
      }
      
      out[[3]] <- data.frame(
        Measure = "Population-group recall gap",
        Detail = group_detail,
        Value = round(gap, 3),
        Rule = group_rule,
        Status = group_status,
        If_breached = "Retrain with reweighting",
        stringsAsFactors = FALSE
      )
    }
  }
  
  list(
    table = do.call(rbind, out),
    recall = rec
  )
}

# -----------------------------------------------------------------------------
# Baseline population-group recall gap.
# -----------------------------------------------------------------------------

baseline_group_gap <- function(root) {
  
  f <- file.path(
    root,
    "outputs",
    "fairness_final_population_group.csv"
  )
  
  if (!file.exists(f)) {
    return(NA_real_)
  }
  
  d <- tryCatch(
    read.csv(f),
    error = function(e) NULL
  )
  
  if (is.null(d) || !"Recall" %in% names(d)) {
    return(NA_real_)
  }
  
  r <- d$Recall[!is.na(d$Recall)]
  
  if (length(r) < 2) {
    return(NA_real_)
  }
  
  max(r) - min(r)
}

# -----------------------------------------------------------------------------
# Monitoring log: one row per monitoring run.
# -----------------------------------------------------------------------------

log_path <- function(app_dir) {
  file.path(app_dir, "monitoring_log.csv")
}

read_log <- function(app_dir) {
  
  f <- log_path(app_dir)
  
  if (file.exists(f)) {
    tryCatch(
      read.csv(f, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  } else {
    NULL
  }
}

append_log <- function(app_dir, window, n, psi_val, n_action, recall) {
  
  row <- data.frame(
    run_time = format(Sys.time(), "%Y-%m-%d %H:%M"),
    window = window,
    rows = n,
    psi = if (is.na(psi_val)) NA_real_ else round(psi_val, 3),
    actions = n_action,
    recall = if (is.na(recall)) NA_real_ else round(recall, 3),
    stringsAsFactors = FALSE
  )
  
  f <- log_path(app_dir)
  
  write.table(
    row,
    f,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(f),
    append = file.exists(f)
  )
}
