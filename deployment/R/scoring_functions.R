# =============================================================================
# scoring_functions.R
#Deployment 
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(recipes)
  library(caret)
  library(ranger)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

POSITIVE <- "Unemployed"
NEGATIVE <- "Not_Unemployed"

# Columns we insist on. If any of these are missing from an upload we stop,
# because filling them with a "typical" value would make the score meaningless.
CORE_COLS <- c("Age", "Province", "gender", "population_group", "geo_type")

# ---- locating things ---------------------------------------------------------
find_project_root <- function() {
  for (p in c(".", "..", "../..")) {
    if (dir.exists(file.path(p, "data", "Cleaned"))) return(normalizePath(p))
  }
  stop("Couldn't find data/Cleaned. Run the app from the project folder, ",
       "e.g. shiny::runApp('deployment').")
}

# Same clean-up that 02b_fix_baked.R applied before the recipe was prepped.
# If this ever drifts from 02b, the dummy columns won't line up - keep in sync.
standardise_inputs <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if ("Province" %in% names(df)) {
    df$Province <- str_to_title(str_trim(as.character(df$Province)))
  }
  is_chr <- vapply(df, is.character, logical(1))
  if ("Province" %in% names(df)) is_chr["Province"] <- FALSE
  df[is_chr] <- lapply(df[is_chr], str_trim)
  df
}

# ---- artefacts -----------------------------------------------------------------
load_artefacts <- function(root = find_project_root()) {
  d <- file.path(root, "data", "Cleaned")
  need <- c("final_model.rds", "final_model_metadata.rds",
            "recipe_prepped_fixed.rds", "train_raw.rds", "test_raw.rds")
  gone <- need[!file.exists(file.path(d, need))]
  if (length(gone)) stop("Missing files in data/Cleaned: ", paste(gone, collapse = ", "))

  meta <- readRDS(file.path(d, "final_model_metadata.rds"))
  art <- list(
    root        = root,
    model       = readRDS(file.path(d, "final_model.rds")),
    recipe      = readRDS(file.path(d, "recipe_prepped_fixed.rds")),
    meta        = meta,
    impute_proc = meta$impute_proc,
    threshold   = meta$chosen_threshold %||% 0.20,
    train_raw   = standardise_inputs(readRDS(file.path(d, "train_raw.rds"))),
    test_raw    = standardise_inputs(readRDS(file.path(d, "test_raw.rds")))
  )
  art$ref <- build_reference(art)
  art
}

# Which raw columns does the recipe actually expect? (read it from the recipe
# rather than hard-coding, so it stays right if the recipe is ever re-prepped)
predictor_columns <- function(recipe_obj) {
  vi <- recipe_obj$var_info
  as.character(vi$variable[vi$role == "predictor"])
}

# ---- reference profile of the training data -----------------------------------
# One entry per predictor: type, a default ("typical") value, and either the
# allowed levels or the observed range. Used for the form, for filling optional
# columns, and for validation warnings.
build_reference <- function(art) {
  cols <- intersect(predictor_columns(art$recipe), names(art$train_raw))
  tr <- art$train_raw
  info <- lapply(cols, function(cn) {
    x <- tr[[cn]]
    if (is.numeric(x)) {
      u <- sort(unique(x[!is.na(x)]))
      code_like <- length(u) <= 30 && all(u == round(u))
      list(kind = "numeric", default = median(x, na.rm = TRUE),
           min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE),
           choices = if (code_like) u else NULL, was_factor = FALSE)
    } else {
      xc <- as.character(x)
      tb <- sort(table(xc[!is.na(xc)]), decreasing = TRUE)
      list(kind = "categorical", default = names(tb)[1],
           levels = sort(names(tb)), was_factor = is.factor(x),
           train_levels = if (is.factor(x)) levels(x) else NULL)
    }
  })
  names(info) <- cols
  list(cols = cols, info = info,
       prevalence = mean(tr$Unemployed %in% c(1, "1", POSITIVE), na.rm = TRUE))
}

# Fill youth_flag from Age, but only if the training labels are what we expect.
derive_youth_flag <- function(df, ref) {
  inf <- ref$info[["youth_flag"]]
  if (is.null(inf) || !"Age" %in% names(df)) return(df)
  lv <- inf$levels
  yl <- lv[grepl("^Youth", lv)]
  nl <- lv[grepl("^Non", lv)]
  if (length(yl) == 1 && length(nl) == 1) {
    age <- suppressWarnings(as.numeric(df$Age))
    df$youth_flag <- ifelse(age <= 34, yl, nl)
  }
  df
}

# ---- validation ------------------------------------------------------------------
# Returns list(ok, errors, warnings, data). `data` is only usable when ok = TRUE.
prepare_input <- function(df, ref, max_rows = 50000) {
  errors <- character(); warns <- character()
  df <- standardise_inputs(df)

  if (nrow(df) == 0) {
    return(list(ok = FALSE, errors = "The file has no rows.", warnings = warns, data = NULL))
  }
  if (nrow(df) > max_rows) {
    return(list(ok = FALSE, data = NULL, warnings = warns,
                errors = sprintf("Too many rows (%s). The limit is %s per upload - please split the file.",
                                 format(nrow(df), big.mark = ","), format(max_rows, big.mark = ","))))
  }

  # youth_flag can be worked out from Age, so don't make people supply it
  if (!"youth_flag" %in% names(df)) df <- derive_youth_flag(df, ref)

  core <- intersect(CORE_COLS, ref$cols)
  miss_core <- setdiff(core, names(df))
  if (length(miss_core)) {
    errors <- c(errors, paste0("Required column(s) missing: ", paste(miss_core, collapse = ", "),
                               ". Download the template on this page to see the expected layout."))
  }

  # optional columns: fill with the typical training value, but say so
  optional_missing <- setdiff(ref$cols, c(names(df), miss_core))
  for (cn in optional_missing) df[[cn]] <- ref$info[[cn]]$default
  if (length(optional_missing)) {
    warns <- c(warns, paste0(length(optional_missing), " optional column(s) weren't supplied and were set to the ",
                             "most typical training value: ", paste(optional_missing, collapse = ", "),
                             ". Scores will be less specific to each person."))
  }
  if (length(errors)) return(list(ok = FALSE, errors = errors, warnings = warns, data = NULL))

  for (cn in ref$cols) {
    inf <- ref$info[[cn]]
    raw <- df[[cn]]
    if (inf$kind == "numeric") {
      v <- suppressWarnings(as.numeric(raw))
      bad <- is.na(v) & !is.na(raw) & trimws(as.character(raw)) != ""
      if (any(bad)) {
        errors <- c(errors, sprintf("'%s' has %d value(s) that aren't numbers (first bad row: %d).",
                                    cn, sum(bad), which(bad)[1]))
      }
      if (cn == "Age" && any(v < 0 | v > 120, na.rm = TRUE)) {
        errors <- c(errors, sprintf("'Age' has %d impossible value(s) (below 0 or above 120).",
                                    sum(v < 0 | v > 120, na.rm = TRUE)))
      }
      outside <- sum(v < inf$min | v > inf$max, na.rm = TRUE)
      if (outside > 0 && !(cn == "Age" && any(v < 0 | v > 120, na.rm = TRUE))) {
        warns <- c(warns, sprintf("'%s': %d value(s) fall outside the range the model was trained on (%s to %s). Treat those scores with extra care.",
                                  cn, outside, inf$min, inf$max))
      }
      df[[cn]] <- v
    } else {
      v <- trimws(as.character(raw))
      v[v == ""] <- NA
      unseen <- setdiff(unique(v[!is.na(v)]), inf$levels)
      if (length(unseen)) {
        warns <- c(warns, sprintf("'%s': unseen value(s) %s will be treated as 'new category'.",
                                  cn, paste0("'", head(unseen, 4), "'", collapse = ", ")))
      }
      df[[cn]] <- if (isTRUE(inf$was_factor)) factor(v, levels = inf$train_levels) else v
    }
  }

  # blanks in the core columns are a data-quality problem worth flagging
  for (cn in core) {
    share <- mean(is.na(df[[cn]]))
    if (share > 0.05) warns <- c(warns, sprintf("'%s' is blank for %.1f%% of rows (the monitoring alert level is 5%%).", cn, 100 * share))
  }

  if (length(errors)) return(list(ok = FALSE, errors = errors, warnings = warns, data = NULL))
  list(ok = TRUE, errors = character(), warnings = warns, data = df[, ref$cols, drop = FALSE])
}

# ---- scoring ---------------------------------------------------------------------
# `df_ready` must be the output of prepare_input()$data
score_data <- function(df_ready, art) {
  baked <- as.data.frame(recipes::bake(art$recipe, new_data = df_ready))
  baked$Unemployed <- NULL

  imputed <- tryCatch(
    predict(art$impute_proc, baked),
    error = function(e) {
      b2 <- baked
      b2$Unemployed <- factor(NA, levels = c(NEGATIVE, POSITIVE))
      predict(art$impute_proc, b2)
    }
  )
  imputed <- as.data.frame(imputed)
  imputed$Unemployed <- NULL

  p <- predict(art$model, newdata = imputed, type = "prob")[, POSITIVE]
  thr <- art$threshold
  tibble::tibble(
    prob_unemployed = round(as.numeric(p), 4),
    flagged = p >= thr,
    risk_band = dplyr::case_when(
      p >= 2 * thr ~ "Well above threshold",
      p >= thr     ~ "Above threshold (flagged)",
      TRUE         ~ "Below threshold"
    )
  )
}

# template row for the "download template" button
make_template <- function(ref) {
  row <- lapply(ref$cols, function(cn) ref$info[[cn]]$default)
  names(row) <- ref$cols
  as.data.frame(row, stringsAsFactors = FALSE)
}
