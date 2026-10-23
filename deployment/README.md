# Shiny scoring prototype

A Shiny app for scoring an input record or CSV, reviewing model information, and monitoring score and feature drift. It uses the final random-forest model and a 0.20 flag threshold.

## Files

| File | Purpose |
|---|---|
| `app.R` | Scoring and monitoring interface |
| `R/scoring_functions.R` | Loads model artefacts, validates inputs, and scores records |
| `R/monitoring_functions.R` | Drift, PSI, recall/fairness checks, and monitoring log |
| `01_build_monitoring_baseline.R` | Optionally creates the monitoring reference baseline |
| `tests/smoke_test.R` | Checks scoring, validation, and monitoring behavior |

## Run

Run these commands from the project root (the folder containing `data/`):

```r
install.packages(c("shiny", "DT", "ggplot2", "dplyr", "stringr", "recipes", "caret", "ranger"))

source("deployment/tests/smoke_test.R")
shiny::runApp("deployment")
```

The app expects these generated files in `data/Cleaned/`: `final_model.rds`, `final_model_metadata.rds`, `recipe_prepped_fixed.rds`, `train_raw.rds`, and `test_raw.rds`. Run the analysis and modelling workflow from the main README first. It also reads `outputs/model_comparison_matrix.csv` and fairness evaluation CSVs for the About tab.

To build the monitoring baseline explicitly, run from the project root:

```r
source("deployment/01_build_monitoring_baseline.R")
```

Without a saved baseline, the app attempts to create one from the held-out test data when it starts. Runtime baseline and monitoring log files are local generated artefacts and are excluded from Git.

## Input handling

The app validates required columns, numeric values, plausible ages, and a 50,000-row limit. It standardizes text, warns about unseen categories or out-of-range values, fills missing optional fields with typical training values, applies the saved recipe and imputation values, and scores with the saved model. A record is flagged when its predicted probability is at least 0.20.

Monitoring checks include score PSI, feature drift, missingness, recall change, and population-group recall gaps. Recall and group-gap checks require a true `Unemployed` outcome column in the uploaded batch.

## Limitations

- Scores show associations in the 2024 QLFS data and do not establish causes.
- Precision at the selected threshold is approximately one in three; flags are not decisions about individuals.
- Group performance differs, and monitoring results depend on batch size and data quality.
- Do not use the model for automated eligibility, hiring, or service-allocation decisions.
