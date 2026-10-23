# =============================================================================
# SDHSA Unemployment Risk Explorer - Shiny app
# Deployment
#
# Run from the project root:
#   shiny::runApp("deployment")
#
# =============================================================================

library(shiny)
library(DT)
library(ggplot2)

# -----------------------------------------------------------------------------
# Paths and artefacts
# -----------------------------------------------------------------------------

app_dir <- normalizePath(".")
if (!file.exists(file.path(app_dir, "R", "scoring_functions.R"))) {
  app_dir <- normalizePath("deployment")
}

source(file.path(app_dir, "R", "scoring_functions.R"))
source(file.path(app_dir, "R", "monitoring_functions.R"))

art <- load_artefacts()
ref <- art$ref
THR <- art$threshold

# -----------------------------------------------------------------------------
# IMPORTANT: allow large CSV uploads
# -----------------------------------------------------------------------------

# Shiny's default request size is relatively small.
# 1 GB is more than enough for the CSV sizes expected by this application.
options(shiny.maxRequestSize = 1024 * 1024^2)

# -----------------------------------------------------------------------------
# Monitoring baseline
# -----------------------------------------------------------------------------

base_file <- file.path(app_dir, "monitoring_baseline.rds")

if (file.exists(base_file)) {
  
  baseline <- readRDS(base_file)
  
} else {
  
  prep0 <- prepare_input(
    art$test_raw,
    ref,
    max_rows = Inf
  )
  
  sc0 <- score_data(prep0$data, art)
  
  baseline <- list(
    built_on = Sys.time(),
    n = nrow(prep0$data),
    threshold = THR,
    scores = sc0$prob_unemployed,
    features = prep0$data,
    flag_rate = mean(sc0$flagged)
  )
  
  try(
    saveRDS(baseline, base_file),
    silent = TRUE
  )
}

base_gap <- baseline_group_gap(art$root)

# -----------------------------------------------------------------------------
# Model comparison information
# -----------------------------------------------------------------------------

final_row <- tryCatch({
  
  m <- read.csv(
    file.path(
      art$root,
      "outputs",
      "model_comparison_matrix.csv"
    )
  )
  
  x <- m[
    grepl(
      "th=0.20",
      m$model,
      fixed = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  if (nrow(x) > 0) x[1, , drop = FALSE] else NULL
  
}, error = function(e) NULL)

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

make_field <- function(cn, prefix = "f_") {
  
  inf <- ref$info[[cn]]
  
  id <- paste0(prefix, cn)
  
  if (inf$kind == "categorical") {
    
    selectInput(
      id,
      cn,
      choices = inf$levels,
      selected = inf$default,
      width = "100%"
    )
    
  } else if (!is.null(inf$choices)) {
    
    selectInput(
      id,
      cn,
      choices = inf$choices,
      selected = inf$default,
      width = "100%"
    )
    
  } else {
    
    numericInput(
      id,
      cn,
      value = round(inf$default),
      min = inf$min,
      max = inf$max,
      width = "100%"
    )
  }
}

status_colour <- function(tbl_id = NULL) {
  
  JS(
    "function(row, data) {
       var s = data[4];

       if (s === 'ACTION') {
         $('td:eq(4)', row).css({
           'background':'#fee2e2',
           'color':'#991b1b',
           'font-weight':'700'
         });
       }

       else if (s === 'Watch') {
         $('td:eq(4)', row).css({
           'background':'#fef3c7',
           'color':'#92400e',
           'font-weight':'700'
         });
       }

       else if (s === 'OK') {
         $('td:eq(4)', row).css({
           'background':'#dcfce7',
           'color':'#166534',
           'font-weight':'700'
         });
       }
     }"
  )
}

key_cols <- intersect(
  c(
    "Age",
    "Province",
    "gender",
    "population_group",
    "geo_type"
  ),
  ref$cols
)

adv_cols <- setdiff(
  ref$cols,
  c(key_cols, "youth_flag")
)

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    
    tags$style(
      HTML(
        "

        /* ================================================================
           GLOBAL
           ================================================================ */

        body {
          font-family:
            -apple-system,
            BlinkMacSystemFont,
            'Segoe UI',
            Roboto,
            Arial,
            sans-serif;

          background: #f5f7fb;
          color: #172033;
          margin: 0;
        }

        .container-fluid {
          max-width: 1500px;
          padding: 0 28px 40px 28px;
        }

        h1, h2, h3, h4 {
          color: #172033;
          font-weight: 700;
        }

        p {
          line-height: 1.55;
        }

        /* ================================================================
           HEADER
           ================================================================ */

        .app-header {
          background:
            linear-gradient(
              135deg,
              #12355b 0%,
              #176b87 55%,
              #1d8a70 100%
            );

          color: white;
          padding: 30px 34px;
          margin: 0 -28px 24px -28px;
          border-radius: 0 0 18px 18px;

          box-shadow:
            0 8px 25px rgba(18, 53, 91, 0.18);
        }

        .app-title {
          font-size: 32px;
          font-weight: 750;
          letter-spacing: -0.5px;
          margin: 0 0 6px 0;
          color: white;
        }

        .app-subtitle {
          font-size: 15px;
          color: rgba(255,255,255,0.86);
          margin: 0;
        }

        .app-badge {
          display: inline-block;
          margin-top: 15px;
          padding: 6px 12px;
          border-radius: 20px;
          background: rgba(255,255,255,0.15);
          border: 1px solid rgba(255,255,255,0.25);
          font-size: 12px;
          font-weight: 600;
        }

        /* ================================================================
           RESPONSIBLE USE
           ================================================================ */

        .decision-banner {
          background: #ffffff;
          border: 1px solid #dbe4ef;
          border-left: 5px solid #176b87;
          border-radius: 12px;
          padding: 16px 20px;
          margin-bottom: 24px;

          box-shadow:
            0 3px 12px rgba(20, 40, 70, 0.05);
        }

        .decision-banner-title {
          font-size: 14px;
          font-weight: 750;
          color: #12355b;
          margin-bottom: 4px;
        }

        .decision-banner-text {
          font-size: 13px;
          color: #526174;
          margin: 0;
        }

        /* ================================================================
           TABS
           ================================================================ */

        .nav-tabs {
          border-bottom: 2px solid #e4eaf2;
          margin-bottom: 25px;
        }

        .nav-tabs > li > a {
          border: none !important;
          color: #64748b;
          font-weight: 650;
          padding: 13px 19px;
          border-radius: 8px 8px 0 0;
        }

        .nav-tabs > li > a:hover {
          background: #edf5f7;
          color: #176b87;
        }

        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover {
          color: #176b87;
          background: white;
          border-bottom: 3px solid #176b87 !important;
        }

        /* ================================================================
           CARDS
           ================================================================ */

        .card {
          background: white;
          border: 1px solid #e3e9f1;
          border-radius: 14px;
          padding: 22px;
          margin-bottom: 20px;

          box-shadow:
            0 4px 15px rgba(20, 40, 70, 0.045);
        }

        .card-title {
          font-size: 18px;
          font-weight: 750;
          color: #172033;
          margin: 0 0 5px 0;
        }

        .card-subtitle {
          font-size: 13px;
          color: #718096;
          margin-bottom: 18px;
        }

        /* ================================================================
           SIDEBAR
           ================================================================ */

        .well {
          background: white;
          border: 1px solid #e3e9f1;
          border-radius: 14px;
          box-shadow:
            0 4px 15px rgba(20, 40, 70, 0.045);
          padding: 22px;
        }

        .well h4 {
          margin-top: 0;
        }

        /* ================================================================
           FORM CONTROLS
           ================================================================ */

        .form-group {
          margin-bottom: 18px;
        }

        .form-control {
          border: 1px solid #cfd8e3;
          border-radius: 8px;
          min-height: 40px;
          box-shadow: none;
          width: 100%;
        }

        .form-control:focus {
          border-color: #176b87;
          box-shadow: 0 0 0 3px rgba(23,107,135,0.10);
        }

        label {
          font-weight: 650;
          font-size: 13px;
          color: #374151;
          margin-bottom: 7px;
        }

        /* ================================================================
           FILE INPUTS
           ================================================================ */

        .shiny-file-input {
          margin-bottom: 22px;
        }

        .shiny-file-input-progress {
          margin-top: 8px;
        }

        .fileinput-button {
          min-height: 42px !important;
          padding: 9px 16px !important;
          line-height: 22px !important;
          white-space: nowrap !important;
        }

        .form-control.file-caption {
          min-height: 42px !important;
          line-height: 22px !important;
        }

        .btn-file {
          min-height: 42px !important;
          padding: 9px 16px !important;
          line-height: 22px !important;
          white-space: nowrap !important;
        }

        /* ================================================================
           BUTTONS
           ================================================================ */

        .btn {
          border-radius: 9px !important;
          font-weight: 700 !important;
          min-height: 46px !important;
          height: auto !important;
          padding: 11px 20px !important;
          border: none !important;
          transition: all 0.18s ease !important;

          /* Prevent text from colliding/wrapping badly */
          white-space: normal !important;
          line-height: 1.35 !important;
          text-align: center !important;
        }

        .btn-primary {
          background:
            linear-gradient(
              135deg,
              #176b87,
              #155e75
            ) !important;

          color: white !important;

          box-shadow:
            0 4px 10px rgba(23,107,135,0.25) !important;
        }

        .btn-primary:hover,
        .btn-primary:focus {
          background:
            linear-gradient(
              135deg,
              #155e75,
              #124c60
            ) !important;

          color: white !important;

          transform: translateY(-1px);

          box-shadow:
            0 6px 15px rgba(23,107,135,0.32) !important;
        }

        .btn-success {
          background: #1d8a70 !important;
          color: white !important;
        }

        .btn-success:hover {
          background: #16725c !important;
          color: white !important;
        }

        .btn-default {
          background: #eef3f8 !important;
          color: #334155 !important;
          border: 1px solid #d6dee8 !important;
        }

        .btn-default:hover {
          background: #e2eaf2 !important;
        }

        #score_one,
        #score_batch,
        #run_monitor {
          width: 100%;
          font-size: 15px !important;
          margin-top: 14px;
          margin-bottom: 8px;
        }

        #dl_template,
        #dl_scored {
          font-weight: 700 !important;
          min-width: 210px;
        }

        /* ================================================================
           RESULT CARD
           ================================================================ */

        .result-box {
          border-radius: 16px;
          padding: 30px 25px;
          margin-top: 8px;
          text-align: center;

          box-shadow:
            0 7px 20px rgba(20,40,70,0.08);
        }

        .flag-yes {
          background:
            linear-gradient(
              135deg,
              #fff4f3,
              #fffafa
            );

          border: 2px solid #dc6b64;
        }

        .flag-no {
          background:
            linear-gradient(
              135deg,
              #f1faf5,
              #fbfffd
            );

          border: 2px solid #50a77e;
        }

        .big {
          font-size: 58px;
          line-height: 1;
          font-weight: 800;
          margin: 5px 0 12px 0;
          color: #12355b;
        }

        .result-box strong {
          font-size: 19px;
        }

        /* ================================================================
           METRIC CARDS
           ================================================================ */

        .metric-card {
          background: white;
          border: 1px solid #e2e8f0;
          border-radius: 12px;
          padding: 18px;
          text-align: center;
          margin-bottom: 15px;

          box-shadow:
            0 3px 12px rgba(20,40,70,0.04);
        }

        .metric-number {
          font-size: 28px;
          font-weight: 800;
          color: #176b87;
        }

        .metric-label {
          font-size: 12px;
          color: #64748b;
          margin-top: 4px;
        }

        /* ================================================================
           ALERTS
           ================================================================ */

        .alert {
          border-radius: 10px;
          border-width: 1px;
        }

        .alert-success {
          background: #f0faf5;
          border-color: #b7e1cc;
          color: #17633f;
        }

        .alert-warning {
          background: #fff9e8;
          border-color: #f3d58b;
          color: #805d00;
        }

        .alert-danger {
          background: #fff2f2;
          border-color: #efb2b2;
          color: #8f2525;
        }

        /* ================================================================
           TABLES
           ================================================================ */

        table.dataTable thead th {
          background: #f1f5f9 !important;
          color: #334155 !important;
          font-weight: 700 !important;
          border-bottom: 1px solid #dbe3ec !important;
        }

        table.dataTable tbody td {
          vertical-align: middle !important;
        }

        /* ================================================================
           DETAILS
           ================================================================ */

        details {
          background: #f8fafc;
          border: 1px solid #e2e8f0;
          border-radius: 9px;
          padding: 10px 13px;
          margin: 15px 0;
        }

        summary {
          cursor: pointer;
          font-weight: 700;
          color: #176b87;
        }

        /* ================================================================
           SMALL TEXT
           ================================================================ */

        .small-note {
          color: #64748b;
          font-size: 12px;
          line-height: 1.5;
        }

        .section-note {
          color: #64748b;
          font-size: 13px;
          margin-bottom: 18px;
        }

        /* ================================================================
           DOWNLOAD AREA
           ================================================================ */

        .download-area {
          background: #f7fafc;
          border: 1px solid #e2e8f0;
          border-radius: 11px;
          padding: 18px;
          margin: 18px 0;
        }

        /* ================================================================
           MONITORING INPUT SPACING
           ================================================================ */

        #mon_file {
          margin-bottom: 18px;
        }

        #mon_window {
          margin-bottom: 18px;
        }

        #run_monitor {
          margin-top: 8px;
        }

        /* ================================================================
           MOBILE
           ================================================================ */

        @media (max-width: 768px) {

          .container-fluid {
            padding-left: 15px;
            padding-right: 15px;
          }

          .app-header {
            margin-left: -15px;
            margin-right: -15px;
            padding: 25px 20px;
          }

          .app-title {
            font-size: 25px;
          }

          .big {
            font-size: 48px;
          }

          .nav-tabs > li > a {
            padding: 10px 12px;
            font-size: 12px;
          }

          .btn {
            width: 100%;
          }

          .btn-file {
            width: auto;
          }

          .form-group {
            margin-bottom: 20px;
          }
        }

        "
      )
    )
  ),
  
  # ===========================================================================
  # HEADER
  # ===========================================================================
  
  div(
    class = "app-header",
    
    div(
      class = "app-title",
      "SDHSA Unemployment Risk Explorer"
    ),
    
    div(
      class = "app-subtitle",
      "Unemployment risk decision-support dashboard • South Africa • QLFS 2024"
    ),
    
    div(
      class = "app-badge",
      "RANDOM FOREST • 20% OPERATING THRESHOLD"
    )
  ),
  
  # ===========================================================================
  # RESPONSIBLE USE
  # ===========================================================================
  
  div(
    class = "decision-banner",
    
    div(
      class = "decision-banner-title",
      "Decision-support tool"
    ),
    
    p(
      class = "decision-banner-text",
      "The model estimates how closely a record resembles patterns associated with unemployment in the 2024 QLFS data. ",
      "The result indicates statistical association, not causation, and should not be used on its own to determine programme eligibility or exclude an individual from support. ",
      "Results should be reviewed by a qualified human analyst."
    )
  ),
  
  # ===========================================================================
  # TABS
  # ===========================================================================
  
  tabsetPanel(
    
    # =======================================================================
    # SCORE ONE
    # =======================================================================
    
    tabPanel(
      "Score one person",
      
      br(),
      
      sidebarLayout(
        
        sidebarPanel(
          width = 4,
          
          div(
            class = "card",
            
            div(
              class = "card-title",
              "Person details"
            ),
            
            div(
              class = "card-subtitle",
              "Enter the available characteristics below."
            ),
            
            lapply(
              key_cols,
              make_field
            ),
            
            if (length(adv_cols)) {
              
              tags$details(
                
                tags$summary(
                  "More details (optional)"
                ),
                
                p(
                  class = "small-note",
                  "Optional fields default to typical values when left unchanged."
                ),
                
                lapply(
                  adv_cols,
                  make_field
                )
              )
            },
            
            actionButton(
              "score_one",
              "Calculate unemployment risk",
              class = "btn-primary",
              icon = icon("chart-line")
            ),
            
            p(
              class = "small-note",
              style = "margin-top:12px;",
              "Youth status is calculated automatically from Age."
            )
          )
        ),
        
        mainPanel(
          width = 8,
          
          uiOutput("single_msgs"),
          
          uiOutput("single_result"),
          
          br(),
          
          div(
            class = "card",
            
            div(
              class = "card-title",
              "Risk position"
            ),
            
            div(
              class = "card-subtitle",
              "The marker shows the model score relative to the selected operating threshold."
            ),
            
            plotOutput(
              "single_plot",
              height = "150px"
            )
          )
        )
      )
    ),
    
    # =======================================================================
    # SCORE FILE
    # =======================================================================
    
    tabPanel(
      "Score a file",
      
      br(),
      
      div(
        class = "card",
        
        div(
          class = "card-title",
          "1. Download the template"
        ),
        
        p(
          class = "section-note",
          "Use the template to prepare a CSV for scoring. The first five key fields are required; other fields are optional."
        ),
        
        p(
          code(
            paste(
              ref$cols,
              collapse = ", "
            )
          )
        ),
        
        div(
          class = "download-area",
          
          downloadButton(
            "dl_template",
            "Download CSV template",
            class = "btn-default",
            icon = icon("download")
          )
        )
      ),
      
      div(
        class = "card",
        
        div(
          class = "card-title",
          "2. Upload and score"
        ),
        
        p(
          class = "section-note",
          "Large CSV files are supported. The application validates the uploaded records before scoring them."
        ),
        
        fileInput(
          "batch_file",
          "Choose CSV file",
          accept = ".csv",
          width = "100%"
        ),
        
        actionButton(
          "score_batch",
          "Validate and score file",
          class = "btn-primary",
          icon = icon("calculator")
        ),
        
        uiOutput("batch_msgs")
      ),
      
      conditionalPanel(
        "output.batch_done",
        
        div(
          class = "card",
          
          div(
            class = "card-title",
            "Scoring results"
          ),
          
          fluidRow(
            
            column(
              4,
              uiOutput("batch_summary")
            ),
            
            column(
              8,
              plotOutput(
                "batch_hist",
                height = "280px"
              )
            )
          ),
          
          div(
            class = "download-area",
            
            downloadButton(
              "dl_scored",
              "Download scored CSV",
              class = "btn-success",
              icon = icon("file-download")
            )
          ),
          
          DTOutput(
            "batch_table"
          )
        )
      )
    ),
    
    # =======================================================================
    # MONITORING
    # =======================================================================
    
    tabPanel(
      "Monitoring",
      
      br(),
      
      div(
        class = "card",
        
        div(
          class = "card-title",
          "Model monitoring"
        ),
        
        p(
          class = "section-note",
          "Compare a new batch with the deployment baseline and identify potential drift or performance problems."
        ),
        
        fluidRow(
          
          column(
            4,
            
            fileInput(
              "mon_file",
              "Batch CSV",
              accept = ".csv",
              width = "100%"
            )
          ),
          
          column(
            3,
            
            textInput(
              "mon_window",
              "Monitoring window",
              value = format(Sys.Date(), "%Y-%m"),
              width = "100%"
            )
          ),
          
          column(
            3,
            
            br(),
            
            actionButton(
              "run_monitor",
              "Run monitoring checks",
              class = "btn-primary",
              icon = icon("heartbeat")
            )
          )
        ),
        
        uiOutput("mon_msgs")
      ),
      
      div(
        class = "card",
        
        div(
          class = "card-title",
          "Monitoring results"
        ),
        
        DTOutput(
          "mon_table"
        ),
        
        br(),
        
        plotOutput(
          "mon_plot",
          height = "280px"
        )
      ),
      
      div(
        class = "card",
        
        div(
          class = "card-title",
          "Monitoring history"
        ),
        
        p(
          class = "section-note",
          "Previous monitoring runs recorded by the application."
        ),
        
        DTOutput(
          "mon_log"
        )
      )
    ),
    
    # =======================================================================
    # ABOUT MODEL
    # =======================================================================
    
    tabPanel(
      "About the model",
      
      br(),
      
      fluidRow(
        
        column(
          8,
          
          div(
            class = "card",
            
            div(
              class = "card-title",
              "What the model does"
            ),
            
            p(
              "A random forest model is used to estimate the probability that a record resembles people who were unemployed in the 2024 QLFS data."
            ),
            
            p(
              "The model uses demographic, geographic and education-related variables. Variables that could introduce target leakage were deliberately excluded."
            )
          ),
          
          div(
            class = "card",
            
            div(
              class = "card-title",
              "How the score becomes a flag"
            ),
            
            p(
              sprintf(
                "The operating threshold is %.0f%%. Records at or above this probability are flagged for review.",
                100 * THR
              )
            ),
            
            p(
              "The threshold is intentionally lower than the usual 50% classification threshold because the business objective prioritises identifying potentially unemployed individuals."
            ),
            
            div(
              class = "alert alert-warning",
              
              strong("Important: "),
              
              "A flag is not a finding of unemployment. At this operating point, false positives are expected and human review is required."
            )
          ),
          
          if (!is.null(final_row)) {
            
            div(
              class = "card",
              
              div(
                class = "card-title",
                "Held-out test-set performance"
              ),
              
              tableOutput(
                "final_metrics"
              )
            )
          },
          
          div(
            class = "card",
            
            div(
              class = "card-title",
              "Limitations and responsible use"
            ),
            
            tags$ul(
              
              tags$li(
                "Predictions represent statistical association, not causation."
              ),
              
              tags$li(
                "Performance differs between demographic and geographic groups."
              ),
              
              tags$li(
                "The model was trained on 2024 QLFS data and may lose accuracy as labour-market conditions change."
              ),
              
              tags$li(
                "The model should support, not replace, human judgement."
              ),
              
              tags$li(
                "The model should not be used as the sole basis for benefit eligibility, exclusion, or punitive action."
              )
            )
          )
        ),
        
        column(
          4,
          
          div(
            class = "card",
            
            div(
              class = "card-title",
              "Subgroup performance"
            ),
            
            p(
              class = "section-note",
              "Recall on the held-out test set."
            ),
            
            selectInput(
              "fair_group",
              "Group dimension",
              choices = c(
                "population_group",
                "Province",
                "gender",
                "geo_type",
                "youth_flag",
                "quarter_label"
              ),
              width = "100%"
            ),
            
            DTOutput(
              "fair_table"
            )
          ),
          
          div(
            class = "card",
            
            div(
              class = "card-title",
              "Deployment safeguards"
            ),
            
            tags$ul(
              
              tags$li(
                "Human review before action"
              ),
              
              tags$li(
                "Fairness monitoring"
              ),
              
              tags$li(
                "Prediction-distribution drift monitoring"
              ),
              
              tags$li(
                "Performance monitoring when labels become available"
              ),
              
              tags$li(
                "Retraining when monitoring thresholds are breached"
              )
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # ===========================================================================
  # SCORE ONE PERSON
  # ===========================================================================
  
  one <- eventReactive(
    input$score_one,
    {
      
      vals <- lapply(
        c(key_cols, adv_cols),
        function(cn) input[[paste0("f_", cn)]]
      )
      
      names(vals) <- c(
        key_cols,
        adv_cols
      )
      
      df <- as.data.frame(
        vals,
        stringsAsFactors = FALSE
      )
      
      prep <- prepare_input(
        df,
        ref,
        max_rows = Inf
      )
      
      if (!prep$ok) {
        
        return(
          list(
            prep = prep,
            score = NULL
          )
        )
      }
      
      list(
        prep = prep,
        score = score_data(
          prep$data,
          art
        )
      )
    }
  )
  
  output$single_msgs <- renderUI({
    
    r <- one()
    
    req(r)
    
    tagList(
      
      lapply(
        r$prep$errors,
        function(m)
          div(
            class = "alert alert-danger",
            m
          )
      ),
      
      lapply(
        r$prep$warnings,
        function(m)
          div(
            class = "alert alert-warning",
            m
          )
      )
    )
  })
  
  output$single_result <- renderUI({
    
    r <- one()
    
    req(r$score)
    
    p <- r$score$prob_unemployed[1]
    
    fl <- r$score$flagged[1]
    
    div(
      
      class = paste(
        "result-box",
        if (fl) "flag-yes" else "flag-no"
      ),
      
      p(
        class = "small-note",
        "MODEL-ESTIMATED PROBABILITY"
      ),
      
      p(
        class = "big",
        sprintf(
          "%.0f%%",
          100 * p
        )
      ),
      
      p(
        strong(
          if (fl)
            "Flagged for review"
          else
            "Below review threshold"
        )
      ),
      
      p(
        class = "small-note",
        sprintf(
          "Risk band: %s",
          r$score$risk_band[1]
        )
      ),
      
      p(
        class = "small-note",
        sprintf(
          "Operating threshold: %.0f%%",
          100 * THR
        )
      ),
      
      p(
        class = "small-note",
        "This score indicates resemblance to patterns in the training data. It is not a determination of employment status."
      )
    )
  })
  
  output$single_plot <- renderPlot({
    
    r <- one()
    
    req(r$score)
    
    ggplot(
      data.frame(
        p = r$score$prob_unemployed[1]
      ),
      aes(
        x = p,
        y = 1
      )
    ) +
      
      geom_vline(
        xintercept = THR,
        linetype = "dashed",
        colour = "#dc6b64",
        linewidth = 1
      ) +
      
      geom_point(
        size = 6,
        colour = "#176b87"
      ) +
      
      annotate(
        "text",
        x = THR,
        y = 1.18,
        label = sprintf(
          "Review threshold: %.0f%%",
          THR * 100
        ),
        colour = "#b3423b",
        hjust = -0.05,
        size = 4,
        fontface = "bold"
      ) +
      
      scale_x_continuous(
        limits = c(0, 1),
        labels = scales::percent
      ) +
      
      scale_y_continuous(
        limits = c(0.85, 1.3)
      ) +
      
      labs(
        x = "Estimated probability",
        y = NULL
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 20, 10, 20)
      )
  })
  
  # ===========================================================================
  # BATCH SCORING
  # ===========================================================================
  
  output$dl_template <- downloadHandler(
    
    filename = function() {
      "sdhsa_scoring_template.csv"
    },
    
    content = function(file) {
      
      write.csv(
        make_template(ref),
        file,
        row.names = FALSE
      )
    }
  )
  
  batch <- eventReactive(
    input$score_batch,
    {
      
      req(input$batch_file)
      
      
      raw <- tryCatch(
        {
          # Read the CSV header first
          csv_header <- names(read.csv(
            input$batch_file$datapath,
            nrows = 0,
            check.names = FALSE
          ))
          
          # Preserve HouseholdID as text only when it exists
          csv_classes <- rep(NA, length(csv_header))
          names(csv_classes) <- csv_header
          
          if ("HouseholdID" %in% csv_header) {
            csv_classes["HouseholdID"] <- "character"
          }
          
          read.csv(
            input$batch_file$datapath,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            colClasses = csv_classes
          )
        },
        error = function(e) NULL
      )
      
      if (is.null(raw)) {
        
        return(
          list(
            prep = list(
              ok = FALSE,
              errors = "That file could not be read as a CSV.",
              warnings = character()
            ),
            out = NULL
          )
        )
      }
      
      prep <- prepare_input(
        raw,
        ref,
        max_rows = Inf
      )
      
      if (!prep$ok) {
        
        return(
          list(
            prep = prep,
            out = NULL
          )
        )
      }
      
      sc <- withProgress(
        message = "Scoring records...",
        value = 0,
        {
          
          incProgress(
            0.2,
            detail = "Preparing records..."
          )
          
          incProgress(
            0.4,
            detail = "Applying model..."
          )
          
          result <- score_data(
            prep$data,
            art
          )
          
          incProgress(
            1,
            detail = "Finished"
          )
          
          result
        }
      )
      
      ids <- raw[
        ,
        intersect(
          c(
            "HouseholdID",
            "PersonNumber",
            "ID",
            "id"
          ),
          names(raw)
        ),
        drop = FALSE
      ]
      
      out <- dplyr::bind_cols(
        ids,
        prep$data,
        sc
      )
      
      list(
        prep = prep,
        out = out,
        n_in = nrow(raw)
      )
    }
  )
  
  output$batch_done <- reactive({
    
    b <- batch()
    
    !is.null(b$out)
    
  })
  
  outputOptions(
    output,
    "batch_done",
    suspendWhenHidden = FALSE
  )
  
  output$batch_msgs <- renderUI({
    
    b <- batch()
    
    req(b)
    
    tagList(
      
      lapply(
        b$prep$errors,
        function(m)
          div(
            class = "alert alert-danger",
            m
          )
      ),
      
      lapply(
        b$prep$warnings,
        function(m)
          div(
            class = "alert alert-warning",
            m
          )
      ),
      
      if (!is.null(b$out))
        div(
          class = "alert alert-success",
          
          strong("Scoring complete. "),
          
          sprintf(
            "Validated and scored %s rows. Rows in = rows out.",
            format(
              nrow(b$out),
              big.mark = ","
            )
          )
        )
    )
  })
  
  output$batch_summary <- renderUI({
    
    b <- batch()
    
    req(b$out)
    
    n <- nrow(b$out)
    
    k <- sum(
      b$out$flagged,
      na.rm = TRUE
    )
    
    tagList(
      
      div(
        class = "metric-card",
        
        div(
          class = "metric-number",
          format(
            n,
            big.mark = ","
          )
        ),
        
        div(
          class = "metric-label",
          "Records scored"
        )
      ),
      
      div(
        class = "metric-card",
        
        div(
          class = "metric-number",
          format(
            k,
            big.mark = ","
          )
        ),
        
        div(
          class = "metric-label",
          "Records flagged for review"
        )
      ),
      
      p(
        class = "small-note",
        
        sprintf(
          "%.1f%% of records are at or above the %.0f%% operating threshold.",
          100 * k / n,
          100 * THR
        )
      ),
      
      p(
        class = "small-note",
        
        sprintf(
          "Held-out baseline flag rate: %.1f%%.",
          100 * baseline$flag_rate
        )
      )
    )
  })
  
  output$batch_hist <- renderPlot({
    
    b <- batch()
    
    req(b$out)
    
    ggplot(
      b$out,
      aes(
        prob_unemployed
      )
    ) +
      
      geom_histogram(
        bins = 30,
        fill = "#176b87",
        colour = "white",
        alpha = 0.92
      ) +
      
      geom_vline(
        xintercept = THR,
        linetype = "dashed",
        colour = "#dc6b64",
        linewidth = 1
      ) +
      
      labs(
        x = "Estimated probability",
        y = "Records",
        title = "Distribution of model scores"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        plot.title = element_text(
          face = "bold"
        )
      )
  })
  
  output$batch_table <- renderDT({
    
    b <- batch()
    
    req(b$out)
    
    datatable(
      
      b$out,
      
      rownames = FALSE,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  output$dl_scored <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "sdhsa_scored_",
        format(
          Sys.Date(),
          "%Y%m%d"
        ),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write.csv(
        batch()$out,
        file,
        row.names = FALSE
      )
    }
  )
  
  # ===========================================================================
  # MONITORING
  # ===========================================================================
  
  mon <- eventReactive(
    input$run_monitor,
    {
      
      req(input$mon_file)
      
      raw <- tryCatch(
        
        read.csv(
          input$mon_file$datapath,
          stringsAsFactors = FALSE
        ),
        
        error = function(e)
          NULL
      )
      
      if (is.null(raw)) {
        
        return(
          list(
            msg = "That file could not be read as a CSV."
          )
        )
      }
      
      prep <- prepare_input(
        raw,
        ref,
        max_rows = Inf
      )
      
      if (!prep$ok) {
        
        return(
          list(
            msg = prep$errors,
            warn = prep$warnings
          )
        )
      }
      
      sc <- score_data(
        prep$data,
        art
      )
      
      drift <- check_drift(
        baseline$features,
        prep$data,
        baseline$scores,
        sc$prob_unemployed,
        ref
      )
      
      rec <- NA_real_
      
      res <- drift
      
      if ("Unemployed" %in% names(raw)) {
        
        truth <- parse_truth(
          raw$Unemployed
        )
        
        if (sum(!is.na(truth)) > 0) {
          
          grp <- if (
            "population_group" %in%
            names(prep$data)
          ) {
            prep$data$population_group
          } else {
            NULL
          }
          
          perf <- check_performance(
            truth,
            sc$flagged,
            grp,
            base_gap,
            read_log(app_dir)
          )
          
          res <- rbind(
            perf$table,
            drift
          )
          
          rec <- perf$recall
        }
      }
      
      n_action <- sum(
        res$Status == "ACTION",
        na.rm = TRUE
      )
      
      psi_val <- psi(
        baseline$scores,
        sc$prob_unemployed
      )
      
      append_log(
        app_dir,
        input$mon_window,
        nrow(prep$data),
        psi_val,
        n_action,
        rec
      )
      
      list(
        table = res,
        scores = sc$prob_unemployed,
        warn = prep$warnings,
        n = nrow(prep$data),
        n_action = n_action
      )
    }
  )
  
  output$mon_msgs <- renderUI({
    
    m <- mon()
    
    req(m)
    
    if (!is.null(m$msg)) {
      
      return(
        lapply(
          m$msg,
          function(x)
            div(
              class = "alert alert-danger",
              x
            )
        )
      )
    }
    
    tagList(
      
      lapply(
        m$warn,
        function(x)
          div(
            class = "alert alert-warning",
            x
          )
      ),
      
      if (
        m$n < MIN_ROWS_DRIFT
      )
        
        div(
          class = "alert alert-warning",
          
          sprintf(
            "Only %d rows were supplied. Drift tests on small batches are unreliable; interpret results cautiously.",
            m$n
          )
        ),
      
      if (
        m$n_action > 0
      )
        
        div(
          class = "alert alert-danger",
          
          strong(
            sprintf(
              "%d monitoring measure(s) need action.",
              m$n_action
            )
          ),
          
          " Follow the 'If breached' guidance and notify the model owners."
        )
      
      else
        
        div(
          class = "alert alert-success",
          
          strong("Monitoring complete. "),
          
          "No monitoring thresholds were breached in this batch."
        )
    )
  })
  
  output$mon_table <- renderDT({
    
    m <- mon()
    cat(
      "\n--- MONITORING TABLE DIAGNOSTIC ---\n",
      "mon is NULL: ", is.null(m), "\n",
      "table exists: ", !is.null(m$table), "\n",
      "table is data.frame: ", is.data.frame(m$table), "\n",
      "table rows: ",
      if (is.data.frame(m$table)) nrow(m$table) else NA,
      "\n",
      sep = ""
    )
    # Always render a useful table state. Earlier versions used req(m$table),
    # which silently hid the whole table when input validation returned early.
    if (is.null(m)) {
      return(datatable(
        data.frame(
          Measure = "Monitoring not run",
          Detail = "Upload a CSV and click Run monitoring checks.",
          Value = NA_character_,
          Rule = NA_character_,
          Status = "n/a",
          If_breached = NA_character_,
          check.names = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE)
      ))
    }
    
    if (!is.null(m$msg)) {
      error_text <- paste(as.character(m$msg), collapse = " ")
      display_table <- data.frame(
        Measure = "Monitoring could not complete",
        Detail = error_text,
        Value = NA_character_,
        Rule = NA_character_,
        Status = "ACTION",
        If_breached = "Correct the upload/validation issue, then run checks again.",
        check.names = FALSE
      )
    } else if (is.data.frame(m$table) && nrow(m$table) > 0) {
      display_table <- m$table
    } else {
      display_table <- data.frame(
        Measure = "No monitoring checks returned",
        Detail = "The monitoring function returned no rows. Check the R console for the underlying error.",
        Value = NA_character_,
        Rule = NA_character_,
        Status = "ACTION",
        If_breached = "Review check_drift() and check_performance() results.",
        check.names = FALSE
      )
    }
    
    DT::datatable(
      display_table,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  output$mon_plot <- renderPlot({
    
    m <- mon()
    
    req(m$scores)
    
    d <- rbind(
      data.frame(
        p = baseline$scores,
        set = "Baseline"
      ),
      data.frame(
        p = m$scores,
        set = "New batch"
      )
    )
    
    # Remove missing or non-finite probabilities before plotting.
    d <- d[is.finite(d$p) & !is.na(d$set), , drop = FALSE]
    d$set <- factor(d$set, levels = c("Baseline", "New batch"))
    
    # A density curve needs at least two distinct valid values per group.
    density_groups <- split(d, d$set, drop = TRUE)
    density_groups <- lapply(density_groups, function(group_data) {
      if (
        nrow(group_data) >= 2 &&
        length(unique(group_data$p)) >= 2
      ) {
        group_data
      } else {
        NULL
      }
    })
    density_groups <- Filter(Negate(is.null), density_groups)
    density_data <- if (length(density_groups) > 0) {
      do.call(rbind, density_groups)
    } else {
      NULL
    }
    
    p <- ggplot(
      d,
      aes(
        x = p,
        fill = set
      )
    )
    
    if (!is.null(density_data) && nrow(density_data) > 0) {
      p <- p + geom_density(
        data = density_data,
        alpha = 0.42,
        na.rm = TRUE
      )
    }
    
    p +
      geom_vline(
        xintercept = THR,
        linetype = "dashed",
        linewidth = 1
      ) +
      scale_fill_manual(
        values = c(
          "Baseline" = "#64748b",
          "New batch" = "#176b87"
        ),
        drop = FALSE
      ) +
      labs(
        x = "Estimated probability",
        y = "Density",
        fill = NULL,
        title = "Score distribution: baseline vs new batch"
      ) +
      theme_minimal(
        base_size = 13
      ) +
      theme(
        plot.title = element_text(
          face = "bold"
        )
      )
  })
  
  output$mon_log <- renderDT({
    
    input$run_monitor
    
    lg <- read_log(
      app_dir
    )
    
    datatable(
      
      if (is.null(lg))
        data.frame(
          note = "No monitoring runs recorded yet."
        )
      else
        lg,
      
      rownames = FALSE,
      
      options = list(
        pageLength = 5,
        dom = "tp",
        scrollX = TRUE
      )
    )
  })
  
  # ===========================================================================
  # ABOUT MODEL
  # ===========================================================================
  
  output$final_metrics <- renderTable({
    
    req(!is.null(final_row))
    
    keep <- intersect(
      
      c(
        "Accuracy",
        "Precision",
        "Recall",
        "F1",
        "Specificity",
        "Balanced_Acc",
        "ROC_AUC"
      ),
      
      names(final_row)
    )
    
    data.frame(
      Metric = keep,
      Value = unlist(
        final_row[keep]
      )
    )
    
  }, digits = 3)
  
  output$fair_table <- renderDT({
    
    g <- input$fair_group
    
    f <- file.path(
      
      art$root,
      "outputs",
      paste0(
        "fairness_final_",
        g,
        if (
          g == "Province"
        )
          "_clean"
        else
          "",
        ".csv"
      )
    )
    
    if (!file.exists(f)) {
      
      f <- file.path(
        
        art$root,
        "outputs",
        paste0(
          "fairness_final_",
          g,
          ".csv"
        )
      )
    }
    
    d <- if (file.exists(f)) {
      
      read.csv(
        f
      )
      
    } else {
      
      data.frame(
        note = "Fairness file not found."
      )
    }
    
    datatable(
      
      d,
      
      rownames = FALSE,
      
      options = list(
        pageLength = 12,
        dom = "t",
        scrollX = TRUE
      )
    )
  })
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(
  ui,
  server
)
