tab_cluster <- bslib::nav_panel(
  "Cluster",
  value = "cluster",
  tags$style(HTML("
    #cluster_controls .form-group { margin-bottom: 0.55rem; }
    #cluster_controls .control-label { margin-bottom: 0.2rem; font-size: 0.92rem; }
    #cluster_controls hr { margin: 0.6rem 0; }
    #cluster_controls .form-control { padding-top: 0.25rem; padding-bottom: 0.25rem; }
    #cluster_controls .selectize-input { min-height: 32px; padding-top: 4px; padding-bottom: 4px; }
    #cluster_controls .selectize-input > input { line-height: 1.1; }
    #cluster_filter_card,
    #cluster_filter_card .card-body {
      height: auto !important;
      max-height: none !important;
      overflow: visible !important;
    }
  ")),
  fluidRow(
    column(
      width = 3,
      bslib::card(
        id = "cluster_filter_card",
        fill = FALSE,
        bslib::card_header("Cluster - Einstellungen"),
        bslib::card_body(
          fillable = FALSE,
          fill = FALSE,
          div(
            id = "cluster_controls",
            selectizeInput(
              inputId = "cluster_samples",
              label = "Samples (lokal)",
              choices = NULL,
              multiple = TRUE
            ),
            fluidRow(
              column(
                width = 6,
                selectizeInput(
                  inputId = "cluster_target_x",
                  label = "Target X",
                  choices = NULL,
                  multiple = FALSE
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  inputId = "cluster_target_y",
                  label = "Target Y",
                  choices = NULL,
                  multiple = FALSE
                )
              )
            ),
            selectInput(
              inputId = "cluster_pair_mode",
              label = "Datei/Run-uebergreifend paaren",
              choices = c(
                "Ja (ueber verschiedene Dateien/Runs)" = "across_runs",
                "Nein (nur innerhalb gleicher Datei/Run)" = "within_run"
              ),
              selected = "across_runs"
            ),
            selectInput(
              inputId = "cluster_display_mode",
              label = "Darstellungsmodus",
              choices = c(
                "Aggregiert pro Sample" = "sample_agg",
                "Einzelpunkte pro Well" = "well_points"
              ),
              selected = "sample_agg"
            ),
            tags$hr(),
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = "cluster_x_param",
                  label = "X-Parameter",
                  choices = c(
                    "Fluoreszenz (letzter Cycle)" = "fluor_last",
                    "Fluoreszenz (Maximum)" = "fluor_max"
                  ),
                  selected = "fluor_last"
                )
              ),
              column(
                width = 6,
                selectInput(
                  inputId = "cluster_y_param",
                  label = "Y-Parameter",
                  choices = c(
                    "Fluoreszenz (letzter Cycle)" = "fluor_last",
                    "Fluoreszenz (Maximum)" = "fluor_max"
                  ),
                  selected = "fluor_last"
                )
              )
            ),
            tags$hr(),
            textInput("cluster_title", "Titel", "Cluster"),
            textInput("cluster_subtitle", "Untertitel", ""),
            fluidRow(
              column(
                width = 6,
                textInput("cluster_x_lab", "X-Label", "")
              ),
              column(
                width = 6,
                textInput("cluster_y_lab", "Y-Label", "")
              )
            ),
            selectInput(
              inputId = "cluster_color_param",
              label = "Farbe/Fuellung nach",
              choices = c("Keine" = "none"),
              selected = "none"
            )
          )
        )
      )
    ),
    column(
      width = 9,
      bslib::card(
        bslib::card_header("Cluster Scatterplot"),
        bslib::card_body(
          uiOutput("cluster_cycle_info"),
          tabsetPanel(
            tabPanel("ggplot2", plotOutput("cluster_plot_gg", height = "520px")),
            tabPanel("plotly", plotly::plotlyOutput("cluster_plotly", height = "520px"))
          )
        )
      ),
      br(),
      bslib::card(
        bslib::card_header("Daten (wie im Plot)"),
        bslib::card_body(
          DTOutput("cluster_table")
        )
      )
    )
  )
)
