tab_outliers <- bslib::nav_panel(
  "Outlier Tests",
  value = "outliers",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Outlier Analyse (auf Residuen der Standardkurve, Ct pro Well)"),
        bslib::card_body(
          fluidRow(
            column(
              width = 4,
              uiOutput("outlier_target_ui")
            ),
            column(
              width = 4,
              uiOutput("outlier_sample_ui")
            )
          ),
          br(),
          uiOutput("outlier_explanation"),
          br(),
          DTOutput("outlier_table"),
          br(),
          downloadButton("download_outlier_table_xlsx", "Download Outlier Tabelle (XLSX)")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Residuenplot - Ct vs Fit (Outlier-Markierung)"),
        bslib::card_body(
          plotlyOutput("outlier_residual_plot", height = "600px"),
          br(),
          downloadButton("download_outlier_plot_png", "Download Residuenplot (PNG)")
        )
      )
    )
  )
)
