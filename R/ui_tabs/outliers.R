tab_outliers <- tabItem(
  tabName = "outliers",
  fluidRow(
    box(
      width = 12,
      title = "Outlier Analyse (auf Residuen der Standardkurve, Ct pro Well)",
      status = "danger",
      solidHeader = TRUE,
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
  ),
  fluidRow(
    box(
      width = 12,
      title = "Residuenplot – Ct vs Fit (Outlier-Markierung)",
      status = "warning",
      solidHeader = TRUE,
      plotlyOutput("outlier_residual_plot", height = "600px"),
      br(),
      downloadButton("download_outlier_plot_png", "Download Residuenplot (PNG)")
    )
  )
)
