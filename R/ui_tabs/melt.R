tab_melt <- bslib::nav_panel(
  "Schmelzkurven",
  value = "melt",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Schmelzkurven"),
        bslib::card_body(
          plotlyOutput("melt_curve_plot", height = "600px"),
          br(),
          actionButton("add_report_melt_plot", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_melt_plot_png", "Download Schmelzkurven (PNG)")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      bslib::card(
        bslib::card_header("Schmelzkurven-Peaks (Tm-Kandidaten)"),
        bslib::card_body(
          DTOutput("melt_peaks_table"),
          br(),
          actionButton("add_report_melt_peaks_table", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_melt_peaks_xlsx", "Download Peak-Tabelle (XLSX)")
        )
      )
    ),
    column(
      width = 6,
      bslib::card(
        bslib::card_header("Zusammenfassung Peaks pro Sample/Target"),
        bslib::card_body(
          DTOutput("melt_peak_summary_table"),
          br(),
          actionButton("add_report_melt_summary_table", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_melt_peaks_summary_xlsx", "Download Peak-Summary (XLSX)")
        )
      )
    )
  )
)
