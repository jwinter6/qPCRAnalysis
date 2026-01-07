tab_ctsample <- bslib::nav_panel(
  "Ct vs Sample",
  value = "ctsample",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Ct (Mean +/- SD) vs Sample"),
        bslib::card_body(
          plotlyOutput("qpcr_plot_sample", height = "600px"),
          br(),
          actionButton("add_report_ctsample_plot", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_ct_plot_sample_png", "Download Ct-Plot (PNG)")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Ct-Tabelle (Mean +/- SD)"),
        bslib::card_body(
          DTOutput("ct_table_sample"),
          br(),
          actionButton("add_report_ctsample_table", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_ct_table_sample_xlsx", "Download Ct-Tabelle (XLSX)")
        )
      )
    )
  )
)
