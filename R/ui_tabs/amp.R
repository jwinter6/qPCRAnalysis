tab_amp <- bslib::nav_panel(
  "Amplifikationskurven",
  value = "amp",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Amplifikationskurven"),
        bslib::card_body(
          plotlyOutput("qpcr_curve_plot", height = "600px"),
          br(),
          actionButton("add_report_amp_plot", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_amp_plot_png", "Download Amplifikationskurven (PNG)")
        )
      )
    )
  )
)
