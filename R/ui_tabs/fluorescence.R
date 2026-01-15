tab_fluorescence <- bslib::nav_panel(
  "Fluoreszenz",
  value = "fluorescence",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Maximale Fluoreszenz (pro Sample)"),
        bslib::card_body(
          plotlyOutput("fluorescence_max_plot", height = "600px"),
          br(),
          actionButton("add_report_fluor_max_plot", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_fluor_max_plot_png", "Download Plot (PNG)")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Delta Fluoreszenz (Max - Min)"),
        bslib::card_body(
          plotlyOutput("fluorescence_delta_plot", height = "600px"),
          br(),
          actionButton("add_report_fluor_delta_plot", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_fluor_delta_plot_png", "Download Plot (PNG)")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Fluoreszenz-Tabelle"),
        bslib::card_body(
          DTOutput("fluorescence_table"),
          br(),
          actionButton("add_report_fluor_table", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_fluor_table_xlsx", "Download Tabelle (XLSX)")
        )
      )
    )
  )
)
