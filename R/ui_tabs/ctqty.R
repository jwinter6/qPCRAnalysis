tab_ctqty <- bslib::nav_panel(
  "Ct vs Quantity",
  value = "ctqty",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Ct (Mean +/- SD) vs Quantity"),
        bslib::card_body(
          uiOutput("quantity_notice_ctqty"),
          plotlyOutput("qpcr_plot", height = "600px"),
          br(),
          actionButton("add_report_ctqty_plot", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_ct_plot_png", "Download Ct-Plot (PNG)")
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
          DTOutput("ct_table"),
          br(),
          actionButton("add_report_ctqty_table", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_ct_table_xlsx", "Download Ct-Tabelle (XLSX)")
        )
      )
    )
  )
)
