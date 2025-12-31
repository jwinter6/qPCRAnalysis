tab_ctqty <- tabItem(
  tabName = "ctqty",
  fluidRow(
    box(
      width = 12,
      title = "Ct (Mean ± SD) vs Quantity",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("qpcr_plot", height = "600px"),
      br(),
      downloadButton("download_ct_plot_png", "Download Ct-Plot (PNG)")
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = "Ct-Tabelle (Mean ± SD)",
      status = "info",
      solidHeader = TRUE,
      DTOutput("ct_table"),
      br(),
      downloadButton("download_ct_table_xlsx", "Download Ct-Tabelle (XLSX)")
    )
  )
)
