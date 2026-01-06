tab_ctsd <- bslib::nav_panel(
  "Ct SD",
  value = "ctsd",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Ct SD vs Quantity"),
        bslib::card_body(
          plotlyOutput("qpcr_sd_plot", height = "500px"),
          br(),
          downloadButton("download_ctsd_plot_png", "Download Ct SD Plot (PNG)")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Ct SD Heatmap (Sample x Target)"),
        bslib::card_body(
          plotlyOutput("qpcr_sd_heatmap", height = "500px"),
          br(),
          downloadButton("download_ctsd_heatmap_png", "Download Ct SD Heatmap (PNG)"),
          downloadButton("download_ctsd_heatmap_xlsx", "Download Ct SD Heatmap Tabelle (XLSX)")
        )
      )
    )
  )
)
