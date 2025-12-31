tab_ctsd <- tabItem(
  tabName = "ctsd",
  fluidRow(
    box(
      width = 12,
      title = "Ct SD vs Quantity",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("qpcr_sd_plot", height = "500px"),
      br(),
      downloadButton("download_ctsd_plot_png", "Download Ct SD Plot (PNG)")
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = "Ct SD Heatmap (Sample × Target)",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("qpcr_sd_heatmap", height = "500px"),
      br(),
      downloadButton("download_ctsd_heatmap_png", "Download Ct SD Heatmap (PNG)"),
      downloadButton("download_ctsd_heatmap_xlsx", "Download Ct SD Heatmap Tabelle (XLSX)")
    )
  )
)
