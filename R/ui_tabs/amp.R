tab_amp <- tabItem(
  tabName = "amp",
  fluidRow(
    box(
      width = 12,
      title = "Amplifikationskurven",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("qpcr_curve_plot", height = "600px"),
      br(),
      downloadButton("download_amp_plot_png", "Download Amplifikationskurven (PNG)")
    )
  )
)
