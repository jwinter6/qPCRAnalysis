tab_melt <- tabItem(
  tabName = "melt",
  fluidRow(
    box(
      width = 12,
      title = "Schmelzkurven",
      status = "primary",
      solidHeader = TRUE,
      plotlyOutput("melt_curve_plot", height = "600px"),
      br(),
      downloadButton("download_melt_plot_png", "Download Schmelzkurven (PNG)")
    )
  ),
  fluidRow(
    box(
      width = 6,
      title = "Schmelzkurven-Peaks (Tm-Kandidaten)",
      status = "info",
      solidHeader = TRUE,
      DTOutput("melt_peaks_table"),
      br(),
      downloadButton("download_melt_peaks_xlsx", "Download Peak-Tabelle (XLSX)")
    ),
    box(
      width = 6,
      title = "Zusammenfassung Peaks pro Sample/Target",
      status = "info",
      solidHeader = TRUE,
      DTOutput("melt_peak_summary_table"),
      br(),
      downloadButton("download_melt_peaks_summary_xlsx", "Download Peak-Summary (XLSX)")
    )
  )
)
