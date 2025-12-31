tab_stdcurves <- tabItem(
  tabName = "stdcurves",
  fluidRow(
    box(
      width = 12,
      title = "Standardkurven – Übersicht (LDR & Effizienz)",
      status = "primary",
      solidHeader = TRUE,
      DTOutput("standardcurve_table"),
      br(),
      downloadButton("download_stdcurves_xlsx", "Download Standardkurven-Tabelle (XLSX)")
    )
  ),
  fluidRow(
    box(
      width = 6,
      title = "Steigungen (Slope) je Sample/Target",
      status = "info",
      solidHeader = TRUE,
      plotlyOutput("stdcurve_slope_plot", height = "500px"),
      br(),
      downloadButton("download_stdcurve_slope_png", "Download Slope-Plot (PNG)")
    ),
    box(
      width = 6,
      title = "Effizienz (%) je Sample/Target",
      status = "info",
      solidHeader = TRUE,
      plotlyOutput("stdcurve_eff_plot", height = "500px"),
      br(),
      downloadButton("download_stdcurve_eff_png", "Download Effizienz-Plot (PNG)")
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = "Standardkurven Scatterplots (Ct ~ log10(Quantity))",
      status = "primary",
      solidHeader = TRUE,
      selectInput(
        "std_scatter_target",
        "Target (inkl. Kanal) für Scatterplot",
        choices = NULL
      ),
      plotlyOutput("stdcurve_scatter_plot", height = "600px"),
      br(),
      downloadButton("download_stdcurve_scatter_png", "Download Scatterplot (PNG)")
    )
  )
)
