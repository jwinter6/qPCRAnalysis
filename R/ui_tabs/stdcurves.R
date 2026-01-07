tab_stdcurves <- bslib::nav_panel(
  "Standardkurven",
  value = "stdcurves",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Standardkurven - Uebersicht (LDR & Effizienz)"),
        bslib::card_body(
          DTOutput("standardcurve_table"),
          br(),
          actionButton("add_report_stdcurve_table", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_stdcurves_xlsx", "Download Standardkurven-Tabelle (XLSX)")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      bslib::card(
        bslib::card_header("Steigungen (Slope) je Sample/Target"),
        bslib::card_body(
          plotlyOutput("stdcurve_slope_plot", height = "500px"),
          br(),
          actionButton("add_report_stdcurve_slope_plot", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_stdcurve_slope_png", "Download Slope-Plot (PNG)")
        )
      )
    ),
    column(
      width = 6,
      bslib::card(
        bslib::card_header("Effizienz (%) je Sample/Target"),
        bslib::card_body(
          plotlyOutput("stdcurve_eff_plot", height = "500px"),
          br(),
          actionButton("add_report_stdcurve_eff_plot", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_stdcurve_eff_png", "Download Effizienz-Plot (PNG)")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Standardkurven Scatterplots (Ct ~ log10(Quantity))"),
        bslib::card_body(
          uiOutput("quantity_notice_stdcurves"),
          selectInput(
            "std_scatter_target",
            "Target (inkl. Kanal) fuer Scatterplot",
            choices = NULL
          ),
          plotlyOutput("stdcurve_scatter_plot", height = "600px"),
          br(),
          actionButton("add_report_stdcurve_scatter_plot", "Zum Report hinzufuegen"),
          br(),
          downloadButton("download_stdcurve_scatter_png", "Download Scatterplot (PNG)")
        )
      )
    )
  )
)
