tab_report <- bslib::nav_panel(
  "Report Export",
  value = "report_export",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Report Export (PDF / HTML / Word)"),
        bslib::card_body(
          h5("Analysedatensatz (XLSX)"),
          tags$p(
            class = "text-muted",
            "Exportiert den aktuell analysierten Master Data Table inklusive Metadaten, aktiven Filtern und Warnhinweisen."
          ),
          uiOutput("analysis_master_export_ui"),
          tags$hr(),
          h5("Report"),
          textInput("report_name", "Report-Name (ohne Dateiendung)", value = ""),
          selectInput(
            "report_format",
            "Format",
            choices = c("PDF" = "pdf", "HTML" = "html", "Word" = "docx"),
            selected = "pdf"
          ),
          uiOutput("report_items_ui"),
          actionButton("report_generate", "Report erstellen", icon = icon("file-export")),
          br(),
          uiOutput("report_download_ui")
        )
      )
    )
  )
)
