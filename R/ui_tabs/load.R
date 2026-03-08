tab_load <- bslib::nav_panel(
  "Daten laden",
  value = "load",
  fluidRow(
    column(
      width = 6,
      bslib::card(
        bslib::card_header("Daten laden (.xlsx / .rdml)"),
        bslib::card_body(
          fileInput(
            "xlsx_files",
            "Waehle eine oder mehrere qPCR-Dateien (.xlsx oder .rdml)",
            multiple = TRUE,
            accept = c(".xlsx", ".rdml", ".xml")
          ),
          actionButton("load_btn", "Daten laden", icon = icon("play")),
          br(),
          br(),
          bslib::accordion(
            bslib::accordion_panel(
              "Hinweise zum Dateiupload",
              tags$ul(
                tags$li("Unterstuetzte Formate: ", tags$code("QuantStudio-aehnlich (.xlsx)"), ", ", tags$code("AriaMX Export (.xlsx)"), " und ", tags$code("RDML (.rdml/.xml)"), "."),
                tags$li("Fuer ", tags$code("QuantStudio"), " muss mindestens ein Sheet ", tags$code("Results"), " existieren (optional ", tags$code("Amplification Data"), " und ", tags$code("Melt Curve Raw Data"), ")."),
                tags$li("Fuer ", tags$code("AriaMX"), " muss ein Sheet ", tags$code("Tabular Results"), " vorhanden sein; das Amplifikations-Sheet wird automatisch erkannt."),
                tags$li("Dateien muessen im Format ", tags$code(".xlsx"), ", ", tags$code(".rdml"), " oder ", tags$code(".xml"), " vorliegen."),
                tags$li("Nach dem Laden kannst du unten auswaehlen, welche Dateien in die Analyse einfliessen sollen."),
                tags$li("Wenn eine Datei nicht eingelesen werden kann, erscheint oben rechts eine Fehlermeldung mit Dateinamen und Ursache.")
              )
            )
          ),
          br(),
          verbatimTextOutput("load_status")
        )
      )
    ),
    column(
      width = 6,
      bslib::card(
        bslib::card_header("Lade-Status"),
        bslib::card_body(
          uiOutput("load_info")
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      bslib::card(
        bslib::card_header("Dateien fuer Analyse auswaehlen"),
        bslib::card_body(
          uiOutput("file_selection_ui")
        )
      )
    ),
    column(
      width = 6,
      bslib::card(
        bslib::card_header("Uebersicht je Datei (Targets / Samples / Quantities)"),
        bslib::card_body(
          DTOutput("file_overview_table"),
          br(),
          actionButton("add_report_file_overview_table", "Zum Report hinzufuegen")
        )
      )
    )
  )
)
