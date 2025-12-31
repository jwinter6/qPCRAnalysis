tab_load <- tabItem(
  tabName = "load",
  fluidRow(
    box(
      width = 6,
      title = "Daten laden (.xlsx)",
      status = "primary",
      solidHeader = TRUE,
      fileInput(
        "xlsx_files",
        "Wähle eine oder mehrere .xlsx-Dateien",
        multiple = TRUE,
        accept = c(".xlsx")
      ),
      actionButton("load_btn", "Daten laden", icon = icon("play")),
      br(), br(),
      tags$div(
        style = "background-color:#f8f9fa; border-radius:8px; padding:10px; border:1px solid #ddd;",
        tags$strong("Hinweise zum Dateiupload:"),
        tags$ul(
          tags$li("Unterstützte Geräteformate: ", tags$code("QuantStudio-ähnlich"), " und ", tags$code("AriaMX Export"), "."),
          tags$li("Für ", tags$code("QuantStudio"), " muss mindestens ein Sheet ", tags$code("Results"), " existieren (optional ", tags$code("Amplification Data"), " und ", tags$code("Melt Curve Raw Data"), ")."),
          tags$li("Für ", tags$code("AriaMX"), " muss ein Sheet ", tags$code("Tabular Results"), " vorhanden sein; das Amplifikations-Sheet wird automatisch erkannt."),
          tags$li("Alle Dateien müssen im Excel-Format ", tags$code(".xlsx"), " vorliegen."),
          tags$li("Nach dem Laden kannst du unten auswählen, welche Dateien in die Analyse einfließen sollen."),
          tags$li("Wenn eine Datei nicht eingelesen werden kann, erscheint oben rechts eine Fehlermeldung mit Dateinamen und Ursache.")
        )
      ),
      br(),
      verbatimTextOutput("load_status")
    ),
    box(
      width = 6,
      title = "Lade-Status",
      status = "info",
      solidHeader = TRUE,
      uiOutput("load_info")
    )
  ),
  fluidRow(
    box(
      width = 6,
      title = "Dateien für Analyse auswählen",
      status = "warning",
      solidHeader = TRUE,
      uiOutput("file_selection_ui")
    ),
    box(
      width = 6,
      title = "Übersicht je Datei (Targets / Samples / Quantities)",
      status = "info",
      solidHeader = TRUE,
      DTOutput("file_overview_table")
    )
  )
)
