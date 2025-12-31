tab_help <- tabItem(
  tabName = "help",
  fluidRow(
    box(
      width = 12,
      title = "Hilfe & Dokumentation",
      status = "primary",
      solidHeader = TRUE,
      tabBox(
        width = 12,

        tabPanel(
          title = "Überblick",
          h3("1. Zweck der App"),
          tags$p(
            "Diese App dient der Auswertung von qPCR-Experimenten aus unterschiedlichen Geräten (QuantStudio-Format und AriaMX-Exports). ",
            "Sie vereinheitlicht die Datenstrukturen und stellt interaktive Visualisierungen und Kennzahlen für Ct-Werte, Standardkurven, ",
            "Effizienz, Schmelzkurven und Outlier-Analysen bereit."
          ),
          h3("2. Typischer Workflow für Nutzer"),
          tags$ol(
            tags$li(
              strong("Daten hochladen (Phase A):"),
              " Im Tab ", code("Daten laden"),
              " eine oder mehrere .xlsx-Dateien auswählen und auf ", code("Daten laden"), " klicken. ",
              "Die Dateien werden eingelesen, aber noch nicht analysiert."
            ),
            tags$li(
              strong("Datei-Übersicht prüfen:"),
              " In der Box ", code("Übersicht je Datei"),
              " wird für jede geladene Datei angezeigt, welche Targets, Samples und Quantities enthalten sind."
            ),
            tags$li(
              strong("Dateien für Analyse auswählen (Phase B):"),
              " In der Box ", code("Dateien für Analyse auswählen"),
              " festlegen, welche Dateien in die Auswertung einfließen sollen (Checkboxen) und dann auf ",
              code("Analyse starten"), " klicken."
            ),
            tags$li(
              strong("Globale Filter setzen:"),
              " In der Sidebar Targets (", code("Target_ID"), ") und Samples auswählen, die analysiert werden sollen."
            ),
            tags$li(
              strong("Ergebnisse ansehen & exportieren:"),
              " Über die Tabs ", code("Ct vs Quantity"), ", ", code("Amplifikationskurven"), ", ",
              code("Ct SD"), ", ", code("Schmelzkurven"), ", ", code("Standardkurven"), " und ",
              code("Outlier Tests"), " navigieren. ",
              "PNG-Plots und XLSX-Tabellen können in den jeweiligen Tabs heruntergeladen werden."
            )
          )
        ),

        tabPanel(
          title = "Technische Details",
          h3("Plot-Design (ggthemes)"),
          tags$ul(
            tags$li(
              "Alle ggplot2-Grafiken verwenden das Paket ",
              code("ggthemes"),
              " mit ",
              code("theme_gdocs()"),
              " als Basis-Theme."
            ),
            tags$li(
              "Zusätzliche Anpassungen (z. B. gedrehte x-Achsen-Beschriftung) werden über ",
              code("theme(...)"),
              " ergänzt."
            )
          )
        )
      )
    )
  )
)
