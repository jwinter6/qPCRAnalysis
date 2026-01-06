tab_help <- bslib::nav_panel(
  "Hilfe",
  value = "help",
  bslib::card(
    bslib::card_header("Hilfe & Dokumentation"),
    bslib::card_body(
      bslib::navset_tab(
        bslib::nav_panel(
          "Ueberblick",
          h3("1. Zweck der App"),
          tags$p(
            "Diese App dient der Auswertung von qPCR-Experimenten aus unterschiedlichen Geraeten (QuantStudio-Format und AriaMX-Exports). ",
            "Sie vereinheitlicht die Datenstrukturen und stellt interaktive Visualisierungen und Kennzahlen fuer Ct-Werte, Standardkurven, ",
            "Effizienz, Schmelzkurven und Outlier-Analysen bereit."
          ),
          h3("2. Navigation & Layout"),
          tags$ul(
            tags$li("Alle Seiten sind in der Navbar oben erreichbar."),
            tags$li("Globale Filter befinden sich links in der Sidebar (Akkordeons)."),
            tags$li("Inhalte sind in Cards gegliedert; einzelne Bereiche nutzen Tabs oder Akkordeons.")
          ),
          h3("3. Typischer Workflow fuer Nutzer"),
          tags$ol(
            tags$li(
              strong("Daten hochladen (Phase A):"),
              " Im Tab ", code("Daten laden"),
              " eine oder mehrere .xlsx-Dateien auswaehlen und auf ", code("Daten laden"), " klicken. ",
              "Die Dateien werden eingelesen, aber noch nicht analysiert."
            ),
            tags$li(
              strong("Datei-Uebersicht pruefen:"),
              " In der Card ", code("Uebersicht je Datei"),
              " wird fuer jede geladene Datei angezeigt, welche Targets, Samples und Quantities enthalten sind."
            ),
            tags$li(
              strong("Dateien fuer Analyse auswaehlen (Phase B):"),
              " In der Card ", code("Dateien fuer Analyse auswaehlen"),
              " festlegen, welche Dateien in die Auswertung einfliessen sollen (Checkboxen) und dann auf ",
              code("Analyse starten"), " klicken."
            ),
            tags$li(
              strong("Globale Filter setzen:"),
              " Links in der Sidebar Targets (", code("Target_ID"), ") und Samples auswaehlen."
            ),
            tags$li(
              strong("Ergebnisse ansehen & exportieren:"),
              " Ueber die Tabs ", code("Ct vs Quantity"), ", ", code("Amplifikationskurven"), ", ",
              code("Ct SD"), ", ", code("Schmelzkurven"), ", ", code("Standardkurven"), " und ",
              code("Outlier Tests"), " navigieren. ",
              "PNG-Plots und XLSX-Tabellen koennen in den jeweiligen Cards heruntergeladen werden."
            )
          )
        ),
        bslib::nav_panel(
          "Technische Details",
          h3("UI-Theming (fresh)"),
          tags$ul(
            tags$li(
              "Die Oberflaeche nutzt ein frisches Theme auf Basis von ",
              code("fresh"), " und Bootstrap 4."
            ),
            tags$li(
              "Karten-Layouts stammen aus ", code("bslib"), " und ersetzen die vorherigen Boxen."
            )
          ),
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
              "Zusaetzliche Anpassungen (z. B. gedrehte x-Achsen-Beschriftung) werden ueber ",
              code("theme(...)"),
              " ergaenzt."
            )
          )
        )
      )
    )
  )
)
