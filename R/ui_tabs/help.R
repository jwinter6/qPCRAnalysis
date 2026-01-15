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
            tags$li("Inhalte sind in Cards gegliedert; einzelne Bereiche nutzen Tabs oder Akkordeons."),
            tags$li("Tooltips sind ueber das kleine ", tags$code("i"), " neben Schaltern/Inputs verfuegbar.")
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
              " Links in der Sidebar Targets (", code("Target_ID"), "), Samples und den Schalter ",
              code("Dateien getrennt anzeigen"), " setzen."
            ),
            tags$li(
              strong("Plate Overview pruefen:"),
              " Der Tab ", code("Plate Overview"), " zeigt Plattenansichten (96/384) fuer Targets, Samples, Farbstoffe, Quantity und Well Type."
            ),
            tags$li(
              strong("Ergebnisse ansehen & exportieren:"),
              " Ueber die Tabs ", code("Ct vs Quantity"), ", ", code("Ct vs Sample"), ", ",
              code("Amplifikationskurven"), ", ", code("Ct SD"), ", ", code("Schmelzkurven"), ", ",
              code("Standardkurven"), " und ", code("Outlier Tests"), " navigieren. ",
              "PNG-Plots und XLSX-Tabellen koennen in den jeweiligen Cards heruntergeladen werden."
            ),
            tags$li(
              strong("Report Export:"),
              " Unter jedem Plot/Tabelle kann der Inhalt per Button dem Report hinzugefuegt werden. ",
              "Im Tab ", code("Report Export"), " kann daraus ein PDF/HTML/Word erstellt werden."
            )
          )
        ),
        bslib::nav_panel(
          "Besonderheiten",
          h3("Quantity & Ct-Only"),
          tags$ul(
            tags$li(
              "Wenn Quantity fehlt, wird sie auf 0 gesetzt und es erscheint ein Hinweis. ",
              "Plots mit Quantity auf der X-Achse koennen dann leer wirken."
            ),
            tags$li(
              "Die Seite ", code("Ct vs Sample"), " erlaubt Ct-only Auswertung (Sample auf der X-Achse)."
            )
          ),
          h3("CT/CRT Erkennung"),
          tags$ul(
            tags$li(
              "QuantStudio Dateien koennen ", code("CT"), " oder ", code("CRT"), " liefern. ",
              "Die App erkennt beide Varianten automatisch."
            ),
            tags$li(
              "Outlier-Analyse arbeitet auf Ct pro Well und nutzt die gewaehlte Outlier-Methode."
            )
          ),
          h3("Dateien zusammenfassen"),
          tags$ul(
            tags$li(
              "Wenn ", code("Dateien getrennt anzeigen"), " deaktiviert ist, werden Daten ueber Dateien hinweg ",
              "zusammengefasst. Dadurch gibt es pro Balken nur einen Errorbar."
            )
          )
        )
      )
    )
  )
)
