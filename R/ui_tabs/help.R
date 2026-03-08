tab_help <- bslib::nav_panel(
  "Hilfe",
  value = "help",
  bslib::card(
    bslib::card_header("Hilfe & Navigation"),
    bslib::card_body(
      bslib::navset_pill(
        bslib::nav_panel(
          "Start & Workflow",
          h3("Schnellstart in 8 Schritten"),
          tags$ol(
            tags$li(
              strong("Tab ", code("Daten laden"), " oeffnen."),
              " Dateien mit ", code(".xlsx"), ", ", code(".rdml"), " oder ", code(".xml"), " auswaehlen und ",
              code("Daten laden"), " klicken."
            ),
            tags$li(
              strong("Lade-Status lesen."),
              " Pruefen, ob Dateien korrekt erkannt wurden (Dateityp, Melt-Status, Quantity-Run-Status)."
            ),
            tags$li(
              strong("Analyse-Dateien waehlen."),
              " In ", code("Dateien fuer Analyse auswaehlen"), " Checkboxen setzen und ", code("Analyse starten"), " klicken."
            ),
            tags$li(
              strong("Sidebar-Filter setzen."),
              " Links Targets/Samples waehlen, optional ", code("Dateien getrennt anzeigen"), " aktivieren."
            ),
            tags$li(
              strong("Plate QC pruefen."),
              " Im Tab ", code("Plate Overview"), " Verteilung von Sample/Target/Farbstoff/Quantity je Well pruefen."
            ),
            tags$li(
              strong("Analyse-Tabs durchgehen."),
              " Ct-, Fluoreszenz-, Amplifikations-, SD-, Melt-, Standardkurven- und Outlier-Auswertung."
            ),
            tags$li(
              strong("Ergebnisse exportieren."),
              " Pro Card stehen Download-Buttons (PNG/XLSX) bereit."
            ),
            tags$li(
              strong("Report erzeugen."),
              " Inhalte ueber ", code("Zum Report hinzufuegen"), " sammeln, dann im Tab ", code("Report Export"), " PDF/HTML/Word erzeugen."
            )
          ),
          h3("Was passiert intern"),
          tags$ul(
            tags$li("Upload und Analyse sind getrennt: Laden erstellt Rohdaten, Analyse erzeugt Kennzahlen und Filteroptionen."),
            tags$li("Quantity-basierte Auswertungen ignorieren fehlende Quantity-Werte (kein Ersatz durch 0)."),
            tags$li("Die meisten Plots/Tables reagieren sofort auf globale Sidebar-Filter.")
          )
        ),
        bslib::nav_panel(
          "Filter verstehen",
          h3("Globale Filter und Effekt"),
          tags$ul(
            tags$li(
              strong(code("Targets auswaehlen")),
              ": Zeigt nur gewaehlte Target_IDs. Keine Auswahl bedeutet: keine Datenanzeige."
            ),
            tags$li(
              strong(code("Samples auswaehlen")),
              ": Begrenzt alle Tabs auf die gewaehlten Samples. Keine Auswahl bedeutet: keine Datenanzeige."
            ),
            tags$li(
              strong(code("Dateien getrennt anzeigen")),
              ": ",
              code("AN"),
              " = Ergebnisse je Datei getrennt; ",
              code("AUS"),
              " = Dateien werden zusammengefasst (gepoolte Statistik)."
            ),
            tags$li(
              strong(code("Y-Achse (Amplifikationskurven)")),
              ": Schaltet zwischen ",
              code("Rn"),
              " und ",
              code("Delta Rn"),
              " (falls vorhanden)."
            ),
            tags$li(
              strong(code("Y-Achse (Schmelzkurven)")),
              ": ",
              code("Derivative"),
              " (Peak-Fokus) oder ",
              code("Fluorescence"),
              " (Rohsignal)."
            ),
            tags$li(
              strong(code("Y-Skalierung (Facets)")),
              ": ",
              code("fixed"),
              " fuer direkte Hoehenvergleiche; ",
              code("free_y"),
              " fuer detailreiche Teilansichten je Target."
            ),
            tags$li(
              strong(code("Outlier-Test")),
              ": Waehlt Methode fuer Residuenanalyse (Dixon, Grubbs, Rosner)."
            ),
            tags$li(
              strong(code("Ct Y-Min / Ct Y-Max")),
              ": Sichtfenster fuer Ct-Plots. Regel: ",
              code("Y-Min < Y-Max"),
              "."
            )
          ),
          h3("Empfohlene Einstellungen"),
          tags$ul(
            tags$li("Vergleich zwischen Files: zuerst getrennt anzeigen, dann zusammenfassen."),
            tags$li("Bei stark unterschiedlichen Signalhoehen: ", code("free_y"), " verwenden."),
            tags$li("Fuer Standards/Outlier nur Samples mit valider Verdunnungsreihe waehlen.")
          )
        ),
        bslib::nav_panel(
          "Probleme loesen",
          h3("Haeufige Probleme und schnelle Loesung"),
          tags$ul(
            tags$li(
              strong("Upload-Fehler bei Datei"),
              ": Dateiformat/Sheetstruktur pruefen. Fuer QuantStudio wird ",
              code("Results"),
              " benoetigt, fuer AriaMX ",
              code("Tabular Results"),
              "."
            ),
            tags$li(
              strong("Keine Daten in Plot/Tabelle"),
              ": Pruefen, ob ",
              code("Analyse starten"),
              " geklickt wurde und ob mindestens ein Target + Sample selektiert ist."
            ),
            tags$li(
              strong("Ct vs Quantity leer"),
              ": In der aktuellen Auswahl fehlen gueltige Quantity-Werte. Auf ",
              code("Ct vs Sample"),
              " wechseln oder Samples mit Quantity waehlen."
            ),
            tags$li(
              strong("Standardkurven/Outlier ohne Ergebnis"),
              ": Es werden mehrere Quantity-Stufen > 0 benoetigt (idealerweise mindestens 3)."
            ),
            tags$li(
              strong("Report-Export fehlschlaegt"),
              ": Bei PDF TinyTeX/LaTeX-Setup pruefen, alternativ HTML/Word exportieren."
            ),
            tags$li(
              strong("App wirkt langsam"),
              ": Weniger Dateien gleichzeitig analysieren, Filter enger setzen, Browser-Tab neu laden."
            )
          ),
          h3("Diagnose-Checkliste"),
          tags$ol(
            tags$li("Im Tab ", code("Daten laden"), " checken: wurden Dateien erfolgreich geladen?"),
            tags$li("In der Datei-Uebersicht checken: sind Targets/Samples/Quantity vorhanden?"),
            tags$li("Filter testweise erweitern (alle Targets/Samples anwaehlen)."),
            tags$li("Zwischen ", code("Dateien getrennt"), " und zusammengefasst umschalten."),
            tags$li("Bei quantity-basierten Tabs pruefen, ob Quantity-Run-Status fehlende Werte meldet.")
          )
        ),
        bslib::nav_panel(
          "Daten laden",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Datei-Upload mit Mehrfachauswahl."),
            tags$li("Lade-Status je Datei (Format, Melt-Status, Quantity-Run-Status)."),
            tags$li("Dateiauswahl fuer Analyse und Datei-Uebersicht als Tabelle.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Hier wird festgelegt, welche Rohdaten in die spaetere Analyse einfliessen. Fehler in diesem Schritt propagieren in alle Tabs."),
          h3("So arbeitest du hier"),
          tags$ol(
            tags$li("Dateien waehlen und ", code("Daten laden"), " klicken."),
            tags$li("Im rechten Bereich kontrollieren, ob alle Dateien korrekt erkannt wurden."),
            tags$li("In ", code("Dateien fuer Analyse auswaehlen"), " die finalen Dateien markieren."),
            tags$li(code("Analyse starten"), " klicken.")
          )
        ),
        bslib::nav_panel(
          "Plate Overview",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Plattenansicht im 96/384-Well-Layout."),
            tags$li("Separate Karten fuer Target, Sample, Farbstoff, Quantity und Well Type."),
            tags$li("Hover-Details pro Well.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Du erkennst schnell Pipettier-/Layoutfehler, vertauschte Wells oder unerwartete Belegungen."),
          h3("Wichtige Einstellungen"),
          tags$ul(
            tags$li(code("Datei auswaehlen"), ": wechselt zwischen geladenen Platten."),
            tags$li("Hover zeigt Well, Type, Sample, Target, Dye, Quantity.")
          )
        ),
        bslib::nav_panel(
          "Fluoreszenz",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Plot 1: maximale Fluoreszenz pro Sample."),
            tags$li("Plot 2: Delta-Fluoreszenz (Max-Min) pro Sample."),
            tags$li("Tabelle mit Mittelwert/SD je Sample-Target-Quantity.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Signalstaerke und Dynamik lassen sich zwischen Proben, Targets und Dateien vergleichen."),
          h3("Einstellungen mit Effekt"),
          tags$ul(
            tags$li("Globale Target-/Sample-Filter begrenzen die dargestellten Gruppen."),
            tags$li(code("Dateien getrennt anzeigen"), " trennt Facets je Datei.")
          )
        ),
        bslib::nav_panel(
          "Ct vs Quantity",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Balkenplot Ct-Mittelwert +/- SD je Quantity und Target."),
            tags$li("Ct-Tabelle als strukturierte Exportansicht.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Direkter Blick auf Konzentrationseffekt und Reproduzierbarkeit je Target/Sample."),
          h3("Einstellungen mit Effekt"),
          tags$ul(
            tags$li(code("Ct Y-Min / Ct Y-Max"), " steuert den sichtbaren Ct-Bereich."),
            tags$li(code("Y-Skalierung"), " bestimmt Vergleichbarkeit ueber Facets."),
            tags$li("Fehlende Quantity wird ausgeschlossen; Hinweis erscheint automatisch.")
          )
        ),
        bslib::nav_panel(
          "Ct vs Sample",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Ct-Mittelwert +/- SD je Sample, facettiert nach Target."),
            tags$li("Ct-Tabelle fuer Sample-zentrierten Vergleich.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Diese Ansicht funktioniert auch bei Ct-only Datensaetzen ohne valide Quantity und ist daher ein robuster Fallback."),
          h3("Einstellungen mit Effekt"),
          tags$ul(
            tags$li("Globale Target-/Sample-Filter steuern die dargestellten Gruppen."),
            tags$li("Quantity wird nur als Legende/Fuellung genutzt, nicht als X-Achse.")
          )
        ),
        bslib::nav_panel(
          "Amplifikationskurven",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Kurvenverlauf ueber Zyklen je Well/Sample/Target."),
            tags$li("Interaktive Darstellung fuer Signalqualitaet und Kurvenform.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Hier siehst du Kurvenanomalien, S-Form-Qualitaet und Signalverschiebungen direkt im Rohverlauf."),
          h3("Einstellungen mit Effekt"),
          tags$ul(
            tags$li(code("Y-Achse (Amplifikationskurven)"), ": ", code("Rn"), " oder ", code("Delta Rn")),
            tags$li(code("Dateien getrennt anzeigen"), ": Dateivergleich ohne Pooling.")
          )
        ),
        bslib::nav_panel(
          "Ct SD",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Plot: Ct-Standardabweichung vs Quantity."),
            tags$li("Heatmap: mittlere Ct-SD pro Sample x Target.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Stabilitaet und Streuung werden sichtbar; hohe SD-Werte zeigen potenzielle Problemstellen."),
          h3("Einstellungen mit Effekt"),
          tags$ul(
            tags$li("Quantity-Plot ignoriert Zeilen ohne Quantity."),
            tags$li(code("Y-Skalierung"), " beeinflusst Vergleich ueber Targets.")
          )
        ),
        bslib::nav_panel(
          "Schmelzkurven",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Kurvenplot mit Temperaturachse."),
            tags$li("Peak-Tabelle (Tm-Kandidaten) je Well."),
            tags$li("Peak-Summary je Sample/Target.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Spezifitaet der Amplifikation und moegliche Nebenprodukte lassen sich ueber Peakmuster beurteilen."),
          h3("Einstellungen mit Effekt"),
          tags$ul(
            tags$li(code("Y-Achse (Schmelzkurven)"), ": Derivative fuer Peakfokus, Fluorescence fuer Rohsignal."),
            tags$li("Target-/Sample-Filter schraenken die Peak-Berechnung entsprechend ein.")
          )
        ),
        bslib::nav_panel(
          "Standardkurven",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Uebersichtstabelle mit LDR, Slope, R2, Effizienz."),
            tags$li("Slope- und Effizienzplots."),
            tags$li("Scatterplot Ct ~ log10(Quantity) je Target.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Bewertet Quantifizierbarkeit und Assay-Qualitaet ueber Konzentrationsreihen."),
          h3("Einstellungen mit Effekt"),
          tags$ul(
            tags$li(code("Target (inkl. Kanal) fuer Scatterplot"), " waehlt den Detailplot."),
            tags$li("Nur Quantity > 0 wird fuer Modellierung verwendet."),
            tags$li("Ohne ausreichend Quantity-Stufen entstehen keine stabilen Fits.")
          )
        ),
        bslib::nav_panel(
          "Outlier Tests",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Auswahl fuer Target und Sample."),
            tags$li("Erklaerung der Testmethode."),
            tags$li("Outlier-Tabelle und Residuenplot.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Findet auffaellige Wells auf Basis von Residuen aus dem Fit Ct ~ log10(Quantity)."),
          h3("Einstellungen mit Effekt"),
          tags$ul(
            tags$li(code("Outlier-Test"), " wechselt Methode (Dixon/Grubbs/Rosner)."),
            tags$li("Nur Wells mit Ct und Quantity > 0 gehen in die Berechnung ein."),
            tags$li("Zu wenige Wells fuehren zu Validierungshinweis statt irrefuehrendem Ergebnis.")
          )
        ),
        bslib::nav_panel(
          "Report Export",
          h3("Was du hier siehst"),
          tags$ul(
            tags$li("Report-Name, Formatwahl (PDF/HTML/Word), Liste hinzugefuegter Inhalte."),
            tags$li("Button zum Erstellen und Download-Link nach erfolgreicher Generierung.")
          ),
          h3("Warum diese Seite wichtig ist"),
          tags$p("Baut aus selektierten Plots/Tabellen einen reproduzierbaren Ergebnisreport."),
          h3("So nutzt du die Seite korrekt"),
          tags$ol(
            tags$li("In Analyse-Tabs relevante Elemente mit ", code("Zum Report hinzufuegen"), " sammeln."),
            tags$li("Im Report-Tab Dateiname und Format setzen."),
            tags$li(code("Report erstellen"), " klicken und Ergebnis herunterladen.")
          ),
          h3("Wenn etwas nicht klappt"),
          tags$ul(
            tags$li("PDF-Fehler: TinyTeX/LaTeX pruefen, alternativ HTML/Word waehlen."),
            tags$li("Leerer Report: vorher sicherstellen, dass Inhalte hinzugefuegt wurden.")
          )
        )
      )
    )
  )
)
