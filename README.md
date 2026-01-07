# qPCRAnalysis

Interaktive Shiny-WebApp zur Auswertung von qPCR-Experimenten. Die App vereinheitlicht Daten
aus verschiedenen Geräteformaten, ermöglicht Filterung nach Target/Sample und liefert Plots,
Tabellen sowie Exportfunktionen.

## Funktionen

- Datenimport von QuantStudio- und AriaMX-Exporten (XLSX)
- Automatische Vereinheitlichung der Datenstruktur
- Globale Filter nach Target und Sample
- Navigation ueber Navbar, globale Sidebar und Cards fuer Inhalte
- UI-Theming mit `fresh` (Bootstrap 4)
- Ct vs Quantity (Mittelwert + SD) als Plot und Tabelle
- Amplifikationskurven (Rn / Delta Rn)
- Ct SD Plots und Heatmap
- Schmelzkurven inkl. Peak-Analyse
- Standardkurven inkl. LDR-Bereich, Steigung, R2 und Effizienz
- Outlier-Analyse auf Residuen (Dixon, Grubbs, Rosner)
- PNG- und XLSX-Downloads pro Analysebereich

## Unterstützte Formate

### QuantStudio (aehnliches Format)
- Sheet: `Results` (ab Zeile 45, Pflicht)
- Optional: `Amplification Data`, `Melt Curve Raw Data`

### AriaMX Export
- Sheet: `Tabular Results` (Pflicht)
- Amplifikations-Sheet wird automatisch erkannt

## Installation

1. R Pakete installieren (einmalig):
   - `shiny`, `bslib`, `fresh`, `tidyverse`, `readxl`, `plotly`, `ggthemes`, `DT`, `writexl`,
     `outliers`, `EnvStats`, `rmarkdown`, `knitr`, `tinytex`
2. Projektordner oeffnen.

Zusatz fuer Report-Export:
- LaTeX (z. B. TinyTeX) fuer PDF-Export

## App starten

In R oder RStudio:

```r
shiny::runApp()
```

## Docker

Das Dockerfile installiert zusaetzliche Pakete fuer Report-Export (rmarkdown, knitr, tinytex).

```sh
docker build -t qpcranalysis .
docker run --rm -p 3838:3838 qpcranalysis
```

## Workflow (Kurzfassung)

1. Tab "Daten laden": XLSX-Dateien auswaehlen und "Daten laden" klicken.
2. Datei-Uebersicht pruefen.
3. In "Dateien fuer Analyse auswaehlen" die gewuenschten Dateien markieren und "Analyse starten".
4. In der Sidebar globale Filter setzen (Targets, Samples, Achsen).
5. Ergebnisse in den Tabs ansehen und bei Bedarf als PNG/XLSX exportieren.

## Ergebnisse & Exporte

- Ct vs Quantity: Plot + Tabelle
- Amplifikationskurven: Plot (Rn / Delta Rn)
- Ct SD: Plot + Heatmap + Tabelle
- Schmelzkurven: Plot + Peak-Tabellen
- Standardkurven: Uebersichtstabelle + Slope/Effizienz-Plots + Scatterplot
- Outlier Tests: Tabelle + Residuenplot

## Projektstruktur (Auszug)

- `app.R` Startpunkt der App
- `R/helpers_*.R` Hilfsfunktionen (Import, Standardkurven, Outlier)
- `R/ui_sidebar.R` Sidebar UI
- `R/ui_tabs/` UI je Tab
- `R/server/` Serverlogik je Tab/Block
