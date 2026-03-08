# qPCRAnalysis

Interaktive Shiny-WebApp zur Auswertung von qPCR-Experimenten. Die App vereinheitlicht Daten
aus verschiedenen Geraeteformaten, ermoeglicht Filterung nach Target/Sample und liefert Plots,
Tabellen sowie Exportfunktionen.

## Funktionen

- Datenimport von QuantStudio- und AriaMX-Exporten (XLSX) sowie RDML-Dateien (.rdml/.xml)
- Automatische Vereinheitlichung der Datenstruktur
- Run-weiser Quantity-Check pro Datei (auch fuer XLSX) mit Kennzeichnung von Runs ohne Quantity
- Fehlende Quantity wird nicht auf 0 gesetzt; quantity-basierte Auswertungen schliessen diese Werte aus
- Globale Filter nach Target und Sample
- Globaler Schalter: Dateien getrennt anzeigen vs. zusammenfassen
- Navigation ueber Navbar, globale Sidebar und Cards fuer Inhalte
- UI-Theming mit `fresh` (Bootstrap 4)
- Ct vs Quantity (Mittelwert + SD) als Plot und Tabelle
- Ct vs Sample (Ct-Only Auswertung)
- Amplifikationskurven (Rn / Delta Rn)
- Ct SD Plots und Heatmap
- Schmelzkurven inkl. Peak-Analyse
- Standardkurven inkl. LDR-Bereich, Steigung, R2 und Effizienz
- Outlier-Analyse auf Residuen (Dixon, Grubbs, Rosner)
- PNG- und XLSX-Downloads pro Analysebereich
- Plate Overview (96/384-Well) mit Targets/Samples/Farbstoff/Quantity
- Report Export (PDF/HTML/Word) mit Auswahl der Inhalte

## Unterstützte Formate

### QuantStudio (aehnliches Format)
- Sheet: `Results` (ab Zeile 45, Pflicht)
- Optional: `Amplification Data`, `Melt Curve Raw Data`

### AriaMX Export
- Sheet: `Tabular Results` (Pflicht)
- Amplifikations-Sheet wird automatisch erkannt

### RDML (Version 1.1)
- Root: `rdml`, Reaktionen aus `experiment/run/react`
- Sowohl direktes XML als auch ZIP-verpackte `.rdml`-Container werden automatisch erkannt
- Ct/Cq aus `react/data/cq` (negative Werte werden als fehlend behandelt)
- Amplifikationskurven aus `react/data/adp` (`cyc` -> `Cycle`, `fluor` -> `Rn`)
- Schmelzkurven aus `react/data/mdp` (`tmp` -> `Temperature`, `fluor` -> `Fluorescence`)
- Derivative fuer Schmelzkurven wird aus den RDML-Messpunkten numerisch berechnet
- Sample-Quantity wird aus `sample/quantity/value` uebernommen
- Targets und Reporter werden ueber `target/@id` und `target/dyeId/@id` gemappt
- Well-Position wird aus `react@id` und `pcrFormat` rekonstruiert (falls klassisches Plattenlayout)

## Installation

1. R Pakete installieren (einmalig):
   - `shiny`, `bslib`, `fresh`, `tidyverse`, `readxl`, `xml2`, `plotly`, `ggthemes`, `DT`, `writexl`,
     `outliers`, `EnvStats`, `rmarkdown`, `knitr`, `tinytex`
   - fuer automatisierte Tests zusaetzlich: `testthat`
2. Projektordner oeffnen.

Zusatz fuer Report-Export:
- LaTeX (z. B. TinyTeX) fuer PDF-Export

## App starten

In R oder RStudio:

```r
shiny::runApp()
```

## Automatisierte Tests

Es ist ein vollautomatisches Test-Setup vorhanden (ohne manuelle Klicks in der UI).

### Schnellstart

Im Projektordner ausfuehren:

```sh
Rscript tests/run_tests.R
```

Falls `testthat` noch nicht installiert ist:

```r
install.packages("testthat")
```

Alternative in R:

```r
testthat::test_dir("tests/testthat", reporter = "summary")
```

### Was wird geprueft

- Reader-Logik fuer alle Formate:
  - QuantStudio-aehnliches XLSX (`Results`, `Amplification Data`, `Melt Curve Raw Data`)
  - AriaMX-XLSX (`Tabular Results` + automatisch erkanntes Amplifikationssheet)
  - RDML/XML (inkl. Reporter-/Target-Mapping, Sample-Normalisierung, Quantity-Handling)
  - ZIP-verpacktes RDML (wenn Systemkommando `zip` verfuegbar)
- Fehlerfall bei unbekanntem XLSX-Format
- End-to-End Server-Workflow mit `shiny::testServer()`:
  - Upload mehrerer Dateien
  - Analyse-Start
  - Run-weiser Quantity-Status in der Datei-Uebersicht
  - Sicherstellung, dass fehlende Quantity als `NA` bleibt (nicht `0`)
  - Sicherstellung, dass quantity-basierte Auswertungen nur Datensaetze mit Quantity verwenden
  - Validierungsfehler fuer Ct-vs-Quantity/Ct-SD-vs-Quantity, wenn komplett keine Quantity vorliegt

### Testdaten

Die Tests erzeugen ihre Beispiel-Dateien zur Laufzeit selbst (temporere Fixtures) und brauchen keine manuellen Eingabedateien:

- synthetische QuantStudio-XLSX-Datei mit `skip=44` kompatibler Struktur
- synthetische AriaMX-XLSX-Datei
- synthetische RDML-Datei mit zwei Runs (ein Run mit, ein Run ohne Quantity)
- optional ZIP-RDML aus der synthetischen RDML-Datei

Dadurch sind die Tests reproduzierbar und unabhaengig von lokalen Upload-Dateien.

## Docker

Das Dockerfile installiert zusaetzliche Pakete fuer Report-Export (rmarkdown, knitr, tinytex).

```sh
docker build -t qpcranalysis .
docker run --rm -p 3838:3838 qpcranalysis
```

## Workflow (Kurzfassung)

1. Tab "Daten laden": qPCR-Dateien (`.xlsx`, `.rdml`, `.xml`) auswaehlen und "Daten laden" klicken.
2. Datei-Uebersicht pruefen (inkl. Melt-Status bei RDML und Quantity-Status pro Run).
3. In "Dateien fuer Analyse auswaehlen" die gewuenschten Dateien markieren und "Analyse starten".
4. In der Sidebar globale Filter setzen (Targets, Samples, Achsen, Dateien getrennt/zusammen).
5. Ergebnisse in den Tabs ansehen und bei Bedarf als PNG/XLSX exportieren.
6. Plate Overview pruefen (Plattenansichten nach Target/Sample/Farbstoff/Quantity).
7. Inhalte per Button zum Report hinzufuegen und im Tab "Report Export" als PDF/HTML/Word exportieren.

## Ergebnisse & Exporte

- Ct vs Quantity: Plot + Tabelle
- Ct vs Sample: Plot + Tabelle
- Amplifikationskurven: Plot (Rn / Delta Rn)
- Ct SD: Plot + Heatmap + Tabelle
- Schmelzkurven: Plot + Peak-Tabellen
- Standardkurven: Uebersichtstabelle + Slope/Effizienz-Plots + Scatterplot
- Outlier Tests: Tabelle + Residuenplot
- Plate Overview: Plattenansichten (Targets, Samples, Farbstoff, Quantity, Well Type)
- Report Export: PDF/HTML/Word mit Auswahl der Inhalte

## Projektstruktur (Auszug)

- `app.R` Startpunkt der App
- `R/helpers_*.R` Hilfsfunktionen (Import, Standardkurven, Outlier)
- `R/ui_sidebar.R` Sidebar UI
- `R/ui_tabs/` UI je Tab
- `R/server/` Serverlogik je Tab/Block

## Tooltips in der Sidebar

Die Sidebar enthaelt Tooltips fuer alle Schalter/Einstellungen. Mit der Maus ueber das "(i)" fahren,
um eine Kurzbeschreibung der jeweiligen Option zu sehen.
