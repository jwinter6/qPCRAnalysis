if (!("shiny" %in% .packages())) {
  library(shiny)
}
if (!("bslib" %in% .packages())) {
  library(bslib)
}

tooltip_label <- function(label, text) {
  tagList(
    label,
    bslib::tooltip(
      tags$span("i", class = "qpcr-tooltip-icon"),
      text
    )
  )
}

ui_sidebar <- bslib::sidebar(
  accordion(
    accordion_panel(
      "Globale Filter",
      tags$style(
        ".qpcr-tooltip-icon{display:inline-block;margin-left:6px;padding:0 4px;",
        "border:1px solid #999;border-radius:10px;font-size:10px;line-height:14px;",
        "cursor:help;color:#555;}"
      ),
      checkboxGroupInput(
        inputId  = "target_filter",
        label    = tooltip_label(
          "Targets auswaehlen",
          "Filtert die Analyse auf die ausgewaehlten Targets."
        ),
        choices  = NULL,
        selected = NULL
      ),
      checkboxGroupInput(
        inputId  = "sample_filter",
        label    = tooltip_label(
          "Samples auswaehlen",
          "Filtert die Analyse auf die ausgewaehlten Samples."
        ),
        choices  = NULL,
        selected = NULL
      ),
      checkboxInput(
        inputId  = "separate_files",
        label    = tooltip_label(
          "Dateien getrennt anzeigen",
          "Wenn aktiv, werden Ergebnisse pro Datei getrennt gezeigt; sonst werden Dateien zusammengefasst."
        ),
        value    = FALSE
      ),
      uiOutput("y_axis_ui"),
      radioButtons(
        inputId  = "melt_y_axis",
        label    = tooltip_label(
          "Y-Achse (Schmelzkurven)",
          "Waehlt den Signaltyp fuer Schmelzkurven."
        ),
        choices  = c(
          "Derivative"   = "Derivative",
          "Fluorescence" = "Fluorescence"
        ),
        selected = "Derivative"
      ),
      radioButtons(
        inputId  = "y_scale_mode",
        label    = tooltip_label(
          "Y-Skalierung (Facets)",
          "Legt fest, ob alle Facets die gleiche Skala haben oder nicht."
        ),
        choices  = c(
          "Alle Facets gleiche Skala" = "fixed",
          "Jedes Facet eigene Skala"  = "free_y"
        ),
        selected = "fixed"
      )
    ),
    accordion_panel(
      "Outlier-Analyse",
      selectInput(
        inputId  = "outlier_test",
        label    = tooltip_label(
          "Outlier-Test",
          "Waehlt die statistische Methode zur Outlier-Erkennung."
        ),
        choices  = c("Dixon", "Grubbs", "Rosner"),
        selected = "Grubbs"
      )
    ),
    accordion_panel(
      "Ct-Achse (Ct vs Quantity)",
      numericInput(
        inputId = "ct_y_min",
        label   = tooltip_label(
          "Ct Y-Min",
          "Untere Grenze der Ct-Y-Achse fuer Ct-Plots."
        ),
        value   = 10
      ),
      numericInput(
        inputId = "ct_y_max",
        label   = tooltip_label(
          "Ct Y-Max",
          "Obere Grenze der Ct-Y-Achse fuer Ct-Plots."
        ),
        value   = 40
      )
    )
  )
)
