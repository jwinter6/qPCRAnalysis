if (!("shiny" %in% .packages())) {
  library(shiny)
}
if (!("bslib" %in% .packages())) {
  library(bslib)
}

ui_sidebar <- bslib::sidebar(
  accordion(
    accordion_panel(
      "Globale Filter",
      checkboxGroupInput(
        inputId  = "target_filter",
        label    = "Targets auswaehlen",
        choices  = NULL,
        selected = NULL
      ),
      checkboxGroupInput(
        inputId  = "sample_filter",
        label    = "Samples auswaehlen",
        choices  = NULL,
        selected = NULL
      ),
      uiOutput("y_axis_ui"),
      radioButtons(
        inputId  = "melt_y_axis",
        label    = "Y-Achse (Schmelzkurven)",
        choices  = c(
          "Derivative"   = "Derivative",
          "Fluorescence" = "Fluorescence"
        ),
        selected = "Derivative"
      ),
      radioButtons(
        inputId  = "y_scale_mode",
        label    = "Y-Skalierung (Facets)",
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
        label    = "Outlier-Test",
        choices  = c("Dixon", "Grubbs", "Rosner"),
        selected = "Grubbs"
      )
    ),
    accordion_panel(
      "Ct-Achse (Ct vs Quantity)",
      numericInput(
        inputId = "ct_y_min",
        label   = "Ct Y-Min",
        value   = 10
      ),
      numericInput(
        inputId = "ct_y_max",
        label   = "Ct Y-Max",
        value   = 40
      )
    )
  )
)
