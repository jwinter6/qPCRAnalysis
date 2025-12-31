if (!("shiny" %in% .packages())) {
  library(shiny)
}
if (!("shinydashboard" %in% .packages())) {
  library(shinydashboard)
}

ui_sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Daten laden",          tabName = "load",      icon = icon("folder-open")),
    menuItem("Ct vs Quantity",       tabName = "ctqty",     icon = icon("chart-column")),
    menuItem("Amplifikationskurven", tabName = "amp",       icon = icon("wave-square")),
    menuItem("Ct SD",                tabName = "ctsd",      icon = icon("chart-line")),
    menuItem("Schmelzkurven",        tabName = "melt",      icon = icon("fire")),
    menuItem("Standardkurven",       tabName = "stdcurves", icon = icon("ruler")),
    menuItem("Outlier Tests",        tabName = "outliers",  icon = icon("exclamation-triangle")),
    menuItem("Hilfe",                tabName = "help",      icon = icon("circle-question"))
  ),
  hr(),
  h4("Globale Filter"),
  checkboxGroupInput(
    inputId  = "target_filter",
    label    = "Targets auswählen",
    choices  = NULL,
    selected = NULL
  ),
  checkboxGroupInput(
    inputId  = "sample_filter",
    label    = "Samples auswählen",
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
  ),
  hr(),
  h4("Outlier-Analyse"),
  selectInput(
    inputId  = "outlier_test",
    label    = "Outlier-Test",
    choices  = c("Dixon", "Grubbs", "Rosner"),
    selected = "Grubbs"
  ),
  hr(),
  h4("Ct-Achse (Ct vs Quantity)"),
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
