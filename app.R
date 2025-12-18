############################################################
# app.R – qPCR Dashboard (shinydashboard, Upload-basiert)
#
# Plot-Themes:
# - Alle ggplot2-Grafiken verwenden jetzt ggthemes::theme_gdocs()
############################################################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(plotly)
library(ggthemes)    # NEU: Themes für ggplot2 (theme_few)
library(DT)
library(writexl)
library(outliers)
library(EnvStats)

############################
# Hilfsfunktionen zum Einlesen
############################

# Einlesen einer qPCR-Excel-Datei:
# - "Results" (ab Zeile 45)
# - "Amplification Data" (ab Zeile 45)
# Join über Well-Position, mit Übernahme des Dateinamens (source_file)
# Unterstützt:
#   * QuantStudio-ähnliches Format mit Sheet "Results"
#   * AriaMX-Exports mit Sheet "Tabular Results"
read_qpcr_file <- function(path, source_name = basename(path)) {
  
  sheets <- excel_sheets(path)
  
  ## ----------------------------------------------------------
  ## FALL 1: QuantStudio-ähnliches Format (Sheet "Results")
  ## ----------------------------------------------------------
  if ("Results" %in% sheets) {
    
    # Results ab Zeile 45
    results_raw <- read_excel(
      path,
      sheet    = "Results",
      skip     = 44,
      col_names = TRUE
    )
    
    # Amplification Data ab Zeile 45, falls vorhanden
    if ("Amplification Data" %in% sheets) {
      amp_raw <- read_excel(
        path,
        sheet    = "Amplification Data",
        skip     = 44,
        col_names = TRUE
      )
    } else {
      amp_raw <- tibble()
    }
    
    # Spalten, die später numerisch verwendet werden, zunächst als character erzwingen,
    # damit bind_rows() über mehrere Dateien funktioniert.
    if ("CRT" %in% names(results_raw)) {
      results_raw <- results_raw %>% mutate(CRT = as.character(CRT))
    }
    if ("Crt Mean" %in% names(results_raw)) {
      results_raw <- results_raw %>% mutate(`Crt Mean` = as.character(`Crt Mean`))
    }
    if ("Quantity" %in% names(results_raw)) {
      results_raw <- results_raw %>% mutate(Quantity = as.character(Quantity))
    }
    
    # Well-Position vereinheitlichen
    results <- results_raw %>%
      rename(
        well_position     = matches("Well[ _-]?Position"),
        `Target Name_res` = `Target Name`
      )
    
    if (nrow(amp_raw) > 0) {
      amp <- amp_raw %>%
        rename(
          well_position = matches("Well[ _-]?Position")
        )
    } else {
      amp <- tibble()
    }
    
    if (nrow(amp) > 0) {
      joined <- results %>%
        left_join(
          amp,
          by = "well_position",
          suffix = c("_res", "_amp")
        )
    } else {
      joined <- results
    }
    
    joined <- joined %>%
      mutate(
        source_file = source_name
      )
    
    return(joined)
  }
  
  ## ----------------------------------------------------------
  ## FALL 2: Neues Format – AriaMX Export
  ## ----------------------------------------------------------
  if ("Tabular Results" %in% sheets) {
    
    # 2.1 Tabular Results einlesen
    tab <- read_excel(
      path,
      sheet    = "Tabular Results",
      col_names = TRUE
    )
    
    # Cq und Quantity einheitlich als character
    if ("Cq (∆R)" %in% names(tab)) {
      tab <- tab %>% mutate(`Cq (∆R)` = as.character(`Cq (∆R)`))
    }
    if ("Quantity (nanograms)" %in% names(tab)) {
      tab <- tab %>% mutate(`Quantity (nanograms)` = as.character(`Quantity (nanograms)`))
    }
    
    # Mapping in internes Schema
    results <- tab %>%
      transmute(
        well_position     = as.character(Well),
        `Sample Name`     = as.character(`Well Name`),
        `Target Name_res` = as.character(Target),
        Reporter          = as.character(Dye),
        CRT               = if ("Cq (∆R)" %in% names(tab)) `Cq (∆R)` else NA_character_,
        Quantity          = if ("Quantity (nanograms)" %in% names(tab)) `Quantity (nanograms)` else NA_character_,
        source_file       = source_name,
        .keep = "all"
      )
    
    # 2.2 Amplifikationssheet erkennen
    known_sheets <- c("Plate Setup", "Thermal Profile", "Tabular Results", "Experiment Notes")
    amp_sheet_candidates <- setdiff(sheets, known_sheets)
    if (length(amp_sheet_candidates) == 0) {
      # Kein Amplifikationssheet gefunden, dann nur Results zurückgeben
      return(results)
    }
    
    amp_sheet <- amp_sheet_candidates[1]
    
    amp_raw <- read_excel(
      path,
      sheet    = amp_sheet,
      col_names = FALSE
    )
    
    # AriaMX Amplifikationsdaten liegen blockweise pro Well vor.
    amp_list <- list()
    current_well <- NA_character_
    
    for (i in seq_len(nrow(amp_raw))) {
      row <- amp_raw[i, ]
      first_col <- as.character(row[[1]])
      
      # Kopfzeile: Well-Kennung, z. B. "A1, Replicate 1, ..."
      if (!is.na(first_col) && grepl("^[A-H][0-9]+", first_col)) {
        well <- sub(",.*$", "", first_col)
        current_well <- well
        next
      }
      
      # Datenzeilen: numeric in den ersten Spalten, current_well gesetzt
      if (!is.na(current_well)) {
        cycle_val <- suppressWarnings(as.numeric(row[[1]]))
        drn_val   <- suppressWarnings(as.numeric(row[[2]]))
        
        if (!is.na(cycle_val) && !is.na(drn_val)) {
          amp_list[[length(amp_list) + 1]] <- tibble(
            well_position = current_well,
            Cycle         = cycle_val,
            Rn            = NA_real_,  # AriaMX liefert direkt DeltaRn
            `Delta Rn`    = drn_val,
            source_file   = source_name
          )
        }
      }
    }
    
    if (length(amp_list) > 0) {
      amp <- bind_rows(amp_list)
    } else {
      amp <- tibble()
    }
    
    if (nrow(amp) > 0) {
      joined <- results %>%
        left_join(
          amp,
          by = c("well_position", "source_file")
        )
    } else {
      joined <- results
    }
    
    return(joined)
  }
  
  stop(
    paste0(
      "Unbekanntes XLSX-Format in Datei: ", source_name,
      " (weder Sheet 'Results' noch 'Tabular Results' gefunden)."
    )
  )
}

# Einlesen der Melt Curve Daten (nur für QuantStudio-Format mit Sheet "Melt Curve Raw Data")
read_qpcr_melt_file <- function(path, source_name = basename(path)) {
  
  sheets <- excel_sheets(path)
  
  if (!("Melt Curve Raw Data" %in% sheets)) {
    return(NULL)
  }
  
  # Melt Curve Raw Data ab Zeile 45
  melt_raw <- read_excel(
    path,
    sheet    = "Melt Curve Raw Data",
    skip     = 44,
    col_names = TRUE
  )
  
  if (nrow(melt_raw) == 0) {
    return(NULL)
  }
  
  # Results ab Zeile 45, um Sample/Target/Reporter dazuzujoinen
  results_raw <- read_excel(
    path,
    sheet    = "Results",
    skip     = 44,
    col_names = TRUE
  )
  
  if (nrow(results_raw) == 0) {
    return(NULL)
  }
  
  results <- results_raw %>%
    rename(
      well_position     = matches("Well[ _-]?Position"),
      `Target Name_res` = `Target Name`
    ) %>%
    mutate(
      source_file = source_name
    )
  
  melt <- melt_raw %>%
    rename(
      well_position = matches("Well[ _-]?Position")
    ) %>%
    mutate(
      source_file = source_name
    )
  
  joined_melt <- melt %>%
    left_join(
      results,
      by = c("well_position", "source_file"),
      suffix = c("_melt", "_res")
    ) %>%
    mutate(
      Temperature  = suppressWarnings(as.numeric(Temperature)),
      Fluorescence = suppressWarnings(as.numeric(Fluorescence)),
      Derivative   = suppressWarnings(as.numeric(Derivative)),
      `Target Name_res` = as.character(`Target Name_res`),
      Reporter     = if ("Reporter" %in% names(.)) as.character(Reporter) else NA_character_,
      Target_ID    = if_else(
        !is.na(Reporter),
        paste0(`Target Name_res`, " [", Reporter, "]"),
        `Target Name_res`
      )
    )
  
  joined_melt
}

############################
# UI
############################

ui <- dashboardPage(
  dashboardHeader(
    title = "qPCR Dashboard"
  ),
  dashboardSidebar(
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
    uiOutput("outlier_target_ui"),
    uiOutput("outlier_sample_ui"),
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
  ),
  dashboardBody(
    tabItems(
      ###################
      # Tab: Daten laden
      ###################
      tabItem(
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
      ),
      
      ###################
      # Tab: Ct vs Quantity
      ###################
      tabItem(
        tabName = "ctqty",
        fluidRow(
          box(
            width = 12,
            title = "Ct (Mean ± SD) vs Quantity",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("qpcr_plot", height = "600px"),
            br(),
            downloadButton("download_ct_plot_png", "Download Ct-Plot (PNG)")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Ct-Tabelle (Mean ± SD)",
            status = "info",
            solidHeader = TRUE,
            DTOutput("ct_table"),
            br(),
            downloadButton("download_ct_table_xlsx", "Download Ct-Tabelle (XLSX)")
          )
        )
      ),
      
      ###################
      # Tab: Amplifikationskurven
      ###################
      tabItem(
        tabName = "amp",
        fluidRow(
          box(
            width = 12,
            title = "Amplifikationskurven",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("qpcr_curve_plot", height = "600px"),
            br(),
            downloadButton("download_amp_plot_png", "Download Amplifikationskurven (PNG)")
          )
        )
      ),
      
      ###################
      # Tab: Ct SD & Heatmap
      ###################
      tabItem(
        tabName = "ctsd",
        fluidRow(
          box(
            width = 12,
            title = "Ct SD vs Quantity",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("qpcr_sd_plot", height = "500px"),
            br(),
            downloadButton("download_ctsd_plot_png", "Download Ct SD Plot (PNG)")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Ct SD Heatmap (Sample × Target)",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("qpcr_sd_heatmap", height = "500px"),
            br(),
            downloadButton("download_ctsd_heatmap_png", "Download Ct SD Heatmap (PNG)"),
            downloadButton("download_ctsd_heatmap_xlsx", "Download Ct SD Heatmap Tabelle (XLSX)")
          )
        )
      ),
      
      ###################
      # Tab: Schmelzkurven
      ###################
      tabItem(
        tabName = "melt",
        fluidRow(
          box(
            width = 12,
            title = "Schmelzkurven",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("melt_curve_plot", height = "600px"),
            br(),
            downloadButton("download_melt_plot_png", "Download Schmelzkurven (PNG)")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Schmelzkurven-Peaks (Tm-Kandidaten)",
            status = "info",
            solidHeader = TRUE,
            DTOutput("melt_peaks_table"),
            br(),
            downloadButton("download_melt_peaks_xlsx", "Download Peak-Tabelle (XLSX)")
          ),
          box(
            width = 6,
            title = "Zusammenfassung Peaks pro Sample/Target",
            status = "info",
            solidHeader = TRUE,
            DTOutput("melt_peak_summary_table"),
            br(),
            downloadButton("download_melt_peaks_summary_xlsx", "Download Peak-Summary (XLSX)")
          )
        )
      ),
      
      ###################
      # Tab: Standardkurven
      ###################
      tabItem(
        tabName = "stdcurves",
        fluidRow(
          box(
            width = 12,
            title = "Standardkurven – Übersicht (LDR & Effizienz)",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("standardcurve_table"),
            br(),
            downloadButton("download_stdcurves_xlsx", "Download Standardkurven-Tabelle (XLSX)")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Steigungen (Slope) je Sample/Target",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("stdcurve_slope_plot", height = "500px"),
            br(),
            downloadButton("download_stdcurve_slope_png", "Download Slope-Plot (PNG)")
          ),
          box(
            width = 6,
            title = "Effizienz (%) je Sample/Target",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("stdcurve_eff_plot", height = "500px"),
            br(),
            downloadButton("download_stdcurve_eff_png", "Download Effizienz-Plot (PNG)")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Standardkurven Scatterplots (Ct ~ log10(Quantity))",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "std_scatter_target",
              "Target (inkl. Kanal) für Scatterplot",
              choices = NULL
            ),
            plotlyOutput("stdcurve_scatter_plot", height = "600px"),
            br(),
            downloadButton("download_stdcurve_scatter_png", "Download Scatterplot (PNG)")
          )
        )
      ),
      
      ###################
      # Tab: Outlier Tests
      ###################
      tabItem(
        tabName = "outliers",
        fluidRow(
          box(
            width = 12,
            title = "Outlier Analyse (auf Residuen der Standardkurve, Ct pro Well)",
            status = "danger",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 4,
                uiOutput("outlier_target_ui")
              ),
              column(
                width = 4,
                uiOutput("outlier_sample_ui")
              )
            ),
            br(),
            uiOutput("outlier_explanation"),
            br(),
            DTOutput("outlier_table"),
            br(),
            downloadButton("download_outlier_table_xlsx", "Download Outlier Tabelle (XLSX)")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Residuenplot – Ct vs Fit (Outlier-Markierung)",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("outlier_residual_plot", height = "600px"),
            br(),
            downloadButton("download_outlier_plot_png", "Download Residuenplot (PNG)")
          )
        )
      ),
      
      ###################
      # Tab: Hilfe / Dokumentation
      ###################
      tabItem(
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
    )
  )
)



############################
# SERVER
############################

server <- function(input, output, session) {
  
  # Zentrale reaktive Werte:
  # - files_loaded: Upload-Phase erfolgreich
  # - data_loaded: Analyse-Daten für die aktuelle Auswahl existieren
  # - raw_qpcr_*: Rohdaten über alle geladenen Dateien
  # - qpcr_*: Daten nach Auswahl der Dateien (für Analysen)
  rv <- reactiveValues(
    files_loaded    = FALSE,
    data_loaded     = FALSE,
    raw_qpcr_all    = NULL,
    raw_qpcr_melt   = NULL,
    qpcr_all        = NULL,
    qpcr_summary    = NULL,
    qpcr_amp        = NULL,
    qpcr_melt       = NULL,
    file_overview   = NULL,
    available_files = NULL,
    has_delta_rn    = FALSE
  )
  
  ##########################
  # UI-Outputs: Dateiauswahl & Datei-Übersicht
  ##########################
  
  # Checkboxen + Analyse-Button für Dateiauswahl
  output$file_selection_ui <- renderUI({
    validate(
      need(rv$files_loaded, "Bitte zuerst qPCR-Dateien laden.")
    )
    req(rv$available_files)
    
    tagList(
      checkboxGroupInput(
        "selected_files",
        "Welche Dateien sollen in die Analyse einfließen?",
        choices  = rv$available_files,
        selected = rv$available_files
      ),
      actionButton("analysis_btn", "Analyse starten", icon = icon("play-circle")),
      helpText("Du kannst die Auswahl später ändern und die Analyse erneut starten.")
    )
  })
  
  # Übersicht pro Datei: Targets, Samples, Quantities
  output$file_overview_table <- DT::renderDT({
    req(rv$files_loaded)
    fo <- rv$file_overview
    validate(
      need(!is.null(fo) && nrow(fo) > 0, "Noch keine verwertbaren Dateien geladen.")
    )
    
    DT::datatable(
      fo,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX   = TRUE
      )
    )
  })
  
  ##########################
  # Daten laden – nur Upload & Datei-Übersicht
  ##########################
  
  observeEvent(input$load_btn, {
    req(input$xlsx_files)
    
    files_df <- input$xlsx_files
    
    withProgress(message = "Lade qPCR-Dateien...", value = 0, {
      n <- nrow(files_df)
      all_runs  <- list()
      all_melts <- list()
      
      for (i in seq_len(n)) {
        incProgress(1/n, detail = paste("Verarbeite:", files_df$name[i]))
        path <- files_df$datapath[i]
        name <- files_df$name[i]
        
        # Hauptdaten (Results + Amplification)
        run_i <- tryCatch(
          read_qpcr_file(path, source_name = name),
          error = function(e) {
            showNotification(
              paste("Fehler beim Lesen von", name, ":", e$message),
              type = "error",
              duration = 10
            )
            NULL
          }
        )
        if (!is.null(run_i) && nrow(run_i) > 0) {
          all_runs[[length(all_runs) + 1]] <- run_i
        }
        
        # Melt-Daten (optional)
        melt_i <- tryCatch(
          read_qpcr_melt_file(path, source_name = name),
          error = function(e) {
            showNotification(
              paste("Hinweis: Melt-Sheet in", name, "nicht lesbar:", e$message),
              type = "warning",
              duration = 8
            )
            NULL
          }
        )
        if (!is.null(melt_i) && nrow(melt_i) > 0) {
          all_melts[[length(all_melts) + 1]] <- melt_i
        }
      }
    })
    
    # Keine gültigen Runs?
    if (length(all_runs) == 0) {
      rv$files_loaded    <- FALSE
      rv$data_loaded     <- FALSE
      rv$raw_qpcr_all    <- NULL
      rv$raw_qpcr_melt   <- NULL
      rv$qpcr_all        <- NULL
      rv$qpcr_summary    <- NULL
      rv$qpcr_amp        <- NULL
      rv$qpcr_melt       <- NULL
      rv$file_overview   <- NULL
      rv$available_files <- NULL
      rv$has_delta_rn    <- FALSE
      
      output$load_status <- renderText(
        "Es konnten keine gültigen qPCR-Dateien geladen werden."
      )
      output$load_info <- renderUI(NULL)
      showNotification("Keine gültigen qPCR-Dateien geladen.", type = "error", duration = 10)
      updateTabItems(session, "tabs", "load")
      return(NULL)
    }
    
    # Rohdaten über alle geladenen Dateien
    raw_qpcr_all <- dplyr::bind_rows(all_runs)
    
    if (length(all_melts) == 0) {
      raw_qpcr_melt <- tibble()
    } else {
      raw_qpcr_melt <- dplyr::bind_rows(all_melts)
    }
    
    # Datei-Übersicht (Targets / Samples / Quantities) pro Datei
    file_overview <- raw_qpcr_all %>%
      group_by(source_file) %>%
      summarise(
        n_targets    = dplyr::n_distinct(`Target Name_res`),
        targets      = paste(sort(unique(`Target Name_res`)), collapse = ", "),
        n_samples    = dplyr::n_distinct(`Sample Name`),
        samples      = paste(sort(unique(`Sample Name`)), collapse = ", "),
        n_quantities = dplyr::n_distinct(Quantity),
        quantities   = paste(sort(unique(Quantity)), collapse = ", "),
        .groups      = "drop"
      )
    
    available_files <- sort(unique(raw_qpcr_all$source_file))
    
    # In rv speichern (Analyse wird erst bei "Analyse starten" erzeugt)
    rv$files_loaded    <- TRUE
    rv$data_loaded     <- FALSE
    rv$raw_qpcr_all    <- raw_qpcr_all
    rv$raw_qpcr_melt   <- raw_qpcr_melt
    rv$qpcr_all        <- NULL
    rv$qpcr_summary    <- NULL
    rv$qpcr_amp        <- NULL
    rv$qpcr_melt       <- NULL
    rv$file_overview   <- file_overview
    rv$available_files <- available_files
    rv$has_delta_rn    <- FALSE
    
    # Status / Info updaten
    output$load_status <- renderText(
      paste0(
        "Es wurden ", length(available_files), " Datei(en) erfolgreich geladen.\n",
        "Wähle unten die Dateien für die Analyse aus und klicke auf 'Analyse starten'."
      )
    )
    
    output$load_info <- renderUI({
      tagList(
        h4("Geladene Dateien"),
        tags$ul(lapply(available_files, function(fn) tags$li(fn)))
      )
    })
    
    # Auf der Load-Seite bleiben
    updateTabItems(session, "tabs", "load")
  })
  
  ##########################
  # Analyse starten – ausgewählte Dateien verarbeiten
  ##########################
  
  observeEvent(input$analysis_btn, {
    # 1) Sicherstellen, dass überhaupt Dateien geladen wurden
    if (!isTRUE(rv$files_loaded) || is.null(rv$raw_qpcr_all) || nrow(rv$raw_qpcr_all) == 0) {
      showNotification(
        "Bitte zuerst qPCR-Dateien mit 'Daten laden' einlesen.",
        type     = "warning",
        duration = 8
      )
      rv$data_loaded <- FALSE
      return(NULL)
    }
    
    # 2) Auswahl der Dateien prüfen
    selected <- input$selected_files
    if (is.null(selected) || length(selected) == 0) {
      showNotification(
        "Bitte mindestens eine Datei für die Analyse auswählen.",
        type     = "warning",
        duration = 8
      )
      rv$data_loaded <- FALSE
      return(NULL)
    }
    
    # 3) Rohdaten auf ausgewählte Dateien einschränken
    qpcr_all <- rv$raw_qpcr_all %>%
      dplyr::filter(source_file %in% selected)
    
    if (nrow(qpcr_all) == 0) {
      showNotification(
        "In den ausgewählten Dateien wurden keine gültigen Messwerte gefunden.",
        type     = "error",
        duration = 10
      )
      rv$data_loaded <- FALSE
      return(NULL)
    }
    
    # Melt-Daten auf Auswahl einschränken
    if (!is.null(rv$raw_qpcr_melt) && nrow(rv$raw_qpcr_melt) > 0) {
      qpcr_melt <- rv$raw_qpcr_melt %>%
        dplyr::filter(source_file %in% selected)
    } else {
      qpcr_melt <- tibble()
    }
    
    # 4) Ct-Spalte bestimmen (CRT / Crt Mean)
    ct_vec <- if ("CRT" %in% names(qpcr_all)) {
      suppressWarnings(as.numeric(qpcr_all$CRT))
    } else if ("Crt Mean" %in% names(qpcr_all)) {
      suppressWarnings(as.numeric(qpcr_all$`Crt Mean`))
    } else {
      showNotification(
        "Keine Ct-Spalte (CRT/Crt Mean) gefunden – Ct wird NA.",
        type     = "warning",
        duration = 8
      )
      rep(NA_real_, nrow(qpcr_all))
    }
    
    # 5) qpcr_summary aus ausgewählten Dateien berechnen
    qpcr_summary <- qpcr_all %>%
      mutate(
        Ct       = ct_vec,
        Quantity = suppressWarnings(as.numeric(Quantity)),
        Reporter = if ("Reporter" %in% names(.)) as.character(Reporter) else NA_character_
      ) %>%
      filter(
        !is.na(Quantity),
        !is.na(Ct),
        !is.na(`Target Name_res`),
        !is.na(`Sample Name`)
      ) %>%
      group_by(
        source_file,
        `Target Name_res`,
        Reporter,
        `Sample Name`,
        Quantity
      ) %>%
      summarise(
        Ct_mean = mean(Ct, na.rm = TRUE),
        Ct_sd   = sd(Ct,  na.rm = TRUE),
        n       = n(),
        .groups = "drop"
      ) %>%
      mutate(
        Target_ID = if_else(
          !is.na(Reporter),
          paste0(`Target Name_res`, " [", Reporter, "]"),
          `Target Name_res`
        )
      )
    
    # 6) Amplifikationsdaten für ausgewählte Dateien
    if (!("Cycle" %in% names(qpcr_all)) || !("Rn" %in% names(qpcr_all))) {
      showNotification(
        "Cycle oder Rn fehlen – Amplifikationskurven werden nicht angezeigt.",
        type     = "warning",
        duration = 8
      )
      qpcr_amp   <- tibble()
      has_delta_rn <- FALSE
    } else {
      delta_rn_colname <- dplyr::case_when(
        "Delta Rn" %in% names(qpcr_all) ~ "Delta Rn",
        "DeltaRn"  %in% names(qpcr_all) ~ "DeltaRn",
        "dRn"      %in% names(qpcr_all) ~ "dRn",
        TRUE                            ~ NA_character_
      )
      has_delta_rn <- !is.na(delta_rn_colname)
      
      qpcr_amp <- qpcr_all %>%
        mutate(
          Cycle    = suppressWarnings(as.numeric(Cycle)),
          Rn       = suppressWarnings(as.numeric(Rn)),
          DeltaRn  = if (has_delta_rn) suppressWarnings(as.numeric(.data[[delta_rn_colname]])) else NA_real_,
          Reporter = if ("Reporter" %in% names(.)) as.character(Reporter) else NA_character_,
          Target_ID = if_else(
            !is.na(Reporter),
            paste0(`Target Name_res`, " [", Reporter, "]"),
            `Target Name_res`
          )
        ) %>%
        filter(
          !is.na(Cycle),
          !is.na(Rn),
          !is.na(Target_ID),
          !is.na(`Sample Name`)
        )
    }
    
    # 7) Ct-Y-Achsen-Defaults aus qpcr_summary bestimmen
    if (nrow(qpcr_summary) > 0) {
      ct_min <- floor(min(qpcr_summary$Ct_mean, na.rm = TRUE))
      ct_max <- ceiling(max(qpcr_summary$Ct_mean, na.rm = TRUE))
      updateNumericInput(session, "ct_y_min", value = ct_min)
      updateNumericInput(session, "ct_y_max", value = ct_max)
    }
    
    # 8) Sidebar-Filter mit Targets/Samples aus den ausgewählten Dateien füllen
    all_targets <- qpcr_summary %>%
      distinct(Target_ID) %>%
      arrange(Target_ID) %>%
      pull()
    
    all_samples <- qpcr_summary %>%
      distinct(`Sample Name`) %>%
      arrange(`Sample Name`) %>%
      pull()
    
    updateCheckboxGroupInput(
      session,
      "target_filter",
      choices  = all_targets,
      selected = all_targets
    )
    updateCheckboxGroupInput(
      session,
      "sample_filter",
      choices  = all_samples,
      selected = all_samples
    )
    
    # 9) Analyse-Daten in rv speichern
    rv$qpcr_all     <- qpcr_all
    rv$qpcr_summary <- qpcr_summary
    rv$qpcr_amp     <- qpcr_amp
    rv$qpcr_melt    <- qpcr_melt
    rv$has_delta_rn <- has_delta_rn
    rv$data_loaded  <- TRUE
    
    # 10) Status-Meldung + Tab-Wechsel
    output$load_status <- renderText(
      paste0(
        "Analyse gestartet mit ", length(selected), " Datei(en).\n",
        "Targets: ", length(all_targets),
        " | Samples: ", length(all_samples)
      )
    )
    
    updateTabItems(session, "tabs", "ctqty")
  })
  
  ##########################
  # Gefilterte Daten
  ##########################
  
  filtered_summary <- reactive({
    validate(
      need(
        rv$data_loaded,
        "Bitte im Tab 'Daten laden' Dateien laden und dann 'Analyse starten' klicken."
      )
    )
    
    df <- rv$qpcr_summary
    if (is.null(df) || nrow(df) == 0) return(df[0, ])
    
    if (!is.null(input$target_filter) && length(input$target_filter) > 0) {
      df <- df %>% dplyr::filter(Target_ID %in% input$target_filter)
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$sample_filter) && length(input$sample_filter) > 0) {
      df <- df %>% dplyr::filter(`Sample Name` %in% input$sample_filter)
    } else {
      df <- df[0, ]
    }
    
    df
  })
  
  filtered_amp <- reactive({
    validate(
      need(
        rv$data_loaded,
        "Bitte im Tab 'Daten laden' Dateien laden und dann 'Analyse starten' klicken."
      )
    )
    
    df <- rv$qpcr_amp
    if (is.null(df) || nrow(df) == 0) return(tibble())
    
    if (!is.null(input$target_filter) && length(input$target_filter) > 0) {
      df <- df %>% dplyr::filter(Target_ID %in% input$target_filter)
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$sample_filter) && length(input$sample_filter) > 0) {
      df <- df %>% dplyr::filter(`Sample Name` %in% input$sample_filter)
    } else {
      df <- df[0, ]
    }
    
    df
  })
  
  filtered_melt <- reactive({
    validate(
      need(
        rv$data_loaded,
        "Bitte im Tab 'Daten laden' Dateien laden und dann 'Analyse starten' klicken."
      )
    )
    
    df <- rv$qpcr_melt
    if (is.null(df) || nrow(df) == 0) return(tibble())
    
    if (!is.null(input$target_filter) && length(input$target_filter) > 0) {
      df <- df %>% dplyr::filter(Target_ID %in% input$target_filter)
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$sample_filter) && length(input$sample_filter) > 0) {
      df <- df %>% dplyr::filter(`Sample Name` %in% input$sample_filter)
    } else {
      df <- df[0, ]
    }
    
    df
  })
  
  ##########################
  # Dynamische UI: Y-Achse Amplifikationskurven
  ##########################
  
  output$y_axis_ui <- renderUI({
    if (isTRUE(rv$has_delta_rn)) {
      radioButtons(
        inputId  = "y_axis",
        label    = "Y-Achse (Amplifikationskurven)",
        choices  = c("Rn" = "Rn", "Delta Rn" = "DeltaRn"),
        selected = "Rn"
      )
    } else {
      radioButtons(
        inputId  = "y_axis",
        label    = "Y-Achse (Amplifikationskurven)",
        choices  = c("Rn" = "Rn"),
        selected = "Rn"
      )
    }
  })
  
  ##########################
  # Ct vs Quantity – Plot
  ##########################
  
  output$qpcr_plot <- renderPlotly({
    validate(
      need(rv$data_loaded, "Bitte zunächst auf der Seite 'Daten laden' qPCR-Dateien laden.")
    )
    df <- filtered_summary()
    req(nrow(df) > 0)
    req(!is.null(input$ct_y_min), !is.null(input$ct_y_max))
    req(input$ct_y_min < input$ct_y_max)
    
    facet_scales <- input$y_scale_mode
    
    p <- ggplot(
      df,
      aes(
        x    = factor(Quantity),
        y    = Ct_mean,
        fill = `Sample Name`
      )
    ) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_errorbar(
        aes(ymin = Ct_mean - Ct_sd, ymax = Ct_mean + Ct_sd),
        position = position_dodge(width = 0.9),
        width = 0.3
      ) +
      facet_wrap(~ Target_ID, scales = facet_scales) +
      coord_cartesian(ylim = c(input$ct_y_min, input$ct_y_max)) +
      labs(
        x    = "Quantity",
        y    = "Ct (Mean ± SD)",
        fill = "Sample Name",
        title = "Ct (Mean ± SD) vs. Quantity je Target"
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  output$download_ct_plot_png <- downloadHandler(
    filename = function() {
      paste0("ct_vs_quantity_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- filtered_summary()
      facet_scales <- input$y_scale_mode
      
      p <- ggplot(
        df,
        aes(
          x    = factor(Quantity),
          y    = Ct_mean,
          fill = `Sample Name`
        )
      ) +
        geom_col(position = position_dodge(width = 0.9)) +
        geom_errorbar(
          aes(ymin = Ct_mean - Ct_sd, ymax = Ct_mean + Ct_sd),
          position = position_dodge(width = 0.9),
          width = 0.3
        ) +
        facet_wrap(~ Target_ID, scales = facet_scales) +
        coord_cartesian(ylim = c(input$ct_y_min, input$ct_y_max)) +
        labs(
          x    = "Quantity",
          y    = "Ct (Mean ± SD)",
          fill = "Sample Name",
          title = "Ct (Mean ± SD) vs. Quantity je Target"
        ) +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  ##########################
  # Ct-Tabelle (Ct vs Quantity)
  ##########################
  
  # Tabelle mit Ct-Mittelwerten und Standardabweichungen
  # - basiert auf filtered_summary(), also auf der aktuellen Auswahl
  #   von Dateien, Targets und Samples
  output$ct_table <- DT::renderDT({
    validate(
      need(rv$data_loaded, "Bitte im Tab 'Daten laden' Dateien laden und dann 'Analyse starten' klicken.")
    )
    df <- filtered_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten für die aktuelle Auswahl von Targets / Samples.")
    )
    
    # Ausgabe: eine Zeile pro Kombination aus Sample, Target, Reporter, Quantity
    out <- df %>%
      dplyr::transmute(
        Sample   = `Sample Name`,
        Target   = `Target Name_res`,
        Reporter = Reporter,
        Quantity = Quantity,
        `Ct Mean` = Ct_mean,
        `Ct SD`   = Ct_sd
      )
    
    DT::datatable(
      out,
      options = list(
        pageLength = 25,
        scrollX    = TRUE
      )
    )
  })
  
  # Download der Ct-Tabelle als XLSX-Datei
  output$download_ct_table_xlsx <- downloadHandler(
    filename = function() {
      paste0("ct_summary_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- filtered_summary()
      if (nrow(df) == 0) {
        # Fallback: leere Info-Tabelle
        write_xlsx(tibble(Hinweis = "Keine Daten für die aktuelle Auswahl."), path = file)
      } else {
        out <- df %>%
          dplyr::transmute(
            Sample   = `Sample Name`,
            Target   = `Target Name_res`,
            Reporter = Reporter,
            Quantity = Quantity,
            `Ct Mean` = Ct_mean,
            `Ct SD`   = Ct_sd
          )
        write_xlsx(out, path = file)
      }
    }
  )
  
  ##########################
  # Amplifikationskurven
  ##########################
  
  output$qpcr_curve_plot <- renderPlotly({
    validate(
      need(rv$data_loaded, "Bitte zunächst auf der Seite 'Daten laden' qPCR-Dateien laden.")
    )
    df <- filtered_amp()
    validate(
      need(nrow(df) > 0, "Keine Amplifikationsdaten vorhanden.")
    )
    
    y_axis <- input$y_axis
    req(y_axis)
    
    y_label <- if (y_axis == "Rn") "Rn" else "Delta Rn"
    
    p <- ggplot(
      df,
      aes(
        x    = Cycle,
        y    = if (y_axis == "Rn") Rn else DeltaRn,
        color = `Sample Name`,
        group = interaction(source_file, well_position)
      )
    ) +
      geom_line(alpha = 0.8) +
      facet_wrap(~ Target_ID, scales = input$y_scale_mode) +
      labs(
        x     = "Cycle",
        y     = y_label,
        color = "Sample Name",
        title = paste("Amplifikationskurven (Y:", y_label, ")")
      ) +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$download_amp_plot_png <- downloadHandler(
    filename = function() {
      paste0("amplification_curves_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- filtered_amp()
      y_axis <- input$y_axis
      y_label <- if (y_axis == "Rn") "Rn" else "Delta Rn"
      
      p <- ggplot(
        df,
        aes(
          x    = Cycle,
          y    = if (y_axis == "Rn") Rn else DeltaRn,
          color = `Sample Name`,
          group = interaction(source_file, well_position)
        )
      ) +
        geom_line(alpha = 0.8) +
        facet_wrap(~ Target_ID, scales = input$y_scale_mode) +
        labs(
          x     = "Cycle",
          y     = y_label,
          color = "Sample Name",
          title = paste("Amplifikationskurven (Y:", y_label, ")")
        ) +
        theme_bw() 
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  ##########################
  # Ct SD vs Quantity & Heatmap
  ##########################
  
  output$qpcr_sd_plot <- renderPlotly({
    df <- filtered_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten für Ct SD Plot.")
    )
    
    p <- ggplot(
      df,
      aes(
        x    = factor(Quantity),
        y    = Ct_sd,
        color = `Sample Name`,
        group = `Sample Name`
      )
    ) +
      geom_point(size = 2) +
      geom_line() +
      facet_wrap(~ Target_ID, scales = input$y_scale_mode) +
      labs(
        x     = "Quantity",
        y     = "Ct SD",
        color = "Sample Name",
        title = "Ct SD vs Quantity"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  output$download_ctsd_plot_png <- downloadHandler(
    filename = function() {
      paste0("ct_sd_vs_quantity_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- filtered_summary()
      
      p <- ggplot(
        df,
        aes(
          x    = factor(Quantity),
          y    = Ct_sd,
          color = `Sample Name`,
          group = `Sample Name`
        )
      ) +
        geom_point(size = 2) +
        geom_line() +
        facet_wrap(~ Target_ID, scales = input$y_scale_mode) +
        labs(
          x     = "Quantity",
          y     = "Ct SD",
          color = "Sample Name",
          title = "Ct SD vs Quantity"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  heatmap_data <- reactive({
    df <- filtered_summary()
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      group_by(`Sample Name`, Target_ID) %>%
      summarise(
        Ct_sd_mean = mean(Ct_sd, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$qpcr_sd_heatmap <- renderPlotly({
    df <- heatmap_data()
    validate(
      need(nrow(df) > 0, "Keine Daten für Ct SD Heatmap.")
    )
    
    p <- ggplot(
      df,
      aes(
        x = Target_ID,
        y = `Sample Name`,
        fill = Ct_sd_mean
      )
    ) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "plasma") +
      labs(
        x     = "Target [Reporter]",
        y     = "Sample Name",
        fill  = "Ct SD (Mean)",
        title = "Ct SD Heatmap (Sample × Target)"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  output$download_ctsd_heatmap_png <- downloadHandler(
    filename = function() {
      paste0("ct_sd_heatmap_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- heatmap_data()
      
      p <- ggplot(
        df,
        aes(
          x = Target_ID,
          y = `Sample Name`,
          fill = Ct_sd_mean
        )
      ) +
        geom_tile(color = "white") +
        scale_fill_viridis_c(option = "plasma") +
        labs(
          x     = "Target [Reporter]",
          y     = "Sample Name",
          fill  = "Ct SD (Mean)",
          title = "Ct SD Heatmap (Sample × Target)"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_ctsd_heatmap_xlsx <- downloadHandler(
    filename = function() {
      paste0("ct_sd_heatmap_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- heatmap_data()
      write_xlsx(df, path = file)
    }
  )
  
  ##########################
  # Schmelzkurven
  ##########################
  
  output$melt_curve_plot <- renderPlotly({
    df <- filtered_melt()
    validate(
      need(nrow(df) > 0, "Keine Schmelzkurvendaten vorhanden oder kein Melt-Sheet in den Dateien.")
    )
    
    y_axis <- input$melt_y_axis
    req(y_axis)
    
    y_var <- if (y_axis == "Derivative") "Derivative" else "Fluorescence"
    y_label <- if (y_axis == "Derivative") "-d(RFU)/dT" else "Fluorescence"
    
    p <- ggplot(
      df,
      aes(
        x = Temperature,
        y = .data[[y_var]],
        color = `Sample Name`,
        group = interaction(source_file, well_position)
      )
    ) +
      geom_line(alpha = 0.8) +
      facet_wrap(~ Target_ID, scales = input$y_scale_mode) +
      labs(
        x     = "Temperature (°C)",
        y     = y_label,
        color = "Sample Name",
        title = paste("Schmelzkurven (Y:", y_label, ")")
      ) +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$download_melt_plot_png <- downloadHandler(
    filename = function() {
      paste0("melt_curves_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- filtered_melt()
      y_axis <- input$melt_y_axis
      y_var  <- if (y_axis == "Derivative") "Derivative" else "Fluorescence"
      y_label <- if (y_axis == "Derivative") "-d(RFU)/dT" else "Fluorescence"
      
      p <- ggplot(
        df,
        aes(
          x = Temperature,
          y = .data[[y_var]],
          color = `Sample Name`,
          group = interaction(source_file, well_position)
        )
      ) +
        geom_line(alpha = 0.8) +
        facet_wrap(~ Target_ID, scales = input$y_scale_mode) +
        labs(
          x     = "Temperature (°C)",
          y     = y_label,
          color = "Sample Name",
          title = paste("Schmelzkurven (Y:", y_label, ")")
        ) +
        theme_bw()
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  ##########################
  # Schmelzkurven – Peak-Analyse
  ##########################
  
  # Peaks pro Well (lokale Maxima in der Derivative)
  melt_peaks <- reactive({
    df <- filtered_melt()
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      arrange(source_file, Target_ID, `Sample Name`, well_position, Temperature) %>%
      group_by(source_file, Target_ID, `Sample Name`, well_position) %>%
      mutate(
        prev_deriv = dplyr::lag(Derivative),
        next_deriv = dplyr::lead(Derivative)
      ) %>%
      filter(
        !is.na(Derivative),
        !is.na(prev_deriv),
        !is.na(next_deriv),
        Derivative >= prev_deriv,
        Derivative >= next_deriv
      ) %>%
      summarise(
        peak_temp  = Temperature[which.max(Derivative)],
        peak_value = max(Derivative, na.rm = TRUE),
        .groups    = "drop"
      )
  })
  
  output$melt_peaks_table <- DT::renderDT({
    df <- melt_peaks()
    validate(
      need(nrow(df) > 0, "Keine Peaks gefunden (oder keine Schmelzkurven vorhanden).")
    )
    
    DT::datatable(
      df,
      options = list(
        pageLength = 20,
        scrollX    = TRUE
      )
    )
  })
  
  output$download_melt_peaks_xlsx <- downloadHandler(
    filename = function() {
      paste0("melt_peaks_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- melt_peaks()
      write_xlsx(df, path = file)
    }
  )
  
  # Zusammenfassung: Mehrere Peaks pro Well etc.
  melt_peak_summary <- reactive({
    df <- melt_peaks()
    if (nrow(df) == 0) return(tibble())
    
    df %>%
      group_by(source_file, Target_ID, `Sample Name`) %>%
      summarise(
        n_wells         = n_distinct(well_position),
        n_peaks         = n(),
        max_peaks_well  = max(table(well_position)),
        wells_multipeak = sum(table(well_position) > 1),
        any_multipeak   = wells_multipeak > 0,
        .groups         = "drop"
      )
  })
  
  output$melt_peak_summary_table <- DT::renderDT({
    df <- melt_peak_summary()
    validate(
      need(nrow(df) > 0, "Keine Peak-Summary verfügbar.")
    )
    
    DT::datatable(
      df,
      options = list(
        pageLength = 20,
        scrollX    = TRUE
      )
    )
  })
  
  output$download_melt_peaks_summary_xlsx <- downloadHandler(
    filename = function() {
      paste0("melt_peak_summary_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- melt_peak_summary()
      write_xlsx(df, path = file)
    }
  )
  
  ##########################
  # Standardkurven
  ##########################
  
  # Hilfsfunktion: linearen Dynamikbereich (LDR) finden
  find_linear_range <- function(df_group) {
    # df_group: Daten einer Kombination aus Target_ID/Sample über verschiedene Quantities
    df_group <- df_group %>%
      arrange(logQ)
    
    n <- nrow(df_group)
    if (n < 3) {
      return(NULL)
    }
    
    best_r2 <- -Inf
    best_fit <- NULL
    best_idx <- c(1, n)
    
    # Alle möglichen Fenster der Länge >= 3 durchprobieren
    for (start_idx in 1:(n - 2)) {
      for (end_idx in (start_idx + 2):n) {
        sub <- df_group[start_idx:end_idx, ]
        fit <- lm(Ct_mean ~ logQ, data = sub)
        r2  <- summary(fit)$r.squared
        
        if (!is.na(r2) && r2 > best_r2) {
          best_r2  <- r2
          best_fit <- fit
          best_idx <- c(start_idx, end_idx)
        }
      }
    }
    
    if (is.null(best_fit) || best_r2 < 0.98) {
      return(NULL)
    }
    
    sub <- df_group[best_idx[1]:best_idx[2], ]
    slope     <- coef(best_fit)[["logQ"]]
    intercept <- coef(best_fit)[["(Intercept)"]]
    
    efficiency <- (10^(-1 / slope) - 1) * 100
    
    tibble(
      logQ_min  = min(sub$logQ),
      logQ_max  = max(sub$logQ),
      Q_min     = min(sub$Quantity),
      Q_max     = max(sub$Quantity),
      n_points  = nrow(sub),
      slope     = slope,
      intercept = intercept,
      r2        = best_r2,
      efficiency = efficiency
    )
  }
  
  standardcurve_data <- reactive({
    df <- filtered_summary()
    if (nrow(df) == 0) return(tibble())
    
    df <- df %>%
      filter(Quantity > 0) %>%
      mutate(
        logQ = log10(Quantity)
      )
    
    df %>%
      group_by(source_file, Target_ID, `Target Name_res`, Reporter, `Sample Name`) %>%
      group_modify(~ {
        res <- find_linear_range(.x)
        if (is.null(res)) {
          tibble(
            logQ_min  = NA_real_,
            logQ_max  = NA_real_,
            Q_min     = NA_real_,
            Q_max     = NA_real_,
            n_points  = nrow(.x),
            slope     = NA_real_,
            intercept = NA_real_,
            r2        = NA_real_,
            efficiency = NA_real_
          )
        } else {
          res
        }
      }) %>%
      ungroup()
  })
  
  output$standardcurve_table <- DT::renderDT({
    df <- standardcurve_data()
    validate(
      need(nrow(df) > 0, "Keine Standardkurven-Daten.")
    )
    
    out <- df %>%
      mutate(
        slope      = round(slope, 3),
        intercept  = round(intercept, 3),
        r2         = round(r2, 4),
        efficiency = round(efficiency, 1)
      )
    
    DT::datatable(
      out,
      options = list(
        pageLength = 25,
        scrollX    = TRUE
      )
    )
  })
  
  output$download_stdcurves_xlsx <- downloadHandler(
    filename = function() {
      paste0("standardcurves_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- standardcurve_data()
      write_xlsx(df, path = file)
    }
  )
  
  output$stdcurve_slope_plot <- renderPlotly({
    df <- standardcurve_data()
    validate(
      need(nrow(df) > 0, "Keine Standardkurven-Daten für Slope-Plot.")
    )
    
    p <- ggplot(
      df,
      aes(
        x    = `Sample Name`,
        y    = slope,
        fill = `Sample Name`
      )
    ) +
      geom_col() +
      facet_wrap(~ Target_ID) +
      labs(
        x     = "Sample",
        y     = "Slope",
        fill  = "Sample",
        title = "Steigungen der Standardkurven"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  output$download_stdcurve_slope_png <- downloadHandler(
    filename = function() {
      paste0("standardcurve_slopes_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- standardcurve_data()
      
      p <- ggplot(
        df,
        aes(
          x    = `Sample Name`,
          y    = slope,
          fill = `Sample Name`
        )
      ) +
        geom_col() +
        facet_wrap(~ Target_ID) +
        labs(
          x     = "Sample",
          y     = "Slope",
          fill  = "Sample",
          title = "Steigungen der Standardkurven"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$stdcurve_eff_plot <- renderPlotly({
    df <- standardcurve_data()
    validate(
      need(nrow(df) > 0, "Keine Standardkurven-Daten für Effizienz-Plot.")
    )
    
    p <- ggplot(
      df,
      aes(
        x    = `Sample Name`,
        y    = efficiency,
        fill = `Sample Name`
      )
    ) +
      geom_col() +
      facet_wrap(~ Target_ID) +
      labs(
        x     = "Sample",
        y     = "Effizienz (%)",
        fill  = "Sample",
        title = "PCR-Effizienz je Sample/Target"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  output$download_stdcurve_eff_png <- downloadHandler(
    filename = function() {
      paste0("standardcurve_efficiency_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- standardcurve_data()
      
      p <- ggplot(
        df,
        aes(
          x    = `Sample Name`,
          y    = efficiency,
          fill = `Sample Name`
        )
      ) +
        geom_col() +
        facet_wrap(~ Target_ID) +
        labs(
          x     = "Sample",
          y     = "Effizienz (%)",
          fill  = "Sample",
          title = "PCR-Effizienz je Sample/Target"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  # Scatterplot: Ct_mean ~ log10(Quantity) je Target_ID, alle ausgewählten Samples
  output$stdcurve_scatter_plot <- renderPlotly({
    df <- filtered_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten für Standardkurven-Scatterplot.")
    )
    
    df <- df %>%
      filter(Quantity > 0) %>%
      mutate(
        logQ = log10(Quantity)
      )
    
    req(input$std_scatter_target)
    df <- df %>%
      filter(Target_ID == input$std_scatter_target)
    validate(
      need(nrow(df) > 0, "Keine Daten für das ausgewählte Target im Scatterplot.")
    )
    
    p <- ggplot(
      df,
      aes(
        x    = logQ,
        y    = Ct_mean,
        color = `Sample Name`
      )
    ) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      geom_segment(
        aes(
          xend = logQ,
          yend = predict(lm(Ct_mean ~ logQ, data = df))
        ),
        alpha = 0.3
      ) +
      labs(
        x     = "log10(Quantity)",
        y     = "Ct (Mean)",
        color = "Sample Name",
        title = paste("Standardkurven Scatterplot –", input$std_scatter_target)
      ) +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$download_stdcurve_scatter_png <- downloadHandler(
    filename = function() {
      paste0("standardcurve_scatter_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- filtered_summary()
      df <- df %>%
        filter(Quantity > 0) %>%
        mutate(
          logQ = log10(Quantity)
        )
      req(input$std_scatter_target)
      df <- df %>%
        filter(Target_ID == input$std_scatter_target)
      
      p <- ggplot(
        df,
        aes(
          x    = logQ,
          y    = Ct_mean,
          color = `Sample Name`
        )
      ) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = FALSE) +
        geom_segment(
          aes(
            xend = logQ,
            yend = predict(lm(Ct_mean ~ logQ, data = df))
          ),
          alpha = 0.3
        ) +
        labs(
          x     = "log10(Quantity)",
          y     = "Ct (Mean)",
          color = "Sample Name",
          title = paste("Standardkurven Scatterplot –", input$std_scatter_target)
        ) +
        theme_bw() 
    
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  ##########################
  # Outlier Tests – Ct pro Well
  ##########################
  
  # Outlier-Daten: Ct pro Well (nicht Ct_mean), inklusive aller Wells über alle Quantities
  outlier_data <- reactive({
    validate(
      need(rv$data_loaded, "Bitte im Tab 'Daten laden' Dateien laden und 'Analyse starten' klicken.")
    )
    req(input$outlier_target, input$outlier_sample)
    req(rv$qpcr_all)
    
    # Ct pro Well aus den Rohdaten (nicht aus qpcr_summary)
    df <- rv$qpcr_all %>%
      mutate(
        Ct = dplyr::case_when(
          "CRT" %in% names(.)      ~ suppressWarnings(as.numeric(CRT)),
          "Crt Mean" %in% names(.) ~ suppressWarnings(as.numeric(`Crt Mean`)),
          TRUE                     ~ NA_real_
        ),
        Quantity = suppressWarnings(as.numeric(Quantity)),
        Reporter = if ("Reporter" %in% names(.)) as.character(Reporter) else NA_character_
      ) %>%
      mutate(
        Target_ID = if_else(
          !is.na(Reporter),
          paste0(`Target Name_res`, " [", Reporter, "]"),
          `Target Name_res`
        )
      ) %>%
      filter(
        Target_ID        == input$outlier_target,
        `Sample Name`    == input$outlier_sample,
        !is.na(Ct),
        !is.na(Quantity),
        Quantity > 0
      ) %>%
      group_by(source_file, Target_ID, `Sample Name`, Quantity, well_position) %>%
      summarise(
        Ct = mean(Ct, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        logQ = log10(Quantity)
      ) %>%
      arrange(Quantity, well_position)
    
    validate(
      need(nrow(df) >= 3, "Zu wenige Ct-Werte für ausgewähltes Target/Sample (mind. 3 Wells erforderlich).")
    )
    
    # Linearer Fit Ct ~ log10(Quantity) über alle Wells
    fit <- lm(Ct ~ logQ, data = df)
    
    df <- df %>%
      mutate(
        fit_value = predict(fit, newdata = df),
        resid     = Ct - fit_value
      )
    
    df
  })
  
  # Lauf eines Outlier-Tests auf einem Vektor von Residuen
  run_outlier_test <- function(x, method) {
    x <- x[is.finite(x)]
    n <- length(x)
    if (n < 3) {
      return(rep(FALSE, length(x)))
    }
    
    if (method == "Dixon") {
      res <- tryCatch(
        DixonTest(x),
        error = function(e) NULL
      )
      if (is.null(res)) return(rep(FALSE, length(x)))
      idx <- as.numeric(res$statistic["index"])
      flags <- rep(FALSE, length(x))
      if (!is.na(idx) && idx >= 1 && idx <= length(x)) {
        flags[idx] <- TRUE
      }
      return(flags)
    }
    
    if (method == "Grubbs") {
      res <- tryCatch(
        grubbs.test(x),
        error = function(e) NULL
      )
      if (is.null(res)) return(rep(FALSE, length(x)))
      idx <- which.max(abs(x - mean(x, na.rm = TRUE)))
      flags <- rep(FALSE, length(x))
      if (!is.na(idx)) {
        flags[idx] <- TRUE
      }
      return(flags)
    }
    
    if (method == "Rosner") {
      k <- min(3, n - 2)
      res <- tryCatch(
        rosnerTest(x, k = k),
        error = function(e) NULL
      )
      if (is.null(res)) return(rep(FALSE, length(x)))
      out <- res$all.stats
      flags <- rep(FALSE, length(x))
      if (!is.null(out$Outlier)) {
        flags[out$Obs.Num[out$Outlier]] <- TRUE
      }
      return(flags)
    }
    
    rep(FALSE, length(x))
  }
  
  # UI-Erklärung für Outlier-Tests
  output$outlier_explanation <- renderUI({
    validate(
      need(rv$data_loaded, "Bitte im Tab 'Daten laden' Dateien laden und 'Analyse starten' klicken.")
    )
    
    method <- input$outlier_test
    if (is.null(method)) return(NULL)
    
    text_method <- switch(
      method,
      "Dixon" = tags$div(
        tags$strong("Dixon-Test (Q-Test)"),
        tags$p(
          "Wird für kleine Stichproben (ca. 3–25 Werte) verwendet. ",
          "Es werden das kleinste und größte Residuum mit den übrigen Residuen verglichen. ",
          "Wenn eines dieser extremen Residuen im Verhältnis zur inneren Spannweite zu groß ist ",
          "(p < 0,05), wird der zugehörige Ct-Wert als Ausreißer markiert."
        )
      ),
      "Grubbs" = tags$div(
        tags$strong("Grubbs-Test"),
        tags$p(
          "Testet, ob genau ein Wert in der Stichprobe signifikant von den übrigen abweicht. ",
          "Es wird der Wert mit der größten Abweichung vom Mittelwert der Residuen untersucht. ",
          "Ist dieser Wert statistisch auffällig (p < 0,05), wird genau ein Ct-Wert als Ausreißer markiert."
        )
      ),
      "Rosner" = tags$div(
        tags$strong("Rosner-Test (Generalized ESD)"),
        tags$p(
          "Erlaubt die Detektion mehrerer Ausreißer gleichzeitig. ",
          "Es werden iterativ bis zu k potentielle Ausreißer getestet (hier maximal 3 Werte). ",
          "Alle Ct-Werte, deren Residuen in diesem Test als Ausreißer gekennzeichnet werden ",
          "(p < 0,05), werden entsprechend markiert."
        )
      )
    )
    
    tags$div(
      style = "background-color:#f8f9fa; border-radius:8px; padding:10px; border:1px solid #ddd;",
      tags$strong("Was wird hier getestet?"),
      tags$p(
        "Für das ausgewählte Target/Sample wird zunächst ein linearer Fit ",
        tags$code("Ct ~ log10(Quantity)"),
        " über alle Wells durchgeführt."
      ),
      tags$p(
        "Für jedes Well wird das Residuum berechnet: ",
        tags$code("Residuum = beobachtetes Ct - vorhergesagtes Ct (aus dem Fit)"),
        ". ",
        "Die Outlier-Tests arbeiten auf diesen Residuen."
      ),
      text_method,
      tags$hr(),
      tags$p(
        tags$strong("Wann ist ein Ct-Wert eines Wells ein Outlier?"),
        tags$br(),
        "Ein Ct-Wert eines bestimmten Wells gilt hier als Ausreißer, wenn ",
        "sein Residuum in dem gewählten Test als statistisch auffällig eingestuft wird ",
        "(Signifikanzniveau p < 0,05). ",
        "Solche Wells werden in der Tabelle in der Spalte ",
        tags$code("is_outlier"),
        " mit ", tags$code("TRUE"), " gekennzeichnet und im Residuenplot ",
        "rot hervorgehoben."
      ),
      tags$p(
        "Hinweis: Alle Tests setzen ungefähr normalverteilte Residuen und unabhängige Wells voraus. ",
        "Bei systematischen Fehlern (z. B. Pipettierfehler in einer kompletten Verdünnungsreihe) ",
        "können die Tests diese Werte unter Umständen nicht als Ausreißer erkennen."
      )
    )
  })
  
  output$outlier_table <- renderDT({
    df <- outlier_data()
    v <- df$resid
    flags <- run_outlier_test(v, method = input$outlier_test)
    
    out <- df %>%
      mutate(
        residual   = resid,
        is_outlier = flags
      ) %>%
      select(
        source_file,
        Target_ID,
        `Sample Name`,
        well_position,
        Quantity,
        Ct,
        residual,
        is_outlier
      )
    
    datatable(
      out,
      options = list(
        pageLength   = 20,
        orderClasses = TRUE,
        autoWidth    = TRUE
      )
    )
  })
  
  output$download_outlier_table_xlsx <- downloadHandler(
    filename = function() {
      paste0("outliers_", input$outlier_target, "_", input$outlier_sample, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- outlier_data()
      v <- df$resid
      flags <- run_outlier_test(v, method = input$outlier_test)
      
      out <- df %>%
        mutate(
          residual   = resid,
          is_outlier = flags
        ) %>%
        select(
          source_file,
          Target_ID,
          `Sample Name`,
          well_position,
          Quantity,
          Ct,
          residual,
          is_outlier
        )
      
      write_xlsx(out, path = file)
    }
  )
  
  output$outlier_residual_plot <- renderPlotly({
    df <- outlier_data()
    v <- df$resid
    flags <- run_outlier_test(v, method = input$outlier_test)
    
    df <- df %>%
      mutate(is_outlier = flags)
    
    p <- ggplot(
      df,
      aes(
        x     = logQ,
        y     = resid,
        color = is_outlier
      )
    ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(size = 3) +
      scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black")) +
      labs(
        x     = "log10(Quantity)",
        y     = "Residual (Ct - Fit)",
        color = "Outlier",
        title = paste(
          "Residuenplot –",
          input$outlier_target, "/",
          input$outlier_sample, "(",
          input$outlier_test, ")"
        )
      ) +
      theme_bw() 
    
    ggplotly(p)
  })
  
  output$download_outlier_plot_png <- downloadHandler(
    filename = function() {
      paste0("outlier_residuals_", input$outlier_target, "_", input$outlier_sample, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- outlier_data()
      v <- df$resid
      flags <- run_outlier_test(v, method = input$outlier_test)
      
      df <- df %>%
        mutate(is_outlier = flags)
      
      p <- ggplot(
        df,
        aes(
          x     = logQ,
          y     = resid,
          color = is_outlier
        )
      ) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_point(size = 3) +
        scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black")) +
        labs(
          x     = "log10(Quantity)",
          y     = "Residual (Ct - Fit)",
          color = "Outlier",
          title = paste(
            "Residuenplot –",
            input$outlier_target, "/",
            input$outlier_sample, "(",
            input$outlier_test, ")"
          )
        ) +
        theme_bw() 
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
  )
  
  ##########################
  # Outlier UI Inputs (Targets & Samples)
  ##########################
  
  output$outlier_target_ui <- renderUI({
    validate(
      need(rv$data_loaded, "")
    )
    df <- rv$qpcr_summary
    if (is.null(df) || nrow(df) == 0) {
      return(selectInput("outlier_target", "Target_ID (Outlier)", choices = NULL))
    }
    targets <- df %>%
      distinct(Target_ID) %>%
      arrange(Target_ID) %>%
      pull()
    selectInput(
      "outlier_target",
      "Target_ID (Outlier)",
      choices  = targets,
      selected = targets[1]
    )
  })
  
  output$outlier_sample_ui <- renderUI({
    validate(
      need(rv$data_loaded, "")
    )
    df <- rv$qpcr_summary
    if (is.null(df) || nrow(df) == 0) {
      return(selectInput("outlier_sample", "Sample (Outlier)", choices = NULL))
    }
    samples <- df %>%
      distinct(`Sample Name`) %>%
      arrange(`Sample Name`) %>%
      pull()
    selectInput(
      "outlier_sample",
      "Sample (Outlier)",
      choices  = samples,
      selected = samples[1]
    )
  })
  
  ##########################
  # Standardkurven-Target-Auswahl für Scatterplots
  ##########################
  
  observe({
    if (!rv$data_loaded || is.null(rv$qpcr_summary) || nrow(rv$qpcr_summary) == 0) {
      updateSelectInput(session, "std_scatter_target", choices = character(0))
    } else {
      targets <- rv$qpcr_summary %>%
        distinct(Target_ID) %>%
        arrange(Target_ID) %>%
        pull()
      updateSelectInput(session, "std_scatter_target", choices = targets, selected = targets[1])
    }
  })
  
}

############################
# App starten
############################

shinyApp(ui = ui, server = server)
