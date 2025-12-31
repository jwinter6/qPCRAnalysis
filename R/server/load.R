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
