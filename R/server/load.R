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
  
  observeEvent(input$add_report_file_overview_table, {
    withProgress(message = "Fuege Tabelle zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Daten aufbereiten")
      fo <- rv$file_overview
      if (is.null(fo)) fo <- tibble()
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Datei-Uebersicht",
        tab = "Daten laden",
        type = "table",
        data = fo
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Tabelle zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  ##########################
  # Daten laden – nur Upload & Datei-Übersicht
  ##########################
  
  observeEvent(input$load_btn, {
    req(input$xlsx_files)
    
    files_df <- input$xlsx_files
    
    withProgress(message = "Dateien werden geladen", value = 0, {
      n <- nrow(files_df)
      all_runs  <- list()
      all_melts <- list()
      
      for (i in seq_len(n)) {
        incProgress(1/n, detail = paste("Lese Datei:", files_df$name[i]))
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
      rv$quantity_missing_any <- FALSE
      rv$quantity_missing_all <- FALSE
      rv$analysis_master <- NULL
      rv$analysis_context_label <- NULL
      
      output$load_status <- renderText(
        "Es konnten keine gültigen qPCR-Dateien geladen werden."
      )
      output$load_info <- renderUI(NULL)
      showNotification("Keine gültigen qPCR-Dateien geladen.", type = "error", duration = 10)
      updateTabsetPanel(session, "tabs", selected = "load")
      return(NULL)
    }
    
    # Rohdaten über alle geladenen Dateien
    raw_qpcr_all <- dplyr::bind_rows(all_runs)
    
    if (length(all_melts) == 0) {
      raw_qpcr_melt <- tibble()
    } else {
      raw_qpcr_melt <- dplyr::bind_rows(all_melts)
    }
    
    melt_counts <- if (nrow(raw_qpcr_melt) > 0) {
      raw_qpcr_melt %>%
        count(source_file, name = "n_melt_points")
    } else {
      tibble(source_file = character(), n_melt_points = integer())
    }

    run_quantity_status <- raw_qpcr_all %>%
      mutate(
        run_id_norm = if ("run_id" %in% names(raw_qpcr_all)) {
          dplyr::if_else(
            !is.na(run_id) & nzchar(as.character(run_id)),
            as.character(run_id),
            NA_character_
          )
        } else {
          NA_character_
        },
        experiment_id_norm = if ("experiment_id" %in% names(raw_qpcr_all)) {
          dplyr::if_else(
            !is.na(experiment_id) & nzchar(as.character(experiment_id)),
            as.character(experiment_id),
            NA_character_
          )
        } else {
          NA_character_
        },
        run_key = dplyr::case_when(
          !is.na(run_id_norm) & !is.na(experiment_id_norm) ~ paste0(experiment_id_norm, "::", run_id_norm),
          !is.na(run_id_norm) ~ run_id_norm,
          TRUE ~ "(Datei-Run)"
        ),
        quantity_num = suppressWarnings(as.numeric(Quantity))
      ) %>%
      group_by(source_file, run_key) %>%
      summarise(
        has_quantity = any(!is.na(quantity_num)),
        .groups = "drop"
      ) %>%
      group_by(source_file) %>%
      summarise(
        n_runs = n(),
        n_runs_without_quantity = sum(!has_quantity),
        runs_without_quantity = if_else(
          n_runs_without_quantity > 0,
          paste(sort(run_key[!has_quantity]), collapse = ", "),
          "-"
        ),
        .groups = "drop"
      )

    # Datei-Übersicht (Targets / Samples / Quantities) pro Datei
    file_overview <- raw_qpcr_all %>%
      group_by(source_file) %>%
      summarise(
        n_targets    = dplyr::n_distinct(`Target Name_res`),
        targets      = paste(sort(unique(`Target Name_res`)), collapse = ", "),
        n_samples    = dplyr::n_distinct(`Sample Name`),
        samples      = paste(sort(unique(`Sample Name`)), collapse = ", "),
        n_quantities = dplyr::n_distinct(suppressWarnings(as.numeric(Quantity)), na.rm = TRUE),
        quantities   = {
          q <- suppressWarnings(as.numeric(Quantity))
          q <- sort(unique(q[!is.na(q)]))
          if (length(q) == 0) "-" else paste(q, collapse = ", ")
        },
        .groups      = "drop"
      ) %>%
      mutate(
        file_type = case_when(
          grepl("\\.rdml$", tolower(source_file)) ~ "RDML",
          grepl("\\.xml$", tolower(source_file))  ~ "RDML",
          grepl("\\.xlsx$", tolower(source_file)) ~ "XLSX",
          TRUE ~ "Unbekannt"
        )
      ) %>%
      left_join(melt_counts, by = "source_file") %>%
      left_join(run_quantity_status, by = "source_file") %>%
      mutate(
        n_melt_points = tidyr::replace_na(n_melt_points, 0L),
        n_runs = tidyr::replace_na(n_runs, 1L),
        n_runs_without_quantity = tidyr::replace_na(n_runs_without_quantity, 0L),
        runs_without_quantity = tidyr::replace_na(runs_without_quantity, "-"),
        rdml_melt_status = case_when(
          file_type == "RDML" & n_melt_points > 0 ~ "vorhanden",
          file_type == "RDML" ~ "nicht vorhanden",
          TRUE ~ "n/a"
        ),
        quantity_run_status = case_when(
          n_runs_without_quantity > 0 ~ paste0(
            "Quantity fehlt in ",
            n_runs_without_quantity,
            "/",
            n_runs,
            " Run(s): ",
            runs_without_quantity
          ),
          TRUE ~ paste0("Quantity in allen ", n_runs, " Run(s) vorhanden")
        )
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
    rv$quantity_missing_any <- FALSE
    rv$quantity_missing_all <- FALSE
    rv$analysis_master <- NULL
    rv$analysis_context_label <- NULL
    
    # Status / Info updaten
    output$load_status <- renderText(
      paste0(
        "Es wurden ", length(available_files), " Datei(en) erfolgreich geladen.\n",
        "Wähle unten die Dateien für die Analyse aus und klicke auf 'Analyse starten'."
      )
    )
    
    output$load_info <- renderUI({
      fo <- file_overview
      tagList(
        h4("Geladene Dateien"),
        tags$ul(lapply(available_files, function(fn) {
          row <- fo %>%
            dplyr::filter(source_file == fn) %>%
            dplyr::slice(1)

          if (nrow(row) == 0) {
            return(tags$li(fn))
          }

          quantity_text <- row$quantity_run_status[[1]]

          if (identical(row$file_type[[1]], "RDML")) {
            return(
              tags$li(
                paste0(
                  fn,
                  " [RDML] - Melt-Daten: ",
                  row$rdml_melt_status[[1]],
                  " (",
                  row$n_melt_points[[1]],
                  " Punkte); ",
                  quantity_text
                )
              )
            )
          }

          tags$li(paste0(fn, " [", row$file_type[[1]], "] - ", quantity_text))
        }))
      )
    })
    
    # Auf der Load-Seite bleiben
    updateTabsetPanel(session, "tabs", selected = "load")
  })
  
  ##########################
  # Analyse starten – ausgewählte Dateien verarbeiten
  ##########################
  
  observeEvent(input$analysis_btn, {
    withProgress(message = "Analyse wird gestartet", value = 0, {
      incProgress(0.05, detail = "Pruefe Dateiverfuegbarkeit")
    # 1) Sicherstellen, dass überhaupt Dateien geladen wurden
    if (!isTRUE(rv$files_loaded) || is.null(rv$raw_qpcr_all) || nrow(rv$raw_qpcr_all) == 0) {
      showNotification(
        "Bitte zuerst qPCR-Dateien mit 'Daten laden' einlesen.",
        type     = "warning",
        duration = 8
      )
      rv$data_loaded <- FALSE
      rv$analysis_master <- NULL
      rv$analysis_context_label <- NULL
      return(NULL)
    }
    
    # 2) Auswahl der Dateien prüfen
    incProgress(0.05, detail = "Pruefe Dateiauswahl")
    selected <- input$selected_files
    if (is.null(selected) || length(selected) == 0) {
      showNotification(
        "Bitte mindestens eine Datei für die Analyse auswählen.",
        type     = "warning",
        duration = 8
      )
      rv$data_loaded <- FALSE
      rv$analysis_master <- NULL
      rv$analysis_context_label <- NULL
      return(NULL)
    }
    
    # 3) Rohdaten auf ausgewählte Dateien einschränken
    incProgress(0.1, detail = "Filtere Rohdaten")
    qpcr_all <- rv$raw_qpcr_all %>%
      dplyr::filter(source_file %in% selected)
    
    if (nrow(qpcr_all) == 0) {
      showNotification(
        "In den ausgewählten Dateien wurden keine gültigen Messwerte gefunden.",
        type     = "error",
        duration = 10
      )
      rv$data_loaded <- FALSE
      rv$analysis_master <- NULL
      rv$analysis_context_label <- NULL
      return(NULL)
    }
    
    # Melt-Daten auf Auswahl einschränken
    incProgress(0.1, detail = "Bereite Melt-Daten vor")
    if (!is.null(rv$raw_qpcr_melt) && nrow(rv$raw_qpcr_melt) > 0) {
      qpcr_melt <- rv$raw_qpcr_melt %>%
        dplyr::filter(source_file %in% selected)
    } else {
      qpcr_melt <- tibble()
    }
    
    # 4) Ct-Spalte bestimmen (CRT / CT / Crt Mean)
    incProgress(0.1, detail = "Bestimme Ct-Spalte")
    ct_vec <- if ("CRT" %in% names(qpcr_all)) {
      suppressWarnings(as.numeric(qpcr_all$CRT))
    } else if ("CT" %in% names(qpcr_all)) {
      suppressWarnings(as.numeric(qpcr_all$CT))
    } else if ("Crt Mean" %in% names(qpcr_all)) {
      suppressWarnings(as.numeric(qpcr_all$`Crt Mean`))
    } else {
      showNotification(
        "Keine Ct-Spalte (CRT/CT/Crt Mean) gefunden – Ct wird NA.",
        type     = "warning",
        duration = 8
      )
      rep(NA_real_, nrow(qpcr_all))
    }
    
    # 5) Quantity normalisieren (fehlende Werte bleiben NA) + Hinweis
    quantity_vec <- suppressWarnings(as.numeric(qpcr_all$Quantity))
    quantity_missing_any <- any(is.na(quantity_vec))
    quantity_missing_all <- all(is.na(quantity_vec))
    
    if (isTRUE(quantity_missing_all)) {
      showModal(
        modalDialog(
          title = "Warnung: Quantity fehlt",
          tags$div(
            style = "color:#721c24; background-color:#f8d7da; padding:12px; border:1px solid #f5c6cb; border-radius:6px;",
            tags$p(
              "In den ausgewaehlten Dateien fehlt die Spalte 'Quantity'. ",
              "Eine Vergleichbarkeit mit Daten, die eine Quantity besitzen, ist daher nicht gegeben."
            ),
            tags$p(
              "Fehlende Quantity-Werte bleiben leer und werden in quantity-basierten Auswertungen ausgeschlossen ",
              "(z. B. Ct vs Quantity, Ct SD vs Quantity, Standardkurven, Outlier)."
            )
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
    }
    
    rv$quantity_missing_any <- quantity_missing_any
    rv$quantity_missing_all <- quantity_missing_all
    
    # 6) qpcr_summary aus ausgewählten Dateien berechnen
    incProgress(0.15, detail = "Berechne Summary")
    qpcr_summary <- qpcr_all %>%
      mutate(
        Ct       = ct_vec,
        Quantity = quantity_vec,
        Reporter = if ("Reporter" %in% names(.)) as.character(Reporter) else NA_character_
      ) %>%
      filter(
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
    
    # 7) Amplifikationsdaten für ausgewählte Dateien
    incProgress(0.15, detail = "Bereite Amplifikationsdaten vor")
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
    
    # 8) Ct-Y-Achsen-Defaults aus qpcr_summary bestimmen
    incProgress(0.1, detail = "Setze Achsen-Defaults")
    if (nrow(qpcr_summary) > 0) {
      ct_min <- floor(min(qpcr_summary$Ct_mean, na.rm = TRUE))
      ct_max <- ceiling(max(qpcr_summary$Ct_mean, na.rm = TRUE))
      updateNumericInput(session, "ct_y_min", value = ct_min)
      updateNumericInput(session, "ct_y_max", value = ct_max)
    }
    
    # 9) Sidebar-Filter mit Targets/Samples aus den ausgewählten Dateien füllen
    incProgress(0.1, detail = "Aktualisiere Sidebar-Filter")
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
    
    # 10) Analyse-Daten in rv speichern
    incProgress(0.1, detail = "Speichere Analyse-Daten")
    rv$qpcr_all     <- qpcr_all
    rv$qpcr_summary <- qpcr_summary
    rv$qpcr_amp     <- qpcr_amp
    rv$qpcr_melt    <- qpcr_melt
    rv$has_delta_rn <- has_delta_rn
    rv$analysis_master <- build_analysis_master_data(qpcr_all, qpcr_summary)
    rv$analysis_context_label <- build_analysis_context_label(
      qpcr_all = qpcr_all,
      selected_files = selected
    )
    rv$data_loaded  <- TRUE
    
    # 11) Status-Meldung + Tab-Wechsel
    output$load_status <- renderText(
      paste0(
        "Analyse gestartet mit ", length(selected), " Datei(en).\n",
        "Targets: ", length(all_targets),
        " | Samples: ", length(all_samples)
      )
    )
    
    updateTabsetPanel(session, "tabs", selected = "plate_overview")
    incProgress(0.1, detail = "Abschluss")
    })
  })
