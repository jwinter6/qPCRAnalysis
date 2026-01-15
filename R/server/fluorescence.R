  ##########################
  # Fluoreszenz
  ##########################
  
  fluorescence_summary <- reactive({
    validate(
      need(rv$data_loaded, "Bitte im Tab 'Daten laden' Dateien laden und 'Analyse starten' klicken.")
    )
    df <- filtered_amp()
    validate(
      need(nrow(df) > 0, "Keine Amplifikationsdaten vorhanden.")
    )
    
    signal_col <- if (!is.null(input$y_axis) && input$y_axis == "DeltaRn" && "DeltaRn" %in% names(df)) {
      "DeltaRn"
    } else {
      "Rn"
    }
    
    df <- df %>%
      mutate(
        Quantity = suppressWarnings(as.numeric(Quantity)),
        signal_value = suppressWarnings(as.numeric(.data[[signal_col]]))
      ) %>%
      filter(!is.na(signal_value))
    
    group_cols <- c("Target_ID", "Target Name_res", "Reporter", "Sample Name", "Quantity")
    if (isTRUE(input$separate_files)) {
      group_cols <- c("source_file", group_cols)
    }
    
    df_well <- df %>%
      group_by(across(all_of(c(group_cols, "well_position")))) %>%
      summarise(
        well_max = max(signal_value, na.rm = TRUE),
        well_min = min(signal_value, na.rm = TRUE),
        well_delta = well_max - well_min,
        .groups = "drop"
      )
    
    df_well %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(
        max_mean = mean(well_max, na.rm = TRUE),
        max_sd = sd(well_max, na.rm = TRUE),
        delta_mean = mean(well_delta, na.rm = TRUE),
        delta_sd = sd(well_delta, na.rm = TRUE),
        n_wells = n(),
        .groups = "drop"
      ) %>%
      mutate(
        facet_id = if (isTRUE(input$separate_files)) {
          paste0(Target_ID, " / ", source_file)
        } else {
          Target_ID
        }
      )
  })
  
  fluorescence_max_plot_gg <- reactive({
    df <- fluorescence_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten fuer Fluoreszenz-Plot.")
    )
    
    ggplot(
      df,
      aes(
        x    = `Sample Name`,
        y    = max_mean,
        fill = factor(Quantity)
      )
    ) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_errorbar(
        aes(ymin = max_mean - max_sd, ymax = max_mean + max_sd),
        position = position_dodge(width = 0.9),
        width = 0.3
      ) +
      facet_wrap(~ facet_id, scales = input$y_scale_mode) +
      labs(
        x     = "Sample",
        y     = "Max. Fluoreszenz (Mean +/- SD)",
        fill  = "Quantity",
        title = "Maximale Fluoreszenz pro Sample"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$fluorescence_max_plot <- renderPlotly({
    ggplotly(fluorescence_max_plot_gg())
  })
  
  fluorescence_delta_plot_gg <- reactive({
    df <- fluorescence_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten fuer Fluoreszenz-Plot.")
    )
    
    ggplot(
      df,
      aes(
        x    = `Sample Name`,
        y    = delta_mean,
        fill = factor(Quantity)
      )
    ) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_errorbar(
        aes(ymin = delta_mean - delta_sd, ymax = delta_mean + delta_sd),
        position = position_dodge(width = 0.9),
        width = 0.3
      ) +
      facet_wrap(~ facet_id, scales = input$y_scale_mode) +
      labs(
        x     = "Sample",
        y     = "Delta Fluoreszenz (Mean +/- SD)",
        fill  = "Quantity",
        title = "Delta Fluoreszenz pro Sample"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$fluorescence_delta_plot <- renderPlotly({
    ggplotly(fluorescence_delta_plot_gg())
  })
  
  output$fluorescence_table <- DT::renderDT({
    df <- fluorescence_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten fuer Fluoreszenz-Tabelle.")
    )
    
    out <- df %>%
      select(
        any_of("source_file"),
        `Sample Name`,
        `Target Name_res`,
        Reporter,
        Quantity,
        n_wells,
        max_mean,
        max_sd,
        delta_mean,
        delta_sd
      )
    
    DT::datatable(
      out,
      options = list(
        pageLength = 25,
        scrollX    = TRUE
      )
    )
  })
  
  output$download_fluor_max_plot_png <- downloadHandler(
    filename = function() {
      paste0("fluorescence_max_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Fluoreszenz-Max (PNG)", value = 0, {
        incProgress(0.4, detail = "Plot erstellen")
        p <- fluorescence_max_plot_gg()
        incProgress(0.6, detail = "Datei schreiben")
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      })
    }
  )
  
  output$download_fluor_delta_plot_png <- downloadHandler(
    filename = function() {
      paste0("fluorescence_delta_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Delta Fluoreszenz (PNG)", value = 0, {
        incProgress(0.4, detail = "Plot erstellen")
        p <- fluorescence_delta_plot_gg()
        incProgress(0.6, detail = "Datei schreiben")
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      })
    }
  )
  
  output$download_fluor_table_xlsx <- downloadHandler(
    filename = function() {
      paste0("fluorescence_table_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Fluoreszenz-Tabelle (XLSX)", value = 0, {
        incProgress(0.4, detail = "Daten aufbereiten")
        df <- fluorescence_summary()
        out <- df %>%
          select(
            any_of("source_file"),
            `Sample Name`,
            `Target Name_res`,
            Reporter,
            Quantity,
            n_wells,
            max_mean,
            max_sd,
            delta_mean,
            delta_sd
          )
        incProgress(0.6, detail = "Datei schreiben")
        write_xlsx(out, path = file)
      })
    }
  )
  
  observeEvent(input$add_report_fluor_max_plot, {
    withProgress(message = "Fuege Plot zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Plot erzeugen")
      plot_obj <- fluorescence_max_plot_gg()
      plotly_obj <- ggplotly(plot_obj)
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Fluoreszenz Max (Plot)",
        tab = "Fluoreszenz",
        type = "plot",
        plot = plot_obj,
        plotly = plotly_obj
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Plot zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_fluor_delta_plot, {
    withProgress(message = "Fuege Plot zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Plot erzeugen")
      plot_obj <- fluorescence_delta_plot_gg()
      plotly_obj <- ggplotly(plot_obj)
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Delta Fluoreszenz (Plot)",
        tab = "Fluoreszenz",
        type = "plot",
        plot = plot_obj,
        plotly = plotly_obj
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Plot zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_fluor_table, {
    withProgress(message = "Fuege Tabelle zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Tabelle erzeugen")
      df <- fluorescence_summary()
      out <- df %>%
        select(
          any_of("source_file"),
          `Sample Name`,
          `Target Name_res`,
          Reporter,
          Quantity,
          n_wells,
          max_mean,
          max_sd,
          delta_mean,
          delta_sd
        )
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Fluoreszenz (Tabelle)",
        tab = "Fluoreszenz",
        type = "table",
        data = out
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Tabelle zum Report hinzugefuegt.", type = "message", duration = 4)
  })
