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
