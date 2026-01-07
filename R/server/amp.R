  ##########################
  # Amplifikationskurven
  ##########################
  
  amp_plot_gg <- reactive({
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
    
    ggplot(
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
  })
  
  output$qpcr_curve_plot <- renderPlotly({
    ggplotly(amp_plot_gg())
  })
  
  output$download_amp_plot_png <- downloadHandler(
    filename = function() {
      paste0("amplification_curves_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Amplifikationskurven (PNG)", value = 0, {
        incProgress(0.3, detail = "Daten filtern")
        df <- filtered_amp()
        y_axis <- input$y_axis
        y_label <- if (y_axis == "Rn") "Rn" else "Delta Rn"
        
        incProgress(0.4, detail = "Plot erstellen")
        p <- amp_plot_gg()
        
        incProgress(0.3, detail = "Datei schreiben")
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      })
    }
  )
  
  observeEvent(input$add_report_amp_plot, {
    withProgress(message = "Fuege Plot zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Plot erzeugen")
      plot_obj <- amp_plot_gg()
      plotly_obj <- ggplotly(plot_obj)
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Amplifikationskurven",
        tab = "Amplifikationskurven",
        type = "plot",
        plot = plot_obj,
        plotly = plotly_obj
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Plot zum Report hinzugefuegt.", type = "message", duration = 4)
  })
