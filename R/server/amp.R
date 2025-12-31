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
