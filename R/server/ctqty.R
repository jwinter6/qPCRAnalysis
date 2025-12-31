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
