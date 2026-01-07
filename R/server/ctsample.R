  ##########################
  # Ct vs Sample - Plot
  ##########################
  
  ctsample_plot_gg <- reactive({
    validate(
      need(rv$data_loaded, "Bitte zunaechst auf der Seite 'Daten laden' qPCR-Dateien laden.")
    )
    df <- filtered_summary()
    req(nrow(df) > 0)
    req(!is.null(input$ct_y_min), !is.null(input$ct_y_max))
    req(input$ct_y_min < input$ct_y_max)
    
    facet_scales <- input$y_scale_mode
    
    ggplot(
      df,
      aes(
        x    = `Sample Name`,
        y    = Ct_mean,
        fill = factor(Quantity)
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
        x    = "Sample Name",
        y    = "Ct (Mean +/- SD)",
        fill = "Quantity",
        title = "Ct (Mean +/- SD) vs. Sample je Target"
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$qpcr_plot_sample <- renderPlotly({
    ggplotly(ctsample_plot_gg())
  })
  
  output$download_ct_plot_sample_png <- downloadHandler(
    filename = function() {
      paste0("ct_vs_sample_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Ct-Plot (PNG)", value = 0, {
        incProgress(0.3, detail = "Daten filtern")
        df <- filtered_summary()
        facet_scales <- input$y_scale_mode
        
        incProgress(0.4, detail = "Plot erstellen")
        p <- ctsample_plot_gg()
        
        incProgress(0.3, detail = "Datei schreiben")
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      })
    }
  )
  
  ##########################
  # Ct-Tabelle (Ct vs Sample)
  ##########################
  
  ctsample_table_data <- reactive({
    validate(
      need(rv$data_loaded, "Bitte im Tab 'Daten laden' Dateien laden und dann 'Analyse starten' klicken.")
    )
    df <- filtered_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten fuer die aktuelle Auswahl von Targets / Samples.")
    )
    
    df %>%
      dplyr::transmute(
        Sample   = `Sample Name`,
        Target   = `Target Name_res`,
        Reporter = Reporter,
        Quantity = Quantity,
        `Ct Mean` = Ct_mean,
        `Ct SD`   = Ct_sd
      )
  })
  
  output$ct_table_sample <- DT::renderDT({
    out <- ctsample_table_data()
    
    DT::datatable(
      out,
      options = list(
        pageLength = 25,
        scrollX    = TRUE
      )
    )
  })
  
  output$download_ct_table_sample_xlsx <- downloadHandler(
    filename = function() {
      paste0("ct_summary_sample_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Ct-Tabelle (XLSX)", value = 0, {
        incProgress(0.5, detail = "Daten aufbereiten")
        out <- ctsample_table_data()
        if (nrow(out) == 0) {
          write_xlsx(tibble(Hinweis = "Keine Daten fuer die aktuelle Auswahl."), path = file)
        } else {
          write_xlsx(out, path = file)
        }
        incProgress(0.5, detail = "Datei schreiben")
      })
    }
  )
  
  observeEvent(input$add_report_ctsample_plot, {
    withProgress(message = "Fuege Plot zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Plot erzeugen")
      plot_obj <- ctsample_plot_gg()
      plotly_obj <- ggplotly(plot_obj)
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Ct vs Sample (Plot)",
        tab = "Ct vs Sample",
        type = "plot",
        plot = plot_obj,
        plotly = plotly_obj
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Plot zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_ctsample_table, {
    withProgress(message = "Fuege Tabelle zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Tabelle erzeugen")
      out <- ctsample_table_data()
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Ct vs Sample (Tabelle)",
        tab = "Ct vs Sample",
        type = "table",
        data = out
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Tabelle zum Report hinzugefuegt.", type = "message", duration = 4)
  })
