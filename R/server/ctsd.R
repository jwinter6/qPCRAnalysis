  ##########################
  # Ct SD vs Quantity & Heatmap
  ##########################
  
  ctsd_plot_gg <- reactive({
    df <- filtered_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten fuer Ct SD Plot.")
    )
    
    ggplot(
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
  })
  
  output$qpcr_sd_plot <- renderPlotly({
    ggplotly(ctsd_plot_gg())
  })
  
  output$download_ctsd_plot_png <- downloadHandler(
    filename = function() {
      paste0("ct_sd_vs_quantity_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Ct SD Plot (PNG)", value = 0, {
        incProgress(0.3, detail = "Daten filtern")
        df <- filtered_summary()
        
        incProgress(0.4, detail = "Plot erstellen")
        p <- ctsd_plot_gg()
        
        incProgress(0.3, detail = "Datei schreiben")
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      })
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
  
  ctsd_heatmap_gg <- reactive({
    df <- heatmap_data()
    validate(
      need(nrow(df) > 0, "Keine Daten fuer Ct SD Heatmap.")
    )
    
    ggplot(
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
        title = "Ct SD Heatmap (Sample x Target)"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$qpcr_sd_heatmap <- renderPlotly({
    ggplotly(ctsd_heatmap_gg())
  })
  
  output$download_ctsd_heatmap_png <- downloadHandler(
    filename = function() {
      paste0("ct_sd_heatmap_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Ct SD Heatmap (PNG)", value = 0, {
        incProgress(0.3, detail = "Daten aufbereiten")
        df <- heatmap_data()
        
        incProgress(0.4, detail = "Heatmap erstellen")
        p <- ctsd_heatmap_gg()
        
        incProgress(0.3, detail = "Datei schreiben")
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      })
    }
  )
  
  output$download_ctsd_heatmap_xlsx <- downloadHandler(
    filename = function() {
      paste0("ct_sd_heatmap_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Ct SD Heatmap (XLSX)", value = 0, {
        incProgress(0.5, detail = "Daten aufbereiten")
        df <- heatmap_data()
        write_xlsx(df, path = file)
        incProgress(0.5, detail = "Datei schreiben")
      })
    }
  )
  
  observeEvent(input$add_report_ctsd_plot, {
    withProgress(message = "Fuege Plot zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Plot erzeugen")
      plot_obj <- ctsd_plot_gg()
      plotly_obj <- ggplotly(plot_obj)
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Ct SD vs Quantity (Plot)",
        tab = "Ct SD",
        type = "plot",
        plot = plot_obj,
        plotly = plotly_obj
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Plot zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_ctsd_heatmap, {
    withProgress(message = "Fuege Heatmap zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Plot erzeugen")
      plot_obj <- ctsd_heatmap_gg()
      plotly_obj <- ggplotly(plot_obj)
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Ct SD Heatmap",
        tab = "Ct SD",
        type = "plot",
        plot = plot_obj,
        plotly = plotly_obj
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Heatmap zum Report hinzugefuegt.", type = "message", duration = 4)
  })
