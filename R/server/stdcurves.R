  ##########################
  # Standardkurven
  ##########################
  
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
  
  standardcurve_table_data <- reactive({
    df <- standardcurve_data()
    validate(
      need(nrow(df) > 0, "Keine Standardkurven-Daten.")
    )
    
    df %>%
      mutate(
        slope      = round(slope, 3),
        intercept  = round(intercept, 3),
        r2         = round(r2, 4),
        efficiency = round(efficiency, 1)
      )
  })
  
  output$standardcurve_table <- DT::renderDT({
    out <- standardcurve_table_data()
    
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
      withProgress(message = "Download vorbereiten: Standardkurven-Tabelle (XLSX)", value = 0, {
        incProgress(0.5, detail = "Daten aufbereiten")
        df <- standardcurve_table_data()
        write_xlsx(df, path = file)
        incProgress(0.5, detail = "Datei schreiben")
      })
    }
  )
  
  stdcurve_slope_plot_gg <- reactive({
    df <- standardcurve_data()
    validate(
      need(nrow(df) > 0, "Keine Standardkurven-Daten fuer Slope-Plot.")
    )
    
    ggplot(
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
  })
  
  output$stdcurve_slope_plot <- renderPlotly({
    ggplotly(stdcurve_slope_plot_gg())
  })
  
  output$download_stdcurve_slope_png <- downloadHandler(
    filename = function() {
      paste0("standardcurve_slopes_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Slope-Plot (PNG)", value = 0, {
        incProgress(0.3, detail = "Daten aufbereiten")
        df <- standardcurve_data()
        
        incProgress(0.4, detail = "Plot erstellen")
        p <- stdcurve_slope_plot_gg()
        
        incProgress(0.3, detail = "Datei schreiben")
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      })
    }
  )
  
  stdcurve_eff_plot_gg <- reactive({
    df <- standardcurve_data()
    validate(
      need(nrow(df) > 0, "Keine Standardkurven-Daten fuer Effizienz-Plot.")
    )
    
    ggplot(
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
  })
  
  output$stdcurve_eff_plot <- renderPlotly({
    ggplotly(stdcurve_eff_plot_gg())
  })
  
  output$download_stdcurve_eff_png <- downloadHandler(
    filename = function() {
      paste0("standardcurve_efficiency_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Effizienz-Plot (PNG)", value = 0, {
        incProgress(0.3, detail = "Daten aufbereiten")
        df <- standardcurve_data()
        
        incProgress(0.4, detail = "Plot erstellen")
        p <- stdcurve_eff_plot_gg()
        
        incProgress(0.3, detail = "Datei schreiben")
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      })
    }
  )
  
  # Scatterplot: Ct_mean ~ log10(Quantity) je Target_ID, alle ausgewaehlten Samples
  stdcurve_scatter_plot_gg <- reactive({
    df <- filtered_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten fuer Standardkurven-Scatterplot.")
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
      need(nrow(df) > 0, "Keine Daten fuer das ausgewaehlte Target im Scatterplot.")
    )
    
    ggplot(
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
        title = paste("Standardkurven Scatterplot -", input$std_scatter_target)
      ) +
      theme_bw()
  })
  
  output$stdcurve_scatter_plot <- renderPlotly({
    ggplotly(stdcurve_scatter_plot_gg())
  })
  
  output$download_stdcurve_scatter_png <- downloadHandler(
    filename = function() {
      paste0("standardcurve_scatter_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(message = "Download vorbereiten: Scatterplot (PNG)", value = 0, {
        incProgress(0.3, detail = "Daten filtern")
        df <- filtered_summary()
        df <- df %>%
          filter(Quantity > 0) %>%
          mutate(
            logQ = log10(Quantity)
          )
        req(input$std_scatter_target)
        df <- df %>%
          filter(Target_ID == input$std_scatter_target)
        
        incProgress(0.4, detail = "Plot erstellen")
        p <- stdcurve_scatter_plot_gg()
    
        incProgress(0.3, detail = "Datei schreiben")
        ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
      })
    }
  )

  ##########################
  # Standardkurven-Target-Auswahl fuer Scatterplots
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

  observeEvent(input$add_report_stdcurve_table, {
    withProgress(message = "Fuege Tabelle zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Tabelle erzeugen")
      out <- standardcurve_table_data()
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Standardkurven (Tabelle)",
        tab = "Standardkurven",
        type = "table",
        data = out
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Tabelle zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_stdcurve_slope_plot, {
    withProgress(message = "Fuege Plot zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Plot erzeugen")
      plot_obj <- stdcurve_slope_plot_gg()
      plotly_obj <- ggplotly(plot_obj)
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Standardkurven Slope",
        tab = "Standardkurven",
        type = "plot",
        plot = plot_obj,
        plotly = plotly_obj
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Plot zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_stdcurve_eff_plot, {
    withProgress(message = "Fuege Plot zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Plot erzeugen")
      plot_obj <- stdcurve_eff_plot_gg()
      plotly_obj <- ggplotly(plot_obj)
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Standardkurven Effizienz",
        tab = "Standardkurven",
        type = "plot",
        plot = plot_obj,
        plotly = plotly_obj
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Plot zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_stdcurve_scatter_plot, {
    withProgress(message = "Fuege Plot zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Plot erzeugen")
      plot_obj <- stdcurve_scatter_plot_gg()
      plotly_obj <- ggplotly(plot_obj)
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Standardkurven Scatterplot",
        tab = "Standardkurven",
        type = "plot",
        plot = plot_obj,
        plotly = plotly_obj
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Plot zum Report hinzugefuegt.", type = "message", duration = 4)
  })
