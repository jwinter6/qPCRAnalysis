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
  
  output$standardcurve_table <- DT::renderDT({
    df <- standardcurve_data()
    validate(
      need(nrow(df) > 0, "Keine Standardkurven-Daten.")
    )
    
    out <- df %>%
      mutate(
        slope      = round(slope, 3),
        intercept  = round(intercept, 3),
        r2         = round(r2, 4),
        efficiency = round(efficiency, 1)
      )
    
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
      df <- standardcurve_data()
      write_xlsx(df, path = file)
    }
  )
  
  output$stdcurve_slope_plot <- renderPlotly({
    df <- standardcurve_data()
    validate(
      need(nrow(df) > 0, "Keine Standardkurven-Daten für Slope-Plot.")
    )
    
    p <- ggplot(
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
    
    ggplotly(p)
  })
  
  output$download_stdcurve_slope_png <- downloadHandler(
    filename = function() {
      paste0("standardcurve_slopes_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- standardcurve_data()
      
      p <- ggplot(
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
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$stdcurve_eff_plot <- renderPlotly({
    df <- standardcurve_data()
    validate(
      need(nrow(df) > 0, "Keine Standardkurven-Daten für Effizienz-Plot.")
    )
    
    p <- ggplot(
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
    
    ggplotly(p)
  })
  
  output$download_stdcurve_eff_png <- downloadHandler(
    filename = function() {
      paste0("standardcurve_efficiency_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- standardcurve_data()
      
      p <- ggplot(
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
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  # Scatterplot: Ct_mean ~ log10(Quantity) je Target_ID, alle ausgewählten Samples
  output$stdcurve_scatter_plot <- renderPlotly({
    df <- filtered_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten für Standardkurven-Scatterplot.")
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
      need(nrow(df) > 0, "Keine Daten für das ausgewählte Target im Scatterplot.")
    )
    
    p <- ggplot(
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
        title = paste("Standardkurven Scatterplot –", input$std_scatter_target)
      ) +
      theme_bw()
    
    ggplotly(p)
  })
  
  output$download_stdcurve_scatter_png <- downloadHandler(
    filename = function() {
      paste0("standardcurve_scatter_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- filtered_summary()
      df <- df %>%
        filter(Quantity > 0) %>%
        mutate(
          logQ = log10(Quantity)
        )
      req(input$std_scatter_target)
      df <- df %>%
        filter(Target_ID == input$std_scatter_target)
      
      p <- ggplot(
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
          title = paste("Standardkurven Scatterplot –", input$std_scatter_target)
        ) +
        theme_bw() 
    
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )

  ##########################
  # Standardkurven-Target-Auswahl für Scatterplots
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
