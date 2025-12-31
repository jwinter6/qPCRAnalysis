  ##########################
  # Ct SD vs Quantity & Heatmap
  ##########################
  
  output$qpcr_sd_plot <- renderPlotly({
    df <- filtered_summary()
    validate(
      need(nrow(df) > 0, "Keine Daten für Ct SD Plot.")
    )
    
    p <- ggplot(
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
    
    ggplotly(p)
  })
  
  output$download_ctsd_plot_png <- downloadHandler(
    filename = function() {
      paste0("ct_sd_vs_quantity_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- filtered_summary()
      
      p <- ggplot(
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
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
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
  
  output$qpcr_sd_heatmap <- renderPlotly({
    df <- heatmap_data()
    validate(
      need(nrow(df) > 0, "Keine Daten für Ct SD Heatmap.")
    )
    
    p <- ggplot(
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
        title = "Ct SD Heatmap (Sample × Target)"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  output$download_ctsd_heatmap_png <- downloadHandler(
    filename = function() {
      paste0("ct_sd_heatmap_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- heatmap_data()
      
      p <- ggplot(
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
          title = "Ct SD Heatmap (Sample × Target)"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      ggsave(file, plot = p, width = 10, height = 7, dpi = 300)
    }
  )
  
  output$download_ctsd_heatmap_xlsx <- downloadHandler(
    filename = function() {
      paste0("ct_sd_heatmap_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- heatmap_data()
      write_xlsx(df, path = file)
    }
  )
