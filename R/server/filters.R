  ##########################
  # Gefilterte Daten
  ##########################
  
  filtered_summary <- reactive({
    validate(
      need(
        rv$data_loaded,
        "Bitte im Tab 'Daten laden' Dateien laden und dann 'Analyse starten' klicken."
      )
    )
    
    df <- rv$qpcr_summary
    if (is.null(df) || nrow(df) == 0) return(df[0, ])
    
    if (!is.null(input$target_filter) && length(input$target_filter) > 0) {
      df <- df %>% dplyr::filter(Target_ID %in% input$target_filter)
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$sample_filter) && length(input$sample_filter) > 0) {
      df <- df %>% dplyr::filter(`Sample Name` %in% input$sample_filter)
    } else {
      df <- df[0, ]
    }
    
    df
  })
  
  filtered_amp <- reactive({
    validate(
      need(
        rv$data_loaded,
        "Bitte im Tab 'Daten laden' Dateien laden und dann 'Analyse starten' klicken."
      )
    )
    
    df <- rv$qpcr_amp
    if (is.null(df) || nrow(df) == 0) return(tibble())
    
    if (!is.null(input$target_filter) && length(input$target_filter) > 0) {
      df <- df %>% dplyr::filter(Target_ID %in% input$target_filter)
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$sample_filter) && length(input$sample_filter) > 0) {
      df <- df %>% dplyr::filter(`Sample Name` %in% input$sample_filter)
    } else {
      df <- df[0, ]
    }
    
    df
  })
  
  filtered_melt <- reactive({
    validate(
      need(
        rv$data_loaded,
        "Bitte im Tab 'Daten laden' Dateien laden und dann 'Analyse starten' klicken."
      )
    )
    
    df <- rv$qpcr_melt
    if (is.null(df) || nrow(df) == 0) return(tibble())
    
    if (!is.null(input$target_filter) && length(input$target_filter) > 0) {
      df <- df %>% dplyr::filter(Target_ID %in% input$target_filter)
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$sample_filter) && length(input$sample_filter) > 0) {
      df <- df %>% dplyr::filter(`Sample Name` %in% input$sample_filter)
    } else {
      df <- df[0, ]
    }
    
    df
  })
  
  ##########################
  # Dynamische UI: Y-Achse Amplifikationskurven
  ##########################
  
  output$y_axis_ui <- renderUI({
    if (isTRUE(rv$has_delta_rn)) {
      radioButtons(
        inputId  = "y_axis",
        label    = "Y-Achse (Amplifikationskurven)",
        choices  = c("Rn" = "Rn", "Delta Rn" = "DeltaRn"),
        selected = "Rn"
      )
    } else {
      radioButtons(
        inputId  = "y_axis",
        label    = "Y-Achse (Amplifikationskurven)",
        choices  = c("Rn" = "Rn"),
        selected = "Rn"
      )
    }
  })
