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
    
    if (!isTRUE(input$separate_files)) {
      df <- df %>%
        group_by(
          Target_ID,
          `Target Name_res`,
          Reporter,
          `Sample Name`,
          Quantity
        ) %>%
        summarise(
          n = sum(n, na.rm = TRUE),
          Ct_mean = sum(Ct_mean * n, na.rm = TRUE) / sum(n, na.rm = TRUE),
          Ct_sd = {
            total_n <- sum(n, na.rm = TRUE)
            mean_c <- sum(Ct_mean * n, na.rm = TRUE) / sum(n, na.rm = TRUE)
            if (total_n <= 1) {
              NA_real_
            } else {
              pooled_var <- sum((n - 1) * Ct_sd^2, na.rm = TRUE) +
                sum(n * (Ct_mean - mean_c)^2, na.rm = TRUE)
              sqrt(pooled_var / (total_n - 1))
            }
          },
          .groups = "drop"
        )
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
  # Hinweis: Quantity fehlt
  ##########################

  quantity_notice_block <- function() {
    if (!isTRUE(rv$quantity_missing_any)) return(NULL)
    tags$div(
      class = "alert alert-warning",
      tags$strong("Hinweis: Quantity fehlt."),
      tags$span(
        " Fehlende Quantity-Werte wurden auf 0 gesetzt. ",
        "Plots mit Quantity auf der X-Achse koennen dadurch leer wirken oder irrefuehrend sein."
      )
    )
  }

  output$quantity_notice_ctqty <- renderUI({
    quantity_notice_block()
  })

  output$quantity_notice_ctsd <- renderUI({
    quantity_notice_block()
  })

  output$quantity_notice_stdcurves <- renderUI({
    quantity_notice_block()
  })

  output$quantity_notice_outliers <- renderUI({
    quantity_notice_block()
  })
  
  ##########################
  # Dynamische UI: Y-Achse Amplifikationskurven
  ##########################
  
  output$y_axis_ui <- renderUI({
    label_with_tip <- function(text) {
      tagList(
        text,
        bslib::tooltip(
          tags$span("i", class = "qpcr-tooltip-icon"),
          "Waehlt den Signaltyp fuer Amplifikationskurven."
        )
      )
    }
    if (isTRUE(rv$has_delta_rn)) {
      radioButtons(
        inputId  = "y_axis",
        label    = label_with_tip("Y-Achse (Amplifikationskurven)"),
        choices  = c("Rn" = "Rn", "Delta Rn" = "DeltaRn"),
        selected = "Rn"
      )
    } else {
      radioButtons(
        inputId  = "y_axis",
        label    = label_with_tip("Y-Achse (Amplifikationskurven)"),
        choices  = c("Rn" = "Rn"),
        selected = "Rn"
      )
    }
  })
