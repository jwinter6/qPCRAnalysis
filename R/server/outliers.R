  ##########################
  # Outlier Tests – Ct pro Well
  ##########################
  
  # Outlier-Daten: Ct pro Well (nicht Ct_mean), inklusive aller Wells über alle Quantities
  outlier_data <- reactive({
    validate(
      need(rv$data_loaded, "Bitte im Tab 'Daten laden' Dateien laden und 'Analyse starten' klicken.")
    )
    req(input$outlier_target, input$outlier_sample)
    req(rv$qpcr_all)
    
    # Ct pro Well aus den Rohdaten (nicht aus qpcr_summary)
    df <- rv$qpcr_all %>%
      mutate(
        Ct = dplyr::case_when(
          "CRT" %in% names(.)      ~ suppressWarnings(as.numeric(CRT)),
          "Crt Mean" %in% names(.) ~ suppressWarnings(as.numeric(`Crt Mean`)),
          TRUE                     ~ NA_real_
        ),
        Quantity = suppressWarnings(as.numeric(Quantity)),
        Reporter = if ("Reporter" %in% names(.)) as.character(Reporter) else NA_character_
      ) %>%
      mutate(
        Target_ID = if_else(
          !is.na(Reporter),
          paste0(`Target Name_res`, " [", Reporter, "]"),
          `Target Name_res`
        )
      ) %>%
      filter(
        Target_ID        == input$outlier_target,
        `Sample Name`    == input$outlier_sample,
        !is.na(Ct),
        !is.na(Quantity),
        Quantity > 0
      ) %>%
      group_by(source_file, Target_ID, `Sample Name`, Quantity, well_position) %>%
      summarise(
        Ct = mean(Ct, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        logQ = log10(Quantity)
      ) %>%
      arrange(Quantity, well_position)
    
    validate(
      need(nrow(df) >= 3, "Zu wenige Ct-Werte für ausgewähltes Target/Sample (mind. 3 Wells erforderlich).")
    )
    
    # Linearer Fit Ct ~ log10(Quantity) über alle Wells
    fit <- lm(Ct ~ logQ, data = df)
    
    df <- df %>%
      mutate(
        fit_value = predict(fit, newdata = df),
        resid     = Ct - fit_value
      )
    
    df
  })
  
  # UI-Erklärung für Outlier-Tests
  output$outlier_explanation <- renderUI({
    validate(
      need(rv$data_loaded, "Bitte im Tab 'Daten laden' Dateien laden und 'Analyse starten' klicken.")
    )
    
    method <- input$outlier_test
    if (is.null(method)) return(NULL)
    
    text_method <- switch(
      method,
      "Dixon" = tags$div(
        tags$strong("Dixon-Test (Q-Test)"),
        tags$p(
          "Wird für kleine Stichproben (ca. 3–25 Werte) verwendet. ",
          "Es werden das kleinste und größte Residuum mit den übrigen Residuen verglichen. ",
          "Wenn eines dieser extremen Residuen im Verhältnis zur inneren Spannweite zu groß ist ",
          "(p < 0,05), wird der zugehörige Ct-Wert als Ausreißer markiert."
        )
      ),
      "Grubbs" = tags$div(
        tags$strong("Grubbs-Test"),
        tags$p(
          "Testet, ob genau ein Wert in der Stichprobe signifikant von den übrigen abweicht. ",
          "Es wird der Wert mit der größten Abweichung vom Mittelwert der Residuen untersucht. ",
          "Ist dieser Wert statistisch auffällig (p < 0,05), wird genau ein Ct-Wert als Ausreißer markiert."
        )
      ),
      "Rosner" = tags$div(
        tags$strong("Rosner-Test (Generalized ESD)"),
        tags$p(
          "Erlaubt die Detektion mehrerer Ausreißer gleichzeitig. ",
          "Es werden iterativ bis zu k potentielle Ausreißer getestet (hier maximal 3 Werte). ",
          "Alle Ct-Werte, deren Residuen in diesem Test als Ausreißer gekennzeichnet werden ",
          "(p < 0,05), werden entsprechend markiert."
        )
      )
    )
    
    tags$div(
      style = "background-color:#f8f9fa; border-radius:8px; padding:10px; border:1px solid #ddd;",
      tags$strong("Was wird hier getestet?"),
      tags$p(
        "Für das ausgewählte Target/Sample wird zunächst ein linearer Fit ",
        tags$code("Ct ~ log10(Quantity)"),
        " über alle Wells durchgeführt."
      ),
      tags$p(
        "Für jedes Well wird das Residuum berechnet: ",
        tags$code("Residuum = beobachtetes Ct - vorhergesagtes Ct (aus dem Fit)"),
        ". ",
        "Die Outlier-Tests arbeiten auf diesen Residuen."
      ),
      text_method,
      tags$hr(),
      tags$p(
        tags$strong("Wann ist ein Ct-Wert eines Wells ein Outlier?"),
        tags$br(),
        "Ein Ct-Wert eines bestimmten Wells gilt hier als Ausreißer, wenn ",
        "sein Residuum in dem gewählten Test als statistisch auffällig eingestuft wird ",
        "(Signifikanzniveau p < 0,05). ",
        "Solche Wells werden in der Tabelle in der Spalte ",
        tags$code("is_outlier"),
        " mit ", tags$code("TRUE"), " gekennzeichnet und im Residuenplot ",
        "rot hervorgehoben."
      ),
      tags$p(
        "Hinweis: Alle Tests setzen ungefähr normalverteilte Residuen und unabhängige Wells voraus. ",
        "Bei systematischen Fehlern (z. B. Pipettierfehler in einer kompletten Verdünnungsreihe) ",
        "können die Tests diese Werte unter Umständen nicht als Ausreißer erkennen."
      )
    )
  })
  
  output$outlier_table <- renderDT({
    df <- outlier_data()
    v <- df$resid
    flags <- run_outlier_test(v, method = input$outlier_test)
    
    out <- df %>%
      mutate(
        residual   = resid,
        is_outlier = flags
      ) %>%
      select(
        source_file,
        Target_ID,
        `Sample Name`,
        well_position,
        Quantity,
        Ct,
        residual,
        is_outlier
      )
    
    datatable(
      out,
      options = list(
        pageLength   = 20,
        orderClasses = TRUE,
        autoWidth    = TRUE
      )
    )
  })
  
  output$download_outlier_table_xlsx <- downloadHandler(
    filename = function() {
      paste0("outliers_", input$outlier_target, "_", input$outlier_sample, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- outlier_data()
      v <- df$resid
      flags <- run_outlier_test(v, method = input$outlier_test)
      
      out <- df %>%
        mutate(
          residual   = resid,
          is_outlier = flags
        ) %>%
        select(
          source_file,
          Target_ID,
          `Sample Name`,
          well_position,
          Quantity,
          Ct,
          residual,
          is_outlier
        )
      
      write_xlsx(out, path = file)
    }
  )
  
  output$outlier_residual_plot <- renderPlotly({
    df <- outlier_data()
    v <- df$resid
    flags <- run_outlier_test(v, method = input$outlier_test)
    
    df <- df %>%
      mutate(is_outlier = flags)
    
    p <- ggplot(
      df,
      aes(
        x     = logQ,
        y     = resid,
        color = is_outlier
      )
    ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(size = 3) +
      scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black")) +
      labs(
        x     = "log10(Quantity)",
        y     = "Residual (Ct - Fit)",
        color = "Outlier",
        title = paste(
          "Residuenplot –",
          input$outlier_target, "/",
          input$outlier_sample, "(",
          input$outlier_test, ")"
        )
      ) +
      theme_bw() 
    
    ggplotly(p)
  })
  
  output$download_outlier_plot_png <- downloadHandler(
    filename = function() {
      paste0("outlier_residuals_", input$outlier_target, "_", input$outlier_sample, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- outlier_data()
      v <- df$resid
      flags <- run_outlier_test(v, method = input$outlier_test)
      
      df <- df %>%
        mutate(is_outlier = flags)
      
      p <- ggplot(
        df,
        aes(
          x     = logQ,
          y     = resid,
          color = is_outlier
        )
      ) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_point(size = 3) +
        scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black")) +
        labs(
          x     = "log10(Quantity)",
          y     = "Residual (Ct - Fit)",
          color = "Outlier",
          title = paste(
            "Residuenplot –",
            input$outlier_target, "/",
            input$outlier_sample, "(",
            input$outlier_test, ")"
          )
        ) +
        theme_bw() 
      
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
  )
  
  ##########################
  # Outlier UI Inputs (Targets & Samples)
  ##########################
  
  output$outlier_target_ui <- renderUI({
    validate(
      need(rv$data_loaded, "")
    )
    df <- rv$qpcr_summary
    if (is.null(df) || nrow(df) == 0) {
      return(selectInput("outlier_target", "Target_ID (Outlier)", choices = NULL))
    }
    targets <- df %>%
      distinct(Target_ID) %>%
      arrange(Target_ID) %>%
      pull()
    selectInput(
      "outlier_target",
      "Target_ID (Outlier)",
      choices  = targets,
      selected = targets[1]
    )
  })
  
  output$outlier_sample_ui <- renderUI({
    validate(
      need(rv$data_loaded, "")
    )
    df <- rv$qpcr_summary
    if (is.null(df) || nrow(df) == 0) {
      return(selectInput("outlier_sample", "Sample (Outlier)", choices = NULL))
    }
    samples <- df %>%
      distinct(`Sample Name`) %>%
      arrange(`Sample Name`) %>%
      pull()
    selectInput(
      "outlier_sample",
      "Sample (Outlier)",
      choices  = samples,
      selected = samples[1]
    )
  })
