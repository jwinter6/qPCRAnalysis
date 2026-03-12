  ##########################
  # Cluster
  ##########################

  cluster_param_label <- function(param_id) {
    labels <- c(
      fluor_last = "Fluoreszenz (letzter Cycle)",
      fluor_max  = "Fluoreszenz (Maximum)"
    )
    if (is.null(param_id) || !(param_id %in% names(labels))) return(param_id)
    labels[[param_id]]
  }

  cluster_param_cycle_label <- function(param_id) {
    labels <- c(
      fluor_last = "Letzter Cycle",
      fluor_max  = "Cycle bei Fluoreszenz-Maximum"
    )
    if (is.null(param_id) || !(param_id %in% names(labels))) return("Cycle")
    labels[[param_id]]
  }

  cluster_param_cycle_col <- function(param_id) {
    cols <- c(
      fluor_last = "last_cycle",
      fluor_max  = "fluor_max_cycle"
    )
    if (is.null(param_id) || !(param_id %in% names(cols))) return("last_cycle")
    cols[[param_id]]
  }

  cluster_single_or_multi <- function(x, multi_label) {
    vals <- unique(as.character(x))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 1) vals[[1]] else multi_label
  }

  cluster_cycle_range_text <- function(x) {
    vals <- sort(unique(suppressWarnings(as.numeric(x))))
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return("n/a")
    if (length(vals) == 1) return(as.character(vals[[1]]))
    paste0(min(vals), " bis ", max(vals), " (", length(vals), " Werte)")
  }

  cluster_empty_inputs <- function() {
    updateSelectizeInput(session, "cluster_samples", choices = character(0), selected = character(0), server = TRUE)
    updateSelectizeInput(session, "cluster_target_x", choices = character(0), selected = character(0), server = TRUE)
    updateSelectizeInput(session, "cluster_target_y", choices = character(0), selected = character(0), server = TRUE)
    updateSelectInput(session, "cluster_color_param", choices = c("Keine" = "none"), selected = "none")
  }

  observe({
    if (!isTRUE(rv$data_loaded) || is.null(rv$qpcr_amp) || nrow(rv$qpcr_amp) == 0) {
      cluster_empty_inputs()
      return()
    }

    df <- rv$qpcr_amp %>%
      mutate(
        sample_id = as.character(`Sample Name`),
        target_id = as.character(Target_ID),
        source_file = as.character(source_file),
        run_key = if ("run_id" %in% names(.)) {
          dplyr::if_else(!is.na(run_id) & nzchar(as.character(run_id)), as.character(run_id), "(Datei-Run)")
        } else {
          "(Datei-Run)"
        }
      ) %>%
      filter(
        !is.na(sample_id),
        nzchar(sample_id),
        !is.na(target_id),
        nzchar(target_id),
        !is.na(Cycle),
        !is.na(Rn)
      )

    if (nrow(df) == 0) {
      cluster_empty_inputs()
      return()
    }

    samples <- sort(unique(df$sample_id))
    targets <- sort(unique(df$target_id))

    selected_samples <- isolate(input$cluster_samples)
    if (is.null(selected_samples)) selected_samples <- character(0)
    selected_samples <- intersect(selected_samples, samples)
    if (length(selected_samples) == 0) selected_samples <- samples

    selected_x <- isolate(input$cluster_target_x)
    if (is.null(selected_x) || !(selected_x %in% targets)) {
      selected_x <- targets[1]
    }

    selected_y <- isolate(input$cluster_target_y)
    if (is.null(selected_y) || !(selected_y %in% targets)) {
      selected_y <- targets[min(2, length(targets))]
    }

    display_mode <- if (!is.null(input$cluster_display_mode)) input$cluster_display_mode else "sample_agg"
    color_choices <- c(
      "Keine" = "none",
      "Sample" = "sample_id",
      "Datei" = "source_file"
    )
    if (dplyr::n_distinct(df$run_key, na.rm = TRUE) > 1) {
      color_choices <- c(color_choices, "Run" = "run_key")
    }
    if (identical(display_mode, "well_points")) {
      color_choices <- c(color_choices, "Well/Paar" = "point_label")
    }

    selected_color <- isolate(input$cluster_color_param)
    if (is.null(selected_color) || !(selected_color %in% unname(color_choices))) {
      selected_color <- "none"
    }

    updateSelectizeInput(
      session,
      "cluster_samples",
      choices = samples,
      selected = selected_samples,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "cluster_target_x",
      choices = targets,
      selected = selected_x,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "cluster_target_y",
      choices = targets,
      selected = selected_y,
      server = TRUE
    )
    updateSelectInput(
      session,
      "cluster_color_param",
      choices = color_choices,
      selected = selected_color
    )
  })

  cluster_metric_data <- reactive({
    validate(
      need(rv$data_loaded, "Bitte im Tab 'Daten laden' Dateien laden und Analyse starten.")
    )

    df <- rv$qpcr_amp
    validate(
      need(!is.null(df) && nrow(df) > 0, "Keine Amplifikationsdaten fuer Cluster verfuegbar.")
    )

    df <- df %>%
      mutate(
        sample_id = as.character(`Sample Name`),
        target_id = as.character(Target_ID),
        source_file = as.character(source_file),
        run_key = if ("run_id" %in% names(.)) {
          dplyr::if_else(!is.na(run_id) & nzchar(as.character(run_id)), as.character(run_id), "(Datei-Run)")
        } else {
          "(Datei-Run)"
        },
        well_label = if ("well_position" %in% names(.)) {
          as.character(well_position)
        } else {
          NA_character_
        },
        well_label = dplyr::if_else(!is.na(well_label) & nzchar(well_label), well_label, "(ohne Well-ID)")
      ) %>%
      filter(
        !is.na(sample_id),
        nzchar(sample_id),
        !is.na(target_id),
        nzchar(target_id),
        !is.na(Cycle),
        !is.na(Rn)
      )

    if (!is.null(input$cluster_samples) && length(input$cluster_samples) > 0) {
      df <- df %>% filter(sample_id %in% input$cluster_samples)
    } else {
      df <- df[0, ]
    }

    validate(
      need(nrow(df) > 0, "Keine Daten fuer die aktuelle lokale Sample-Auswahl.")
    )

    df %>%
      group_by(source_file, run_key, sample_id, target_id, well_label) %>%
      summarise(
        last_cycle = max(Cycle, na.rm = TRUE),
        fluor_last = {
          lc <- max(Cycle, na.rm = TRUE)
          mean(Rn[Cycle == lc], na.rm = TRUE)
        },
        fluor_max = max(Rn, na.rm = TRUE),
        fluor_max_cycle = {
          rn_max <- max(Rn, na.rm = TRUE)
          cyc <- suppressWarnings(min(Cycle[Rn == rn_max], na.rm = TRUE))
          if (is.finite(cyc)) cyc else NA_real_
        },
        .groups = "drop"
      )
  })

  cluster_prepared_data <- reactive({
    df <- cluster_metric_data()
    display_mode <- if (!is.null(input$cluster_display_mode)) input$cluster_display_mode else "sample_agg"

    if (identical(display_mode, "well_points")) {
      return(df %>% mutate(point_label = well_label))
    }

    df %>%
      group_by(source_file, run_key, sample_id, target_id) %>%
      summarise(
        point_label = sample_id[1],
        last_cycle = max(last_cycle, na.rm = TRUE),
        fluor_last = mean(fluor_last, na.rm = TRUE),
        fluor_max = max(fluor_max, na.rm = TRUE),
        fluor_max_cycle = {
          fmax <- max(fluor_max, na.rm = TRUE)
          cyc <- suppressWarnings(min(fluor_max_cycle[fluor_max == fmax], na.rm = TRUE))
          if (is.finite(cyc)) cyc else NA_real_
        },
        .groups = "drop"
      )
  })

  cluster_plot_data <- reactive({
    req(
      input$cluster_target_x,
      input$cluster_target_y,
      input$cluster_x_param,
      input$cluster_y_param
    )

    df <- cluster_prepared_data()
    validate(
      need(input$cluster_x_param %in% names(df), "X-Parameter ist nicht verfuegbar."),
      need(input$cluster_y_param %in% names(df), "Y-Parameter ist nicht verfuegbar.")
    )

    x_cycle_col <- cluster_param_cycle_col(input$cluster_x_param)
    y_cycle_col <- cluster_param_cycle_col(input$cluster_y_param)

    df_x <- df %>%
      filter(target_id == input$cluster_target_x) %>%
      transmute(
        source_file,
        run_key,
        sample_id,
        point_label,
        x_target = target_id,
        x_point = point_label,
        x_last_cycle = last_cycle,
        x_param_cycle = .data[[x_cycle_col]],
        x_value = .data[[input$cluster_x_param]]
      )

    df_y <- df %>%
      filter(target_id == input$cluster_target_y) %>%
      transmute(
        source_file,
        run_key,
        sample_id,
        point_label,
        y_target = target_id,
        y_point = point_label,
        y_last_cycle = last_cycle,
        y_param_cycle = .data[[y_cycle_col]],
        y_value = .data[[input$cluster_y_param]]
      )

    pairing_mode <- if (!is.null(input$cluster_pair_mode)) input$cluster_pair_mode else "across_runs"
    display_mode <- if (!is.null(input$cluster_display_mode)) input$cluster_display_mode else "sample_agg"

    if (identical(display_mode, "well_points")) {
      if (identical(pairing_mode, "within_run")) {
        df_x_join <- df_x %>%
          group_by(source_file, run_key, sample_id) %>%
          arrange(x_point, .by_group = TRUE) %>%
          mutate(pair_idx = row_number()) %>%
          ungroup()

        df_y_join <- df_y %>%
          group_by(source_file, run_key, sample_id) %>%
          arrange(y_point, .by_group = TRUE) %>%
          mutate(pair_idx = row_number()) %>%
          ungroup()

        out <- inner_join(df_x_join, df_y_join, by = c("source_file", "run_key", "sample_id", "pair_idx"))
      } else {
        df_x_join <- df_x %>%
          group_by(sample_id) %>%
          arrange(source_file, run_key, x_point, .by_group = TRUE) %>%
          mutate(pair_idx = row_number()) %>%
          ungroup()

        df_y_join <- df_y %>%
          group_by(sample_id) %>%
          arrange(source_file, run_key, y_point, .by_group = TRUE) %>%
          mutate(pair_idx = row_number()) %>%
          ungroup()

        out <- inner_join(df_x_join, df_y_join, by = c("sample_id", "pair_idx")) %>%
          transmute(
            source_file = dplyr::if_else(source_file.x == source_file.y, source_file.x, "(mehrere Dateien)"),
            run_key = dplyr::if_else(run_key.x == run_key.y, run_key.x, "(mehrere Runs)"),
            sample_id,
            point_label = paste0("Pair ", pair_idx),
            x_target,
            x_point,
            x_last_cycle,
            x_param_cycle,
            x_value,
            y_target,
            y_point,
            y_last_cycle,
            y_param_cycle,
            y_value
          )
      }

      if (nrow(out) > 0 && !all(c("point_label") %in% names(out))) {
        out <- out %>%
          mutate(
            point_label = dplyr::if_else(x_point == y_point, x_point, paste0(x_point, " | ", y_point))
          )
      }
    } else {
      if (identical(pairing_mode, "within_run")) {
        out <- inner_join(df_x, df_y, by = c("source_file", "run_key", "sample_id")) %>%
          mutate(point_label = sample_id)
      } else {
        df_x_sample <- df_x %>%
          group_by(sample_id, x_target) %>%
          summarise(
            source_file = cluster_single_or_multi(source_file, "(mehrere Dateien)"),
            run_key = cluster_single_or_multi(run_key, "(mehrere Runs)"),
            x_point = sample_id[1],
            x_last_cycle = max(x_last_cycle, na.rm = TRUE),
            x_param_cycle = max(x_param_cycle, na.rm = TRUE),
            x_value = mean(x_value, na.rm = TRUE),
            .groups = "drop"
          )

        df_y_sample <- df_y %>%
          group_by(sample_id, y_target) %>%
          summarise(
            source_file_y = cluster_single_or_multi(source_file, "(mehrere Dateien)"),
            run_key_y = cluster_single_or_multi(run_key, "(mehrere Runs)"),
            y_point = sample_id[1],
            y_last_cycle = max(y_last_cycle, na.rm = TRUE),
            y_param_cycle = max(y_param_cycle, na.rm = TRUE),
            y_value = mean(y_value, na.rm = TRUE),
            .groups = "drop"
          )

        out <- inner_join(df_x_sample, df_y_sample, by = "sample_id") %>%
          transmute(
            source_file = dplyr::if_else(source_file == source_file_y, source_file, "(mehrere Dateien)"),
            run_key = dplyr::if_else(run_key == run_key_y, run_key, "(mehrere Runs)"),
            sample_id,
            point_label = sample_id,
            x_target,
            x_point,
            x_last_cycle,
            x_param_cycle,
            x_value,
            y_target,
            y_point,
            y_last_cycle,
            y_param_cycle,
            y_value
          )
      }
    }

    x_cycle_label <- cluster_param_cycle_label(input$cluster_x_param)
    y_cycle_label <- cluster_param_cycle_label(input$cluster_y_param)

    out <- out %>%
      filter(!is.na(x_value), !is.na(y_value)) %>%
      mutate(
        last_cycle_diff = !is.na(x_last_cycle) & !is.na(y_last_cycle) & x_last_cycle != y_last_cycle,
        tooltip = paste0(
          "Datei: ", source_file,
          "<br>Run: ", run_key,
          "<br>Sample: ", sample_id,
          "<br>Punkt: ", point_label,
          "<br>X Target: ", x_target,
          "<br>X letzter Cycle: ", x_last_cycle,
          "<br>X ", x_cycle_label, ": ", x_param_cycle,
          "<br>X Wert: ", signif(x_value, 6),
          "<br>Y Target: ", y_target,
          "<br>Y letzter Cycle: ", y_last_cycle,
          "<br>Y ", y_cycle_label, ": ", y_param_cycle,
          "<br>Y Wert: ", signif(y_value, 6)
        )
      )

    validate(
      need(nrow(out) > 0, "Keine gemeinsamen Sample-Punkte fuer gewaehlte Targets/Parameter.")
    )

    out
  })

  cluster_cycle_summary <- reactive({
    df <- cluster_plot_data()

    tibble::tibble(
      x_last = cluster_cycle_range_text(df$x_last_cycle),
      y_last = cluster_cycle_range_text(df$y_last_cycle),
      x_param_cycle = cluster_cycle_range_text(df$x_param_cycle),
      y_param_cycle = cluster_cycle_range_text(df$y_param_cycle),
      n_points = nrow(df),
      n_last_diff = sum(df$last_cycle_diff, na.rm = TRUE)
    ) %>%
      slice(1)
  })

  output$cluster_cycle_info <- renderUI({
    info <- cluster_cycle_summary()

    warn_cycle <- !is.na(info$n_last_diff) && info$n_last_diff > 0
    warn_param <- !identical(input$cluster_x_param, "fluor_last") || !identical(input$cluster_y_param, "fluor_last")

    alert_class <- if (isTRUE(warn_cycle)) "alert alert-warning" else "alert alert-info"

    tags$div(
      class = alert_class,
      style = "margin-bottom: 0.7rem; padding: 0.55rem 0.8rem;",
      tags$strong("Cycle-Info"),
      tags$br(),
      tags$span(paste0("X letzter Cycle: ", info$x_last, " | Y letzter Cycle: ", info$y_last)),
      tags$br(),
      tags$span(
        paste0(
          "X ", cluster_param_cycle_label(input$cluster_x_param), ": ", info$x_param_cycle,
          " | Y ", cluster_param_cycle_label(input$cluster_y_param), ": ", info$y_param_cycle
        )
      ),
      if (isTRUE(warn_cycle)) {
        tagList(
          tags$br(),
          tags$strong(paste0("Hinweis: letzter Cycle unterscheidet sich bei ", info$n_last_diff, " von ", info$n_points, " Punkten."))
        )
      },
      if (isTRUE(warn_param)) {
        tagList(
          tags$br(),
          tags$span("Mindestens ein Parameter nutzt keinen direkten Letzt-Cycle-Wert.")
        )
      }
    )
  })

  cluster_plot_gg <- reactive({
    df <- cluster_plot_data()

    x_default <- paste0(cluster_param_label(input$cluster_x_param), " - ", input$cluster_target_x)
    y_default <- paste0(cluster_param_label(input$cluster_y_param), " - ", input$cluster_target_y)

    x_lab <- if (!is.null(input$cluster_x_lab) && nzchar(trimws(input$cluster_x_lab))) input$cluster_x_lab else x_default
    y_lab <- if (!is.null(input$cluster_y_lab) && nzchar(trimws(input$cluster_y_lab))) input$cluster_y_lab else y_default
    title_txt <- if (!is.null(input$cluster_title) && nzchar(trimws(input$cluster_title))) input$cluster_title else "Cluster"
    subtitle_txt <- if (!is.null(input$cluster_subtitle) && nzchar(trimws(input$cluster_subtitle))) input$cluster_subtitle else NULL

    color_param <- if (!is.null(input$cluster_color_param)) input$cluster_color_param else "none"

    if (identical(color_param, "none") || !(color_param %in% names(df))) {
      ggplot(df, aes(x = x_value, y = y_value, text = tooltip)) +
        geom_point(shape = 21, size = 3, alpha = 0.9, color = "#2c7fb8", fill = "#2c7fb8") +
        labs(
          title = title_txt,
          subtitle = subtitle_txt,
          x = x_lab,
          y = y_lab
        ) +
        theme_minimal(base_size = 12)
    } else {
      ggplot(df, aes(x = x_value, y = y_value, color = .data[[color_param]], fill = .data[[color_param]], text = tooltip)) +
        geom_point(shape = 21, size = 3, alpha = 0.9) +
        labs(
          title = title_txt,
          subtitle = subtitle_txt,
          x = x_lab,
          y = y_lab,
          color = "Gruppe",
          fill = "Gruppe"
        ) +
        theme_minimal(base_size = 12)
    }
  })

  output$cluster_plot_gg <- renderPlot({
    cluster_plot_gg()
  })

  output$cluster_plotly <- plotly::renderPlotly({
    ggplotly(cluster_plot_gg(), tooltip = "text")
  })

  output$cluster_table <- DT::renderDT({
    df <- cluster_plot_data() %>%
      select(
        source_file,
        run_key,
        sample_id,
        point_label,
        x_target,
        x_point,
        x_last_cycle,
        x_param_cycle,
        x_value,
        y_target,
        y_point,
        y_last_cycle,
        y_param_cycle,
        y_value
      )

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
