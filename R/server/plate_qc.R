  ##########################
  # Plate QC
  ##########################
  
  observe({
    if (!isTRUE(rv$files_loaded) || is.null(rv$available_files)) {
      updateSelectInput(session, "plate_qc_file", choices = character(0))
      return()
    }
    updateSelectInput(
      session,
      "plate_qc_file",
      choices = rv$available_files,
      selected = rv$available_files[1]
    )
  })
  
  plate_qc_data <- reactive({
    validate(
      need(rv$data_loaded, "Bitte im Tab 'Daten laden' Dateien laden und 'Analyse starten' klicken.")
    )
    req(rv$raw_qpcr_all, input$plate_qc_file)
    
    df <- rv$raw_qpcr_all %>%
      dplyr::filter(source_file == input$plate_qc_file)
    if (nrow(df) == 0) return(tibble())
    
    well_type_source <- if ("WellType" %in% names(df)) {
      "WellType"
    } else if ("Task" %in% names(df)) {
      "Task"
    } else {
      NA_character_
    }
    
    df <- df %>%
      mutate(
        well_position = as.character(well_position),
        Quantity = suppressWarnings(as.numeric(Quantity)),
        Quantity = if_else(is.na(Quantity), 0, Quantity),
        Reporter = if ("Reporter" %in% names(.)) as.character(Reporter) else NA_character_,
        Well_Type = if (!is.na(well_type_source)) as.character(.data[[well_type_source]]) else NA_character_
      )
    
    out <- df %>%
      group_by(well_position) %>%
      summarise(
        Sample   = paste(sort(unique(na.omit(`Sample Name`))), collapse = "/"),
        Target   = paste(sort(unique(na.omit(`Target Name_res`))), collapse = "/"),
        Dye      = paste(sort(unique(na.omit(Reporter))), collapse = "/"),
        Quantity = paste(sort(unique(Quantity)), collapse = "/"),
        Well_Type = paste(sort(unique(na.omit(Well_Type))), collapse = "/"),
        .groups  = "drop"
      )
    
    attr(out, "well_type_label") <- if (!is.na(well_type_source)) well_type_source else "Well Type"
    out
  })
  
  plate_layout_info <- function(df) {
    if (nrow(df) == 0) return(list(rows = character(0), cols = integer(0)))
    parts <- stringr::str_match(df$well_position, "^\\s*([A-P])\\s*0*([0-9]+)\\s*$")
    row_letters <- parts[, 2]
    col_nums <- suppressWarnings(as.integer(parts[, 3]))
    
    row_letters <- row_letters[!is.na(row_letters)]
    col_nums <- col_nums[!is.na(col_nums)]
    if (length(row_letters) == 0 || length(col_nums) == 0) {
      return(list(rows = character(0), cols = integer(0)))
    }
    
    max_row <- max(match(row_letters, LETTERS), na.rm = TRUE)
    max_col <- max(col_nums, na.rm = TRUE)
    
    if (max_row <= 8 && max_col <= 12) {
      list(rows = LETTERS[1:8], cols = 1:12)
    } else {
      list(rows = LETTERS[1:16], cols = 1:24)
    }
  }
  
  format_cell_text <- function(label, value) {
    value_text <- ifelse(is.na(value) || value == "", "-", value)
    paste0("<div class='plate-qc-line'><strong>", label, ":</strong> ", value_text, "</div>")
  }
  
  format_hover_text <- function(well, well_type, sample, target, dye, quantity) {
    paste(
      "Well:", well,
      "| Type:", ifelse(is.na(well_type) || well_type == "", "-", well_type),
      "| Sample:", ifelse(is.na(sample) || sample == "", "-", sample),
      "| Target:", ifelse(is.na(target) || target == "", "-", target),
      "| Dye:", ifelse(is.na(dye) || dye == "", "-", dye),
      "| Quantity:", ifelse(is.na(quantity) || quantity == "", "-", quantity)
    )
  }
  
  color_map_for <- function(values) {
    vals <- sort(unique(na.omit(values)))
    if (length(vals) == 0) return(list(values = vals, colors = character(0)))
    cols <- grDevices::hcl.colors(length(vals), palette = "Set2")
    list(values = vals, colors = setNames(cols, vals))
  }
  
  build_plate_table <- function(df_lookup, layout, field, color_map, label = field) {
    table_rows <- lapply(layout$rows, function(r) {
      row_cells <- lapply(layout$cols, function(c) {
        cell <- df_lookup %>%
          filter(row_letter == r, col_num == c)
        if (nrow(cell) == 0) {
          return(tags$td(class = "plate-qc-empty", "\u00A0"))
        }
        
        cell_value <- cell[[field]][1]
        hover_text <- format_hover_text(
          well = cell$well_position[1],
          well_type = cell$Well_Type[1],
          sample = cell$Sample[1],
          target = cell$Target[1],
          dye = cell$Dye[1],
          quantity = cell$Quantity[1]
        )
        bg <- if (!is.null(color_map$colors[[cell_value]])) color_map$colors[[cell_value]] else NA_character_
        style <- if (!is.na(bg)) paste0("background-color:", bg, ";") else NULL
        
        tags$td(
          title = hover_text,
          style = style,
          HTML(format_cell_text(label = label, value = cell_value))
        )
      })
      
      tags$tr(tags$th(r), row_cells)
    })
    
    tags$table(
      class = "plate-qc-table",
      tags$thead(
        tags$tr(
          tags$th(""),
          lapply(layout$cols, function(c) tags$th(c))
        )
      ),
      tags$tbody(table_rows)
    )
  }
  
  output$plate_qc_view <- renderUI({
    df <- plate_qc_data()
    validate(
      need(nrow(df) > 0, "Keine Daten fuer Plate QC verfuegbar.")
    )
    
    layout <- plate_layout_info(df)
    validate(
      need(length(layout$rows) > 0, "Well-Positionen konnten nicht erkannt werden.")
    )
    
    parts <- stringr::str_match(df$well_position, "^\\s*([A-P])\\s*0*([0-9]+)\\s*$")
    df <- df %>%
      mutate(
        row_letter = parts[, 2],
        col_num = suppressWarnings(as.integer(parts[, 3]))
      )
    
    df_lookup <- df %>%
      filter(!is.na(row_letter), !is.na(col_num))
    
    sample_colors <- color_map_for(df_lookup$Sample)
    target_colors <- color_map_for(df_lookup$Target)
    dye_colors <- color_map_for(df_lookup$Dye)
    quantity_colors <- color_map_for(df_lookup$Quantity)
    welltype_colors <- color_map_for(df_lookup$Well_Type)
    well_type_label <- attr(df, "well_type_label")
    
    tagList(
      tags$style(
        ".plate-qc-table{border-collapse:collapse;width:100%;table-layout:fixed;font-size:11px;}",
        ".plate-qc-table th,.plate-qc-table td{border:1px solid #ddd;padding:4px;vertical-align:top;}",
        ".plate-qc-table th{background:#f5f5f5;text-align:center;}",
        ".plate-qc-table td{height:70px;}",
        ".plate-qc-line{line-height:1.2;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}",
        ".plate-qc-empty{background:#fafafa;}"
      ),
      tags$div(
        class = "text-muted",
        "Hover ueber einem Well zeigt Details (Well/Type/Sample/Target/Dye/Quantity)."
      ),
      bslib::card(
        bslib::card_header("Targets"),
        bslib::card_body(
          tags$div(style = "overflow:auto;", build_plate_table(df_lookup, layout, "Target", target_colors)),
          br(),
          actionButton("add_report_plate_targets", "Zum Report hinzufuegen")
        )
      ),
      bslib::card(
        bslib::card_header("Samples"),
        bslib::card_body(
          tags$div(style = "overflow:auto;", build_plate_table(df_lookup, layout, "Sample", sample_colors)),
          br(),
          actionButton("add_report_plate_samples", "Zum Report hinzufuegen")
        )
      ),
      bslib::card(
        bslib::card_header("Farbstoffe"),
        bslib::card_body(
          tags$div(style = "overflow:auto;", build_plate_table(df_lookup, layout, "Dye", dye_colors)),
          br(),
          actionButton("add_report_plate_dye", "Zum Report hinzufuegen")
        )
      ),
      bslib::card(
        bslib::card_header("Quantity"),
        bslib::card_body(
          tags$div(style = "overflow:auto;", build_plate_table(df_lookup, layout, "Quantity", quantity_colors)),
          br(),
          actionButton("add_report_plate_quantity", "Zum Report hinzufuegen")
        )
      ),
      bslib::card(
        bslib::card_header(well_type_label),
        bslib::card_body(
          tags$div(
            style = "overflow:auto;",
            build_plate_table(df_lookup, layout, "Well_Type", welltype_colors, well_type_label)
          ),
          br(),
          actionButton("add_report_plate_welltype", "Zum Report hinzufuegen")
        )
      )
    )
  })
  
  plate_qc_report_table <- function(field) {
    df <- plate_qc_data()
    if (nrow(df) == 0) return(tibble())
    df %>%
      dplyr::transmute(
        well_position = well_position,
        value = .data[[field]]
      )
  }
  
  observeEvent(input$add_report_plate_targets, {
    withProgress(message = "Fuege Platte zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Daten aufbereiten")
      out <- plate_qc_report_table("Target")
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Plate Overview - Targets",
        tab = "Plate Overview",
        type = "table",
        data = out
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Platte zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_plate_samples, {
    withProgress(message = "Fuege Platte zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Daten aufbereiten")
      out <- plate_qc_report_table("Sample")
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Plate Overview - Samples",
        tab = "Plate Overview",
        type = "table",
        data = out
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Platte zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_plate_dye, {
    withProgress(message = "Fuege Platte zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Daten aufbereiten")
      out <- plate_qc_report_table("Dye")
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Plate Overview - Farbstoffe",
        tab = "Plate Overview",
        type = "table",
        data = out
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Platte zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_plate_quantity, {
    withProgress(message = "Fuege Platte zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Daten aufbereiten")
      out <- plate_qc_report_table("Quantity")
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = "Plate Overview - Quantity",
        tab = "Plate Overview",
        type = "table",
        data = out
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Platte zum Report hinzugefuegt.", type = "message", duration = 4)
  })
  
  observeEvent(input$add_report_plate_welltype, {
    withProgress(message = "Fuege Platte zum Report hinzu", value = 0, {
      incProgress(0.4, detail = "Daten aufbereiten")
      out <- plate_qc_report_table("Well_Type")
      well_type_label <- attr(plate_qc_data(), "well_type_label")
      if (is.null(well_type_label) || is.na(well_type_label)) {
        well_type_label <- "Well Type"
      }
      incProgress(0.4, detail = "Speichern")
      report_add_item(
        title = paste("Plate Overview -", well_type_label),
        tab = "Plate Overview",
        type = "table",
        data = out
      )
      incProgress(0.2, detail = "Fertig")
    })
    showNotification("Platte zum Report hinzugefuegt.", type = "message", duration = 4)
  })
