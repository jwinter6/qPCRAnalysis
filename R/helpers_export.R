first_existing_column <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    return(NA_character_)
  }

  hit[[1]]
}

ensure_character_columns <- function(df, columns) {
  for (col_name in columns) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <- NA_character_
    } else {
      df[[col_name]] <- as.character(df[[col_name]])
    }
  }

  df
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

last_value_by_order <- function(values, order_by) {
  idx <- which(!is.na(values) & !is.na(order_by))
  if (length(idx) == 0) {
    return(NA_real_)
  }

  values[idx[order(order_by[idx], idx)]][length(idx)]
}

sanitize_filename_component <- function(x, fallback = "analysis") {
  if (length(x) == 0 || all(is.na(x))) {
    return(fallback)
  }

  value <- x[[1]]
  if (is.na(value) || !nzchar(trimws(value))) {
    return(fallback)
  }

  value <- iconv(as.character(value), from = "", to = "ASCII//TRANSLIT")
  if (is.na(value) || !nzchar(trimws(value))) {
    return(fallback)
  }

  value <- gsub("[^A-Za-z0-9._-]+", "_", value)
  value <- gsub("_+", "_", value)
  value <- gsub("^_+|_+$", "", value)

  if (!nzchar(value)) fallback else value
}

build_analysis_context_label <- function(qpcr_all = NULL, selected_files = character()) {
  if (!is.null(qpcr_all) && nrow(qpcr_all) > 0) {
    experiment_values <- if ("experiment_id" %in% names(qpcr_all)) qpcr_all$experiment_id else NA_character_
    run_values <- if ("run_id" %in% names(qpcr_all)) qpcr_all$run_id else NA_character_
    source_values <- if ("source_file" %in% names(qpcr_all)) qpcr_all$source_file else NA_character_

    experiment_ids <- unique(stats::na.omit(trimws(as.character(experiment_values))))
    run_ids <- unique(stats::na.omit(trimws(as.character(run_values))))
    source_files <- unique(stats::na.omit(trimws(as.character(source_values))))

    if (length(experiment_ids) == 1 && length(run_ids) == 1) {
      return(sanitize_filename_component(paste(experiment_ids, run_ids, sep = "_")))
    }

    if (length(source_files) == 1) {
      return(sanitize_filename_component(tools::file_path_sans_ext(basename(source_files[[1]]))))
    }

    if (length(experiment_ids) == 1) {
      return(sanitize_filename_component(experiment_ids[[1]]))
    }

    if (length(run_ids) == 1) {
      return(sanitize_filename_component(run_ids[[1]]))
    }
  }

  if (length(selected_files) == 1) {
    return(sanitize_filename_component(tools::file_path_sans_ext(basename(selected_files[[1]]))))
  }

  if (length(selected_files) > 1) {
    return(paste0(length(selected_files), "_Dateien"))
  }

  "analysis"
}

build_analysis_export_filename <- function(
  analysis_label,
  version_info = read_app_metadata(),
  export_date = Sys.Date()
) {
  label <- sanitize_filename_component(analysis_label, fallback = "analysis")
  version_label <- sanitize_filename_component(version_info$version, fallback = "development")

  paste0(
    "Analysedatensatz_",
    label,
    "_",
    format(as.Date(export_date), "%Y-%m-%d"),
    "_v",
    version_label,
    ".xlsx"
  )
}

build_analysis_master_data <- function(qpcr_all, qpcr_summary = NULL) {
  if (is.null(qpcr_all) || nrow(qpcr_all) == 0) {
    return(tibble::tibble())
  }

  df <- ensure_character_columns(
    qpcr_all,
    c(
      "source_file",
      "experiment_id",
      "run_id",
      "well_position",
      "Sample Name",
      "Target Name_res",
      "Reporter",
      "CRT",
      "CT",
      "Crt Mean",
      "Quantity",
      "Cycle",
      "Rn",
      "Delta Rn",
      "DeltaRn",
      "dRn"
    )
  )

  delta_col <- first_existing_column(df, c("Delta Rn", "DeltaRn", "dRn"))

  df <- df %>%
    dplyr::mutate(
      Reporter = dplyr::na_if(Reporter, ""),
      `Target Name_res` = dplyr::na_if(`Target Name_res`, ""),
      `Sample Name` = dplyr::na_if(`Sample Name`, ""),
      Quantity_raw = Quantity,
      Ct_source = dplyr::case_when(
        !is.na(CRT) & nzchar(CRT) ~ "CRT",
        !is.na(CT) & nzchar(CT) ~ "CT",
        !is.na(`Crt Mean`) & nzchar(`Crt Mean`) ~ "Crt Mean",
        TRUE ~ NA_character_
      ),
      Ct_value = dplyr::case_when(
        Ct_source == "CRT" ~ safe_numeric(CRT),
        Ct_source == "CT" ~ safe_numeric(CT),
        Ct_source == "Crt Mean" ~ safe_numeric(`Crt Mean`),
        TRUE ~ NA_real_
      ),
      Quantity_num = safe_numeric(Quantity_raw),
      Cycle_num = safe_numeric(Cycle),
      Rn_value = safe_numeric(Rn),
      DeltaRn_value = if (!is.na(delta_col)) safe_numeric(.data[[delta_col]]) else NA_real_,
      row_type = dplyr::if_else(!is.na(Cycle_num), "amplification_cycle", "result_only"),
      Target_ID = dplyr::case_when(
        !is.na(`Target Name_res`) & !is.na(Reporter) ~ paste0(`Target Name_res`, " [", Reporter, "]"),
        !is.na(`Target Name_res`) ~ `Target Name_res`,
        TRUE ~ NA_character_
      ),
      run_key = dplyr::case_when(
        !is.na(experiment_id) & nzchar(experiment_id) & !is.na(run_id) & nzchar(run_id) ~ paste0(experiment_id, "::", run_id),
        !is.na(run_id) & nzchar(run_id) ~ run_id,
        TRUE ~ "(Datei-Run)"
      )
    )

  well_keys <- c(
    "source_file",
    "experiment_id",
    "run_id",
    "run_key",
    "well_position",
    "Sample Name",
    "Target Name_res",
    "Reporter",
    "Target_ID",
    "Quantity_num"
  )

  well_metrics <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(well_keys))) %>%
    dplyr::summarise(
      cycle_points = sum(!is.na(Cycle_num)),
      cycle_min = {
        vals <- Cycle_num[!is.na(Cycle_num)]
        if (length(vals) == 0) NA_real_ else min(vals)
      },
      cycle_max = {
        vals <- Cycle_num[!is.na(Cycle_num)]
        if (length(vals) == 0) NA_real_ else max(vals)
      },
      rn_min = {
        vals <- Rn_value[!is.na(Rn_value)]
        if (length(vals) == 0) NA_real_ else min(vals)
      },
      rn_max = {
        vals <- Rn_value[!is.na(Rn_value)]
        if (length(vals) == 0) NA_real_ else max(vals)
      },
      rn_delta = {
        vals <- Rn_value[!is.na(Rn_value)]
        if (length(vals) == 0) NA_real_ else max(vals) - min(vals)
      },
      rn_last = last_value_by_order(Rn_value, Cycle_num),
      delta_rn_min = {
        vals <- DeltaRn_value[!is.na(DeltaRn_value)]
        if (length(vals) == 0) NA_real_ else min(vals)
      },
      delta_rn_max = {
        vals <- DeltaRn_value[!is.na(DeltaRn_value)]
        if (length(vals) == 0) NA_real_ else max(vals)
      },
      delta_rn_delta = {
        vals <- DeltaRn_value[!is.na(DeltaRn_value)]
        if (length(vals) == 0) NA_real_ else max(vals) - min(vals)
      },
      delta_rn_last = last_value_by_order(DeltaRn_value, Cycle_num),
      .groups = "drop"
    )

  summary_metrics <- tibble::tibble()
  if (!is.null(qpcr_summary) && nrow(qpcr_summary) > 0) {
    summary_df <- ensure_character_columns(
      qpcr_summary,
      c("source_file", "Target Name_res", "Reporter", "Sample Name", "Quantity", "Target_ID", "Ct_mean", "Ct_sd", "n")
    )

    summary_metrics <- summary_df %>%
      dplyr::mutate(
        Reporter = dplyr::na_if(Reporter, ""),
        `Target Name_res` = dplyr::na_if(`Target Name_res`, ""),
        `Sample Name` = dplyr::na_if(`Sample Name`, ""),
        Target_ID = dplyr::case_when(
          !is.na(Target_ID) & nzchar(Target_ID) ~ Target_ID,
          !is.na(`Target Name_res`) & !is.na(Reporter) ~ paste0(`Target Name_res`, " [", Reporter, "]"),
          !is.na(`Target Name_res`) ~ `Target Name_res`,
          TRUE ~ NA_character_
        ),
        Quantity_num = safe_numeric(Quantity),
        Ct_mean = safe_numeric(Ct_mean),
        Ct_sd = safe_numeric(Ct_sd),
        replicate_n = safe_numeric(n)
      ) %>%
      dplyr::select(
        source_file,
        `Target Name_res`,
        Reporter,
        `Sample Name`,
        Quantity_num,
        Target_ID,
        Ct_mean,
        Ct_sd,
        replicate_n
      )
  }

  out <- df %>%
    dplyr::left_join(well_metrics, by = well_keys)

  if (nrow(summary_metrics) > 0) {
    out <- out %>%
      dplyr::left_join(
        summary_metrics,
        by = c(
          "source_file",
          "Target Name_res",
          "Reporter",
          "Sample Name",
          "Quantity_num",
          "Target_ID"
        )
      )
  }

  out %>%
    dplyr::select(
      dplyr::any_of(
        c(
          "source_file",
          "experiment_id",
          "run_id",
          "run_key",
          "well_position",
          "row_type",
          "Sample Name",
          "Target Name_res",
          "Reporter",
          "Target_ID",
          "Quantity_raw",
          "Quantity_num",
          "Ct_source",
          "Ct_value",
          "Ct_mean",
          "Ct_sd",
          "replicate_n",
          "Cycle_num",
          "Rn_value",
          "DeltaRn_value",
          "cycle_points",
          "cycle_min",
          "cycle_max",
          "rn_min",
          "rn_max",
          "rn_delta",
          "rn_last",
          "delta_rn_min",
          "delta_rn_max",
          "delta_rn_delta",
          "delta_rn_last"
        )
      ),
      dplyr::everything()
    ) %>%
    dplyr::arrange(source_file, experiment_id, run_id, well_position, Target_ID, Cycle_num)
}

filter_analysis_master_data <- function(df, target_filter = NULL, sample_filter = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  out <- tibble::as_tibble(df)

  if (!is.null(target_filter)) {
    if (length(target_filter) > 0) {
      out <- out %>% dplyr::filter(Target_ID %in% target_filter)
    } else {
      out <- out[0, , drop = FALSE]
    }
  }

  if (!is.null(sample_filter)) {
    if (length(sample_filter) > 0) {
      out <- out %>% dplyr::filter(`Sample Name` %in% sample_filter)
    } else {
      out <- out[0, , drop = FALSE]
    }
  }

  tibble::as_tibble(out)
}

format_filter_values <- function(values) {
  if (is.null(values)) {
    return("nicht gesetzt")
  }

  if (length(values) == 0) {
    return("keine Auswahl")
  }

  paste(values, collapse = ", ")
}

build_analysis_export_bundle <- function(
  analysis_master,
  version_info = read_app_metadata(),
  analysis_label,
  selected_files = character(),
  target_filter = NULL,
  sample_filter = NULL,
  quantity_missing_any = FALSE,
  quantity_missing_all = FALSE,
  separate_files = FALSE,
  export_time = Sys.time()
) {
  analysis_rows <- if (is.null(analysis_master)) 0L else nrow(analysis_master)
  analysis_cols <- if (is.null(analysis_master)) 0L else ncol(analysis_master)

  data_sheet <- if (is.null(analysis_master) || nrow(analysis_master) == 0) {
    tibble::tibble(Hinweis = "Keine Daten fuer den Analysedatensatz verfuegbar.")
  } else {
    tibble::as_tibble(analysis_master)
  }

  metadata_sheet <- tibble::tibble(
    Feld = c(
      "Export erstellt am",
      "App-Version",
      "Build",
      "Analyse-Label",
      "Ausgewaehlte Dateien",
      "Exportierte Zeilen",
      "Exportierte Spalten"
    ),
    Wert = c(
      format(as.POSIXct(export_time), "%Y-%m-%d %H:%M:%S %Z"),
      version_info$version,
      if (!is.null(version_info$build) && !is.na(version_info$build)) version_info$build else "-",
      analysis_label,
      if (length(selected_files) > 0) paste(selected_files, collapse = "; ") else "-",
      analysis_rows,
      analysis_cols
    )
  )

  parameter_sheet <- tibble::tibble(
    Parameter = c(
      "Target-Filter",
      "Sample-Filter",
      "Dateien getrennt anzeigen",
      "Quantity fehlt in Auswahl",
      "Quantity fehlt in allen Runs"
    ),
    Wert = c(
      format_filter_values(target_filter),
      format_filter_values(sample_filter),
      if (isTRUE(separate_files)) "Ja" else "Nein",
      if (isTRUE(quantity_missing_any)) "Ja" else "Nein",
      if (isTRUE(quantity_missing_all)) "Ja" else "Nein"
    )
  )

  warnings_text <- character()
  if (isTRUE(quantity_missing_any)) {
    warnings_text <- c(
      warnings_text,
      "Fehlende Quantity-Werte bleiben leer und werden in quantity-basierten Auswertungen ausgeschlossen."
    )
  }
  if (analysis_rows == 0) {
    warnings_text <- c(
      warnings_text,
      "Der Export enthaelt keine Datenzeilen. Bitte Analyse und aktive Filter pruefen."
    )
  }
  if (length(warnings_text) == 0) {
    warnings_text <- "Keine Warnungen."
  }

  warning_sheet <- tibble::tibble(Hinweis = warnings_text)

  list(
    Analysedatensatz = data_sheet,
    Metadaten = metadata_sheet,
    Parameter = parameter_sheet,
    Warnings = warning_sheet
  )
}
