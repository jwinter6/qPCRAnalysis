############################
# Hilfsfunktionen zum Einlesen
############################

# RDML-Dateien werden beim Upload u. a. über die Dateiendung erkannt.
is_rdml_upload <- function(path, source_name = basename(path)) {
  ext <- tolower(tools::file_ext(source_name))
  ext %in% c("rdml", "xml")
}

# Einheitliche Typen fuer formatuebergreifendes bind_rows()
normalize_qpcr_main_types <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  chr_cols <- c(
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
    "dRn",
    "source_file",
    "experiment_id",
    "run_id"
  )

  present <- intersect(chr_cols, names(df))
  if (length(present) == 0) return(df)

  df %>%
    mutate(across(all_of(present), as.character))
}

rdml_text_or_na <- function(node, xpath) {
  hit <- xml2::xml_find_first(node, xpath)
  if (inherits(hit, "xml_missing")) {
    return(NA_character_)
  }
  txt <- xml2::xml_text(hit, trim = TRUE)
  if (!nzchar(txt)) NA_character_ else txt
}

rdml_attr_or_na <- function(node, attr_name) {
  if (inherits(node, "xml_missing") || length(node) == 0) {
    return(NA_character_)
  }
  val <- xml2::xml_attr(node, attr_name)
  if (is.na(val) || !nzchar(val)) NA_character_ else val
}

rdml_normalize_sample_name <- function(sample_id, quantity_text = NA_character_, known_sample_refs = character()) {
  if (is.na(sample_id) || !nzchar(sample_id)) {
    return(NA_character_)
  }

  pattern <- "^(.+)_([0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?)$"
  m <- regexec(pattern, sample_id, perl = TRUE)
  parts <- regmatches(sample_id, m)[[1]]
  if (length(parts) != 3) {
    return(sample_id)
  }

  base_name <- parts[2]
  suffix_num <- suppressWarnings(as.numeric(parts[3]))
  qty_num <- suppressWarnings(as.numeric(quantity_text))

  base_seen <- length(known_sample_refs) > 0 && base_name %in% known_sample_refs
  qty_matches <- !is.na(suffix_num) && !is.na(qty_num) &&
    abs(suffix_num - qty_num) <= max(1e-6, abs(qty_num) * 1e-6)

  if (isTRUE(base_seen) || isTRUE(qty_matches)) {
    return(base_name)
  }

  sample_id
}

rdml_is_zip_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  sig <- tryCatch(readBin(path, what = "raw", n = 2), error = function(e) raw())
  length(sig) == 2 && identical(sig, charToRaw("PK"))
}

rdml_extract_xml_from_zip <- function(path) {
  zip_listing <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) {
      stop(paste0("ZIP-RDML konnte nicht gelesen werden: ", e$message))
    }
  )

  if (is.null(zip_listing) || nrow(zip_listing) == 0) {
    stop("ZIP-RDML ist leer und enthaelt keine Dateien.")
  }

  names_vec <- zip_listing$Name
  candidate_idx <- which(grepl("\\.(xml|rdml)$", tolower(names_vec)))
  target_name <- if (length(candidate_idx) > 0) names_vec[candidate_idx[1]] else names_vec[1]

  exdir <- tempfile("rdml_zip_")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

  unzip_ok <- tryCatch(
    {
      utils::unzip(path, files = target_name, exdir = exdir, overwrite = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )

  if (!isTRUE(unzip_ok)) {
    stop("XML aus ZIP-RDML konnte nicht entpackt werden.")
  }

  xml_path <- file.path(exdir, target_name)
  if (!file.exists(xml_path)) {
    extracted <- list.files(exdir, recursive = TRUE, full.names = TRUE)
    extracted_xml <- extracted[grepl("\\.(xml|rdml)$", tolower(extracted))]
    if (length(extracted_xml) == 0) {
      stop("ZIP-RDML enthaelt keine lesbare XML/RDML-Datei.")
    }
    xml_path <- extracted_xml[1]
  }

  xml_path
}

rdml_read_xml <- function(path) {
  xml_err <- NULL
  doc <- tryCatch(
    xml2::read_xml(path),
    error = function(e) {
      xml_err <<- e$message
      NULL
    }
  )

  if (!is.null(doc)) {
    return(doc)
  }

  if (!rdml_is_zip_file(path)) {
    stop(paste0("RDML-Datei konnte nicht gelesen werden: ", xml_err))
  }

  xml_path <- rdml_extract_xml_from_zip(path)

  tryCatch(
    xml2::read_xml(xml_path),
    error = function(e) {
      stop(
        paste0(
          "RDML-Datei konnte nicht gelesen werden: ",
          xml_err,
          " | ZIP-XML Fehler: ",
          e$message
        )
      )
    }
  )
}

# RDML react@id in Well-Position umsetzen (wenn pcrFormat ein klassisches Plattenlayout beschreibt)
rdml_react_id_to_well <- function(react_id, rows, cols, row_label, column_label) {
  react_num <- suppressWarnings(as.integer(react_id))
  if (is.na(react_num) || react_num <= 0) {
    return(as.character(react_id))
  }

  if (is.na(rows) || is.na(cols) || rows <= 0 || cols <= 0) {
    return(as.character(react_id))
  }

  # Bei Rotoren oder nicht-klassischen Label-Schemata bleibt die numerische ID erhalten.
  if (cols == 1 || row_label != "ABC" || column_label != "123") {
    return(as.character(react_id))
  }

  row_idx <- ((react_num - 1) %/% cols) + 1
  col_idx <- ((react_num - 1) %% cols) + 1

  if (row_idx > rows) {
    return(as.character(react_id))
  }

  if (rows <= length(LETTERS)) {
    row_txt <- LETTERS[row_idx]
  } else {
    row_txt <- paste0("R", row_idx)
  }

  paste0(row_txt, col_idx)
}

.rdml_bundle_cache <- new.env(parent = emptyenv())

# RDML einlesen und in dieselbe interne Struktur wie XLSX überführen.
read_qpcr_rdml_bundle <- function(path, source_name = basename(path)) {
  cache_key <- paste0(normalizePath(path, winslash = "/", mustWork = FALSE), "::", source_name)
  if (exists(cache_key, envir = .rdml_bundle_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .rdml_bundle_cache, inherits = FALSE))
  }

  doc <- rdml_read_xml(path)

  root <- xml2::xml_root(doc)
  if (tolower(xml2::xml_name(root)) != "rdml") {
    stop("Datei hat Endung .rdml/.xml, enthaelt aber kein <rdml>-Root-Element.")
  }

  react_sample_nodes <- xml2::xml_find_all(root, ".//*[local-name()='react']/*[local-name()='sample']")
  react_sample_ids <- unique(xml2::xml_attr(react_sample_nodes, "id"))
  react_sample_ids <- react_sample_ids[!is.na(react_sample_ids) & nzchar(react_sample_ids)]

  # Mapping: Samples (inkl. Quantity)
  sample_nodes <- xml2::xml_find_all(root, "./*[local-name()='sample']")
  sample_tbl <- tibble(
    sample_id = character(),
    sample_name = character(),
    quantity_text = character(),
    sample_type = character()
  )

  if (length(sample_nodes) > 0) {
    sample_ids <- xml2::xml_attr(sample_nodes, "id")
    sample_qty <- vapply(
      sample_nodes,
      function(node) {
        rdml_text_or_na(node, "./*[local-name()='quantity']/*[local-name()='value']")
      },
      character(1)
    )

    sample_tbl <- tibble(
      sample_id = sample_ids,
      sample_name = vapply(
        seq_along(sample_ids),
        function(i) {
          rdml_normalize_sample_name(
            sample_id = sample_ids[i],
            quantity_text = sample_qty[i],
            known_sample_refs = react_sample_ids
          )
        },
        character(1)
      ),
      quantity_text = sample_qty,
      sample_type = vapply(
        sample_nodes,
        function(node) {
          rdml_text_or_na(node, "./*[local-name()='type']")
        },
        character(1)
      )
    )
  }

  # Mapping: Dyes
  dye_nodes <- xml2::xml_find_all(root, "./*[local-name()='dye']")
  dye_tbl <- tibble(
    dye_id = character(),
    dye_label = character()
  )

  if (length(dye_nodes) > 0) {
    dye_id <- xml2::xml_attr(dye_nodes, "id")
    dye_desc <- vapply(
      dye_nodes,
      function(node) {
        rdml_text_or_na(node, "./*[local-name()='description']")
      },
      character(1)
    )

    dye_lbl <- ifelse(!is.na(dye_desc) & nzchar(dye_desc), dye_desc, dye_id)

    dye_tbl <- tibble(
      dye_id = dye_id,
      dye_label = dye_lbl
    )
  }

  # Mapping: Targets inkl. Reporter/Dye
  target_nodes <- xml2::xml_find_all(root, "./*[local-name()='target']")
  target_tbl <- tibble(
    target_id = character(),
    target_name = character(),
    reporter = character()
  )

  if (length(target_nodes) > 0) {
    target_id <- xml2::xml_attr(target_nodes, "id")
    dye_ref <- vapply(
      target_nodes,
      function(node) {
        dye_node <- xml2::xml_find_first(node, "./*[local-name()='dyeId']")
        rdml_attr_or_na(dye_node, "id")
      },
      character(1)
    )

    target_tbl <- tibble(
      target_id = target_id,
      target_name = target_id,
      dye_id = dye_ref
    ) %>%
      left_join(dye_tbl, by = "dye_id") %>%
      mutate(
        reporter = if_else(
          !is.na(dye_id) & nzchar(dye_id),
          dye_id,
          if_else(!is.na(dye_label) & nzchar(dye_label), dye_label, NA_character_)
        )
      ) %>%
      select(target_id, target_name, reporter)
  }

  experiment_nodes <- xml2::xml_find_all(root, "./*[local-name()='experiment']")
  if (length(experiment_nodes) == 0) {
    stop("RDML-Datei enthaelt keine <experiment>-Eintraege.")
  }

  main_rows <- list()
  melt_rows <- list()

  for (exp_idx in seq_along(experiment_nodes)) {
    exp_node <- experiment_nodes[[exp_idx]]
    exp_id <- rdml_attr_or_na(exp_node, "id")
    if (is.na(exp_id) || !nzchar(exp_id)) {
      exp_id <- paste0("experiment_", exp_idx)
    }

    run_nodes <- xml2::xml_find_all(exp_node, "./*[local-name()='run']")
    if (length(run_nodes) == 0) {
      next
    }

    for (run_idx in seq_along(run_nodes)) {
      run_node <- run_nodes[[run_idx]]
      run_id <- rdml_attr_or_na(run_node, "id")
      if (is.na(run_id) || !nzchar(run_id)) {
        run_id <- paste0("run_", run_idx)
      }

      rows_n <- suppressWarnings(as.integer(rdml_text_or_na(run_node, "./*[local-name()='pcrFormat']/*[local-name()='rows']")))
      cols_n <- suppressWarnings(as.integer(rdml_text_or_na(run_node, "./*[local-name()='pcrFormat']/*[local-name()='columns']")))
      row_label <- rdml_text_or_na(run_node, "./*[local-name()='pcrFormat']/*[local-name()='rowLabel']")
      col_label <- rdml_text_or_na(run_node, "./*[local-name()='pcrFormat']/*[local-name()='columnLabel']")
      if (is.na(row_label)) row_label <- "ABC"
      if (is.na(col_label)) col_label <- "123"

      react_nodes <- xml2::xml_find_all(run_node, "./*[local-name()='react']")
      if (length(react_nodes) == 0) {
        next
      }

      for (react_idx in seq_along(react_nodes)) {
        react_node <- react_nodes[[react_idx]]
        react_id <- rdml_attr_or_na(react_node, "id")
        if (is.na(react_id) || !nzchar(react_id)) {
          react_id <- as.character(react_idx)
        }

        well_position <- rdml_react_id_to_well(
          react_id = react_id,
          rows = rows_n,
          cols = cols_n,
          row_label = row_label,
          column_label = col_label
        )

        sample_node <- xml2::xml_find_first(react_node, "./*[local-name()='sample']")
        sample_ref <- rdml_attr_or_na(sample_node, "id")

        sample_name <- sample_ref
        sample_quantity_text <- NA_character_

        if (nrow(sample_tbl) > 0 && !is.na(sample_ref) && nzchar(sample_ref)) {
          s_idx <- match(sample_ref, sample_tbl$sample_id)
          if (!is.na(s_idx)) {
            sample_name <- sample_tbl$sample_name[s_idx]
            sample_quantity_text <- sample_tbl$quantity_text[s_idx]
          }
        }

        data_nodes <- xml2::xml_find_all(react_node, "./*[local-name()='data']")
        if (length(data_nodes) == 0) {
          next
        }

        for (data_idx in seq_along(data_nodes)) {
          data_node <- data_nodes[[data_idx]]

          tar_node <- xml2::xml_find_first(data_node, "./*[local-name()='tar']")
          target_ref <- rdml_attr_or_na(tar_node, "id")

          target_name <- target_ref
          reporter <- NA_character_

          if (nrow(target_tbl) > 0 && !is.na(target_ref) && nzchar(target_ref)) {
            t_idx <- match(target_ref, target_tbl$target_id)
            if (!is.na(t_idx)) {
              target_name <- target_tbl$target_name[t_idx]
              reporter <- target_tbl$reporter[t_idx]
            }
          }

          cq_text <- rdml_text_or_na(data_node, "./*[local-name()='cq']")
          cq_num <- suppressWarnings(as.numeric(cq_text))
          if (!is.na(cq_num) && cq_num < 0) {
            cq_text <- NA_character_
          }

          data_quantity_text <- rdml_text_or_na(data_node, "./*[local-name()='quantity']/*[local-name()='value']")
          quantity_text <- if (!is.na(data_quantity_text) && nzchar(data_quantity_text)) {
            data_quantity_text
          } else {
            sample_quantity_text
          }

          sample_name_data <- rdml_normalize_sample_name(
            sample_id = sample_name,
            quantity_text = quantity_text,
            known_sample_refs = react_sample_ids
          )

          adp_nodes <- xml2::xml_find_all(data_node, "./*[local-name()='adp']")

          if (length(adp_nodes) == 0) {
            main_rows[[length(main_rows) + 1]] <- tibble(
              source_file = source_name,
              experiment_id = exp_id,
              run_id = run_id,
              well_position = well_position,
              `Sample Name` = sample_name_data,
              `Target Name_res` = target_name,
              Reporter = reporter,
              CRT = cq_text,
              Quantity = quantity_text,
              Cycle = NA_real_,
              Rn = NA_real_,
              `Delta Rn` = NA_real_
            )
          } else {
            for (adp_node in adp_nodes) {
              cyc_num <- suppressWarnings(as.numeric(rdml_text_or_na(adp_node, "./*[local-name()='cyc']")))
              fluor_num <- suppressWarnings(as.numeric(rdml_text_or_na(adp_node, "./*[local-name()='fluor']")))

              main_rows[[length(main_rows) + 1]] <- tibble(
                source_file = source_name,
                experiment_id = exp_id,
                run_id = run_id,
                well_position = well_position,
                `Sample Name` = sample_name_data,
                `Target Name_res` = target_name,
                Reporter = reporter,
                CRT = cq_text,
                Quantity = quantity_text,
                Cycle = cyc_num,
                Rn = fluor_num,
                `Delta Rn` = NA_real_
              )
            }
          }

          mdp_nodes <- xml2::xml_find_all(data_node, "./*[local-name()='mdp']")
          if (length(mdp_nodes) > 0) {
            for (mdp_node in mdp_nodes) {
              tmp_num <- suppressWarnings(as.numeric(rdml_text_or_na(mdp_node, "./*[local-name()='tmp']")))
              fluor_num <- suppressWarnings(as.numeric(rdml_text_or_na(mdp_node, "./*[local-name()='fluor']")))

              melt_rows[[length(melt_rows) + 1]] <- tibble(
                source_file = source_name,
                experiment_id = exp_id,
                run_id = run_id,
                well_position = well_position,
                `Sample Name` = sample_name_data,
                `Target Name_res` = target_name,
                Reporter = reporter,
                Temperature = tmp_num,
                Fluorescence = fluor_num
              )
            }
          }
        }
      }
    }
  }

  main_df <- if (length(main_rows) > 0) {
    bind_rows(main_rows)
  } else {
    tibble()
  }

  if (nrow(main_df) == 0) {
    stop("RDML-Datei enthaelt keine auswertbaren Reaktionsdaten (<react>/<data>).")
  }

  main_df <- main_df %>%
    mutate(
      Target_ID = if_else(
        !is.na(Reporter) & Reporter != "",
        paste0(`Target Name_res`, " [", Reporter, "]"),
        `Target Name_res`
      )
    )

  melt_df <- if (length(melt_rows) > 0) {
    bind_rows(melt_rows)
  } else {
    tibble()
  }

  if (nrow(melt_df) > 0) {
    melt_df <- melt_df %>%
      mutate(
        Temperature = suppressWarnings(as.numeric(Temperature)),
        Fluorescence = suppressWarnings(as.numeric(Fluorescence))
      ) %>%
      arrange(source_file, experiment_id, run_id, well_position, `Target Name_res`, Temperature) %>%
      group_by(source_file, experiment_id, run_id, well_position, `Sample Name`, `Target Name_res`, Reporter) %>%
      mutate(
        temp_step = Temperature - lag(Temperature),
        fluor_step = Fluorescence - lag(Fluorescence),
        Derivative = if_else(
          !is.na(temp_step) & temp_step != 0,
          -(fluor_step / temp_step),
          NA_real_
        )
      ) %>%
      ungroup() %>%
      select(-temp_step, -fluor_step) %>%
      mutate(
        Target_ID = if_else(
          !is.na(Reporter) & Reporter != "",
          paste0(`Target Name_res`, " [", Reporter, "]"),
          `Target Name_res`
        )
      )
  }

  bundle <- list(
    main = main_df,
    melt = melt_df
  )

  assign(cache_key, bundle, envir = .rdml_bundle_cache)
  bundle
}

# Einlesen einer qPCR-Datei:
# - XLSX: QuantStudio oder AriaMX
# - RDML: XML-Format nach RDML-Schema
read_qpcr_file <- function(path, source_name = basename(path)) {

  if (is_rdml_upload(path, source_name)) {
    out <- read_qpcr_rdml_bundle(path, source_name = source_name)$main
    return(normalize_qpcr_main_types(out))
  }

  sheets <- excel_sheets(path)

  ## ----------------------------------------------------------
  ## FALL 1: QuantStudio-aehnliches Format (Sheet "Results")
  ## ----------------------------------------------------------
  if ("Results" %in% sheets) {

    # Results ab Zeile 45
    results_raw <- read_excel(
      path,
      sheet    = "Results",
      skip     = 44,
      col_names = TRUE
    )

    # QuantStudio kann Ct als "CT" oder "CRT" benennen.
    if ("CT" %in% names(results_raw) && !("CRT" %in% names(results_raw))) {
      names(results_raw)[names(results_raw) == "CT"] <- "CRT"
    }

    # Amplification Data ab Zeile 45, falls vorhanden
    if ("Amplification Data" %in% sheets) {
      amp_raw <- read_excel(
        path,
        sheet    = "Amplification Data",
        skip     = 44,
        col_names = TRUE
      )
    } else {
      amp_raw <- tibble()
    }

    # Spalten, die spaeter numerisch verwendet werden, zunaechst als character erzwingen,
    # damit bind_rows() ueber mehrere Dateien funktioniert.
    if ("CRT" %in% names(results_raw)) {
      results_raw <- results_raw %>% mutate(CRT = as.character(CRT))
    }
    if ("Crt Mean" %in% names(results_raw)) {
      results_raw <- results_raw %>% mutate(`Crt Mean` = as.character(`Crt Mean`))
    }
    if ("Quantity" %in% names(results_raw)) {
      results_raw <- results_raw %>% mutate(Quantity = as.character(Quantity))
    }

    # Well-Position vereinheitlichen
    results <- results_raw %>%
      rename(
        well_position     = matches("Well[ _-]?Position"),
        `Target Name_res` = `Target Name`
      )

    if (nrow(amp_raw) > 0) {
      amp <- amp_raw %>%
        rename(
          well_position = matches("Well[ _-]?Position")
        )
    } else {
      amp <- tibble()
    }

    if (nrow(amp) > 0) {
      joined <- results %>%
        left_join(
          amp,
          by = "well_position",
          suffix = c("_res", "_amp")
        )
    } else {
      joined <- results
    }

    joined <- joined %>%
      mutate(
        source_file = source_name
      )

    return(normalize_qpcr_main_types(joined))
  }

  ## ----------------------------------------------------------
  ## FALL 2: Neues Format – AriaMX Export
  ## ----------------------------------------------------------
  if ("Tabular Results" %in% sheets) {

    # 2.1 Tabular Results einlesen
    tab <- read_excel(
      path,
      sheet    = "Tabular Results",
      col_names = TRUE
    )

    # Cq und Quantity einheitlich als character
    if ("Cq (∆R)" %in% names(tab)) {
      tab <- tab %>% mutate(`Cq (∆R)` = as.character(`Cq (∆R)`))
    }
    if ("Quantity (nanograms)" %in% names(tab)) {
      tab <- tab %>% mutate(`Quantity (nanograms)` = as.character(`Quantity (nanograms)`))
    }

    # Mapping in internes Schema
    results <- tab %>%
      transmute(
        well_position     = as.character(Well),
        `Sample Name`     = as.character(`Well Name`),
        `Target Name_res` = as.character(Target),
        Reporter          = as.character(Dye),
        CRT               = if ("Cq (∆R)" %in% names(tab)) `Cq (∆R)` else NA_character_,
        Quantity          = if ("Quantity (nanograms)" %in% names(tab)) `Quantity (nanograms)` else NA_character_,
        source_file       = source_name
      )

    # 2.2 Amplifikationssheet erkennen
    known_sheets <- c("Plate Setup", "Thermal Profile", "Tabular Results", "Experiment Notes")
    amp_sheet_candidates <- setdiff(sheets, known_sheets)
    if (length(amp_sheet_candidates) == 0) {
      # Kein Amplifikationssheet gefunden, dann nur Results zurueckgeben
      return(normalize_qpcr_main_types(results))
    }

    amp_sheet <- amp_sheet_candidates[1]

    amp_raw <- read_excel(
      path,
      sheet    = amp_sheet,
      col_names = FALSE
    )

    # AriaMX Amplifikationsdaten liegen blockweise pro Well vor.
    amp_list <- list()
    current_well <- NA_character_

    for (i in seq_len(nrow(amp_raw))) {
      row <- amp_raw[i, ]
      first_col <- as.character(row[[1]])

      # Kopfzeile: Well-Kennung, z. B. "A1, Replicate 1, ..."
      if (!is.na(first_col) && grepl("^[A-H][0-9]+", first_col)) {
        well <- sub(",.*$", "", first_col)
        current_well <- well
        next
      }

      # Datenzeilen: numeric in den ersten Spalten, current_well gesetzt
      if (!is.na(current_well)) {
        cycle_val <- suppressWarnings(as.numeric(row[[1]]))
        drn_val   <- suppressWarnings(as.numeric(row[[2]]))

        if (!is.na(cycle_val) && !is.na(drn_val)) {
          amp_list[[length(amp_list) + 1]] <- tibble(
            well_position = current_well,
            Cycle         = cycle_val,
            Rn            = NA_real_,  # AriaMX liefert direkt DeltaRn
            `Delta Rn`    = drn_val,
            source_file   = source_name
          )
        }
      }
    }

    if (length(amp_list) > 0) {
      amp <- bind_rows(amp_list)
    } else {
      amp <- tibble()
    }

    if (nrow(amp) > 0) {
      joined <- results %>%
        left_join(
          amp,
          by = c("well_position", "source_file")
        )
    } else {
      joined <- results
    }

    return(normalize_qpcr_main_types(joined))
  }

  stop(
    paste0(
      "Unbekanntes Dateiformat in Datei: ", source_name,
      " (weder RDML noch XLSX mit Sheet 'Results' oder 'Tabular Results' erkannt)."
    )
  )
}

# Einlesen der Melt Curve Daten:
# - XLSX (QuantStudio): Sheet "Melt Curve Raw Data"
# - RDML: mdp-Daten aus react/data
read_qpcr_melt_file <- function(path, source_name = basename(path)) {

  if (is_rdml_upload(path, source_name)) {
    melt_df <- read_qpcr_rdml_bundle(path, source_name = source_name)$melt
    if (is.null(melt_df) || nrow(melt_df) == 0) {
      return(NULL)
    }
    return(melt_df)
  }

  sheets <- excel_sheets(path)

  if (!("Melt Curve Raw Data" %in% sheets)) {
    return(NULL)
  }

  # Melt Curve Raw Data ab Zeile 45
  melt_raw <- read_excel(
    path,
    sheet    = "Melt Curve Raw Data",
    skip     = 44,
    col_names = TRUE
  )

  if (nrow(melt_raw) == 0) {
    return(NULL)
  }

  # Results ab Zeile 45, um Sample/Target/Reporter dazuzujoinen
  results_raw <- read_excel(
    path,
    sheet    = "Results",
    skip     = 44,
    col_names = TRUE
  )

  if (nrow(results_raw) == 0) {
    return(NULL)
  }

  results <- results_raw %>%
    rename(
      well_position     = matches("Well[ _-]?Position"),
      `Target Name_res` = `Target Name`
    ) %>%
    mutate(
      source_file = source_name
    )

  melt <- melt_raw %>%
    rename(
      well_position = matches("Well[ _-]?Position")
    ) %>%
    mutate(
      source_file = source_name
    )

  joined_melt <- melt %>%
    left_join(
      results,
      by = c("well_position", "source_file"),
      suffix = c("_melt", "_res")
    ) %>%
    mutate(
      Temperature  = suppressWarnings(as.numeric(Temperature)),
      Fluorescence = suppressWarnings(as.numeric(Fluorescence)),
      Derivative   = suppressWarnings(as.numeric(Derivative)),
      `Target Name_res` = as.character(`Target Name_res`),
      Reporter     = if ("Reporter" %in% names(.)) as.character(Reporter) else NA_character_,
      Target_ID    = if_else(
        !is.na(Reporter),
        paste0(`Target Name_res`, " [", Reporter, "]"),
        `Target Name_res`
      )
    )

  joined_melt
}
