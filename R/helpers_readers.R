############################
# Hilfsfunktionen zum Einlesen
############################

# Einlesen einer qPCR-Excel-Datei:
# - "Results" (ab Zeile 45)
# - "Amplification Data" (ab Zeile 45)
# Join über Well-Position, mit Übernahme des Dateinamens (source_file)
# Unterstützt:
#   * QuantStudio-ähnliches Format mit Sheet "Results"
#   * AriaMX-Exports mit Sheet "Tabular Results"
read_qpcr_file <- function(path, source_name = basename(path)) {
  
  sheets <- excel_sheets(path)
  
  ## ----------------------------------------------------------
  ## FALL 1: QuantStudio-ähnliches Format (Sheet "Results")
  ## ----------------------------------------------------------
  if ("Results" %in% sheets) {
    
    # Results ab Zeile 45
    results_raw <- read_excel(
      path,
      sheet    = "Results",
      skip     = 44,
      col_names = TRUE
    )
    
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
    
    # Spalten, die später numerisch verwendet werden, zunächst als character erzwingen,
    # damit bind_rows() über mehrere Dateien funktioniert.
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
    
    return(joined)
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
        source_file       = source_name,
        .keep = "all"
      )
    
    # 2.2 Amplifikationssheet erkennen
    known_sheets <- c("Plate Setup", "Thermal Profile", "Tabular Results", "Experiment Notes")
    amp_sheet_candidates <- setdiff(sheets, known_sheets)
    if (length(amp_sheet_candidates) == 0) {
      # Kein Amplifikationssheet gefunden, dann nur Results zurückgeben
      return(results)
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
    
    return(joined)
  }
  
  stop(
    paste0(
      "Unbekanntes XLSX-Format in Datei: ", source_name,
      " (weder Sheet 'Results' noch 'Tabular Results' gefunden)."
    )
  )
}

# Einlesen der Melt Curve Daten (nur für QuantStudio-Format mit Sheet "Melt Curve Raw Data")
read_qpcr_melt_file <- function(path, source_name = basename(path)) {
  
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
