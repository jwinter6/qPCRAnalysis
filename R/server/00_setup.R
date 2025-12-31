  # Zentrale reaktive Werte:
  # - files_loaded: Upload-Phase erfolgreich
  # - data_loaded: Analyse-Daten für die aktuelle Auswahl existieren
  # - raw_qpcr_*: Rohdaten über alle geladenen Dateien
  # - qpcr_*: Daten nach Auswahl der Dateien (für Analysen)
  rv <- reactiveValues(
    files_loaded    = FALSE,
    data_loaded     = FALSE,
    raw_qpcr_all    = NULL,
    raw_qpcr_melt   = NULL,
    qpcr_all        = NULL,
    qpcr_summary    = NULL,
    qpcr_amp        = NULL,
    qpcr_melt       = NULL,
    file_overview   = NULL,
    available_files = NULL,
    has_delta_rn    = FALSE
  )
