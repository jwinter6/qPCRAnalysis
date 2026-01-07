tab_plate_qc <- bslib::nav_panel(
  "Plate Overview",
  value = "plate_overview",
  fluidRow(
    column(
      width = 12,
      bslib::card(
        bslib::card_header("Plate QC - Uebersicht"),
        bslib::card_body(
          selectInput(
            "plate_qc_file",
            "Datei auswaehlen",
            choices = NULL
          ),
          uiOutput("plate_qc_view")
        )
      )
    )
  )
)
