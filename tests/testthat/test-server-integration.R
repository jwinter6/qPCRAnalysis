test_that("Server-Workflow verarbeitet Uploads und Quantity-Filterung korrekt", {
  qs_file <- tempfile(fileext = ".xlsx")
  rdml_file <- tempfile(fileext = ".rdml")
  create_quantstudio_fixture(qs_file, include_quantity = TRUE, include_melt = TRUE)
  create_rdml_fixture(rdml_file, include_melt = TRUE)

  server_fun <- load_app_server()

  with_project_root(
    shiny::testServer(server_fun, {
      uploads <- data.frame(
        name = c("fixture_qs.xlsx", "fixture_rdml.rdml"),
        size = c(file.info(qs_file)$size, file.info(rdml_file)$size),
        type = c(
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          "application/xml"
        ),
        datapath = c(qs_file, rdml_file),
        stringsAsFactors = FALSE
      )

      session$setInputs(xlsx_files = uploads)
      session$setInputs(load_btn = 1)

      expect_true(isTRUE(rv$files_loaded))
      expect_false(isTRUE(rv$data_loaded))
      expect_true(all(c("quantity_run_status", "rdml_melt_status") %in% names(rv$file_overview)))

      rdml_status <- rv$file_overview %>%
        dplyr::filter(source_file == "fixture_rdml.rdml") %>%
        dplyr::pull(quantity_run_status)
      expect_length(rdml_status, 1)
      expect_match(rdml_status, "Quantity fehlt in 1/2 Run\\(s\\)")

      session$setInputs(selected_files = c("fixture_qs.xlsx", "fixture_rdml.rdml"))
      session$setInputs(analysis_btn = 1)

      expect_true(isTRUE(rv$data_loaded))
      expect_true(any(is.na(rv$qpcr_summary$Quantity)))

      rdml_summary <- rv$qpcr_summary %>%
        dplyr::filter(source_file == "fixture_rdml.rdml")
      expect_gt(nrow(rdml_summary), 0)
      expect_true(any(is.na(rdml_summary$Quantity)))
      expect_false(any(rdml_summary$Quantity == 0, na.rm = TRUE))

      session$setInputs(
        target_filter = sort(unique(rv$qpcr_summary$Target_ID)),
        sample_filter = sort(unique(rv$qpcr_summary$`Sample Name`)),
        separate_files = TRUE,
        ct_y_min = 10,
        ct_y_max = 45,
        y_scale_mode = "fixed"
      )

      fs <- filtered_summary()
      expect_gt(nrow(fs), 0)
      expect_true(any(is.na(fs$Quantity)))

      qty_df <- ctqty_quantity_data()
      expect_gt(nrow(qty_df), 0)
      expect_false(any(is.na(qty_df$Quantity)))

      sd_df <- ctsd_quantity_data()
      expect_gt(nrow(sd_df), 0)
      expect_false(any(is.na(sd_df$Quantity)))

      sc_df <- standardcurve_data()
      if (nrow(sc_df) > 0) {
        expect_true(all(is.na(sc_df$Q_min) | sc_df$Q_min > 0))
      }
    })
  )
})

test_that("Analyse mit vollstaendig fehlender Quantity liefert erwartete Hinweise", {
  ar_file <- tempfile(fileext = ".xlsx")
  create_ariamx_fixture(ar_file, include_quantity = FALSE)

  server_fun <- load_app_server()

  with_project_root(
    shiny::testServer(server_fun, {
      uploads <- data.frame(
        name = "fixture_noqty.xlsx",
        size = file.info(ar_file)$size,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        datapath = ar_file,
        stringsAsFactors = FALSE
      )

      session$setInputs(xlsx_files = uploads)
      session$setInputs(load_btn = 1)
      session$setInputs(selected_files = "fixture_noqty.xlsx")
      session$setInputs(analysis_btn = 1)

      expect_true(isTRUE(rv$data_loaded))
      expect_true(isTRUE(rv$quantity_missing_all))

      session$setInputs(
        target_filter = sort(unique(rv$qpcr_summary$Target_ID)),
        sample_filter = sort(unique(rv$qpcr_summary$`Sample Name`)),
        separate_files = TRUE,
        ct_y_min = 10,
        ct_y_max = 45,
        y_scale_mode = "fixed"
      )

      expect_error(
        ctqty_quantity_data(),
        "Keine Daten mit Quantity fuer Ct-vs-Quantity verfuegbar"
      )
      expect_error(
        ctsd_quantity_data(),
        "Keine Daten mit Quantity fuer Ct SD vs Quantity verfuegbar"
      )
    })
  )
})
