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

      export_df <- analysis_export_data()
      expect_gt(nrow(export_df), 0)
      expect_true(all(c("Target_ID", "Ct_value", "Ct_mean", "rn_delta", "delta_rn_last") %in% names(export_df)))
      expect_true(all(export_df$Target_ID %in% input$target_filter))
      expect_true(all(export_df$`Sample Name` %in% input$sample_filter))

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

test_that("Cluster-Ansicht nutzt lokale Auswahl unabhaengig vom globalen Sample-Filter", {
  rdml_file <- tempfile(fileext = ".rdml")
  create_rdml_cluster_fixture(rdml_file)

  server_fun <- load_app_server()

  with_project_root(
    shiny::testServer(server_fun, {
      uploads <- data.frame(
        name = "fixture_cluster.rdml",
        size = file.info(rdml_file)$size,
        type = "application/xml",
        datapath = rdml_file,
        stringsAsFactors = FALSE
      )

      session$setInputs(xlsx_files = uploads)
      session$setInputs(load_btn = 1)
      session$setInputs(selected_files = "fixture_cluster.rdml")
      session$setInputs(analysis_btn = 1)

      expect_true(isTRUE(rv$data_loaded))

      targets <- sort(unique(rv$qpcr_amp$Target_ID))
      samples <- sort(unique(rv$qpcr_amp$`Sample Name`))
      target_x <- targets[grep("^GeneA", targets)][1]
      target_y <- targets[grep("^GeneB", targets)][1]
      sample_a <- "Sample_A"
      sample_b <- "Sample_B"

      expect_true(sample_a %in% samples)
      expect_true(sample_b %in% samples)
      expect_false(is.na(target_x))
      expect_false(is.na(target_y))

      # Globaler Filter schliesst Sample_B aus.
      session$setInputs(
        target_filter = targets,
        sample_filter = sample_a,
        separate_files = TRUE,
        ct_y_min = 10,
        ct_y_max = 45,
        y_scale_mode = "fixed"
      )

      global_amp <- filtered_amp()
      expect_false(sample_b %in% global_amp$`Sample Name`)

      # Cluster muss dennoch Sample_B liefern (lokaler Filter).
      session$setInputs(
        cluster_samples = sample_b,
        cluster_target_x = target_x,
        cluster_target_y = target_y,
        cluster_x_param = "fluor_last",
        cluster_y_param = "fluor_last",
        cluster_color_param = "none",
        cluster_pair_mode = "across_runs",
        cluster_display_mode = "sample_agg"
      )

      cdf <- cluster_plot_data()
      expect_equal(nrow(cdf), 1)
      expect_identical(cdf$sample_id[[1]], sample_b)
      expect_false(sample_a %in% cdf$sample_id)
      expect_equal(cdf$x_value[[1]], 0.28, tolerance = 1e-8)
      expect_equal(cdf$y_value[[1]], 0.31, tolerance = 1e-8)
    })
  )
})

test_that("Cluster-Ansicht paart Targets notfalls auf Sample-Ebene ueber Runs hinweg", {
  rdml_file <- tempfile(fileext = ".rdml")
  create_rdml_cluster_crossrun_fixture(rdml_file)

  server_fun <- load_app_server()

  with_project_root(
    shiny::testServer(server_fun, {
      uploads <- data.frame(
        name = "fixture_cluster_crossrun.rdml",
        size = file.info(rdml_file)$size,
        type = "application/xml",
        datapath = rdml_file,
        stringsAsFactors = FALSE
      )

      session$setInputs(xlsx_files = uploads)
      session$setInputs(load_btn = 1)
      session$setInputs(selected_files = "fixture_cluster_crossrun.rdml")
      session$setInputs(analysis_btn = 1)

      expect_true(isTRUE(rv$data_loaded))

      targets <- sort(unique(rv$qpcr_amp$Target_ID))
      target_x <- targets[grep("^GeneA", targets)][1]
      target_y <- targets[grep("^GeneB", targets)][1]

      session$setInputs(
        target_filter = targets,
        sample_filter = sort(unique(rv$qpcr_amp$`Sample Name`)),
        separate_files = TRUE,
        ct_y_min = 10,
        ct_y_max = 45,
        y_scale_mode = "fixed"
      )

      session$setInputs(
        cluster_samples = c("Sample_X", "Sample_Y"),
        cluster_target_x = target_x,
        cluster_target_y = target_y,
        cluster_x_param = "fluor_last",
        cluster_y_param = "fluor_last",
        cluster_color_param = "none",
        cluster_pair_mode = "within_run",
        cluster_display_mode = "sample_agg"
      )

      expect_error(
        cluster_plot_data(),
        "Keine gemeinsamen Sample-Punkte fuer gewaehlte Targets/Parameter."
      )

      session$setInputs(cluster_pair_mode = "across_runs")
      cdf <- cluster_plot_data()
      expect_equal(nrow(cdf), 2)
      expect_true(all(c("Sample_X", "Sample_Y") %in% cdf$sample_id))
      expect_true(all(cdf$run_key == "(mehrere Runs)"))
      expect_true(all(cdf$source_file == "fixture_cluster_crossrun.rdml"))
    })
  )
})

test_that("Cluster-Darstellungsmodus zeigt aggregiert vs. Well-Einzelpunkte", {
  rdml_file <- tempfile(fileext = ".rdml")
  create_rdml_cluster_well_fixture(rdml_file)

  server_fun <- load_app_server()

  with_project_root(
    shiny::testServer(server_fun, {
      uploads <- data.frame(
        name = "fixture_cluster_well.rdml",
        size = file.info(rdml_file)$size,
        type = "application/xml",
        datapath = rdml_file,
        stringsAsFactors = FALSE
      )

      session$setInputs(xlsx_files = uploads)
      session$setInputs(load_btn = 1)
      session$setInputs(selected_files = "fixture_cluster_well.rdml")
      session$setInputs(analysis_btn = 1)

      targets <- sort(unique(rv$qpcr_amp$Target_ID))
      target_x <- targets[grep("^GeneA", targets)][1]
      target_y <- targets[grep("^GeneB", targets)][1]

      session$setInputs(
        target_filter = targets,
        sample_filter = "Sample_A",
        separate_files = TRUE,
        ct_y_min = 10,
        ct_y_max = 45,
        y_scale_mode = "fixed",
        cluster_samples = "Sample_A",
        cluster_target_x = target_x,
        cluster_target_y = target_y,
        cluster_x_param = "fluor_last",
        cluster_y_param = "fluor_last",
        cluster_pair_mode = "within_run"
      )

      session$setInputs(cluster_display_mode = "sample_agg")
      agg_df <- cluster_plot_data()
      expect_equal(nrow(agg_df), 1)

      session$setInputs(cluster_display_mode = "well_points")
      well_df <- cluster_plot_data()
      expect_equal(nrow(well_df), 2)
      expect_true(all(c("x_last_cycle", "y_last_cycle", "x_param_cycle", "y_param_cycle") %in% names(well_df)))
      expect_true(all(!is.na(well_df$point_label)))
    })
  )
})
