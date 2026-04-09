test_that("Versionsinformationen werden aus DESCRIPTION und Build-Variable gelesen", {
  helpers <- load_app_helpers()

  desc_path <- tempfile(fileext = ".dcf")
  writeLines(
    c(
      "Package: qPCRAnalysis",
      "Version: 1.2.3"
    ),
    con = desc_path,
    useBytes = TRUE
  )

  Sys.unsetenv("QPCRANALYSIS_VERSION")
  Sys.unsetenv("QPCRANALYSIS_BUILD")
  on.exit({
    Sys.unsetenv("QPCRANALYSIS_VERSION")
    Sys.unsetenv("QPCRANALYSIS_BUILD")
  }, add = TRUE)

  meta <- helpers$read_app_metadata(description_path = desc_path)
  expect_identical(meta$version, "1.2.3")
  expect_true(is.na(meta$build))
  expect_identical(helpers$format_app_version_label(meta), "Version 1.2.3")

  Sys.setenv(QPCRANALYSIS_BUILD = "184")
  meta_with_build <- helpers$read_app_metadata(description_path = desc_path)
  expect_identical(
    helpers$format_app_version_label(meta_with_build),
    "Version 1.2.3 (Build 184)"
  )
})

test_that("Analysedatensatz-Export erzeugt gueltige XLSX-Sheets mit Masterdaten", {
  helpers <- load_app_helpers()

  qpcr_all <- tibble::tibble(
    source_file = rep("run1.xlsx", 4),
    experiment_id = rep("exp1", 4),
    run_id = rep("runA", 4),
    well_position = c("A1", "A1", "A2", "A2"),
    `Sample Name` = rep("Sample_1", 4),
    `Target Name_res` = rep("GeneA", 4),
    Reporter = rep("FAM", 4),
    CRT = c("20.1", "20.1", "20.3", "20.3"),
    Quantity = rep("100", 4),
    Cycle = c("1", "2", "1", "2"),
    Rn = c("0.11", "0.25", "0.12", "0.28"),
    `Delta Rn` = c("0.02", "0.10", "0.03", "0.12")
  )

  qpcr_summary <- tibble::tibble(
    source_file = "run1.xlsx",
    `Target Name_res` = "GeneA",
    Reporter = "FAM",
    `Sample Name` = "Sample_1",
    Quantity = "100",
    Ct_mean = 20.2,
    Ct_sd = 0.1414214,
    n = 2,
    Target_ID = "GeneA [FAM]"
  )

  master_df <- helpers$build_analysis_master_data(qpcr_all, qpcr_summary)
  expect_gt(nrow(master_df), 0)
  expect_true(all(c("Ct_value", "Ct_mean", "rn_delta", "delta_rn_last", "replicate_n") %in% names(master_df)))
  expect_equal(unique(stats::na.omit(master_df$rn_delta)), c(0.14, 0.16), tolerance = 1e-8)

  export_df <- helpers$filter_analysis_master_data(
    master_df,
    target_filter = "GeneA [FAM]",
    sample_filter = "Sample_1"
  )
  expect_equal(nrow(export_df), nrow(master_df))

  version_info <- list(version = "1.2.3", build = NA_character_)
  bundle <- helpers$build_analysis_export_bundle(
    analysis_master = export_df,
    version_info = version_info,
    analysis_label = "exp1_runA",
    selected_files = "run1.xlsx",
    target_filter = "GeneA [FAM]",
    sample_filter = "Sample_1",
    quantity_missing_any = FALSE,
    quantity_missing_all = FALSE,
    separate_files = TRUE,
    export_time = as.POSIXct("2026-04-09 10:15:00", tz = "UTC")
  )

  out_path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(bundle, path = out_path)

  expect_true(file.exists(out_path))
  expect_setequal(
    readxl::excel_sheets(out_path),
    c("Analysedatensatz", "Metadaten", "Parameter", "Warnings")
  )

  exported_sheet <- readxl::read_xlsx(out_path, sheet = "Analysedatensatz")
  expect_gt(nrow(exported_sheet), 0)
  expect_true(all(c("Target_ID", "Ct_value", "Ct_mean", "rn_delta") %in% names(exported_sheet)))

  metadata_sheet <- readxl::read_xlsx(out_path, sheet = "Metadaten")
  expect_true(any(metadata_sheet$Wert == "1.2.3"))

  expect_identical(
    helpers$build_analysis_export_filename(
      analysis_label = "exp1_runA",
      version_info = version_info,
      export_date = as.Date("2026-04-09")
    ),
    "Analysedatensatz_exp1_runA_2026-04-09_v1.2.3.xlsx"
  )
})
