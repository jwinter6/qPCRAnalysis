test_that("RDML-Erkennung nach Dateiendung funktioniert", {
  env <- load_reader_helpers()

  expect_true(env$is_rdml_upload("a.rdml"))
  expect_true(env$is_rdml_upload("a.xml"))
  expect_false(env$is_rdml_upload("a.xlsx"))
})

test_that("QuantStudio-Reader liest Haupt- und Melt-Daten aus Fixture", {
  env <- load_reader_helpers()
  f <- tempfile(fileext = ".xlsx")
  create_quantstudio_fixture(f, include_quantity = TRUE, include_melt = TRUE)

  main_df <- env$read_qpcr_file(f, source_name = "fixture_qs.xlsx")
  melt_df <- env$read_qpcr_melt_file(f, source_name = "fixture_qs.xlsx")

  expect_gt(nrow(main_df), 0)
  expect_true(all(c("well_position", "Target Name_res", "Sample Name", "CRT", "Quantity", "Cycle", "Rn") %in% names(main_df)))
  expect_true(any(!is.na(suppressWarnings(as.numeric(main_df$Quantity)))))

  expect_s3_class(melt_df, "data.frame")
  expect_gt(nrow(melt_df), 0)
  expect_true(all(c("Temperature", "Fluorescence", "Derivative", "Target_ID") %in% names(melt_df)))
})

test_that("AriaMX-Reader liest Tabular Results und Amplifikation aus Fixture", {
  env <- load_reader_helpers()
  f <- tempfile(fileext = ".xlsx")
  create_ariamx_fixture(f, include_quantity = TRUE)

  main_df <- env$read_qpcr_file(f, source_name = "fixture_aria.xlsx")

  expect_gt(nrow(main_df), 0)
  expect_true(all(c("well_position", "Sample Name", "Target Name_res", "Reporter", "CRT", "Quantity") %in% names(main_df)))
  expect_true(any(!is.na(suppressWarnings(as.numeric(main_df$`Delta Rn`)))))
})

test_that("RDML-Reader trennt Sample, Quantity, Reporter und Run korrekt", {
  env <- load_reader_helpers()
  f <- tempfile(fileext = ".rdml")
  create_rdml_fixture(f, include_melt = TRUE)

  main_df <- env$read_qpcr_file(f, source_name = "fixture.rdml")
  melt_df <- env$read_qpcr_melt_file(f, source_name = "fixture.rdml")

  expect_gt(nrow(main_df), 0)
  expect_true(all(c("experiment_id", "run_id", "Reporter", "Target_ID") %in% names(main_df)))
  expect_true(any(main_df$Reporter == "FAM", na.rm = TRUE))
  expect_true("Enzymmix1" %in% unique(main_df$`Sample Name`))
  expect_false("Enzymmix1_1.00E03" %in% unique(main_df$`Sample Name`))

  run2_df <- dplyr::filter(main_df, run_id == "run2")
  expect_gt(nrow(run2_df), 0)
  expect_true(all(is.na(suppressWarnings(as.numeric(run2_df$Quantity)))))

  expect_s3_class(melt_df, "data.frame")
  expect_gt(nrow(melt_df), 0)
  expect_true(all(c("Temperature", "Fluorescence", "Derivative", "Target_ID") %in% names(melt_df)))
})

test_that("ZIP-RDML wird gelesen, wenn System-zip verfuegbar ist", {
  env <- load_reader_helpers()
  z <- tempfile(fileext = ".rdml")
  create_rdml_zip_fixture(z)

  main_df <- env$read_qpcr_file(z, source_name = "fixture_zip.rdml")
  expect_gt(nrow(main_df), 0)
  expect_true("run_id" %in% names(main_df))
})

test_that("Unbekanntes XLSX-Format liefert klaren Fehler", {
  env <- load_reader_helpers()
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Foo = tibble::tibble(x = 1)), path = f)

  expect_error(
    env$read_qpcr_file(f, source_name = "invalid.xlsx"),
    "Unbekanntes Dateiformat"
  )
})
