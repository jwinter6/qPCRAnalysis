if (!exists("build_skip44_sheet")) {
  build_skip44_sheet <- function(header, rows) {
    n_cols <- length(header)
    stopifnot(n_cols > 0)

    prefix <- matrix("", nrow = 43, ncol = n_cols)
    header_row <- matrix(as.character(header), nrow = 1)

    data_rows <- if (length(rows) == 0) {
      matrix(nrow = 0, ncol = n_cols)
    } else {
      do.call(
        rbind,
        lapply(rows, function(x) {
          x_chr <- as.character(x)
          if (length(x_chr) != n_cols) {
            stop("Zeilenlaenge passt nicht zur Header-Laenge.")
          }
          x_chr
        })
      )
    }

    out <- as.data.frame(
      rbind(prefix, header_row, data_rows),
      stringsAsFactors = FALSE
    )
    names(out) <- paste0("X", seq_len(n_cols))
    out
  }
}

if (!exists("create_quantstudio_fixture")) {
  create_quantstudio_fixture <- function(path, include_quantity = TRUE, include_melt = TRUE) {
    results_header <- c("Well Position", "Target Name", "Sample Name", "Reporter", "CRT", "Quantity")
    results_rows <- list(
      c("A1", "GeneA", "QS_Sample", "FAM", "20.1", if (include_quantity) "100" else ""),
      c("A2", "GeneA", "QS_Sample", "FAM", "20.3", if (include_quantity) "100" else ""),
      c("A3", "GeneA", "QS_Sample", "FAM", "23.1", if (include_quantity) "10" else ""),
      c("A4", "GeneA", "QS_Sample", "FAM", "23.4", if (include_quantity) "10" else "")
    )
    results_sheet <- build_skip44_sheet(results_header, results_rows)

    amp_header <- c("Well Position", "Cycle", "Rn", "Delta Rn")
    amp_rows <- list(
      c("A1", "1", "0.11", "0.02"),
      c("A1", "2", "0.24", "0.09"),
      c("A2", "1", "0.12", "0.03"),
      c("A2", "2", "0.25", "0.10")
    )
    amp_sheet <- build_skip44_sheet(amp_header, amp_rows)

    sheets <- list(
      Results = results_sheet,
      `Amplification Data` = amp_sheet
    )

    if (isTRUE(include_melt)) {
      melt_header <- c("Well Position", "Temperature", "Fluorescence", "Derivative")
      melt_rows <- list(
        c("A1", "70.0", "0.95", "-0.02"),
        c("A1", "71.0", "0.90", "-0.05"),
        c("A1", "72.0", "0.82", "-0.08")
      )
      sheets$`Melt Curve Raw Data` <- build_skip44_sheet(melt_header, melt_rows)
    }

    writexl::write_xlsx(sheets, path = path)
    path
  }
}

if (!exists("create_ariamx_fixture")) {
  create_ariamx_fixture <- function(path, include_quantity = TRUE) {
    tab <- tibble::tibble(
      Well = c("A1", "A2", "A3"),
      `Well Name` = c("AR_Sample", "AR_Sample", "AR_Sample"),
      Target = c("GeneA", "GeneA", "GeneA"),
      Dye = c("FAM", "FAM", "FAM"),
      `Cq (∆R)` = c("19.8", "22.9", "26.1"),
      `Quantity (nanograms)` = if (include_quantity) c("100", "10", "1") else c(NA, NA, NA)
    )

    amp <- tibble::tibble(
      col1 = c(
        "A1, Replicate 1, SYBR/FAM",
        "1",
        "2",
        "A2, Replicate 1, SYBR/FAM",
        "1",
        "2"
      ),
      col2 = c("", "0.05", "0.10", "", "0.04", "0.09")
    )

    writexl::write_xlsx(
      list(
        `Tabular Results` = tab,
        `Raw Amplification` = amp
      ),
      path = path
    )
    path
  }
}

if (!exists("create_rdml_fixture")) {
  create_rdml_fixture <- function(path, include_melt = TRUE) {
    melt_run1 <- if (isTRUE(include_melt)) {
      paste0(
        "<mdp><tmp>70.0</tmp><fluor>0.95</fluor></mdp>",
        "<mdp><tmp>71.0</tmp><fluor>0.89</fluor></mdp>",
        "<mdp><tmp>72.0</tmp><fluor>0.80</fluor></mdp>"
      )
    } else {
      ""
    }

    xml_txt <- paste0(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<rdml version=\"1.1\">",
      "<dye id=\"FAM\"><description>FAM</description></dye>",
      "<target id=\"GeneA\"><dyeId id=\"FAM\"/></target>",
      "<sample id=\"Enzymmix1_1.00E03\"><quantity><value>1.00E03</value></quantity></sample>",
      "<sample id=\"NoQtySample\"></sample>",
      "<experiment id=\"exp1\">",
      "<run id=\"run1\">",
      "<pcrFormat><rows>8</rows><columns>12</columns><rowLabel>ABC</rowLabel><columnLabel>123</columnLabel></pcrFormat>",
      "<react id=\"1\"><sample id=\"Enzymmix1_1.00E03\"/><data>",
      "<tar id=\"GeneA\"/><cq>21.2</cq>",
      "<adp><cyc>1</cyc><fluor>0.11</fluor></adp>",
      "<adp><cyc>2</cyc><fluor>0.23</fluor></adp>",
      melt_run1,
      "</data></react>",
      "</run>",
      "<run id=\"run2\">",
      "<pcrFormat><rows>8</rows><columns>12</columns><rowLabel>ABC</rowLabel><columnLabel>123</columnLabel></pcrFormat>",
      "<react id=\"2\"><sample id=\"NoQtySample\"/><data>",
      "<tar id=\"GeneA\"/><cq>24.7</cq>",
      "<adp><cyc>1</cyc><fluor>0.08</fluor></adp>",
      "<adp><cyc>2</cyc><fluor>0.16</fluor></adp>",
      "</data></react>",
      "</run>",
      "</experiment>",
      "</rdml>"
    )

    writeLines(xml_txt, con = path, useBytes = TRUE)
    path
  }
}

if (!exists("create_rdml_zip_fixture")) {
  create_rdml_zip_fixture <- function(path) {
    zip_bin <- Sys.which("zip")
    testthat::skip_if_not(nzchar(zip_bin), "Systemkommando 'zip' ist nicht verfuegbar.")

    tmp_dir <- tempfile("rdml_zip_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    xml_path <- file.path(tmp_dir, "fixture.rdml")
    create_rdml_fixture(xml_path, include_melt = TRUE)

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(tmp_dir)
    utils::zip(zipfile = path, files = "fixture.rdml")
    path
  }
}
