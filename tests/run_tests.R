#!/usr/bin/env Rscript

script_path <- {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- args[grepl(file_arg, args)]
  if (length(hit) == 0) {
    normalizePath("tests/run_tests.R", mustWork = FALSE)
  } else {
    sub(file_arg, "", hit[1], fixed = TRUE)
  }
}

project_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
setwd(project_root)

required_pkgs <- c(
  "testthat",
  "shiny",
  "writexl",
  "readxl",
  "xml2",
  "dplyr",
  "tidyr",
  "tibble",
  "stringr"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    paste0(
      "Fehlende R-Pakete fuer Tests: ",
      paste(missing_pkgs, collapse = ", "),
      ". Bitte zuerst installieren."
    )
  )
}

cat("Starte automatisierte Tests in:", file.path(project_root, "tests", "testthat"), "\n")

test_dir_args <- list(
  path = file.path(project_root, "tests", "testthat"),
  reporter = "summary"
)

td_formals <- names(formals(testthat::test_dir))
if ("stop_on_failure" %in% td_formals) {
  test_dir_args$stop_on_failure <- TRUE
}
if ("stop_on_warning" %in% td_formals) {
  test_dir_args$stop_on_warning <- TRUE
}

res <- do.call(testthat::test_dir, test_dir_args)

collect_results <- function(x) {
  if (is.null(x)) return(character())

  if (is.data.frame(x) && "result" %in% names(x)) {
    return(as.character(x$result))
  }

  if (is.list(x)) {
    out <- character()
    for (el in x) {
      out <- c(out, collect_results(el))
    }
    return(out)
  }

  character()
}

all_results <- collect_results(res)
if (length(all_results) > 0 && any(all_results %in% c("failure", "error"))) {
  stop("Mindestens ein Test ist fehlgeschlagen.")
}

cat("Alle Tests erfolgreich abgeschlossen.\n")
