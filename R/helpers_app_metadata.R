read_app_metadata <- function(description_path = "DESCRIPTION") {
  env_version <- trimws(Sys.getenv("QPCRANALYSIS_VERSION", unset = ""))
  env_build <- trimws(Sys.getenv("QPCRANALYSIS_BUILD", unset = ""))

  version_value <- env_version
  if (!nzchar(version_value) && file.exists(description_path)) {
    version_value <- tryCatch(
      as.character(read.dcf(description_path, fields = "Version")[1, "Version"]),
      error = function(e) ""
    )
  }

  if (!nzchar(version_value)) {
    version_value <- "development"
  }

  build_value <- if (nzchar(env_build)) env_build else NA_character_

  list(
    version = version_value,
    build = build_value
  )
}

format_app_version_label <- function(metadata = read_app_metadata()) {
  stopifnot(is.list(metadata), !is.null(metadata$version))

  if (!is.null(metadata$build) && !is.na(metadata$build) && nzchar(metadata$build)) {
    return(paste0("Version ", metadata$version, " (Build ", metadata$build, ")"))
  }

  paste0("Version ", metadata$version)
}
