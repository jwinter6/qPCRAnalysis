testthat::local_edition(3)

if (!exists("project_root_path")) {
  project_root_path <- function() {
    normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  }
}

if (!exists("with_project_root")) {
  with_project_root <- function(expr) {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(project_root_path())
    force(expr)
  }
}

if (!exists("load_reader_helpers")) {
  load_reader_helpers <- function() {
    library(dplyr)
    library(tidyr)
    library(tibble)
    library(stringr)
    library(readxl)
    library(xml2)

    env <- new.env(parent = globalenv())
    with_project_root(source("R/helpers_readers.R", local = env))
    env
  }
}

if (!exists("load_app_helpers")) {
  load_app_helpers <- function() {
    library(dplyr)
    library(tidyr)
    library(tibble)
    library(stringr)
    library(readxl)
    library(xml2)

    env <- new.env(parent = globalenv())
    with_project_root({
      source("R/helpers_app_metadata.R", local = env)
      source("R/helpers_export.R", local = env)
      source("R/helpers_readers.R", local = env)
    })
    env
  }
}

if (!exists("load_app_server")) {
  load_app_server <- function() {
    env <- new.env(parent = globalenv())
    app_obj <- with_project_root(source("app.R", local = env)$value)

    if (!inherits(app_obj, "shiny.appobj")) {
      stop("Konnte shinyApp-Objekt aus app.R nicht laden.")
    }
    app_obj$serverFuncSource()
  }
}
