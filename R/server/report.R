  ##########################
  # Report Export
  ##########################
  
  report_add_item <- function(title, tab, type, plot = NULL, plotly = NULL, data = NULL) {
    id <- paste0(
      gsub("[^a-z0-9]+", "_", tolower(tab)),
      "_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "_",
      sample.int(9999, 1)
    )
    rv$report_items[[id]] <- list(
      id = id,
      title = title,
      tab = tab,
      type = type,
      plot = plot,
      plotly = plotly,
      data = data,
      created = Sys.time()
    )
  }
  
  report_items_df <- reactive({
    items <- rv$report_items
    if (length(items) == 0) {
      return(tibble())
    }
    tibble::tibble(
      id = vapply(items, `[[`, character(1), "id"),
      title = vapply(items, `[[`, character(1), "title"),
      tab = vapply(items, `[[`, character(1), "tab"),
      type = vapply(items, `[[`, character(1), "type")
    )
  })
  
  output$report_items_ui <- renderUI({
    df <- report_items_df()
    if (nrow(df) == 0) {
      return(tags$div(class = "text-muted", "Noch keine Inhalte zum Report hinzugefuegt."))
    }
    choices <- stats::setNames(df$id, paste0(df$tab, " - ", df$title, " [", df$type, "]"))
    checkboxGroupInput(
      "report_items_selected",
      "Inhalte fuer den Report auswaehlen",
      choices = choices,
      selected = df$id
    )
  })

  output$report_download_ui <- renderUI({
    if (is.null(rv$report_last_path) || !file.exists(rv$report_last_path)) {
      return(tags$div(class = "text-muted", "Noch kein Report erstellt."))
    }
    downloadButton("report_download", "Report herunterladen")
  })

  output$report_download <- downloadHandler(
    filename = function() {
      name <- rv$report_last_name
      fmt <- rv$report_last_format
      if (is.null(name) || name == "" || is.null(fmt) || fmt == "") {
        return(paste0("report_", Sys.Date(), ".html"))
      }
      paste0(name, ".", fmt)
    },
    content = function(file) {
      src <- rv$report_last_path
      if (is.null(src) || !file.exists(src)) {
        stop("Report-Datei nicht gefunden.")
      }
      file.copy(src, file, overwrite = TRUE)
    }
  )
  
  observeEvent(input$report_generate, {
    report_name <- trimws(input$report_name)
    if (is.null(report_name) || report_name == "") {
      showNotification("Bitte zuerst einen Report-Namen eingeben.", type = "error", duration = 6)
      return(NULL)
    }
    
    selected <- input$report_items_selected
    if (is.null(selected) || length(selected) == 0) {
      showNotification("Bitte mindestens einen Report-Inhalt auswaehlen.", type = "warning", duration = 6)
      return(NULL)
    }
    
    fmt <- input$report_format
    out_dir <- tempdir()
    out_name <- paste0("qpcr_report_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", fmt)
    out_path <- file.path(out_dir, out_name)
    log_path <- file.path(out_dir, paste0(tools::file_path_sans_ext(out_name), ".log"))
    
    show_report_error_modal <- function(title, message, log_excerpt = NULL) {
      body <- tagList(
        tags$p(message)
      )
      if (!is.null(log_excerpt) && nzchar(log_excerpt)) {
        body <- tagList(
          body,
          tags$hr(),
          tags$strong("Log-Auszug:"),
          tags$pre(style = "max-height:300px; overflow:auto;", log_excerpt)
        )
      }
      footer <- tagList(
        modalButton("OK"),
        if (!is.null(rv$report_last_log) && file.exists(rv$report_last_log)) {
          downloadButton("report_log_download", "Log herunterladen")
        }
      )
      showModal(
        modalDialog(
          title = title,
          body,
          easyClose = TRUE,
          size = "l",
          footer = footer
        )
      )
    }
    
    read_log_excerpt <- function(path, max_lines = 60) {
      if (!file.exists(path)) return(NULL)
      lines <- readLines(path, warn = FALSE)
      if (length(lines) == 0) return(NULL)
      paste(tail(lines, max_lines), collapse = "\n")
    }
    
    items <- rv$report_items[selected]
    tmp_rds <- tempfile(fileext = ".rds")
    saveRDS(items, tmp_rds)
    
    template_path <- file.path("R", "report_template.Rmd")
    if (!file.exists(template_path)) {
      showNotification(paste("Report-Template fehlt:", template_path), type = "error", duration = 8)
      return(NULL)
    }
    
    if (fmt == "pdf") {
      has_latex <- nzchar(Sys.which("pdflatex")) || nzchar(Sys.which("xelatex"))
      if (!has_latex) {
        rv$report_last_log <- log_path
        show_report_error_modal(
          "Report-Export fehlgeschlagen",
          "PDF-Export nicht moeglich: Es ist keine LaTeX-Installation gefunden (pdflatex/xelatex)."
        )
        return(NULL)
      }
    }
    
    withProgress(message = "Report wird erstellt", value = 0, {
      incProgress(0.2, detail = "Vorbereitung")
      
      output_format <- switch(
        fmt,
        "pdf" = "pdf_document",
        "html" = "html_document",
        "docx" = "word_document",
        "html_document"
      )
      
      output_options <- list()
      if (fmt == "pdf") {
        output_options$geometry <- "a4paper"
        output_options$keep_tex <- TRUE
      }
      
      if (fmt == "docx") {
        ref_docx <- file.path("R", "report_reference.docx")
        if (file.exists(ref_docx)) {
          output_options$reference_docx <- ref_docx
        } else {
          showNotification(
            "Hinweis: Kein report_reference.docx gefunden, Word nutzt Standard-Seitengroesse.",
            type = "warning",
            duration = 8
          )
        }
      }
      
      incProgress(0.5, detail = "Rendern")
      render_ok <- TRUE
      render_err <- NULL
      tryCatch(
        rmarkdown::render(
          template_path,
          output_file = out_name,
          output_format = output_format,
          output_options = output_options,
          output_dir = out_dir,
          params = list(
            items_rds = tmp_rds,
            report_title = report_name
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE,
          clean = FALSE
        ),
        error = function(e) {
          render_ok <<- FALSE
          render_err <<- e$message
        }
      )
      if (!render_ok) {
        rv$report_last_log <- log_path
        log_excerpt <- read_log_excerpt(log_path)
        show_report_error_modal(
          "Report-Export fehlgeschlagen",
          render_err,
          log_excerpt
        )
        return(NULL)
      }
      
      incProgress(0.3, detail = "Fertigstellen")
    })
    
    rv$report_last_path <- out_path
    rv$report_last_name <- report_name
    rv$report_last_format <- fmt
    rv$report_last_log <- log_path
    showNotification("Report erstellt. Bitte unten herunterladen.", type = "message", duration = 6)
  })

  output$report_log_download <- downloadHandler(
    filename = function() {
      base <- rv$report_last_name
      if (is.null(base) || base == "") base <- "report"
      paste0(base, "_report.log")
    },
    content = function(file) {
      src <- rv$report_last_log
      if (is.null(src) || !file.exists(src)) {
        stop("Log-Datei nicht gefunden.")
      }
      file.copy(src, file, overwrite = TRUE)
    }
  )
