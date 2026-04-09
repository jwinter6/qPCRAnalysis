############################################################
# app.R – qPCR Dashboard (navbar, Upload-basiert)
#
# Plot-Themes:
# - Alle ggplot2-Grafiken verwenden jetzt ggthemes::theme_gdocs()
############################################################

library(shiny)
library(bslib)
library(fresh)
library(tidyverse)
library(readxl)
library(xml2)
library(plotly)
library(ggthemes)    # NEU: Themes für ggplot2 (theme_few)
library(DT)
library(writexl)
library(outliers)
library(EnvStats)

source("R/helpers_app_metadata.R")
source("R/helpers_export.R")
source("R/helpers_readers.R")
source("R/helpers_standardcurves.R")
source("R/helpers_outliers.R")

app_version_info <- read_app_metadata()
app_version_label <- format_app_version_label(app_version_info)

############################
# UI
############################

source("R/ui_sidebar.R")
source("R/ui_tabs/load.R")
source("R/ui_tabs/ctqty.R")
source("R/ui_tabs/ctsample.R")
source("R/ui_tabs/plate_qc.R")
source("R/ui_tabs/fluorescence.R")
source("R/ui_tabs/cluster.R")
source("R/ui_tabs/amp.R")
source("R/ui_tabs/ctsd.R")
source("R/ui_tabs/melt.R")
source("R/ui_tabs/stdcurves.R")
source("R/ui_tabs/outliers.R")
source("R/ui_tabs/report.R")
source("R/ui_tabs/help.R")



ui <- tagList(
  #fresh::use_theme(theme_qpcr),
  bslib::page_navbar(
    title = "qPCR Dashboard",
    id = "tabs",
    theme = bslib::bs_theme(version = 5, bootswatch = "cosmo"),
    header = tags$style(
      HTML(paste0(
        ".qpcr-statusbar{display:flex;align-items:center;justify-content:flex-start;",
        "padding:0.5rem 1rem;font-size:0.85rem;color:#6c757d;",
        "background:#f8f9fa;border-top:1px solid #dee2e6;}"
      ))
    ),
    footer = tags$div(class = "qpcr-statusbar", app_version_label),
    sidebar = ui_sidebar,
    tab_load,
    tab_plate_qc,
    tab_fluorescence,
    tab_cluster,
    tab_ctqty,
    tab_ctsample,
    tab_amp,
    tab_ctsd,
    tab_melt,
    tab_stdcurves,
    tab_outliers,
    tab_report,
    tab_help
  )
)



############################
# SERVER
############################

server <- function(input, output, session) {
  source("R/server/00_setup.R", local = TRUE)
  source("R/server/load.R", local = TRUE)
  source("R/server/filters.R", local = TRUE)
  source("R/server/ctqty.R", local = TRUE)
  source("R/server/ctsample.R", local = TRUE)
  source("R/server/plate_qc.R", local = TRUE)
  source("R/server/fluorescence.R", local = TRUE)
  source("R/server/cluster.R", local = TRUE)
  source("R/server/amp.R", local = TRUE)
  source("R/server/ctsd.R", local = TRUE)
  source("R/server/melt.R", local = TRUE)
  source("R/server/stdcurves.R", local = TRUE)
  source("R/server/outliers.R", local = TRUE)
  source("R/server/report.R", local = TRUE)
}

############################
# App starten
############################

shinyApp(ui = ui, server = server)
