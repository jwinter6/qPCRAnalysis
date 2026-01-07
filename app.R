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
library(plotly)
library(ggthemes)    # NEU: Themes für ggplot2 (theme_few)
library(DT)
library(writexl)
library(outliers)
library(EnvStats)

source("R/helpers_readers.R")
source("R/helpers_standardcurves.R")
source("R/helpers_outliers.R")

############################
# UI
############################

source("R/ui_sidebar.R")
source("R/ui_tabs/load.R")
source("R/ui_tabs/ctqty.R")
source("R/ui_tabs/ctsample.R")
source("R/ui_tabs/plate_qc.R")
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
    sidebar = ui_sidebar,
    tab_load,
    tab_plate_qc,
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
