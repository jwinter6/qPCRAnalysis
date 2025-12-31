############################################################
# app.R – qPCR Dashboard (shinydashboard, Upload-basiert)
#
# Plot-Themes:
# - Alle ggplot2-Grafiken verwenden jetzt ggthemes::theme_gdocs()
############################################################

library(shiny)
library(shinydashboard)
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
source("R/ui_tabs/amp.R")
source("R/ui_tabs/ctsd.R")
source("R/ui_tabs/melt.R")
source("R/ui_tabs/stdcurves.R")
source("R/ui_tabs/outliers.R")
source("R/ui_tabs/help.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "qPCR Dashboard"
  ),
  ui_sidebar,
  dashboardBody(
    tabItems(
      tab_load,
      tab_ctqty,
      tab_amp,
      tab_ctsd,
      tab_melt,
      tab_stdcurves,
      tab_outliers,
      tab_help
    )
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
  source("R/server/amp.R", local = TRUE)
  source("R/server/ctsd.R", local = TRUE)
  source("R/server/melt.R", local = TRUE)
  source("R/server/stdcurves.R", local = TRUE)
  source("R/server/outliers.R", local = TRUE)
}

############################
# App starten
############################

shinyApp(ui = ui, server = server)
