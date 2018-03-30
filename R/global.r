#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

is_in_package <-
  function()
    ! identical(environment(is_in_package), .GlobalEnv)

if (!is_in_package()) {
  library(data.table)
  library(rlang)
  library(purrr)
  library(tidyr)
  library(plyr)
  library(dplyr)
  library(stringr)
  library(phyloseq)
  library(ggplot2)
  library(shiny)
  library(htmltools)
  library(plotly)
  library(utils)
  library(shinyjs)
  library(shinythemes)
}

pkg_file <- function(path = ".") {
  if (is_in_package())
    system.file(path, package = "MetaMap", mustWork = T)
  else
    file.path("../inst", path)
}
