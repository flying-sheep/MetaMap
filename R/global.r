#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

is_in_package <- function() !identical(environment(is_in_package), .GlobalEnv)

#' @import data.table
#' @import rlang
#' @import purrr
#' @import tidyr
#' @import plyr
#' @import dplyr
#' @import stringr
#' @import phyloseq
#' @import ggplot2
#' @import shiny
#' @import htmltools
#' @import plotly
#' @import utils
#' @import shinyjs

if(!is_in_package()){
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
}

pkg_file <- function(path = '.') {
  if (is_in_package()) system.file(path, package = 'metatranscriptome', mustWork = T)
  else {
    path
  }
}

DIR <- ifelse(is_in_package(), "R/data", "data")
MAX_SAMPLES <- 250

print(DIR)
print(getwd())
print(list.files("R"))
print(list.files("R/data"))
print(environment(is_in_package))
print(pkg_file("R"))
print(pkg_file(file.path(DIR, 'studies')))
STUDIES <- list.files(pkg_file(file.path(DIR, 'studies'))) %>%
  str_split_fixed("\\.", n = 2) %>% .[,1]

load(pkg_file(file.path(DIR, 'study_info.RData')))

# only show studies that exist in the data/studie directory
study_info <- subset(study_info, study %in% STUDIES)

# add links redirecting to the sra website for each study
study_info$link <-
  with(
    study_info,
    paste0(
      "<a href='https://trace.ncbi.nlm.nih.gov/Traces/sra/?study=",
      study,
      "'>",
      study,
      "</a>"
    )
  )
